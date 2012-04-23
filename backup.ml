(* Saving of backups *)

open Batteries_uni

module StringMap = Map.StringMap

(* Represent this pathname in a nice fixed-width. *)
let clean_name name =
  let width = 67 in
  let len = String.length name in
  if len <= width + 3 then name
  else "..." ^ (String.sub name (len - width) width)

(* Wrap a storage pool in a tracker that monitors a progress meter. *)
class write_track_pool (inner : #Pool.writable) =
object (self)
  val start_time = Unix.gettimeofday ()

  val mutable count = 0L
  val mutable compressed = 0L
  val mutable uncompressed = 0L
  val mutable dup = 0L
  val mutable skip = 0L
  val mutable path = ""

  (* TODO: How to share this with the node meter. *)
  initializer (
    let show fmt =
      let age = Unix.gettimeofday () -. start_time in
      let total = Int64.add uncompressed (Int64.add dup skip) in
      Format.fprintf fmt "%9Ld nodes, " count;
      Log.format_size fmt "%s dup, " dup;
      Log.format_size fmt "%s skip, " skip;
      Log.format_size fmt "%s total@\n" total;
      Log.format_size_rate fmt "  %s uncompressed (%s/sec)@\n" uncompressed age;
      Log.format_size_rate fmt "  %s compressed   (%s/sec)  " compressed age;
      Log.format_ratio fmt "%.1f%%@\n" uncompressed compressed;
      Format.fprintf fmt "path: %s@." (clean_name path) in
    Log.set_meter (Log.build_format_meter show))

  method finish = Log.finish_meter ()

  method add chunk =
    count <- Int64.succ count;
    if inner#mem chunk#hash then
      dup <- Int64.add dup (Int64.of_int chunk#data_length)
    else begin
      compressed <- Int64.add compressed (Int64.of_int chunk#write_size);
      uncompressed <- Int64.add uncompressed (Int64.of_int chunk#data_length);
      inner#add chunk
    end;
    Log.update_meter ()

  method add_skip size =
    skip <- Int64.add skip size;
    Log.update_meter ()

  method with_path : 'a. string -> (unit -> 'a) -> 'a =
    fun new_path thunk ->
      let old_path = path in
      path <- new_path;
      Std.finally (fun () -> path <- old_path) thunk ()

  method mem = inner#mem
  method find = inner#find
  method find_option = inner#find_option
  method find_full = inner#find_full
  method find_index = inner#find_index
  method get_backups = inner#get_backups
  method flush = inner#flush
  method close = inner#close
  method uuid = inner#uuid
end

let block_size = 256 * 1024

let decode_atts atts =
  let each att =
    try String.split att "=" with
      | Not_found -> Log.failf "attribute has no '=': %S" att in
  StringMap.of_enum (Enum.map each (List.enum atts))

(* Read a chunk-sized block from [fd].  Returns the data read, and
   'true' to indicate that we reached EOF. *)
let read_chunk fd =
  let buffer = String.create block_size in
  let rec read offset =
    if offset = block_size then (buffer, false) else begin
      let count = Unix.read fd buffer offset (block_size - offset) in
      if count = 0 then (String.sub buffer 0 offset, true)
      else read (offset + count)
    end in
  read 0

module Cache : sig
  type t

  (** [get c parent_stat add_skip]. *)
  val get : Seendb.t -> Dbunix.stat -> (int64 -> unit) -> t

  (** [check c kind child_stat op] If the child_stat matches the op, return
      the information from the cache.  Otherwise, call op to generate
      it. *)
  val check : t -> string -> Dbunix.stat -> (unit -> (Hash.t * Dbunix.stat)) ->
    (Hash.t * Dbunix.stat)

  val flush : t -> unit

end = struct
  type t = {
    cache : Seendb.t;
    pinode : int64;
    prior : Seendb.cache_entry;
    mutable next : Seendb.cache_entry;
    add_skip : int64 -> unit;
  }

  let get cache dir_props add_skip =
    let pino = Dbunix.get_int64 "ino" dir_props in
    { cache = cache;
      pinode = pino;
      prior = Seendb.get cache pino;
      next = Seendb.Int64Map.empty;
      add_skip = add_skip }

  let check' c kind child_stat get_op =
    let cino = Dbunix.get_int64 "ino" child_stat in
    match Seendb.Int64Map.Exceptionless.find cino c.prior with
      | None -> get_op ()
      | Some node ->
	let ctime = Dbunix.get_time "ctime" child_stat in
	if node.Seendb.n_ctime = ctime then begin
	  Log.debugf "cache: ino=%Ld hash=%S" cino (Hash.to_string node.Seendb.n_hash);
	  c.add_skip (Dbunix.get_int64 "size" child_stat);
	  let stat' = StringMap.add "hash" (Hash.to_string node.Seendb.n_hash) child_stat in
	  (node.Seendb.n_hash, stat')
	end else get_op ()

  let check c kind child_stat get_op = match kind with
    | "REG" ->
      let ((hash, props) as result) = check' c kind child_stat get_op in
      let entry = Seendb.entry_of_node hash props in
      c.next <- Seendb.Int64Map.add entry.Seendb.n_inode entry c.next;
      result
    | _ -> get_op ()

  let flush c = Seendb.update c.cache c.pinode c.next
end

let store_file pool path =
  let ind = Indirect.make_indirect pool "ind" block_size in
  let rec read fd =
    let (data, eof) = read_chunk fd in
    let node = if String.length data = 0 then Nodes.NullNode
      else Nodes.BlobNode data in
    let hash = Nodes.try_put pool node in
    Indirect.add ind hash;
    if not eof then read fd in
  with_dispose ~dispose:Unix.close read (Dbunix.open_for_read path);
  Indirect.finish ind

let save' pool cache backup_path atts =
  let pool = new write_track_pool pool in
  let now = Unix.gettimeofday () in
  let atts = decode_atts atts in
  let root_stat = match Dbunix.lstat backup_path with
    | ("DIR", stat) -> stat
    | (kind, _) -> Log.failf "Root of backup is not a DIR: %S" kind in

  let rec walk path kind props =
    pool#with_path path (fun () ->
      let props = match kind with
	| "DIR" -> get_dir path kind props

	| "REG" ->
	    let hash = store_file pool path in
	    StringMap.add "data" (Hash.to_string hash) props

	| "LNK" ->
	    let target = Unix.readlink path in
	    StringMap.add "target" target props

	| _ -> props
      in
      (Nodes.try_put pool (Nodes.NodeNode (kind, props)), props))

  and get_dir path kind dir_props =
    let dircache = Cache.get cache dir_props pool#add_skip in

    let children = Dbunix.dir_with_stats path in
    let children = List.filter (fun (name, _) ->
      name <> "." && name <> "..") children in
    let buf = Indirect.Dir.make pool block_size in

    List.iter (fun (name, (child_kind, child_props)) ->
      let (hash, child_props) = Cache.check dircache child_kind child_props
	(fun () ->
	  let path = Filename.concat path name in
	  walk path child_kind child_props) in
      Log.debugf "add name=%S hash=%S" name (Hash.to_string hash);
      Indirect.Dir.add buf name hash) children;
    Cache.flush dircache;
    let child_hashes = Indirect.Dir.finish buf in
    StringMap.add "children" (Hash.to_string child_hashes) dir_props

  in

  Seendb.begin_transaction cache;
  let (root_hash, _) = walk backup_path "DIR" root_stat in
  let atts = StringMap.add "hash" (Hash.to_string root_hash) atts in

  let hash = Nodes.try_put pool (Nodes.BackupNode (now, atts)) in
  pool#finish;
  Seendb.commit cache;
  Log.infof "Completed backup: %s" (Hash.to_string hash)

let save pool cache_dir backup_path atts =
  Seendb.with_cache cache_dir (fun cache -> save' pool cache backup_path atts)
