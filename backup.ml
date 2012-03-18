(* Saving of backups *)

open Batteries_uni

module StringMap = Map.StringMap

(* Wrap a storage pool in a tracker that monitors a progress meter. *)
class write_track_pool (inner : File_pool.file_pool) =
object (self)
  inherit Log.meter

  val mutable count = 0L
  val mutable compressed = 0L
  val mutable uncompressed = 0L
  val mutable dup = 0L
  val mutable skip = 0L

  (* TODO: How to share this with the node meter. *)
  method get_text =
    let now = Unix.gettimeofday () in
    let out = IO.output_string () in
    let fmt = Format.formatter_of_output out in
    Format.fprintf fmt "%9Ld nodes" count;
    let funcompressed = Int64.to_float uncompressed in
    let fcompressed = Int64.to_float compressed in
    let rate = funcompressed /. (now -. start_time) in
    let zrate = fcompressed /. (now -. start_time) in
    let ratio = ((funcompressed -. fcompressed) /. funcompressed) *. 100.0 in
    let total = Int64.add uncompressed (Int64.add dup skip) in
    Format.fprintf fmt ", %s dup, %s skip, %s total@\n"
      (Misc.nice_number dup) (Misc.nice_number skip) (Misc.nice_number total);
    Format.fprintf fmt "  %s uncompressed (%s/sec)@\n"
      (Misc.nice_number uncompressed)
      (Misc.fnice_number rate);
    Format.fprintf fmt "  %s compressed   (%s/sec)  %.1f%%@."
      (Misc.nice_number compressed)
      (Misc.fnice_number zrate) ratio;
    IO.close_out out

  method add chunk =
    count <- Int64.succ count;
    if inner#mem chunk#hash then
      skip <- Int64.add skip (Int64.of_int chunk#data_length)
    else begin
      compressed <- Int64.add compressed (Int64.of_int chunk#write_size);
      uncompressed <- Int64.add uncompressed (Int64.of_int chunk#data_length);
      inner#add chunk
    end;
    self#update

  method mem = inner#mem
  method find = inner#find
  method find_option = inner#find_option
  method find_full = inner#find_full
  method find_index = inner#find_index
  method get_backups = inner#get_backups
  method flush = inner#flush
  method close = inner#close
end

let block_size = 256 * 1024

let decode_atts atts =
  let each att =
    try String.split att "=" with
      | Not_found -> Log.failure ("attribute has no '='", ["attr", att]) in
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

let save pool cache_dir backup_path atts =
  let full_pool = new write_track_pool pool in
  let pool = (full_pool :> File_pool.t) in
  let now = Unix.gettimeofday () in
  let atts = decode_atts atts in
  let root_stat = match Dbunix.lstat backup_path with
    | ("DIR", stat) -> stat
    | (kind, _) -> Log.failure ("Root of backup is not a DIR", ["kind", kind]) in
  let root_stat = StringMap.of_enum (List.enum root_stat) in

  let rec walk path kind props =
    let props = match kind with
      | "DIR" ->
	let children = Dbunix.dir_with_stats path in
	let children = List.filter (fun (name, _) ->
	  name <> "." && name <> "..") children in
	let buf = Indirect.Dir.make pool block_size in
	List.iter (fun (name, (kind, props)) ->
	  let props = StringMap.of_enum (List.enum props) in
	  let path = Filename.concat path name in
	  let hash = walk path kind props in
	  Indirect.Dir.add buf name hash) children;
	let child_hashes = Indirect.Dir.finish buf in
	StringMap.add "children" (Hash.to_string child_hashes) props

      | "REG" ->
	let hash = store_file pool path in
	StringMap.add "data" (Hash.to_string hash) props

      | "LNK" ->
	let target = Unix.readlink path in
	StringMap.add "target" target props

      | _ -> props
    in
    Nodes.try_put pool (Nodes.NodeNode (kind, props)) in

  let root_hash = walk backup_path "DIR" root_stat in
  let atts = StringMap.add "hash" (Hash.to_string root_hash) atts in

  let hash = Nodes.try_put pool (Nodes.BackupNode (now, atts)) in
  full_pool#finish;
  Log.info (fun () -> "Completed backup", ["hash", Hash.to_string hash])
