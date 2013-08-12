(* File-based storage pools *)

open Batteries
(* open LegacyIO *)
open Printf

module StringMap = Maps.StringMap

let default_limit = 640 * 1024 * 1024
let limit_lower_bound = 1 lsl 20
let limit_upper_bound = 1 lsl 30 - 1 (* To avoid using LargeFile IO *)

(* The Uuidm module uses [Random.make_self_init], which as of Ocaml
   3.12, only uses the time of day (admittedly with some precision), and
   the process id.  For small use cases, this isn't a problem, but for
   something supposedly universal, it's quite terrible.

   To improve this, we'll use random entropy we get from /dev/urandom,
   and generate a name-based UUID using the URL of
   "http://random.davidb.org/" followed by the 16 bytes of random data
   in hex.
*)
let get_uuid () =
  let buf = Buffer.create 57 in
  Buffer.add_string buf "http://random.davidb.org/";
  let random = String.create 16 in
  with_dispose ~dispose:Legacy.close_in
    (fun chan -> Legacy.really_input chan random 0 (String.length random))
    (Legacy.open_in_bin "/dev/urandom");
  for i = 0 to String.length random - 1 do
    Buffer.add_string buf (Printf.sprintf "%02x" (Char.code random.[i]))
  done;
  Uuidm.to_string (Uuidm.create (`V5 (Uuidm.ns_url, Buffer.contents buf)))

let create_file_pool ?(limit=default_limit) ?(newfile=false) path =
  if limit < limit_lower_bound || limit > limit_upper_bound then
    Log.fail "Pool size limit out of range";
  Misc.ensure_empty_directory ~what:"pool" path;
  let metadata = Filename.concat path "metadata" in
  let props_name = Filename.concat metadata "props.txt" in
  let uuid = get_uuid () in
  Unix.mkdir metadata 0o755;
  let put out =
    fprintf out "# Ldump metadata properties (odump)\n";
    fprintf out "uuid=%s\n" uuid;
    fprintf out "newfile=%b\n" newfile;
    fprintf out "limit=%d\n" limit in
  with_dispose ~dispose:close_out put (open_out props_name)

(* Add another entry to the backup list. *)
let append_backup path hash =
  File.with_file_out ~mode:[`append; `create] path (fun out ->
    fprintf out "%s\n" (Hash.to_string hash))

(* Real Java property files are more complex than this, but we only
   need to be able to read back what we've written. *)
let read_flat_properties filename =
  let decode map line =
    if String.length line > 0 && line.[0] == '#' then map
    else begin
      match String.Exceptionless.split line ~by:"=" with
	  None -> Log.failf "Invalid line in property file: %S" line
	| Some (key, value) -> StringMap.add key value map
    end in
  let get inp = fold decode StringMap.empty (IO.lines_of inp) in
  with_dispose ~dispose:close_in get (open_in filename)

type props = {
  p_uuid: Uuidm.t;
  p_limit: int;
  p_newfile: bool }

let decode_backup_properties map =
  { p_uuid = Option.get @@ Uuidm.of_string @@ StringMap.find "uuid" map;
    p_limit = Option.map_default int_of_string default_limit @@
      StringMap.Exceptionless.find "limit" map;
    p_newfile = Option.map_default bool_of_string false @@
      StringMap.Exceptionless.find "newfile" map }

let to_index_name path =
  if String.ends_with path ".data" then
    String.sub path 0 (String.length path - 5) ^ ".idx"
  else
    Log.failf "Malformed datafile name: '%S'" path

let data_re = Str.regexp "^pool-data-\\([0-9][0-9][0-9][0-9]\\)\\.data$"
(* Note that Str doesn't appear to be reentrant. *)

let make_pool_name path idx = Filename.concat path (sprintf "pool-data-%04d.data" idx)

type node = {
  n_file: Chunk.chunk_file;
  n_index: File_index.t;
  n_path: string }

(* Attempt to regenerate the index for this pool file. *)
let recover_index name file index =
  Log.warnf "Index recovery for %S" name;
  index#clear;
  let limit = file#size in
  let rec loop pos =
    if pos < limit then begin
      let info = file#read_info pos in
      index#add info.Chunk.in_hash pos info.Chunk.in_kind;
      loop (pos + info.Chunk.in_write_size)
    end in
  loop 0;
  index#save limit;
  index#load limit

let find_backup_nodes path =
  let redata name =
    if Str.string_match data_re name 0 then
      Some (int_of_string @@ Str.matched_group 1 name)
    else None in
  let nums = Sys.files_of path //@ redata in
  let nums = List.sort (fun a b -> compare b a) (List.of_enum nums) in

  let lookup num =
    let fname = make_pool_name path num in
    let file = Chunk.open_chunk_file fname in
    let index = File_index.make (to_index_name fname) in
    begin try index#load file#size with
      | Sys_error _
      | File_index.Index_read_error _ -> recover_index fname file index
    end;
    { n_file = file; n_index = index; n_path = fname } in
  List.map lookup nums

(* Increment the number embedded in this name. *)
let number_re = Str.regexp "[0-9][0-9][0-9][0-9]"
let get_number name =
  let pos = Str.search_backward number_re name (String.length name) in
  int_of_string (String.sub name pos 4)

(* Open a file and lock it.  As long as everyone uses this same
   locking protocol, then the pool will be safe from concurrent
   access.  Returns the descriptor, appropriate for calling
   release_lock later. *)
let open_lock path =
  let fd = Unix.openfile path [Unix.O_RDWR; Unix.O_CREAT] 0o644 in
  Unix.set_close_on_exec fd;
  (* Wait? *)
  begin try Unix.lockf fd Unix.F_TLOCK 0 with
    | Unix.Unix_error (e, _, _) ->
      Log.failf "Unable to get pool lock in %S, %s" path (Unix.error_message e)
  end;
  fd

let release_lock fd =
  Unix.lockf fd Unix.F_ULOCK 0;
  Unix.close fd

let open_file_pool path =
  Misc.ensure_directory ~what:"pool" path;
  let metadata = Filename.concat path "metadata" in
  let props_name = Filename.concat metadata "props.txt" in

  let lock_fd = open_lock (Filename.concat path "lock") in

object (self)
  val mutable props = decode_backup_properties @@ read_flat_properties props_name
  val mutable nodes = find_backup_nodes path
  val mutable dirty = false
  val mutable first_write = true

  method private cur_file = match nodes with
      [] -> raise Not_found
    | ({ n_file=file; _ } :: _) -> file
  method private cur_index = match nodes with
      [] -> raise Not_found
    | ({ n_index=index; _ } :: _) -> index

  method private next_name =
    let num = match nodes with
      | [] -> 0
      | (node::_) -> 1 + get_number node.n_path in
    make_pool_name path num

  (* Is there room for [chunk] in the current pool file?  Returns false when
   * there are no files yet created, or newfile is requesting a fresh file for
   * the first write. *)
  method private room chunk =
    (not props.p_newfile || first_write) &&
      nodes <> [] && self#cur_file#size + chunk#write_size <= props.p_limit

  method private make_new_pool_file =
    self#flush;
    let fname = self#next_name in
    let file = Chunk.open_chunk_file fname in
    let index = File_index.make (to_index_name fname) in
    nodes <- { n_file=file; n_index=index; n_path=fname } :: nodes

  method add chunk =
    if not (self#mem chunk#hash) then begin
      if not (self#room chunk) then self#make_new_pool_file;
      let file = self#cur_file in
      let index = self#cur_index in
      let pos = file#append chunk in
      index#add chunk#hash pos chunk#kind;
      if chunk#kind = "back" then
	append_backup (Filename.concat metadata "backups.txt") chunk#hash;
      dirty <- true;
      first_write <- false
    end

  method find_full hash =
    let lookup node =
      let info = node.n_index#find_option hash in
      Option.map (fun (pos, kind) ->
	((fun () -> node.n_file#read pos),
	 (fun () -> node.n_file#read_info pos),
	 kind)) info in
    List.Exceptionless.find_map lookup nodes

  method mem hash =
    let lookup node = node.n_index#mem hash in
    List.exists lookup nodes

  method find_option hash =
    Option.map (fun (chunk, _, _) -> chunk ()) (self#find_full hash)

  method find hash = Option.get @@ self#find_option hash

  (* method flush_files = *)
  (*   match nodes with *)
  (* 	[] -> () *)
  (*     | ({ n_file=file} :: _ ) -> file#flush *)

  method find_index hash =
    (* Start with the oldest ones, since those shouldn't ever change.
       The index will be based on the sizes from each. *)
    let rec loop offset nodes =
      match nodes with
	| [] -> raise Not_found
	| ({ n_index=index; _ }) :: rest ->
	  begin match index#find_offset hash with
	    | None -> loop (offset + index#count) rest
	    | Some pos -> offset + pos
	  end in
    loop 0 nodes

  method flush =
    if dirty then begin
      match nodes with
	  [] -> ()
	| ({ n_file=file; n_index=index; _ } :: _) ->
	  file#flush;
	  index#save file#size
    end;
    dirty <- false

  method close =
    self#flush;
    List.iter (fun n -> n.n_file#close) nodes;
    release_lock lock_fd

  method get_backups =
    let backups_name = Filename.concat metadata "backups.txt" in
    try
      let get inp = List.of_enum @@ Enum.map Hash.of_string @@ IO.lines_of inp in
      with_dispose ~dispose:close_in get (open_in backups_name)
    with
	Sys_error _ -> []

  method uuid = Uuidm.to_string props.p_uuid
end

let with_file_pool path f =
  with_dispose ~dispose:(fun x -> x#close) f
    (open_file_pool path)
