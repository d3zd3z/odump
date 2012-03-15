(* File-based storage pools *)

open Batteries_uni
(* open LegacyIO *)
open Printf

module StringMap = Map.StringMap

let ensure_directory path =
  if not (Sys.is_directory path) then
    failwith (Printf.sprintf "Pool pathname is not a directory: %s" path)

let ensure_empty_directory path =
  ensure_directory path;
  if Sys.readdir path <> [| |] then
    failwith (Printf.sprintf "Cannot create pool in non-empty directory: %s" path)

let default_limit = 640 * 1024 * 1024
let limit_lower_bound = 1 lsl 20
let limit_upper_bound = 1 lsl 30 - 1 (* To avoid using LargeFile IO *)

(* TODO: The uuidm library seems to only use the time as it's basis
   for randomness.  It would be better to base this on /dev/urandom, mask
   appropriately for type 4 uuid, and just make it ourselves. *)
let get_uuid () = Uuidm.to_string (Uuidm.create `V4)

let create_file_pool ?(limit=default_limit) ?(newfile=false) path =
  if limit < limit_lower_bound || limit > limit_upper_bound then
    failwith "Pool size limit out of range";
  ensure_empty_directory path;
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

(* Real Java property files are more complex than this, but we only
   need to be able to read back what we've written. *)
let read_flat_properties filename =
  let decode map line =
    if String.length line > 0 && line.[0] == '#' then map
    else begin
      match String.Exceptionless.split line "=" with
	  None -> failwith (sprintf "Invalid line in property file '%s'" line)
	| Some (key, value) -> StringMap.add key value map
    end in
  let get inp = fold decode StringMap.empty (IO.lines_of inp) in
  with_dispose ~dispose:close_in get (open_in filename)

type props = {
  p_uuid: Uuidm.t;
  p_limit: int;
  p_newfile: bool }

let decode_backup_properties map =
  { p_uuid = Option.get **> Uuidm.of_string **> StringMap.find "uuid" map;
    p_limit = Option.map_default int_of_string default_limit **>
      StringMap.Exceptionless.find "limit" map;
    p_newfile = Option.map_default bool_of_string false **>
      StringMap.Exceptionless.find "newfile" map }

let to_index_name path =
  if String.ends_with path ".data" then
    String.sub path 0 (String.length path - 5) ^ ".idx"
  else
    failwith **> "Malformed datafile name: " ^ path

let data_re = Str.regexp "^pool-data-\\([0-9][0-9][0-9][0-9]\\)\\.data$"
(* Note that Str doesn't appear to be reentrant. *)

let make_pool_name path idx = Filename.concat path (sprintf "pool-data-%04d.data" idx)

type node = {
  n_file: Chunk.chunk_file;
  n_index: File_index.t;
  n_path: string }

let find_backup_nodes path =
  let redata name =
    if Str.string_match data_re name 0 then
      Some (int_of_string **> Str.matched_group 1 name)
    else None in
  let nums = Sys.files_of path //@ redata in
  let nums = List.sort ~cmp:(fun a b -> compare b a) (List.of_enum nums) in

  let lookup num =
    let fname = make_pool_name path num in
    let file = Chunk.open_chunk_file fname in
    let index = File_index.make **> to_index_name fname in
    index#load file#size; (* TODO: handle exception *)
    { n_file = file; n_index = index; n_path = fname } in
  List.map lookup nums

(* TODO: Handle newfile. *)
class type file_pool =
object
  method add : Chunk.t -> unit
  method find : Hash.t -> Chunk.t
  method find_option : Hash.t -> Chunk.t option

  method find_full : Hash.t -> ((unit -> Chunk.t) * (unit -> Chunk.info) * string) option

  method get_backups : Hash.t list

  method flush : unit
  method close : unit
end

type t = file_pool

let open_file_pool path =
  ensure_directory path;
  let metadata = Filename.concat path "metadata" in
  let props_name = Filename.concat metadata "props.txt" in

object (self)
  val mutable props = decode_backup_properties **> read_flat_properties props_name
  val mutable nodes = find_backup_nodes path
  val mutable dirty = false

  method private cur_file = match nodes with
      [] -> raise Not_found
    | ({ n_file=file } :: _) -> file
  method private cur_index = match nodes with
      [] -> raise Not_found
    | ({ n_index=index } :: _) -> index

  method private next_name =
    if nodes <> [] then failwith "TODO: next_name";
    make_pool_name path 0

  (* Is there room for [chunk] in the current pool file? *)
  method private room chunk =
    nodes <> [] && self#cur_file#size + chunk#write_size <= props.p_limit

  method private make_new_pool_file =
    self#flush;
    let fname = self#next_name in
    let file = Chunk.open_chunk_file fname in
    let index = File_index.make (to_index_name fname) in
    nodes <- { n_file=file; n_index=index; n_path=fname } :: nodes

  method add chunk =
    if not (self#room chunk) then self#make_new_pool_file;
    let file = self#cur_file in
    let index = self#cur_index in
    let pos = file#append chunk in
    index#add chunk#hash pos chunk#kind;
    dirty <- true

  method find_full hash =
    let lookup node =
      let info = node.n_index#find_option hash in
      Option.map (fun (pos, kind) ->
	((fun () -> node.n_file#read pos),
	 (fun () -> node.n_file#read_info pos),
	 kind)) info in
    List.Exceptionless.find_map lookup nodes

  method find_option hash =
    Option.map (fun (chunk, _, _) -> chunk ()) (self#find_full hash)

  method find hash = Option.get **> self#find_option hash

  (* method flush_files = *)
  (*   match nodes with *)
  (* 	[] -> () *)
  (*     | ({ n_file=file} :: _ ) -> file#flush *)

  method flush =
    if dirty then begin
    match nodes with
	[] -> ()
      | ({ n_file=file; n_index=index } :: _) ->
	file#flush;
	index#save file#size
    end;
    dirty <- false

  method close =
    self#flush;
    List.iter (fun n -> n.n_file#close) nodes

  method get_backups =
    let backups_name = Filename.concat metadata "backups.txt" in
    try
      let get inp = List.of_enum **> Enum.map Hash.of_string **> IO.lines_of inp in
      with_dispose ~dispose:close_in get (open_in backups_name)
    with
	Sys_error _ -> []
    
end

let with_file_pool path f =
  with_dispose ~dispose:(fun x -> x#close) f
    (open_file_pool path)
