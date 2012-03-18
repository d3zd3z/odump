(* Saving of backups *)

open Batteries_uni

module StringMap = Map.StringMap

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
  let ind = Indirect.make_indirect pool "ind" (256*1024) in
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
	let buf = Indirect.Dir.make pool (256*1024) in
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
  Log.info (fun () -> "Completed backup", ["hash", Hash.to_string hash])
