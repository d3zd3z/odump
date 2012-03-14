(* Backup nodes. *)

open Batteries_uni
open Printf

module StringMap = Map.StringMap

type indirect_kind =
  | Dir_Indirect
  | Data_Indirect

type node =
  | BackupNode of float * string StringMap.t
  | NodeNode of string * string Map.StringMap.t
  | DirNode of Hash.t Map.StringMap.t
  | IndirectNode of indirect_kind * int * Hash.t array
  | NullNode
  | OtherNode

let extract_dir data =
  let len = String.length data in
  let rec loop map pos =
    if pos > len then failwith "extract_dir overflow";
    if pos = len then map else begin
      let name_len = Binary.get16be data pos in
      let name = String.sub data (pos + 2) name_len in
      let hash = String.sub data (pos + 2 + name_len) 20 in
      let hash = Hash.of_raw hash in
      loop (StringMap.add name hash map) (pos + 2 + name_len + 20)
    end in
  loop StringMap.empty 0

let extract_indirect data =
  let len = String.length data in
  if len mod 20 <> 0 then failwith "Invalid indirect data length";
  let len = len / 20 in
  let result = Array.create len Hash.null_hash in
  for i = 0 to len - 1 do
    result.(i) <- Hash.of_raw (String.sub data (20 * i) 20)
  done;
  result

let decode_node chunk =
  match chunk#kind with
    | "back" ->
      let props = Properties.of_java_xml chunk#data in
      let date = (float_of_string **> StringMap.find "_date" props) /. 1000.0 in
      let props = StringMap.remove "_date" props in
      BackupNode (date, props)
    | "node" ->
      let name, props = Properties.of_jpool_xml chunk#data in
      NodeNode (name, props)
    | "dir " ->
      DirNode (extract_dir chunk#data)
    | "dir0" -> IndirectNode (Dir_Indirect, 0, extract_indirect chunk#data)
    | "dir1" -> IndirectNode (Dir_Indirect, 1, extract_indirect chunk#data)
    | "dir2" -> IndirectNode (Dir_Indirect, 2, extract_indirect chunk#data)
    | "dir3" -> IndirectNode (Dir_Indirect, 3, extract_indirect chunk#data)
    | "dir4" -> IndirectNode (Dir_Indirect, 4, extract_indirect chunk#data)
    | "null" -> NullNode
    | kind ->
      Pdump.pdump chunk#data;
      failwith **> sprintf "Unknown node kind: '%s'" kind

let get pool hash =
  let chunk = pool#find hash in
  decode_node chunk

let get_prop_hash name props = Hash.of_string (StringMap.find name props)

class type visitor =
object
  method want_full_data : bool
  method data_summary : string -> Chunk.info -> unit
  method enter : string -> node -> unit
  method leave : string -> node -> unit
end
class virtual empty_visitor : visitor =
object
  method want_full_data = true
  method data_summary _ _ = ()
  method enter _ _ = ()
  method leave _ _ = ()
end

let walk (pool : File_pool.t) path hash (visitor : visitor) =
  let full_data = visitor#want_full_data in
  let rec descend path hash =
    let (chunk_get, info_get, kind) = Option.get (pool#find_full hash) in
    if (not full_data) && kind == "data" then
      visitor#data_summary path (info_get ())
    else begin
      let chunk = chunk_get () in
      let node = decode_node chunk in
      visitor#enter path node;
      begin match node with
	| BackupNode (time, props) ->
	  descend path (get_prop_hash "hash" props)
	| NodeNode (kind, props) when kind = "DIR" ->
	  descend path (get_prop_hash "children" props)
	| DirNode children ->
	  let each name hash =
	    let child_path = Filename.concat path name in
	    descend child_path hash in
	  StringMap.iter each children
	| IndirectNode (_, level, subs) ->
	  Array.iter (descend path) subs
	| _ -> ()
      end;
      visitor#leave path node
    end
  in descend path hash
