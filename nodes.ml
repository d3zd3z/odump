(* Backup nodes. *)

open Batteries_uni

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
  | BlobNode of string
  | OtherNode

let extract_dir data =
  let len = String.length data in
  let rec loop map pos =
    if pos > len then Log.failure ("extract_dir overflow", []);
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
  if len mod 20 <> 0 then Log.failure ("Invalid indirect data length", []);
  let len = len / 20 in
  let result = Array.create len Hash.null_hash in
  for i = 0 to len - 1 do
    result.(i) <- Hash.of_raw (String.sub data (20 * i) 20)
  done;
  result

let decode_indirect kind ind chunk =
  let level = Char.code kind.[3] - Char.code '0' in
  IndirectNode (ind, level, extract_indirect chunk#data)

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

    (* Make sure this comes after "dir " so that it doesn't catch that. *)
    | kind when String.starts_with kind "dir" ->
      decode_indirect kind Dir_Indirect chunk
    | kind when String.starts_with kind "ind" ->
      decode_indirect kind Dir_Indirect chunk

    | "null" -> NullNode
    | "blob" -> BlobNode (chunk#data)
    | kind ->
      Pdump.pdump chunk#data;
      Log.failure ("Unknown node kind", ["kind", kind])

let get pool hash =
  let chunk = pool#find hash in
  decode_node chunk

let get_prop_hash name props = Hash.of_string (StringMap.find name props)

exception Prune

class type visitor =
object
  method want_full_data : bool
  method data_summary : string -> Chunk.info -> unit
  method enter : string -> Chunk.t -> node -> unit
  method leave : string -> Chunk.t -> node -> unit
end
class virtual empty_visitor : visitor =
object
  method want_full_data = true
  method data_summary _ _ = ()
  method enter _ _ _ = ()
  method leave _ _ _ = ()
end

class node_meter =
object
  inherit Log.meter
  val mutable count = 0L
  val mutable compressed = 0L
  val mutable uncompressed = 0L

  val mutable dirs = 0L
  val mutable nondirs = 0L

  method add_node (chunk : Chunk.t) =
    count <- Int64.succ count;
    uncompressed <- Int64.add uncompressed (Int64.of_int chunk#data_length);
    compressed <- Int64.add compressed (Int64.of_int chunk#write_size)

  method add_dir = dirs <- Int64.succ dirs
  method add_nondir = nondirs <- Int64.succ nondirs

  method get_text =
    let now = Unix.gettimeofday () in
    let out = IO.output_string () in
    let fmt = Format.formatter_of_output out in
    Format.fprintf fmt "%9Ld nodes, %Ld dirs, %Ld nondirs" count dirs nondirs;
    let funcomp = Int64.to_float uncompressed in
    let fcomp = Int64.to_float compressed in
    let rate = funcomp /. (now -. start_time) in
    let zrate = fcomp /. (now -. start_time) in
    let ratio = ((funcomp -. fcomp) /. funcomp) *. 100.0 in
    Format.fprintf fmt " @ %s uncompressed (%s/sec)"
      (Misc.nice_number uncompressed)
      (Misc.fnice_number rate);
    Format.fprintf fmt "@\n%s compressed   (%s/sec)  %.1f%%"
      (Misc.nice_number compressed)
      (Misc.fnice_number zrate)
      ratio;
    Format.fprintf fmt "@.";
    IO.close_out out
end

let walk (pool : File_pool.t) path hash (visitor : visitor) =
  let full_data = visitor#want_full_data in
  let meter = new node_meter in
  let rec descend path hash =
    let (chunk_get, info_get, kind) = Option.get (pool#find_full hash) in
    if (not full_data) && kind == "blob" then
      visitor#data_summary path (info_get ())
    else begin
      let chunk = chunk_get () in
      let node = decode_node chunk in
      meter#add_node chunk;
      begin match node with
	| NodeNode ("DIR", _) -> meter#add_dir
	| NodeNode _ -> meter#add_nondir
	| _ -> ()
      end;
      meter#update;
      begin
	try
	  visitor#enter path chunk node;
	  begin match node with
	    | BackupNode (time, props) ->
	      descend path (get_prop_hash "hash" props)
	    | NodeNode (kind, props) when kind = "DIR" ->
	      descend path (get_prop_hash "children" props)
	    | NodeNode (kind, props) when kind = "REG" ->
	      descend path (get_prop_hash "data" props)
	    | DirNode children ->
	      let each name hash =
		let child_path = Filename.concat path name in
		descend child_path hash in
	      StringMap.iter each children
	    | IndirectNode (_, level, subs) ->
	      Array.iter (descend path) subs
	    | _ -> ()
	  end
	with
	  | Prune -> ()
      end;
      visitor#leave path chunk node
    end
  in descend path hash;
  meter#finish
