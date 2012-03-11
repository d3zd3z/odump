(* File index *)

open Batteries_uni
open LegacyIO

open Binary

module HashMap = Map.Make(Hash)
module StringMap = Map.StringMap
module StringSet = Set.StringSet
module IntMap = Map.IntMap

open Enum.Infix

class type file_index =
object
  method add: Hash.t -> int -> string -> unit
  method mem: Hash.t -> bool
  method find_option: Hash.t -> (int * string) option
  method find: Hash.t -> (int * string)

  method load: int -> unit
  method save: int -> unit
  method clear: unit
end

type t = file_index

(* Enum.drop_while is too lazy to combine well with side effects, so here
   is a strict version. *)
let rec strict_drop_while f enum =
  match Enum.peek enum with
      None -> ()
    | Some item ->
      if f item then begin
	Enum.junk enum;
	strict_drop_while f enum
      end

(* Compute the "top" index.  Given a list of hashes, returns a list of
   offsets for the top-level index.  Each index gives the first hash with
   a first byte greater than that index offset. *)
let compute_top_index hashes =
  let top = Array.make 256 0 in
  let offset = ref 0 in
  for first = 0 to 255 do
    let is_more hash =
      if first >= (Hash.peek_byte hash 0) then begin
	offset := !offset + 1;
	true
      end else false in
    strict_drop_while is_more hashes;
    top.(first) <- !offset
  done;
  top

let index_magic = "ldumpidx"
let index_version = 4
let header_size = 16

let write_header chan file_size =
  let buf = String.create header_size in
  String.blit index_magic 0 buf 0 8;
  put32le buf 8 index_version;
  put32le buf 12 file_size;
  output_string chan buf

let write_top chan enum =
  let hashes = Enum.map (fun (hash, _) -> hash) enum in
  let top = compute_top_index hashes in
  let buf = String.create (4 * Array.length top) in
  for i = 0 to Array.length top - 1 do
    put32le buf (4*i) top.(i)
  done;
  output_string chan buf

let write_hashes chan enum =
  Enum.iter (fun (hash, _) -> output_string chan (Hash.get_raw hash)) enum

let write_offsets chan enum =
  (* The complex merged enum is not fast, so use a growable buffer for this. *)
  let buf = Buffer.create 1024 in
  let each (_, (offset, _)) =
    let tmp = String.create 4 in
    put32le tmp 0 offset;
    Buffer.add_string buf tmp in
  Enum.iter each enum;
  Legacy.Buffer.output_buffer chan buf

(* Compute a mapping of kind names to integer indices starting with 0. *)
let build_kind_map (kinds: string Enum.t) : int StringMap.t =
  let unique_kinds = StringSet.enum (StringSet.of_enum kinds) in
  let pairs = Enum.combine (unique_kinds, Enum.range 0) in
  StringMap.of_enum pairs

let write_kind_header chan kind_map =
  let count = StringMap.cardinal kind_map in
  let buf = String.create (4 + 4 * count) in
  put32le buf 0 count;
  let put pos (kind, _) = String.blit kind 0 buf (4 + 4 * pos) 4 in
  Enum.iteri put (StringMap.enum kind_map);
  output_string chan buf

let write_kinds chan enum =
  let kind_map = build_kind_map (Enum.map (fun (_, (_, kind)) -> kind) (Enum.clone enum)) in
  write_kind_header chan kind_map;
  let put (_, (_, kind)) = output_byte chan (StringMap.find kind kind_map) in
  Enum.iter put enum

(* TODO: Reorder this to put chan first *)
let write_index' file_size (enum: (Hash.t * (int * string)) Enum.t) chan =
  write_header chan file_size;
  write_top chan (Enum.clone enum);
  write_hashes chan (Enum.clone enum);
  write_offsets chan (Enum.clone enum);
  write_kinds chan enum

let write_index path file_size enum =
  let temp_name = path ^ ".tmp" in
  let chan = open_out_bin temp_name in
  begin
    try
      write_index' file_size enum chan;
      (* TODO: get an fsync here. *)
      close_out chan;
    with 
	e -> close_out chan; raise e
  end;
  Unix.rename temp_name path

let read_header chan file_size =
  let buf = read_buffer chan header_size in
  if index_magic <> String.sub buf 0 8 then
    failwith "Invalid index file magic";
  if get32le buf 8 <> 4 then
    failwith "Incorrect index version";
  if get32le buf 12 <> file_size then
    failwith "File size doesn't match index"

let read_top chan =
  let buf = read_buffer chan (4 * 256) in
  let top = Array.create 256 0 in
  for i = 0 to 255 do
    top.(i) <- get32le buf (4 * i)
  done;
  top

let read_offsets chan count =
  let buf = read_buffer chan (4 * count) in
  Array.init count (fun x -> get32le buf (4 * x))

let read_kind_map chan =
  let buf1 = read_buffer chan 4 in
  let count = get32le buf1 0 in
  let buf2 = read_buffer chan (4 * count) in
  let all = Enum.map (fun i -> (i, String.sub buf2 (4 * i) 4)) (0 -- (count - 1)) in
  IntMap.of_enum all

(** {6 Loading} *)
let load_index path file_size =
  let chan = open_in_bin path in
  begin
    try
      read_header chan file_size;
      let top = read_top chan in
      let count = top.(255) in
      let hashes = read_buffer chan (20 * count) in
      let offsets = read_offsets chan count in
      let kind_map = read_kind_map chan in
      let kinds = read_buffer chan count in
      close_in chan;
      (top, hashes, offsets, kind_map, kinds)
    with
	e -> close_in chan; raise e
  end

class type simple_index =
object
  method find : Hash.t -> (int * string) option
  method enum : (Hash.t * (int * string)) Enum.t
end

class empty_index : simple_index =
object
  method find _ = None
  method enum = Enum.empty ()
end

class loaded_index path file_size : simple_index =

  let (top, hashes, offsets, kind_map, kinds) = load_index path file_size in

  let search hash =
    let hash = Hash.get_raw hash in
    let first_byte = Char.code hash.[0] in
    let low = if first_byte > 0 then top.(first_byte-1) else 0 in
    let high = top.(first_byte) - 1 in
    let rec loop low high =
      if high < low then None else begin
	let mid = low + ((high - low) / 2) in
	(* TODO: Don't blit them out, make a compare that works within. *)
	let comp = compare hash (String.sub hashes (20 * mid) 20) in
	if comp < 0 then loop low (mid - 1)
	else if comp > 0 then loop (mid + 1) high
	else Some mid
      end
    in loop low high in

  let get_kind index = IntMap.find (Char.code kinds.[index]) kind_map in

object (self)
  method find hash = match search hash with
      None -> None
    | Some pos -> Some (offsets.(pos), get_kind pos)

  method enum =
    let get idx = (Hash.of_raw (String.sub hashes (20 * idx) 20), (offsets.(idx), get_kind idx)) in
    Enum.map get (0 --^ top.(255))
end

(* BatEnum.merge seems to be broken, let's try a possibly less
   efficient, but hopefully correct version. *)
let merge test a b =
  let next () =
    match (Enum.get a, Enum.get b) with
	(None, None) -> raise Enum.No_more_elements
      | (Some aa, None) -> aa
      | (None, Some bb) -> bb
      | (Some aa, Some bb) ->
	if test aa bb then begin
	  Enum.push b bb;
	  aa
	end else begin
	  Enum.push a aa;
	  bb
	end in
  Enum.from next

class combined_file_index path : file_index =
object (self)
  val mutable ram = (HashMap.empty : (int * string) HashMap.t)
  val mutable file = (new empty_index :> simple_index)

  method add hash pos kind =
    if HashMap.mem hash ram then
      failwith "Attempt to add duplicate key";
    ram <- HashMap.add hash (pos, kind) ram

  method mem hash = match self#find_option hash with
      None -> false
    | Some _ -> true

  method find_option hash =
    match HashMap.Exceptionless.find hash ram with
	Some p -> Some p
      | None -> file#find hash

  method find hash = match self#find_option hash with
      Some p -> p
    | None -> raise Not_found

  method load file_size =
    ram <- HashMap.empty;
    file <- new loaded_index path file_size

  method save file_size =
    let cmp (a, _) (b, _) = Hash.compare a b < 0 in
    write_index path file_size (merge cmp (HashMap.enum ram) file#enum);
    ram <- HashMap.empty;
    self#load file_size

  method clear =
    ram <- HashMap.empty;
    file <- new empty_index
end

let make = new combined_file_index
