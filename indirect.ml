(* Indirect block management. *)

open Batteries_uni

type t = {
  pool: File_pool.t;
  limit: int;
  prefix: string;
  buffers: Buffer.t Stack.t }

let make_indirect pool prefix limit =
  let limit = (limit / 20) * 20 in
  let core = Buffer.create limit in
  let buffers = Stack.create () in
  Stack.push core buffers;
  { pool = pool; prefix = prefix; limit = limit;
    buffers = buffers }

(* Push a new buffer level, containing the initial hash. *)
let push ind hash =
  let buf = Buffer.create ind.limit in
  Buffer.add_string buf (Hash.get_raw hash);
  Stack.push buf ind.buffers

(* Summarize the buffer at the given level, returning the hash of the summary. *)
let summarize ind buffer level =
  if Buffer.length buffer = 0 then begin
    if level > 0 then
      Log.failure ("Empty has at non-zero level", []);
    (* An empty chunk is allowed, but only at level 0. *)
    let chunk = Chunk.chunk_of_string "null" "" in
    ind.pool#add chunk;
    chunk#hash
  end else if Buffer.length buffer = 20 then begin
    (* If there is a single hash, just use it instead of making an indirect block of one element. *)
    Hash.of_raw (Buffer.contents buffer)
  end else begin
    (* Otherwise make a new chunk out of the data. *)
    let kind = Printf.sprintf "%s%d" ind.prefix level in
    let chunk = Chunk.chunk_of_string kind (Buffer.contents buffer) in
    ind.pool#add chunk;
    chunk#hash
  end

let rec append ind hash level =
  if Stack.is_empty ind.buffers then
    push ind hash
  else if Buffer.length (Stack.top ind.buffers) >= ind.limit then begin
    let summary_hash = summarize ind (Stack.pop ind.buffers) (level+1) in
    append ind summary_hash (level+1);
    push ind hash
  end else
    Buffer.add_string (Stack.top ind.buffers) (Hash.get_raw hash)

let add ind hash = append ind hash 0

let finish ind =
  (* Collapse the buffers. *)
  let level = ref 0 in (* Note this is occasionally wrong *)
  while Stack.length ind.buffers > 1 do
    let tmp = Stack.pop ind.buffers in
    let summary = summarize ind tmp (!level + 1) in
    append ind summary (!level + 1);
    level := !level + 1;
  done;
  let top = Stack.pop ind.buffers in
  summarize ind top 0

(* Builder for directories. *)
module Dir = struct

  let ind_add = add
  let ind_finish = finish
  type ind_t = t

  module SM = Map.StringMap

  type t = { pool: File_pool.t;
	     limit: int;
	     mutable buffer: Hash.t SM.t;
	     mutable length: int;
	     ind: ind_t }

  let make pool limit = { pool = pool;
			  limit = limit;
			  buffer = SM.empty;
			  length = 0;
			  ind = make_indirect pool "dir" limit }

  let ship dir =
    if not (SM.is_empty dir.buffer) then begin
      let node = Nodes.DirNode dir.buffer in
      let hash = Nodes.try_put dir.pool node in
      ind_add dir.ind hash
    end

  let add dir name hash =
    let len = 22 + String.length name in
    if dir.length + len > dir.limit then ship dir;
    dir.length <- dir.length + len;
    dir.buffer <- SM.add name hash dir.buffer

  let finish dir =
    ship dir;
    ind_finish dir.ind

end
