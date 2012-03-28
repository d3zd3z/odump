(* General storage pools *)

(** A [readable] pool is a source of chunks. *)
class type readable =
object
  (** General queries of the pool. *)
  method mem : Hash.t -> bool
  method find : Hash.t -> Chunk.t
  method find_option : Hash.t -> Chunk.t option

  method find_full : Hash.t -> ((unit -> Chunk.t) * (unit -> Chunk.info) * string) option
  (** Lookup the hash in the pool.  If not found, returns [None].
      Otherwise returns [Some (chunk, info, kind)].  The chunk and the
      info are functions that will read in the given value.  The
      storage pool code is not synchronized, so these values should
      not be called when another thread might be executing pool
      code. *)

  method find_index : Hash.t -> int
  (** [#find_index hash] returns a small integer that is uniquely
      associated with the specified hash.  If the hash is not found,
      raises [Not_found].  These results are only meaningful for a
      given pool until it has data written to it.  Once there is data
      written, the values returned for a given hash are subject to
      change. *)

  method get_backups : Hash.t list
  (** Returns a list of the hashes of chunks that were written with a
      kind of "back". *)

  method close : unit

  method uuid : string
  (** Get the unique string that identifies this backup pool. *)
end

(** A [writable] pool can also have chunks written to it. *)
class type writable =
object
  (* All writable pools can be read from. *)
  inherit readable

  method add : Chunk.t -> unit
  (** [add chunk] adds the given chunk to the storage pool.  If the
      chunk is already present, does nothing. *)

  method flush : unit
end
