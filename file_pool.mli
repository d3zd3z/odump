(* File storage pools *)

class type file_pool =
object
  method add : Chunk.t -> unit
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

  method flush : unit
  method close : unit
end

type t = file_pool

val create_file_pool : ?limit:int -> ?newfile:bool -> string -> unit
val open_file_pool : string -> file_pool

val with_file_pool : string -> (file_pool -> 'a) -> 'a
