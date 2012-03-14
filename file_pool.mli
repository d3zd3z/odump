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

  method get_backups : Hash.t list

  method flush : unit
  method close : unit
end

type t = file_pool

val create_file_pool : ?limit:int -> ?newfile:bool -> string -> unit
val open_file_pool : string -> file_pool
