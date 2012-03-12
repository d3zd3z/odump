(* File storage pools *)

class type file_pool =
object
  method add : Chunk.t -> unit
  method find : Hash.t -> Chunk.t
  method find_option : Hash.t -> Chunk.t option

  method get_backups : Hash.t list

  method flush : unit
  method close : unit
end

type t = file_pool

val create_file_pool : ?limit:int -> ?newfile:bool -> string -> unit
val open_file_pool : string -> file_pool
