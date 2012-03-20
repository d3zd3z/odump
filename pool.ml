
class type writable =
object
  method add : Chunk.t -> unit
  method mem : Hash.t -> bool
  method find : Hash.t -> Chunk.t
  method find_option : Hash.t -> Chunk.t option
  method find_full : Hash.t -> ((unit -> Chunk.t) * (unit -> Chunk.info) * string) option
  method find_index : Hash.t -> int
  method get_backups : Hash.t list
  method flush : unit
  method close : unit
end
