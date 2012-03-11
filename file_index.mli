(* File index. *)

class type file_index =
object
  method add: Hash.t -> int -> string -> unit
  (** [#add hash pos kind] *)

  method mem: Hash.t -> bool
  method find: Hash.t -> (int * string)
  method find_option: Hash.t -> (int * string) option

  method load: int -> unit
  method save: int -> unit
  method clear: unit
end

val make: string -> file_index