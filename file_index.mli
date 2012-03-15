(* File index. *)

class type file_index =
object
  method add: Hash.t -> int -> string -> unit
  (** [#add hash pos kind] *)

  method mem: Hash.t -> bool
  method find: Hash.t -> (int * string)
  method find_option: Hash.t -> (int * string) option

  method find_offset: Hash.t -> int option
  method count: int
  (** Return the number of elements stored in this index. *)

  method load: int -> unit
  method save: int -> unit
  method clear: unit
end

type t = file_index

val make: string -> file_index
