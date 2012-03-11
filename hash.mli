(* SHA1 hashes. *)

type t
val of_data: string list -> t
val to_string: t -> string
val of_string: string -> t

val of_raw: string -> t
val get_raw: t -> string

val succ: t -> t
val pred: t -> t

val peek_byte: t -> int -> int

(* To satisfy Batteries.Interfaces.OrderedType. *)
val compare: t -> t -> int
