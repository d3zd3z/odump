(* SHA1 hashes. *)

type t
val of_data: string list -> t
val to_string: t -> string
val of_string: string -> t

val of_raw: string -> t

val succ: t -> t
val pred: t -> t
