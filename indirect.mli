(* Indirect buffer management. *)

type t

val make_indirect : File_pool.t -> string -> int -> t
val add : t -> Hash.t -> unit
val finish : t -> Hash.t
