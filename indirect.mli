(* Indirect buffer management. *)

type t

val make_indirect : #Pool.writable -> string -> int -> t
val add : t -> Hash.t -> unit
val finish : t -> Hash.t

module Dir : sig
  type t
  val make : #Pool.writable -> int -> t
  val add : t -> string -> Hash.t -> unit
  val finish : t -> Hash.t
end
