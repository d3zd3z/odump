(* Indirect buffer management. *)

type t

val make_indirect : File_pool.t -> string -> int -> t
val add : t -> Hash.t -> unit
val finish : t -> Hash.t

module Dir : sig
  type t
  val make : File_pool.t -> int -> t
  val add : t -> string -> Hash.t -> unit
  val finish : t -> Hash.t
end
