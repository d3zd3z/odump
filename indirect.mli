(* Indirect buffer management. *)

type t

val make_indirect : #File_pool.file_pool -> string -> int -> t
val add : t -> Hash.t -> unit
val finish : t -> Hash.t

module Dir : sig
  type t
  val make : #File_pool.file_pool -> int -> t
  val add : t -> string -> Hash.t -> unit
  val finish : t -> Hash.t
end
