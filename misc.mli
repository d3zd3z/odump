(* Misc support used by the rest of the code. *)

val ensure_directory : ?what:string -> string -> unit
val ensure_empty_directory : ?what:string -> string -> unit
val nice_number : int64 -> string
val fnice_number : float -> string
