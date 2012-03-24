(* Misc support used by the rest of the code. *)

val ensure_directory : ?what:string -> string -> unit
val ensure_empty_directory : ?what:string -> string -> unit
val mkdir_safely : string -> unit
val cache_path : string -> string -> string
