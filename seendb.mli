(* Managing the database of seen files *)

val make_cache : #File_pool.file_pool -> string -> Hash.t -> unit
