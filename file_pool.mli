(* File storage pools *)

val create_file_pool : ?limit:int -> ?newfile:bool -> string -> unit
val open_file_pool : string -> Pool.writable
