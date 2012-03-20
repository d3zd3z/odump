(* File storage pools *)

val create_file_pool : ?limit:int -> ?newfile:bool -> string -> unit
val open_file_pool : string -> Pool.writable

val with_file_pool : string -> (Pool.writable -> 'a) -> 'a
