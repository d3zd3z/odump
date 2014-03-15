(* SQL storage pools. *)

val create_sql_pool : string -> unit
val open_sql_pool : string -> Pool.writable
