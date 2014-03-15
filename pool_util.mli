(* Accessors for opening pools that tries to determine the pool type. *)
val open_pool : string -> Pool.writable
val with_pool : string -> (Pool.writable -> 'a) -> 'a
