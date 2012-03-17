(* Utilities for storing integers in packed binary data. *)

val put32le: string -> int -> int -> unit
val get32le: string -> int -> int
val read_buffer: in_channel -> int -> string

val get16be: string -> int -> int
val put16be: string -> int -> int -> unit

val buffer_add_16be: Buffer.t -> int -> unit
