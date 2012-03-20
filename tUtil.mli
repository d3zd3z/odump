(* Test utilities. *)

val with_temp_dir: (string -> 'a) -> unit -> 'a
(** [TUtil.with_temp_dir op] Create a temporary directory, and call
    [op] passing the pathname of the temporary directory.  Cleans up
    the temporary directory before returning the value returned by
    [op]. *)

val with_temp_pool: (string -> File_pool.file_pool -> 'a) -> unit -> 'a
(** [TUtil.with_temp_pool op] Creates a temporary directory, and then
    a pool within it, and calls [op] passing the path of the pool and
    the pool.  Cleans up when [op] returns. *)

val make_random_string: int -> int -> string
(** [TUtil.make_random_string size seed] generate a pseudo-random
    string of length [size] using [seed] for the randomness.  The string
    will start with the [seed] and [size], so should be distinct even when
    fairly small. *)

val make_random_chunk: ?kind:string -> int -> int -> Chunk.t
(** [TUtil.make_random_chunk size seed] generates a pseudo-random
    chunk.  The data of the chunk itself will be the result of calling
    [make_random_chunk] with the same parameters.  The default [kind] is
    "blob". *)

val do_cleanup: bool ref

val copy_file: string -> string -> unit
(** [copy_file src dest] Copy the contents of the file src into dest. *)
