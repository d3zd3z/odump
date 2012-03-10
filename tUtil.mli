(* Test utilities. *)

val with_temp_dir: (string -> 'a) -> unit -> 'a
(** [TUtil.with_temp_dir op] Create a temporary directory, and call
    [op] passing the pathname of the temporary directory.  Cleans up
    the temporary directory before returning the value returned by
    [op]. *)
