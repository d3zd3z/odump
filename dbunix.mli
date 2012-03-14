(* My unix lib. *)

type dir_handle
val opendir : string -> dir_handle
val readdir : dir_handle -> (string * int64)
val closedir : dir_handle -> unit

type file_descr
val open_for_read : string -> file_descr

(* Convenience utility *)
val get_directory_contents : string -> (string * int64) list

val lstat : string -> (string * (string * string) list)
