(* My unix lib. *)

type dir_handle
val opendir : string -> dir_handle
val readdir : dir_handle -> (string * int64)
val closedir : dir_handle -> unit

(* Convenience utility *)
val get_directory_contents : string -> (string * int64) list
