(* My unix lib. *)

open Batteries_uni

type dir_handle
val opendir : string -> dir_handle
val readdir : dir_handle -> (string * int64)
val closedir : dir_handle -> unit

type file_descr
val open_for_read : string -> file_descr

(* Convenience utility *)
val get_directory_contents : string -> (string * int64) list

val lstat : string -> (string * (string * string) list)

val restore_stat : string -> string -> string Map.StringMap.t -> unit
(** [Dbunix.restore_stat path kind stats] restores the file stats
    appropriately for the kind of file. *)

val float_of_time : string -> float
(** Decode an attribute representing a time into a stdlib float time. *)
