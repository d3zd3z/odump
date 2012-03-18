(* My unix lib. *)

open Batteries_uni

type dir_handle
val opendir : string -> dir_handle
val readdir : dir_handle -> (string * int64)
val closedir : dir_handle -> unit

type file_descr = Unix.file_descr
val open_for_read : string -> file_descr

(* Convenience utility *)
val get_directory_contents : string -> (string * int64) list

type stat = string Map.StringMap.t
type stat_info = string * stat
val lstat : string -> stat_info

val get_int : string -> stat -> int
val get_int64 : string -> stat -> int64
val get_time : string -> stat -> float
val get_hash : string -> stat -> Hash.t

val dir_with_stats : string -> (string * stat_info) list
(** Read the named directory, stat all of the files, and return the
    result, sorted by filename. *)

val restore_stat : string -> string -> string Map.StringMap.t -> unit
(** [Dbunix.restore_stat path kind stats] restores the file stats
    appropriately for the kind of file. *)

val float_of_time : string -> float
(** Decode an attribute representing a time into a stdlib float time. *)

val realpath : string -> string

val mountpoint_of : string -> string
(** Determine the mountpoint associated with this path. *)
