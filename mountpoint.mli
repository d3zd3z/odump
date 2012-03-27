(* Mountpoint resolver *)

(* Given a pathname of a tree to be backed up, attempt to resolve this
   into a unique identifier for that. *)

val cache_path : string -> string
(** [cache_path backup_dir] Given a base directory 'base' (ending
    with a slash if necessary), return the pathname of a cache file
    appropriate to uniquely identify this backup. *)

val make_cache_path : string -> string -> string
(** [make_cache_path base backup_dir] Like [cache_path], but also will
    make the necessary subdirectory if not present.  Returns the full
    resolved path. *)
