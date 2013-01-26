(* Saving backups *)

val save : #Pool.writable -> string -> string -> string list -> unit
(** [save pool cache_dir backup_path atts] *)

val dump : string -> string -> string list -> unit
(** [dump pool_path backup_path atts] *)
