(* Saving backups *)

val save : #File_pool.file_pool -> string -> string -> string list -> unit
(** [save pool cache_dir backup_path atts] *)
