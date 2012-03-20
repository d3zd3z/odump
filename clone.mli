(* Cloning trees from one pool to another *)

val clone_trees : #Pool.readable -> #Pool.writable -> Hash.t list -> unit
(** [clone_trees src_pool dest_pool hashes] copies the backup trees
    listed in [hashes] from [src_pool] to [dest_pool] *)
