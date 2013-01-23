(* Managing the database of seen files *)

open Batteries

val make_cache : #Pool.readable -> string -> Hash.t -> unit
(** [make_cache pool path hash] updates the seen database at the given
    path with data from the specified backup. *)

module Int64Map : Map.S with type key = int64

type dir_node = {
  n_inode : int64;
  n_ctime : float;
  n_expire : float;
  n_hash : Hash.t
}

type cache_entry = dir_node Int64Map.t
(** The type of cached data for a single directory.  It maps from
    inode number to the data about that inode.  If the inode and c_time
    match, the file is unlikely to have been changed.  The expire time is
    the date that this entry will expire. *)

val entry_of_node : Hash.t -> string Maps.StringMap.t -> dir_node
(** [entry_of_node hash props] takes the properties from a backup node
    and extracts a dir_node from it, generating a fresh expiration. *)

val randomize_expire : unit -> float
(** Return a randomized expiration time.  Randomization helps reduce
    the chance of large numbers of entries expiring at the same time
    (resulting in lots of rehashing), while still ensuring that things are
    refreshed periodically.  Currently, it is a value evenly distributed
    between 2-6 weeks in the future. *)

type t
(** Represents a connection to an open seen database. *)

val open_cache : string -> t
(** Opens or creates a cache at the given file. *)

val close : t -> unit

val with_cache : string -> (t -> 'a) -> 'a
(** Convenience wrapper to ensure the cache is closed properly on
    return from the function. *)

val get : t -> int64 -> cache_entry
(** Retrieves the cache entry for a given directory inode.  If the
    directory hasn't been cached, will return an empty map. *)

val update : t -> int64 -> cache_entry -> unit
(** Replace (or add if new) the cache data for a given inode. *)

val begin_transaction : t -> unit
val commit : t -> unit
