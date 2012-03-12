(* Backup nodes *)

open Batteries_uni

type node =
  | BackupNode of float * string Map.StringMap.t
  | OtherNode

val get : File_pool.t -> Hash.t -> node
