(* Backup nodes *)

open Batteries_uni

type indirect_kind =
  | Dir_Indirect
  | Data_Indirect

type node =
  | BackupNode of float * string Map.StringMap.t
  | NodeNode of string * string Map.StringMap.t
  | DirNode of Hash.t Map.StringMap.t
  | IndirectNode of indirect_kind * int * Hash.t array
  | NullNode
  | OtherNode

val get : File_pool.t -> Hash.t -> node
