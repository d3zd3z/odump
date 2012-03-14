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
  | BlobNode of string
  | OtherNode

val get : File_pool.t -> Hash.t -> node

(** The [visitor] handles the traversal. *)
class type visitor =
object
  method want_full_data : bool
  method data_summary : string -> Chunk.info -> unit
  method enter : string -> Chunk.t -> node -> unit
  method leave : string -> Chunk.t -> node -> unit
end

(** A visitor that doesn't do anything.  Useful to inherit from to
    override some of the behavior. *)
class virtual empty_visitor : visitor

(** [Nodes.walk pool path hash visitor] Does a traversal of the given
    backup.  For each node, calls [visitor#enter path node], then any
    children nodes, and then [visitor#leave path node].  If the
    visitor has [#want_full_data] as [true], then enter/leave will be
    called for each data chunk, otherwise the info about the data
    chunks will be passed to [#data_summary]. *)
val walk : File_pool.t -> string -> Hash.t -> visitor -> unit
