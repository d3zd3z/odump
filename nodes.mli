(* Backup nodes *)

(* open Batteries *)

type indirect_kind =
  | Dir_Indirect
  | Data_Indirect

type node =
  | BackupNode of float * string Maps.StringMap.t
  | NodeNode of string * string Maps.StringMap.t
  | DirNode of Hash.t Maps.StringMap.t
  | IndirectNode of indirect_kind * int * Hash.t array
  | NullNode
  | BlobNode of string
  | XattrNode of string Maps.StringMap.t

val get : #Pool.readable -> Hash.t -> node

val put : #Pool.writable -> node -> Hash.t
(** [put pool node] Writes the node encoded into the pool.  Does not
    catch [File_pool.Already_present]. *)

val try_put : #Pool.writable -> node -> Hash.t
(** [try_put pool node] Same as [put] but catches and ignores the File_pool.Already_present exception. *)

exception Prune

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
val walk : ?aux_meter:(unit -> string) ->
  #Pool.readable -> string -> Hash.t -> visitor -> unit
