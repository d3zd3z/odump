(* Wrapper around SQL database. *)

type schema_info = {
  schema_version : string;
  schema_text : string array;
}
type t

(* Re-exporting the data types from Sqlite3. *)
(* Note that the 'module type of' is new in ocaml 3.12. *)
module Data : module type of Sqlite3.Data

val connect : string -> schema_info -> t
val close : t -> unit

(* Some useful queries. *)

val sql0 : t -> string -> Data.t list -> unit
(** [sql0 db query args] Run the given query, expecting no results. *)

val sql1 : t -> string -> Data.t list -> Data.t array
(** [sql0 db query args] Run the given query, expecting a single row as a result. *)

val sqln : t -> string -> Data.t list -> Data.t array list
(** [sql0 db query args] Run the given query, returning all of the result rows. *)

