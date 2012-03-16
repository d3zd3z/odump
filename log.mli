(* Logger. *)

open Batteries_uni

(* The logger to use within the program. *)
val odump : Logger.log
type event = Logger.event
type log = Logger.log
type level = Logger.level
val log : log -> level -> (unit -> event) -> unit

(* Log failure, and then exit. *)
val failure : event -> 'a
val warn : (unit -> event) -> unit
