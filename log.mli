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
val info : (unit -> event) -> unit
val warn : (unit -> event) -> unit
val debug : (unit -> event) -> unit

val message : string -> unit
val with_output : (unit -> unit) -> unit

(* Progress meter support.  Implement [#get_text] and call [#update]
   to implement a meter. *)
class virtual meter :
object
  val mutable last_update : float
  val start_time : float
  method virtual get_text : string
  method clear : unit
  method force : unit
  method update : unit
  method finish : unit
end
