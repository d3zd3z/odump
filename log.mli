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

(* The progress meter.  The meter can be set at any time.  It is a
   function that can produce the text of the meter on demand.  The
   [update_meter] call will display it if appropriate (it updates about
   once a second).  [clear_meter ()] will remove the meter display.
   [finish_meter ()] will output the meter text, and arrange to not have
   it cleared when more text needs to be shown. *)
val set_meter : (unit -> string) -> unit
val update_meter : unit -> unit
val clear_meter : unit -> unit
val restore_meter : unit -> unit
val finish_meter : unit -> unit
val null_meter : unit -> string

class type meter_type = object
  method get_text : string
  method clear : unit
  method force : unit
  method update : unit
  method finish : unit
end

(* Progress meter support.  Implement [#get_text] and call [#update]
   to implement a meter. *)
class virtual meter :
object
  method virtual get_text : string
  method clear : unit
  method force : unit
  method update : unit
  method finish : unit
end
