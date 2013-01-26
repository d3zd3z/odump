(* Logger. *)

open Batteries

val log : Netlog.level -> string -> unit
val logf : Netlog.level -> ('a, unit, string, unit) format4 -> 'a

(* Log failure, and then exit. *)
val fail : string -> 'a
val failf : ('a, 'b BatInnerIO.output, unit, 'b) format4 -> 'a

(* Wrappers for the various levels. *)
val info : string -> unit
val infof : ('a, 'b BatInnerIO.output, unit, unit) format4 -> 'a
val warn : string -> unit
val warnf : ('a, 'b BatInnerIO.output, unit, unit) format4 -> 'a
val debug : string -> unit
val debugf : ('a, 'b BatInnerIO.output, unit, unit) format4 -> 'a

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

(* Build a meter up that formats. *)
val build_format_meter : (Format.formatter -> unit) -> (unit -> string)

(* Some useful formatters *)
val nice_number : int64 -> string
val fnice_number : float -> string
val format_size :
  Format.formatter ->
  (string -> unit, Format.formatter, unit) format ->
  int64 -> unit
val format_size_rate :
  Format.formatter ->
  (string -> string -> unit, Format.formatter, unit) format ->
  int64 -> float -> unit
val format_ratio :
  Format.formatter ->
  (float -> unit, Format.formatter, unit) format ->
  int64 -> int64 -> unit
