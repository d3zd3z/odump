(* Logging. *)

open Batteries_uni

include Logger

let odump = Logger.make_log "odump"

let failure event =
  log odump FATAL (fun () -> event);
  exit 1

let warn event_fun = log odump WARN event_fun

(* Register a logger that just prints stuff out. *)
let _ = init ["odump", DEBUG] stderr_formatter
