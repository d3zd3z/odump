(* Logging. *)

open Batteries_uni

include Logger

let odump = Logger.make_log "odump"

let failure event =
  log odump FATAL (fun () -> event);
  exit 1

let warn event_fun = log odump WARN event_fun
let info event_fun = log odump INFO event_fun
let debug event_fun = log odump DEBUG event_fun

let has_console = Unix.isatty Unix.stderr

let last_progress_lines = ref 0
let last_progress = ref ""

let count_chars text ch =
  let count = ref 0 in
  for i = 0 to String.length text - 1 do
    if text.[i] == ch then
      count := !count + 1
  done;
  !count

let null_meter () = ""
let current_meter = ref null_meter

let set_meter meter = current_meter := meter

let clear_meter () =
  if !last_progress_lines > 0 then begin
    Printf.fprintf stderr "\x1b[%dA\x1b[J" !last_progress_lines;
    (* flush stderr; *)
    last_progress_lines := 0
  end

let show_progress text =
  if has_console then begin
    clear_meter ();
    last_progress_lines := count_chars text '\n';
    last_progress := text;
    output_string stderr text;
    flush stderr
  end

(* Show a message, and a newline, interleaving correctly with the
   progress meter. *)
let with_output f =
  let prior_count = !last_progress_lines in
  clear_meter ();
  flush stderr;
  f ();
  flush stdout;
  flush stderr;
  if prior_count > 0 then
    show_progress !last_progress

let message text = with_output (fun () -> print_string text; print_newline ())

(* 'Format' based formatter. *)
let event_to_string log level (desc, parms) time =
  let out = IO.output_string () in
  let fmt = Format.formatter_of_output out in
  Format.fprintf fmt "%12.3f: @[%s@," time desc;
  let each (key, value) = Format.fprintf fmt "@ %s:%s" key value in
  List.iter each parms;
  Format.fprintf fmt "@.";
  IO.close_out out

let mingled_formatter log level event time =
  let text = event_to_string log level event time in
  with_output (fun () -> output_string stderr text)

(* Register a formater that intermingles correctly with the progress
   meter. *)
let _ = init ["odump", INFO] mingled_formatter

let last_update = ref (Unix.gettimeofday ())

let update_meter () =
  let now = Unix.time () in
  if now > !last_update then begin
    show_progress ((!current_meter) ());
    last_update := now
  end

let restore_meter () =
  if !last_progress_lines = 0 then
    show_progress ((!current_meter) ())

let finish_meter () =
  clear_meter ();
  restore_meter ();
  last_progress_lines := 0;
  last_progress := ""

let build_format_meter f () =
  let out = IO.output_string () in
  let fmt = Format.formatter_of_output out in
  f fmt;
  IO.close_out out

(* Units for nicely printing sizes.  YiB would take 70 bits, so cannot
   be reached by a 64-bit number. *)
let units = ["B"; "Kib"; "MiB"; "GiB"; "TiB"; "PiB"; "EiB"; "ZiB"; "YiB"]
let fnice_number num =
  let rec loop num units =
    if abs_float num > 1024.0 then loop (num /. 1024.0) (List.tl units)
    else (num, List.hd units) in
  let (num, unit) = loop num units in
  Printf.sprintf "%6.1f%-3s" num unit

let nice_number num = fnice_number (Int64.to_float num)

let format_size f fmt size =
  Format.fprintf f fmt (nice_number size)

let format_size_rate f fmt size age =
  let rate = (Int64.to_float size) /. age in
  Format.fprintf f fmt (nice_number size) (fnice_number rate)

let format_ratio f fmt before after =
  let fbefore = Int64.to_float before in
  let fafter = Int64.to_float after in
  let ratio = ((fbefore -. fafter) /. fbefore) *. 100.0 in
  Format.fprintf f fmt ratio
