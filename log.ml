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

let clear_progress () =
  if !last_progress_lines > 0 then begin
    Printf.fprintf stderr "\x1b[%dA\x1b[J" !last_progress_lines;
    (* flush stderr; *)
    last_progress_lines := 0
  end

let show_progress text =
  if has_console then begin
    clear_progress ();
    last_progress_lines := count_chars text '\n';
    last_progress := text;
    output_string stderr text;
    flush stderr
  end

(* Show a message, and a newline, interleaving correctly with the
   progress meter. *)
let with_output f =
  let prior_count = !last_progress_lines in
  clear_progress ();
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
  let prior_count = !last_progress_lines in
  clear_progress ();
  output_string stderr text;
  if prior_count > 0 then
    show_progress !last_progress
  else
    flush stderr

(* Print a simple message to stdout, interleaving correctly with any
   progress meter. *)

(* Register a formater that intermingles correctly with the progress
   meter. *)
let _ = init ["odump", INFO] mingled_formatter

class type meter_type = object
  method get_text : string
  method clear : unit
  method force : unit
  method update : unit
  method finish : unit
end

class virtual meter =
object (self)
  val mutable last_update = Unix.time ()
  val start_time = Unix.gettimeofday ()

  method virtual get_text: string

  method update =
    let now = Unix.time () in
    if now > last_update then begin
      show_progress self#get_text;
      last_update <- now
    end

  method force =
    show_progress self#get_text;
    last_update <- Unix.time ()

  method clear =
    clear_progress ()

  method finish =
    self#force;
    last_progress_lines := 0;
    last_progress := ""
end
