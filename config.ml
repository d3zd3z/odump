(* The odump config file. *)

module C = Config_file

let group = new Config_file.group
let pool = new Config_file.option_cp Config_file.string_wrappers
  ~group:group ["defaults";"pool"] None "Default pool"

(* The client record. *)
type client = {
  client_name: string;
  client_command: string * (string list);
  client_db_dir: string }

(* Get an option, or raise an exception. *)
let must section name = function
  | Some o -> o
  | None ->
    Printf.eprintf "Execting field '%s' in %s\n%!" name section;
    exit 1

let string_list_wrappers = C.list_wrappers C.string_wrappers
let command_wrappers = C.tuple2_wrappers C.string_wrappers string_list_wrappers
let client_wrappers =
  let must name field = must "client" name field in
  let to_raw = fun client ->
    C.Raw.Section [ "name", C.string_wrappers.C.to_raw client.client_name;
		    "command", command_wrappers.C.to_raw client.client_command;
		    "db_dir", C.string_wrappers.C.to_raw client.client_db_dir ] in
  let of_raw = function
    | C.Raw.Section l ->
      let name = ref None and command = ref None and db_dir = ref None in
      List.iter
	(fun (field_name, value) -> match field_name with
	  | "name" -> name := Some (C.string_wrappers.C.of_raw value)
	  | "command" -> command := Some (command_wrappers.C.of_raw value)
	  | "db_dir" -> db_dir := Some (C.string_wrappers.C.of_raw value)
	  | s ->
	    Printf.eprintf "Unexpected field in client list '%s'\n" s;
	    exit 1)
	l;
      {
	client_name = must "name" !name;
	client_command = must "command" !command;
	client_db_dir = must "db_dir" !db_dir }
    | r -> raise (C.Wrong_type (fun outchan -> Printf.fprintf outchan
      "Raw.Section expected, got %a\n%!" C.Raw.to_channel r))
 in
  { C.to_raw = to_raw;
    C.of_raw = of_raw }

let clients = new C.list_cp client_wrappers
  ~group:group ["clients"] [ { client_name = "sample";
			       client_command = ("ssh", ["sample.example.com"; "odump"; "-client"]);
			       client_db_dir = "/var/lib/odump" } ]
  "List of remote clients"

(* Try loading the given config file.  Note that the Config_file
   parser will try to create the file, with all of the defaults, if it
   doesn't exist.  To allow this to be run as non-root, we catch, and
   ignore the Sys_error. *)
let load_config path =
  (try group#read path with
    | Sys_error (msg) -> Log.warn (fun () -> "Unable to write default config file", ["message", msg]))

let bogus_client = { client_name = ""; client_command = "", []; client_db_dir = "" }
