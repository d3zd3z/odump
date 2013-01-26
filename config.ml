(* The odump config file. *)

open Batteries
module C = Config_file

let group = new Config_file.group
let pool = new Config_file.option_cp Config_file.string_wrappers
  ~group:group ["defaults";"pool"] None "Default pool"

let surelog = new Config_file.string_cp
  ~group:group ["defaults";"surelog"] "/tmp/surelog" "Sure log"

let rsynclog = new Config_file.string_cp
  ~group:group ["defaults";"rsynclog"] "/tmp/rsynclog" "Rsync log"

(* A command, which will default to being looked up in the path. *)
class command_cp name =
  let full_name = ["commands";name] in
  let help = "Path to '" ^ name ^ "' executable" in
object
  inherit [string] Config_file.option_cp Config_file.string_wrappers ~group:group full_name None help as super
  method! get =
    match super#get with
	Some name -> Some name
      | None ->
	Some (Shell_sys.lookup_executable name)
end

let commands = Maps.StringMap.of_enum (List.enum [
  "gosure", new command_cp "gosure";
  "lvcreate", new command_cp "lvcreate";
  "lvremove", new command_cp "lvremove";
  "mount", new command_cp "mount";
  "umount", new command_cp "umount";
  "find", new command_cp "find";
  "rm", new command_cp "rm";
  "rsync", new command_cp "rsync";
  "cp", new command_cp "cp";
])

(* Get an option, or raise an exception. *)
let must section name = function
  | Some o -> o
  | None ->
    Printf.eprintf "Execting field '%s' in %s\n%!" name section;
    exit 1

(* Information about a single filesystem. *)
type filesystem = {
  fs_volume: string;
  fs_base: string;
  fs_clean: string;
  fs_style: string }

let filesystem_wrappers =
  let must name field = must "filesystem" name field in
  let to_raw = fun fs ->
    C.Raw.Section [ "volume", C.string_wrappers.C.to_raw fs.fs_volume;
		    "base", C.string_wrappers.C.to_raw fs.fs_base;
		    "clean", C.string_wrappers.C.to_raw fs.fs_clean;
		    "style", C.string_wrappers.C.to_raw fs.fs_style ] in
  let of_raw = function
    | C.Raw.Section l ->
      let volume = ref None and base = ref None and clean = ref None and style = ref None in
      List.iter
	(fun (field_name, value) -> match field_name with
	  | "volume" -> volume := Some (C.string_wrappers.C.of_raw value)
	  | "base" -> base := Some (C.string_wrappers.C.of_raw value)
	  | "clean" -> clean := Some (C.string_wrappers.C.of_raw value)
	  | "style" -> style := Some (C.string_wrappers.C.of_raw value)
	  | s ->
	    Printf.eprintf "Unexpected field in filesystem '%s'\n" s;
	    exit 1)
	l;
      { fs_volume = must "volume" !volume;
	fs_base = must "base" !base;
	fs_clean = must "clean" !clean;
	fs_style = must "style" !style }
    | r -> raise (C.Wrong_type (fun outchan -> Legacy.Printf.fprintf outchan
      "Raw.Section expected, got %a\n%!" C.Raw.to_channel r))
  in
  { C.to_raw = to_raw;
    C.of_raw = of_raw }

let fs_list_wrappers = Config_file.list_wrappers filesystem_wrappers

type host = {
  host_vol: string;
  host_host: string;
  host_mirror: string;
  host_fs: filesystem list }

let host_wrappers =
  let must name field = must "host" name field in
  let to_raw = fun host ->
    C.Raw.Section [ "vol", C.string_wrappers.C.to_raw host.host_vol;
		    "host", C.string_wrappers.C.to_raw host.host_host;
		    "mirror", C.string_wrappers.C.to_raw host.host_mirror;
		    "fs", fs_list_wrappers.C.to_raw host.host_fs ] in
  let of_raw = function
    | C.Raw.Section l ->
      let vol = ref None and host = ref None and mirror = ref None and fs = ref None in
      List.iter
	(fun (field_name, value) -> match field_name with
	  | "vol" -> vol := Some (C.string_wrappers.C.of_raw value)
	  | "host" -> host := Some (C.string_wrappers.C.of_raw value)
	  | "mirror" -> mirror := Some (C.string_wrappers.C.of_raw value)
	  | "fs" -> fs := Some (fs_list_wrappers.C.of_raw value)
	  | s ->
	    Printf.eprintf "Unexpected field in filesystem '%s'\n" s;
	    exit 1)
	l;
      { host_vol = must "vol" !vol;
	host_host = must "host" !host;
	host_mirror = must "mirror" !mirror;
	host_fs = must "fs" !fs }
    | r -> raise (C.Wrong_type (fun outchan -> Legacy.Printf.fprintf outchan
      "Raw.Section expected, got %a\n%!" C.Raw.to_channel r))
  in
  { C.to_raw = to_raw;
    C.of_raw = of_raw }

let sample_host = { host_vol = "volume";
		    host_host = "hostname";
		    host_mirror = "/mnt/mirrors/hostname";
		    host_fs = [ { fs_volume = "boot";
				  fs_base = "/boot";
				  fs_clean = "/boot/clean.sh";
				  fs_style = "plain" };
				{ fs_volume = "root";
				  fs_base = "/";
				  fs_clean = "/path/to/clean-root.sh";
				  fs_style = "ext4-lvm" } ] }

let hosts = new C.list_cp host_wrappers
  ~group:group ["hosts"] [sample_host]
  "Backup host definitions"

(* The client record. *)
type client = {
  client_name: string;
  client_command: string * (string list);
  client_db_dir: string }

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
    | r -> raise (C.Wrong_type (fun outchan -> Legacy.Printf.fprintf outchan
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
    | Sys_error (msg) -> Log.warnf "Unable to write default config file (%s) %S" msg path)

let bogus_client = { client_name = ""; client_command = "", []; client_db_dir = "" }
