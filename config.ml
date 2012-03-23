(* The odump config file. *)

let group = new Config_file.group
let pool = new Config_file.option_cp Config_file.string_wrappers
  ~group:group ["defaults";"pool"] None "Default pool"

(* Try loading the given config file.  Note that the Config_file
   parser will try to create the file, with all of the defaults, if it
   doesn't exist.  To allow this to be run as non-root, we catch, and
   ignore the Sys_error. *)
let load_config path =
  (try group#read path with
    | Sys_error (msg) -> Log.warn (fun () -> "Unable to write default config file", ["message", msg]))
