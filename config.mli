(* Config parsing. *)

val load_config : string -> unit

val pool : (string option) Config_file.cp

type client = {
  client_name: string;
  client_command: string * (string list);
  client_db_dir: string }

val clients : client list Config_file.cp
val bogus_client : client
