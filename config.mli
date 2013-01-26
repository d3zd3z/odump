(* Config parsing. *)

val load_config : string -> unit

val pool : (string option) Config_file.cp
val surelog : string Config_file.cp
val rsynclog : string Config_file.cp

type client = {
  client_name: string;
  client_command: string * (string list);
  client_db_dir: string }

val clients : client list Config_file.cp
val bogus_client : client

(* Backup host definition. *)
type filesystem = {
  fs_volume: string;
  fs_base: string;
  fs_clean: string;
  fs_style: string }

type host = {
  host_vol: string;
  host_host: string;
  host_mirror: string;
  host_fs: filesystem list }

val hosts : host list Config_file.cp

(* Commands found in the path. *)
val commands : (string option Config_file.cp) Maps.StringMap.t
