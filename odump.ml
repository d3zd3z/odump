(* odump driver *)

open Batteries
open Printf

module StringMap = Maps.StringMap

let dump_fmt = "%Y-%m-%d_%H:%M"

let format_date date =
  let nd = Netdate.create ~localzone:true date in
  Netdate.format ~fmt:dump_fmt nd

let show_backup_node hash node = match node with
  | Nodes.BackupNode (date, props) ->
    let props = StringMap.enum (StringMap.remove "hash" props) in
    let buf = Buffer.create 32 in
    let one_prop (key, value) =
      Buffer.add_char buf ' ';
      Buffer.add_string buf key;
      Buffer.add_char buf '=';
      Buffer.add_string buf value in
    Enum.iter one_prop props;
    printf "%s %s%s\n"
      (Hash.to_string hash)
      (format_date date)
      (Buffer.contents buf)
  | _ -> Log.fail "Invalid node"

let backup_compare a b = match (a, b) with
  | (Nodes.BackupNode (da, _), Nodes.BackupNode (db, _)) -> compare da db
  | _ -> Log.fail "Attempt to sort non-backup nodes"

let list path =
  File_pool.with_file_pool path (fun pool ->
    let backups = pool#get_backups in
    let get hash = (hash, Nodes.get pool hash) in
    let backups = List.map get backups in
    let backups = List.sort (fun (_, a) (_, b) -> backup_compare a b) backups in
    List.iter (fun (hash, b) -> show_backup_node hash b) backups)

let tree_show path node =
  match node with
    | Nodes.BackupNode (_time, _props) ->
      printf "back -> %s\n" path;
    | Nodes.NodeNode (kind, _props) ->
      if kind == "DIR" then
	printf "Enter %04s %s\n" kind path
      else
	printf " Node %04s %s\n" kind path
    | _ -> ()

class walk_visitor =
object
  inherit Nodes.empty_visitor

  method! want_full_data = false
  method! enter path _ node =
    tree_show path node;
    match node with
      | Nodes.NodeNode("REG", _) -> raise Nodes.Prune
      | _ -> ()
end

let walk path root_hash =
  File_pool.with_file_pool path (fun pool ->
    let root_hash = Hash.of_string root_hash in
    (* tree_walk pool "." root_hash *)
    Nodes.walk pool "." root_hash (new walk_visitor))

type du_type = {
  du_data: int64;
  du_write: int64;
  du_count: int64;
}
let empty_du = { du_data = 0L; du_write = 0L; du_count = 0L }

let update_du_data data write count du =
  { du_data = Int64.add du.du_data (Int64.of_int data);
    du_write = Int64.add du.du_write (Int64.of_int write);
    du_count = Int64.add du.du_count (Int64.of_int count) }

class du_visitor pool =
object (self)
  inherit Nodes.empty_visitor

  method! want_full_data = false

  val mutable sizes = StringMap.empty

  val seen = BitSet.empty () (* TODO: Get size. *)

  method private update hash kind size write_size =
    let index = pool#find_index hash in
    let (write, count) = if BitSet.mem seen index then (0, 0) else begin
      BitSet.set seen index;
      (write_size, 1)
    end in
    sizes <- StringMap.modify_def empty_du kind (update_du_data size write count) sizes

  method! enter _ chunk _ =
    self#update chunk#hash chunk#kind chunk#data_length chunk#write_size

  method! data_summary _ info =
    self#update info.Chunk.in_hash
      info.Chunk.in_kind
      info.Chunk.in_data_length
      info.Chunk.in_write_size

  method show_result : 'a. (#Pool.readable as 'a) -> Hash.t -> unit = fun pool hash ->
    (* printf "seen size: %d\n" (Size.size_b seen); *)
    (* Display.display "debug.dot" seen; *)
    let node = Nodes.get pool hash in
    (* printf "backup: %s\n" (Hash.to_string hash); *)
    show_backup_node hash node;
    printf "\n";
    printf "kind          data size                  compressed size        count\n";
    printf "---- ---------------------------   ---------------------------  -----\n";
    let each kind info =
      printf "%4s %15Ld (%s)   %15Ld (%s)  (%Ld)\n"
	kind
	info.du_data (Log.nice_number info.du_data)
	info.du_write (Log.nice_number info.du_write)
	info.du_count in
    StringMap.iter each sizes;
    printf "\n"
end

let du path root_hash =
  let root_hash = Hash.of_string root_hash in
  File_pool.with_file_pool path (fun pool ->
    let visitor = new du_visitor pool in
    Nodes.walk pool "." root_hash (visitor :> Nodes.visitor);
    visitor#show_result pool root_hash)

let restore path node dest =
  let node = Hash.of_string node in
  File_pool.with_file_pool path (fun pool ->
    Restore.run_restore pool node dest)

let make_cache path hash backup_dir =
  let cache_name = Mountpoint.make_cache_path path backup_dir in
  let hash = Hash.of_string hash in
  File_pool.with_file_pool path (fun pool ->
    Seendb.make_cache pool cache_name hash)

let show_cache path =
  let cache_name = Mountpoint.cache_path path in
  printf "Cache base name would be: %S\n" cache_name

let create_pool path =
  File_pool.create_file_pool path

let pool_clone src_path dest_path hashes =
  let hashes = List.map Hash.of_string hashes in
  File_pool.with_file_pool src_path (fun src_pool ->
    File_pool.with_file_pool dest_path (fun dest_pool ->
      Clone.clone_trees src_pool dest_pool hashes))

(** {4 Argument processing} *)

type command = {
  help: string; (** Short description *)
  usage: string; (** Usage text. *)
  args: (Arg.key * Arg.spec * Arg.doc) list;
  action: (unit -> unit) -> string list -> unit }

let set_option opt value = opt := Some value

let config_file = ref "/etc/odump.conf"
let config_file_arg = ("-config", Arg.Set_string config_file,
		       "Config file, default: " ^ !config_file)
let pool = ref None
let pool_arg = ("-pool", Arg.String (set_option pool), "Path to storage pool")
let verify_arg = ("-verify-hashes", Arg.Set Chunk.verify_hashes,
		  "Verify hashes while reading")
let must_pool usage =
  match !pool with
    | None ->
      eprintf "Must specify a pool with -pool\n";
      usage ();
      exit 1
    | Some p -> p

let client = ref None
let client_arg = ("-client", Arg.String (set_option client), "Remote client to use")

let command_list usage = function
  | [] -> list (must_pool usage)
  | _ -> usage ()

let command_make_cache usage = function
  | [node; backup_dir] -> make_cache (must_pool usage) node backup_dir
  | _ -> usage ()

let command_show_cache usage = function
  | [path] -> show_cache path
  | _ -> usage ()

let command_du usage = function
  | [node] -> du (must_pool usage) node
  | _ -> usage ()

let command_restore usage = function
  | [node; dest] -> restore (must_pool usage) node dest
  | _ -> usage ()

let command_dump usage = function
  | (root :: att1 :: atts) -> Backup.dump (must_pool usage) root (att1::atts)
  | _ -> usage ()

let command_create_pool usage = function
  | [] -> create_pool (must_pool usage)
  | _ -> usage ()

let command_clone usage = function
  | (dest_path :: hash1 :: hashes) -> pool_clone (must_pool usage) dest_path (hash1 :: hashes)
  | _ -> usage ()

let command_walk usage = function
  | [node] -> walk (must_pool usage) node
  | _ -> usage ()

let command_remote_ping usage = function
  | [] -> Remote_host.ping ()
  | _ -> usage ()

let command_receiver usage = function
  | [] -> Remote_receiver.process ()
  | _ -> usage ()

let command_version usage = function
  | [] -> printf "odump version %s\n" Version.version
  | _ -> usage ()

let command_verify usage = function
  | (file :: files) -> Verify.verify (file :: files)
  | _ -> usage ()

let command_managed usage = function
  | [host] -> Managed.managed (must_pool usage) host
  | _ -> usage ()

let remote_commands = Maps.StringMap.of_enum (List.enum [
  "ping", { help = "Test a remote connection";
	    usage = "odump remote -client name ping";
	    args = [ client_arg ];
	    action = command_remote_ping }
])

let command_remote usage = function
  | [] -> Log.fail "Must specify remote command"
  | (command :: _) as command_line ->
    Remote_host.client := begin match !client with
      | None -> Log.warn "Must specify -client <name>"; usage (); exit 1
      | Some c ->
	(try List.find (fun n -> n.Config.client_name = c) Config.clients#get
	 with Not_found -> Log.failf "Unknown client: %S" c)
    end;
    match Maps.StringMap.Exceptionless.find command remote_commands with
      | None ->
	eprintf "Unknown remote command: '%s'\n" command;
	exit 1
      | Some command ->
	let command_line = Array.of_list command_line in
	let args = ref [] in
	let current = ref 0 in
	let add_arg arg = args := arg :: !args in
	Arg.parse_argv ~current:current command_line command.args add_arg command.usage;
	let show_use = fun () ->
	  eprintf "%s\n\nusage: " command.help;
	  Arg.usage command.args command.usage;
	  exit 1 in
	command.action show_use (List.rev !args)

let commands = Maps.StringMap.of_enum (List.enum [
  "list", { help = "List backups available in a pool";
	    usage = "odump list -pool <path>";
	    args = [ pool_arg ];
	    action = command_list };
  "make-cache", { help = "Update a visited file cache";
		  usage = "odump make-cache -pool <path> <hash> <path>";
		  args = [ pool_arg ];
		  action = command_make_cache };
  "show-cache", { help = "Show name of cache file";
		  usage = "odump show-cache <path>";
		  args = [ ];
		  action = command_show_cache };
  "du", { help = "Show space used";
	  usage = "odump du -pool <path> <hash>";
	  args = [ pool_arg ];
	  action = command_du };
  "restore", { help = "Restore a backup";
	       usage = "restore -pool <path> <hash> <destdir>";
	       args = [ pool_arg ];
	       action = command_restore };
  "dump", { help = "Make a backup of a directory";
	    usage = "dump -pool <path> <root> key=value ...";
	    args = [ pool_arg ];
	    action = command_dump };
  "create-pool", { help = "Create a new storage pool in an empty directory";
		   usage = "create-pool -pool <path>";
		   args = [ pool_arg ];
		   action = command_create_pool };
  "clone", { help = "Clone a backup to a new pool";
	     usage = "clone -pool <path> <dest-pool-path> <hashes>";
	     args = [ pool_arg ];
	     action = command_clone };
  "walk", { help = "Show the contents of a backup";
	    usage = "walk -pool <path> <hash>";
	    args = [ pool_arg ];
	    action = command_walk };
  "remote", { help = "Run a remote command";
	      usage = "remote -client <name> command...";
	      args = [ client_arg ];
	      action = command_remote };
  "receiver", { help = "Remote receiver (don't use directly)";
		usage = "intentionally undocumented";
		args = [];
		action = command_receiver };
  "version", { help = "Show program version";
	       usage = "version";
	       args = [];
	       action = command_version };
  "verify", { help = "Verify pool file(s)";
              usage = "verify file1.dat file2.dat ...";
              args = [ ];
              action = command_verify };
  "managed", { help = "Perform a managed backup";
	       usage = "managed host";
	       args = [];
	       action = command_managed };
])

let usage () =
  let out = IO.output_string () in
  let fmt = Format.formatter_of_output out in
  let names = Maps.StringMap.enum commands in
  Format.fprintf fmt "usage: odump <command> [<args>]@\n@\n";
  Format.fprintf fmt "commands: @[";
  Enum.iter (fun (name, {help; _}) -> Format.fprintf fmt "%-12s %s@\n" name help) names;
  Format.fprintf fmt "@]@\n";
  Format.fprintf fmt "Use 'odump command --help' for more information on a specific command.@\n@\n";
  Format.fprintf fmt "Global options:";
  IO.close_out out

exception Got_command

let main () =
  (* Scan the arguments, stopping at the first anonymous argument.
     This parses the arguments up until the command name. *)
  let global_usage = usage () in
  let global_args = [ pool_arg; verify_arg; config_file_arg ] in
  begin try Arg.parse global_args (fun _arg -> raise Got_command) global_usage;
	    Arg.usage global_args global_usage;
	    exit 1
    with Got_command -> ()
  end;
  Config.load_config !config_file;

  (* If the pool was not specified on the command line, use a possible
     value from the config file. *)
  if !pool = None then pool := Config.pool#get;

  match Maps.StringMap.Exceptionless.find Sys.argv.(!Arg.current) commands with
    | None ->
      eprintf "Unknown command: '%s'\n" Sys.argv.(!Arg.current);
      Arg.usage [] global_usage;
      exit 1
    | Some command ->
      let args = ref [] in
      let add_arg arg = args := arg :: !args in
      Arg.parse command.args add_arg command.usage;
      let show_use = fun () ->
	eprintf "%s\n\nusage: " command.help;
	Arg.usage command.args command.usage;
	exit 1 in
      command.action show_use (List.rev !args)

let () = main ()
