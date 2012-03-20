(* odump driver *)

open Batteries_uni
open Printf

module StringMap = Map.StringMap

let dump_fmt = "%Y-%m-%d_%H:%M"

let format_date date =
  let tm = Unix.localtime date in
  sprintf "%04d-%02d-%02d %02d:%02d"
    (tm.Unix.tm_year + 1900)
    tm.Unix.tm_mon
    tm.Unix.tm_mday
    tm.Unix.tm_hour
    tm.Unix.tm_min

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
  | _ -> Log.failure ("Invalid node", [])

let backup_compare a b = match (a, b) with
  | (Nodes.BackupNode (da, _), Nodes.BackupNode (db, _)) -> compare da db
  | _ -> Log.failure ("Attempt to sort non-backup nodes", [])

let list path =
  File_pool.with_file_pool path (fun pool ->
    let backups = pool#get_backups in
    let get hash = (hash, Nodes.get pool hash) in
    let backups = List.map get backups in
    let backups = List.sort ~cmp:(fun (_, a) (_, b) -> backup_compare a b) backups in
    List.iter (fun (hash, b) -> show_backup_node hash b) backups)

let tree_show path node =
  match node with
    | Nodes.BackupNode (time, props) ->
      printf "back -> %s\n" path;
    | Nodes.NodeNode (kind, props) ->
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

module HashSet = Set.Make(Hash)

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

  val mutable sizes = StringMap.empty

  val seen = BitSet.empty () (* TODO: Get size. *)

  method private update hash kind size write_size =
    let index = pool#find_index hash in
    let (write, count) = if BitSet.is_set seen index then (0, 0) else begin
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

let mkdir_safely path =
  try Unix.mkdir path 0o755
  with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

(* Convert a pathname (with slashes) into a name that can be used as a
   filename.  Slashes are converted to hyphens, and hyphens are converted
   to doubled hyphens. *)
let flatten_path path prefix suffix =
  let buf = Buffer.create (String.length prefix + String.length path + 10) in
  Buffer.add_string buf prefix;
  let each = function
    | '/' -> Buffer.add_char buf '-'
    | '-' -> Buffer.add_string buf "--"
    | ch -> Buffer.add_char buf ch in
  String.iter each path;
  Buffer.add_string buf suffix;
  Buffer.contents buf

(* Convert a path of a directory to backup into a cached path.  Also
   builds the directory if necessary. *)
let cache_path pool_dir backup_path =
  let canonical = Dbunix.mountpoint_of backup_path in
  let seen_dir = Filename.concat pool_dir "seen" in
  mkdir_safely seen_dir;
  flatten_path canonical (Filename.concat seen_dir "cache") ".sqlite"

let make_cache path hash backup_dir =
  let cache_name = cache_path path backup_dir in
  let hash = Hash.of_string hash in
  File_pool.with_file_pool path (fun pool ->
    Seendb.make_cache pool cache_name hash)

let create_pool path =
  File_pool.create_file_pool path

let pool_clone src_path dest_path hashes =
  let hashes = List.map Hash.of_string hashes in
  File_pool.with_file_pool src_path (fun src_pool ->
    File_pool.with_file_pool dest_path (fun dest_pool ->
      Clone.clone_trees src_pool dest_pool hashes))

let dump pool_path backup_path atts =
  File_pool.with_file_pool pool_path (fun pool ->
    let cache = cache_path pool_path backup_path in
    Backup.save pool cache backup_path atts)

(** {4 Argument processing} *)

type command = {
  help: string; (** Short description *)
  usage: string; (** Usage text. *)
  args: (Arg.key * Arg.spec * Arg.doc) list;
  action: (unit -> unit) -> string list -> unit }

let pool = ref ""
let pool_arg = ("-pool", Arg.Set_string pool, "Path to storage pool")
let must_pool usage =
  if !pool = "" then begin
    eprintf "Must specify a pool with -pool\n";
    usage ()
  end

let command_list usage = function
  | [] -> must_pool usage; list !pool
  | _ -> usage ()

let command_make_cache usage = function
  | [node; backup_dir] -> must_pool usage; make_cache !pool node backup_dir
  | _ -> usage ()

let command_du usage = function
  | [node] -> must_pool usage; du !pool node
  | _ -> usage ()

let command_restore usage = function
  | [node; dest] -> must_pool usage; restore !pool node dest
  | _ -> usage ()

let command_dump usage = function
  | (root :: att1 :: atts) -> must_pool usage; dump !pool root (att1::atts)
  | _ -> usage ()

let command_create_pool usage = function
  | [] -> must_pool usage; create_pool !pool
  | _ -> usage ()

let command_clone usage = function
  | (dest_path :: hash1 :: hashes) -> must_pool usage; pool_clone !pool dest_path (hash1 :: hashes)
  | _ -> usage ()

let command_walk usage = function
  | [node] -> must_pool usage; walk !pool node
  | _ -> usage ()

let commands = Map.StringMap.of_enum (List.enum [
  "list", { help = "List backups available in a pool";
	    usage = "odump list -pool <path>";
	    args = [ pool_arg ];
	    action = command_list };
  "make-cache", { help = "Update a visited file cache";
		  usage = "odump make-cache -pool <path> <hash> <path>";
		  args = [ pool_arg ];
		  action = command_make_cache };
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
])

let usage () =
  let out = IO.output_string () in
  let fmt = Format.formatter_of_output out in
  let names = Map.StringMap.enum commands in
  Format.fprintf fmt "usage: odump <command> [<args>]@\n@\n";
  Format.fprintf fmt "commands: @[";
  Enum.iter (fun (name, {help}) -> Format.fprintf fmt "%-12s %s@\n" name help) names;
  Format.fprintf fmt "@]@\n";
  Format.fprintf fmt "Use 'odump command --help' for more information on a specific command.@\n@\n";
  Format.fprintf fmt "Global options:";
  IO.close_out out

exception Got_command

let main () =
  (* Scan the arguments, stopping at the first anonymous argument.
     This parses the arguments up until the command name. *)
  let global_usage = usage () in
  let global_args = [ pool_arg ] in
  begin try Arg.parse global_args (fun arg -> raise Got_command) global_usage;
	    Arg.usage global_args global_usage;
	    exit 1
    with Got_command -> ()
  end;
  match Map.StringMap.Exceptionless.find Sys.argv.(!Arg.current) commands with
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

let _ = main ()
