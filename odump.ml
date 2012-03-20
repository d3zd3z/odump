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

  method show_result (pool : Pool.writable) hash =
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
	info.du_data (Misc.nice_number info.du_data)
	info.du_write (Misc.nice_number info.du_write)
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

let main () =
  match Array.to_list Sys.argv with
    | [ _; "list"; path ] -> list path
    | [ _; "walk"; path; node ] -> walk path node
    | [ _; "du"; path; node ] -> du path node
    | [ _; "restore"; path; node; dest ] -> restore path node dest
    | (_ :: "dump" :: path :: root :: att1 :: atts) -> dump path root (att1::atts)
    | [ _; "make-cache"; path; node; backup_dir ] -> make_cache path node backup_dir
    | [ _; "create-pool"; path ] -> create_pool path
    | (_ :: "clone" :: src_path :: dest_path :: hash1 :: hashes) ->
      pool_clone src_path dest_path (hash1 :: hashes)
    | _ -> Log.failure ("Incorrect usage", [])

let _ = main ()
