(* Misc support *)

let ensure_directory ?(what="Unknown") path =
  if not (Sys.file_exists path && Sys.is_directory path) then
    Log.failf "Pathname for %s is not a directory: %S" what path

let ensure_empty_directory ?(what="Unknown") path =
  ensure_directory ~what:what path;
  if Sys.readdir path <> [| |] then
    Log.failf "Pathname for %s is not an empty directory: %S" what path

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
