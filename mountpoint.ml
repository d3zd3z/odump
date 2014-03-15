(* Mountpoint determination. *)

(* TODO: at some point, we'd like to be able to come up with a
   reasonbly good unique label for btrfs volumes.  Generally, backups
   will be made from a snapshot, which will probably get a different
   ID number each time.  So, for btrfs, we probably want to use the
   subvolume name.  However, there doesn't seem to yet be a consistent
   way of getting this information.  The btrfs tools don't install
   their headers, so the ioctls aren't even readily available
   programmatically.

   So, at this point, there isn't even a reasonable way to find out
   what the primary filesystem is for btrfs, so we aren't even going
   to try yet.

   For other filesystem types, we can use the output of blkid to
   determine the uuid of the particular filesystem.

   For now, just use the name of the root of the mountpoint, and
   pretend that is good enough. *)

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
let cache_path backup_path =
  let canonical = Dbunix.mountpoint_of backup_path in
  let base = "cache-" ^ Unix.gethostname () in
  flatten_path canonical base ".sqlite"

let make_cache_path pool_dir backup_path =
  let cp = cache_path backup_path in
  let seen_dir =
    if Sys.is_directory pool_dir then
      Filename.concat pool_dir "seen"
    else
      pool_dir ^ ".seen" in
  mkdir_safely seen_dir;
  Filename.concat seen_dir cp
