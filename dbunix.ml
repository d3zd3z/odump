open Batteries_uni

module SM = Map.StringMap

type dir_handle = Unix.dir_handle

let opendir = Unix.opendir
external readdir : dir_handle -> (string * int64) = "db_readdir"
let closedir = Unix.closedir
external raw_lstat : string -> (string * (string * string) list) = "db_lstat"
let lstat path =
  match raw_lstat path with
    | (kind, props) -> (kind, SM.of_enum (List.enum props))

type file_descr = Unix.file_descr
external open_for_read : string -> file_descr = "db_open_for_read"

let get_directory_contents path =
  let hand = opendir path in
  let result = ref [] in
  begin
    try
      while true do
	let ent = readdir hand in
	result := ent :: !result
      done
    with
      | End_of_file -> ()
      | e -> closedir hand; raise e
  end;
  closedir hand;
  !result

type stat = string SM.t
type stat_info = string * stat
let dir_with_stats path =
  let inames = get_directory_contents path in
  let inames = List.sort ~cmp:(fun (_, a) (_, b) -> Int64.compare a b) inames in
  let lookup (name, _) = (name, lstat (Filename.concat path name)) in
  let nodes = List.map lookup inames in
  List.sort ~cmp:(fun (a, _) (b, _) -> compare a b) nodes

(* Call f a with the umask set accordingly, and restore after the
   call. *)
let with_umask mask =
  let old_mask = Unix.umask mask in
  finally (fun () -> let _ = Unix.umask old_mask in ())

external utimensat : string -> int64 -> int64 -> unit = "db_utimensat"
external lchown : string -> int -> int -> unit = "db_lchown"
external make_special : string -> string -> int -> int64 -> unit = "db_make_special"

let is_root () = Lazy.force (lazy (Unix.geteuid () = 0))

let get_int key map = int_of_string (SM.find key map)
let get_int64 key map = Int64.of_string (SM.find key map)
let get_hash key map = Hash.of_string (SM.find key map)

(* Decode a time string.  Acceptable formats are either a simple
   integer, or a fractional time with a single decimal point.  Returns
   the seconds a nsec values of the time. *)
let decode_time time =
  match String.nsplit time "." with
    | [sec] -> (Int64.of_string sec, 0L)
    | [sec; nsec] ->
      let len = String.length nsec in
      let nsec = if len < 9 then
	  nsec ^ String.make (9-len) ' '
	else nsec in
      (Int64.of_string sec, Int64.of_string nsec)
    | _ -> Log.failf "Invalid time data: '%S'" time

let float_of_time time =
  let (sec, nsec) = decode_time time in
  Log.debugf "float_of_time %s, sec=%Ld, nsec=%Ld" time sec nsec;
  (Int64.to_float sec (* +. Int64.to_float nsec /. 1.0e9 *))

let get_time key map = float_of_time (SM.find key map)

let set_time path props =
  let (sec, nsec) = decode_time (SM.find "mtime" props) in
  utimensat path sec nsec

let restore_stat path kind props = match kind with

  | "REG" | "DIR" ->
    if is_root () then
      Unix.chown path (get_int "uid" props) (get_int "gid" props);
    Unix.chmod path (get_int "mode" props);
    set_time path props

  | "LNK" ->
    (* Linux has no lchmod, but the umask is used in the link permissions
       (they don't actually get used for anything, though). *)
    with_umask ((get_int "mode" props) land 4095)
      (Unix.symlink (SM.find "target" props)) path;
    if is_root () then
      lchown path (get_int "uid" props) (get_int "gid" props)

  | "CHR" | "BLK" | "FIFO" | "SOCK" ->
    let is_dev = SM.mem "rdev" props in
    if is_dev && not (is_root ()) then
      Log.warnf "Cannot restore device node as non-root: %S" path
    else begin
      let dev = if is_dev then get_int64 "rdev" props else 0L in
      with_umask 0 (make_special path kind (get_int "mode" props)) dev;
      if is_root () then
	Unix.chown path (get_int "uid" props) (get_int "gid" props);
      set_time path props
    end

  | _ ->
    Log.warnf "TODO: Restore kind '%s %S" kind path

external realpath : string -> string = "db_realpath"

(* Chop up a canonical path into strings with successively shorter
   components.  (Not tail recursive, paths have limited bound) *)
let rec chop_path path =
  if path = "/" then ["/"]
  else path :: chop_path (Filename.dirname path)

(* The mount point is either the first directory element whose parent
   is on a different device, or the first one with a parent as the root
   of the filesystem. *)
let mountpoint_of path =
  let rpath = realpath path in
  let parts = chop_path rpath in
  let rec loop parts = match parts with
    | [(root, _)] -> root
    | ((a, astat) :: (((b, bstat) :: _) as rest)) ->
      if astat.Unix.st_dev = bstat.Unix.st_dev then
	loop rest
      else a
    | [] -> Log.failf "Empty path: path=%S, rpath=%S" path rpath in
  let stats = List.map Unix.lstat parts in
  loop (List.combine parts stats)
