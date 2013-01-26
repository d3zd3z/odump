open Batteries
open Printf

module C = Config

(* true to not run commands, just debug them. *)
let debug = false

let safe_find pred list =
  try List.find pred list
  with Not_found ->
    eprintf "Host not present in config file\n";
    exit 1

let command name =
  match (Maps.StringMap.find name Config.commands)#get with
    | None -> failwith ("Unable to find command for " ^ name)
    | Some x -> x

(* Shell's 'to_file' truncates on append, despite the good intentions
   (it's just wrong).  Open a file for writing (it'll have to be closed
   manually). *)
let append_out name =
  Unix.openfile name [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND ] 0o644

class fs_manager pool host fs =
object (self)
  val reg_dest = fs.C.fs_base

  (* TODO: Make the mirror optional. *)
  val mirror = Option.map (fun m -> m ^ "/" ^ fs.C.fs_volume) host.C.host_mirror

  val pool = pool

  val surelog = Config.surelog#get
  val rsynclog = Config.rsynclog#get

  method sure_path = reg_dest

  method setup_check_paths =
    if not (Sys.is_directory reg_dest) then
      failwith (sprintf "Backup base directory doesn't exist '%s'" reg_dest);
    Option.may (fun mirror ->
      if not (Sys.is_directory mirror) then
	failwith (sprintf "Mirror dir '%s' doesn't exist" mirror))
      mirror;
    if not (Sys.is_directory pool) then
      failwith (sprintf "Pool dir doesn't exist: '%s'" pool)

  method teardown_check_paths = ()
  method setup_start_snapshot = ()
  method teardown_start_snapshot = ()
  method setup_mount_snapshot = ()
  method teardown_mount_snapshot = ()

  method setup_run_clean =
    self#run ~chdir:(self#sure_path) fs.C.fs_clean [self#sure_path]

  method teardown_run_clean = ()

  method setup_sure_update =
    self#run ~chdir:(self#sure_path) (command "gosure") ["update"]

  method teardown_sure_update = ()

  method setup_sure_write =
    self#banner surelog "sure" self#sure_path;
    self#run ~chdir:(self#sure_path) ~log:surelog (command "gosure") ["signoff"]

  method teardown_sure_write = ()

  method setup_rsync =
    Option.may (fun mirror ->
      self#banner rsynclog "rsync" self#sure_path;
      self#run ~log:rsynclog (command "rsync") ["-aiH"; "--delete";
						self#sure_path ^ "/"; mirror])
      mirror

  method teardown_rsync = ()

  method setup_dump =
    Log.infof "Dumping fs=%s host=%s" fs.C.fs_volume host.C.host_host;
    if not debug then
      Backup.dump pool self#sure_path ["fs=" ^ fs.C.fs_volume;
				       "host=" ^ host.C.host_host]

  method teardown_dump = ()

  method run ?chdir ?log program args =
    let place = match chdir with
      | None -> ""
      | Some dir -> " in '" ^ dir ^ "'" in
    Log.infof "Run command: '%s' %a%s" program (List.print String.print) args place;
    let (log, close) = match log with
      | None -> (None, fun () -> ())
      | Some name ->
	let fd = append_out name in
	(Some (Shell.to_fd fd), fun () -> Unix.close fd) in
    if not debug then begin
      let cmd = Shell.cmd ?chdir:chdir program args in
      Shell.call ?stdout:log [cmd]
    end;
    close ()

  method banner fname task dest =
    let now = Sys.time () in
    let nd = Netdate.create ~localzone:true now in
    let now_fmt = Netdate.format ~fmt:"%Y-%m-%d_%H:%M" nd in
    let line = sprintf "--- %s of %s (%s) on %s ---\n" task fs.C.fs_volume dest now_fmt in
    let header = String.make (String.length line - 1) '-' ^ "\n" in
    let write fd =
      output_string fd header;
      output_string fd line;
      output_string fd header in
    File.with_file_out ~mode:[`append;`create;`text] fname write

end

class lvm_manager pool host fs is_xfs =
object (self)
  inherit fs_manager pool host fs as super

  val snap_dest = "/mnt/snap/" ^ fs.C.fs_volume
  val snap_vol = "/dev/" ^ host.C.host_vol ^ "/" ^ fs.C.fs_volume ^ ".snap"

  method! sure_path = snap_dest

  method! setup_check_paths =
    super#setup_check_paths;
    if not (Sys.is_directory snap_dest) then
      failwith (sprintf "Snapshot destination doesn't exist '%s'" snap_dest)

  method! setup_start_snapshot =
    self#run (command "lvcreate") ["-L"; "5g"; "-n"; fs.C.fs_volume ^ ".snap";
				   "-s"; "/dev/" ^ host.C.host_vol ^ "/" ^ fs.C.fs_volume]

  method! teardown_start_snapshot =
    self#run (command "lvremove") ["-f"; snap_vol]

  method! setup_mount_snapshot =
    let uuid = if is_xfs then ["-o"; "nouuid"] else [] in
    self#run (command "mount") (uuid @ [snap_vol; snap_dest])

  method! teardown_mount_snapshot =
    self#run (command "umount") [snap_dest]

  method! setup_sure_write =
    super#setup_sure_write;
    self#run (command "cp") ["-p"; snap_dest ^ "/2sure.dat.gz";
			     reg_dest ^ "/2sure.dat.gz"]

end

let make_manager pool host fs = match fs.C.fs_style with
  | "plain" -> new fs_manager pool host fs
  | "xfs-lvm" -> new lvm_manager pool host fs true
  | "ext4-lvm" -> new lvm_manager pool host fs false
  | style -> failwith ("Unknown management style: " ^ style)

let make_steps obj = ([
  ("check paths", (fun () -> obj#setup_check_paths), (fun () -> obj#teardown_check_paths));
  ("start snapshot", (fun () -> obj#setup_start_snapshot), (fun () -> obj#teardown_start_snapshot));
  ("mount snapshot", (fun () -> obj#setup_mount_snapshot), (fun () -> obj#teardown_mount_snapshot));
  ("run clean", (fun () -> obj#setup_run_clean), (fun () -> obj#teardown_run_clean));
  ("sure update", (fun () -> obj#setup_sure_update), (fun () -> obj#teardown_sure_update));
  ("sure write", (fun () -> obj#setup_sure_write), (fun () -> obj#teardown_sure_write));
  ("rsync", (fun () -> obj#setup_rsync), (fun () -> obj#teardown_rsync));
  ("dump", (fun () -> obj#setup_dump), (fun () -> obj#teardown_dump));
] : (string * (unit -> unit) * (unit -> unit)) list)

let get_step_name = function
  | ((name, _, _)::_) -> name
  | [] -> "???"

let log_rotate () =
  let rotate name =
    if (Sys.file_exists name) then
      Sys.rename name (name ^ ".bak") in
  rotate (Config.surelog#get);
  rotate (Config.rsynclog#get)

let managed pool host =
  (* Lookup the host. *)
  let hosts = Config.hosts#get in
  let host = safe_find (fun h -> host = h.C.host_host) hosts in
  let managers = List.map (make_manager pool host) host.C.host_fs in
  let all_steps = List.transpose (List.map make_steps managers) in
  let cleanup = ref [] in
  let step1 fss =
    let name = get_step_name fss in
    Log.infof "*** setup %s ***" name;
    let step2 (_name, setup, teardown) =
      setup ();
      cleanup := (fun () -> teardown ()) :: !cleanup
    in
    List.iter step2 fss in
  log_rotate ();
  finally (fun () ->
    Log.info "*** Teardown ***";
    List.iter (fun x -> x ()) !cleanup)
    (List.iter step1) all_steps
