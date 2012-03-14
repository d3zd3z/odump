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
  | _ -> failwith "Invalid node"

let backup_compare a b = match (a, b) with
  | (Nodes.BackupNode (da, _), Nodes.BackupNode (db, _)) -> compare da db
  | _ -> failwith "Attempt to sort non-backup nodes"

let list path =
  let pool = File_pool.open_file_pool path in
  let backups = pool#get_backups in
  let get hash = (hash, Nodes.get pool hash) in
  let backups = List.map get backups in
  let backups = List.sort ~cmp:(fun (_, a) (_, b) -> backup_compare a b) backups in
  List.iter (fun (hash, b) -> show_backup_node hash b) backups

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
  method! enter path _ node = tree_show path node
end

let walk path root_hash =
  let pool = File_pool.open_file_pool path in
  let root_hash = Hash.of_string root_hash in
  (* tree_walk pool "." root_hash *)
  Nodes.walk pool "." root_hash (new walk_visitor)

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

(* Units for nicely printing sizes.  YiB would take 70 bits, so cannot
   be reached by a 64-bit number. *)
let units = ["B"; "Kib"; "MiB"; "GiB"; "TiB"; "PiB"; "EiB"; "ZiB"; "YiB"]
let nice_number num =
  let num = Int64.to_float num in
  let rec loop num units =
    if abs_float num > 1024.0 then loop (num /. 1024.0) (List.tl units)
    else (num, List.hd units) in
  let (num, unit) = loop num units in
  sprintf "%6.1f%-3s" num unit

class du_visitor =
object (self)
  inherit Nodes.empty_visitor

  val mutable sizes = StringMap.empty

  (* This isn't very efficient, but without doing this, we won't know
     we've visited a node, and will count the write size multiple
     times. *)
  val mutable seen = HashSet.empty

  method private update hash kind size write_size =
    let (write, count) = if HashSet.mem hash seen then (0, 0) else begin
      seen <- HashSet.add hash seen;
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

  method show_result pool hash =
    let node = Nodes.get pool hash in
    (* printf "backup: %s\n" (Hash.to_string hash); *)
    show_backup_node hash node;
    printf "\n";
    printf "kind          data size                  compressed size        count\n";
    printf "---- ---------------------------   ---------------------------  -----\n";
    let each kind info =
      printf "%4s %15Ld (%s)   %15Ld (%s)  (%Ld)\n"
	kind
	info.du_data (nice_number info.du_data)
	info.du_write (nice_number info.du_write)
	info.du_count in
    StringMap.iter each sizes;
    printf "\n"
end

let du path root_hash =
  let pool = File_pool.open_file_pool path in
  let root_hash = Hash.of_string root_hash in
  let visitor = new du_visitor in
  Nodes.walk pool "." root_hash (visitor :> Nodes.visitor);
  visitor#show_result pool root_hash

let main () =
  match Sys.argv with
    | [| _; "list"; path |] -> list path
    | [| _; "walk"; path; node |] -> walk path node
    | [| _; "du"; path; node |] -> du path node
    | _ -> failwith "Incorrect usage"

let _ = main ()
