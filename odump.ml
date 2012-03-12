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

let main () =
  match Sys.argv with
    | [| _; "list"; path |] -> list path
    | _ -> failwith "Incorrect usage"

let _ = main ()
