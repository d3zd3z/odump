(* Misc support *)

let ensure_directory ?(what="Unknown") path =
  if not (Sys.file_exists path && Sys.is_directory path) then
    Log.failure ("Pathname is not a directory", ["operation", what; "path", path])

let ensure_empty_directory ?(what="Unknown") path =
  ensure_directory ~what:what path;
  if Sys.readdir path <> [| |] then
    Log.failure ("Pathname is not an empty directory", ["operation", what; "path", path])

(* Units for nicely printing sizes.  YiB would take 70 bits, so cannot
   be reached by a 64-bit number. *)
let units = ["B"; "Kib"; "MiB"; "GiB"; "TiB"; "PiB"; "EiB"; "ZiB"; "YiB"]
let fnice_number num =
  let rec loop num units =
    if abs_float num > 1024.0 then loop (num /. 1024.0) (List.tl units)
    else (num, List.hd units) in
  let (num, unit) = loop num units in
  Printf.sprintf "%6.1f%-3s" num unit

let nice_number num = fnice_number (Int64.to_float num)
