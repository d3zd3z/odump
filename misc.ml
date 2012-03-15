(* Misc support *)

open Printf

let ensure_directory ?(what="Unknown") path =
  if not (Sys.file_exists path && Sys.is_directory path) then
    let message = sprintf "Pathname for %s is not a directory: '%s'" what path in
    failwith message

let ensure_empty_directory ?(what="Unknown") path =
  ensure_directory ~what:what path;
  if Sys.readdir path <> [| |] then
    let message = sprintf "Pathname for %s is not an empty directory: '%s'" what path in
    failwith message
