(* Misc support *)

let ensure_directory ?(what="Unknown") path =
  if not (Sys.file_exists path && Sys.is_directory path) then
    Log.failf "Pathname for %s is not a directory: %S" what path

let ensure_empty_directory ?(what="Unknown") path =
  ensure_directory ~what:what path;
  if Sys.readdir path <> [| |] then
    Log.failf "Pathname for %s is not an empty directory: %S" what path
