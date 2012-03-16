(* Misc support *)

let ensure_directory ?(what="Unknown") path =
  if not (Sys.file_exists path && Sys.is_directory path) then
    Log.failure ("Pathname is not a directory", ["operation", what; "path", path])

let ensure_empty_directory ?(what="Unknown") path =
  ensure_directory ~what:what path;
  if Sys.readdir path <> [| |] then
    Log.failure ("Pathname is not an empty directory", ["operation", what; "path", path])
