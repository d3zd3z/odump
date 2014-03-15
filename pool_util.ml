(* Pool query *)

open Batteries

exception Pool_error of string

(* TODO: Use better detection of incorrect pool info. *)
let open_pool path =
  if Sys.file_exists (path ^ "/metadata/props.txt") then
    File_pool.open_file_pool path
  else if Sys.is_directory (path ^ ".blobs") && Sys.file_exists path then
    Sql_pool.open_sql_pool path
  else
    raise (Pool_error ("Unable to open pool " ^ path))

let with_pool path f =
  with_dispose ~dispose:(fun x -> x#close) f
    (open_pool path)
