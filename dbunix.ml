type dir_handle = Unix.dir_handle

let opendir = Unix.opendir
external readdir : dir_handle -> (string * int64) = "db_readdir"
let closedir = Unix.closedir
external lstat : string -> (string * (string * string) list) = "db_lstat"

let get_directory_contents path =
  let hand = opendir path in
  let result = ref [] in
  begin
    try
      while true do
	let ent = readdir hand in
	result := ent :: !result
      done
    with End_of_file -> ()
  end;
  !result
