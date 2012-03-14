open Batteries_uni
open OUnit
open Printf

(* Read the current directory, and verify that every entry has the
   right inode number. *)
let readdir () =
  let entries = Dbunix.get_directory_contents "." in
  let each (name, ino) =
    let sbuf = Unix.lstat name in
    assert_equal ino (Int64.of_int sbuf.Unix.st_ino) in
  List.iter each entries

let suite = "dbunix" >::: [
  "readdir" >:: readdir;
]
