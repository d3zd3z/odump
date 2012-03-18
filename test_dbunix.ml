open Batteries_uni
open OUnit
open Printf

module SM = Map.StringMap

(* Read the current directory, and verify that every entry has the
   right inode number. *)
let readdir () =
  let entries = Dbunix.get_directory_contents "." in
  let each (name, ino) =
    let sbuf = Unix.lstat name in
    assert_equal ino (Int64.of_int sbuf.Unix.st_ino) in
  List.iter each entries

(* Perform a basic sanity test of lstat. *)
let lstat () =
  let (kind, map) = Dbunix.lstat "." in
  let real_stat = Unix.lstat "." in   (* There is a race here. *)
  assert_equal kind "DIR";

  (* Assume directories fit into an int. *)
  assert_equal (int_of_string (SM.find "size" map)) real_stat.Unix.st_size;

  (* Test one time field. *)
  let (ns, sec) = modf (float_of_string (SM.find "ctime" map)) in
  assert_equal sec real_stat.Unix.st_ctime
  (* printf "\nkind: %s\n" kind;
  SM.iter (fun key value -> printf "%-8s %s\n" key value) map *)

let suite = "dbunix" >::: [
  "readdir" >:: readdir;
  "lstat" >:: lstat;
]
