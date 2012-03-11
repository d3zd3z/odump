(* Testing the file index. *)

open Batteries_uni
open LegacyIO

open OUnit
open Printf
open TUtil

open Enum.Infix

module HashMap = Map.Make(Hash)
module HashSet = Set.Make(Hash)

let numbered_chunk n = Chunk.chunk_of_string "blob" (string_of_int n)
let numbered_hash n = Hash.of_data [string_of_int n]

(* Check that the given hash is present (and correct), and that the
   off-by-one hashes are not present. *)
let check_hash map hash pos =
  assert_equal (pos, "blob") (HashMap.find hash map);
  assert_bool "inc key" (not (HashMap.mem (Hash.succ hash) map));
  assert_bool "dec key" (not (HashMap.mem (Hash.pred hash) map))

let memory () =
  let src = 1 -- 10000 in
  let hashes = Enum.map (fun n -> Hash.of_data [string_of_int n]) src in
  let dests = Enum.map (fun n -> (n, "blob")) src in
  let table = HashMap.of_enum (Enum.combine (hashes, dests)) in
  Enum.iter (fun (pos, hash) -> check_hash table hash pos)
    (Enum.combine (src, hashes))

let check_hash idx hash pos kind =
  assert_equal (pos, kind) (idx#find hash);
  assert_bool "inc key" (not (idx#mem (Hash.succ hash)));
  assert_bool "dec key" (not (idx#mem (Hash.pred hash)))

let memory tmpdir =
  let size = 10000 in
  let src = 1 -- size in
  let kinds = Enum.cycle (BatList.enum ["blob"; "dir "; "dir0"; "dir1"; "qwwe"]) in
  let info = Enum.combine (src, kinds) in
  let idx = File_index.make (Filename.concat tmpdir "data.idx") in
  Enum.iter (fun (n, kind) -> idx#add (numbered_hash n) n kind) (Enum.clone info);
  Enum.iter (fun (n, kind) -> check_hash idx (numbered_hash n) n kind) (Enum.clone info);
  idx#save 42;
  Enum.iter (fun (n, kind) -> check_hash idx (numbered_hash n) n kind) (Enum.clone info);

  (* Add more *)
  let src = Enum.append src ((size+1) -- (2*size)) in
  let kinds = Enum.cycle (BatList.enum ["blob"; "dir "; "dir0"; "dir1"; "qwwe"]) in
  let info2 = Enum.combine (src, kinds) in
  Enum.iter (fun (n, kind) -> idx#add (numbered_hash n) n kind) (Enum.clone info2);
  Enum.iter (fun (n, kind) -> check_hash idx (numbered_hash n) n kind) (Enum.clone info);

  (* One last test to make sure that writing the combination works. *)
  idx#save 44;
  Enum.iter (fun (n, kind) -> check_hash idx (numbered_hash n) n kind) (Enum.clone info);
  Enum.iter (fun (n, kind) -> check_hash idx (numbered_hash n) n kind) (Enum.clone info2)

let merges () =
  let a = HashSet.enum (HashSet.of_enum (Enum.map numbered_hash (1 -- 100))) in
  let b = HashSet.enum (HashSet.of_enum (Enum.map numbered_hash (101 -- 200))) in
  let c = Enum.merge (fun a b -> Hash.compare a b < 0) a b in
  assert_equal 200 (Enum.count c)

let suite = "file_index" >::: [
  "memory" >:: with_temp_dir memory;
  "merge" >:: merges;
]
