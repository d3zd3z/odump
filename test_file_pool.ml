open Batteries
(* open LegacyIO *)

open OUnit
open TUtil

(* TODO: Test newfile mode *)

let pool_monitor path =
object (self)
  val mutable pool = None
  val mutable known = ISet.empty

  method create ?limit ?newfile () = File_pool.create_file_pool ?limit ?newfile path

  method openit = self#close; pool <- Some (File_pool.open_file_pool path)

  method close =
    Option.may (fun x -> x#close) pool;
    pool <- None

  method flush = (Option.get pool)#flush

  method add_one index =
    let chunk = make_random_chunk 32 index in
    (Option.get pool)#add chunk;
    known <- ISet.add index known

  method add range =
    Enum.iter self#add_one range

  method check_one index =
    let chunk = make_random_chunk 32 index in
    assert_equal chunk#data @@ ((Option.get pool)#find chunk#hash)#data

  method check =
    ISet.iter self#check_one known

  method check_index =
    (* Verify that the index numbers cover the consecutive integers
       starting with 0. *)
    let each index set =
      let chunk = make_random_chunk 32 index in
      let hash = chunk#hash in
      ISet.add ((Option.get pool)#find_index hash) set in
    let rset = ISet.fold each known ISet.empty in
    let known_size = ISet.cardinal known in
    assert_equal (ISet.cardinal rset) known_size;
    assert_equal (ISet.max_elt rset) (known_size - 1)
end

let creation tmpdir =

  (* do_cleanup := false; *)
  let monitor = pool_monitor tmpdir in

  monitor#create ();
  monitor#openit;

  monitor#add (1 -- 2000);
  monitor#flush;
  monitor#check;
  monitor#openit;
  monitor#check;
  monitor#add (2001 -- 4000);
  monitor#check;
  monitor#openit;
  monitor#check;
  monitor#check_index;
  (* let _ = Sys.command ("ls -l " ^ tmpdir) in *)
  ()

let index_recovery tmpdir =
  let index_name = Filename.concat tmpdir "pool-data-0000.idx" in
  let tmp_name = index_name ^ ".old" in
  let monitor = pool_monitor tmpdir in
  monitor#create ();

  (* Check completely missing index. *)
  monitor#openit;
  monitor#add (1 -- 2000);
  monitor#close;
  Sys.remove index_name;

  monitor#openit;
  monitor#check;
  monitor#close;

  (* Check index of smaller file. *)
  copy_file index_name tmp_name;
  monitor#openit;
  monitor#add (2001 -- 4000);
  monitor#close;

  (* Sys.remove index_name; *)
  copy_file tmp_name index_name;
  monitor#openit;
  monitor#check;
  monitor#close;
  ()

let newfile_check tmpdir =
  (* do_cleanup := false; *)
  let pfile n = Filename.concat tmpdir (Printf.sprintf "pool-data-%04d.data" n) in
  let monitor = pool_monitor tmpdir in
  monitor#create ~newfile:true ();
  monitor#openit;
  monitor#add (1 -- 1);
  monitor#close;

  assert_bool "file 0" (Sys.file_exists (pfile 0));
  assert_bool "file 1" (not (Sys.file_exists (pfile 1)));

  (* Re-open, and make sure that creates a new file. *)
  monitor#openit;
  monitor#add (2 -- 2);
  monitor#add (3 -- 3);
  monitor#close;

  assert_bool "file 0" (Sys.file_exists (pfile 0));
  assert_bool "file 1" (Sys.file_exists (pfile 1));
  assert_bool "file 2" (not (Sys.file_exists (pfile 2)))

let suite = "file_pool" >::: [
  "creation" >:: with_temp_dir creation;
  "recovery" >:: with_temp_dir index_recovery;
  "newfile" >:: with_temp_dir newfile_check;
]
