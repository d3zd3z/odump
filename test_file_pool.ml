open Batteries_uni
open LegacyIO

open OUnit
open Printf
open TUtil

module IntSet = Set.IntSet

(* TODO: Test newfile mode *)

let pool_monitor path =
object (self)
  val mutable pool = None
  val mutable known = IntSet.empty

  method create = File_pool.create_file_pool path

  method openit = self#close; pool <- Some (File_pool.open_file_pool path)

  method close =
    Option.may (fun x -> x#close) pool;
    pool <- None

  method flush = (Option.get pool)#flush

  method add_one index =
    let chunk = make_random_chunk 32 index in
    (Option.get pool)#add chunk;
    known <- IntSet.add index known

  method add range =
    Enum.iter self#add_one range

  method check_one index =
    let chunk = make_random_chunk 32 index in
    assert_equal chunk#data **> ((Option.get pool)#find chunk#hash)#data

  method check =
    IntSet.iter self#check_one known
end

let creation tmpdir =

  (* do_cleanup := false; *)
  let monitor = pool_monitor tmpdir in

  monitor#create;
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
  let _ = Sys.command ("ls -l " ^ tmpdir) in
  ()

let suite = "file_pool" >::: [
  "creation" >:: with_temp_dir creation;
]
