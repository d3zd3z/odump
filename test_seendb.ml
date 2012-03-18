(* Test the seendb cache. *)

open Batteries_uni

open OUnit
open Printf
open TUtil

let normalize map = List.of_enum (Seendb.Int64Map.enum map)
let rnd = Random.State.make [| 123456 |]

let updates path pool =
  let cache_name = Filename.concat path "tmp.sqlite" in
  let make_node i = { Seendb.n_inode = Int64.of_int i;
		      Seendb.n_ctime = Random.State.float rnd 1000000000.0;
		      Seendb.n_expire = Seendb.randomize_expire ();
		      Seendb.n_hash = Hash.of_data [string_of_int i] } in
  let nodes = Enum.map make_node (1 -- 5) in
  let nodes = Enum.map (fun ({ Seendb.n_inode } as node) -> (n_inode, node)) nodes in
  let nodes = Seendb.Int64Map.of_enum nodes in

  let check () =
    Seendb.with_cache cache_name (fun cache ->
      let in_nodes = Seendb.get cache 12345L in
      assert_equal (normalize nodes) (normalize in_nodes);
      let nodes2 = Seendb.get cache 12346L in
      assert_equal (normalize nodes2) []) in

  Seendb.with_cache cache_name (fun cache ->
    Seendb.begin_transaction cache;
    Seendb.update cache 12345L nodes;
    Seendb.commit cache);
  (* let _ = Unix.system (sprintf "sqlite3 %s .dump" cache_name) in *)
  check ();

  (* Verify that transactions work. *)
  Seendb.with_cache cache_name (fun cache ->
    Seendb.begin_transaction cache;
    Seendb.update cache 12345L Seendb.Int64Map.empty);
  check ()

let suite = "seendb" >::: [
  "updates" >:: with_temp_pool updates
]
