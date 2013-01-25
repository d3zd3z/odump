(* Test the seendb cache. *)

open Batteries

open OUnit
open TUtil

let normalize map = List.of_enum (Seendb.Int64Map.enum map)
let rnd = Random.State.make [| 123456 |]

let now = Unix.gettimeofday ()

let make_node expired i =
  let expire = if expired then now -. 1000.0 else Seendb.randomize_expire () in
  { Seendb.n_inode = Int64.of_int i;
    Seendb.n_ctime = Random.State.float rnd 1000000000.0;
    Seendb.n_expire = expire;
    Seendb.n_hash = Hash.of_data [string_of_int i] }

let check cache_name inum nodes =
  Seendb.with_cache cache_name (fun cache ->
    let in_nodes = Seendb.get cache inum in
    assert_equal (normalize nodes) (normalize in_nodes);
    let nodes2 = Seendb.get cache 12346L in
    assert_equal (normalize nodes2) [])

let store cache_name inum nodes =
  Seendb.with_cache cache_name (fun cache ->
    Seendb.begin_transaction cache;
    Seendb.update cache inum nodes;
    Seendb.commit cache)

let updates path _pool =
  let cache_name = Filename.concat path "tmp.sqlite" in
  let nodes = Enum.map (make_node false) (1 -- 500) in
  let nodes = Enum.map (fun ({ Seendb.n_inode; _ } as node) -> (n_inode, node)) nodes in
  let nodes = Seendb.Int64Map.of_enum nodes in

  store cache_name 12345L nodes;
  (* let _ = Unix.system (sprintf "sqlite3 %s .dump" cache_name) in *)
  check cache_name 12345L nodes;

  (* Verify that transactions work. *)
  Seendb.with_cache cache_name (fun cache ->
    Seendb.begin_transaction cache;
    Seendb.update cache 12345L Seendb.Int64Map.empty);
  check cache_name 12345L nodes

let expire path _pool =
  let cache_name = Filename.concat path "tmp.sqlite" in
  let nodes = Enum.map (fun i -> make_node (i mod 3 = 0) i) (1 -- 500) in
  let nodes = Enum.map (fun ({ Seendb.n_inode; _ } as node) -> (n_inode, node)) nodes in
  let nodes = Seendb.Int64Map.of_enum nodes in
  store cache_name 12345L nodes;
  let isgood { Seendb.n_expire; _ } = n_expire >= now in
  let nodes = Seendb.Int64Map.filterv isgood nodes in
  check cache_name 12345L nodes

let suite = "seendb" >::: [
  "updates" >:: with_temp_pool updates;
  "expire" >:: with_temp_pool expire;
]
