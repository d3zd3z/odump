(* Testing round-trip of nodes. *)

open Batteries_uni

open OUnit
open Printf
open TUtil

module SM = Map.StringMap

let int_hash num = Hash.of_data [string_of_int num]

let sample_nodes () =
  let dir1 = [ "a", int_hash 1;
	       "This is longer", int_hash 2;
	       (make_random_string 1023 1023), int_hash 3] in
  let dir1 = SM.of_enum (List.enum dir1) in

  let props = [ "simple", "simple value";
		"complex", (make_random_string 1023 1023);
		"other", "other thingy" ] in
  let props = SM.of_enum (List.enum props) in

  [ Nodes.BlobNode (make_random_string 32 32);
    Nodes.BlobNode (make_random_string 1 1);
    Nodes.BlobNode (make_random_string 131072 131072);

    Nodes.NullNode;
    Nodes.DirNode dir1;
    Nodes.NodeNode ("REG", props);
]

(* Maps don't generally compare equal, unless they were created in the
   same order.  Resolve this by regenerating the maps in the nodes to be
   built in a deterministic order. *)
let normalize_map src = SM.of_enum (SM.enum src)
let normalize_node = function
  | Nodes.BackupNode (d, map) -> Nodes.BackupNode (d, normalize_map map)
  | Nodes.NodeNode (k, map) -> Nodes.NodeNode (k, normalize_map map)
  | Nodes.DirNode map -> Nodes.DirNode (normalize_map map)
  | n -> n

let roundtrip pool =
  let base = sample_nodes () in
  let hashes = List.map (Nodes.put pool) base in
  let read_back = List.map (Nodes.get pool) hashes in
  assert_equal (List.map normalize_node base)
    (List.map normalize_node read_back);

  (* Make sure that empty data comes back as Null nodes. *)
  let h1 = Nodes.try_put pool (Nodes.BlobNode "") in
  assert_equal (Nodes.get pool h1) (Nodes.NullNode)

let suite = "nodes" >::: [
  "roundtrip" >:: with_temp_pool roundtrip
]
