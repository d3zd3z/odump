(* Testing round-trip of nodes. *)

open Batteries_uni

open OUnit
open Print
open TUtil

let sample_nodes () =
  [ Nodes.BlobNode (make_random_string 32 32);
    Nodes.BlobNode (make_random_string 1 1);
    Nodes.BlobNode (make_random_string 131072 131072); ]

let roundtrip pool =
  let base = sample_nodes () in
  let hashes = List.map (Nodes.put pool) base in
  let read_back = List.map (Nodes.get pool) hashes in
  assert_equal base read_back

let suite = "nodes" >::: [
  "roundtrip" >:: with_temp_pool roundtrip
]
