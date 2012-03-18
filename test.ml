(* Run the test suite. *)

open OUnit

let all = "test" >::: [
  Test_hash.suite;
  Test_chunk.suite;
  Test_file_index.suite;
  Test_file_pool.suite;
  Test_properties.suite;
  Test_dbunix.suite;
  Test_nodes.suite;
  Test_indirect.suite;
]

let _ = run_test_tt_main all
