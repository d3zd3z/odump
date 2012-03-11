(* Run the test suite. *)

open OUnit

let all = "test" >::: [
  Test_hash.suite;
  Test_chunk.suite;
  Test_file_index.suite;
  Test_file_pool.suite;
]

let _ = run_test_tt_main all
