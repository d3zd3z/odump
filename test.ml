(* Run the test suite. *)

open OUnit

let all = "test" >::: [
  Test_hash.suite
]

let _ = run_test_tt_main all
