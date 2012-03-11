open Batteries_uni
open LegacyIO
open OUnit

let basic () =
  assert_equal "0000000000000000000000000000000000000000"
    (Hash.to_string (Hash.of_string "0000000000000000000000000000000000000000"));
  assert_equal "ffffffffffffffffffffffffffffffffffffffff"
    (Hash.to_string (Hash.of_string "ffffffffffffffffffffffffffffffffffffffff"));
  assert_equal "81fe8bfe87576c3ecb22426f8e57847382917acf"
    (Hash.to_string (Hash.of_data ["abcd"]));
  assert_equal "04d80ad2e56cbb49109f46d85f4315457c7cafb3"
    (Hash.to_string (Hash.of_data ["blob";
				   "This is a sample multiple-part message"]))

let bump () =
  let counted_change f text count expect =
    let h1 = Hash.of_string text in
    let rec loop h count =
      if count = 0 then h
      else loop (f h) (count - 1) in
    let h2 = loop h1 count in
    assert_equal (Hash.to_string h2) expect in
  let inc = counted_change Hash.succ in
  let dec = counted_change Hash.pred in
  inc "0000000000000000000000000000000000000000" 1 "0000000000000000000000000000000000000001";
  inc "0000000000000000000000000000000000000000" 256 "0000000000000000000000000000000000000100";
  inc "00000000000000000000000000000000ffffffff" 1 "0000000000000000000000000000000100000000";
  inc "ffffffffffffffffffffffffffffffffffffffff" 1 "0000000000000000000000000000000000000000";
  dec "ffffffffffffffffffffffffffffffffffffffff" 1 "fffffffffffffffffffffffffffffffffffffffe";
  dec "ffffffffffffffffffffffffffffffffffffffff" 256 "fffffffffffffffffffffffffffffffffffffeff";
  dec "ffffffffffffffffffffffffffffffff00000000" 1 "fffffffffffffffffffffffffffffffeffffffff";
  dec "0000000000000000000000000000000000000000" 1 "ffffffffffffffffffffffffffffffffffffffff";
  ()

let suite = "hash" >::: [
  "basic" >:: basic;
  "bump" >:: bump;
]
