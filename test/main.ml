open OUnit2
open Processor
open Utilities

(*[test_dec_to_bin] compares the [dec_to_bin n] to expected output*)
let test_dec_to_bin (name : string) (n : int) (expected_output : string) : test
    =
  name >:: fun _ ->
  assert_equal expected_output (dec_to_bin n) ~printer:String.escaped

(*[test_dec_to_bin] compares the [dec_to_hex n] to expected output*)
let test_dec_to_hex (name : string) (n : int) (expected_output : string) : test
    =
  name >:: fun _ ->
  assert_equal expected_output (dec_to_hex n) ~printer:String.escaped

let dec_conversions_tests =
  [
    test_dec_to_bin "convert 10 to binary rep" 10
      "0b00000000000000000000000000001010";
    test_dec_to_bin "" 56 "0b00000000000000000000000000111000";
    test_dec_to_hex "" 26 "0x0000001a";
    test_dec_to_hex "" 45 "0x0000002d";
  ]

let suite = "test suite for A2" >::: List.flatten [ dec_conversions_tests ]
let _ = run_test_tt_main suite