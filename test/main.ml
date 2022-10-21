open OUnit2
open Processor
open Utilities

(*[test_dec_to_bin name n expected_output] constructs an OUnit test named [name]
    that asserts the quality of expected output with [dec_to_bin n]*)
let test_dec_to_bin (name : string) (n : int) (expected_output : string) : test
    =
  name >:: fun _ ->
  assert_equal expected_output (dec_to_bin n) ~printer:String.escaped

(*[test_dec_to_hex name n expected_output] constructs an OUnit test named [name]
    that asserts the quality of expected output with [dec_to_hex n]*)
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

(*[test_split_instruction name n expected_output] constructs an OUnit test
  named [name] that asserts the quality of expected output with [split_instruction n]*)
let test_split_instruction (name : string) (n : string)
    (expected_output : string * string list) : test =
  name >:: fun _ -> assert_equal expected_output (split_instruction n)

let split_instruction_tests =
  [
    test_split_instruction "" "add x1, x2," ("add", [ "x1"; "x2" ]);
    test_split_instruction "" "     sub x1, x2," ("sub", [ "x1"; "x2" ]);
  ]

let suite = "test suite for A2" >::: List.flatten [ dec_conversions_tests ]
let _ = run_test_tt_main suite