open OUnit2
open Processor
open Registers
open Utilities
open Input
open ProcessInstructions

let rfile = register_init
let rfile1 = update_register "x1" 5 rfile

(** [test_register name adv expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with
    [Registers.get_register r rfile]. *)
let test_register (name : string) (r : string)
    (rfile : (int * bool) RegisterFile.t) (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal (get_register "x1" rfile) expected_output ~printer:string_of_int

let register_tests =
  [
    test_register "init values" "x1" rfile 0;
    test_register "update register" "x1" rfile1 5;
  ]

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

let suite = "test suite for A2" >::: List.flatten [ dec_conversions_tests; register_tests  ]
let _ = run_test_tt_main suite

