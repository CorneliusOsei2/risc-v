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

let suite = "test suite for A2" >::: List.flatten [ register_tests ]
let _ = run_test_tt_main suite
