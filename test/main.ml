open OUnit2
open Processor
open Registers
open Memory
open Utilities
open IO
open ProcessInstructions
open GenerateInstructions
open Utilities

(************************************ Utilities Tests *********************************** *)

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
  name >:: fun _ ->
  assert_equal expected_output (split_instruction n) ~printer:pp_instruction

let split_instruction_tests =
  [
    test_split_instruction "no extra whitespaces" "add x1, x2, x3"
      ("add", [ "x1"; "x2"; "x3" ]);
    test_split_instruction "extra whitespaces" "     sub x1, x2, x3"
      ("sub", [ "x1"; "x2"; "x3" ]);
  ]

(************************************ IO Tests *********************************** *)

(** [test_file_to_list name file expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with
    [file_to_list file]. *)
let test_file_to_list (name : string) (file : string)
    (expected_output : string list) : test =
  name >:: fun _ -> assert_equal expected_output (file_to_list file)

let data_dir_prefix = "data" ^ Filename.dir_sep
let e1 = [ "addi x1, x1, 6"; "addi x1, x1, 6"; "addi x1, x1, 6" ]
let file_to_list_tests = []
(* [ test_file_to_list "file to list" (data_dir_prefix ^ "test" ^ ".txt") e1 ] *)

(************************************ Registers Tests *********************************** *)

let rfile = register_init
let rfile1 = update_register "x1" 5l rfile

(** [test_register name r expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with
    [Registers.get_register r rfile]. *)
let test_register (name : string) (r : string)
    (rfile : (int32 * bool) RegisterFile.t) (expected_output : int32) : test =
  name >:: fun _ ->
  assert_equal expected_output (get_register r rfile) ~printer:Int32.to_string

let register_tests =
  [
    test_register "init values" "x1" rfile 0l;
    test_register "update register" "x1" rfile1 5l;
  ]

(************************************ Memory Tests *********************************** *)

let mem = memory_init
let mem1 = update_memory 0 5 mem

(** [test_memory name addr expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with
    [Memory.get_memory addr mem]. *)
let test_memory (name : string) (addr : int) (mem : (int * bool) Memory.t)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal (get_memory addr mem) expected_output ~printer:string_of_int

let memory_tests =
  let _ = GenerateInstructions.gen_itype "subi" 15 [] in
  [ test_memory "init values" 0 mem 0; test_memory "update memory" 0 mem1 5 ]

(************************************ Memory Tests *********************************** *)

let suite =
  "test suite for A2"
  >::: List.flatten
         [
           dec_conversions_tests;
           split_instruction_tests;
           file_to_list_tests;
           register_tests;
           memory_tests;
         ]

let _ = run_test_tt_main suite