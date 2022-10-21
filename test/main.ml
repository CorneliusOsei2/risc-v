open OUnit2
open Processor
open Registers
open Utilities
open Input
open ProcessInstructions

let pp_list lst =
  let start = "[" in
  let e = "]" in
  let rec print lst = match lst with [] -> "" | h :: t -> h ^ ";" ^ print t in
  start ^ print lst ^ e

let test_dec_to_bin (name : string) (num : int) (expected_output : string) :
    test =
  name >:: fun _ -> assert_equal expected_output (dec_to_bin num)

let test_file_to_list (name : string) (file : string)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal expected_output (file_to_list file) ~printer:pp_list

let data_dir_prefix = "data" ^ Filename.dir_sep
let e1 = [ "addi x1, x1, 6"; "addi x1, x1, 6"; "addi x1, x1, 6" ]
let inpt_file = data_dir_prefix ^ "test" ^ ".txt"
let insns_list = process_input_insns (file_to_list inpt_file)

let tests_file_to_list =
  [ test_file_to_list "" (data_dir_prefix ^ "test" ^ ".txt") [] ]

let suite = "test suite for A2" >::: List.flatten [ tests_file_to_list ]
let _ = insns_list
