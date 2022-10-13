open OUnit2
open Processor
open Registers
open Utilities

let test_dec_to_bin (name : string) (num : int) (expected_output : string) :
    test =
  name >:: fun _ -> assert_equal expected_output (dec_to_bin num)
