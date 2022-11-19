open OUnit2
open Processor
open Registers
open Utilities
open IO
open ProcessInstructions
open GenerateInstructions
open Int32

(************************************ Utilities Tests *********************************** *)

module UtilityTests = struct
  (*[test_split_instruction name n expected_output] constructs an OUnit test
    named [name] that asserts the quality of expected output with [split_instruction n]*)
  let test_split_instruction (name : string) (n : string)
      (expected_output : string * string list) : test =
    name >:: fun _ ->
    assert_equal expected_output (split_instruction n) ~printer:string_of_insn

  let test_split_stype (name : string) (n : string)
      (expected_output : string * string list) : test =
    name >:: fun _ ->
    assert_equal expected_output (split_stype n) ~printer:string_of_insn

  let split_riu_instruction_tests =
    [
      test_split_instruction "no extra whitespaces" "add x1, x2, x3"
        ("add", [ "x1"; "x2"; "x3" ]);
      test_split_instruction "extra whitespaces at start" "     sub x1, x2, x3"
        ("sub", [ "x1"; "x2"; "x3" ]);
      test_split_instruction "trailing whitespace"
        "andi x1, x2, 12                  "
        ("andi", [ "x1"; "x2"; "12" ]);
      test_split_instruction "extra whitespaces in between"
        "     sub x1,            x2,           x3"
        ("sub", [ "x1"; "x2"; "x3" ]);
      test_split_instruction "lui instruction" "     lui x1, 12"
        ("lui", [ "x1"; "12" ]);
      test_split_instruction "andi instruction" "andi x1, x2, 12"
        ("andi", [ "x1"; "x2"; "12" ]);
      test_split_instruction "lui instruction" "lui x2, 0xfffff000"
        ("lui", [ "x2"; "0xfffff000" ]);
    ]

  let split_stype_tests =
    [
      test_split_stype "valid sw instruction" "sw x2, 0x45(x4)"
        ("sw", [ "x2"; "0x45"; "x4" ]);
      test_split_stype "binary" "sw x3, 0b10111(x6)"
        ("sw", [ "x3"; "0b10111"; "x6" ]);
    ]

  let tests = List.flatten [ split_riu_instruction_tests; split_stype_tests ]
end

(************************************ IO Tests *********************************** *)

(** [test_file_to_list name file expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with
    [file_to_list file]. *)
let test_file_to_list (name : string) (file : string)
    (expected_output : string list) : test =
  name >:: fun _ -> assert_equal expected_output (file_to_list file)

module IOTests = struct
  let data_dir_prefix = "data" ^ Filename.dir_sep
  let e1 = [ "addi x1, x1, 6"; "addi x1, x1, 6"; "addi x1, x1, 6" ]
  let file_to_list_tests = []
  (* [ test_file_to_list "file to list" (data_dir_prefix ^ "test" ^ ".txt") e1 ] *)
end
(************************************ Registers Tests *********************************** *)

let rfile = Registers.init
let rfile1 = update_register "x1" 5 rfile
let rfile2 = update_register "x2" 7 rfile
let rfile3 = update_register "x3" 56 rfile
let rfile4 = update_register "x4" 112 rfile
let rfile5 = update_register "x5" 320 rfile
let rfile6 = update_register "x6" 97 rfile
let lower_bound = (pow 2 31 - 1) * -1 |> of_int
let upper_bound = pow 2 31 - 1 |> of_int
let rfile7 = update_register "x7" (Int32.to_int lower_bound) rfile
let rfile8 = update_register "x8" (Int32.to_int upper_bound) rfile

(** [test_register name r expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with
    [Registers.get_register r rfile]. *)
let test_register (name : string) (r : string)
    (rfile : (int32 * bool) RegisterFile.t) (expected_output : int32) : test =
  name >:: fun _ ->
  assert_equal expected_output (get_register r rfile) ~printer:Int32.to_string

module RegisterTests = struct
  let register_tests =
    [
      test_register "init values" "x1" rfile 0l;
      test_register "update register" "x1" rfile1 5l;
      test_register "update register" "x2" rfile2 7l;
      test_register "update register" "x3" rfile3 56l;
      test_register "update register" "x4" rfile4 112l;
      test_register "update register" "x5" rfile5 320l;
      test_register "update register" "x6" rfile6 97l;
      test_register "lower bound" "x7" rfile7 lower_bound;
      test_register "upper bound" "x8" rfile8 upper_bound;
    ]
end
(************************************ Memory Tests *********************************** *)
(*
   let mem = memory_init
   let mem1 = update_memory 0 5 mem
   let mem2 = update_memory 1 2 mem
   let mem3 = update_memory 15 5 mem

   (** [test_memory name addr expected_output] constructs an OUnit test named
       [name] that asserts the quality of [expected_output] with
       [Memory.get_memory addr mem]. *)
   let test_memory (name : string) (addr : int) (mem : (int * bool) Memory.t)
       (expected_output : int) : test =
     name >:: fun _ ->
     assert_equal (get_memory addr mem) expected_output ~printer:string_of_int

   module MemoryTests = struct
     let memory_tests =
       let _ = GenerateInstructions.gen_itype "subi" 15 [] in
       [
         test_memory "init values" 0 mem 0;
         test_memory "update memory" 0 mem1 5;
         test_memory "update memory" 1 mem2 2;
         test_memory "update memory" 15 mem3 5;
       ]
   end *)
(************************************ Memory Tests *********************************** *)

let _ = Memory.(init |> pp_memory)

let suite =
  "test suite for A2"
  >::: List.flatten
         [
           UtilityTests.tests;
           IOTests.file_to_list_tests;
           RegisterTests.register_tests (* MemoryTests.memory_tests; *);
         ]

let _ = run_test_tt_main suite