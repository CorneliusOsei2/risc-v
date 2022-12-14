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

  let test_valid_register (name : string) (r : string) (expected_output : bool)
      : test =
    name >:: fun _ -> assert_equal expected_output (register_check r)

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

  let valid_register_tests =
    [
      test_valid_register "valid" "x13" true;
      test_valid_register "invalid" "x145" false;
      test_valid_register "invalid" "x-145" false;
    ]

  let tests =
    List.flatten
      [ split_riu_instruction_tests; split_stype_tests; valid_register_tests ]
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

module RegisterTests = struct
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

   [test_memory name addr expected_output] constructs an OUnit test named
       [name] that asserts the quality of [expected_output] with
       [Memory.get_memory addr mem]. 
module MemoryTests = struct
  let test_memory (name : string) (addr : int)
      (mem : ('t * bool) Memory.Memory.t) (expected_output : int) : test =
    name >:: fun _ ->
    assert_equal
      (Memory.get_memory addr mem)
      expected_output ~printer:string_of_int
end

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

(************************************ Process Instructions Tests *********************************** *)
module ProcessInstructionsTests = struct
  include RegisterTests

  let test_register_stype (name : string) (r : string)
      (reg_mem : (int32 * bool) RegisterFile.t * (int32 * bool) Memory.Memory.t)
      (expected_output : int32) : test =
    name >:: fun _ ->
    assert_equal
      (get_register r (fst reg_mem))
      expected_output ~printer:Int32.to_string

  let test_memory_stype (name : string) (addr : int)
      (reg_mem : (int32 * bool) RegisterFile.t * (int32 * bool) Memory.Memory.t)
      (expected_output : int32) : test =
    name >:: fun _ ->
    assert_equal
      (Memory.get_memory addr (snd reg_mem))
      expected_output ~printer:Int32.to_string

  let test_process_file_insns (name : string) (lst : string list)
      (expected_output :
        ((int32 * bool) RegisterFile.t * (int32 * bool) Memory.Memory.t) list) :
      test =
    name >:: fun _ -> assert_equal (process_file_insns lst) expected_output

  let test_process_step_insns (name : string) (i : string)
      (r : (int32 * bool) RegisterFile.t) (m : (int32 * bool) Memory.Memory.t)
      (expected_output :
        (int32 * bool) RegisterFile.t * (int32 * bool) Memory.Memory.t) : test =
    name >:: fun _ -> assert_equal (process_step_insns i r m) expected_output

  let rfile = Registers.init
  let rfile1 = process_itype "addi" "x1" "x1" "9" rfile
  let rfile2 = process_itype "addi" "x2" "x1" "10" rfile1
  let rfile3 = process_rtype "add" "x3" "x1" "x2" rfile2
  let rfile4 = process_itype "addi" "x4" "x4" "2047" rfile3
  let rfile5 = process_rtype "sub" "x5" "x3" "x1" rfile4
  let rfile6 = process_rtype "and" "x6" "x5" "x1" rfile5
  let rfile7 = process_itype "addi" "x7" "x7" "3" rfile6
  let rfile8 = process_itype "addi" "x8" "x8" "10" rfile7
  let rfile9 = process_rtype "or" "x9" "x7" "x8" rfile8
  let rfile10 = process_rtype "xor" "x10" "x7" "x8" rfile9
  let rfile11 = process_rtype "sll" "x11" "x7" "x8" rfile10
  let rfile12 = process_rtype "srl" "x12" "x8" "x7" rfile11
  let rfile13 = process_itype "addi" "x13" "x13" "-25" rfile12
  let rfile14 = process_rtype "slt" "x14" "x13" "x14" rfile13
  let rfile15 = process_rtype "sltu" "x15" "x13" "x14" rfile14
  let rfile16 = process_itype "addi" "x16" "x16" "300" rfile15
  let rfile17 = process_itype "andi" "x17" "x16" "159" rfile16
  let rfile18 = process_itype "ori" "x18" "x16" "48" rfile17
  let rfile19 = process_itype "xori" "x19" "x18" "174" rfile18
  let rfile20 = process_itype "addi" "x20" "x20" "-2048" rfile19
  let rfile21 = process_itype "addi" "x21" "x21" "256" rfile20
  let rfile22 = process_itype "addi" "x23" "x23" "14" rfile20
  let mem = Memory.init
  let reg_mem1 = process_stype "sw" "x18" "12" "x17" rfile20 mem
  let reg_mem2 = process_stype "sb" "x21" "12" "x1" rfile21 (snd reg_mem1)
  let reg_mem3 = process_stype "lw" "x22" "5" "x2" rfile21 (snd reg_mem2)
  let reg_mem4 = process_stype "sb" "x9" "2" "x23" rfile22 (snd reg_mem3)
  let reg_mem5 = process_stype "lb" "x24" "2" "x23" rfile22 (snd reg_mem4)

  let process_optype_tests =
    [
      test_register
        "non-edge case of addi with positive offset where we first initialize \
         an empty register"
        "x1" rfile1 9l;
      test_register
        "non-edge case of addi with positive offset where we use the value in \
         1 register to initialize another register"
        "x2" rfile2 19l;
      test_register
        "non-edge case where we test that the values manipulated by previous \
         operations on previous register files still holds for the new \
         register file"
        "x1" rfile2 9l;
      test_register
        "edge case where we test that the maximum integer that an itype \
         operations can represent is 2^11 - 1 "
        "x4" rfile4 2047l;
      test_register
        "edge case where we test that the minimum integer that an itype \
         operation can represent is -2^11 "
        "x20" rfile20 (-2048l);
      test_register "non-edge case of the add operation" "x3" rfile3 28l;
      test_register "non-edge case of the sub operation" "x5" rfile5 19l;
      test_register "non-edge case of the add operation" "x6" rfile6 1l;
      test_register "non-edge case of the or operation" "x9" rfile9 11l;
      test_register "non-edge case of the xor operation" "x10" rfile10 9l;
      test_register "non-edge case of the sll operation" "x11" rfile11 3072l;
      test_register "non-edge case of the srl operation" "x12" rfile12 1l;
      test_register
        "non-edge case of the slt operation where we compare two signed \
         integers"
        "x14" rfile14 1l;
      test_register
        "non-edge case of the sltu operation where we compare two unsigned \
         integers"
        "x15" rfile15 0l;
      test_register "non-edge case of the andi operation" "x17" rfile17 12l;
      test_register "non-edge case of the ori operation" "x18" rfile18 316l;
      test_register "non-edge case of the xori operation" "x19" rfile19 402l;
      test_memory_stype
        "non-edge case of sw operation in which we check the updated value at \
         the memory address"
        24 reg_mem1 60l;
      test_register_stype
        "non-edge case of sw operation in which we check the value of the \
         register destination  which should be unchanged"
        "x18" reg_mem1 316l;
      test_memory_stype
        "edge case of store operation in which we check that the max \
         representation of store byte is 2^8 - 1 since it wraps values greater \
         than this"
        21 reg_mem2 0l;
      test_register_stype
        "non-edge case of lw operation in which we check the value of the \
         register destination  which should be changed"
        "x22" reg_mem3 316l;
      test_memory_stype
        "non-edge case of lw operation in which we check the value at the \
         memory address which should be unchanged"
        24 reg_mem3 60l;
      test_memory_stype
        "non-edge case of sb operation in which we check the value of the \
         register destination  which should be changed"
        16 reg_mem4 11l;
      test_register_stype
        "non-edge case of lb operation in which we check the value of the \
         register destination  which should be changed"
        "x24" reg_mem5 11l;
    ]

  let process_file_insns_tests = []

  let process_step_insns_tests =
    [
      test_process_step_insns "step test" "addi x1, x1, 9" rfile mem
        (rfile1, mem);
      test_process_step_insns "step test" "addi x2, x0, 7" rfile mem
        (rfile2, mem)
      (* test_process_step_insns "step test" "sub x2, x1, x3" rfile mem
           (rfile2, mem);
         test_process_step_insns "step test" "andi x2, x1, 3" rfile mem
           (rfile3, mem);
         test_process_step_insns "step test" "sll x2, x1, x3" rfile mem
           (rfile4, mem); *);
    ]
end

let suite =
  "test suite for Processor"
  >::: List.flatten
         [
           UtilityTests.tests;
           IOTests.file_to_list_tests;
           RegisterTests.register_tests (* MemoryTests.memory_tests; *);
           ProcessInstructionsTests.process_optype_tests;
           ProcessInstructionsTests.process_file_insns_tests;
           ProcessInstructionsTests.process_step_insns_tests;
         ]

let _ = run_test_tt_main suite
