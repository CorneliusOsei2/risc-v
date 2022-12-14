open OUnit2
open ExecGen
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

  (*[test_split_stype name n expected_output] constructs an OUnit test
    named [name] that asserts the quality of expected output with [split_stype n]*)
  let test_split_stype (name : string) (n : string)
      (expected_output : string * string list) : test =
    name >:: fun _ ->
    assert_equal expected_output (split_stype n) ~printer:string_of_insn

  (*[test_valid_register name r expected_output] constructs an OUnit test
    named [name] that asserts the quality of expected output with [register_check r]*)
  let test_valid_register (name : string) (r : string) (expected_output : bool)
      : test =
    name >:: fun _ -> assert_equal expected_output (register_check r)

  (*[test_fill_string name n c v expected_output] constructs an OUnit test
    named [name] that asserts the quality of expected output with [fill_string n c v]*)
  let test_fill_string (name : string) n c v (expected_output : string) : test =
    name >:: fun _ -> assert_equal expected_output (fill_string n c v)

  (*[test_fill_string_rev name n c v expected_output] constructs an OUnit test
    named [name] that asserts the quality of expected output with [fill_string_rev n c v]*)
  let test_fill_string_rev (name : string) n c v (expected_output : string) :
      test =
    name >:: fun _ -> assert_equal expected_output (fill_string_rev n c v)

  (*[test_pow name a n expected_output] constructs an OUnit test
    named [name] that asserts the quality of expected output with [pow a n]*)
  let test_pow (name : string) a n (expected_output : int) : test =
    name >:: fun _ ->
    assert_equal expected_output (pow a n) ~printer:string_of_int

  (*[test_string_of_list name lst expected_output] constructs an OUnit test
    named [name] that asserts the quality of expected output with [string_of_list lst]*)
  let test_string_of_list (name : string) (lst : string list)
      (expected_output : string) : test =
    name >:: fun _ ->
    assert_equal expected_output (string_of_list lst) ~printer:String.escaped

  (*[test_list_of_string name s expected_output] constructs an OUnit test
    named [name] that asserts the quality of expected output with [list_of_string s]*)
  let test_list_of_string (name : string) (s : string)
      (expected_output : string list) : test =
    name >:: fun _ -> assert_equal expected_output (list_of_string s)

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
      test_split_stype "binary immediate" "sw x3, 0b10111(x6)"
        ("sw", [ "x3"; "0b10111"; "x6" ]);
      test_split_stype "decimal immediate" "sb x3, 55(x6)"
        ("sb", [ "x3"; "55"; "x6" ]);
    ]

  let valid_register_tests =
    [
      test_valid_register "valid register" "x13" true;
      test_valid_register "invalid register" "x145" false;
      test_valid_register "invalid register" "x-145" false;
      test_valid_register "valid register" "13x" false;
      test_valid_register "invalid register - whitespace string" " " false;
      test_valid_register "valid register" "x1" true;
      test_valid_register "invalid register" "xop" false;
      test_valid_register "invalid register" "..." false;
      test_valid_register "invalid register - empty" "" false;
    ]

  let fill_strings_tests =
    [
      test_fill_string "pad with whitespace" 8 ' ' "hello" "   hello";
      test_fill_string "pad front with 0s" 4 '0' "99" "0099";
      test_fill_string "pad front with 0s" 4 ' ' "" "    ";
      test_fill_string_rev "pad back with 0s" 5 '0' "99" "99000";
      test_fill_string_rev "pad back with 1s" 10 '1' "0" "0111111111";
    ]

  let pow_tests =
    [
      test_pow "2^5" 2 5 32;
      test_pow "-2^5" ~-2 5 ~-32;
      test_pow "3^2" 3 2 9;
      test_pow "2^10" 2 10 1024;
    ]

  let string_list_tests =
    [
      test_string_of_list "non-empty string" [ "1"; "2"; "3" ] "[1, 2, 3]";
      test_string_of_list "empty list" [] "[]";
      test_list_of_string "string to list" "1, 2, 3" [ "1"; "2"; "3" ];
      test_list_of_string "string of registers to list" "x10, x12, x11"
        [ "x10"; "x12"; "x11" ];
    ]

  let tests =
    List.flatten
      [
        split_riu_instruction_tests;
        split_stype_tests;
        valid_register_tests;
        fill_strings_tests;
        pow_tests;
        string_list_tests;
      ]
end

(************************************ IO Tests *********************************** *)

module IOTests = struct
  (** [test_file_to_list name file expected_output] constructs an OUnit test named
    [name] that converts a txt file [file] to a string list with
    [file_to_list file]. *)
  let test_file_to_list (name : string) (file : string)
      (expected_output : string list) : test =
    name >:: fun _ -> assert_equal expected_output (file_to_list file)

  (** [test_list_to_file name list expected_output] constructs an OUnit test named
    [name] that converts a string list [lst] to a txt file with
    [list_to_file list]. *)
  let test_list_to_file (name : string) (list : string list)
      (expected_output : unit) : test =
    name >:: fun _ -> assert_equal expected_output (list_to_file list)

  let test = "data" ^ Filename.dir_sep ^ "test.txt"

  let io_tests =
    [
      test_file_to_list "test that converts file to list" test
        [ "addi x4, x1, 352"; "addi x1, x1, 15"; "sw x1, 8(x4)" ];
      test_list_to_file "test that converts list to file"
        [ "addi x4, x1, 352"; "addi x1, x1, 15"; "sw x1, 8(x4)" ]
        ();
    ]

  let data_dir_prefix = "data" ^ Filename.dir_sep
  let e1 = [ "addi x1, x1, 6"; "addi x1, x1, 6"; "addi x1, x1, 6" ]
  let file_to_list_tests = []
  (* [ test_file_to_list "file to list" (data_dir_prefix ^ "test" ^ ".txt") e1 ] *)
end
(************************************ Registers Tests *********************************** *)

module RegisterTests = struct
  let rfile = Registers.init ()
  let rfile1 = update_register "x1" 5 rfile
  let rfile2 = update_register "x2" 7 rfile
  let rfile3 = update_register "x3" 2147483648 rfile
  let rfile4 = update_register "x4" (-2147483648) rfile
  let lower_bound = (pow 2 31 - 1) * -1 |> of_int
  let upper_bound = pow 2 31 - 1 |> of_int
  let rfile5 = update_register "x7" (Int32.to_int lower_bound) rfile
  let rfile6 = update_register "x8" (Int32.to_int upper_bound) rfile

  (** [test_register name r expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with
    [Registers.get_register r rfile]. *)
  let test_register (name : string) (r : string)
      (rfile : (int32 * bool) RegisterFile.t) (expected_output : int32) : test =
    name >:: fun _ ->
    assert_equal expected_output (get_register r rfile) ~printer:Int32.to_string

  (** [test_gen_register name i expected_output] constructs an OUnit test named
    [name] that tests an integer [i] converts to string "xi" with
     [Registers.gen_register i] *)
  let test_gen_register (name : string) (i : int) (expected_output : string) :
      test =
    name >:: fun _ ->
    assert_equal expected_output (gen_register i) ~printer:Fun.id

  (** [test_pp_registers name rfile expected_output] constructs an OUnit test named
    [name] that tests whether  a register file will be printed with
     [Registers.pp_registers rfile] *)
  let test_pp_registers (name : string) (rfile : (int32 * bool) RegisterFile.t)
      (expected_output : unit) : test =
    name >:: fun _ -> assert_equal expected_output (pp_registers rfile)

  let register_tests =
    [
      test_register "init () values" "x1" rfile 0l;
      test_register "random register update test" "x1" rfile1 5l;
      test_register "random register update test" "x2" rfile2 7l;
      test_register
        "edge register test to test max representation of a register is 2^31"
        "x3" rfile3 2147483648l;
      test_register
        "edge register test to test min representation of a register is -2^31"
        "x4" rfile4 (-2147483648l);
      test_register "random register lower bound test" "x7" rfile5 lower_bound;
      test_register "random register upper bound test" "x8" rfile6 upper_bound
      (* test_pp_registers "random pp_register test" rfile1 (); *);
    ]
end

(************************************ Memory Tests *********************************** *)
module MemoryTests = struct
  let mem = Memory.init ()
  let mem1 = Memory.update_memory 0 5l mem
  let mem2 = Memory.update_memory 15 2147483648l mem1
  let mem3 = Memory.update_memory 20 (-2147483648l) mem2
  let mem4 = Memory.update_memory 15 78l mem3

  (*[test_memory name addr expected_output] constructs an OUnit test named
      [name] that asserts the quality of [expected_output] with
      [Memory.get_memory addr mem]. *)
  let test_memory (name : string) (addr : int)
      (mem : (int32 * bool) Memory.Memory.t) (expected_output : Int32.t) : test
      =
    name >:: fun _ ->
    assert_equal
      (Memory.get_memory addr mem)
      expected_output ~printer:Int32.to_string

  (** [test_pp_memory name rfile expected_output] constructs an OUnit test named
    [name] that tests whether  a register file will be printed with
     [Registers.pp_registers rfile] *)
  let test_pp_memory (name : string) (mem : (int32 * bool) Memory.Memory.t)
      (expected_output : unit) : test =
    name >:: fun _ -> assert_equal expected_output (Memory.pp_memory mem)

  let memory_tests =
    [
      test_memory "init memory test" 0 mem 0l;
      test_memory "random memory update test" 0 mem1 5l;
      test_memory
        "edge memory test to test that the maximum value a memory address can \
         represent is 2^31"
        15 mem2 2147483648l;
      test_memory
        "edge memory test to test that the minimum value a memory address can \
         represent is -2^31"
        15 mem2 (-2147483648l);
      test_memory
        "random memory test that tests if a memory address with an existing \
         value is properly updated"
        15 mem4 78l
      (* test_pp_memory "random pp_memory test" mem1 (); *);
    ]
end
(************************************ Memory Tests *********************************** *)

let _ = Memory.(init () |> pp_memory)

(************************************ Process Instructions Tests *********************************** *)
module ProcessInstructionsTests = struct
  include RegisterTests
  include MemoryTests

  (*[test_register_stype name r mem expected output] constructs an OUnit test
    named [name] that asserts the value of register [r] in the register file in
    [mem] for stype operations with [get_register r mem]*)
  let test_register_stype (name : string) (r : string)
      (mem : (int32 * bool) RegisterFile.t * (int32 * bool) Memory.Memory.t)
      (expected_output : int32) : test =
    name >:: fun _ ->
    assert_equal
      (get_register r (fst mem))
      expected_output ~printer:Int32.to_string

  (*[test_memory_stype name r mem expected output] constructs an OUnit test
    named [name] that asserts the value of memory address [addr] in
    [mem] for stype operations with [get_memory addr mem]*)
  let test_memory_stype (name : string) (addr : int)
      (mem : (int32 * bool) RegisterFile.t * (int32 * bool) Memory.Memory.t)
      (expected_output : int32) : test =
    name >:: fun _ ->
    assert_equal
      (Memory.get_memory addr (snd mem))
      expected_output ~printer:Int32.to_string

  (*[test_process_step_insns name i r m expected output] constructs an OUnit test
       named [name] that asserts the value of [ProcessInstructions.process_step_insns i r m]
      with [expected_output]. *)
  let test_process_step_insns (name : string) (i : string)
      (r : (int32 * bool) RegisterFile.t) (m : (int32 * bool) Memory.Memory.t)
      (expected_output :
        (int32 * bool) RegisterFile.t * (int32 * bool) Memory.Memory.t) : test =
    name >:: fun _ -> assert_equal (process_step_insns i r m) expected_output

  let rfile = Registers.init ()
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
  let mem = Memory.init ()
  let mem1 = process_stype "sw" "x18" "12" "x17" rfile20 mem
  let mem2 = process_stype "sb" "x21" "12" "x1" rfile21 (snd mem1)
  let mem3 = process_stype "lw" "x22" "5" "x2" rfile21 (snd mem2)
  let mem4 = process_stype "sb" "x9" "2" "x23" rfile22 (snd mem3)
  let mem5 = process_stype "lb" "x24" "2" "x23" rfile22 (snd mem4)

  let process_optype_tests =
    [
      test_register
        "random case of addi with positive offset where we first initialize an \
         empty register"
        "x1" rfile1 9l;
      test_register
        "random case of addi with positive offset where we use the value in 1 \
         register to initialize another register"
        "x2" rfile2 19l;
      test_register
        "random case where we test that the values manipulated by previous \
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
      test_register "random case of the add operation" "x3" rfile3 28l;
      test_register "random case of the sub operation" "x5" rfile5 19l;
      test_register "random case of the add operation" "x6" rfile6 1l;
      test_register "random case of the or operation" "x9" rfile9 11l;
      test_register "random case of the xor operation" "x10" rfile10 9l;
      test_register "random case of the sll operation" "x11" rfile11 3072l;
      test_register "random case of the srl operation" "x12" rfile12 1l;
      test_register
        "random case of the slt operation where we compare two signed integers"
        "x14" rfile14 1l;
      test_register
        "random case of the sltu operation where we compare two unsigned \
         integers"
        "x15" rfile15 0l;
      test_register "random case of the andi operation" "x17" rfile17 12l;
      test_register "random case of the ori operation" "x18" rfile18 316l;
      test_register "random case of the xori operation" "x19" rfile19 402l;
      test_memory_stype
        "random case of sw operation in which we check the updated value at \
         the memory address"
        24 mem1 60l;
      test_register_stype
        "random case of sw operation in which we check the value of the \
         register destination  which should be unchanged"
        "x18" mem1 316l;
      test_memory_stype
        "edge case of store operation in which we check that the max \
         representation of store byte is 2^8 - 1 since it wraps values greater \
         than this"
        21 mem2 0l;
      test_register_stype
        "random case of lw operation in which we check the value of the \
         register destination  which should be changed"
        "x22" mem3 316l;
      test_memory_stype
        "random case of lw operation in which we check the value at the memory \
         address which should be unchanged"
        24 mem3 60l;
      test_memory_stype
        "random case of sb operation in which we check the value of the \
         register destination  which should be changed"
        16 mem4 11l;
      test_register_stype
        "random case of lb operation in which we check the value of the \
         register destination  which should be changed"
        "x24" mem5 11l;
    ]

  let instr_list =
    [
      "addi x1, x1, 9";
      "addi x2, x1, 10";
      "add x3, x1, x2";
      "addi x4, x4, 204";
      "sub x5, x3, x1";
      "and x6, x5, x1";
      "addi x7, x7, 3";
      "srl x8, x4, x7";
      "or x9, x7, x8";
      "xor x10, x7, x8";
      "sw x4, 7(x1)";
      "sw x8, 5(x7)";
      "sb x8, 19(x1)";
      "lw x21, 5(x7)";
    ]

  let tuple_list = process_file_insns instr_list
  let reg_list n = List.nth (List.map (fun out -> fst out) tuple_list) n
  let mem_list n = List.nth (List.map (fun out -> snd out) tuple_list) n

  let process_file_insns_tests =
    [
      test_register "file test" "x1" (reg_list 0) 9l;
      test_register "file test" "x2" (reg_list 0) 19l;
      test_register "file test" "x3" (reg_list 0) 28l;
      test_register "file test" "x4" (reg_list 0) 204l;
      test_register "file test" "x5" (reg_list 0) 19l;
      test_register "file test" "x6" (reg_list 0) 1l;
      test_register "file test" "x7" (reg_list 0) 3l;
      test_register "file test" "x8" (reg_list 0) 25l;
      test_register "file test" "x9" (reg_list 0) 27l;
      test_register "file test" "x10" (reg_list 0) 26l;
      test_memory "file test" 16 (mem_list 0) 204l;
      test_memory "file test" 28 (mem_list 0) 25l;
      test_register "file test" "x21" (reg_list 0) 25l;
    ]

  let process_step_insns_tests =
    [
      test_process_step_insns "step add test" "add x3, x1, x2" rfile2 mem
        (rfile3, mem);
      test_process_step_insns "step addi test" "addi x4, x4, 2047" rfile3 mem
        (rfile4, mem);
      test_process_step_insns "step sub test" "sub x5, x3, x1" rfile4 mem
        (rfile5, mem);
      test_process_step_insns "step sll test" "sll x11, x7, x8" rfile10 mem
        (rfile11, mem);
      test_process_step_insns "step xor test" "xor x10, x7, x8" rfile9 mem
        (rfile10, mem);
      test_process_step_insns "step sw test" "sw x18, 12(x17)" rfile20 mem mem1;
      test_process_step_insns "step sb test" "sb x9, 2(x23)" rfile22 (snd mem3)
        mem4;
      test_process_step_insns "step lw test" "lw x22, 5(x2)" rfile21 (snd mem2)
        mem3;
      test_process_step_insns "step lb test" "lb x24, 2(x23)" rfile22 (snd mem4)
        mem5;
    ]
end

let suite =
  "test suite for ExecGen"
  >::: List.flatten
         [
           UtilityTests.tests;
           IOTests.file_to_list_tests;
           RegisterTests.register_tests;
           ProcessInstructionsTests.process_optype_tests;
           ProcessInstructionsTests.process_file_insns_tests;
           ProcessInstructionsTests.process_step_insns_tests;
           MemoryTests.memory_tests;
           IOTests.io_tests;
         ]

let _ = run_test_tt_main suite
