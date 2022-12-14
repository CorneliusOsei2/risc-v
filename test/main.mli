module UtilityTests :
  sig
    val test_split_instruction :
      string -> string -> string * string list -> OUnitTest.test
    val test_split_stype :
      string -> string -> string * string list -> OUnitTest.test
    val test_valid_register : string -> string -> bool -> OUnitTest.test
    val split_riu_instruction_tests : OUnitTest.test list
    val split_stype_tests : OUnitTest.test list
    val valid_register_tests : OUnitTest.test list
    val tests : OUnitTest.test list
  end
module IOTests :
  sig
    val test_file_to_list : string -> string -> string list -> OUnitTest.test
    val test_list_to_file : string -> string list -> unit -> OUnitTest.test
    val test : string
    val io_tests : OUnitTest.test list
    val data_dir_prefix : string
    val e1 : string list
    val file_to_list_tests : 'a list
  end
module RegisterTests :
  sig
    val rfile : 'a
    val rfile1 : 'a
    val rfile2 : 'a
    val rfile3 : 'a
    val rfile4 : 'a
    val lower_bound : int32
    val upper_bound : int32
    val rfile5 : 'a
    val rfile6 : 'a
    val test_register : string -> string -> 'a -> int32 -> OUnitTest.test
    val test_gen_register : string -> int -> string -> OUnitTest.test
    val test_pp_registers : string -> 'a -> unit -> OUnitTest.test
    val register_tests : OUnitTest.test list
  end
module MemoryTests :
  sig
    val mem : 'a
    val mem1 : 'a
    val mem2 : 'a
    val mem3 : 'a
    val mem4 : 'a
    val test_memory : string -> int -> 'a -> int32 -> OUnitTest.test
    val test_pp_memory : string -> 'a -> unit -> OUnitTest.test
    val memory_tests : OUnitTest.test list
  end
module ProcessInstructionsTests :
  sig
    val lower_bound : int32
    val upper_bound : int32
    val test_register : string -> string -> 'a -> int32 -> OUnitTest.test
    val test_gen_register : string -> int -> string -> OUnitTest.test
    val test_pp_registers : string -> 'a -> unit -> OUnitTest.test
    val register_tests : OUnitTest.test list
    val test_memory : string -> int -> 'a -> int32 -> OUnitTest.test
    val test_pp_memory : string -> 'a -> unit -> OUnitTest.test
    val memory_tests : OUnitTest.test list
    val test_register_stype :
      string -> string -> 'a * 'b -> int32 -> OUnitTest.test
    val test_memory_stype :
      string -> int -> 'a * 'b -> int32 -> OUnitTest.test
    val test_process_step_insns :
      string -> string -> 'a -> 'b -> 'c * 'd -> OUnitTest.test
    val rfile : 'a
    val rfile1 : 'a
    val rfile2 : 'a
    val rfile3 : 'a
    val rfile4 : 'a
    val rfile5 : 'a
    val rfile6 : 'a
    val rfile7 : 'a
    val rfile8 : 'a
    val rfile9 : 'a
    val rfile10 : 'a
    val rfile11 : 'a
    val rfile12 : 'a
    val rfile13 : 'a
    val rfile14 : 'a
    val rfile15 : 'a
    val rfile16 : 'a
    val rfile17 : 'a
    val rfile18 : 'a
    val rfile19 : 'a
    val rfile20 : 'a
    val rfile21 : 'a
    val rfile22 : 'a
    val mem : 'a
    val mem1 : 'a
    val mem2 : 'a
    val mem3 : 'a
    val mem4 : 'a
    val mem5 : 'a
    val process_optype_tests : OUnitTest.test list
    val instr_list : string list
    val tuple_list : 'a
    val reg_list : int -> 'a
    val mem_list : int -> 'a
    val process_file_insns_tests : OUnitTest.test list
    val process_step_insns_tests : OUnitTest.test list
  end
val suite : OUnitTest.test
