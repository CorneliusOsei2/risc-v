module UtilityTests : sig
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

module IOTests : sig
  val test_file_to_list : string -> string -> string list -> OUnitTest.test
  val test_list_to_file : string -> string list -> unit -> OUnitTest.test
  val test : string
  val io_tests : OUnitTest.test list
  val data_dir_prefix : string
  val e1 : string list
  val file_to_list_tests : 'a list
end

module RegisterTests : sig
  val rfile : (Int32.t * bool) ExecGen.Registers.RegisterFile.t
  val rfile1 : (Int32.t * bool) ExecGen.Registers.RegisterFile.t
  val rfile2 : (Int32.t * bool) ExecGen.Registers.RegisterFile.t
  val rfile3 : (Int32.t * bool) ExecGen.Registers.RegisterFile.t
  val rfile4 : (Int32.t * bool) ExecGen.Registers.RegisterFile.t
  val lower_bound : int32
  val upper_bound : int32
  val rfile5 : (Int32.t * bool) ExecGen.Registers.RegisterFile.t
  val rfile6 : (Int32.t * bool) ExecGen.Registers.RegisterFile.t

  val test_register :
    string ->
    string ->
    (int32 * bool) ExecGen.Registers.RegisterFile.t ->
    int32 ->
    OUnit2.test

  val test_gen_register : string -> int -> string -> OUnitTest.test

  val test_pp_registers :
    string ->
    (int32 * bool) ExecGen.Registers.RegisterFile.t ->
    unit ->
    OUnit2.test

  val register_tests : OUnitTest.test list
end

module MemoryTests : sig
  val mem : (Int32.t * bool) ExecGen.Memory.Memory.t
  val mem1 : (Int32.t * bool) ExecGen.Memory.Memory.t
  val mem2 : (Int32.t * bool) ExecGen.Memory.Memory.t
  val mem3 : (Int32.t * bool) ExecGen.Memory.Memory.t
  val mem4 : (Int32.t * bool) ExecGen.Memory.Memory.t

  val test_memory :
    string ->
    int ->
    (int32 * bool) ExecGen.Memory.Memory.t ->
    Int32.t ->
    OUnit2.test

  val test_pp_memory :
    string -> (int32 * bool) ExecGen.Memory.Memory.t -> unit -> OUnit2.test

  val memory_tests : OUnitTest.test list
end

module ProcessInstructionsTests : sig
  val lower_bound : int32
  val upper_bound : int32

  val test_register :
    string ->
    string ->
    (int32 * bool) ExecGen.Registers.RegisterFile.t ->
    int32 ->
    OUnit2.test

  val test_gen_register : string -> int -> string -> OUnitTest.test

  val test_pp_registers :
    string ->
    (int32 * bool) ExecGen.Registers.RegisterFile.t ->
    unit ->
    OUnit2.test

  val register_tests : OUnitTest.test list

  val test_memory :
    string ->
    int ->
    (int32 * bool) ExecGen.Memory.Memory.t ->
    Int32.t ->
    OUnit2.test

  val test_pp_memory :
    string -> (int32 * bool) ExecGen.Memory.Memory.t -> unit -> OUnit2.test

  val memory_tests : OUnitTest.test list

  val test_register_stype :
    string ->
    string ->
    (int32 * bool) ExecGen.Registers.RegisterFile.t
    * (int32 * bool) ExecGen.Memory.Memory.t ->
    int32 ->
    OUnit2.test

  val test_memory_stype :
    string ->
    int ->
    (int32 * bool) ExecGen.Registers.RegisterFile.t
    * (int32 * bool) ExecGen.Memory.Memory.t ->
    int32 ->
    OUnit2.test

  val test_process_step_insns :
    string ->
    string ->
    (int32 * bool) ExecGen.Registers.RegisterFile.t ->
    (int32 * bool) ExecGen.Memory.Memory.t ->
    (int32 * bool) ExecGen.Registers.RegisterFile.t
    * (int32 * bool) ExecGen.Memory.Memory.t ->
    OUnit2.test

  val rfile : (Int32.t * bool) ExecGen.Registers.RegisterFile.t
  val rfile1 : (Int32.t * bool) ExecGen.Registers.RegisterFile.t
  val rfile2 : (Int32.t * bool) ExecGen.Registers.RegisterFile.t
  val rfile3 : (Int32.t * bool) ExecGen.Registers.RegisterFile.t
  val rfile4 : (Int32.t * bool) ExecGen.Registers.RegisterFile.t
  val rfile5 : (Int32.t * bool) ExecGen.Registers.RegisterFile.t
  val rfile6 : (Int32.t * bool) ExecGen.Registers.RegisterFile.t
  val rfile7 : (Int32.t * bool) ExecGen.Registers.RegisterFile.t
  val rfile8 : (Int32.t * bool) ExecGen.Registers.RegisterFile.t
  val rfile9 : (Int32.t * bool) ExecGen.Registers.RegisterFile.t
  val rfile10 : (Int32.t * bool) ExecGen.Registers.RegisterFile.t
  val rfile11 : (Int32.t * bool) ExecGen.Registers.RegisterFile.t
  val rfile12 : (Int32.t * bool) ExecGen.Registers.RegisterFile.t
  val rfile13 : (Int32.t * bool) ExecGen.Registers.RegisterFile.t
  val rfile14 : (Int32.t * bool) ExecGen.Registers.RegisterFile.t
  val rfile15 : (Int32.t * bool) ExecGen.Registers.RegisterFile.t
  val rfile16 : (Int32.t * bool) ExecGen.Registers.RegisterFile.t
  val rfile17 : (Int32.t * bool) ExecGen.Registers.RegisterFile.t
  val rfile18 : (Int32.t * bool) ExecGen.Registers.RegisterFile.t
  val rfile19 : (Int32.t * bool) ExecGen.Registers.RegisterFile.t
  val rfile20 : (Int32.t * bool) ExecGen.Registers.RegisterFile.t
  val rfile21 : (Int32.t * bool) ExecGen.Registers.RegisterFile.t
  val rfile22 : (Int32.t * bool) ExecGen.Registers.RegisterFile.t
  val mem : (Int32.t * bool) ExecGen.Memory.Memory.t

  val mem1 :
    (Int32.t * bool) ExecGen.Registers.RegisterFile.t
    * (Int32.t * bool) ExecGen.Memory.Memory.t

  val mem2 :
    (Int32.t * bool) ExecGen.Registers.RegisterFile.t
    * (Int32.t * bool) ExecGen.Memory.Memory.t

  val mem3 :
    (Int32.t * bool) ExecGen.Registers.RegisterFile.t
    * (Int32.t * bool) ExecGen.Memory.Memory.t

  val mem4 :
    (Int32.t * bool) ExecGen.Registers.RegisterFile.t
    * (Int32.t * bool) ExecGen.Memory.Memory.t

  val mem5 :
    (Int32.t * bool) ExecGen.Registers.RegisterFile.t
    * (Int32.t * bool) ExecGen.Memory.Memory.t

  val process_optype_tests : OUnitTest.test list
  val instr_list : string list

  val tuple_list :
    ((Int32.t * bool) ExecGen.Registers.RegisterFile.t
    * (Int32.t * bool) ExecGen.Memory.Memory.t)
    list

  val reg_list : int -> (Int32.t * bool) ExecGen.Registers.RegisterFile.t
  val mem_list : int -> (Int32.t * bool) ExecGen.Memory.Memory.t
  val process_file_insns_tests : OUnitTest.test list
  val process_step_insns_tests : OUnitTest.test list
end

val suite : OUnitTest.test
