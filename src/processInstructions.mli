(** Implements functions to execute supported RISC-V instructions. *)

val rtype : string list
(** Suported R-type instruction instructions: add, and, or, xor, sll, srl, slt. *)

val itype : string list
(** Suported I-type instruction instructions: addi, andi, ori, xori, slli, srli, slti. *)

val stype : string list
(** Suported S-type instruction instructions: sw, sb, lw, lb. *)

val utype : string list
(** Suported U-type instruction instructions: lui. *)

exception WrongFormat of int
(** Raised when an invalid or not currently-supported RISC-V instruction is encoutered. 
  It carries the instruction number (particularly useful in a test file). *)

exception NotWordAligned
(** Raised when a memory address involving a word is not word-aligned. *)

exception IncorrectRTypeFormat of int
(** Raised when an invalid or not currently-supported R-Type instruction is encoutered.
  It carries the instruction number (particularly useful in a test file). *)

exception IncorrectITypeFormat of int
(** Raised when an invalid or not currently-supported I-Type instruction is encoutered.
  It carries the instruction number (particularly useful in a test file). *)

exception IncorrectUTypeFormat of int
(** Raised when an invalid or not currently-supported U-Type instruction is encoutered. 
  It carries the instruction number (particularly useful in a test file). *)

exception IncorrectSTypeFormat of int
(** Raised when an invalid or not currently-supported S-Type instruction is encoutered. 
  It carries the instruction number (particularly useful in a test file). *)

val min_i : int32
(** Minimum immediate value (2's complement) for I-Type instruction. *)

val max_i : int32
(** Maximum immediate value (2's complement) for I-Type instruction. *)

val min_u : int32
(** Minimum immediate value (2's complement) for U-Type instruction. *)

val max_u : int32
(** Maximum immediate value (2's complement) for U-Type instruction. *)

val process_rtype :
  string ->
  string ->
  string ->
  string ->
  (Int32.t * bool) Registers.RegisterFile.t ->
  (Int32.t * bool) Registers.RegisterFile.t
(** [process_rtype op rd rs1 rs2 rfile] processes the R-format instruction
     with [rs1] and [rs2] as source registers, [rd] as the destination register
     [op] as the operator and [rfile] as the register file. *)

val process_itype :
  string ->
  string ->
  string ->
  string ->
  (Int32.t * bool) Registers.RegisterFile.t ->
  (Int32.t * bool) Registers.RegisterFile.t
(** [process_itype op rd rs1 imm rfile] processes the I-format instruction
     with [rs1] and [rs2] as source registers, [rd] as the destination register
     [op] as the operator and [rfile] as the register file. *)

val process_utype :
  string ->
  string ->
  (Int32.t * bool) Registers.RegisterFile.t ->
  (Int32.t * bool) Registers.RegisterFile.t
(** [process_utype op rd rs1 rs2 rfile] processes the U-format instruction
     with [rs1] and [rs2] as source registers, [rd] as the destination register
     [op] as the operator and [rfile] as the register file. *)

val process_stype :
  string ->
  string ->
  string ->
  string ->
  (Int32.t * bool) Registers.RegisterFile.t ->
  (Int32.t * bool) Memory.Memory.t ->
  (Int32.t * bool) Registers.RegisterFile.t * (Int32.t * bool) Memory.Memory.t
(** [process_stype op rd rs1 rs2 rfile] processes the S-format instruction
     with [rs1] and [rs2] as source registers, [rd] as the destination register
     [op] as the operator and [rfile] as the register file. *)

val process_file_insns :
  string list ->
  ((Int32.t * bool) Registers.RegisterFile.t * (Int32.t * bool) Memory.Memory.t)
  list
(** [process_file_insns insns] processes [insns] from the test file and
   returns a list of the resulting states of [rfile] and [mem].
*)

val process_step_insns :
  string ->
  (Int32.t * bool) Registers.RegisterFile.t ->
  (Int32.t * bool) Memory.Memory.t ->
  (Int32.t * bool) Registers.RegisterFile.t * (Int32.t * bool) Memory.Memory.t
(** [process_step_insns insn rfile mem] executes [insn] using the states of [rfile] and [mem]
  and returns the resulting states of [rfile] and [mem].
*)