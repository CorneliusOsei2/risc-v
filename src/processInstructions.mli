val rtype : string list
(* Suported R-type instruction operators *)

val itype : string list
(* Suported I-type instruction operators *)

val stype : string list
(* Suported S-type instruction operators *)

val utype : string list
(* Suported U-type instruction operators *)

val min_i : int32
val max_i : int32

exception WrongFormat of int
exception NotWordAligned
exception IncorrectRTypeFormat of int
exception IncorrectITypeFormat of int
exception IncorrectUTypeFormat of int
exception IncorrectSTypeFormat of int

val process_rtype :
  string ->
  string ->
  string ->
  string ->
  (Int32.t * bool) Registers.RegisterFile.t ->
  (Int32.t * bool) Registers.RegisterFile.t
(** [process_rtype op rd rs1 rs2 rfile] processes the R-format instruction
     with [rs1] and [rs2] as source registers, [rd] as the destination register
     [op] as the operator and [rfile] as the register file *)

val process_itype :
  string ->
  string ->
  string ->
  string ->
  (Int32.t * bool) Registers.RegisterFile.t ->
  (Int32.t * bool) Registers.RegisterFile.t
(** [process_itype op rd rs1 imm rfile] processes the I-format instruction
     with [rs1] and [rs2] as source registers, [rd] as the destination register
     [op] as the operator and [rfile] as the register file *)

val process_utype :
  string ->
  string ->
  (Int32.t * bool) Registers.RegisterFile.t ->
  (Int32.t * bool) Registers.RegisterFile.t
(** [process_utype op rd rs1 rs2 rfile] processes the U-format instruction
     with [rs1] and [rs2] as source registers, [rd] as the destination register
     [op] as the operator and [rfile] as the register file *)

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
     [op] as the operator and [rfile] as the register file *)

val process_file_insns :
  string list ->
  ((Int32.t * bool) Registers.RegisterFile.t * (Int32.t * bool) Memory.Memory.t)
  list
(** [process_rtype insns] processes input instructions from the test file and returns
  and returns the states of the register after each instruction execution
*)

val process_step_insns :
  string ->
  (Int32.t * bool) Registers.RegisterFile.t ->
  (Int32.t * bool) Memory.Memory.t ->
  (Int32.t * bool) Registers.RegisterFile.t * (Int32.t * bool) Memory.Memory.t
