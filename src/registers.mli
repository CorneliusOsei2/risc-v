(** Implements a RISC-V Processor Register File. *)

module RegisterFile : Map.S
(** [RegisterFile] simulates a CPU's 32-bit register file using a TreeMap.
    Registers are labeled from x0 to x31.*)

val init : unit -> (Int32.t * bool) RegisterFile.t
(** [init] is a 32-bit register file with all 32 registers initialized to 0.*)

val pp_registers : (Int32.t * bool) RegisterFile.t -> unit
(** [pp_registers rfile] pretty prints the register file [rfile].*)

val gen_register : int -> string
(** [gen_register n] a random register identifier x[n]. *)

val get_register : string -> (Int32.t * bool) RegisterFile.t -> Int32.t
(** [get_register r rfile] returns the integer value stored at [r] in register 
    [rfile].*)

val get_rval : Int32.t -> int
(** get_rval[n] is the integer value of [n] stored at register. *)

val update_register :
  string ->
  int ->
  (Int32.t * bool) RegisterFile.t ->
  (Int32.t * bool) RegisterFile.t
(** [update_register r v rfile] sets the integer value of register [r] to be [v].*)
