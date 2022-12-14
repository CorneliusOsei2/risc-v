module RegisterFile : Map.S
(** [RegisterFile] simulates a CPU's 32-bit register file using a TreeMap.
    Registers are labeled from x0 to x31.*)

val init : unit -> (Int32.t * bool) RegisterFile.t
(** [init] is a 32-bit register file with all 32 registers initialized to 0.*)

val pp_registers : (Int32.t * bool) RegisterFile.t -> unit
(** [pp_registers rfile] pretty prints the register file [rfile].*)

val gen_register : int -> string
(** [gen_register n] a random register identifier x[n]. *)

val gen_imm : Int32.t -> Int32.t -> Int32.t
(** [gen_imm n hi lo] returns a random int between [lo] and [hi] inclusive. *)

val get_register : string -> (Int32.t * bool) RegisterFile.t -> Int32.t
(** [get_register r rfile] returns the integer value stored at [r] in register 
    [rfile].*)

val update_register :
  string ->
  int ->
  (Int32.t * bool) RegisterFile.t ->
  (Int32.t * bool) RegisterFile.t
(** [update_register r v rfile] sets the integer value of register [r] to be [v].*)
