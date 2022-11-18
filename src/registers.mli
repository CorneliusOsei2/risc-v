module RegisterFile : Map.S
(* [RegisterFile] simulates a CPU's 32-bit register file using a TreeMap *)

val init : (Stdint.Int32.t * bool) RegisterFile.t 
(** [init] returns a 32-bit register file with all 32 registers initialized to 0*)

val pp_registers : (Stdint.Int32.t * bool) RegisterFile.t  -> unit
(** [pp_registers rfile] pretty prints the register file [rfile]*)

val prep_registers :
  int -> (Stdint.Int32.t * bool) RegisterFile.t  -> (Stdint.Int32.t * bool) RegisterFile.t 
(** [prep_registers n rfile] initializes the [n] registers of an input register file 
[rfile] to random integers and returns a new register file with those integers.contents
  The registers are chosen in a consecutive order.
*)

val gen_register : int -> string
(** [gen_register n] a random register identifier x[n] *)

val gen_imm : Stdint.Int32.t -> Stdint.Int32.t  -> Stdint.Int32.t 
(** [gen_imm n hi lo] returns a random int between [lo] and [hi] inclusive *)

val get_register : string -> (Stdint.Int32.t * bool) RegisterFile.t  -> Stdint.Int32.t 
(** [get_register r rfile] returns the first of the value stored at [r] in register 
    [rfile] *)

val update_register : string -> int -> (Stdint.Int32.t * bool) RegisterFile.t -> (Stdint.Int32.t * bool) RegisterFile.t
(** [update_register r rfile] returns the data stored by the register [r] *)
