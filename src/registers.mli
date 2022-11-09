module RegisterFile : Map.S
(* [RegisterFile] simulates a CPU's 32-bit register file using a TreeMap *)

val register_init : (int32 * bool) RegisterFile.t
(** [init] returns a 32-bit register file with all 32 registers initialized to 0*)

val pp_registers : (int32 * bool) RegisterFile.t -> unit
(** [pp_registers rfile] pretty prints the register file [rfile]*)

val prep_registers :
  int -> (int32 * bool) RegisterFile.t -> (int32 * bool) RegisterFile.t
(** [prep_registers n rfile] initializes the [n] registers of an input register file 
[rfile] to random integers and returns a new register file with those integers.contents
  The registers are chosen in a consecutive order.
*)

val gen_register : int -> string
(** [gen_register n] a random register identifier x[n] *)

val gen_imm : int32 -> int32 -> int32
(** [gen_imm n hi lo] returns a random int between [lo] and [hi] inclusive *)

val get_register : string -> (int32 * bool) RegisterFile.t -> int32
(** [get_register r rfile] returns the first of the value stored at [r] in register 
    [rfile] *)

val update_register :
  string ->
  int32 ->
  (int32 * bool) RegisterFile.t ->
  (int32 * bool) RegisterFile.t
(** [update_register r rfile] returns the data stored by the register [r] *)
