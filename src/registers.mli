module RegisterFile : Map.S
(** [RegisterFile] simulates a CPU's 32-bit register file using a TreeMap *)

val init : (int * bool) RegisterFile.t
(** [init] returns a 32-bit register file with all 32 registers initialized to 0*)

val pp_registers : (string * (int * bool)) list -> unit
(** [pp_registers rfile] pretty prints the register file [rfile]*)

val prep_registers :
  int -> (int * bool) RegisterFile.t -> (int * bool) RegisterFile.t
(** [prep_registers n rfile] initializes the [n] registers of an input register file [rfile] to random 
integers and returns a new register file with those integers.contents
  The registers are chosen in a consecutive order.
*)
