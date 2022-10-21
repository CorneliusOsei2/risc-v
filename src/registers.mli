module RegisterFile : Map.S
(** [RegisterFile] simulates a CPU's 32-bit register file using a TreeMap *)

val register_init : (int * bool) RegisterFile.t
(** [init] returns a 32-bit register file with all 32 registers initialized to 0*)

val pp_registers : (int * bool) RegisterFile.t -> unit
(** [pp_registers rfile] pretty prints the register file [rfile]*)

val prep_registers :
  int -> (int * bool) RegisterFile.t -> (int * bool) RegisterFile.t
(** [prep_registers n rfile] initializes the [n] registers of an input register file [rfile] to random 
integers and returns a new register file with those integers.contents
  The registers are chosen in a consecutive order.
*)

val gen_register : int -> string
val gen_imm : int -> int -> int
val get_register : string -> 'a RegisterFile.t -> 'a
val update_register : string -> 'a -> 'a RegisterFile.t -> 'a RegisterFile.t
val reset_register : string -> int RegisterFile.t -> int RegisterFile.t

val create_tests :
  int -> (int * bool) RegisterFile.t -> (int * bool) RegisterFile.t
