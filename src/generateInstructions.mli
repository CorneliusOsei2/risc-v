(** Implements functions to generate supported RISC-V instructions. *)

val gen_rtype : string -> int -> unit
(** [gen_rtype op n acc] generates [n] [op] instructions. [op] is an R-type
  instruction.*)

val gen_itype : string -> int -> unit
(** [gen_itype op n acc] generates [n] [op] instructions. [op] is an I-type
  instruction.*)

val gen_utype : string -> int -> unit
(** [gen_itype op n acc] generates [n] [op] instructions. [op] is a U-type
  instruction.*)

val gen_swtype : string -> int -> unit
(** [gen_swtype op n acc] generates [n] [op] instructions. [op] is either [sw] or
  [lw] S-type instruction.*)

val gen_sbtype : string -> int -> unit
(** [gen_swtype op n acc] generates [n] [op] instructions. [op] is either [sb] or
  [lb] S-type instruction.*)

val gen_specific_insns : string list -> unit
(** [gen_specific_insns ops] generates instructions for the operators in [ops].
    [ops] may contain the actual operator name or an integer string that maps to the
    operator.*)

val gen_insns : unit -> unit
(** [gen_insns ()] generates instructions for currently supported RISC-V 
    instructions.*)