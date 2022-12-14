val gen_specific_insns : string list -> unit
(** [gen_specific_insns ops] generates n R-type instructions.*)

val gen_insns : unit -> unit
(** [gen_insns ()] generates instructions for all supported RISC-V instructions.*)

val gen_rtype : string -> int -> unit
(** [gen_rtype op n acc] generates n R-type instructions.*)

val gen_itype : string -> int -> unit
(** [gen_itype op n acc] generates n I-type (op) instructions.*)

val gen_utype : string -> int -> unit
(** [gen_itype op n acc] generates n U-type (op) instructions.*)

val gen_swtype : string -> int -> unit
(** [gen_swtype op n acc] generates n U-type (op) instructions.*)
