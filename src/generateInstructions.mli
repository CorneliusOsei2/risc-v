val gen_specific_insns : string list -> unit
val gen_insns : unit -> unit

val gen_rtype : string -> int -> unit
(** [gen_rtype op n acc] generates 5 random R-type instructions into [acc].
  [n] tracks the number of instructions generated    
*)

val gen_itype : string -> int -> unit
(** [gen_itype op n acc] generates 5 random I-type instructions into [acc].
  [n] tracks the number of instructions generated    
*)
