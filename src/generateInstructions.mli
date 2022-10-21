val gen_rtype : string -> int -> string list -> string list
(** [gen_rtype op n acc] generates 5 random R-type instructions into [acc].
  [n] tracks the number of instructions generated    
*)

val gen_itype : string -> int -> string list -> string list
(** [gen_itype op n acc] generates 5 random I-type instructions into [acc].
  [n] tracks the number of instructions generated    
*)
