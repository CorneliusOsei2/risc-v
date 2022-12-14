val fill_string : int -> char -> string -> string
(** [fill_string n c v] is v with [n - len(v)] [c] characters padded to the front.*)

val fill_string_rev : int -> char -> string -> string
(** [fill_string_rev n c v] is v with [n - len(v)] [c] characters padded to the back.*)

val pow : int -> int -> int
(** [pow a n] is [a] exponent [n]. *)

val dec_to_bin : int -> string
(** [dec_to_bin num] is the binary string representation of [num]. *)

val dec_to_hex : int -> string
(** [dec_to_hex num] is the hexadecimal string representation of [num]. *)

val gen_imm : Int32.t -> Int32.t -> Int32.t
(** [gen_imm n hi lo] is a random integer between [lo] and [hi] inclusive. *)

val split_instruction : string -> string * string list
(** [split_instruction ins] splits an R, I and U-Type instruction [ins] into 
    a tuple of the [opcode] and [registers]. *)

val split_stype : string -> string * string list
(** [split_stype ins] splits an S-Type instruction [ins] into a tuple of
    the [opcode] and [registers]. *)

val string_of_list : string list -> string
(** [string_of_list l] is the string representation of [l].*)

val string_of_insn : string * string list -> string
(** [string_of_insn (op, args)] is the string representation of an instruction 
    in the form [(op, args)].*)

val list_of_string : string -> string list
(** [list_of_string s] is the list representation of a string [s].*)

val register_check : string -> bool
(** [register_check r] returns true if [r] is a valid register number. 
    Valid register numbers start with [x] followed by digits [0-31].  *)