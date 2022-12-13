val pp_string : int -> char -> string -> string
(** [pp_string n] prints n padded with enough spaces for good output formatting *)

val pow : int -> int -> int
(** [pow] takes in an integer base and an integer exponent and calculates the 
    mathematical value of the base to the exponent. *)

val dec_to_bin : int -> string
(** [dec_to_bin] takes in an integer in decimal form and converts it to its
    equivalent binary representation as a string. For example, if 10 is passed in, the
      function will return the string 1010*)

val dec_to_hex : int -> string
(** [dec_to_hex] takes in an integer in decimal form and converts it to its
    equivalent hexademical representation as a string.*)

val split_instruction : string -> string * string list
(** [split_instruction] takes in an instruction and separates the 
    op code and the registers to return a tuple of the (op code, registers) *)

val split_stype : string -> string * string list
(** [split_stype] takes in an instruction and separates the 
    op code and the registers to return a tuple of the (op code, registers + offset) *)

val string_of_list : string list -> string
(** [string_of_list ] converts the string representation of list [l]*)

val string_of_insn : string * string list -> string
(** [pp_instriction l] prints the string representation of instruction [l]*)

val list_of_string : string -> string list
(** [ist_of_string s] converts the string representation to a list [l]*)

val gen_ops : unit -> string

val register_check : string -> bool
(** [register_check s] returns true if s is a valid register number. 
    Valid register numbers start with [x] followed by digits [0-31]  *)