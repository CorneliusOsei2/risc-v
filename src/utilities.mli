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

val pp_instruction : string * string list -> string
(** [pp_instruction] pretty prints instruction. *)
