val fill_string_rev : int -> char -> string -> string
(** [fill_string_rev n c v] is v with [n - len(v)] [c] characters padded to the back.*)

val fill_string : int -> char -> string -> string
(** [fill_string n c v] is v with [n - len(v)] [c] characters padded to the front.*)

val pow : int -> int -> int
(** [pow a n] takes in an integer base [a] and an integer exponent [n] and calculates the 
    mathematical value of the base to the exponent. *)

val dec_to_bin : int -> string
(** [dec_to_bin num] takes in an integer [num] in decimal form and converts it to its
    equivalent binary representation as a string. For example, if 10 is passed in, the
      function will return the string 1010. *)

val dec_to_hex : int -> string
(** [dec_to_hex num] takes in an integer [num] in decimal form and converts it to its
    equivalent hexademical representation as a string.*)

val split_instruction : string -> string * string list
(** [split_instruction instruct] takes in an instruction [instruct] and separates the 
    op code and the registers to return a tuple of the (op code, registers). *)

val split_stype : string -> string * string list
(** [split_stype instruct] takes in an instruction [instruct] and separates the 
    op code and the registers to return a tuple of the (op code, registers + offset). *)

val string_of_list : string list -> string
(** [string_of_list l] is the string representation of list [l].*)

val string_of_insn : string * string list -> string
(** [string_of_insn (op, args)] is the string representation of instruction [(op, args)].*)

val list_of_string : string -> string list
(** [list_of_string s] is the list representation of a string [s].*)

val register_check : string -> bool
(** [register_check r] returns true if [r] is a valid register number. 
    Valid register numbers start with [x] followed by digits [0-31].  *)