exception FileDoesNotExist
(** Raised when an invalid or non-existent file is encoutered. *)

val file_to_list : string -> string list
(** [file_to_list filename] parses the content of [filename].txt into 
   a list.*)

val list_to_file : string list -> unit
(** [list_to_file insns] writes the instructions in [insns] to [data/instructions.txt] *)