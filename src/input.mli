val file_to_list : string -> string list
(*[file_to_list] takes a txt file with individual instructions and returns
   a string list with each element being one instruction.contents *)

val list_to_file : string list -> unit
(*[list_to_file] takes a list with each element being an instruction
   and returns a txt file with each line being an instruction *)
