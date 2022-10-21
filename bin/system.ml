open Processor
open Registers
open Memory
open Utilities
open IO
open ProcessInstructions

let data_dir_prefix = "data" ^ Filename.dir_sep
let process f = process_input_insns (file_to_list f)

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to The RISC-V Processor.\n";
  print_endline "Please enter the name of the test file you want to load.\n";
  print_string ">> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> process (data_dir_prefix ^ file_name ^ ".txt")

let () = main ()
