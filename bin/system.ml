open Processor
open Registers
open Memory
open Utilities
open IO
open ProcessInstructions

let data_dir_prefix = "data" ^ Filename.dir_sep
let ansi_print color = ANSITerminal.print_string [ color ]

let eval_pattern pat f =
  match String.trim pat with
  | "run all" | "r" -> process_input_insns (file_to_list f)
  | "step" -> ()
  | _ -> ()

let eval_pattern_inpt f =
  ansi_print ANSITerminal.green "\n File successfully loaded!\n";
  ansi_print ANSITerminal.blue
    "\n\
     How will you like to visualize the execution? \n\
    \ step (s) or run all (r)? \n";
  ansi_print ANSITerminal.yellow ">> ";
  match read_line () with
  | exception End_of_file -> ()
  | pattern -> eval_pattern pattern f

let process f = eval_pattern_inpt f

let main () =
  ansi_print ANSITerminal.red "\n\nWelcome to The RISC-V Processor.\n";
  ansi_print ANSITerminal.blue
    "Please enter the name of the test file you want to load.\n";
  ansi_print ANSITerminal.yellow ">> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> process (data_dir_prefix ^ file_name ^ ".txt")

let () = main ()
