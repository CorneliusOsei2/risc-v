open Processor
open Registers
open Memory
open Utilities
open IO
open ProcessInstructions

let data_dir_prefix = "data" ^ Filename.dir_sep
let ansi_print color = ANSITerminal.print_string [ color ]

let rec eval_pattern f =
  ansi_print ANSITerminal.yellow ">> ";
  match read_line () with
  | exception End_of_file -> ()
  | pat -> (
      match String.trim pat with
      | "run all" | "r" -> process_input_insns (file_to_list f)
      | "step" | "s" -> ()
      | "q" | "quit" -> print_string "Hope you had fun! 😃 Bye! 👋👋🏽\n"
      | _ ->
          ansi_print ANSITerminal.red "Invalid option. Please try again: \n";
          eval_pattern f)

let eval_pattern_inpt f =
  ansi_print ANSITerminal.green "\n .....file successfully loaded!\n";
  ansi_print ANSITerminal.blue
    "\n\
     How will you like to visualize the execution? \n\
     You can hit [r] in the process of stepping to evaluate all!\n";
  print_endline "\t step (s) or run all (r)?";
  eval_pattern f

let process f = eval_pattern_inpt f

let main () =
  ansi_print ANSITerminal.red "\n\nWelcome to The RISC-V Processor.\n";
  ansi_print ANSITerminal.yellow
    "You can quit at any time with [q] or [quit]\n\n";
  ansi_print ANSITerminal.blue
    "Please enter the name of the test file you want to load.\n";
  ansi_print ANSITerminal.yellow ">> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> process (data_dir_prefix ^ file_name ^ ".txt")

let () = main ()
