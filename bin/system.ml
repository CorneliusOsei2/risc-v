open Processor
open Registers
open Memory
open Utilities
open IO
open ProcessInstructions

let data_dir_prefix = "data" ^ Filename.dir_sep
let ansi_print color = ANSITerminal.print_string [ color ]

(** [eval_step ]*)
let eval_step n output =
  if n < List.length output then (
    pp_registers (List.nth output n);
    n + 1)
  else (
    print_string "All instructions executed";
    n + 1)

let rec eval_pattern n output f =
  if n > List.length output then exit 0
  else ansi_print ANSITerminal.yellow ">> ";
  match read_line () with
  | exception End_of_file -> ()
  | pat -> (
      match String.trim pat with
      | "run all" | "r" ->
          pp_registers (List.nth output 0);
          exit 0
      | "step" | "s" ->
          let output_rev = output |> List.rev in
          eval_pattern (eval_step n output_rev) output f
      | "q" | "quit" -> print_string "Hope you had fun! 😃 Bye! 👋👋🏽\n"
      | _ ->
          ansi_print ANSITerminal.red "Invalid option. Please try again: \n";
          eval_pattern n output f)

let eval_pattern_inpt f =
  let output = process_input_insns (file_to_list f) in
  ansi_print ANSITerminal.green "\n .....file successfully loaded!\n";
  ansi_print ANSITerminal.blue
    "\n\
     How will you like to visualize the execution? \n\
     You can hit [r] in the process of stepping to evaluate all!\n";
  print_endline "\t step (s) or run all (r)?";
  eval_pattern 0 output f

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
