open Processor
open Registers
open Memory
open Utilities
open IO
open ProcessInstructions

exception NoSuchFile of string

let data_dir_prefix = "data" ^ Filename.dir_sep
let ansi_print style = ANSITerminal.print_string style

let rec get_insns () =
  ansi_print [ ANSITerminal.yellow ] ">> ";
  match read_line () with
  | (exception End_of_file) | "q" | "quit" ->
      ansi_print [ ANSITerminal.green ] "Hope you had fun! 😃 Bye! 👋👋🏽\n";
      exit 0
  | f -> (
      try data_dir_prefix ^ f ^ ".txt" |> file_to_list
      with _ ->
        ansi_print [ ANSITerminal.red ]
          ("No such file '" ^ f ^ ".txt' in the data/ directory.\n");
        ansi_print [ ANSITerminal.yellow ]
          "Please enter valid file name or [q] to quit\n";
        get_insns ())

(** [eval_step n output] prints out the current state of 
    the register at index [n] in [output] *)
let eval_step n output =
  if n < List.length output then (
    pp_registers (List.nth output n);
    n + 1)
  else (
    ansi_print [ ANSITerminal.green ] "All instructions executed \n";
    n + 1)

(** [eval_pattern n output] prints out the current state of 
    the register depending on the user input pattern. 
    If pattern is [s] or [step] the state at index [n] in [output] 
    is printed. 
    If pattern is [r] or [run all] the most current state is printed*)
let rec eval_pattern n output =
  if n > List.length output then exit 0
  else ansi_print [ ANSITerminal.yellow ] ">> ";
  match read_line () with
  | exception End_of_file -> ()
  | pat -> (
      match String.trim pat with
      | "run all" | "r" ->
          pp_registers (List.nth output 0);
          ansi_print [ ANSITerminal.green ] "All instructions executed \n";
          exit 0
      | "step" | "s" ->
          let output_rev = output |> List.rev in
          eval_pattern (eval_step n output_rev) output
      | "q" | "quit" ->
          ansi_print [ ANSITerminal.green ] "Hope you had fun! 😃 Bye! 👋👋🏽\n"
      | _ ->
          ansi_print [ ANSITerminal.red ] "Invalid option. Please try again: \n";
          eval_pattern n output)

(** [eval_pattern_inpt f] executes the instructions in test file [f] and triggers
    [eval_pattern]  *)
let eval_pattern_inpt insns =
  let output = process_input_insns insns in
  ansi_print [ ANSITerminal.green ] "\n .....file successfully loaded!\n";
  ansi_print [ ANSITerminal.red ]
    "\n\
     How will you like to visualize the execution? \n\
     You can hit [r] in the process of stepping to evaluate all!\n";
  ansi_print [ ANSITerminal.blue ] "\t step (s) or run all (r)?\n";
  eval_pattern 0 output

let rec eval_insn_step_format rfile =
  ansi_print [ ANSITerminal.yellow ] ">> ";
  match read_line () with
  | exception End_of_file -> ()
  | f -> (
      match f with
      | "q" -> exit 0
      | f -> (
          try
            let output = process_step_instructions f ~rfile in
            ignore (eval_step 0 [ output ]);
            eval_insn_step_format output
          with _ ->
            ansi_print [ ANSITerminal.yellow ] "Enter valid instruction \n";
            eval_insn_step_format rfile))

(** [process f] initiates the processor system with test file [t] *)
let process f =
  match f with
  | "1" ->
      ansi_print [ ANSITerminal.red ] "Enter the name of test file\n";
      get_insns () |> eval_pattern_inpt
  | "2" ->
      ansi_print [ ANSITerminal.red ]
        "Enter the instruction. Hit the Return Key when done\n";
      eval_insn_step_format register_init
  | "3" -> ()
  | _ -> ()

let main () =
  ansi_print
    [ ANSITerminal.red; ANSITerminal.Background White ]
    "\n\n Welcome to The RISC-V Tests Executer and Generator. \n";
  ansi_print [ ANSITerminal.yellow ]
    "You can quit at any time with [q] or [quit]\n\n";
  ansi_print [ ANSITerminal.red ] "What would you like to do? \n";
  ansi_print [ ANSITerminal.blue ]
    " 1. Execute test file. \n\
    \ 2. Run instructions step by step. \n\
    \ 3. Auto-generate instructions.\n";
  ansi_print [ ANSITerminal.yellow ] ">> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> process file_name

let () = main ()
