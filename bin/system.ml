open Processor
open Registers
open Utilities
open IO
open ProcessInstructions
open GenerateInstructions

exception NoSuchFile of string

let data_dir_prefix = "data" ^ Filename.dir_sep

(** [ansi_print style s] prints s after applying [style] to it*)
let ansi_print style = ANSITerminal.print_string style

let ansi_print_green s = ansi_print [ ANSITerminal.green ] s
let ansi_print_red s = ansi_print [ ANSITerminal.red ] s
let ansi_print_yellow s = ansi_print [ ANSITerminal.yellow ] s
let ansi_print_blue s = ansi_print [ ANSITerminal.blue ] s
let quit_msg = "Hope you had fun! 😃 Bye! 👋👋🏽\n"
let return_msg = "Returning to the Main Menu\n\n"
let invalid_msg = "\tInvalid option; please try again: \n\n"
let executed_msg = "All instructions executed.\n"
let end_prompt = "\tPress [m] to return to main menu or any other key to quit\n"

(** [eval_step n output] handles the execution pattern: either step-wise or a run-all *)
let rec eval_pattern n output =
  if n > List.length output then (
    ansi_print_green return_msg;
    main ())
  else ansi_print_yellow ">> ";
  match read_line () with
  | exception End_of_file -> ()
  | pat -> (
      match String.trim pat with
      | "run" | "r" -> (
          ansi_print_green "\nRegister File\n";
          pp_registers (List.nth (List.map (fun out -> fst out) output) 0);
          ansi_print_green "\nMemory\n";
          Memory.pp_memory (List.nth (List.map (fun out -> snd out) output) 0);
          ansi_print_green executed_msg;
          ansi_print_red "PROMPT ";
          ansi_print_yellow end_prompt;
          ansi_print [ ANSITerminal.blue ] ">> ";
          match read_line () with
          | "m" -> main ()
          | _ ->
              ansi_print_green quit_msg;
              exit 0)
      | "step" | "s" ->
          let output_rev = output |> List.rev in
          eval_pattern (eval_step n output_rev) output
      | "q" | "quit" -> ansi_print_green quit_msg
      | "m" | "menu" -> main ()
      | _ ->
          ansi_print_red invalid_msg;
          eval_pattern n output)

(** [eval_step n output] prints out the current state of 
    the register and memory at index [n] in [output]. *)
and eval_step n output =
  if n < List.length output then (
    ansi_print_green "\nRegister File\n";
    pp_registers (List.nth (List.map (fun out -> fst out) output) n);
    ansi_print_green "\nMemory\n";
    Memory.pp_memory (List.nth (List.map (fun out -> snd out) output) n);
    n + 1)
  else (
    ansi_print_green executed_msg;
    ansi_print_red "PROMPT ";
    ansi_print_yellow end_prompt;
    ansi_print [ ANSITerminal.blue ] ">> ";
    match read_line () with
    | "m" ->
        main ();
        n + 1
    | _ ->
        ansi_print_green quit_msg;
        exit 0)

(** [eval_insn_file_format insns] evaluates all instructions in an uploaded file  *)
and eval_insn_file_format insns =
  try
    let output = process_file_insns insns in
    ansi_print_green "\n .....file successfully loaded!\n\n";
    ansi_print_red "PROMPT";
    ansi_print_blue
      "\tHow will you like to visualize the execution? \n\
       \tYou can hit [r] in the process of stepping to evaluate all!\n";
    ansi_print_yellow "\tstep (s) or run all (r)?\n";
    eval_pattern 0 output
  with WrongFormat i ->
    ansi_print_blue ("\tInstruction " ^ string_of_int i ^ " is invalid\n");
    ansi_print_yellow "\tPlease choose a test file with valid instructions \n";
    eval_insn_file_format (get_insns_from_file ())

(** [get_insns_from_file ()] requests the name of the test file *)
and get_insns_from_file () =
  ansi_print_yellow ">> ";
  match read_line () with
  | (exception End_of_file) | "q" | "quit" ->
      ansi_print_green quit_msg;
      exit 0
  | "m" | "menu" ->
      main ();
      []
  | f -> (
      try data_dir_prefix ^ f ^ ".txt" |> file_to_list
      with FileDoesNotExist ->
        ansi_print_red "ALERT";
        ansi_print_blue
          ("\tNo such file '" ^ f ^ ".txt' in the data/ directory.\n");
        ansi_print_yellow "\tPlease enter valid file name or [q] to quit\n";
        get_insns_from_file ())

and eval_insn_step_format (rfile, mem) =
  ansi_print_yellow ">> ";
  match read_line () with
  | exception End_of_file -> ()
  | "q" | "quit" -> ansi_print_green quit_msg
  | cmd -> (
      match cmd with
      | "q" | "quit" ->
          ansi_print_green quit_msg;
          exit 0
      | "m" | "menu" -> main ()
      | cmd -> (
          try
            let output = process_step_insns cmd rfile mem in
            ignore (eval_step 0 [ output ]);
            eval_insn_step_format (fst output, snd output)
          with _ ->
            ansi_print_red "ALERT";
            ansi_print_yellow "\tEnter valid instruction \n";
            eval_insn_step_format (rfile, mem)))

and gen_specific_insns_handler () =
  ansi_print_blue
    "\tPlease choose the operations [corresponding numbers] you want \
     instructions generated for.\n\
     \tYou can choose multiple operations by separating their numbers with a \
     comma.\n\
     \tRegisters will be initialized with [addi] instructions first\n";
  ansi_print_yellow (gen_ops ());
  ansi_print_blue ">> ";
  match read_line () with
  | exception End_of_file -> ()
  | f ->
      (try
         let ops = f |> list_of_string in
         gen_specific_insns ops;
         ansi_print_green
           ("\t..... instructions successfully generated in \
             data/instructions.txt.\n\
             \t" ^ return_msg);
         ansi_print_red "PROMPT ";
         ansi_print_yellow end_prompt;
         ansi_print [ ANSITerminal.blue ] ">> ";
         match read_line () with
         | "m" -> main ()
         | _ ->
             ansi_print_green quit_msg;
             exit 0
       with _ -> gen_specific_insns_handler ());
      gen_insns ()

and gen_insns_handler () =
  ansi_print_red "PROMPT";
  ansi_print_blue "\tDo you have specific instructions you want to generate?\n";
  ansi_print_yellow
    "\tYou can hit [y] or [yes] to choose specific instructions or \n\
    \ [n] or [no] to generate for all currently supported instructions\n";
  ansi_print_blue ">> ";
  match read_line () with
  | exception End_of_file -> ()
  | "q" | "quit" ->
      ansi_print_green quit_msg;
      exit 0
  | f -> (
      match String.trim f with
      | "n" | "no" ->
          gen_insns_handler ();
          ansi_print_green
            ("\t..... instructions successfully generated in \
              data/instructions.txt.\n\
              \t" ^ return_msg);
          main ()
      | "y" | "yes" -> gen_specific_insns_handler ()
      | "m" | "menu" -> main ()
      | "q" | "quit" ->
          ansi_print_green quit_msg;
          exit 0
      | _ ->
          ansi_print_red "ALERT";
          ansi_print_yellow "\tPlease enter valid command \n";
          gen_insns_handler ())

and process f =
  try
    match f with
    | "1" ->
        ansi_print_red "PROMPT";
        ansi_print_blue "\tEnter the name of test file\n";
        get_insns_from_file () |> eval_insn_file_format
    | "2" ->
        ansi_print_red "PROMPT";
        ansi_print_blue
          "\tEnter the instruction. Hit the Return Key when done\n";
        eval_insn_step_format (Registers.init, Memory.init)
    | "3" -> gen_insns_handler ()
    | "m" | "menu" -> main ()
    | "q" | "quit" ->
        ansi_print_green quit_msg;
        exit 0
    | _ ->
        ansi_print_red invalid_msg;
        main ()
  with NotWordAligned ->
    ansi_print_red ("Your [sw] instructions are not word-aligned." ^ return_msg);
    main ()

and main () =
  ansi_print_red "MENU";
  ansi_print_blue "\tWhat would you like to do? \n";
  ansi_print_yellow
    "\t1. Execute test file. \n\
     \t2. Run instructions step by step. \n\
     \t3. Auto-generate instructions.\n";
  ansi_print_blue ">> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> process file_name

let () =
  ansi_print
    [ ANSITerminal.red; ANSITerminal.Background White ]
    "\n\n Welcome to The RISC-V Tests Executer and Generator. \n";
  ansi_print_yellow "You can quit at any time with [q] or [quit]\n\n";
  main ()
