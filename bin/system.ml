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

(** [eval_step n output] handles the execution pattern: either step-wise or a run-all *)
let rec eval_pattern n output =
  if n > List.length output then (ansi_print [ANSITerminal.green] "...... returning you to the Main Menu\n\n"; main())
  else ansi_print [ ANSITerminal.yellow ] ">> ";
  match read_line () with
  | exception End_of_file -> ()
  | pat -> (
      match String.trim pat with
      | "run" | "r" ->
          ansi_print [ ANSITerminal.green ] "\nRegister File\n";
          pp_registers (List.nth (List.map (fun out -> fst out) output) 0);
          ansi_print [ ANSITerminal.green ] "\nMemory\n";
          Memory.pp_memory (List.nth (List.map (fun out -> snd out) output) 0);
          ansi_print [ ANSITerminal.green ] "All instructions executed.\n...... returning you to the Main Menu\n\n";
          main()
      | "step" | "s" ->
          let output_rev = output |> List.rev in
          eval_pattern (eval_step n output_rev) output
      | "q" | "quit" ->
          ansi_print [ ANSITerminal.green ] "Hope you had fun! 😃 Bye! 👋👋🏽\n"
      | "m" | "menu" -> main ()
      | _ ->
          ansi_print [ ANSITerminal.red ] "Invalid option. Please try again: \n";
          eval_pattern n output)

(** [eval_step n output] prints out the current state of 
    the register and memory at index [n] in [output]. *)
and eval_step n output =
  if n < List.length output then (
    ansi_print [ ANSITerminal.green ] "\nRegister File\n";
    pp_registers (List.nth (List.map (fun out -> fst out) output) n);
    ansi_print [ ANSITerminal.green ] "\nMemory\n";
    Memory.pp_memory (List.nth (List.map (fun out -> snd out) output) n);
    n + 1)
  else (
    ansi_print [ ANSITerminal.green ] "All instructions executed \n";
    ansi_print [ ANSITerminal.red ] "PROMPT ";
    ansi_print [ ANSITerminal.yellow ]
      "\tPress [m] to return to main menu or any other key to quit\n";
    ansi_print [ ANSITerminal.blue ] ">> ";
    match read_line () with
    | "m" ->
        main ();
        n + 1
    | _ -> n + 1)

(** [eval_insn_file_format insns] evaluates all instructions in an uploaded file  *)
and eval_insn_file_format insns =
  try
    let output = process_file_insns insns in
    ansi_print [ ANSITerminal.green ] "\n .....file successfully loaded!\n\n";
    ansi_print [ ANSITerminal.red ] "PROMPT";
    ansi_print [ ANSITerminal.blue ]
      "\tHow will you like to visualize the execution? \n\
       \tYou can hit [r] in the process of stepping to evaluate all!\n";
    ansi_print [ ANSITerminal.yellow ] "\tstep (s) or run all (r)?\n";
    eval_pattern 0 output
  with WrongFormat i ->
    ansi_print [ ANSITerminal.blue ]
      ("\tInstruction " ^ string_of_int i ^ " is invalid\n");
    ansi_print [ ANSITerminal.yellow ]
      "\tPlease choose a test file with valid instructions \n";
    eval_insn_file_format (get_insns_from_file ())

(** [get_insns_from_file ()] requests the name of the test file *)
and get_insns_from_file () =
  ansi_print [ ANSITerminal.yellow ] ">> ";
  match read_line () with
  | (exception End_of_file) | "q" | "quit" ->
      ansi_print [ ANSITerminal.green ] "Hope you had fun! 😃 Bye! 👋👋🏽\n";
      exit 0
  | "m" | "menu" ->
      main ();
      []
  | f -> (
      try data_dir_prefix ^ f ^ ".txt" |> file_to_list
      with FileDoesNotExist ->
        ansi_print [ ANSITerminal.red ] "ALERT";
        ansi_print [ ANSITerminal.blue ]
          ("\tNo such file '" ^ f ^ ".txt' in the data/ directory.\n");
        ansi_print [ ANSITerminal.yellow ]
          "\tPlease enter valid file name or [q] to quit\n";
        get_insns_from_file ())

and eval_insn_step_format (rfile, mem) =
  ansi_print [ ANSITerminal.yellow ] ">> ";
  match read_line () with
  | exception End_of_file -> ()
  | f -> (
      match f with
      | "q" | "quit" -> exit 0
      | "m" | "menu" -> main ()
      | f -> (
          try
            let output = process_step_insns f rfile mem in
            ignore (eval_step 0 [ output ]);
            eval_insn_step_format (fst output, snd output)
          with _ ->
            ansi_print [ ANSITerminal.red ] "ALERT";
            ansi_print [ ANSITerminal.yellow ] "Enter valid instruction \n";
            eval_insn_step_format (rfile, mem)))

and gen_specific_insns_handler () = ansi_print [ ANSITerminal.blue ]
  "\tPlease choose the operations [corresponding numbers] you want instructions generated for.\n\tYou can choose multiple operations by separating their numbers with a comma.\n\tRegisters will be initialized with [addi] instructions first\n";
  ansi_print [ANSITerminal.yellow] "\n\tI-Type:\n\t1. addi\t 2. andi\t 3. ori\n\t4. xori\t 5. slli\t 5. srli\n\n\tR-Type:\n\t1. add\t 2. and\t 3. or\n\t4. xor\t 5. sll\t 5. srl\n\n";           
match read_line () with
   | exception End_of_file -> ()
   | f -> (try let ops = f |> String.trim in gen_specific_insns [] with _ -> gen_specific_insns_handler ());gen_insns ()




and gen_insns_handler () =  ansi_print [ ANSITerminal.red ] "PROMPT";
ansi_print [ ANSITerminal.blue ]
  "\tDo you have specific instructions you want to generate?\n";
  ansi_print [ ANSITerminal.yellow ] "\tYou can hit [y] or [yes] to choose specific instructions or [n] or [no] to generate for all currently supported instructions\n";
   ansi_print [ ANSITerminal.blue ] ">> ";
   match read_line () with
   | exception End_of_file -> ()
   | f -> (
       match String.trim f with
       | "n" | "no" -> gen_insns_handler (); 
                  ansi_print [ ANSITerminal.green ] "\t..... instructions successfully generated in data/instructions.txt.\n\tReturning to main menu\n\n"; 
                  main()
       | "y" | "yes" -> 
        gen_specific_insns_handler ()
       | "m" | "menu" -> main ()
       | _ -> (ansi_print [ ANSITerminal.red ] "ALERT";
       ansi_print [ ANSITerminal.yellow ] "\tPlease enter valid command \n"; gen_insns_handler ()))

and process f =
  try
    match f with
    | "1" ->
        ansi_print [ ANSITerminal.red ] "PROMPT";
        ansi_print [ ANSITerminal.blue ] "\tEnter the name of test file\n";
        get_insns_from_file () |> eval_insn_file_format
    | "2" ->
        ansi_print [ ANSITerminal.red ] "PROMPT";
        ansi_print [ ANSITerminal.blue ]
          "\tEnter the instruction. Hit the Return Key when done\n";
        eval_insn_step_format (Registers.init, Memory.init)
    | "3" -> gen_insns_handler ()
    | "m" | "menu" -> main()
    | _ -> ()
  with NotWordAligned ->
    ansi_print [ ANSITerminal.red ]
      "Your [sw] instructions are not word-aliged. Returning you to Main Menu\n\n";
    main ()

and main () =
  ansi_print [ ANSITerminal.red ] "MENU";
  ansi_print [ ANSITerminal.blue ] "\tWhat would you like to do? \n";
  ansi_print [ ANSITerminal.yellow ]
    "\t1. Execute test file. \n\
     \t2. Run instructions step by step. \n\
     \t3. Auto-generate instructions.\n";
  ansi_print [ ANSITerminal.blue ] ">> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> process file_name

let () =
  ansi_print
    [ ANSITerminal.red; ANSITerminal.Background White ]
    "\n\n Welcome to The RISC-V Tests Executer and Generator. \n";
  ansi_print [ ANSITerminal.yellow ]
    "You can quit at any time with [q] or [quit]\n\n";
  main ()
