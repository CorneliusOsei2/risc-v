let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to The RISC-V Processor.\n";
  print_endline "Please enter the name of the test file you want to load.\n";
  print_string ">> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> print_string ""

(* Execute the game engine. *)
let () = main ()
