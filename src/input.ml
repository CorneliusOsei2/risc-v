let file_to_list file =
  let list = ref [] in
  let ic = open_in file in
  try
    while true do
      let input = input_line ic in
      list := input :: !list
    done;
    !list
  with End_of_file ->
    close_in ic;
    List.rev !list

let rec print_lst oc = function
  | [] -> ()
  | h :: t ->
      Printf.fprintf oc "%s\n" h;
      print_lst oc t

let list_to_file lst =
  let oc = open_out "instructions.txt" in
  print_lst oc lst;
  close_out oc
