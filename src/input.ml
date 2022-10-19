let file_to_list file =
  let ic = open_in file in
  let rec loop acc =
    match try Some (input_line ic) with End_of_file -> None with
    | Some a -> loop (a :: acc)
    | None ->
        close_in ic;
        List.rev acc
  in
  loop []

let rec print_lst oc = function
  | [] -> ()
  | h :: t ->
      Printf.fprintf oc "%s\n" h;
      print_lst oc t

let list_to_file lst =
  let oc = open_out "instructions.txt" in
  print_lst oc lst;
  close_out oc
