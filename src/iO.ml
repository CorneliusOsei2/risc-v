exception FileDoesNotExist

let test_file = "data" ^ Filename.dir_sep ^ "instructions.txt"

let file_to_list file =
  try
    let ic = open_in file in
    let rec loop acc =
      match try Some (input_line ic) with End_of_file -> None with
      | Some a -> if a <> "" then loop (a :: acc) else loop acc
      | None ->
          close_in ic;
          List.rev acc
    in
    loop []
  with _ -> raise FileDoesNotExist

(* TODO:  Write spec *)
let rec print_lst oc = function
  | [] -> ()
  | h :: t ->
      Printf.fprintf oc "%s\n" h;
      print_lst oc t

let list_to_file lst =
  let oc = open_out test_file in
  print_lst oc lst;
  close_out oc
