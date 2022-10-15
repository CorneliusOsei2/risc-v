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
