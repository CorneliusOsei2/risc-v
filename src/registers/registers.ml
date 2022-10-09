let register_num r1 = int_of_string (String.sub r1 1 2)

module RegisterKey = struct
  type t = string

  let compare r1 r2 = register_num r1 - register_num r2
end

module RegisterFile = Map.Make (RegisterKey)

let init =
  let open RegisterFile in
  empty |> add "x00" 0 |> add "x01" 0 |> add "x02" 0 |> add "x03" 0
  |> add "x04" 0 |> add "x05" 0 |> add "x06" 0 |> add "x07" 0 |> add "x08" 0
  |> add "x09" 0 |> add "x10" 0 |> add "x11" 0 |> add "x12" 0 |> add "x13" 0
  |> add "x14" 0 |> add "x15" 0 |> add "x15" 0 |> add "x16" 0 |> add "x17" 0
  |> add "x18" 0 |> add "x19" 0 |> add "x20" 0 |> add "x21" 0 |> add "x22" 0
  |> add "x23" 0 |> add "x24" 0

let pp_registers register_file =
  let open RegisterFile in
  let registers = bindings register_file in
  let rec print rs =
    match rs with
    | [] -> ()
    | (r, v) :: t ->
        print_endline (r ^ " | " ^ string_of_int v);
        print t
  in
  print registers
