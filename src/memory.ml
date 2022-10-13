module MemoryKey = struct
  type t = int

  let compare addr1 addr2 = addr1 - addr2
end

module Memory = Map.Make (MemoryKey)

let init =
  let open Memory in
  let empty_rom = empty in
  let rec helper rom n =
    if n = 36 then rom else helper (add n (0, false) rom) (n + 4)
  in
  helper empty_rom 0

let get_mem addr ram =
  let open Memory in
  try find addr ram with Not_found -> failwith "Invalid memory address"

let pp_memory memory =
  print_endline "Address |  Decimal |  Binary |  Hexadecimal ";
  print_endline "--------------------------------------------";
  let rec print mem =
    match mem with
    | [] -> ()
    | (r, v) :: t ->
        let dec = fst v in
        let bin = dec in
        let hex = dec in
        print_endline (string_of_int r ^ "\t | " ^ string_of_int dec);
        print t
  in
  print memory

let _ = init |> Memory.bindings |> pp_memory