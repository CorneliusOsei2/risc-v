open Utilities
open Stdint

module MemoryKey = struct
  type t = int

  let compare addr1 addr2 = addr1 - addr2
end

module Memory = Map.Make (MemoryKey)

let init =
  let open Memory in
  let empty_rom = empty in
  let rec helper rom n =
    if n = 36 then rom else helper (add n (Int8.of_int 0, false) rom) (n + 4)
  in
  helper empty_rom 0

let get_memory addr ram =
  let open Memory in
  try fst (find addr ram) with Not_found -> failwith "Invalid memory address"

let pp_memory memory =
  print_endline "Address |  Decimal |  Binary |  Hexadecimal ";
  print_endline "--------------------------------------------";
  let memory = Memory.bindings memory in
  let rec print mem =
    let open Int8 in
    match mem with
    | [] -> ()
    | (r, v) :: t ->
        let dec = fst v in
        let bin = to_string_bin dec in
        let hex = to_string_hex dec in
        print_endline
          (string_of_int r ^ "\t | " ^ to_string dec ^ "\t | " ^ bin ^ "\t | "
         ^ hex);
        print t
  in
  print memory

let update_memory addr v memory =
  let open Memory in
  add addr (Int8.of_int v, true) memory
