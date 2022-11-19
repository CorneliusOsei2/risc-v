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
    if n = 36 then rom else helper (add n (Int32.of_int 0, false) rom) (n + 4)
  in
  helper empty_rom 0

let get_memory addr ram =
  let open Memory in
  try fst (find addr ram) with Not_found -> failwith "Invalid memory address"

let visited_memory mem = List.filter (fun (k, v) -> snd v = true) mem

let pp_memory memory =
  let memory = visited_memory (Memory.bindings memory) in
  if memory = [] then
    print_endline "No change to memory. All addresses set to 0"
  else print_endline "Address |  Decimal |  Binary |  Hexadecimal ";
  print_endline "--------------------------------------------";
  let rec print mem =
    let open Int32 in
    match mem with
    | [] -> ()
    | (r, v) :: t ->
        let dec = Int32.to_int (fst v) in
        let bin = dec_to_bin dec in
        let hex = dec_to_hex dec in
        print_endline
          (string_of_int r ^ "\t | " ^ string_of_int dec ^ "\t | " ^ bin
         ^ "\t | " ^ hex);
        print t
  in
  print memory

let update_memory addr v memory =
  let open Memory in
  add addr (Int32.of_int v, true) memory
