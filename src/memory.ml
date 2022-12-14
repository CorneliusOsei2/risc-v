open Utilities
open Int32

module MemoryKey = struct
  type t = int

  let compare addr1 addr2 = addr1 - addr2
end

module Memory = Map.Make (MemoryKey)

let init () =
  let open Memory in
  let empty_rom = empty in
  let rec helper rom n =
    if n = 32 then rom else helper (add n (of_int 0, false) rom) (n + 1)
  in
  helper empty_rom 0

let get_memory addr ram =
  let open Memory in
  try fst (find addr ram) with Not_found -> failwith "Invalid memory address"

let visited_memory mem = Memory.bindings mem

let pp_memory memory =
  let memory = visited_memory memory in
  if memory = [] then
    print_endline "No change to memory. All addresses set to 0"
  else
    print_endline
      (fill_string_rev 10 ' ' "Address"
      ^ "| "
      ^ fill_string_rev 10 ' ' " Decimal"
      ^ "| "
      ^ fill_string_rev 40 ' ' "Binary"
      ^ "| "
      ^ fill_string_rev 15 ' ' " Hexadecimal");
  let rec print mem =
    let open Int32 in
    match mem with
    | [] -> ()
    | (m1, v1) :: (m2, v2) :: (m3, v3) :: (m4, v4) :: t ->
        let dec =
          to_int
            (add
               (add (fst v1) (shift_left (fst v2) 8))
               (add (shift_left (fst v3) 16) (shift_left (fst v4) 24)))
        in
        let bin = dec_to_bin dec in
        let hex = dec_to_hex dec in
        print_endline
          (fill_string_rev 10 ' ' (string_of_int m1)
          ^ "| "
          ^ fill_string_rev 10 ' ' (string_of_int dec)
          ^ "| " ^ fill_string_rev 40 ' ' bin ^ "| "
          ^ fill_string_rev 15 ' ' hex);
        print t
    | _ -> ()
  in
  print memory

let update_memory addr v memory =
  let open Memory in
  add addr (v, true) memory
