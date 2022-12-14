open Utilities
open Random

module StringComp = struct
  type t = string

  let compare r1 r2 =
    let register_num r =
      List.nth (String.split_on_char 'x' r) 1 |> int_of_string
    in
    compare (register_num r1) (register_num r2)
end

module RegisterFile = Map.Make (StringComp)

let init () =
  let open RegisterFile in
  let empty_file = empty in
  let rec helper rom n =
    let r = "x" ^ string_of_int n in
    if n = 33 then rom else helper (add r (0l, false) rom) (n + 1)
  in
  helper empty_file 0

let update_register r v rfile =
  let open RegisterFile in
  add r (Int32.of_int v, true) rfile

let visited_registers rfile =
  List.filter (fun el -> snd (snd el) = true) (RegisterFile.bindings rfile)

let pp_registers registerfile =
  let registers = registerfile |> visited_registers in
  print_endline
    ("Register |"
    ^ fill_string_rev 10 ' ' " Decimal"
    ^ " | "
    ^ fill_string_rev 40 ' ' "Binary"
    ^ "|"
    ^ fill_string_rev 15 ' ' " Hexadecimal");
  let rec print rs =
    match rs with
    | [] -> ()
    | (r, v) :: t ->
        let v = Int32.to_int (fst v) in
        let bin = dec_to_bin v in
        let hex = dec_to_hex v in
        print_endline
          (r ^ "\t | "
          ^ (v |> string_of_int |> fill_string_rev 10 ' ')
          ^ "| " ^ fill_string_rev 40 ' ' bin ^ "| "
          ^ fill_string_rev 15 ' ' hex);
        print t
  in
  print registers

let gen_register n = "x" ^ string_of_int (((n + 1) mod 31) + 1)

let get_register r rfile =
  try
    let open RegisterFile in
    fst (find r rfile)
  with Not_found -> failwith "Invalud register"

let get_rval v = Int32.to_int v