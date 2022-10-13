let register_num r = List.nth (String.split_on_char 'x' r) 1 |> int_of_string

module RegisterKey = struct
  type t = string

  let compare r1 r2 = register_num r1 - register_num r2
end

module RegisterFile = Map.Make (RegisterKey)

let init =
  let open RegisterFile in
  let empty_file = empty in
  let rec helper rom n =
    let r = "x" ^ string_of_int n in
    if n = 33 then rom else helper (add r (0, false) rom) (n + 1)
  in
  helper empty_file 0

let pp_registers registers =
  print_endline "Register | Decimal | Binary | Hexadecima";
  let rec print rs =
    match rs with
    | [] -> ()
    | (r, v) :: t ->
        print_endline (r ^ "\t | " ^ string_of_int (fst v));
        print t
  in
  print registers

let visited_registers rfile =
  List.filter (fun el -> snd (snd el) = true) (RegisterFile.bindings rfile)

let gen_register n = "x" ^ string_of_int (n + 1)

let rec gen_imm lower_bound upper_bound =
  let i = Random.int upper_bound in
  if i >= lower_bound && i < upper_bound then i
  else gen_imm upper_bound lower_bound

let prep rfile n =
  let rnum = gen_register n in
  let imm = gen_imm ~-2048 2047 in
  let open RegisterFile in
  add rnum (imm, true) rfile

let rec prep_registers n rfile =
  if n < 0 then rfile else prep rfile n |> prep_registers (n - 1)

let get_register r rfile =
  try
    let open RegisterFile in
    find r rfile
  with Not_found -> failwith "Invalid register access"

let update_register r v rfile =
  let open RegisterFile in
  add r v rfile

let reset_register r rfile =
  let open RegisterFile in
  add r 0 rfile

let process_r op rd rs1 rs2 rfile =
  match String.lowercase_ascii op with
  | "add" ->
      update_register rd (get_register rs1 rfile + get_register rs2 rfile) rfile
  | "sub" ->
      update_register rd (get_register rs1 rfile - get_register rs2 rfile) rfile
  | "and" ->
      update_register rd
        (get_register rs1 rfile land get_register rs2 rfile)
        rfile
  | "or" ->
      update_register rd
        (get_register rs1 rfile lor get_register rs2 rfile)
        rfile
  | "xor" ->
      update_register rd
        (get_register rs1 rfile lxor get_register rs2 rfile)
        rfile
  | "nor" ->
      update_register rd
        (get_register rs1 rfile lor get_register rs2 rfile)
        rfile
  | "sll" ->
      update_register rd
        (get_register rs1 rfile lsl get_register rs2 rfile)
        rfile
  | "slr" ->
      update_register rd
        (get_register rs1 rfile lsr get_register rs2 rfile)
        rfile
  | _ -> rfile

let rec gen_rtype op n acc =
  if n mod 3 = 0 then acc
  else
    let rd = gen_register n in
    let rs1 = gen_register (n + 1) in
    let rs2 = gen_register (n + 2) in
    (op ^ " " ^ rd ^ " " ^ rs1 ^ " " ^ rs2) :: acc |> gen_rtype op (n + 1)

let rec gen_itype op n acc =
  if n mod 5 = 0 then acc
  else
    let rd = gen_register n in
    let rs1 = gen_register (n + 1) in
    let imm = string_of_int (Random.int 2047) in
    (op ^ " " ^ rd ^ " " ^ rs1 ^ " " ^ imm) :: acc |> gen_itype op (n + 1)

let _ = gen_itype "andi" 1 []
let rtype = [ "add"; "sub"; "and"; "or"; "xor"; "nor"; "sll"; "srl" ]
let itype = [ "addi"; "andi"; "ori"; "xori" ]
let utype = [ "lui" ]
let stype = [ "sw"; "sb"; "lw"; "lb" ]

let rec insns fmts n acc =
  match fmts with
  | [] -> acc
  | h :: t -> gen_rtype h n [] :: acc |> insns t (n + 2)

let ins = insns rtype 2 []
let create_tests n rfile = prep_registers n rfile

(* let _ = init |> RegisterFile.bindings |> pp_registers *)
let _ = init |> create_tests 31 |> RegisterFile.bindings |> pp_registers
(* let r = init |> create_tests  |> visited_registers |> pp_registers *)
