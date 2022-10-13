open Registers

let rtype = [ "add"; "sub"; "and"; "or"; "xor"; "nor"; "sll"; "srl" ]
let itype = [ "addi"; "andi"; "ori"; "xori" ]
let utype = [ "lui" ]
let stype = [ "sw"; "sb"; "lw"; "lb" ]

let process_rtype op rd rs1 rs2 rfile =
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

let rec insns fmts n acc =
  match fmts with
  | [] -> acc
  | h :: t -> gen_rtype h n [] :: acc |> insns t (n + 2)

let ins = insns rtype 2 []
