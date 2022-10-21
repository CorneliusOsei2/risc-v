open Registers

let rtype = [ "add"; "sub"; "and"; "or"; "xor"; "nor"; "sll"; "srl" ]
let itype = [ "addi"; "andi"; "ori"; "xori" ]
let utype = [ "lui" ]
let stype = [ "sw"; "sb"; "lw"; "lb" ]

let eval_ins rd rs1 rs2 rfile op =
  update_register rd
    (op (get_register rs1 rfile) (get_register rs2 rfile))
    rfile

let process_rtype op rd rs1 rs2 rfile =
  match String.lowercase_ascii op with
  | "add" -> eval_ins rd rs1 rs2 rfile ( + )
  | "sub" -> eval_ins rd rs1 rs2 rfile ( - )
  | "and" -> eval_ins rd rs1 rs2 rfile ( land )
  | "or" -> eval_ins rd rs1 rs2 rfile ( lor )
  | "xor" -> eval_ins rd rs1 rs2 rfile ( lxor )
  | "nor" -> eval_ins rd rs1 rs2 rfile ( lxor )
  | "sll" -> eval_ins rd rs1 rs2 rfile ( lsl )
  | "slr" -> eval_ins rd rs1 rs2 rfile ( lsr )
  | _ -> rfile

let process_itype op rd rs imm rfile =
  match String.lowercase_ascii op with
  | "addi" -> eval_ins rd rs imm rfile ( + )
  | "subi" -> eval_ins rd rs imm rfile ( - )
  | "andi" -> eval_ins rd rs imm rfile ( land )
  | "ori" -> eval_ins rd rs imm rfile ( lor )
  | "xori" -> eval_ins rd rs imm rfile ( lxor )
  | "nori" -> eval_ins rd rs imm rfile ( lxor )
  | "slli" -> eval_ins rd rs imm rfile ( lsl )
  | "slri" -> eval_ins rd rs imm rfile ( lsr )
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
