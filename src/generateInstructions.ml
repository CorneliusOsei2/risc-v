open Registers
open ProcessInstructions

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
