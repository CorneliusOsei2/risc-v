open Registers
open IO
open ProcessInstructions
open Utilities

let acc = ref []

let rec gen_rtype op n =
  if n mod 15 = 0 then ()
  else
    let rd = gen_register n in
    let rs1 = gen_register (n + 1) in
    let rs2 = gen_register (n + 2) in
    acc := (op ^ " " ^ rd ^ ", " ^ rs1 ^ ", " ^ rs2) :: !acc;
    gen_rtype op (n + 1)

let rec gen_itype op n =
  if n mod 15 = 0 then ()
  else
    let rd = gen_register n in
    let rs1 = gen_register (n + 1) in
    let imm = gen_imm min_i max_i in
    acc := (op ^ " " ^ rd ^ ", " ^ rs1 ^ ", " ^ Int32.to_string imm) :: !acc;
    gen_itype op (n + 1)

let rec gen_stype op n =
  if n mod 15 = 0 then ()
  else
    let rd = gen_register n in
    let rs1 = gen_register (n + 1) in
    let rs2 = gen_register (n + 2) in
    acc := (op ^ " " ^ rd ^ ", " ^ rs1 ^ ", " ^ rs2) :: !acc;
    gen_stype op (n + 1)

let rec gen_utype op n =
  if n mod 15 = 0 then ()
  else
    let rd = gen_register n in
    let imm = gen_imm (Int32.of_int ~-2048) 2047l in
    acc := (op ^ " " ^ rd ^ ", " ^ Int32.to_string imm) :: !acc;
    gen_stype op (n + 1)

let gen_specific_insns ops =
  let rec helper ops =
    match ops with
    | [] -> ()
    | h :: t ->
        (match h with
        | "1" | "addi" -> gen_itype "addi" 1
        | "2" | "andi" -> gen_itype "andi" 1
        | "3" | "ori" -> gen_itype "ori" 1
        | "4" | "xori" -> gen_itype "xori" 1
        | "5" | "slli" -> gen_itype "slli" 1
        | "6" | "slri" -> gen_itype "slri" 1
        | "7" | "add" -> gen_rtype "add" 1
        | "8" | "and" -> gen_rtype "and" 1
        | "9" | "or" -> gen_rtype "or" 1
        | "10" | "xor" -> gen_rtype "xor" 1
        | "11" | "sll" -> gen_rtype "sll" 1
        | "12" | "srl" -> gen_rtype "srl" 1
        | "13" | "lui" -> gen_stype "lui" 1
        | "14" | "sw" -> gen_stype "sw" 1
        | "15" | "sb" -> gen_stype "sb" 1
        | "16" | "lw" -> gen_stype "lw" 1
        | "17" | "lb" -> gen_stype "lb" 1
        | _ -> ());
        helper t
  in
  helper ops;
  list_to_file (List.rev !acc)

let rec gen_insns () =
  ignore (List.map (fun op -> gen_stype op 1) rtype);
  ignore (List.map (fun op -> gen_rtype op 1) rtype);
  list_to_file (List.rev !acc)
