open Registers
open IO
open ProcessInstructions
open Utilities

let acc = ref [] 



let rec gen_rtype op n = if n mod 65 = 0 then ()
  else
    let rd = gen_register n in
    let rs1 = gen_register (n + 1) in
    let rs2 = gen_register (n + 2) in
    acc := (op ^ " " ^ rd ^ ", " ^ rs1 ^ ", " ^ rs2) :: !acc; gen_rtype op (n + 1)

let rec gen_itype op n = if n mod 65 = 0 then ()
  else
    let rd = gen_register n in
    let rs1 = gen_register (n + 1) in
    let imm = gen_imm (Int32.of_int ~-2048) 2047l in
    acc := (op ^ " " ^ rd ^ ", " ^ rs1 ^ ", " ^ (Int32.to_string imm)) :: !acc; gen_itype op (n + 1)

let rec gen_stype op n = if n mod 65 = 0 then ()
  else
    let rd = gen_register n in
    let rs1 = gen_register (n + 1) in
    let rs2 = gen_register (n + 2) in
    acc := (op ^ " " ^ rd ^ ", " ^ rs1 ^ ", " ^ rs2) :: !acc; gen_stype op (n + 1)

let gen_specific_insns ops = let rec helper ops = match ops with [] -> () | h :: t -> (match h with  "1" | "addi" -> gen_itype "addi" 1 | "2" | "andi" -> gen_itype "andi" 1 | _ -> ()); helper t  
in helper ops; list_to_file (List.rev !acc) 


  let rec gen_insns () =  ignore (List.map (fun op -> gen_stype op 1) rtype); ignore (List.map (fun op -> gen_rtype op 1) rtype) ; list_to_file (List.rev !acc) 