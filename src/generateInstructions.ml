open Registers
open IO
open ProcessInstructions
open Utilities

let acc = ref [] 

let rec gen_insns () =  
 List.map (fun op -> gen_rtype op 1) rtype;  List.map (fun op -> gen_itype op 1) itype; list_to_file (List.rev !acc); 

and gen_rtype op n =
  if n mod 65 = 0 then ()
  else
    let rd = gen_register n in
    let rs1 = gen_register (n + 1) in
    let rs2 = gen_register (n + 2) in
    acc := (op ^ " " ^ rd ^ ", " ^ rs1 ^ ", " ^ rs2) :: !acc; gen_rtype op (n + 1)

and gen_itype op n =
if n mod 76 = 0 then () else
    let rd = gen_register n in
    let rs1 = gen_register (n + 1) in
    let imm = string_of_int (Random.int 2047) in
    acc := (op ^ " " ^ rd ^ ", " ^ rs1 ^ ", " ^ imm) :: !acc; gen_itype op (n + 1)

and gen_utype op n =
if n mod 3 = 0 then () else
    let rd = gen_register n in
    let rs1 = gen_register (n + 1) in
    let imm = string_of_int (Random.int 2047) in
    acc := (op ^ " " ^ rd ^ ", " ^ rs1 ^ ", " ^ imm) :: !acc; gen_utype op (n + 1)
    