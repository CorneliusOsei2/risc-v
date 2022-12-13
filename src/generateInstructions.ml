open Registers
open IO
open ProcessInstructions
open Utilities

let acc = ref [] 



let rec gen_rtype () = 
let rec helper op n = if n mod 65 = 0 then ()
  else
    let rd = gen_register n in
    let rs1 = gen_register (n + 1) in
    let rs2 = gen_register (n + 2) in
    acc := (op ^ " " ^ rd ^ ", " ^ rs1 ^ ", " ^ rs2) :: !acc; helper op (n + 1)
  in List.map (fun op -> helper op 1) rtype 

let rec gen_itype () = 
let rec helper op n = if n mod 65 = 0 then ()
  else
    let rd = gen_register n in
    let rs1 = gen_register (n + 1) in
    let rs2 = gen_register (n + 2) in
    acc := (op ^ " " ^ rd ^ ", " ^ rs1 ^ ", " ^ rs2) :: !acc; helper op (n + 1)
  in List.map (fun op -> helper op 1) rtype 


let rec gen_stype () = 
let rec helper op n = if n mod 65 = 0 then ()
  else
    let rd = gen_register n in
    let rs1 = gen_register (n + 1) in
    let rs2 = gen_register (n + 2) in
    acc := (op ^ " " ^ rd ^ ", " ^ rs1 ^ ", " ^ rs2) :: !acc; helper op (n + 1)
  in List.map (fun op -> helper op 1) rtype 


  let rec gen_insns () =  
 ignore (gen_rtype ());
 list_to_file (List.rev !acc); 