open Registers

let rtype = [ "add"; "sub"; "and"; "or"; "xor"; "nor"; "sll"; "srl" ]
let itype = [ "addi"; "andi"; "ori"; "xori" ]
let utype = [ "lui" ]
let stype = [ "sw"; "sb"; "lw"; "lb" ]

let eval_r_i_ins rd rs1 rs2 rfile op =
  update_register rd
    (op (fst (get_register rs1 rfile)) (fst (get_register rs2 rfile)), true)
    rfile

let process_rtype op rd rs1 rs2 rfile =
  match String.lowercase_ascii op with
  | "add" -> eval_r_i_ins rd rs1 rs2 rfile ( + )
  | "sub" -> eval_r_i_ins rd rs1 rs2 rfile ( - )
  | "and" -> eval_r_i_ins rd rs1 rs2 rfile ( land )
  | "or" -> eval_r_i_ins rd rs1 rs2 rfile ( lor )
  | "xor" -> eval_r_i_ins rd rs1 rs2 rfile ( lxor )
  | "nor" -> eval_r_i_ins rd rs1 rs2 rfile ( lxor )
  | "sll" -> eval_r_i_ins rd rs1 rs2 rfile ( lsl )
  | "slr" -> eval_r_i_ins rd rs1 rs2 rfile ( lsr )
  | _ -> rfile

let process_itype op rd rs imm rfile =
  match String.lowercase_ascii op with
  | "addi" -> eval_r_i_ins rd rs imm rfile ( + )
  | "subi" -> eval_r_i_ins rd rs imm rfile ( - )
  | "andi" -> eval_r_i_ins rd rs imm rfile ( land )
  | "ori" -> eval_r_i_ins rd rs imm rfile ( lor )
  | "xori" -> eval_r_i_ins rd rs imm rfile ( lxor )
  | _ -> rfile

let process_utype op rd rs imm rfile =
  match String.lowercase_ascii op with
  | "lui" -> eval_r_i_ins rd rs imm rfile ( + )
  | _ -> rfile

let process_stype op rd rs imm rfile =
  match String.lowercase_ascii op with
  | "sb" -> eval_r_i_ins rd rs imm rfile ( + )
  | "sw" -> eval_r_i_ins rd rs imm rfile ( - )
  | "lb" -> eval_r_i_ins rd rs imm rfile ( land )
  | "lw" -> eval_r_i_ins rd rs imm rfile ( land )
  | _ -> rfile

(* let rec evaluate_input_insns insns rfile =
   match insns with
   | [] -> pp_registers rfile
   | h :: t ->
       let ins = String.split_on_char ' ' h in
       let op = List.hd ins in
       if List.exists (fun x -> x = op) rtype then (
         let rd, rs1, rs2 = (List.nth ins 1, List.nth ins 2, List.nth ins 3) in
         let new_register_state = process_rtype op rd rs1 rs2 rfile in
         pp_registers new_register_state;
         evaluate_input_insns t new_register_state)
       else pp_registers rfile *)

(* let process_input_insns insns = register_init |> evaluate_input_insns insns *)