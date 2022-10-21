open Registers
open Utilities

let rtype = [ "add"; "sub"; "and"; "or"; "xor"; "nor"; "sll"; "srl" ]
let itype = [ "addi"; "andi"; "ori"; "xori" ]
let utype = [ "lui" ]
let stype = [ "sw"; "sb"; "lw"; "lb" ]

let eval_r_ins rd rs1 rs2 rfile op =
  let res = op (get_register rs1 rfile) (get_register rs2 rfile) in
  update_register rd res rfile

let eval_i_ins rd rs1 imm rfile op =
  let res = op (get_register rs1 rfile) imm in
  update_register rd res rfile

let process_rtype op rd rs1 rs2 rfile =
  match String.lowercase_ascii op with
  | "add" -> eval_r_ins rd rs1 rs2 rfile ( + )
  | "sub" -> eval_r_ins rd rs1 rs2 rfile ( - )
  | "and" -> eval_r_ins rd rs1 rs2 rfile ( land )
  | "or" -> eval_r_ins rd rs1 rs2 rfile ( lor )
  | "xor" -> eval_r_ins rd rs1 rs2 rfile ( lxor )
  | "nor" -> eval_r_ins rd rs1 rs2 rfile ( lxor )
  | "sll" -> eval_r_ins rd rs1 rs2 rfile ( lsl )
  | "slr" -> eval_r_ins rd rs1 rs2 rfile ( lsr )
  | _ -> rfile

let process_itype op rd rs imm rfile =
  match String.lowercase_ascii op with
  | "addi" -> eval_i_ins rd rs imm rfile ( + )
  | "subi" -> eval_i_ins rd rs imm rfile ( - )
  | "andi" -> eval_i_ins rd rs imm rfile ( land )
  | "ori" -> eval_i_ins rd rs imm rfile ( lor )
  | "xori" -> eval_i_ins rd rs imm rfile ( lxor )
  | _ -> rfile

let process_utype op rd rs imm rfile =
  match String.lowercase_ascii op with
  | "lui" -> eval_r_ins rd rs imm rfile ( + )
  | _ -> rfile

let process_stype op rd rs imm rfile =
  match String.lowercase_ascii op with
  | "sb" -> eval_r_ins rd rs imm rfile ( + )
  | "sw" -> eval_r_ins rd rs imm rfile ( - )
  | "lb" -> eval_r_ins rd rs imm rfile ( land )
  | "lw" -> eval_r_ins rd rs imm rfile ( land )
  | _ -> rfile

let rec evaluate_input_insns insns rfile =
  match insns with
  | [] -> pp_registers rfile
  | h :: t ->
      let op, rgs = split_instruction h in
      if List.exists (fun x -> x = op) rtype then
        let rd, rs1, rs2 = (List.nth rgs 0, List.nth rgs 1, List.nth rgs 2) in
        let new_register_state = process_rtype op rd rs1 rs2 rfile in
        evaluate_input_insns t new_register_state
      else if List.exists (fun x -> x = op) itype then
        let rd, rs1, imm = (List.nth rgs 0, List.nth rgs 1, List.nth rgs 2) in
        let new_register_state =
          process_itype op rd rs1 (int_of_string imm) rfile
        in
        evaluate_input_insns t new_register_state
      else pp_registers rfile

let process_input_insns insns = register_init |> evaluate_input_insns insns