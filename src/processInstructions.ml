open Registers
open Utilities

let rtype = [ "add"; "sub"; "and"; "or"; "xor"; "nor"; "sll"; "srl" ]
let itype = [ "addi"; "andi"; "ori"; "xori" ]
let utype = [ "lui" ]
let stype = [ "sw"; "sb"; "lw"; "lb" ]

let eval_ri_ins rd rs1 rs2 rfile op r_type =
  let in1, in2 =
    ( get_register rs1 rfile,
      if r_type then get_register rs2 rfile else int_of_string rs2 )
  in
  let res = op in1 in2 in
  update_register rd res rfile

let process_rtype op rd rs1 rs2 rfile =
  match String.lowercase_ascii op with
  | "add" -> eval_ri_ins rd rs1 rs2 rfile ( + ) true
  | "sub" -> eval_ri_ins rd rs1 rs2 rfile ( - ) true
  | "and" -> eval_ri_ins rd rs1 rs2 rfile ( land ) true
  | "or" -> eval_ri_ins rd rs1 rs2 rfile ( lor ) true
  | "xor" -> eval_ri_ins rd rs1 rs2 rfile ( lxor ) true
  | "sll" -> eval_ri_ins rd rs1 rs2 rfile ( lsl ) true
  | "slr" -> eval_ri_ins rd rs1 rs2 rfile ( lsr ) true
  | _ -> rfile

let process_itype op rd rs imm rfile =
  match String.lowercase_ascii op with
  | "addi" -> eval_ri_ins rd rs imm rfile ( + ) false
  | "subi" -> eval_ri_ins rd rs imm rfile ( - ) false
  | "andi" -> eval_ri_ins rd rs imm rfile ( land ) false
  | "ori" -> eval_ri_ins rd rs imm rfile ( lor ) false
  | "xori" -> eval_ri_ins rd rs imm rfile ( lxor ) false
  | _ -> rfile

let process_utype op rd rs imm rfile = failwith "Unimplemented"
(* match String.lowercase_ascii op with
   | "lui" -> eval_ri_ins rd rs imm rfile ( + )
   | _ -> rfile *)

let process_stype op rd rs imm rfile = failwith "Unimplemented"
(* match String.lowercase_ascii op with
   | "sb" -> eval_ri_ins rd rs imm rfile ( + )
   | "sw" -> eval_ri_ins rd rs imm rfile ( - )
   | "lb" -> eval_ri_ins rd rs imm rfile ( land )
   | "lw" -> eval_ri_ins rd rs imm rfile ( land )
   | _ -> rfile *)

let rec evaluate_input_insns insns acc rfile =
  match insns with
  | [] -> acc
  | h :: t ->
      let op, rgs = split_instruction h in
      if List.exists (fun x -> x = op) rtype then
        let rd, rs1, rs2 = (List.nth rgs 0, List.nth rgs 1, List.nth rgs 2) in
        let new_register_state = process_rtype op rd rs1 rs2 rfile in
        evaluate_input_insns t (new_register_state :: acc) new_register_state
      else if List.exists (fun x -> x = op) itype then
        let rd, rs1, imm = (List.nth rgs 0, List.nth rgs 1, List.nth rgs 2) in
        let new_register_state = process_itype op rd rs1 imm rfile in
        evaluate_input_insns t (new_register_state :: acc) new_register_state
      else acc

let process_input_insns insns = register_init |> evaluate_input_insns insns []