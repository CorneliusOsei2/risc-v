open Registers
open Utilities
open Int32

let rtype = [ "add"; "sub"; "and"; "or"; "xor"; "nor"; "sll"; "srl" ]
let itype = [ "addi"; "andi"; "ori"; "xori" ]
let utype = [ "lui" ]
let stype = [ "sw"; "sb"; "lw"; "lb" ]

let eval_ri_ins rd rs1 rs2 rfile op r_type =
  let in1, in2 =
    ( get_register rs1 rfile,
      if r_type then get_register rs2 rfile else of_string rs2 )
  in
  let res = op in1 in2 in
  update_register rd res rfile

let eval_shift_insns rd rs1 rs2 rfile op r_type =
  let in1, in2 =
    ( get_register rs1 rfile,
      if r_type then get_register rs2 rfile else of_string rs2 )
  in
  let res = op in1 (to_int in2) in
  update_register rd res rfile

let process_rtype op rd rs1 rs2 rfile =
  match String.lowercase_ascii op with
  | "add" -> eval_ri_ins rd rs1 rs2 rfile add true
  | "sub" -> eval_ri_ins rd rs1 rs2 rfile sub true
  | "and" -> eval_ri_ins rd rs1 rs2 rfile logand true
  | "or" -> eval_ri_ins rd rs1 rs2 rfile logor true
  | "xor" -> eval_ri_ins rd rs1 rs2 rfile logxor true
  | "sll" -> eval_shift_insns rd rs1 rs2 rfile shift_left true
  | "slr" -> eval_shift_insns rd rs1 rs2 rfile shift_right true
  | _ -> rfile

let process_itype op rd rs imm rfile =
  match String.lowercase_ascii op with
  | "addi" -> eval_ri_ins rd rs imm rfile add false
  | "subi" -> eval_ri_ins rd rs imm rfile sub false
  | "andi" -> eval_ri_ins rd rs imm rfile logand false
  | "ori" -> eval_ri_ins rd rs imm rfile logor false
  | "xori" -> eval_ri_ins rd rs imm rfile logxor false
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

let process_step_instructions insn ?(rfile = register_init) =
  List.nth (evaluate_input_insns [ insn ] [] rfile) 0
