open Registers
open Utilities

let rtype =
  [ "add"; "sub"; "and"; "or"; "xor"; "nor"; "sll"; "srl"; "slt"; "sltu" ]

let itype = [ "addi"; "andi"; "ori"; "xori"; "slli"; "srli" ]
let utype = [ "lui" ]
let stype = [ "sw"; "sb"; "lw"; "lb" ]
let mem_bitmask = 255l
let max_i = 2047l
let min_i = -2048l
let max_u = 524287l
let min_u = -524288l

exception WrongFormat of int
exception NotWordAligned
exception IncorrectRTypeFormat of int
exception IncorrectITypeFormat of int
exception IncorrectUTypeFormat of int
exception IncorrectSTypeFormat of int

let ins_track = ref 0
(* Instruction counter  *)

(** [compare_int32 n1 n2] compares [n1] and [n2] using Stdlib.compare but returns an 
  [int32] value instead of [int] value *)
let compare_int32 n1 n2 =
  let open Int32 in
  let n = compare n1 n2 in
  if n = 1 then 0l else 1l

(** [eval_shift_r_insns rd rs1 rs2 rfile op] evaluates [op] R-type instructions with 
    source registers [rs1], [rs2], destination register [rd] and register file [rfile]. *)
let eval_r_insns rd rs1 rs2 rfile op =
  let open Int32 in
  let in1, in2 = (get_register rs1 rfile, get_register rs2 rfile) in
  let res = op in1 in2 in
  update_register rd (Int32.to_int res) rfile

(** [eval_i_insns rd rs imm rfile op] evaluates [op] I-type instructions with 
    source register [rs], immediate [imm], destination register [rd] and
   register file [rfile]. *)
let eval_i_insns rd rs imm rfile op =
  let open Int32 in
  let in1, in2 = (get_register rs rfile, of_string imm) in
  if in2 > max_i || in2 < min_i then raise (IncorrectITypeFormat !ins_track)
  else
    let res = op in1 in2 in
    update_register rd (Int32.to_int res) rfile |> update_register "x0" 0

(** [eval_shift_r_insns rd rs1 rs2 rfile op] evaluates [op] R-type (shift) instructions 
  with source registers [rs1], [rs2], destination register [rd] and register file [rfile].*)
let eval_shift_r_insns rd rs1 rs2 rfile op =
  let open Int32 in
  let in1, in2 = (get_register rs1 rfile, get_register rs2 rfile) in
  let res = op in1 (to_int in2) in
  update_register rd (Int32.to_int res) rfile |> update_register "x0" 0

(** [eval_shift_r_insns rd rs1 rs2 rfile op] evaluates [op] I-type (shift) instructions 
  with source register [rs], immediate [imm], destination register [rd] and register file 
  [rfile].*)
let eval_shift_i_insns rd rs imm rfile op =
  let open Int32 in
  let in1, in2 = (get_register rs rfile, of_string imm) in
  if in2 > max_i || in2 < min_i then raise (IncorrectITypeFormat !ins_track)
  else
    let res = op in1 (to_int in2) in
    update_register rd (Int32.to_int res) rfile |> update_register "x0" 0

(** [eval_store_insns op rs1 offset rs2 rfile mem] evaluates an [op] S-type (store) 
  instruction with registers [rs1] [rs2], register file [rfile] and memory [mem].
  The supported store instructions are [sw] and [sb]. *)
let eval_store_insns op rs1 offset rs2 rfile mem =
  let open Int32 in
  let v = get_register rs1 rfile in
  let byte_one = logand (shift_right v 0) mem_bitmask in
  let byte_two = logand (shift_right v 8) mem_bitmask in
  let byte_three = logand (shift_right v 16) mem_bitmask in
  let byte_four = logand (shift_right v 24) mem_bitmask in
  let addr = int_of_string offset + to_int (get_register rs2 rfile) in
  if op = "sb" then Memory.update_memory addr byte_one mem
  else if addr mod 4 <> 0 then raise NotWordAligned
  else
    Memory.update_memory addr byte_one mem
    |> Memory.update_memory (addr + 1) byte_two
    |> Memory.update_memory (addr + 2) byte_three
    |> Memory.update_memory (addr + 3) byte_four

(** [eval_load_insns op rs1 offset rs2 rfile mem] evaluates an [op] S-type (load) 
  instruction with registers [rs1] [rs2], register file [rfile] and memory [mem].
  The supported store instructions are [lw] and [lb]. *)
let eval_load_insns op rs1 offset rs2 rfile mem =
  let open Int32 in
  let addr = int_of_string offset + to_int (get_register rs2 rfile) in
  if op = "lb" then
    let v = Memory.get_memory addr mem in
    update_register rs1 (to_int v) rfile
  else if addr mod 4 <> 0 then raise NotWordAligned
  else
    let byte_one =
      shift_left (logand (Memory.get_memory addr mem) mem_bitmask) 0
    in
    let byte_two =
      shift_left (logand (Memory.get_memory (addr + 1) mem) mem_bitmask) 8
    in
    let byte_three =
      shift_left (logand (Memory.get_memory (addr + 2) mem) mem_bitmask) 16
    in
    let byte_four =
      shift_left (logand (Memory.get_memory (addr + 3) mem) mem_bitmask) 24
    in
    let v = add byte_one byte_two |> add byte_three |> add byte_four in
    update_register rs1 (Int32.to_int v) rfile |> update_register "x0" 0

let process_rtype op rd rs1 rs2 rfile =
  if not (register_check rd && register_check rs1 && register_check rs2) then
    failwith "Wrong Register Number"
  else
    let open Int32 in
    match String.lowercase_ascii op with
    | "add" -> eval_r_insns rd rs1 rs2 rfile add
    | "sub" -> eval_r_insns rd rs1 rs2 rfile sub
    | "and" -> eval_r_insns rd rs1 rs2 rfile logand
    | "or" -> eval_r_insns rd rs1 rs2 rfile logor
    | "xor" -> eval_r_insns rd rs1 rs2 rfile logxor
    | "sll" -> eval_shift_r_insns rd rs1 rs2 rfile shift_left
    | "srl" -> eval_shift_r_insns rd rs1 rs2 rfile shift_right_logical
    | "sra" -> eval_shift_r_insns rd rs1 rs2 rfile shift_right
    | "slt" -> eval_r_insns rd rs1 rs2 rfile compare_int32
    | _ -> rfile

let process_itype op rd rs imm rfile =
  if not (register_check rd && register_check rs) then
    failwith "Wrong Register Number"
  else
    let open Int32 in
    match String.lowercase_ascii op with
    | "addi" -> eval_i_insns rd rs imm rfile add
    | "andi" -> eval_i_insns rd rs imm rfile logand
    | "ori" -> eval_i_insns rd rs imm rfile logor
    | "xori" -> eval_i_insns rd rs imm rfile logxor
    | "slli" -> eval_shift_i_insns rd rs imm rfile shift_left
    | "srli" -> eval_shift_i_insns rd rs imm rfile shift_right_logical
    | "srai" -> eval_shift_i_insns rd rs imm rfile shift_right
    | "slti" -> eval_r_insns rd rs imm rfile compare_int32
    | _ -> rfile

let process_utype rd imm rfile =
  if not (register_check rd) then failwith "Wrong Register Number"
  else
    let open Int32 in
    let v = shift_left (of_string imm) 12 in
    update_register rd (Int32.to_int v) rfile

let process_stype op rs1 offset rs2 rfile mem =
  if not (register_check rs1 && register_check rs2) then
    failwith "Wrong Register Number"
  else
    let open Int32 in
    match String.lowercase_ascii op with
    | s when s = "sw" || s = "sb" ->
        (rfile, eval_store_insns s rs1 offset rs2 rfile mem)
    | s when s = "lw" || s = "lb" ->
        (eval_load_insns op rs1 offset rs2 rfile mem, mem)
    | _ -> (rfile, mem)

let rec process_insns insns acc rfile mem =
  match insns with
  | [] -> acc
  | h :: t -> (
      try
        ins_track := !ins_track + 1;
        let op, rgs = split_instruction h in
        if List.exists (fun x -> x = op) rtype then
          (* R-Type Instructions: op rd rs1 rs2 *)
          try
            let rd, rs1, rs2 =
              (List.nth rgs 0, List.nth rgs 1, List.nth rgs 2)
            in
            let new_register_state = process_rtype op rd rs1 rs2 rfile in
            process_insns t
              ((new_register_state, mem) :: acc)
              new_register_state mem
          with _ -> raise (IncorrectRTypeFormat !ins_track)
        else if List.exists (fun x -> x = op) itype then
          (* I-Type Instructions: op rd rs imm *)
          try
            let rd, rs1, imm =
              (List.nth rgs 0, List.nth rgs 1, List.nth rgs 2)
            in
            let new_register_state = process_itype op rd rs1 imm rfile in
            process_insns t
              ((new_register_state, mem) :: acc)
              new_register_state mem
          with _ -> raise (IncorrectITypeFormat !ins_track)
        else if List.exists (fun x -> x = op) stype then
          (* S-Type Instructions: op rs1 offset(rs2) *)
          try
            let op, rgs = split_stype h in
            let rs1, offset, rs2 =
              (List.nth rgs 0, List.nth rgs 1, List.nth rgs 2)
            in
            let rfile, mem = process_stype op rs1 offset rs2 rfile mem in
            process_insns t ((rfile, mem) :: acc) rfile mem
          with _ -> raise (IncorrectSTypeFormat !ins_track)
        else if List.exists (fun x -> x = op) utype then
          try
            (* U-Type Instructions: op rs1 imm *)
            let op, rgs = split_instruction h in
            let rd, imm = (List.nth rgs 0, List.nth rgs 1) in
            let new_register_state = process_utype rd imm rfile in
            process_insns t
              ((new_register_state, mem) :: acc)
              new_register_state mem
          with _ -> raise (IncorrectUTypeFormat !ins_track)
        else raise (WrongFormat !ins_track)
      with _ -> raise (WrongFormat !ins_track))

let process_file_insns insns =
  Memory.init () |> process_insns insns [] (Registers.init ())

let process_step_insns insn rfile mem =
  List.nth (process_insns [ insn ] [] rfile mem) 0
