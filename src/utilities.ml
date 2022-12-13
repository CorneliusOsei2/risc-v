let pp_string n c v = v ^ String.make (n - String.length v) c
let pp_string_rev n c v = String.make (n - String.length v) c ^ v

let rec pow a n =
  match n with
  | 0 -> 1
  | 1 -> a
  | _ ->
      let rem = a mod n in
      let half = pow a (n / 2) in
      if rem = 1 then a * half * half else half * half

let rec dec_to_bin_helper num acc =
  match num with
  | 0 -> acc
  | _ ->
      dec_to_bin_helper (num / 2)
        (if num mod 2 = 0 then string_of_int 0 ^ acc else string_of_int 1 ^ acc)

let dec_to_bin num =
  let is_neg = num < 0 in
  let n = if is_neg then pow 2 31 + num else num in
  let bin = dec_to_bin_helper n "" in
  "0b" ^ pp_string_rev 32 (if is_neg then '1' else '0') bin

let rec dec_to_hex_helper num acc =
  if num = 0 then acc
  else
    let n = num / 16 in
    match num mod 16 with
    | 10 -> dec_to_hex_helper n ("a" ^ acc)
    | 11 -> dec_to_hex_helper n ("b" ^ acc)
    | 12 -> dec_to_hex_helper n ("c" ^ acc)
    | 13 -> dec_to_hex_helper n ("d" ^ acc)
    | 14 -> dec_to_hex_helper n ("e" ^ acc)
    | 15 -> dec_to_hex_helper n ("f" ^ acc)
    | x -> dec_to_hex_helper n (string_of_int x ^ acc)

let dec_to_hex num =
  let is_neg = num < 0 in
  let n = if is_neg then pow 2 31 + num else num in
  let hex = dec_to_hex_helper n "" in
  "0x" ^ pp_string_rev 8 (if is_neg then 'f' else '0') hex

let split_instruction instruct =
  let s = String.trim instruct in
  let op_idx = String.index_from s 0 ' ' in
  let op = String.sub s 0 op_idx in
  let args =
    String.sub s (op_idx + 1) (String.length s - (op_idx + 1))
    |> String.trim |> String.split_on_char ',' |> List.map String.trim
  in
  (op, args)

let split_offset off =
  match off with
  | [] -> []
  | [ x ] ->
      String.sub x 0 (String.index_from x 0 '(')
      :: [
           String.sub x
             (String.index_from x 0 '(' + 1)
             (String.length x - (String.index_from x 0 '(' + 2));
         ]
  | h :: t -> failwith "Invalid form"

let split_stype instruct =
  let op, args = split_instruction instruct in
  (op, List.hd args :: split_offset (List.tl args))

let string_of_insn (op, args) =
  let rec pp_rs rs acc =
    match rs with [] -> acc | h :: t -> pp_rs t (acc ^ " " ^ h)
  in
  op ^ pp_rs args ""

let string_of_list lst =
  let len = List.length lst in
  let open Buffer in
  let buf = create (3 * len) in
  add_string buf "[";
  List.iteri
    (fun i v ->
      add_string buf (String.escaped v);
      if i < len - 1 then add_string buf ", ")
    lst;
  add_string buf "]";
  contents buf

let list_of_string s = s |> String.split_on_char ',' |> List.map String.trim

let gen_ops () =
  "\n\
   \tI-Type:\n\
   \t1. addi    2. andi    3. ori    4. xori  \n\
   \t5. slli    6. srli    7. slti\n\n\
   \tR-Type:\n\
   \t8. add    9. and    10. or    11. xor   \n\
   \t12. sll   13. srl   14. slt\n\n\
   \tS-Type:\n\
   \t15. sw    16. sb    17. lw    18. lb\n\n\
   \tU-Type:\n\
   \t19. lui\n\n"

let register_check r =
  if String.length r < 2 then false
  else if String.starts_with "x" r then
    let digits = String.sub r 1 (String.length r - 1) |> int_of_string_opt in
    match digits with Some x -> x >= 0 && x < 32 | None -> false
  else false
