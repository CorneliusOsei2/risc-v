open OUnit2

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
        (if num mod 2 == 0 then string_of_int 0 ^ acc
        else string_of_int 1 ^ acc)

let dec_to_bin num acc =
  let is_neg = num < 0 in
  let n = if is_neg then pow 2 31 + num else num in
  let bin = dec_to_bin_helper n "" in
  "0b"
  ^ String.make (32 - String.length bin) (if is_neg then '1' else '0')
  ^ bin

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

let dec_to_hex num acc =
  let is_neg = num < 0 in
  let n = if is_neg then pow 2 31 + num else num in
  let hex = dec_to_hex_helper n "" in
  "0x" ^ String.make (8 - String.length hex) (if is_neg then 'f' else '0') ^ hex

type bool_expr =
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr
  | Xor of bool_expr * bool_expr
  | Nor of bool_expr * bool_expr

let rec eval val_a val_b = function
  | Not e -> not (eval val_a val_b e)
  | And (e1, e2) -> eval val_a val_b e1 && eval val_a val_b e2
  | Or (e1, e2) -> eval val_a val_b e1 || eval val_a val_b e2
  | Xor (e1, e2) -> eval val_a val_b e1 <> eval val_a val_b e2
  | Nor (e1, e2) -> not (eval val_a val_b e1 || eval val_a val_b e2)

let test_dec_to_bin (name : string) (num : int) (expected_output : string) :
    test =
  name >:: fun _ -> assert_equal expected_output (dec_to_bin num)
