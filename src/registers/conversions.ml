let rec decimal_to_binary num acc =
  match num with
  | 0 -> acc
  | _ ->
      decimal_to_binary (num / 2)
        (if num mod 2 == 0 then 0 :: acc else 1 :: acc)

let dec_to_binary_aux num acc = decimal_to_binary num []

let rec decimal_to_hex num acc =
  let n = num / 16 in
  match num mod 16 with
  | 0 -> string_of_int 0 :: acc
  | 1 -> decimal_to_hex n (string_of_int 1 :: acc)
  | 2 -> decimal_to_hex n (string_of_int 2 :: acc)
  | 3 -> decimal_to_hex n (string_of_int 3 :: acc)
  | 4 -> decimal_to_hex n (string_of_int 4 :: acc)
  | 5 -> decimal_to_hex n (string_of_int 5 :: acc)
  | 6 -> decimal_to_hex n (string_of_int 6 :: acc)
  | 7 -> decimal_to_hex n (string_of_int 7 :: acc)
  | 8 -> decimal_to_hex n (string_of_int 8 :: acc)
  | 9 -> decimal_to_hex n (string_of_int 9 :: acc)
  | 10 -> decimal_to_hex n ("A" :: acc)
  | 11 -> decimal_to_hex n ("B" :: acc)
  | 12 -> decimal_to_hex n ("C" :: acc)
  | 13 -> decimal_to_hex n ("D" :: acc)
  | 14 -> decimal_to_hex n ("E" :: acc)
  | 15 -> decimal_to_hex n ("F" :: acc)

let dec_to_hex_aux num acc = decimal_to_hex num []

type bool_expr =
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr
  | Xor of bool_expr * bool_expr
  | Nor of bool_expr * bool_expr

let rec eval val_a b val_b = function
  | Not e -> not (eval val_a b val_b e)
  | And (e1, e2) -> eval val_a b val_b e1 && eval val_a b val_b e2
  | Or (e1, e2) -> eval val_a b val_b e1 || eval val_a b val_b e2
  | Xor (e1, e2) -> eval val_a b val_b e1 <> eval val_a b val_b e2
  | Nor (e1, e2) -> not (eval val_a b val_b e1 || eval val_a b val_b e2)
