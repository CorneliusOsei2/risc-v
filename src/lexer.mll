{
  open parser
  open Lexing


  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_bol = pos.pos_cnum;
      pos_lnum = pos.pos_lnum + 1
    }
}

let digit = '-'? ['0'-'9'] ['0'-'9']*

let white = [' ' '\t']+
let newline = ('\r' | '\n' | "\r\n")


rule read =
  parse
  | int { OFFSET (int_of_string(Lexing.lexeme lexbuf)) }
  | "x" ^ string_of_int(digit) {REG}
  | white { read lexbuf}
  | "," { COMMA }
  | newline { next_line lexbuf; NEWLINE }
  |eof { EOF }

