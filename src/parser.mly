%{
open Ast
open Processor
open Registers
open Utilities
open IO
open ProcessInstructions
%}
%token <string> OP
%token <int> OFFSET
%token <string> REG
%token COMMA
%token NEWSPACE
%token WHITESPACE
%token EOF

%start <Ast.expr> prog

%%

prog:
  | e = expr; EOF { e }
  ;

  expr:
  | x = OP { String x }
  | y = REG { String y}
  | i = OFFSET { Int i }
  |OP; REG; COMMA; REG; COMMA, REG {R (Rtype)}

  | e1 = expr; LEQ; e2 = expr { Binop (Leq, e1, e2) }
  | e1 = expr; TIMES; e2 = expr { Binop (Mult, e1, e2) }
  | e1 = expr; PLUS; e2 = expr { Binop (Add, e1, e2) }
  | LET; x = ID; EQUALS; e1 = expr; IN; e2 = expr { Let (x, e1, e2) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
  | LPAREN; e=expr; RPAREN {e}
  ;

