open Syntax;;

let parse str =
  Parser.main Lexer.token
    (Lexing.from_string str)

let run str =
  Eval.eval (parse str) (Eval.emptyenv ())