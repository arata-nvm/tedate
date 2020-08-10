type exp =
  | IntLit of int
  | Plus of exp * exp
  | Minus of exp * exp
  | Times of exp * exp
  | Divide of exp * exp

let rec eval1 e =
  match e with
  | IntLit(n) -> n
  | Plus(e1, e2) -> (eval1 e1) + (eval1 e2)
  | Minus(e1, e2) -> (eval1 e1) - (eval1 e2)
  | Times(e1, e2) -> (eval1 e1) * (eval1 e2)
  | Divide(e1, e2) -> (eval1 e1) / (eval1 e2)
  | _ -> failwith "unknown expression"