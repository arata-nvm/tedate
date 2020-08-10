type exp =
  | IntLit of int
  | BoolLit of bool
  | Plus of exp * exp
  | Minus of exp * exp
  | Times of exp * exp
  | Divide of exp * exp

type value =
  | IntVal of int
  | BoolVal of bool

let rec eval1 e =
  match e with
  | IntLit(n) -> n
  | Plus(e1, e2) -> (eval1 e1) + (eval1 e2)
  | Minus(e1, e2) -> (eval1 e1) - (eval1 e2)
  | Times(e1, e2) -> (eval1 e1) * (eval1 e2)
  | Divide(e1, e2) -> (eval1 e1) / (eval1 e2)
  | _ -> failwith "unknown expression"

let rec eval2 e =
  match e with
  | IntLit(n) -> IntVal(n)
  | BoolLit(b) -> BoolVal(b)
  | Plus(e1, e2) ->
    begin
      match (eval2 e1, eval2 e2) with
      | (IntVal(n1), IntVal(n2)) -> IntVal(n1 + n2)
      | _ -> failwith "integer values expected"
    end
  | Times(e1, e2) ->
    begin
      match (eval2 e1, eval2 e2) with
      | (IntVal(n1), IntVal(n2)) -> IntVal(n1 * n2)
      | _ -> failwith "integer values expected"
    end
  | _ -> failwith "unknown expression e"