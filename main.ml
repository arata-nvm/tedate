type exp =
  | IntLit of int
  | BoolLit of bool
  | Plus of exp * exp
  | Minus of exp * exp
  | Times of exp * exp
  | Divide of exp * exp
  | If of exp * exp * exp
  | Eq of exp * exp
  | Greater of exp * exp

type value =
  | IntVal of int
  | BoolVal of bool

let rec eval e =
  let binop f e1 e2 =
    match (eval e1, eval e2) with
    | (IntVal(n1), IntVal(n2)) -> IntVal(f n1 n2)
    | _ -> failwith "integer values exprected"
  in
  match e with
  | IntLit(n) -> IntVal(n)
  | BoolLit(b) -> BoolVal(b)
  | Plus(e1, e2) -> binop (+) e1 e2
  | Minus(e1, e2) -> binop (-) e1 e2
  | Times(e1, e2) -> binop ( * ) e1 e2
  | Divide(e1, e2) -> binop (/) e1 e2
  | If(e1, e2, e3) ->
    begin
      match (eval e1) with
      | BoolVal(true) -> eval e2
      | BoolVal(false) -> eval e3
      | _ -> failwith "wrong value"
    end
  | Eq(e1, e2) ->
    begin
      match (eval e1, eval e2) with
      | (IntVal(n1), IntVal(n2)) -> BoolVal(n1 = n2)
      | (BoolVal(b1), BoolVal(b2)) -> BoolVal(b1 = b2)
      | _ -> failwith "wrong value"
    end
  | Greater(e1, e2) ->
    begin
      match (eval e1, eval e2) with
      | (IntVal(n1), IntVal(n2)) -> BoolVal(n1 > n2)
      | _ -> failwith "wrong value"
    end
  | _ -> failwith "unknown expression e"