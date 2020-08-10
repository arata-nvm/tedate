open Syntax;;

let emptyenv () = []

let ext env x v = (x, v) :: env

let rec lookup x env =
  match env with
  | [] -> failwith ("unbound variable: " ^ x)
  | (y, v)::tl -> if x = y then v
                  else lookup x tl

let rec eval e env =
  let binop f e1 e2 env =
    match (eval e1 env, eval e2 env) with
    | (IntVal(n1), IntVal(n2)) -> IntVal(f n1 n2)
    | _ -> failwith "integer values exprected"
  in
  match e with
  | IntLit(n) -> IntVal(n)
  | BoolLit(b) -> BoolVal(b)
  | Var(x) -> lookup x env
  | Plus(e1, e2) -> binop (+) e1 e2 env
  | Minus(e1, e2) -> binop (-) e1 e2 env
  | Times(e1, e2) -> binop ( * ) e1 e2 env
  | Div(e1, e2) -> binop (/) e1 e2 env
  | If(e1, e2, e3) ->
    begin
      match (eval e1 env) with
      | BoolVal(true) -> eval e2 env
      | BoolVal(false) -> eval e3 env
      | _ -> failwith "wrong value"
    end
  | Eq(e1, e2) ->
    begin
      match (eval e1 env, eval e2 env) with
      | (IntVal(n1), IntVal(n2)) -> BoolVal(n1 = n2)
      | (BoolVal(b1), BoolVal(b2)) -> BoolVal(b1 = b2)
      | _ -> failwith "wrong value"
    end
  | Greater(e1, e2) ->
    begin
      match (eval e1 env, eval e2 env) with
      | (IntVal(n1), IntVal(n2)) -> BoolVal(n1 > n2)
      | _ -> failwith "wrong value"
    end
  | Let(x, e1, e2) ->
    let env1 = ext env x (eval e1 env)
    in eval e2 env1
  | _ -> failwith "unknown expression e"
