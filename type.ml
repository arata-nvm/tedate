open Syntax;;

type ty =
  | TInt
  | TBool

let rec tcheck e =
  match e with
  | IntLit(_) -> TInt
  | BoolLit(_) -> TBool
  | Plus(e1, e2) ->
    begin
      match (tcheck e1, tcheck e2) with
      | (TInt, TInt) -> TInt
      | _ -> failwith "type error in Plus"
    end
  | If(e1, e2, e3) ->
    begin
      match (tcheck e1, tcheck e2, tcheck e3) with
      | (TBool, TInt, TInt) -> TInt
      | (TBool, TBool, TBool) -> TBool
      | _ -> failwith "type error in IF"
    end
  | Eq(e1, e2) ->
    begin
      match (tcheck e1, tcheck e2) with
      | (TInt, TInt) | (TBool, TBool) -> TBool
      | _ -> failwith "type error in Eq"
    end
  | _ -> failwith "unknown expression"
