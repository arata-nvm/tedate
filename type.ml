open Syntax;;
open Common;;

type ty =
  | TInt
  | TBool

type tyenv = (string * ty) list

let rec tcheck te e =
  match e with
  | Var(s) -> lookup s te
  | IntLit(_) -> TInt
  | BoolLit(_) -> TBool
  | Plus(e1, e2) ->
    begin
      match (tcheck te e1, tcheck te e2) with
      | (TInt, TInt) -> TInt
      | _ -> failwith "type error in Plus"
    end
  | If(e1, e2, e3) ->
    begin
      match (tcheck te e1, tcheck te e2, tcheck te e3) with
      | (TBool, TInt, TInt) -> TInt
      | (TBool, TBool, TBool) -> TBool
      | _ -> failwith "type error in IF"
    end
  | Eq(e1, e2) ->
    begin
      match (tcheck te e1, tcheck te e2) with
      | (TInt, TInt) | (TBool, TBool) -> TBool
      | _ -> failwith "type error in Eq"
    end
  | _ -> failwith "unknown expression"
