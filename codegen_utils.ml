open Typed_ast
open Alloc_ast
open Typing_utils

let tget_call_id e = match e.texpr with
  | TECst cst ->
    begin
      match cst.tconst with
        | TCVar s -> Some s
        | _ -> None
    end
  | _ -> None

let get_value_cst e = match e.texpr with
(* Traite l'expression e pour renvoyer sa valeur
Notamment traite les expressions arithmétiques *)
  | TECst cst -> cst.tconst
  | _ -> failwith "to do"

let compute_const c = 
    match c.tconst with
    | TCInt n -> 
      (AECst {
        aconst = ACallPrintIntImm n;
        typ = (TUnit, singleton_eff Div)
      })
    | _ -> failwith "non implémenté"