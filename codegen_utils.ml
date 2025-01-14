open Typed_ast
open Alloc_ast
open Typing_utils

let n_label = ref 0

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
    | TCBool b ->
      (AECst {
        aconst = ACallPrintBoolImm b;
        typ = (TUnit, singleton_eff Div)
      })
    | TCString s ->
      (AECst {
        aconst = ACallPrintStringImm s;
        typ = (TUnit, singleton_eff Div)
      })
    | _ -> failwith "non implémenté"

let int_of_bool b =
  if b then 1 else 0

module VarSet = Set.Make(String) (* contient les variables libres *)

let free_variables (b: tfunbody) =
  let args = b.tbody.args in
  let content = b.tbody.tcontent in
  let rec aux = function
    | TECst _ ->
      failwith "to do"
    | TEList l ->
      List.fold_left (fun s e -> VarSet.union s (aux (e.texpr))) (VarSet.empty) l
    | TENot e -> aux (e.texpr)
    | TETilde e -> aux (e.texpr)
    | TEBinop (_,e1, e2) -> VarSet.union (aux (e1.texpr)) (aux (e2.texpr))
    | TEUpdate (id, e) -> aux (e.texpr)
    | TEReturn e -> aux (e.texpr)
    | TEIf_then_else (e_if, e_then, e_else) ->
      VarSet.union (VarSet.union (aux e_if.texpr) (aux e_then.texpr)) (aux e_else.texpr)
    | TEBlock b ->
      failwith "to do"
    | _ -> failwith "to do"
  in aux content.texpr

let give_label p =
  incr n_label;
  "label_"^(string_of_int !n_label)
