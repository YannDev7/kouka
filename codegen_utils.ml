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

let rec free_expr = function
  | TECst c ->
    begin
      match c.tconst with
      | TCUnit | TCBool _ | TCInt _ | TCString _ -> VarSet.empty
      | TCVar x -> VarSet.add x (VarSet.empty)
    end      
  | TEList l ->
    List.fold_left (fun s e -> VarSet.union s (free_expr (e.texpr))) (VarSet.empty) l
  | TENot e -> free_expr (e.texpr)
  | TETilde e -> free_expr (e.texpr)
  | TEBinop (_,e1, e2) -> VarSet.union (free_expr (e1.texpr)) (free_expr (e2.texpr))
  | TEUpdate (id, e) -> free_expr (e.texpr)
  | TEReturn e -> free_expr (e.texpr)
  | TEIf_then_else (e_if, e_then, e_else) ->
    VarSet.union (VarSet.union (free_expr e_if.texpr) (free_expr e_then.texpr)) (free_expr e_else.texpr)
  | TEBlock b -> free_block b
  | _ -> failwith "to do"

and free_block (b: tblock) = 
  List.fold_left (fun s stmt -> VarSet.union s (free_stmt stmt)) VarSet.empty b.tblock

and free_stmt (s: tstmt) = match s.tstmt with
  | TSExpr e -> free_expr e.texpr
  (* to do : je ne suis pas sûr que cela fasse le bon truc, notamment
     je ne sais pas ce que représente assign et update *)
  | TSAssign (x,e) -> VarSet.remove x (free_expr e.texpr)
  | TSUpdate (x,e) -> VarSet.remove x (free_expr e.texpr)

let free_variables (b: tfunbody) =
  let args = b.tbody.args in
  let content = b.tbody.tcontent
  in List.fold_left 
  (fun s arg -> VarSet.remove arg s) (free_expr content.texpr) args

let give_label p =
  incr n_label;
  "label_"^(string_of_int !n_label)
