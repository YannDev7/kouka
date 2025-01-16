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

let rec free_expr e env = match e with
  | TECst c ->
    begin
      match c.tconst with
      | TCUnit | TCBool _ | TCInt _ | TCString _ -> VarSet.empty
      | TCVar x -> 
        if Idmap.mem x env then VarSet.add x (VarSet.empty)
        else VarSet.empty
    end      
  | TEList l ->
    List.fold_left (fun s e -> VarSet.union s (free_expr (e.texpr) env)) (VarSet.empty) l
  | TENot e -> free_expr (e.texpr) env
  | TETilde e -> free_expr (e.texpr) env
  | TEBinop (_,e1, e2) -> VarSet.union (free_expr (e1.texpr) env) (free_expr (e2.texpr) env)
  | TEUpdate (id, e) -> free_expr (e.texpr) env
  | TEReturn e -> free_expr (e.texpr) env
  | TEIf_then_else (e_if, e_then, e_else) ->
    VarSet.union (VarSet.union (free_expr e_if.texpr env) (free_expr e_then.texpr env))
                 (free_expr e_else.texpr env)
  | TEBlock b -> free_block b env
  | _ -> failwith "to do"

and free_block (b: tblock) env = 
  List.fold_left (fun s stmt -> VarSet.union s (free_stmt stmt env)) VarSet.empty b.tblock

and free_stmt (s: tstmt) env = match s.tstmt with
  | TSExpr e -> free_expr e.texpr env
  (* to do : je ne suis pas sûr que cela fasse le bon truc, notamment
     je ne sais pas ce que représente assign et update *)
  | TSAssign (x,e) -> VarSet.remove x (free_expr e.texpr env)
  | TSUpdate (x,e) -> VarSet.remove x (free_expr e.texpr env)

let free_variables (b: tfunbody) env =
  let args = b.tbody.args in
  let content = b.tbody.tcontent
  in List.fold_left 
  (fun s arg -> VarSet.remove arg s) (free_expr content.texpr env) args

let give_label p =
  incr n_label;
  "label_"^(string_of_int !n_label)
