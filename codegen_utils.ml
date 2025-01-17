open Typed_ast
open Alloc_ast
open Typing_utils

let n_label = ref 0


module Idmap = Map.Make(String)

type alloc_env = {
  local_env: int Idmap.t;
  clos_env: int Idmap.t
}

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

let compute_const c env = 
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
    | TCVar id ->
      begin
      try 
      (AECst {
        aconst = ACVar (Vlocal (Idmap.find id env.local_env));
        typ = (TUnit, singleton_eff Div)
      })
      with _ ->
      (AECst {
        aconst = ACVar (Vlocal (Idmap.find id env.clos_env));
        typ = (TUnit, singleton_eff Div)
      })
      end
    | _ -> failwith "non implémenté"

let int_of_bool b =
  if b then 1 else 0

module VarMap = Map.Make(String) (* contient les variables libres *)
(* 
let rec free_expr e env = match e with
  | TECst c ->
    begin
      match c.tconst with
      | TCUnit | TCBool _ | TCInt _ | TCString _ -> VarMap.empty
      | TCVar x -> 
        if Idmap.mem x env then VarMap.add x (Idmap.find x env) VarMap.empty
        else VarMap.empty
    end      
  | TEList l ->
    List.fold_left (fun s e -> 
    VarMap.union (fun id x y -> Some(y)) s (free_expr (e.texpr) env)) (VarMap.empty) l
  | TENot e -> free_expr (e.texpr) env
  | TETilde e -> free_expr (e.texpr) env
  | TEBinop (_,e1, e2) -> 
    VarMap.union (fun id x y -> Some(y)) (free_expr (e1.texpr) env) (free_expr (e2.texpr) env)
  | TEUpdate (id, e) -> free_expr (e.texpr) env
  | TEReturn e -> free_expr (e.texpr) env
  | TEIf_then_else (e_if, e_then, e_else) ->
    VarMap.union (VarMap.union (free_expr e_if.texpr env) (free_expr e_then.texpr env))
                 (free_expr e_else.texpr env)
  | TEBlock b -> free_block b env
  | _ -> failwith "to do"

and free_block (b: tblock) env = 
  List.fold_left (fun s stmt -> VarMap.union s (free_stmt stmt env)) VarMap.empty b.tblock

and free_stmt (s: tstmt) env = match s.tstmt with
  | TSExpr e -> free_expr e.texpr env
  (* to do : je ne suis pas sûr que cela fasse le bon truc, notamment
     je ne sais pas ce que représente assign et update *)
  | TSAssign (x,e) -> VarMap.remove x (free_expr e.texpr env)
  | TSUpdate (x,e) -> VarMap.remove x (free_expr e.texpr env)

let free_variables (b: tfunbody) env =
  let args = b.tbody.args in
  let content = b.tbody.tcontent
  in List.fold_left 
  (fun s arg -> VarMap.remove arg s) (free_expr content.texpr env) args *)

let give_label p =
  incr n_label;
  "label_"^(string_of_int !n_label)
