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

let sigma = Hashtbl.create 32

let init_sigma =
  Hashtbl.add sigma 'a' 1;
  Hashtbl.add sigma 'b' 2;
  Hashtbl.add sigma 'c' 3;
  Hashtbl.add sigma 'd' 4;
  Hashtbl.add sigma 'e' 5;
  Hashtbl.add sigma 'f' 6;
  Hashtbl.add sigma 'g' 7;
  Hashtbl.add sigma 'h' 8;
  Hashtbl.add sigma 'i' 9;
  Hashtbl.add sigma 'j' 10;
  Hashtbl.add sigma 'k' 11;
  Hashtbl.add sigma 'l' 12;
  Hashtbl.add sigma 'm' 13;
  Hashtbl.add sigma 'n' 14;
  Hashtbl.add sigma 'o' 15;
  Hashtbl.add sigma 'p' 16;
  Hashtbl.add sigma 'q' 17;
  Hashtbl.add sigma 'r' 18;
  Hashtbl.add sigma 's' 19;
  Hashtbl.add sigma 't' 20;
  Hashtbl.add sigma 'u' 21;
  Hashtbl.add sigma 'v' 22;
  Hashtbl.add sigma 'w' 23;
  Hashtbl.add sigma 'x' 24;
  Hashtbl.add sigma 'y' 25;
  Hashtbl.add sigma 'z' 26

let c_to_int c =
  (* transforme un charactère en entier lui correspondant *)
  Hashtbl.find sigma c

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
    | 
    | _ -> failwith "to do"
  in aux content.texpr