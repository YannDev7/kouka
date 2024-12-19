open Ast 
open Exception
open Typed_ast 
open Pp
open Lexing

module Idmap = Map.Make(String)

type env = {
  types: typ Idmap.t;
  vars: bool Idmap.t
}

(* Finds the representant of a typ. *)
let rec head = function
  | TVar { def = Some t }, eff -> head t
  | t -> t

(* Applies head to all tvar of a typ. *)
let rec cannon ty = match ty with
  | TVar { def = Some t }, eff -> cannon t
  | TUnit, eff | TInt, eff | TBool, eff | TString, eff -> ty
  | TList t, eff -> (TList (cannon t), eff)
  | TFun (args, ret), eff ->
    (* be careful of args order *)
    (TFun (
      (* fun bug, if we switch args and [] it is still well typed lol... *)
      List.rev (List.fold_left (fun acc a -> (cannon a)::acc) [] args),
      cannon ret
    ), eff)
  | TMaybe t, eff -> (TMaybe (cannon t), eff)
  | t -> t

(* Returns the typ_val part of a typ. *)
let head_typ t = fst (head t)

(* Finds the representant of an effects. *)
let rec head_teff = function
  | TEff { edef = Some te } -> head_teff te
  | te -> te

(* Returns the effects part of a typ. *)
let head_eff t = head_teff (snd t)
  (*snd (head t)*) (* todo: to update after 
                              having added teff *)

let no_eff tv = (tv, ESet (Effset.empty))
let singleton_eff e = ESet (Effset.singleton e)


(* Returns true iff the effect type teff 
  has effect e. *)
let has_eff_e e teff = match head_teff teff with
  | ESet s -> Effset.mem e s
  | _ -> false

(* Returns true iff the typ t
  has effect e. *)
let has_eff_t ef t = has_eff_e ef (head_eff t)

let fresh_eff tv = 
  (tv, TEff (VEff.create ()))
  (*(tv, ESet Effset.empty)*)

let eff_set_t t = match head_eff t with
  | ESet s -> s
  | _ -> failwith "todo:\n"

let eff_set_e = function
  | ESet s -> s
  | _ -> failwith "todo:\n"

(* 
Returns the union of the effects e1 and e2.
Assumes both e1 and e2 are ESet.
TODO: allow effect variables in ESet so
that we can support general union. *)
let union_eff_e e1 e2 = match head_teff e1, head_teff e2 with
  | ESet s1, ESet s2 -> ESet (Effset.union s1 s2)
  | _, _ -> e2 (* arbitrary (will make sure the Div is added though) *)
  (*ESet (Effset.union (eff_set_e e1) (eff_set_e e2))*)

(* Returns the union of the effects of the
typ t1 and t2 *)
let union_eff_t t1 t2 =
  (* EUnion (head_eff t1, head_eff t2) *)
  (* ESet (Effset.union (eff_set_t t1) (eff_set_t t2)) *)
  union_eff_e (head_eff t1) (head_eff t2)

(* Converts a list of strings to a list
of effects. *)
let rec eff_from_str = function
  | [] -> ESet (Effset.empty)
  | hd::tl ->
    if hd = "div" then
      union_eff_e (singleton_eff Div) (eff_from_str tl)
    else if hd = "console" then
      union_eff_e (singleton_eff Console) (eff_from_str tl)
    else
      raise (Error ((dummy_pos, dummy_pos), "unknown effect " ^ hd ^ "."))

(* Checks if the tvar v occurs in the typ tb *)
let rec occur v tb = match head_typ tb with
  | TVar tv -> tv.id = v.id (* no rec because tv.def is None; we can use V.equal *)
  | TUnit | TInt | TBool | TString -> false 
  | TList t -> occur v t
  | TFun (args, ret) ->
      List.fold_left (fun any a -> any || occur v a) false args
    ||occur v ret
  | TMaybe t -> occur v t

(* Unifies the typ_val of t1: typ and t2: typ *)
let rec unify pos t1 t2 = match head t1, head t2 with
  | (TVar tv1, e1), (TVar tv2, e2) when tv1.id = tv2.id -> ()
  | (TVar tv1, e1), (t2, e2) -> 
    if occur tv1 (t2, e2) then raise (Error (pos, "unify: cycle"));
    tv1.def <- Some (t2, e2)
  | (t1, e1), (TVar tv2, e2) -> 
    if occur tv2 (t1, e1) then raise (Error (pos, "unify: cycle"));
    tv2.def <- Some (t1, e1)
  | (t1, e1), (t2, e2) when t1 = t2 -> ()
    (*unify_eff pos e1 e2;*)
    (* if e1 <> e2 then raise (Error (pos, "unify: effects skill issue")); *) (* x doubt *)
  | (TList t1, e1), (TList t2, e2) ->
    unify pos t1 t2;
    (* if e1 <> e2 then raise (Error (pos, "unify: effects skill issue")); *) (* x doubt *)
  | (TFun (args1, res1), e1), (TFun (args2, res2), e2) ->
    List.iter (fun (a1, a2) -> unify pos a1 a2) 
              (List.combine args1 args2);
    (* fpp_typ t1;
    fpp_typ t2;
    fpp_typ res1;
    fpp_typ res2; *)

    (* unify the effects of the return type of
       two functions. *)
    unify_eff pos (head_eff res1) (head_eff res2);
    unify pos res1 res2;
  | (TMaybe t1, e1), (TMaybe t2, e2) ->
    (*if e1 <> e2 then raise (Error (pos, "unify: effects skill issue"));*) (* x doubt *)
    unify pos t1 t2
  | _, _ -> raise (Error (pos, "unify: error."))

(* Checks if teff occurs in eff: effects *)
and occur_eff teff eff = match head_teff eff with 
  | ESet s -> false
  | TEff te -> te.eid = teff.eid

(* Unifies two effects. *)
and unify_eff pos e1 e2 = match head_teff e1, head_teff e2 with
  | TEff te1, TEff te2 when te1.eid = te2.eid -> ()
  | ESet s1, ESet s2 -> if s1 <> s2 then raise (Error (pos, "unify effect: error"))
  | e1, TEff te2 ->
    if occur_eff te2 e1 then raise (Error (pos, "unify effect: cycle."));
    te2.edef <- Some e1;
  | TEff te1, e2 ->
    if occur_eff te1 e2 then raise (Error (pos, "unify effect: cycle."));
    te1.edef <- Some e2 
(*| e1, e2 when e1 = e2 -> () *)
(*  | _, _ -> raise (Error (pos, "unsuported unify effect."))*)

(* If e is a call, it returns the id of
the function being called. *)
let get_call_id e = match e.expr with
  | ECst cst ->
    begin
      match cst.const with
        | CVar s -> Some s
        | _ -> None
    end
  | _ -> None

(* Returns eps given args -> eps/t_res*)
let get_epsilon e = 
  let () = ignore(e.texpr) in
  match head e.typ with 
    | TFun (args, res), eff ->
      head_eff (head res) (* x doubt ? todo: infer effects... *)
      (* remove the "head res" if problems *)
    | _ -> ESet Effset.empty
    (* type validity is checked later... *) 

(* Converts a kwutype (type in the AST) into
a typ. *) 
let rec kwutype_to_typ t = match t.kwutype with
  | KUnit -> no_eff TUnit
  | KType (id, dep) ->
    begin
      match id with
        | "unit" -> 
          if dep.kwutype <> KUnit then 
            raise (Error ((dummy_pos, dummy_pos), "unit<*> is invalid."));
          no_eff TUnit
        | "int" ->
          if dep.kwutype <> KUnit then 
            raise (Error ((dummy_pos, dummy_pos), "int<*> is invalid."));
          no_eff TInt
        | "string" ->
          if dep.kwutype <> KUnit then 
            raise (Error ((dummy_pos, dummy_pos), "string<*> is invalid."));
          no_eff TString
        | "bool" ->
          if dep.kwutype <> KUnit then 
            raise (Error ((dummy_pos, dummy_pos), "bool<*> is invalid."));
          no_eff TBool
        | "list" ->
          (* by default, list means list<unit>*)
          no_eff (TList (kwutype_to_typ dep))
        | _ -> raise (Error ((dummy_pos, dummy_pos), "unknown constructor for ktype."))
    end
  | KProd (dep1, dep2) ->
    raise (Error ((dummy_pos, dummy_pos), "product of types isn't supported"))
  | KFun (args, res) ->
    let rt_args = List.fold_left (
      fun ls a ->
        (kwutype_to_typ a)::ls
    ) [] args in
    let t_args = List.rev rt_args in
    let eff, res = res.result in
    let tres = kwutype_to_typ res in
    let teff = eff_from_str eff in
    (TFun (t_args, (fst tres, union_eff_e (snd tres) teff)), teff)