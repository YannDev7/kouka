open Ast
open Typed_ast
open Pp
exception Error of pos * string

module Idmap = Map.Make(String)

type env = {
  types: typ Idmap.t;
  vars: bool Idmap.t
}

module V = struct
  type t = tvar
  let compare v1 v2 = Stdlib.compare v1.id v2.id
  let equal v1 v2 = v1.id = v2.id
  let create = let r = ref 0 in fun () -> incr r; { id = !r; def = None }
end

let rec head = function
  | TVar { def = Some t }, eff -> head t
  | t -> t

let head_typ t = fst (head t)
let head_eff t = snd (head t)

let no_eff tv = (tv, (false, false))

let union_eff ef1 ef2 =
  (
    ((fst ef1) || (fst ef2)), 
    ((snd ef1) || (snd ef2))
  )

let union_teff t1 t2 =
  (head_typ t2, union_eff (head_eff t1) (head_eff t2))

let add_to_env env id t = Idmap.add id t env

let rec occur v t = match head_typ t with
  | TVar tv -> tv.id = v.id (* no rec because tv.def is None; we can use V.equal *)
  | TUnit | TInt | TBool | TString -> false 
  | TList t -> occur v (no_eff t)
  | TFun (args, ret) ->
      List.fold_left (fun any a -> any || occur v (no_eff a)) false args
    ||occur v ret
  | TMaybe t -> occur v (no_eff t)

let rec unify pos t1 t2 = match head t1, head t2 with
  | (TVar tv1, e1) , (TVar tv2, e2) when tv1.id = tv2.id ->
    if e1 <> e2 then raise (Error (pos, "unify: effects skill issue"));
  | (TVar tv1, e1), (t2, e2) -> 
    if occur tv1 (t2, e2) then raise (Error (pos, "unify: cycle"));
    tv1.def <- Some (t2, e2)
  | (t1, e1), (TVar tv2, e2) -> 
    if occur tv2 (t1, e1) then raise (Error (pos, "unify: cycle"));
    tv2.def <- Some (t1, e1)
  | (t1, e1), (t2, e2) when t1 = t2 ->
    if e1 <> e2 then raise (Error (pos, "unify: effects skill issue"));
  | (TList t1, e1), (TList t2, e2) ->
    if e1 <> e2 then raise (Error (pos, "unify: effects skill issue"));
    unify pos (no_eff t1) (no_eff t2);
  | (TFun (args1, res1), e1), (TFun (args2, res2), e2) ->
    if e1 <> e2 then raise (Error (pos, "unify: effects skill issue"));
    List.iter (fun (a1, a2) -> unify pos (no_eff a1) (no_eff a2)) 
              (List.combine args1 args2);
    unify pos res1 res2
  | (TMaybe t1, e1), (TMaybe t2, e2) ->
    if e1 <> e2 then raise (Error (pos, "unify: effects skill issue"));
    unify pos (no_eff t1) (no_eff t2)
  | _, _ -> raise (Error (pos, "unify: error"))

let unify_no_eff pos t1 t2 =
  unify pos (no_eff (fst t1)) (no_eff (fst t2))

let get_call_id e = match e.expr with
  | ECst cst ->
    begin
      match cst.const with
        | CVar s -> Some s
        | _ -> None
    end
  | _ -> None

(* TODO: distinguish val and var *)
let rec type_const env const = match const.const with
  | CUnit -> { tconst = TCUnit; typ = no_eff TUnit }
  | CBool b -> { tconst = TCBool b; typ = no_eff TBool }
  | CInt i -> { tconst = TCInt i; typ = no_eff TInt }
  | CString s -> { tconst = TCString s; typ = no_eff TString }
  | CVar id -> try { tconst = TCVar id; typ = Idmap.find id env.types }
               with
                | Not_found -> raise (Error (const.pos, 
                                            "couldn't infer the type of " ^ id ^ "."))

and type_binop pos op te1 te2 = match op with
  | Add | Sub | Mul | Div | Mod ->
    begin
      (* magic fix, else ocaml screams *)
      ignore(te1.texpr); ignore(te2.texpr);
      (* theorically useless, but why not *)
      unify pos te1.typ te2.typ;

      if (head_typ te1.typ) <> TInt || (head_typ te2.typ) <> TInt then begin
        raise (Error (pos, "+-*/% require int on both sides."))
      end else { texpr = TEBinop (op, te1, te2);
                typ = union_teff te1.typ te2.typ}
    end
  | Lt | Leq | Gt | Geq ->
    begin
      ignore(te1.texpr); ignore(te2.texpr);
      unify pos te1.typ te2.typ;

      if (head_typ te1.typ) <> TInt || (head_typ te2.typ) <> TInt then begin
        raise (Error (pos, "< <= > >= require int on both sides."))
      end else 
        { texpr = TEBinop (op, te1, te2);
          typ = (TBool, union_eff (head_eff te1.typ) (head_eff te2.typ)) }
    end
  | Eq | Neq ->
    begin
      ignore(te1.texpr); ignore(te2.texpr);
      unify pos te1.typ te2.typ;

      if (head_typ te1.typ) <> (head_typ te2.typ) then
        (* theorically, should be checked by unify *)
        raise (Error (pos, "!= == require same type on both sides."))
      else if not (List.mem (head_typ te1.typ) [TInt; TBool; TString]) then
        raise (Error (pos, "!= == require the type to be int, bool or string"))
      else 
        { texpr = TEBinop (op, te1, te2);
          typ = (TBool, union_eff (head_eff te1.typ) (head_eff te2.typ)) }
    end 
  | And | Or -> 
    begin
      ignore(te1.texpr); ignore(te2.texpr);
      unify pos te1.typ te2.typ;

      if (head_typ te1.typ) <> TBool || (head_typ te2.typ) <> TBool then begin
        raise (Error (pos, "&& || require bool on both sides."))
      end else
        { texpr = TEBinop (op, te1, te2);
          typ = (TBool, union_eff (head_eff te1.typ) (head_eff te2.typ)) }
    end
  | _ -> failwith "non impl binop\n";

and type_expr env exp = match exp.expr with
  | ECst c ->
    let tc = type_const env c in
    { texpr = TECst tc; typ = tc.typ }
  | EBlock bl ->
    let tb = type_block env bl in
    { texpr = TEBlock tb; typ = tb.typ }
  | ENot e ->
    let te = type_expr env e in
    if head_typ te.typ <> TBool then
        raise (Error (exp.pos, "! require bool."));
    { texpr = TENot te; typ = te.typ }
  | ETilde e ->
    let te = type_expr env e in
    if head_typ te.typ <> TInt then
        raise (Error (exp.pos, "~ require int."));
    { texpr = TETilde te; typ = te.typ }
  | EBinop (op, e1, e2) ->
    let te1 = type_expr env e1 in
    let te2 = type_expr env e2 in
    type_binop exp.pos op te1 te2
  | EUpdate (id, e) ->
    let te = type_expr env e in
    let tv = try Idmap.find id env.types
             with 
              | Not_found -> raise (Error
                                      (exp.pos, 
                                      "occurence of " 
                                    ^ id ^ " before declaration."))
    in unify exp.pos te.typ tv;
    
    if not (Idmap.mem id env.vars) then
      raise (Error (exp.pos, id ^ " is not mutable."));

    { texpr = TEUpdate (id, te); typ = te.typ }
  | EReturn e ->
    let te = type_expr env e in
    { texpr = TEReturn te; typ = te.typ }
  | EIf_then_else (e1, e2, e3) ->
    let te1 = type_expr env e1 in
    let te2 = type_expr env e2 in
    let te3 = type_expr env e3 in
    
    if head_typ te1.typ <> TBool then
      raise (Error (exp.pos, "condition doesn't evaluate to boolean."));

    unify_no_eff exp.pos te2.typ te3.typ;
    { texpr = TEIf_then_else (te1, te2, te3);
      typ = union_teff (union_teff te1.typ te2.typ)
                        te3.typ }
  | EList ls ->
    if ls = [] then { texpr = TEList []; typ = no_eff (TVar (V.create ())) }
    else begin
      let tls = List.map (fun ei -> type_expr env ei) ls in
      if not (List.for_all 
                (fun tei -> ignore(tei.texpr); (* ocaml magic... *)
                            head_typ tei.typ = head_typ (List.hd tls).typ)
                tls) then
        raise (Error (exp.pos, "list elements must have the same type"));

      let eff = List.fold_left
                  (fun cur_eff tei -> ignore(tei.texpr);
                                      union_eff cur_eff (head_eff tei.typ))
                  (false, false) tls in
      { texpr = TEList tls; typ = (TList (head_typ (List.hd tls).typ), eff) }
    end
  | ECall (e, args) ->
    let pre_def = ["println"] in
    begin
      match get_call_id e with
        | Some id when List.mem id pre_def ->
          begin
            match id with
              | "println" ->
                if List.length args <> 1 then
                  raise (Error (exp.pos, "println requires exactly one argument."));

                let te = type_expr env (List.hd args) in
                if not (List.mem (head_typ te.typ) [TUnit; TBool; TInt; TString]) then
                  raise (Error (exp.pos, "println requires constant arguments."));

                (* probably do not care about type of Cst println *)
                { texpr = TECall ({
                            texpr = TECst {
                              tconst = TCVar "println";
                              typ = (TFun ([head_typ te.typ], no_eff TUnit), 
                                    (false, true));
                            };
                            typ = (TUnit, (false, true))
                          }, [te]);
                  typ = (TUnit, union_eff (snd te.typ) (false, true)) }
              | _ -> failwith "todo\n"
          end
        | _ -> failwith "todo\n";
    end;
  | _ -> failwith "todo\n"

and type_block env bl =
  let env = ref env in
  let bl = bl.block in
  let rec aux = function
    | [] -> { tblock = []; typ = no_eff TUnit }
    | [hd] ->
      (* we give a reference to the env, for stmt to upd it *)
      let st = type_stmt env hd in
      { tblock = [st]; typ = st.typ }
    | hd::tl ->
      let st = type_stmt env hd in
      let tb = aux tl in
      (* type of block is the type of the last stmt *)
      {tblock = st::tb.tblock; typ = union_teff st.typ tb.typ }
  in aux bl

(* note: this is essentially the base case of type_block.
it could have been included inside of the function. *)
and type_stmt env st = match st.stmt with
  | SExpr e -> let te = type_expr !env e in
              { tstmt = TSExpr te; typ = te.typ }
  | SAssign (id, e) ->
    let te = type_expr !env e in
    let tv = no_eff (TVar (V.create ())) in
    unify st.pos tv te.typ;
    env := { !env with types = Idmap.add id tv !env.types };
    { tstmt = TSAssign (id, te); typ = tv } (* x doubt *)
  | SUpdate (id, e) ->
    let te = type_expr !env e in
    let tv = no_eff (TVar (V.create ())) in
    unify st.pos tv te.typ;
    env := { types = Idmap.add id tv !env.types;
             vars = Idmap.add id true !env.vars };
    { tstmt = TSUpdate (id, te); typ = tv } (* x doubt *)

and type_body env b =
  (* TODO: generalise with type of args,
  check ident of args etc *)
  let b = b.funbody in
  let te = type_expr env b.content in
  { tbody = { args = b.args; tcontent = te }; typ = te.typ}

and type_decl env d =
  let d = d.decl in
  let body = d.body in
  let tb = type_body env body in
  { tdecl = { name = d.name; tbody = tb }; typ = tb.typ }

and type_file =
  let env = ref { types = Idmap.empty; vars = Idmap.empty} in
  let rec aux = function
    | [] -> []
    | hd::tl -> type_decl !env hd::aux tl
  in aux
