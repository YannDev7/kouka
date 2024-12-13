open Ast
open Typed_ast
open Pp
exception Error of pos * string

module Idmap = Map.Make(String)

(* todo: check if function is defined before calling *)

type env = {
  types: typ Idmap.t;
  vars: bool Idmap.t
}

let rec head = function
  | TVar { def = Some t }, eff -> head t
  | t -> t

let rec cannon tb = match head tb with
  | TVar t, eff -> (TVar t, eff)
  | TUnit, eff | TInt, eff | TBool, eff | TString, eff -> tb
  | TList t, eff -> (TList (cannon t), eff)
  | TFun (args, ret), eff ->
    (* be careful of args order *)
    (TFun (
      List.rev (List.fold_left (fun acc a -> cannon a::acc) args []),
      cannon ret
    ), eff)
  | TMaybe t, eff -> (TMaybe (cannon t), eff)

let head_typ t = fst (head t)
let head_eff t = snd (head t)

let no_eff tv = (tv, ESet (Effset.empty))

let fresh_eff tv = (tv, TEff (VEff.create ()))

let union_eff t1 t2 = EUnion (head_eff t1, head_eff t2)

let add_to_env env id t = Idmap.add id t env

let rec occur v tb = match head_typ tb with
  | TVar tv -> tv.id = v.id (* no rec because tv.def is None; we can use V.equal *)
  | TUnit | TInt | TBool | TString -> false 
  | TList t -> occur v t
  | TFun (args, ret) ->
      List.fold_left (fun any a -> any || occur v a) false args
    ||occur v ret
  | TMaybe t -> occur v t

let constraints = ref []

let rec unify pos t1 t2 = match head t1, head t2 with
  | (TVar tv1, e1), (TVar tv2, e2) when tv1.id = tv2.id ->
    unify_eff pos e1 e2
  | (TVar tv1, e1), (t2, e2) -> 
    if occur tv1 (t2, e2) then raise (Error (pos, "unify: cycle"));
    unify_eff pos e1 e2;
    tv1.def <- Some (t2, e2)
  | (t1, e1), (TVar tv2, e2) -> 
    if occur tv2 (t1, e1) then raise (Error (pos, "unify: cycle"));
    unify_eff pos e1 e2;
    tv2.def <- Some (t1, e1)
  | (t1, e1), (t2, e2) when t1 = t2 ->
    unify_eff pos e1 e2;
    (* if e1 <> e2 then raise (Error (pos, "unify: effects skill issue")); *) (* x doubt *)
  | (TList t1, e1), (TList t2, e2) ->
    unify_eff pos e1 e2;
    unify pos t1 t2;
    (* if e1 <> e2 then raise (Error (pos, "unify: effects skill issue")); *) (* x doubt *)
  | (TFun (args1, res1), e1), (TFun (args2, res2), e2) ->
    List.iter (fun (a1, a2) -> unify pos a1 a2) 
              (List.combine args1 args2);
    unify_eff pos e1 e2;
    unify pos res1 res2;
    if e1 <> e2 then raise (Error (pos, "unify: effects skill issue")); (* x doubt *)
  | (TMaybe t1, e1), (TMaybe t2, e2) ->
    if e1 <> e2 then raise (Error (pos, "unify: effects skill issue"));
    unify pos t1 t2
  | _, _ -> raise (Error (pos, "unify: error"))

and unify_eff pos e1 e2 =
  ()

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

      constraints := ([TInt], te1.typ, Error (pos, "+-*/% require int on both sides."))
                      ::!constraints;
      constraints := ([TInt], te2.typ, Error (pos, "+-*/% require int on both sides."))
                      ::!constraints;
      { texpr = TEBinop (op, te1, te2);
        typ = (TInt, union_eff te1.typ te2.typ) }
    end
  | Lt | Leq | Gt | Geq ->
    begin
      ignore(te1.texpr); ignore(te2.texpr);
      unify pos te1.typ te2.typ;
      
      constraints := ([TInt], te1.typ, Error (pos, "< <= > >= require int on both sides."))
                      ::!constraints;
      constraints := ([TInt], te2.typ, Error (pos, "< <= > >= require int on both sides."))
                      ::!constraints;
      { texpr = TEBinop (op, te1, te2);
        typ = (TBool, union_eff te1.typ te2.typ) }
    end
  | Eq | Neq ->
    begin
      ignore(te1.texpr); ignore(te2.texpr);
      unify pos te1.typ te2.typ;

      constraints := ([TInt; TBool; TString], te1.typ, Error (pos, "!= == require the type to be int, bool or string"))
                      ::!constraints;
      constraints := ([TInt; TBool; TString], te2.typ, Error (pos, "!= == require the type to be int, bool or string"))
                      ::!constraints;
     
      { texpr = TEBinop (op, te1, te2);
        typ = (TBool, union_eff te1.typ te2.typ) }
    end 
  | And | Or -> 
    begin
      ignore(te1.texpr); ignore(te2.texpr);
      unify pos te1.typ te2.typ;

      constraints := ([TBool], te1.typ, Error (pos, "&& || require bool on both sides."))
                      ::!constraints;
      constraints := ([TBool], te2.typ, Error (pos, "&& || require bool on both sides."))
                      ::!constraints;

      { texpr = TEBinop (op, te1, te2);
        typ = (TBool, union_eff te1.typ te2.typ) }
    end
  | Pplus ->
    begin
      ignore(te1.texpr); ignore(te2.texpr);
      unify pos te1.typ te2.typ;
      
      constraints := ([TString; TList (TUnit, ESet Effset.empty)], te1.typ, Error (pos, "++ only supports list and strings."))
                      ::!constraints;
      constraints := ([TString; TList (TUnit, ESet Effset.empty)], te2.typ, Error (pos, "++ only supports list and strings."))
                      ::!constraints;

      let t = ((TVar (V.create ())), union_eff te1.typ te2.typ) in
      unify pos t te1.typ;

      { texpr = TEBinop (op, te1, te2);
          typ = t }
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
    constraints := ([TBool], te.typ, Error (exp.pos, "! requires bool."))
                   ::!constraints;
    { texpr = TENot te; typ = te.typ }
  | ETilde e ->
    let te = type_expr env e in
    constraints := ([TInt], te.typ, Error (exp.pos, "~ requires int."))
                   ::!constraints;
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
    { texpr = TEReturn te;
      typ = fresh_eff (TVar (V.create ())) } (* todo: in the end, unify
                                                      with types of all the return *)
  | EIf_then_else (e1, e2, e3) ->
    let te1 = type_expr env e1 in
    let te2 = type_expr env e2 in
    let te3 = type_expr env e3 in

    constraints := ([TBool], te1.typ, Error (exp.pos, "condition doesn't evaluate to boolean."))
                   ::!constraints;

    unify exp.pos te2.typ te3.typ;
    { texpr = TEIf_then_else (te1, te2, te3);
      typ = ((head_typ te2.typ), EUnion (union_eff te1.typ te2.typ,
                        head_eff te3.typ)) }
  | EList ls ->
    if ls = [] then { texpr = TEList []; typ = fresh_eff (TVar (V.create ())) }
    else begin
      let tls = List.map (fun ei -> type_expr env ei) ls in
      List.iter (fun tei -> ignore(tei.texpr); (* ocaml magic... *)
                             unify exp.pos tei.typ (List.hd tls).typ)
                tls;

      let eff = List.fold_left
                  (fun cur_eff tei -> ignore(tei.texpr);
                                      EUnion (cur_eff, (head_eff tei.typ)))
                  (head_eff (List.hd tls).typ) tls in
      { texpr = TEList tls; typ = (TList (List.hd tls).typ, eff) }
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
                constraints := ([TUnit; TBool; TInt; TString], 
                                te.typ,
                                Error (exp.pos, "println requires constant arguments."))
                            ::!constraints;

                (* probably do not care about type of Cst println *)
                let print_t = (TFun ([te.typ], (TUnit, ESet (Effset.singleton Console))),
                               ESet (Effset.singleton Console)) in
                { texpr = TECall ({
                            texpr = TECst {
                              tconst = TCVar "println";
                              typ = print_t
                            };
                            typ = (TUnit, ESet (Effset.singleton Console))
                          }, [te]);
                  typ = (TUnit, union_eff te.typ print_t) }
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
      {tblock = st::tb.tblock; typ = (head_typ tb.typ, union_eff st.typ tb.typ) }
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

and type_file file =
  let env = ref { types = Idmap.empty; vars = Idmap.empty} in
  let rec aux = function
    | [] -> []
    | hd::tl -> type_decl !env hd::aux tl
  in
  let ast = aux file in
  List.iter (
    fun (cons, t, err) ->
      begin
        match head_typ t with
          | TList tau ->
            if not (List.mem (TList (TUnit, ESet Effset.empty)) cons) then
              raise err;
          | ty ->
            if not (List.mem (head_typ t) cons) then
              raise err;
      end
  ) !constraints;

  ast