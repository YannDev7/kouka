open Ast
open Typed_ast
open Pp
open Lexing
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
let head_eff t = 
  snd t
  (* snd (head t) *) (* todo: to update after 
                              having added teff *)

let no_eff tv = (tv, ESet (Effset.empty))
let singleton_eff e = ESet (Effset.singleton e)

(* todo: use some sort of head_eff when we add
teff *)
let has_eff_e e = function
  | ESet s -> Effset.mem e s
  | _ -> failwith "raaah\n"

let has_eff_t ef t = has_eff_e ef (head_eff t)

let fresh_eff tv = 
  (* (tv, TEff (VEff.create ())) *)
  (tv, ESet Effset.empty)

let eff_set_t t = match head_eff t with
  | ESet s -> s
  | _ -> failwith "todo:\n"

let eff_set_e = function
  | ESet s -> s
  | _ -> failwith "todo:\n"

let union_eff_t t1 t2 =
  (* EUnion (head_eff t1, head_eff t2) *)
  ESet (Effset.union (eff_set_t t1) (eff_set_t t2))

let union_eff_e e1 e2 =
  ESet (Effset.union (eff_set_e e1) (eff_set_e e2))

let rec eff_from_str = function
  | [] -> ESet (Effset.empty)
  | hd::tl ->
    if hd = "div" then
      union_eff_e (singleton_eff Div) (eff_from_str tl)
    else if hd = "console" then
      union_eff_e (singleton_eff Console) (eff_from_str tl)
    else
      raise (Error ((dummy_pos, dummy_pos), "unknown effect " ^ hd ^ "."))

let add_to_env env id t = Idmap.add id t env

let rec occur v tb = match head_typ tb with
  | TVar tv -> tv.id = v.id (* no rec because tv.def is None; we can use V.equal *)
  | TUnit | TInt | TBool | TString -> false 
  | TList t -> occur v t
  | TFun (args, ret) ->
      List.fold_left (fun any a -> any || occur v a) false args
    ||occur v ret
  | TMaybe t -> occur v t

let rec unify pos t1 t2 = match head t1, head t2 with
  | (TVar tv1, e1), (TVar tv2, e2) when tv1.id = tv2.id -> ()
  | (TVar tv1, e1), (t2, e2) -> 
    if occur tv1 (t2, e2) then raise (Error (pos, "unify: cycle"));
    tv1.def <- Some (t2, e2)
  | (t1, e1), (TVar tv2, e2) -> 
    if occur tv2 (t1, e1) then raise (Error (pos, "unify: cycle"));
    tv2.def <- Some (t1, e1)
  | (t1, e1), (t2, e2) when t1 = t2 ->
    unify_eff pos e1 e2;
    (* if e1 <> e2 then raise (Error (pos, "unify: effects skill issue")); *) (* x doubt *)
  | (TList t1, e1), (TList t2, e2) ->
    unify pos t1 t2;
    (* if e1 <> e2 then raise (Error (pos, "unify: effects skill issue")); *) (* x doubt *)
  | (TFun (args1, res1), e1), (TFun (args2, res2), e2) ->
    List.iter (fun (a1, a2) -> unify pos a1 a2) 
              (List.combine args1 args2);
    unify_eff pos e1 e2;
    unify pos res1 res2;
  | (TMaybe t1, e1), (TMaybe t2, e2) ->
    if e1 <> e2 then raise (Error (pos, "unify: effects skill issue"));
    unify pos t1 t2
  | _, _ -> raise (Error (pos, "unify: error"))

and unify_eff pos e1 e2 =
  if e1 <> e2 then raise (Error (pos, "unify: effects skill issue"))

let get_call_id e = match e.expr with
  | ECst cst ->
    begin
      match cst.const with
        | CVar s -> Some s
        | _ -> None
    end
  | _ -> None

let rec kwutype_to_typ t = match t.kwutype with
  | KUnit -> no_eff TUnit
  | KType (id, dep) ->
    begin
      match id with
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
    (TFun (t_args, tres), teff)

let constraints = ref []
let cur_id = ref ""
let cur_res = ref (no_eff TUnit)
let divg = ref Idmap.empty
let defs = ref Idmap.empty

(* TODO: distinguish val and var *)
let rec type_const env const = match const.const with
  | CUnit -> { tconst = TCUnit; typ = no_eff TUnit }
  | CBool b -> { tconst = TCBool b; typ = no_eff TBool }
  | CInt i -> { tconst = TCInt i; typ = no_eff TInt }
  | CString s -> { tconst = TCString s; typ = no_eff TString }
  | CVar id -> 
    begin
      try 
        begin
          let ans = { tconst = TCVar id; typ = Idmap.find id env.types } in
          (* add div effect *)
          if id = !cur_id then divg := Idmap.add id true !divg;
          ans
        end
      with
        | Not_found -> raise (Error (const.pos, 
                                    "couldn't infer the type of " ^ id ^ "."))
    end

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
        typ = (TInt, union_eff_t te1.typ te2.typ) }
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
        typ = (TBool, union_eff_t te1.typ te2.typ) }
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
        typ = (TBool, union_eff_t te1.typ te2.typ) }
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
        typ = (TBool, union_eff_t te1.typ te2.typ) }
    end
  | Pplus ->
    begin
      ignore(te1.texpr); ignore(te2.texpr);
      unify pos te1.typ te2.typ;
      
      constraints := ([TString; TList (TUnit, ESet Effset.empty)], te1.typ, Error (pos, "++ only supports list and strings."))
                      ::!constraints;
      constraints := ([TString; TList (TUnit, ESet Effset.empty)], te2.typ, Error (pos, "++ only supports list and strings."))
                      ::!constraints;

      let t = ((TVar (V.create ())), union_eff_t te1.typ te2.typ) in
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
    (* do we unify the return values too ? *)
    unify exp.pos !cur_res te.typ;
    { texpr = TEReturn te;
      typ = (TVar (V.create ())), snd te.typ } (* todo: in the end, unify
                                                      with types of all the return *)
  | EIf_then_else (e1, e2, e3) ->
    let te1 = type_expr env e1 in
    let te2 = type_expr env e2 in
    let te3 = type_expr env e3 in

    constraints := ([TBool], te1.typ, Error (exp.pos, "condition doesn't evaluate to boolean."))
                   ::!constraints;

    unify exp.pos te2.typ te3.typ;
    { texpr = TEIf_then_else (te1, te2, te3);
      typ = ((head_typ te2.typ), union_eff_e (union_eff_t te1.typ te2.typ)
                                             (head_eff te3.typ)) }
  | EList ls ->
    if ls = [] then { texpr = TEList []; typ = fresh_eff (TVar (V.create ())) }
    else begin
      let tls = List.map (fun ei -> type_expr env ei) ls in
      List.iter (fun tei -> ignore(tei.texpr); (* ocaml magic... *)
                             unify exp.pos tei.typ (List.hd tls).typ)
                tls;

      let eff = List.fold_left
                  (fun cur_eff tei -> ignore(tei.texpr);
                                      union_eff_e cur_eff (head_eff tei.typ))
                  (head_eff (List.hd tls).typ) tls in
      { texpr = TEList tls; typ = (TList (List.hd tls).typ, eff) }
    end
  | ECall (e, args) ->
    let pre_def = ["println"] in
    begin
      match get_call_id e with
        | Some id when List.mem id pre_def ->
          (* made particular cases because typing the
          signature is a bit tricky + I wrote this before
          generic calls to make debugging easier. *)
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
                let print_t = (TFun ([te.typ], (TUnit, singleton_eff Console)),
                               singleton_eff Console) in
                { texpr = TECall ({
                            texpr = TECst {
                              tconst = TCVar "println";
                              typ = print_t
                            };
                            typ = (TUnit, union_eff_e (head_eff te.typ) 
                                                      (singleton_eff Console))
                          }, [te]);
                  typ = (TUnit, union_eff_t te.typ print_t) }
              | _ -> failwith "todo\n"
          end
        | Some id ->
          (try
            let ft = Idmap.find id env.types in
            if id = !cur_id then divg := Idmap.add id true !divg;
            begin
              match ft with
                | TFun (targs, res), eff ->
                  let t_args = List.map (type_expr env) args in
                  List.iter (
                    fun (a1, a2) -> 
                      ignore(a1.texpr);
                      unify exp.pos a1.typ a2
                  ) (List.combine t_args targs);

                  let uef = List.fold_left(
                    fun eff a ->
                      ignore(a.texpr);
                      union_eff_e eff (head_eff a.typ)
                  ) (ESet Effset.empty) t_args in

                  { texpr = TECall ({
                              texpr = TECst {
                                tconst = TCVar id;
                                typ = ft;
                              };
                              typ = ft;
                            }, t_args);
                    typ = (fst res, union_eff_e uef 
                                              (union_eff_e (head_eff res) eff)) }
              | _ -> raise (Error (exp.pos, id ^ " is not a function."))
            end
          with Not_found ->
            raise (Error (exp.pos, id ^ " is not declared.")))
        (* we could handle definition in the wrong order (def f: g(), def g: ())
        by using the constraints list *)
        | None -> raise (Error (exp.pos, "uncallable object was called"));
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
      {tblock = st::tb.tblock; typ = (head_typ tb.typ, union_eff_t st.typ tb.typ) }
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

and type_body env ob =
  (* TODO: generalise with type of args,
  check ident of args etc DONE *)
  let b = ob.funbody in
  (* todo: check div and check return DONE
           check infered effects included in eff DONE *)
  let te = type_expr env b.content in
  { tbody = { args = List.map (fun (a, b) -> a) b.args;
              tcontent = te }; typ = te.typ}

and type_decl env od =
  let d = od.decl in
  let body = d.body in
  let b = body.funbody in
  cur_id := d.name;
  Printf.printf "adding %s\n" d.name;

  if Idmap.mem d.name !defs then
    raise (Error (od.pos, "function " ^ d.name ^ " is defined more
                           than once."));

  if d.name = "main" && b.args <> [] then
    raise (Error (od.pos, "main function shouldn't have any arguments."));

  defs := Idmap.add d.name true !defs;

  (* add args types to env *)
  let id_rt_ls = List.fold_left (
    fun t_ls (a_id, ua_t) ->
      (* todo: create type effect here *)
      let a_t = (fresh_eff (TVar (V.create ()))) in
      unify body.pos a_t (kwutype_to_typ ua_t);
      (a_id, a_t)::t_ls
  ) [] b.args in

  let id_t_ls = List.rev id_rt_ls in
  let t_ls = List.map (fun (a, b) -> b) id_t_ls in
  let eff, res = b.tag.result in
  let t_res = fresh_eff (TVar (V.create ())) in

  (* ff is banned in effects *)
  if eff <> ["ff"] then
    unify body.pos t_res (kwutype_to_typ res);
  
  cur_res := t_res;
  
  let t_decl = (TFun (t_ls, t_res),
                if eff <> ["ff"] then eff_from_str eff
                else ESet Effset.empty) in
                
  let new_env = List.fold_left (
    fun cur_env (id, t) ->
      { cur_env with types = Idmap.add id t cur_env.types}
  ) !env ((d.name, t_decl)::id_t_ls) in

  Printf.printf "added %s\n" d.name;
  let tb = type_body new_env body in
  (* unify body.pos t_res tb.typ; *) (* x doubt *)

  (* we shout if the usr didn't add the div effect
    but he is allowed to add console and div even if useless *)
  if eff <> ["ff"] &&
    Idmap.mem !cur_id !divg && 
    not (List.mem "div" eff) then 
      raise (Error (od.pos, "function " ^ !cur_id ^ " should have effect div."));

  if eff <> ["ff"] &&
    (has_eff_t Console tb.typ)
    && not (List.mem "console" eff) then 
      raise (Error (od.pos, "function " ^ !cur_id ^ " should have effect console."));

  (* check if args have distinct names *)
  let seen = ref Idmap.empty in
  List.iter (
    fun (id, kt) ->
      if Idmap.mem id !seen then
        raise (Error (od.pos, "function " ^ !cur_id ^ " should have
                              distinct arguments names but " ^ id ^ " has
                              multiple occurences."));
      seen := Idmap.add id true !seen;
  ) b.args;
  
  let inf_eff =
    if Idmap.mem !cur_id !divg then
      union_eff_e (head_eff tb.typ) (singleton_eff Div)
    else
      head_eff tb.typ in
      
  let tfun = if eff <> ["ff"] then t_decl else (fst t_decl, inf_eff) in
  env := { !env with types = Idmap.add !cur_id tfun !env.types };

  if has_eff_t Console tfun then Printf.printf "concon\n";
  if has_eff_t Div tfun then Printf.printf "didi\n";
  { tdecl = { name = d.name; tbody = tb }; typ = tfun }

and type_file file =
  let env = ref { types = Idmap.empty; vars = Idmap.empty} in
  let rec aux = function
    | [] -> []
    | hd::tl -> let nh = (type_decl env hd) (* otherwise, wrong order *)
                in nh::aux tl
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

  if not (Idmap.mem "main" !defs) then 
    raise (Error ((dummy_pos, dummy_pos), "the file has no main."));
  ast