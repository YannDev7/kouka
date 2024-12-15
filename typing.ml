open Ast
open Typed_ast
open Pp
open Lexing
open Exception
open Typing_utils

let constraints = ref []

let cnt_ano = ref 0

let cur_id = ref ""
let b_id = ""
let b_res = ref (no_eff TUnit)

let cur_ids = Stack.create ()
let cur_ress = Stack.create ()

let () =
  Stack.push b_id cur_ids;
  Stack.push b_res cur_ress

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
          if id = Stack.top cur_ids then divg := Idmap.add id true !divg;
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

    { texpr = TEUpdate (id, te); typ = (TUnit, snd te.typ) }
  | EReturn e ->
    let te = type_expr env e in
    (* do we unify the return values too ? *)
    unify exp.pos !(Stack.top cur_ress) te.typ;
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
    let pre_def = ["println"; "repeat"; "while"; "for"; "head"; "tail"; "default"] in
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
              | "repeat" ->
                if List.length args <> 2 then
                  raise (Error (exp.pos, "repeat requires two arguments."));

                let te1 = type_expr env (List.hd args) in
                constraints := ([TInt], te1.typ,
                                Error (exp.pos, "repeat requires an int as
                                                 first argument."))
                            ::!constraints;
                
                let te2 = type_expr env (List.nth args 1) in
                constraints := ([TFun ([], no_eff TUnit)], te2.typ,
                                Error (exp.pos, "repeat requires () -> () as
                                                 second argument."))
                            ::!constraints;

                let eps = get_epsilon te2 in

                let repeat_t = (TFun ([te1.typ; te2.typ], (TUnit, ESet Effset.empty)),
                               ESet Effset.empty) in
                let ueff = union_eff_e (head_eff te1.typ) 
                                       (union_eff_e (head_eff te2.typ)
                                                    eps) in
                { texpr = TECall ({
                            texpr = TECst {
                              tconst = TCVar "repeat";
                              typ = repeat_t
                            };
                            typ = (TUnit, ueff)
                          }, [te1; te2]);
                  typ = (TUnit, ueff) }
              | "while" ->
                if List.length args <> 2 then
                  raise (Error (exp.pos, "while requires two arguments."));

                let te1 = type_expr env (List.hd args) in
                constraints := ([TFun ([], no_eff TBool)], te1.typ,
                                Error (exp.pos, "repeat requires () -> bool as
                                                  first argument."))
                            ::!constraints;
                
                let te2 = type_expr env (List.nth args 1) in
                constraints := ([TFun ([], no_eff TUnit)], te2.typ,
                                Error (exp.pos, "while requires () -> () as
                                                  second argument."))
                            ::!constraints;
                
                let eps3 = get_epsilon te1 in
                let eps4 = get_epsilon te2 in

                let while_t = (TFun ([te1.typ; te2.typ], (TUnit, singleton_eff Div)),
                                ESet Effset.empty) in
                let ueff = union_eff_e (head_eff te1.typ) 
                                        (union_eff_e (head_eff te2.typ)
                                                      (union_eff_e eps3
                                                                   (union_eff_e eps4
                                                                                (singleton_eff Div)))) in
                { texpr = TECall ({
                            texpr = TECst {
                              tconst = TCVar "while";
                              typ = while_t
                            };
                            typ = (TUnit, ueff)
                          }, [te1; te2]);
                  typ = (TUnit, ueff) }
              | "for" ->
                if List.length args <> 3 then
                  raise (Error (exp.pos, "for requires two arguments."));

                let te1 = type_expr env (List.hd args) in
                constraints := ([TInt], te1.typ,
                                Error (exp.pos, "for requires int as
                                                  first argument."))
                            ::!constraints;
                
                let te2 = type_expr env (List.nth args 1) in
                constraints := ([TInt], te2.typ,
                                Error (exp.pos, "for requires int as
                                                  second argument."))
                            ::!constraints;

                let te3 = type_expr env (List.nth args 2) in
                constraints := ([TFun ([no_eff TInt], no_eff TUnit)], te3.typ,
                                Error (exp.pos, "for requires (int) -> () as
                                                  third argument."))
                            ::!constraints;
                
                let eps = get_epsilon te3 in

                let for_t = (TFun ([te1.typ; te2.typ; te3.typ], (TUnit, ESet Effset.empty)),
                                ESet Effset.empty) in
                let ueff = union_eff_e (head_eff te1.typ) 
                                        (union_eff_e (head_eff te2.typ)
                                                      (union_eff_e
                                                                  (head_eff te3.typ)
                                                                   eps)) in
                { texpr = TECall ({
                            texpr = TECst {
                              tconst = TCVar "for";
                              typ = for_t
                            };
                            typ = (TUnit, ueff)
                          }, [te1; te2; te3]);
                  typ = (TUnit, ueff) }
              | "head" ->
                if List.length args <> 1 then
                  raise (Error (exp.pos, "head requires exactly one argument."));

                let te = type_expr env (List.hd args) in
                let tau = no_eff (TVar (V.create ())) in
                constraints := ([TList tau], 
                                te.typ,
                                Error (exp.pos, "head requires list<tau> as argument."))
                            ::!constraints;

                let head_t = (TFun ([te.typ],  (TMaybe (tau), head_eff te.typ)),
                              ESet Effset.empty) in
                { texpr = TECall ({
                            texpr = TECst {
                              tconst = TCVar "head";
                              typ = head_t
                            };
                            typ = (TMaybe (tau), head_eff te.typ)
                          }, [te]);
                  typ = (TMaybe (tau), union_eff_t te.typ head_t) }
              | "tail" ->
                if List.length args <> 1 then
                  raise (Error (exp.pos, "tail requires exactly one argument."));

                let te = type_expr env (List.hd args) in
                let tau = no_eff (TVar (V.create ())) in
                constraints := ([TList tau], 
                                te.typ,
                                Error (exp.pos, "tail requires list<tau> as argument."))
                            ::!constraints;

                let tail_t = (TFun ([te.typ],  (TList (tau), head_eff te.typ)),
                              ESet Effset.empty) in
                { texpr = TECall ({
                            texpr = TECst {
                              tconst = TCVar "tail";
                              typ = tail_t
                            };
                            typ = (TList (tau), head_eff te.typ)
                          }, [te]);
                  typ = (TList (tau), union_eff_t te.typ tail_t) }
              | "default" ->
                if List.length args <> 2 then
                  raise (Error (exp.pos, "default requires exactly two arguments."));

                let te1 = type_expr env (List.hd args) in
                let te2 = type_expr env (List.nth args 1) in

                constraints := ([TMaybe te2.typ], 
                                te1.typ,
                                Error (exp.pos, "default requires maybe<tau> and tau as arguments."))
                            ::!constraints;

                let eff = (union_eff_t te1.typ te2.typ) in
                let default_t = (TFun ([te1.typ; te2.typ], (head_typ te2.typ, eff)),
                              ESet Effset.empty) in
                { texpr = TECall ({
                            texpr = TECst {
                              tconst = TCVar "default";
                              typ = default_t
                            };
                            typ = (head_typ te2.typ, eff)
                          }, [te1; te2]);
                  typ = (head_typ te2.typ, eff) }
              | _ -> failwith "todo\n"
          end
        | Some id ->
          (try
            let ft = Idmap.find id env.types in
            if id = Stack.top cur_ids then divg := Idmap.add id true !divg;
            if id = !cur_id then divg := Idmap.add id true !divg;

            begin
              match head ft with
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
        | None -> 
          pp_expr Format.std_formatter e;
          raise (Error (exp.pos, "uncallable object was called"));
    end;
  | EFn cbody ->
    (* todo:handle div effect when def f: fn () {f()}
            handle ff effect vs user effects 
             currently assume ff *)
    let new_name = "-ano" ^ (string_of_int !cnt_ano) in
    incr cnt_ano;

    let decl = {
      decl = {
        name = new_name;
        body = cbody
      };
      pos = exp.pos
    } in 
    let t_decl = type_decl env decl in
    {
      texpr = TEFn t_decl.tdecl.tbody;
      typ = no_eff (fst t_decl.typ)
    }
  | _ -> failwith "todooo\n"

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
  
  Stack.push d.name cur_ids;
  (*Printf.printf "adding %s\n" d.name;*)

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
  let t_res = ref (fresh_eff (TVar (V.create ()))) in

  (* ff is banned in effects *)
  if eff <> ["ff"] then begin
    (*Printf.printf "using user sig for %s\n" d.name;*)
    unify body.pos !t_res (kwutype_to_typ res);
    t_res := (fst !t_res, eff_from_str eff);
  end;
  
  Stack.push t_res cur_ress;
  
  let t_decl = (TFun (t_ls, !t_res),
                if eff <> ["ff"] then eff_from_str eff
                else TEff (VEff.create ())) in

  let new_env = List.fold_left (
    fun cur_env (id, t) ->
      { cur_env with types = Idmap.add id t cur_env.types}
  ) env ((d.name, t_decl)::id_t_ls) in

  (*Printf.printf "added %s\n" d.name;*)
  (*fpp_typ t_decl;*)
  let tb = type_body new_env body in

  (*Printf.printf "BEGIN tyty %s\n" d.name;
  fpp_typ !t_res;
  fpp_typ tb.typ;*)

  unify body.pos !t_res tb.typ; (* x doubt *)

  (*fpp_typ !t_res;
  fpp_typ (cannon t_decl);
  Printf.printf "END tyty %s\n" d.name;*)

  (* we shout if the usr didn't add the div effect
    but he is allowed to add console and div even if useless *)
  if eff <> ["ff"] &&
    (Idmap.mem d.name !divg || has_eff_t Div tb.typ) && 
    not (List.mem "div" eff) then 
      raise (Error (od.pos, "function " ^ d.name ^ " should have effect div."));

  if eff <> ["ff"] &&
    (has_eff_t Console tb.typ)
    && not (List.mem "console" eff) then 
      raise (Error (od.pos, "function " ^ d.name ^ " should have effect console."));

  (* check if args have distinct names *)
  let seen = ref Idmap.empty in
  List.iter (
    fun (id, kt) ->
      if Idmap.mem id !seen then
        raise (Error (od.pos, "function " ^ d.name ^ " should have
                              distinct arguments names but " ^ id ^ " has
                              multiple occurences."));
      seen := Idmap.add id true !seen;
  ) b.args;
  
  let inf_eff =
    if Idmap.mem d.name !divg then
      union_eff_e (head_eff tb.typ) (singleton_eff Div)
    else
      head_eff tb.typ in

  let tfun = if eff <> ["ff"] then t_decl else (fst t_decl, inf_eff) in

  (*if has_eff_t Console tfun then Printf.printf "concon\n";
  if has_eff_t Div tfun then Printf.printf "didi\n";*)
  
  ignore(Stack.pop cur_ids);
  ignore(Stack.pop cur_ress);

  { tdecl = { name = d.name; tbody = tb }; typ = tfun }

and type_file file =
  (*fpp_typ (cannon (no_eff (TFun ([no_eff 
    (TVar ({ id = 1; def = Some (no_eff TInt) }))], 
  (TInt, singleton_eff Div)))));*)

  let env = ref { types = Idmap.empty; vars = Idmap.empty} in
  let rec aux = function
    | [] -> []
    | hd::tl -> 
                cur_id := hd.decl.name;
                let nh = (type_decl !env hd) in (* otherwise, wrong order *)
                env := { !env with types = Idmap.add hd.decl.name nh.typ !env.types };
                nh::aux tl
  in
  let ast = aux file in
  let resolve_const p_err =
    List.iter (
      fun (cons, t, err) ->
        (*pp_typ Format.std_formatter t;*)
        (*fpp_typ (cannon t);*)
        begin
          match head_typ (cannon t) with
            | TFun ([], (TUnit, _)) ->
              if p_err && (not (List.mem (TFun ([], no_eff TUnit)) cons)) then
                raise err;
            | TFun ([], (TBool, _)) ->
              if p_err && (not (List.mem (TFun ([], no_eff TBool)) cons)) then
                raise err;
            | TFun([TInt, _], (TUnit, _)) ->
              if p_err && (not (List.mem (TFun ([no_eff TInt], no_eff TUnit)) cons)) then
                raise err;
            | TList tau ->
              (*fpp_typ (cannon t);*)
              let cnt = ref 0 in
              List.iter (
                fun t ->
                  begin 
                    match t with
                      | TList ((TUnit, _)) -> incr cnt
                      | TList tau2 -> (*fpp_typ (no_eff (TList tau2));*)
                                      unify (get_pos err) tau tau2;
                                      incr cnt;
                      | _ -> ()
                  end
              ) cons;

              if p_err && !cnt = 0 then raise err;
            | TMaybe tau ->
              (*fpp_typ (cannon t);*)
              let cnt = ref 0 in
              List.iter (
                fun t ->
                  begin 
                    match t with
                      | TMaybe tau2 -> (*fpp_typ (no_eff (TMaybe tau2));*)
                                      unify (get_pos err) tau tau2;
                                      incr cnt;
                      | _ -> ()
                  end
              ) cons;

              if p_err && !cnt = 0 then raise err;
            | ty ->
              (*Printf.printf "wannnnt\n";
              fpp_typ (cannon (no_eff ty));*)
              if p_err && (not (List.mem (head_typ (cannon t)) cons)) then
                raise err;
        end
    ) !constraints in

  resolve_const false;
  resolve_const true;

  if not (Idmap.mem "main" !defs) then 
    raise (Error ((dummy_pos, dummy_pos), "the file has no main."));
  ast