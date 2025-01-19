(* phase 1 : allocation des variables *)
open Alloc_ast
open Typed_ast
open Typing_utils
open Codegen_utils
(* 
module Idmap = Map.Make(String)

type alloc_env = {
  local_env: int Idmap.t;
  clos_env: int Idmap.t
} *)

let defined_functions = Hashtbl.create 16
let total_decalage = ref 0

let rec alloc_const fpcur env c = 
  ignore(c.tconst);
  let wrap_const _const =
    { aconst = _const; typ = c.typ } in
  match c.tconst with
    | TCUnit -> wrap_const ACUnit
    | TCBool b -> wrap_const (ACBool b)
    | TCInt i -> wrap_const (ACInt i)
    | TCString s -> wrap_const (ACString s)
    | TCVar id -> 
      try
        let x = Idmap.find id (env.local_env) in
        wrap_const (ACVar (Vlocal x))
      with _ ->
        try
        wrap_const (ACVar (Vclos (Idmap.find id env.clos_env)))
        with _ -> 
          wrap_const (ACVar (Vglobal id))

and alloc_expr fpcur env c =
  total_decalage := fpcur;
  ignore(c.texpr);
  let wrap_expr _expr =
    { aexpr = _expr; typ = c.typ } in
  match c.texpr with
    | TECst c -> wrap_expr (AECst (alloc_const fpcur env c))
    | TEList ls ->
      wrap_expr (AEList (
                  List.rev(
                    List.fold_left (fun acc e -> 
                                    (alloc_expr fpcur env e)::acc) 
                                  [] ls
                  )
                ))
    | TENot e -> wrap_expr (AENot (alloc_expr fpcur env e))
    | TETilde e -> wrap_expr (AETilde (alloc_expr fpcur env e))
    | TEBinop (op, a, b) ->
      wrap_expr (AEBinop (op, alloc_expr fpcur env a,
                              alloc_expr fpcur env b))
    | TEUpdate (id, e) ->
      wrap_expr (AEUpdate (Idmap.find id env.local_env, alloc_expr fpcur env e))
    | TEReturn e -> wrap_expr (AEReturn (alloc_expr fpcur env e))
    | TEIf_then_else (e_if, e_then, e_else) ->
      wrap_expr (AEIf_then_else(alloc_expr fpcur env e_if,
                                alloc_expr fpcur env e_then,
                                alloc_expr fpcur env e_else))
    | TEBlock b ->
      wrap_expr (AEBlock (alloc_block fpcur env b))
    | TEFn b ->
      let new_env = {
        local_env = Idmap.empty;
        clos_env = Idmap.union (fun id x y -> Some(x)) env.local_env env.clos_env
      } in
      wrap_expr (AEClos (alloc_body fpcur new_env b))
    | TECall (f, exp_list) ->
      begin
        match tget_call_id f with 
        | Some s when s = "println" ->
          (* On traite le cas particulier de println *)
          begin
          match exp_list with
            | e::[] ->
              begin
              match e.texpr with
              (* En fonction du type du paramètre, on n'appelle pas 
              la même fonction print *)
                | TECst c ->
                  wrap_expr (compute_const c env)
                | TEBinop (op, a, b) ->
                  begin 
                  match op with
                  | Add | Sub | Mul | Div | Mod ->
                  let computed_binop = alloc_expr fpcur env e in
                  wrap_expr (AECst {
                    aconst = ACallPrintInt (computed_binop);
                    typ = (TUnit, singleton_eff Div)
                  })
                  | Eq | Neq | Lt | Leq | Gt | Geq | And | Or ->
                    let computed_binop = alloc_expr fpcur env e in
                  wrap_expr (AECst {
                    aconst = ACallPrintBool (computed_binop);
                    typ = (TUnit, singleton_eff Div)
                  })
                  | Pplus ->
                    let computed_binop = alloc_expr fpcur env e in
                  wrap_expr (AECst {
                    aconst = ACallPrintString (computed_binop);
                    typ = (TUnit, singleton_eff Div)
                  })
                  end
                | TENot e ->
                  let computed_e = alloc_expr fpcur env e in
                  let not_computed_e = {
                    aexpr = AENot computed_e;
                    typ = computed_e.typ
                  } in
                  wrap_expr (AECst {
                    aconst = ACallPrintBool (not_computed_e);
                    typ = (TUnit,singleton_eff Div)
                  })
                | TETilde e ->
                  let computed_e = alloc_expr fpcur env e in
                  let tilde_e = {
                    aexpr = AETilde computed_e;
                    typ = computed_e.typ
                  } in
                  wrap_expr (AECst {
                    aconst = ACallPrintInt (tilde_e);
                    typ = tilde_e.typ
                  })
                | TECall (f, b) ->
                  wrap_expr (AECall
                  (alloc_expr fpcur env f,
                   List.map (fun x -> alloc_expr fpcur env x) b))
                | _ -> failwith "invalid parameter of println"
              end
            | _ -> failwith "wrong number of parameters for println"
          end
        | Some s ->
          wrap_expr (AECall (
            alloc_expr fpcur env f
          , List.map (fun x -> alloc_expr fpcur env x) exp_list))
        | None -> failwith "Function expression is not correct"
      end

and alloc_block fpcur env c =
  let env = ref env in
  let wrap_block _block =
    { ablock = _block; typ = c.typ } in 
  let _block =
    let rec aux cc = match cc with
      | [] -> []
      | hd::tl -> 
        let allowed, new_env = (alloc_stmt fpcur !env hd) in
        env := new_env;
        allowed::(aux tl)
    in aux c.tblock
  in wrap_block _block
      
and alloc_stmt fpcur env c =
  let wrap_stmt _stmt =
    { astmt = _stmt; typ = c.typ } in 
  match c.tstmt with
    | TSExpr e -> wrap_stmt (ASExpr (alloc_expr fpcur env e)), env
    | TSAssign (id, e) ->
      let new_env_loc = Idmap.add id (fpcur - 8) env.local_env in
      total_decalage := !total_decalage + 8;
      let new_env = {
        local_env = new_env_loc;
        clos_env = env.clos_env
      } in
      total_decalage := !total_decalage + 8;
      wrap_stmt (ASAssign (fpcur - 8,
                          alloc_expr (fpcur - 8) new_env e)), new_env
    | TSUpdate (id, e) ->
      let new_env_loc = Idmap.add id (fpcur - 8) env.local_env in
      total_decalage := !total_decalage + 8;
      let new_env = {
        local_env = new_env_loc;
        clos_env = env.clos_env
      } in
      total_decalage := !total_decalage + 8;
      wrap_stmt (ASUpdate (fpcur - 8,
                          alloc_expr (fpcur - 8) new_env e)), new_env
and alloc_body fpcur env (c: tfunbody) =
  let wrap_body _body =
    { abody = _body; typ = c.typ } in 
  
  let new_env_loc, delta = List.fold_left
                  (fun (acc, delta) id ->
                    (Idmap.add id (fpcur + delta) acc, delta + 8))
                  (env.local_env, 16)
                  c.tbody.args in
  total_decalage := !total_decalage + 8*(List.length c.tbody.args);
  let new_env = {
    local_env = new_env_loc;
    clos_env = Idmap.empty
  } in
  let _content = alloc_expr (fpcur + delta) new_env 
                            c.tbody.tcontent in
  let _body = { acontent = _content; args = c.tbody.args } in
  wrap_body _body

and alloc_decl fpcur env c =
  let wrap_decl _decl =
    { adecl = _decl; typ = c.typ } in
  let _body = alloc_body fpcur env c.tdecl.tbody in
  let _decl = { abody = _body; name = c.tdecl.name } in
  wrap_decl _decl

and alloc_file ls =
  List.rev(
    List.fold_left
      (fun acc decl -> 
        let allocated_decl = (alloc_decl 0 {
          local_env = Idmap.empty;
          clos_env = Idmap.empty
        } decl) in
       Hashtbl.add defined_functions decl.tdecl.name allocated_decl;
       allocated_decl::acc)
      [] ls
  )