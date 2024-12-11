open Ast
open Typed_ast
open Pp
exception Error of pos * string

let no_eff tv = (tv, (false, false))

let union_eff ef1 ef2 =
  (
    ((fst ef1) || (fst ef2)), 
    ((snd ef1) || (snd ef2))
  )

let union_teff t1 t2 =
  (fst t2, union_eff (snd t1) (snd t2))

module V = struct
  type t = tvar
  let compare v1 v2 = Stdlib.compare v1.id v2.id
  let equal v1 v2 = v1.id = v2.id
  let create = let r = ref 0 in fun () -> incr r; { id = !r; def = None }
end

(* TODO: distinguish val and var *)
let rec type_const const = match const.const with
  | CUnit -> { tconst = TCUnit; typ = no_eff TUnit }
  | CBool b -> { tconst = TCBool b; typ = no_eff TBool }
  | CInt i -> { tconst = TCInt i; typ = no_eff TInt }
  | CString s -> { tconst = TCString s; typ = no_eff TString }
  | CVar id -> { tconst = TCVar id; typ = no_eff (TVar (V.create ())) }
and type_expr exp = match exp.expr with
  | ECst c ->
    let tc = type_const c in
    { texpr = TECst tc; typ = tc.typ }
  | EBlock bl ->
    let tb = type_block bl in
    { texpr = TEBlock tb; typ = tb.typ }
  | _ -> failwith "todo\n"
and type_block bl =
  let bl = bl.block in
  let rec aux = function
    | [] -> { tblock = []; typ = no_eff TUnit }
    | [hd] ->
      let st = type_stmt hd in
      { tblock = [st]; typ = st.typ }
    | hd::tl ->
      let st = type_stmt hd in
      let tb = aux tl in
      (* type of block is the type of the last stmt *)
      {tblock = st::tb.tblock; typ = union_teff st.typ tb.typ }
  in aux bl
and type_stmt st = match st.stmt with
  | SExpr e -> let te = type_expr e in
              { tstmt = TSExpr te; typ = te.typ }
  | _ -> failwith "todo stmt\n"
and type_body b =
  (* TODO: generalise with type of args,
  check ident of args etc *)
  let b = b.funbody in
  let te = type_expr b.content in
  { tbody = { args = b.args; tcontent = te }; typ = te.typ}

and type_decl d =
  let d = d.decl in
  let body = d.body in
  let tb = type_body body in
  { tdecl = { name = d.name; tbody = tb }; typ = tb.typ }
and type_file = function
  | [] -> []
  | hd::tl -> type_decl hd::type_file tl