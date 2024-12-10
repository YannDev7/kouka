open Ast
open Typed_ast
exception Error of pos * string

let no_eff tv = (tv, (false, false))

module V = struct
  type t = tvar
  let compare v1 v2 = Stdlib.compare v1.id v2.id
  let equal v1 v2 = v1.id = v2.id
  let create = let r = ref 0 in fun () -> incr r; { id = !r; def = None }
end

let rec type_const const = match const.const with
  | CUnit -> { tconst = TCUnit; typ = no_eff TUnit }
  | CBool b -> { tconst = TCBool b; typ = no_eff TBool }
  | CInt i -> { tconst = TCInt i; typ = no_eff TInt }
  | CString s -> { tconst = TCString s; typ = no_eff TString }
  | CVar id -> { tconst = TCVar id; typ = no_eff (TVar (V.create ())) }
and type_expr exp = match exp.expr with
  | ECst c -> let tc = type_const c in { texpr = TECst tc; typ = tc.typ }
  | _ -> failwith "todo\n"
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