type ident = string
type loc = Lexing.position * Lexing.position (* TODO: add pos for errors*)

type binop =
  (* =     !=     <     <=     >     >= *)
  | Eq | Neq | Lt | Leq | Gt | Geq
  (*  +      -     *       /      %  *)
  | Add | Sub | Mul | Div | Mod
  (* &&     || *)
  | And | Or
  (*++*)
  | Pplus
(* TODO: add more *)

type atom_k =
  | AUnit
  | ABool of bool
  | AInt of int
  | AString of string
  | AVar of ident
and atom = {
  kind: atom_k;
  pos: loc
}

type expr_k =
  | ECst of atom
  | EList of expr list
  | ENot of expr
  | ETilde of expr
  | EBinop of binop * expr * expr
  | EUpdate of ident * expr
  | EReturn of expr
  | EIf_then_else of expr *  expr * expr
  | EBlock of block
  | EFn of funbody
  | ECall of expr * expr list
and expr = {
  kind: expr_k;
  pos: loc
}

and stmt_k =
  | SExpr of expr
  | SAssign of ident * expr
  | SUpdate of ident * expr
and stmt = {
  kind: stmt_k;
  pos: loc
}

and block_k = stmt list
and block = {
  kind: block_k;
  pos: loc
}

and kwutype_k =
  | KUnit
  | KType of ident * kwutype (* TODO: list or not list ? *)
  | KProd of kwutype * kwutype
  | KFun of kwutype list * result
and kwutype = {
  kind: kwutype_k;
  pos: loc
}

and result_k = ident list * kwutype
and result = {
  kind: result_k;
  pos: loc
}

and funbody = {
  args: ident list;
  tag: result;
  content: expr;
  pos: loc
}

and decl = {
  name: ident;
  body: funbody;
  pos: loc
}

and file_k = decl list
and file = {
  kind: file_k;
  pos: loc
}
