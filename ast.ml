type ident = string
type pos = Lexing.position * Lexing.position (* TODO: add pos for errors*)

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

type _const =
  | CUnit
  | CBool of bool
  | CInt of int
  | CString of string
  | CVar of ident
and const = {
  const: _const;
  pos: pos 
}

type _expr =
  | ECst of const
  | EList of expr list (* not in const, kinda sus but yeah *)
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
  expr: _expr;
  pos: pos
}

and _stmt =
  | SExpr of expr
  | SAssign of ident * expr
  | SUpdate of ident * expr
and stmt = {
  stmt: _stmt;
  pos: pos
}

and _block = stmt list
and block = {
  block: _block;
  pos: pos
}

and _kwutype =
  | KUnit
  | KType of ident * kwutype (* TODO: list or not list ? *)
  | KProd of kwutype * kwutype
  | KFun of kwutype list * result
and kwutype = {
  kwutype: _kwutype;
  pos: pos
}

and _result = ident list * kwutype
and result = {
  result: _result;
  pos: pos
}

and _funbody = {
  args: (ident * kwutype) list;
  tag: result;
  content: expr
}
and funbody = {
  funbody: _funbody;
  pos: pos
}

and _decl = {
  name: ident;
  body: funbody
}
and decl = {
  decl: _decl;
  pos: pos
}

and file = decl list
