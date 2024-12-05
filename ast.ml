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

type atom =
  | AUnit
  | ABool of bool
  | AInt of int
  | AString of string
  | AVar of ident

type expr =
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

and stmt =
  | SExpr of expr
  | SAssign of ident * expr
  | SUpdate of ident * expr

and block = stmt list

and kwutype =
  | KUnit
  | KType of ident * kwutype (* TODO: list or not list ? *)
  | KProd of kwutype * kwutype
  | KFun of kwutype list * result

and result = ident list * kwutype

and funbody = {
  args: ident list;
  tag: result;
  content: expr
}

and decl = {
  name: ident;
  body: funbody
}

and file = decl list
