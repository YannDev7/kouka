type ident = string

type binop =
  (* =     !=     <     <=     >     >= *)
  | Eq | Neq | Lt | Leq | Gt | Ge
  (*  +      -     *       /      %  *)
  | Add | Sub | Mul | Div | Mod
  (* &&     || *)
  | And | Or

type atom =
  | Bool of bool
  | Int of int
  | String of string
  | Var of ident

type expr =
  | Cst of atom
  | Binop of binop * expr * expr
  | Assign of ident * expr
  | Return of expr
and stmt =
  | UwU of int
