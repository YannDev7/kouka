open Lexing
open Ast

exception Block_not_end_expr
exception Expr_is_a_call
exception Error of pos * string

let get_pos = function
  | Error (p, s) -> p
  | _ -> (dummy_pos, dummy_pos)