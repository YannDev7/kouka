open Lexing
open Parser
open Format
open Ast

let pp_lexbuf lb =
  let pos = lb.lex_curr_pos in
  let len = lb.lex_buffer_len - pos in
  let content = Bytes.to_string (Bytes.sub lb.lex_buffer pos len) in

  Format.print_string (Printf.sprintf "Lexbuf: len = %d, content = [\n" len);
  Format.print_string content;
  Format.print_string "\n]\n"

let rec pp_list printer fmt = function 
  | [] -> ()
  | hd::tl -> fprintf fmt "%a;%a" printer hd (pp_list printer) tl;

let rec pp_atom fmt = function
  | AInt i -> fprintf fmt "%d" i
  | _ -> fprintf fmt "atom "
and pp_binop fmt = function
  | Add -> fprintf fmt "+"
  | _ -> fprintf fmt "binop"
and pp_expr fmt = function 
  | ECst a -> fprintf fmt "%a" pp_atom a
  | EList els -> fprintf fmt "[%a]" (pp_list pp_expr) els
  | _ -> fprintf fmt "expr"
and pp_stmt fmt = function 
  | SExpr e -> fprintf fmt "%a" pp_expr e 
  | SAssign (id, e) -> fprintf fmt "%s = %a" id pp_expr e
  | SUpdate (id, e) -> fprintf fmt "%s = %a" id pp_expr e
and pp_block fmt ls =
  fprintf fmt "[%a]" (pp_list pp_stmt) ls


let pp_tok fmt = function
  | FUN -> fprintf fmt "fun "
  | IDENT s -> fprintf fmt "%s" s
  | LBRACE -> fprintf fmt "{\n"
  | RBRACE -> fprintf fmt "\n}\n"
  | LPAR -> fprintf fmt "("
  | RPAR -> fprintf fmt ")"
  | COMMA -> fprintf fmt ","
  | SEMICOLON -> fprintf fmt ";"
  | ATOM a -> pp_atom fmt a
  | NEWLINE -> fprintf fmt "\nNEWLINE\n";
  | ASSIGN -> fprintf fmt "=\n";
  | UPDATE -> fprintf fmt ":=\n";
  | EOF -> fprintf fmt "\neof\n";
  | _ -> fprintf fmt "? "