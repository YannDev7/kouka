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

let pp_atom fmt = function
  | AInt i -> fprintf fmt "%d" i
  | _ -> fprintf fmt "atom "

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