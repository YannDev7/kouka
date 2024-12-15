open Lexing
open Parser
open Format
open Ast
open Typed_ast

let pp_lexbuf lb =
  let pos = lb.lex_curr_pos in
  let len = lb.lex_buffer_len - pos in
  let content = Bytes.to_string (Bytes.sub lb.lex_buffer pos len) in

  Format.print_string (Printf.sprintf "Lexbuf: len = %d, content = [\n" len);
  Format.print_string content;
  Format.print_string "\n]\n"

let rec pp_list printer fmt = function 
  | [] -> ()
  | hd::tl -> fprintf fmt "%a; %a" printer hd (pp_list printer) tl
let pp_string fmt s =
  fprintf fmt "%s" s

let rec pp_atom fmt at = match at.const with
  | CInt i -> fprintf fmt "%d" i
  | CString s -> fprintf fmt "\"%a\"" pp_string s
  | CVar id -> pp_string fmt id
  | _ -> fprintf fmt "atom "
and pp_binop fmt = function
  | Add -> fprintf fmt "+"
  | _ -> fprintf fmt "binop"
and pp_expr fmt exp = match exp.expr with  
  | ECst a -> fprintf fmt "%a" pp_atom a
  | EList els -> fprintf fmt "[%a]" (pp_list pp_expr) els
  | EBinop (op, e1, e2) -> fprintf fmt "(%a %a %a)" pp_expr e1 pp_binop op pp_expr e2
  | EBlock b -> fprintf fmt "%a" pp_block b
  | EUpdate (id, e) -> fprintf fmt "%s := %a" id pp_expr e
  | EReturn e -> fprintf fmt "return %a\n" pp_expr e
  | EIf_then_else (e1, e2, e3) -> fprintf fmt "if (%a) then \n\t%a\n else \n\t %a" pp_expr e1 pp_expr e2 pp_expr e3
  | EFn body -> fprintf fmt "fn %a" pp_funbody body
  | ECall (e, ls) -> fprintf fmt "%a (%a)\n" pp_expr e (pp_list pp_expr) ls
  | _ -> fprintf fmt "expr"
and pp_stmt fmt stmt = match stmt.stmt with
  | SExpr e -> fprintf fmt "%a" pp_expr e 
  | SAssign (id, e) -> fprintf fmt "val %s = %a" id pp_expr e
  | SUpdate (id, e) -> fprintf fmt "var %s := %a" id pp_expr e
and pp_block fmt b =
  let ls = b.block in
  fprintf fmt "\n{\n%a\n}\n" (pp_list pp_stmt) ls
and pp_type fmt t = match t.kwutype with
  | KUnit -> fprintf fmt "()"
  | KProd (t1, t2) -> fprintf fmt "(%a) * (%a)" pp_type t1 pp_type t2
  | KType (id, kt) -> fprintf fmt "%s <%a>" id pp_type kt
  | _ -> fprintf fmt "typ"
and pp_result fmt res = match res.result with
  | ls, t -> fprintf fmt "<%a> %a" (pp_list pp_string) ls pp_type t
and pp_arg fmt (id, t) =
  fprintf fmt "(%a): %a" pp_string id pp_type t
and pp_funbody fmt body = 
  let body = body.funbody in
  fprintf fmt "(%a) %a %a\n" (pp_list pp_arg) body.args pp_result body.tag pp_expr body.content 
and pp_decl fmt f =
  let f = f.decl in
  fprintf fmt "fun %s %a\n" f.name pp_funbody f.body
and pp_file fmt ls =
  fprintf fmt "%a\n" (pp_list pp_decl) ls

let pp_tok fmt = function
  | FUN -> fprintf fmt "fun "
  | IDENT s -> fprintf fmt "%s" s
  | LBRACE -> fprintf fmt "{\n"
  | RBRACE -> fprintf fmt "\n}\n"
  | LPAR -> fprintf fmt "("
  | RPAR -> fprintf fmt ")"
  | COMMA -> fprintf fmt ","
  | SEMICOLON -> fprintf fmt ";"
  | CONST a -> pp_atom fmt a
  | NEWLINE -> fprintf fmt "\nNEWLINE\n";
  | ASSIGN -> fprintf fmt "=\n";
  | UPDATE -> fprintf fmt ":=\n";
  | EOF -> fprintf fmt "\neof\n";
  | _ -> fprintf fmt "? "

let rec pp_eff fmt = function
  | ESet s -> Effset.iter (fun x -> if x = Div then
                                      fprintf fmt "Div,"
                                    else 
                                      fprintf fmt "Console,")
                          s
  | TEff te -> 
    (match te.edef with
      | None -> fprintf fmt "t_eff"
      | Some t -> fprintf fmt "te=>(%a)" pp_eff t)

let rec pp_typ_val fmt = function
  | TUnit -> fprintf fmt "UNIT"
  | TInt -> fprintf fmt "int"
  | TBool -> fprintf fmt "bool"
  | TString -> fprintf fmt "string"
  | TList t -> fprintf fmt "list<%a>" pp_typ t
  | TFun (ls, res) ->
    fprintf fmt "fun (%a) -> %a" (pp_list pp_typ) ls pp_typ res
  | TMaybe t -> fprintf fmt "maybe<%a>" pp_typ t
  | TVar tv ->
    (match tv.def with
      | None -> fprintf fmt "t_var"
      | Some t -> fprintf fmt "tv=>(%a)" pp_typ t)
and pp_typ fmt t =
  fprintf fmt "(%a, <%a>)" pp_typ_val (fst t) pp_eff (snd t)
 
let fpp_typ t =
  fprintf (Format.std_formatter) "%a\n" pp_typ t
