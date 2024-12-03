%{
  open Ast
  open Exception

  (* cringe case when last stmt of block is expr... *)
  let rec not_last_exp = function
    | [] -> false
    | [v] ->
      begin
        match v with
          | SExpr e -> false
          | _ -> true
      end
    | hd::tl -> not_last_exp tl

  let is_ok_block ls =
    if not_last_exp ls then raise Block_not_end_expr
    else ()
%}

/* Définitions des priorités et associativités des tokens */

%token NEWLINE, EOF
%token SEMICOLON, COMMA, COLON, DOT
%token LBRACE, RBRACE
%token LPAR, RPAR
%token IF, THEN, ELIF, ELSE, FUN, FN, ARROW
%token LT, LEQ, GT, GEQ
%token VAL, VAR, ASSIGN, UPDATE
%token ADD, SUB, MUL, DIV, MOD, PPLUS
%token DEQ, NEQ
%token AND, OR
%token <string> IDENT
%token <Ast.atom> ATOM

/* max priority down */

// then moins prio que else et elif
%left OR
%left AND
%nonassoc UPDATE, DEQ, NEQ, LT, LEQ, GT, GEQ
%left ADD SUB PPLUS
%left MUL DIV MOD

/* Point d'entrée de la grammaire */
%start file

/* Type des valeurs renvoyées par l'analyseur syntaxique */
%type <Ast.file> file

%%

(* TODO: add SEMICOLON+ instead of SEMICOLON* *)
file:
| SEMICOLON* dl = list(de = decl SEMICOLON+ { de }) EOF { dl }
;

decl:
| FUN dname = ident dbody = funbody { { name = dname; body = dbody } }
;

ident:
id = IDENT { id }
;

(* MATCHING FUNCTIONS AND ARGS *)

/* TODO: add annot */
funbody:
| LPAR fargs = separated_list(COMMA, param) RPAR res_t = annot? fcontent = expr {
    match res_t with
      | None -> { args = fargs; content = fcontent; tag = ([], KUnit) }
      | Some res -> { args = fargs; content = fcontent; tag = res }
  }
;

annot:
| COLON res_t = result { res_t }
;

/* TODO: COLON type */
param:
| pname = ident COLON typ { pname }
;


(* conflicts between (atyp) -> res vs (typ ls) -> res
when ls of size 1... 

idea: type list ? 
sol: we retard reading the comma!
if we read at least one type and then something with comma*)

(*
conflict with (typ) via atyp -> or not ->?
=> ltyp reads whatever is on the left
and then we have ltyp ->

we match (typ, non empty list)
we make common prefix in same rule
*)

typ:
| t = atyp { t }
| LPAR t = typ RPAR { t } 
| args_t = atyp ARROW res_t = result { KFun ([args_t], res_t) }
| LPAR t1 = typ RPAR ARROW res_t = result { KFun ([t1], res_t) }
| LPAR t1 = typ COMMA args_t = separated_nonempty_list(COMMA, typ) RPAR ARROW res_t = result { KFun (t1::args_t, res_t) }
;

atyp:
| LPAR RPAR { KUnit }
| id = ident t = lt_typ_gt? { match t with
                                | None -> KType (id, KUnit)
                                | Some st -> KType (id, st)
                            }
;

lt_typ_gt:
| LT t = typ GT { t } 
;

(* TODO: add effects *)
result:
| eff = lt_ident_ls_gt? t = typ { match eff with
                                | None -> ([], t)
                                | Some eff -> (eff, t)
                              }
;

lt_ident_ls_gt:
| LT id_ls = separated_list(COMMA, ident) GT { id_ls } 
;

(* EXPR BLOCKS STMT *)

expr:
| b = block { EBlock b }
| e = bexpr { e }
;

bexpr:
| a = atom { a }
| e1 = bexpr op = binop e2 = bexpr { EBinop (op, e1, e2) }
(*| IF e1 = bexpr then e2 = expr*) 
;

(* TODO: handle string in lexer, and other atom rules *)
(* unit cringe *)
atom:
| a = ATOM { ECst a }
| id = ident { ECst (AVar id) }
| LPAR RPAR { ECst AUnit }
| a = atom LPAR ls = separated_list(COMMA, expr) RPAR { ECall (a, ls) }
;

block:
| LBRACE SEMICOLON* ls = list(dst = stmt SEMICOLON+ { dst }) RBRACE { is_ok_block ls;
                                                                      ls }
;

stmt:
| e = bexpr { SExpr e }
| VAL id = ident ASSIGN e = expr { SAssign (id, e) }
| VAR id = ident UPDATE e = expr { SUpdate (id, e) }
;

%inline binop:
| DEQ { Eq }
| NEQ { Neq }
| LT { Lt }
| LEQ { Leq }
| GT { Gt }
| GEQ { Geq }
| ADD { Add }
| SUB { Sub }
| MUL { Mul }
| DIV { Div }
| MOD { Mod }
| AND { And }
| OR { Or }
| PPLUS { Pplus }
