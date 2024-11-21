%{
  open Ast
%}

/* Définitions des priorités et associativités des tokens */

%token EOF
%token SEMICOLON, COMMA, COLON
%token LBRACE, RBRACE
%token LPAR, RPAR
%token FUN, ARROW
%token LT, GT
%token <string> IDENT

/* Point d'entrée de la grammaire */
%start file

/* Type des valeurs renvoyées par l'analyseur syntaxique */
%type <Ast.file> file

%%

(* TODO: add SEMICOLON+ instead of SEMICOLON* *)
file:
| SEMICOLON* dl = separated_list(SEMICOLON+, decl) SEMICOLON* EOF { dl }
;

decl:
| FUN dname = ident dbody = funbody { { name = dname; body = dbody } }
;

ident:
id = IDENT { id }
;

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

typ:
| t = atyp { t }
| args_t = atyp ARROW res_t = result { KFun ([args_t], res_t) }
| LPAR args_t = separated_list(COMMA, typ) RPAR ARROW res_t = result { KFun (args_t, res_t) }
;

atyp:
| LPAR RPAR { KUnit }
| LPAR t = typ RPAR { t } 
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


expr:
| block { ECst AUnit }
| bexpr { ECst AUnit }
;

bexpr:
| EOF { ECst AUnit }
;

block:
| LBRACE  SEMICOLON* RBRACE { ECst AUnit }
;