%{
  open Ast
%}

/* Définitions des priorités et associativités des tokens */

%token EOF
%token SEMICOLON, COMMA, COLON
%token LBRACE, RBRACE
%token LPAR, RPAR
%token FUN
%token <string> IDENT

/* Point d'entrée de la grammaire */
%start file

/* Type des valeurs renvoyées par l'analyseur syntaxique */
%type <Ast.file> file

%%

file:
| SEMICOLON* dl = separated_list(SEMICOLON+, decl) EOF { dl }
;

decl:
| FUN dname = ident dbody = funbody { { name = dname; body = dbody } }
;

ident:
id = IDENT { id }
;

/* TODO: add annot */
funbody:
| LPAR fargs = separated_list(COMMA, param) RPAR fcontent = expr { { args = fargs; content = fcontent; tag = ([], KUnit) } }
;

/* TODO: COLON type */
param:
| pname = ident { pname }
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