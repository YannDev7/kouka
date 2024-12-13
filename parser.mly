%{
  open Ast
  open Exception

  (* cringe case when last stmt of block is expr... *)
  let rec not_last_exp = function
    | [] -> false
    | [v] ->
      begin
        match v.stmt with
          | SExpr e -> false
          | _ -> true
      end
    | hd::tl -> not_last_exp tl

  let is_ok_block ls =
    if not_last_exp ls then raise Block_not_end_expr
    else ()

  let is_not_call exp = match exp.expr with
    | ECall (e, ls) -> false
    | _ -> true
%}

/* Définitions des priorités et associativités des tokens */

%token NEWLINE, EOF
%token SEMICOLON, COMMA, COLON, DOT
%token LBRACE, RBRACE
%token LPAR, RPAR
%token LBRACKET, RBRACKET
%token IF, THEN, ELSE, ELIF, FUN, FN, RETURN, ARROW
%token LT, LEQ, GT, GEQ
%token VAL, VAR, ASSIGN, UPDATE
%token ADD, SUB, MUL, DIV, MOD, PPLUS
%token DEQ, NEQ, NOT
%token AND, OR, TILDE
%token <string> IDENT
%token <Ast.const> CONST

/* max priority down */

// then moins prio que else et elif
// lu le plus prio = "le - prio"

%nonassoc RETURN
%nonassoc THEN
%nonassoc ELSE

%nonassoc bexpr_p

%nonassoc ASSIGN
%left OR
%left AND
%nonassoc UPDATE, DEQ, NEQ, LT, LEQ, GT, GEQ
%left ADD SUB PPLUS
%left MUL DIV MOD
%nonassoc TILDE, NOT
%nonassoc atom_p
%nonassoc LPAR
%nonassoc DOT, FN, LBRACE

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
| FUN dname = ident dbody = funbody { { decl = { name = dname; body = dbody };
                                        pos = ($startpos, $endpos) } }
;

ident:
id = IDENT { id }
;

(* MATCHING FUNCTIONS AND ARGS *)

/* TODO: add annot */
funbody:
| LPAR fargs = separated_list(COMMA, param) RPAR res_t = annot? fcontent = expr {
    match res_t with
      | None -> { funbody = { args = fargs; content = fcontent; tag = { result = (["ff"], { kwutype = KUnit; pos = ($startpos, $endpos) } );
                                                                        pos = ($startpos, $endpos) } };
                              pos = ($startpos, $endpos) }
      | Some res -> { funbody = { args = fargs; content = fcontent; tag = res };
                      pos = ($startpos, $endpos) }
  }
;

annot:
| COLON res_t = result { res_t }
;

/* TODO: COLON type */
param:
| pname = ident COLON t = typ { (pname, t) } (* do not forget arg type.. *)
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
| args_t = atyp ARROW res_t = result { { kwutype = KFun ([args_t], res_t);
                                         pos = ($startpos, $endpos) } }
| LPAR t1 = typ RPAR ARROW res_t = result { { kwutype = KFun ([t1], res_t);
                                            pos = ($startpos, $endpos) } }
| LPAR t1 = typ COMMA args_t = separated_nonempty_list(COMMA, typ) 
  RPAR ARROW res_t = result { { kwutype = KFun (t1::args_t, res_t) ;
                                          pos = ($startpos, $endpos) } }
;

atyp:
| LPAR RPAR { { kwutype = KUnit; pos = ($startpos, $endpos) } }
| id = ident t = lt_typ_gt? { { kwutype =
                                  (match t with
                                    | None -> KType (id, {kwutype = KUnit; pos = ($startpos, $endpos)})
                                    | Some st -> KType (id, st));
                              pos = ($startpos, $endpos) } }
;

lt_typ_gt:
| LT t = typ GT { t } 
;

(* TODO: add effects *)
result:
| eff = lt_ident_ls_gt? t = typ { { result =
                                      (match eff with
                                        | None -> ([], t)
                                        | Some eff -> (eff, t));
                                    pos = ($startpos, $endpos) }
                                }
;

lt_ident_ls_gt:
| LT id_ls = separated_list(COMMA, ident) GT { id_ls } 
;

(* EXPR BLOCKS STMT *)

// when in expr, read bexpr first (vs block)
expr: // magic inline
| b = block { { expr = EBlock b; pos = ($startpos, $endpos) } }
| e = bexpr %prec bexpr_p { e }
;

// when in bexpr, read atom first (vs bexpr atom)
bexpr:
| TILDE e = bexpr { { expr = ETilde e; pos = ($startpos, $endpos) } }
| NOT e = bexpr { { expr = ENot e; pos = ($startpos, $endpos)}  }
| a = atom %prec atom_p { a }
| e1 = bexpr op = binop e2 = bexpr { { expr = EBinop (op, e1, e2); pos = ($startpos, $endpos) } }
| id = ident UPDATE e = bexpr { { expr = EUpdate (id, e); pos = ($startpos, $endpos) } }
| IF e1 = bexpr THEN e2 = expr ELSE e3 = expr
  { 
    {expr = EIf_then_else (e1, e2, e3); pos = ($startpos, $endpos) }
  }
| IF e1 = bexpr THEN e2 = expr { { expr = EIf_then_else (e1, e2, { expr = EBlock ({ block = []; pos = ($startpos, $endpos) }) ; pos = ($startpos, $endpos) });
                                   pos = ($startpos, $endpos) } }
| IF e1 = bexpr RETURN e2 = expr { { expr = EIf_then_else (e1, {expr = EReturn e2; pos = ($startpos, $endpos)}, { expr = EBlock ({ block = []; pos = ($startpos, $endpos) }); pos = ($startpos, $endpos) } ); 
                                     pos = ($startpos, $endpos) }}
| FN body = funbody { { expr = EFn body;
                        pos = ($startpos, $endpos) }}
| RETURN e = expr { { expr = EReturn e; pos = ($startpos, $endpos) } }
;

(* TODO: handle string in lexer, and other atom rules *)
(* unit cringe *)
atom:
| a = CONST { { expr = ECst a; pos = ($startpos, $endpos) } }
| id = ident { { expr = ECst ({const = CVar id; pos = ($startpos, $endpos)}); pos = ($startpos, $endpos) } }
| LPAR e = expr RPAR { e }
| LPAR RPAR { {expr = ECst {const = CUnit; 
                            pos = ($startpos, $endpos)};
              pos = ($startpos, $endpos) } }
| a = atom DOT id = ident { if is_not_call a then { expr = ECall (
                                                              { 
                                                                expr = ECst ({const = CString id;
                                                                              pos = ($startpos, $endpos)
                                                                            }); 
                                                                pos = ($startpos, $endpos)
                                                              }
                                                              ,[a]
                                                            );
                                                    pos = ($startpos, $endpos) }
                            else raise Expr_is_a_call } (* TODO CHECK E NOT CALL *)
| a = atom LPAR ls = separated_list(COMMA, expr) RPAR { { expr = ECall (a, ls);
                                                          pos = ($startpos, $endpos) } }
| LBRACKET ls = separated_list(COMMA, expr) RBRACKET { { expr = EList ls;
                                                         pos = ($startpos, $endpos) } }
| a = atom b = block 
  { { 
      expr =  
        ECall (a,
          [{expr=EFn ({funbody={
              args = [];
              tag = {result=([], {kwutype = KUnit; pos = ($startpos, $endpos)});
                      pos = ($startpos, $endpos)};
              content = {expr = EBlock b; pos = ($startpos, $endpos) }
            };pos = ($startpos, $endpos)}); pos = ($startpos, $endpos)}]);
      pos = ($startpos, $endpos)
    }
  }
| e = atom FN body = funbody { {expr = ECall (e, [{ expr = EFn body; pos = ($startpos, $endpos) } ]);
                                pos = ($startpos, $endpos) } }
;

block:
| LBRACE SEMICOLON* ls = list(dst = stmt SEMICOLON+ { dst }) RBRACE { is_ok_block ls;
                                                                      { block = ls;
                                                                        pos = ($startpos, $endpos) } }
;

stmt:
| e = bexpr { { stmt = SExpr e; pos = ($startpos, $endpos) } }
| VAL id = ident ASSIGN e = expr { { stmt = SAssign (id, e); pos = ($startpos, $endpos) } }
| VAR id = ident UPDATE e = expr { { stmt = SUpdate (id, e); pos = ($startpos, $endpos) } }
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
