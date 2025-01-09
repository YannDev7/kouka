(* phase 1 : allocation des variables *)
open Ast
open Typed_ast

type _aconst =
  | ACUnit
  | ACBool of bool
  | ACInt of int
  | ACString of string
  | ACVar of avar

  | APrintInt
  | ACallPrintInt of int
and avar =
  | Vlocal of int
  | Vglobal of ident
  | Vclos of int
  | Varg
and aconst = {
  aconst: _aconst;
  typ: typ
}

and _aexpr =
  | AECst of aconst
  | AEList of aexpr list
  | AENot of aexpr
  | AETilde of aexpr
  | AEBinop of binop * aexpr * aexpr
  | AEUpdate of int * aexpr
  | AEReturn of aexpr
  | AEIf_then_else of aexpr *  aexpr * aexpr
  | AEBlock of ablock
  | AEClos of afunbody
  | AECall of aexpr * aexpr list
and aexpr = {
  aexpr: _aexpr;
  typ: typ
}

and _astmt =
  | ASExpr of aexpr
  | ASAssign of int * aexpr
  | ASUpdate of int * aexpr
and astmt = {
  astmt: _astmt;
  typ: typ
}

and _ablock = astmt list
and ablock = {
  ablock: _ablock;
  typ: typ
}

and _afunbody = {
  args: ident list;
  (*tag: tresult; a tej ?*)
  acontent: aexpr
}
and afunbody = {
  abody: _afunbody;
  typ: typ
}

and _adecl = {
  abody: afunbody;
  name: ident
}
and adecl = {
  adecl: _adecl;
  typ: typ
}

and afile = adecl list

