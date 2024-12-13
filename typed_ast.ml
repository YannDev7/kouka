open Ast

          (*  div    cons  *)
type effect =
  | Div
  | Console

module Effset = Set.Make(struct type t = effect let compare = compare end)

type typ_val =
  | TUnit
  | TBool
  | TInt
  | TString
  | TList of typ
  | TFun of typ list * typ
  | TMaybe of typ
  | TVar of tvar
and tvar = {
  id: int;
  mutable def: typ option;
}
and effects =
  | ESet of Effset.t
  | EUnion of effects * effects
  | TEff of teff
and teff = {
  eid: int;
  mutable edef: teff option
}
and typ = typ_val * effects

module V = struct
  type t = tvar
  let compare v1 v2 = Stdlib.compare v1.id v2.id
  let equal v1 v2 = v1.id = v2.id
  let create = let r = ref 0 in fun () -> incr r; { id = !r; def = None }
end

module VEff = struct
  type t = tvar
  let compare v1 v2 = Stdlib.compare v1.id v2.id
  let equal v1 v2 = v1.id = v2.id
  let create = let r = ref 0 in fun () -> incr r; { eid = !r; edef = None }
end

type _tconst =
  | TCUnit
  | TCBool of bool
  | TCInt of int
  | TCString of string
  | TCVar of ident
and tconst = {
  tconst: _tconst;
  typ: typ
}

and _texpr =
  | TECst of tconst
  | TEList of texpr list
  | TENot of texpr
  | TETilde of texpr
  | TEBinop of binop * texpr * texpr
  | TEUpdate of ident * texpr
  | TEReturn of texpr
  | TEIf_then_else of texpr *  texpr * texpr
  | TEBlock of tblock
  | TEFn of funbody
  | TECall of texpr * texpr list 
and texpr = {
  texpr: _texpr;
  typ: typ
}

and _tstmt =
  | TSExpr of texpr
  | TSAssign of ident * texpr
  | TSUpdate of ident * texpr
and tstmt = {
  tstmt: _tstmt;
  typ: typ
}

and _tblock = tstmt list
and tblock = {
  tblock: _tblock;
  typ: typ
}

and _tfunbody = {
  args: ident list;
  (*tag: tresult; a tej ?*)
  tcontent: texpr
}
and tfunbody = {
  tbody: _tfunbody;
  typ: typ
}

and _tdecl = {
  tbody: tfunbody;
  name: ident
}
and tdecl = {
  tdecl: _tdecl;
  typ: typ
}

and tfile = tdecl list
