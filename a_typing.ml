(* typage avec algorithme W (cf TD6) *)

type typc =
  | Tint
  | Tarrow of typ * typ
  | Tproduct of typ * typ
and typ =
  | Te of typc
  | Tvar of tvar
and tvar =
  { id : int;
    mutable def : typ option }

let rec pp_typ fmt = function
  | Tproduct (t1, t2) -> Format.fprintf fmt "%a *@ %a" pp_atom t1 pp_atom t2
  | Tarrow (t1, t2) -> Format.fprintf fmt "%a ->@ %a" pp_atom t1 pp_typ t2
  | (Tint | Tvar _) as t -> pp_atom fmt t
and pp_atom fmt = function
  | Tint -> Format.fprintf fmt "int"
  | Tvar v -> pp_tvar fmt v
  | Tarrow _ | Tproduct _ as t -> Format.fprintf fmt "@[<1>(%a)@]" pp_typ t
and pp_tvar fmt = function
  | { def = None; id } -> Format.fprintf fmt "'%d" id
  | { def = Some t; id } -> Format.fprintf fmt "@[<1>('%d := %a)@]" id pp_typ t

module V = struct
  type t = tvar
  let compare v1 v2 = Stdlib.compare v1.id v2.id
  let equal v1 v2 = v1.id = v2.id
  let create = let r = ref 0 in fun () -> incr r; { id = !r; def = None }
end

let rec head = function
  | Tvar { def = Some t } -> head t
  | t -> t

let rec canon = function
  | Tint -> Tint
  | Tvar { def = Some t } -> canon t
  | Tarrow (ta, tr) -> Tarrow (canon ta, canon tr)
  | Tproduct (ta, tb) -> Tproduct (canon ta, canon tb)
  | t -> t

let () =
  let a = V.create () in
  let b = V.create () in
  let ta = Tvar a in
  let tb = Tvar b in
  assert (head ta == ta);
  assert (head tb == tb);
  let ty = Tarrow (ta, tb) in
  a.def <- Some tb;
  assert (head ta == tb);
  assert (head tb == tb);
  b.def <- Some Tint;
  assert (head ta = Tint);
  assert (head tb = Tint);
  assert (canon ta = Tint);
  assert (canon tb = Tint);
  assert (canon ty = Tarrow (Tint, Tint))

exception UnificationFailure of typ * typ
let unification_error t1 t2 = raise (UnificationFailure (canon t1, canon t2))

let rec occur v t = match head t with
  | Tvar tv -> tv.id = v.id (* no rec because tv.def is None; we can use V.equal *)
  | Tint -> false
  | Tarrow (ta, tr) -> occur v ta || occur v tr
  | Tproduct (ta, tb) -> occur v ta || occur v tb

let rec unify t1 t2 = match head t1, head t2 with
  | Tvar tv1, Tvar tv2 when tv1.id = tv2.id -> ()
  | Tvar tv1, t2 -> 
    if occur tv1 t2 then unification_error t1 t2;
    tv1.def <- Some t2
  | t1, Tvar tv2 -> 
    if occur tv2 t1 then unification_error t1 t2;
    tv2.def <- Some t1
  | Tarrow (ta1, tr1), Tarrow (ta2, tr2) ->
    unify ta1 ta2;
    unify tr1 tr2
  | Tproduct (ta1, tb1), Tproduct (ta2, tb2) ->
    unify ta1 ta2;
    unify tb1 tb2
  | Tint, Tint -> ()
  | _, _ -> unification_error t1 t2

let () =
  let a = V.create () in
  let b = V.create () in
  let ta = Tvar a in
  let tb = Tvar b in
  assert (occur a ta);
  assert (occur b tb);
  assert (not (occur a tb));
  let ty = Tarrow (ta, tb) in
  assert (occur a ty);
  assert (occur b ty);
  (* unifie 'a-> 'b et int->int *)
  unify ty (Tarrow (Tint, Tint));
  assert (canon ta = Tint);
  assert (canon ty = Tarrow (Tint, Tint));
  (* unifie 'c et int->int *)
  let c = V.create () in
  let tc = Tvar c in
  unify tc ty;
  assert (canon tc = Tarrow (Tint, Tint))

let cant_unify ty1 ty2 =
  try let _ = unify ty1 ty2 in false with UnificationFailure _ -> true

let () =
  assert (cant_unify Tint (Tarrow (Tint, Tint)));
  assert (cant_unify Tint (Tproduct (Tint, Tint)));
  let a = V.create () in
  let ta = Tvar a in
  unify ta (Tarrow (Tint, Tint));
  assert (cant_unify ta Tint)

module Vset = Set.Make(V)

let fvars bt =
  let ans = ref Vset.empty in
  let rec aux t = match head t with
    | Tvar tv -> ans := Vset.add tv !ans (* we used head so must be None *)
    | Tarrow (ta, tr) -> aux ta; aux tr
    | Tproduct (ta, tb) -> aux ta; aux tb
    | Tint -> ()
  in aux bt;
  !ans

let () =
  assert (Vset.is_empty (fvars (Tarrow (Tint, Tint))));
  let a = V.create () in
  let ta = Tvar a in
  let ty = Tarrow (ta, ta) in
  assert (Vset.equal (fvars ty) (Vset.singleton a));
  unify ty (Tarrow (Tint, Tint));
  assert (Vset.is_empty (fvars ty))

type schema = { vars : Vset.t; typ : typ }

module Smap = Map.Make(String)
type env = { bindings : schema Smap.t; fvars : Vset.t }

let empty = { bindings = Smap.empty; fvars = Vset.empty }

let add ident t env =
  let fv = fvars t in
  { 
    bindings = Smap.add ident { vars = Vset.empty; typ = t } env.bindings; 
    fvars = Vset.union fv env.fvars
  }
let upd_fvars fvars_s =
  Vset.fold (fun tv acc -> Vset.union acc (fvars (Tvar tv))) fvars_s Vset.empty
  
let add_gen ident t env =
  let fv = fvars t in
  { 
    bindings = Smap.add ident { vars = Vset.diff fv (upd_fvars env.fvars); typ = t } env.bindings; 
    fvars = Vset.union fv (upd_fvars env.fvars)
  }

module Vmap = Map.Make(V)


let find ident env =
  let t = Smap.find ident env.bindings in
  let fresh_vars = ref Vmap.empty in
  Vset.iter (
    fun fv -> fresh_vars := Vmap.add fv (V.create ()) !fresh_vars
  ) (upd_fvars t.vars);

  let rec clean t = match head t with
    | Tvar tv ->
      if Vmap.mem tv !fresh_vars then Tvar (Vmap.find tv !fresh_vars)
      else Tvar tv
    | Tint -> Tint
    | Tarrow (ta, tr) -> Tarrow (clean ta, clean tr)
    | Tproduct (ta, tb) -> Tproduct (clean ta, clean tb)
  in clean t.typ

type expression =
  | Var of string
  | Const of int
  | Op of string
  | Fun of string * expression
  | App of expression * expression
  | Pair of expression * expression
  | Let of string * expression * expression

let rec w env = function
  | Var ident -> find ident env
  | Const i -> Tint
  | Op ident -> Tarrow (Tproduct (Tint, Tint), Tint) (* only + *)
  | Fun (ident, e) ->
    let alpha = Tvar (V.create ()) in
    let tau = w (add ident alpha env) e in
    Tarrow (alpha, tau)
  | App (e1, e2) ->
    let tau1 = w env e1 in
    let tau2 = w env e2 in
    let alpha = Tvar (V.create ()) in
    unify tau1 (Tarrow (tau2, alpha));
    alpha
  | Pair (e1, e2) ->
    let tau1 = w env e1 in
    let tau2 = w env e2 in
    Tproduct (tau1, tau2)
  | Let (ident, e1, e2) ->
    let tau1 = w env e1 in
    w (add_gen ident tau1 env) e2
let typeof e = canon (w empty e)

(* 1 : int *)
let () = assert (typeof (Const 1) = Tint)

(* fun x -> x : 'a -> 'a *)
let () = assert (match typeof (Fun ("x", Var "x")) with
  | Tarrow (Tvar v1, Tvar v2) -> V.equal v1 v2
  | _ -> false)

(* fun x -> x+1 : int -> int *)
let () = assert (typeof (Fun ("x", App (Op "+", Pair (Var "x", Const 1))))
                 = Tarrow (Tint, Tint))

(* fun x -> x+x : int -> int *)
let () = assert (typeof (Fun ("x", App (Op "+", Pair (Var "x", Var "x"))))
                 = Tarrow (Tint, Tint))

(* let x = 1 in x+x : int *)
let () =
  assert (typeof (Let ("x", Const 1, App (Op "+", Pair (Var "x", Var "x"))))
          = Tint)

(* let id = fun x -> x in id 1 *)
let () =
  assert (typeof (Let ("id", Fun ("x", Var "x"), App (Var "id", Const 1)))
          = Tint)

(* let id = fun x -> x in id id 1 *)
let () =
  assert (typeof (Let ("id", Fun ("x", Var "x"),
		       App (App (Var "id", Var "id"), Const 1)))
          = Tint)

(* let id = fun x -> x in (id 1, id (1,2)) : int * (int * int) *)
let () =
  assert (typeof (Let ("id", Fun ("x", Var "x"),
		       Pair (App (Var "id", Const 1),
			     App (Var "id", Pair (Const 1, Const 2)))))
          = Tproduct (Tint, Tproduct (Tint, Tint)))

(* app = fun f x -> let y = f x in y : ('a -> 'b) -> 'a -> 'b *)
let () =
  let ty =
    typeof (Fun ("f", Fun ("x", Let ("y", App (Var "f", Var "x"), Var "y"))))
  in
  assert (match ty with
    | Tarrow (Tarrow (Tvar v1, Tvar v2), Tarrow (Tvar v3, Tvar v4)) ->
        V.equal v1 v3 && V.equal v2 v4
    | _ -> false)

let cant_type e =
  try let _ = typeof e in false with UnificationFailure _ -> true

(* 1 2 *)
let () = assert (cant_type (App (Const 1, Const 2)))

(* fun x -> x x *)
let () = assert (cant_type (Fun ("x", App (Var "x", Var "x"))))

(* (fun f -> +(f 1)) (fun x -> x) *)
let () = assert (cant_type
                    (App (Fun ("f", App (Op "+", App (Var "f", Const 1))),
                    Fun ("x", Var "x"))))

(* fun x -> (x 1, x (1,2)) *)
let () = assert (cant_type
                    (Fun ("x", Pair (App (Var "x", Const 1),
                  App (Var "x", Pair (Const 1, Const 2))))))

(* fun x -> let z = x in (z 1, z (1,2)) *)
let () = assert (cant_type
                    (Fun ("x",
              Let ("z", Var "x",
            Pair (App (Var "z", Const 1),
                  App (Var "z", Pair (Const 1, Const 2)))))))

(* let distr_pair = fun f -> (f 1, f (1,2)) in distr_pair (fun x -> x) *)
let () =
  assert (cant_type
            (Let ("distr_pair",
      Fun ("f", Pair (App (Var "f", Const 1),
          App (Var "f", Pair (Const 1, Const 2)))),
      App (Var "distr_pair", (Fun ("x", Var "x"))))))
