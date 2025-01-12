open  Alloc_vars
open Printf
open X86_64
open Typing_utils
open Alloc_ast
open Format
open Codegen_utils

(* Production de code *)

let popn n = addq (imm n) !%rsp
let pushn n = subq (imm n) !%rsp

let rec compile_cst c = match c.aconst with
  | ACInt n ->
    pushq (imm n) 
  | ACBool b ->
    pushq (imm (int_of_bool b))
  | ACString s ->
    let code = ref (pushq (imm 0)) in
    for i = 0 to (String.length s - 1) do
      code := !code ++ pushq (imm (c_to_int (s.[i])))
    done;
    !code
  | ACallPrintIntImm n ->
    (* cas où on appelle println sur un entier *)
    pushq (imm n) ++
    popq rdi ++
    call "print_int"
  | ACallPrintInt e ->
    compile_expr e ++
    popq rdi ++
    call "print_int"
  | ACallPrintBoolImm b ->
    let int_of_b = int_of_bool b in
    pushq (imm int_of_b) ++
    popq rdi ++
    call "print_int"
  | ACallPrintBool e ->
    compile_expr e ++
    popq rdi ++
    call "print_int"
  | ACallPrintStringImm s ->
    (* to do : à refaire entièrment *)
    pushq (imm 0) ++
    popq rdi ++ 
    call "print_string"
  | ACallPrintString e ->
    compile_expr e ++
    popq rdi ++
    call "print_string"
  | _ -> failwith "to do"

and compile_expr e = match e.aexpr with
  | AECst c -> 
    compile_cst c
  | AEBlock b ->
    (* TODO : check le list.rev, sans ça traite dans le mauvais sens *)
    List.fold_left (fun code s -> let codefun,codemain = compile_stmt (code, nop) s in
                                  codefun) nop (List.rev b.ablock);
  | AEBinop (op, a, b) ->
    compile_expr a ++
    compile_expr b ++
    popq rbx ++
    popq rax ++
    (match op with
      | Add -> addq !%rbx !%rax ++ pushq !%rax
      | Mul -> imulq !%rbx !%rax ++ pushq !%rax
      | Sub -> subq !%rbx !%rax ++ pushq !%rax
      | Div -> cqto ++ idivq !%rbx ++ pushq !%rax
      | Mod -> cqto ++ idivq !%rbx ++ pushq !%rdx
      | And -> andq !%rbx !%rax ++ pushq !%rax
      | Or  -> orq !%rbx !%rax ++ pushq !%rax
      | Eq  -> 
        (* todo : comprendre comment fonctionnent les flags*)
        nop
      | Neq ->
        nop
      | Lt ->
        nop
      | Leq ->
        nop
      | Gt ->
        nop
      | Geq ->
        nop
      | Pplus ->
        (* todo : concaténation chaîne de caractères *)
        nop)
  | _ -> failwith "to do"

and compile_stmt (codefun, codemain) s = match s.astmt with
  | ASExpr e -> 
    let code = compile_expr {aexpr = e.aexpr; typ = s.typ}
    in code ++ codefun, codemain
  | _ -> failwith "pas encore implémenté"

let compile_body (codefun, codemain) (b : afunbody) =
  let typ = b.typ in
  let body = b.abody in
  let args = body.args in
  let expr = body.acontent in
  let code = compile_expr {aexpr = expr.aexpr;
                           typ = typ} in
  (codefun,codemain ++ code) 

let compile_decl (codefun, codemain) d =
  let name = d.adecl.name in
  let abody = d.adecl.abody in
  compile_body (codefun, codemain) abody

let compile_program p ofile =
  let p = alloc_file p in
  (* Format.eprintf "%a@." print p; *)
  let codefun, code = List.fold_left compile_decl (nop, nop) p in
  let p =
    { text =
        globl "main" ++ label "main" ++
        movq !%rsp !%rbp ++
        code ++
        movq (imm 0) !%rax ++ (* exit *)
        ret ++
        label "print_int" ++
        movq !%rdi !%rsi ++
        movq (ilab ".Sprint_int") !%rdi ++
        movq (imm 0) !%rax ++ (* todo : vérifier l'alignement de la pile *)
        call "printf" ++
        ret ++
        label "print_string" ++
        movq !%rdi !%rsi ++
        movq (ilab "message") !%rdi ++
        movq (imm 0) !%rax ++
        call "printf" ++
        ret ++
        codefun;
      data =
      (label ".Sprint_int" ++ string "%d\n" ++
       label "message" ++ string "%s\n")
    }
  in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  X86_64.print_program fmt p;
  fprintf fmt "@?";
  close_out f