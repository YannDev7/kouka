open  Alloc_vars
open Printf
open X86_64
open Typing_utils
open Alloc_ast
open Format
open Codegen_utils

(* Production de code *)

let n_string = ref 0 (* nombre de string à print *)
let s_to_print = ref [] (* liste des string à print *)

let popn n = addq (imm n) !%rsp
let pushn n = subq (imm n) !%rsp

let rec compile_cst c = match c.aconst with
  | ACInt n ->
    pushq (imm n) 
  | ACBool b ->
    pushq (imm (int_of_bool b))
  | ACString s ->
    (* to do : faire les strings *)
    nop
  | ACallPrintIntImm n ->
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
    call "print_bool"
  | ACallPrintBool e ->
    compile_expr e ++
    popq rdi ++
    call "print_bool"
  | ACallPrintStringImm s ->
    s_to_print := s::(!s_to_print);
    let code =
    movq !%rdi !%rsi ++
    movq (ilab ("string"^(string_of_int !n_string))) !%rdi ++
    movq (imm 0) !%rax ++
    call "puts" in
    incr n_string;
    code
  | ACallPrintString e ->
    compile_expr e ++
    popq rdi ++
    call "print_string"
  | _ -> failwith "to do"

and compile_expr e = match e.aexpr with
  | AECst c -> 
    compile_cst c
  | AEList l ->
    List.fold_left (fun code e -> code ++ (compile_expr e)) nop l
  | AENot e ->
    compile_expr e ++
    popq rax ++
    notq !%rax ++
    pushq !%rax
  | AETilde e ->
    compile_expr e ++
    popq rax ++
    negq !%rax ++
    pushq !%rax
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
        let label_name = give_label 0 in
        pushq (imm 1) ++
        cmpq !%rbx !%rax ++
        je label_name ++
        popq rdi ++
        pushq (imm 0) ++
        label label_name ++
        popq rax ++
        pushq !%rax
      | Neq ->
        let label_name = give_label 0 in
        pushq (imm 1) ++
        cmpq !%rbx !%rax ++
        jne label_name ++
        popq rdi ++
        pushq (imm 0) ++
        label label_name ++
        popq rax ++
        pushq !%rax
      | Lt ->
        let label_name = give_label 0 in
        pushq (imm 1) ++
        cmpq !%rbx !%rax ++
        jl label_name ++
        popq rdi ++
        pushq (imm 0) ++
        label label_name ++
        popq rax ++
        pushq !%rax
      | Leq ->
        let label_name = give_label 0 in
        pushq (imm 1) ++
        cmpq !%rbx !%rax ++
        jle label_name ++
        popq rdi ++
        pushq (imm 0) ++
        label label_name ++
        popq rax ++
        pushq !%rax
      | Gt ->
        let label_name = give_label 0 in
        pushq (imm 1) ++
        cmpq !%rbx !%rax ++
        ja label_name ++
        popq rdi ++
        pushq (imm 0) ++
        label label_name ++
        popq rax ++
        pushq !%rax
      | Geq ->
        let label_name = give_label 0 in
        pushq (imm 1) ++
        cmpq !%rbx !%rax ++
        jae label_name ++
        popq rdi ++
        pushq (imm 0) ++
        label label_name ++
        popq rax ++
        pushq !%rax
      | Pplus ->
        (* todo : concaténation chaîne de caractères *)
        nop)
  | AEIf_then_else (e1, e2, e3) ->
    compile_expr e1 ++
    popq rax
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
        movq (imm 0) !%rax ++ (* todo : aligner la pile *)
        pushq !%rbp ++
        movq !%rsp !%rbp ++
        andq (imm (-16)) !%rsp ++
        call "printf" ++
        movq !%rbp !%rsp ++
        popq rbp ++
        ret ++
        label "print_false" ++
        movq !%rdi !%rsi ++
        movq (ilab (".false")) !%rdi ++
        movq (imm 0) !%rax ++
        pushq !%rbp ++
        movq !%rsp !%rbp ++
        andq (imm (-16)) !%rsp ++
        call "puts" ++
        movq !%rbp !%rsp ++
        popq rbp ++
        ret ++
        label "print_true" ++
        movq !%rdi !%rsi ++
        movq (ilab (".true")) !%rdi ++
        movq (imm 0) !%rax ++
        pushq !%rbp ++
        movq !%rsp !%rbp ++
        andq (imm (-16)) !%rsp ++
        call "puts" ++
        movq !%rbp !%rsp ++
        popq rbp ++
        ret ++
        label "print_bool" ++
        movq (ilab "print_false") !%r10 ++
        cmpq (imm 0) !%rdi ++
        je "chg_b_to_print" ++
        movq (ilab "print_true") !%r10 ++
        label "chg_b_to_print" ++
        call_star !%r10 ++
        ret ++
        codefun;
      data =
      let messages,_ = List.fold_left 
      (fun (m, i) s -> 
        (m ++ label ("string"^(string_of_int i)) ++
        string s,
        i+1)
      ) (nop, 0) (!s_to_print) in
      (label ".Sprint_int" ++ string "%d\n" ++
       label ".false" ++ string "False" ++
       label ".true" ++ string "True" ++
       messages)
    }
  in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  X86_64.print_program fmt p;
  fprintf fmt "@?";
  close_out f