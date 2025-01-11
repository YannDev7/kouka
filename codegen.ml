open  Alloc_vars
open Printf
open X86_64
open Typing_utils

(* Production de code *)

let popn n = addq (imm n) !%rsp
let pushn n = subq (imm n) !%rsp

let compile_cst c = match c._aconst with
  | 

let compile_expr e = match e.aexpr with
  | AECst c -> compile_cst c
  | AECall(f, lst_exp) -> 
    match get_call_id f with
      | Some s when s = "println" ->


let compile_stmt s = match s.astmt with
  | ASExpr e -> compile_expr {aexpr = e; typ = s.typ}
  | _ -> failwith "pas encore implémenté"

let compile_program p ofile =
  let p = alloc p in
  Format.eprintf "%a@." print p;
  let codefun, code = List.fold_left compile_stmt (nop, nop) p in
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
        movq (imm 0) !%rax ++
        call "printf" ++
        ret ++
        codefun;
      data =
        Hashtbl.fold (fun x _ l -> label x ++ dquad [1] ++ l) genv
          (label ".Sprint_int" ++ string "%d\n")
    }
  in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  X86_64.print_program fmt p;
  fprintf fmt "@?";
  close_out f