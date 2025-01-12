open Typed_ast

let tget_call_id e = match e.texpr with
  | TECst cst ->
    begin
      match cst.tconst with
        | TCVar s -> Some s
        | _ -> None
    end
  | _ -> None

let get_value_cst e = match e.texpr with
  | TECst cst -> cst.tconst
  | _ -> failwith "to do"