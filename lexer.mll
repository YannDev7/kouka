{
    open Parser
    open Lexing
    open Ast

    exception Lexing_error of string

    let valid_ident s =
        let len = String.length s in
        let ok = ref true in
        let ban = [
            '0'; '1'; '2'; '3'; '4'; 
            '5'; '6'; '7'; '8'; '9'
        ] in
        for i = 0 to len - 1 do
            if s.[i] = '-' then begin
                if i > 0 && List.mem s.[i - 1] ban then ok := false;
                if i + 1 < len && List.mem s.[i - 1] ban then ok := false;
            end
        done;
        !ok

    let kwd_or_id =
        let kws = Hashtbl.create 42 in
        List.iter (fun (name, token) -> Hashtbl.add kws name token) 
            [
                ("fun", FUN); ("val", VAL); ("var", VAR); ("if", IF);
                ("then", THEN); ("elif", ELIF); ("else", ELSE)
            ];
        fun s -> try Hashtbl.find kws s with | Not_found -> IDENT s

    let pp_lexbuf lb =
        let pos = lb.lex_curr_pos in
        let len = lb.lex_buffer_len - pos in
        let content = Bytes.to_string (Bytes.sub lb.lex_buffer pos len) in

        Format.print_string (Printf.sprintf "Lexbuf: len = %d, content = [\n" len);
        Format.print_string content;
        Format.print_string "\n]\n"
}

(* a5 - 6 vs a5-6 wtf ???? *)
let digit = ['0'-'9']
let integer = ['0'-'9']*
let lower = ['a'-'z'] | '_'
let upper = ['A'-'Z']
let other = lower | upper | digit | '-'
let ident = lower other* "'"* (* TODO: check if it works *)
let space = [' ' '\t'] (* TODO: handle indentation *)
let comment_line = "//" [^'\n']*

(* TODO: add string handling *)

rule next_tokens = parse
    | '\n' { new_line lexbuf; next_tokens lexbuf }
    | (space | comment_line)+ { pp_lexbuf lexbuf; next_tokens lexbuf }
    | "/*" { comment lexbuf }
    | "{" { LBRACE }
    | "}" { pp_lexbuf lexbuf; RBRACE }
    | "(" { LPAR }
    | ")" { RPAR }
    | "," { COMMA }
    | ";" { SEMICOLON }
    | ":" { COLON }
    | "->" { ARROW }
    | "<" { LT }
    | ">" { GT }
    | "+" { ADD }
    | "-" { SUB }
    | "*" { MUL }
    | "/" { DIV }
    | "%" { MOD }
    | "==" { DEQ }
    | "!=" { NEQ }
    | "<" { LT }
    | "<=" { LEQ }
    | ">" { GT }
    | ">=" { GEQ }
    | "&&" { AND }
    | "||" { OR }
    | "++" { PPLUS }
    | "=" { ASSIGN }
    | ":=" { UPDATE }
    | "True" { ATOM (ABool true) }
    | "False" { ATOM (ABool false) }
    | ident as id { if not (valid_ident id) then failwith "[Error]: Invalid ident name";
                    pp_lexbuf lexbuf; kwd_or_id id }
    | integer as s 
        {
            pp_lexbuf lexbuf;
            try ATOM (AInt (int_of_string s))
            with _ -> raise (Lexing_error ("constant too large: " ^ s))
        }
    | eof { EOF }
and comment = parse
    | "*/" { next_tokens lexbuf }
    | "\n" { new_line lexbuf; comment lexbuf }
    | _ { comment lexbuf }
    | eof { failwith "Comment without end..." }
