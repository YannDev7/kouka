{
    open Parser
    open Lexing
    open Ast
    open Pp

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
}

(* a5 - 6 vs a5-6 wtf ????
    a    a+b  b

    a + b - a+b
    a+b-a+b
*)
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
    | '\n' { new_line lexbuf; NEWLINE }
    | (space | comment_line)+ { (*pp_lexbuf lexbuf;*) next_tokens lexbuf }
    | "/*" { comment lexbuf }
    | "{" { LBRACE }
    | "}" { (*pp_lexbuf lexbuf;*) RBRACE }
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
                    (*pp_lexbuf lexbuf;*) kwd_or_id id }
    | integer as s 
        {
            (*pp_lexbuf lexbuf;*)
            try ATOM (AInt (int_of_string s))
            with _ -> raise (Lexing_error ("constant too large: " ^ s))
        }
    | eof { EOF }
and comment = parse
    | "*/" { next_tokens lexbuf }
    | "\n" { new_line lexbuf; comment lexbuf }
    | _ { comment lexbuf }
    | eof { failwith "Comment without end..." }

{
    let next_token =
        let end_of_cont tok = List.mem tok [
            ADD; SUB; MUL; DIV; MOD; PPLUS; LT; LEQ;
            GT; GEQ; DEQ; NEQ; AND; OR; LPAR; LBRACE; COMMA
        ] in
        let beg_of_cont tok = List.mem tok [
            ADD; SUB; MUL; DIV; MOD; PPLUS; LT; LEQ;
            GT; GEQ; DEQ; NEQ; AND; OR; THEN; ELSE; ELIF; RPAR;
            RBRACE; COMMA; ARROW; LBRACE; ASSIGN; DOT; UPDATE
        ] in
        let last = ref EOF in
        let tokens = Queue.create () in
        let ident_st = Stack.create () in
        let emit tok =
            if tok = RBRACE then Queue.push SEMICOLON tokens;
            Queue.push tok tokens;
            last := tok in
        Stack.push 0 ident_st;

        fun lb ->
            let col_num () = lb.lex_curr_p.pos_cnum - lb.lex_curr_p.pos_bol in
            if Queue.is_empty tokens then begin
                let rec act action = function
                    | NEWLINE -> act true (next_tokens lb)
                    | next when action = false -> emit next
                    | next when action = true ->
                        let c = (if next = EOF then 0 else col_num ()) in
                        let m = Stack.top ident_st in
                        if c > m then begin
                            if not (end_of_cont !last) && not (beg_of_cont next) then begin
                                emit LBRACE;
                                (* Queue.push SEMICOLON tokens; lecture du sujet *)
                            end;
                            
                            if !last = LBRACE then
                                Stack.push c ident_st;

                            emit next;
                        end else begin
                            while c < Stack.top ident_st do
                                ignore(Stack.pop ident_st);
                                if next <> RBRACE then begin
                                    emit RBRACE;
                                end
                            done;

                            if c > m then raise (Lexing_error "Indentation error");

                            if not (end_of_cont !last) && not (beg_of_cont next) then
                                emit SEMICOLON;
                            emit next;
                        end 
                    | _ -> failwith "plz no warning"
                in act false (next_tokens lb)
            end;

            let ans = Queue.pop tokens in
            (*in pp_tok Format.std_formatter ans;*)
            ans
}