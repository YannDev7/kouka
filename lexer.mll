{
    open Parser
    open Lexing
    open Ast
    open Pp

    exception Lexing_error of string

    let c = ref 0

    let buf = Buffer.create 4242

    let col_num lb = lb.lex_curr_p.pos_cnum - lb.lex_curr_p.pos_bol

    let valid_ident s =
        let len = String.length s in
        let ok = ref true in
        let banS = [
            '0'; '1'; '2'; '3'; '4'; 
            '5'; '6'; '7'; '8'; '9';
            '-'; '_'
        ] in
        let banF = ['-'; '_'] in
        for i = 0 to len - 1 do
            if s.[i] = '-' then begin
                if (i = 0) || (List.mem s.[i - 1] banF) then ok := false;
                if i + 1 < len && List.mem s.[i + 1] banS then ok := false;
            end
        done;
        !ok

    let kwd_or_id =
        let kws = Hashtbl.create 42 in
        List.iter (fun (name, token) -> Hashtbl.add kws name token) 
            [
                ("fun", FUN); ("val", VAL); ("var", VAR); ("if", IF);
                ("then", THEN); ("elif", ELIF); ("else", ELSE); ("fn", FN);
                ("return", RETURN)
            ];
        fun s -> try Hashtbl.find kws s with | Not_found -> IDENT s
}

(* a5 - 6 vs a5-6 wtf ????
    a    a+b  b

    a + b - a+b
    a+b-a+b

    TODO: fun main() fn () avec autre chose que fn (i.e un appel)
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
    | (space | comment_line)+ { (*pp_lexbuf lexbuf;*) c := col_num lexbuf; next_tokens lexbuf }
    | "/*" { comment lexbuf }
    | "{" { LBRACE }
    | "}" { (*pp_lexbuf lexbuf;*) RBRACE }
    | "(" { LPAR }
    | ")" { RPAR }
    | "[" { LBRACKET }
    | "]" { RBRACKET }
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
    | "!" { NOT }
    | "<" { LT }
    | "<=" { LEQ }
    | ">" { GT }
    | ">=" { GEQ }
    | "&&" { AND }
    | "||" { OR }
    | "~" { TILDE }
    | "++" { PPLUS }
    | "=" { ASSIGN }
    | ":=" { UPDATE }
    | "True" { ATOM (ABool true) }
    | "False" { ATOM (ABool false) }
    | "\"" { Buffer.clear buf; string lexbuf }
    | ident as id { if not (valid_ident id) then raise (Lexing_error "Invalid ident name");
                    (*pp_lexbuf lexbuf;*) kwd_or_id id }
    | "-"integer as s (* TODO: fix spaghetti code *)
        {
            (*pp_lexbuf lexbuf;*)
            try ATOM (AInt (-(int_of_string s)))
            with _ -> raise (Lexing_error ("Constant too large: " ^ s))
        }
    | integer as s 
        {
            (*pp_lexbuf lexbuf;*)
            try ATOM (AInt (int_of_string s))
            with _ -> raise (Lexing_error ("Constant too large: " ^ s))
        }
    | eof { EOF }
and comment = parse
    | "*/" { c := col_num lexbuf; next_tokens lexbuf }
    | "\n" { new_line lexbuf; c := col_num lexbuf; comment lexbuf }
    | _ { c := col_num lexbuf; comment lexbuf }
    | eof { raise (Lexing_error ("Comment without end...")) }
and string = parse
    | '"' { ATOM (AString (Buffer.contents buf)) }
    | "\\\\" { Buffer.add_char buf '\\'; string lexbuf }
    | "\\\"" { Buffer.add_char buf '\\'; Buffer.add_char buf '"'; string lexbuf }
    | "\\t" { Buffer.add_char buf '\t'; string lexbuf }
    | "\\n" { Buffer.add_char buf '\n'; string lexbuf }
    | _ as c { Buffer.add_char buf c; string lexbuf }
    | eof { raise (Lexing_error ("String without end quote...")) }

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
            (* pp_tok Format.std_formatter tok; *)
            if tok = RBRACE then Queue.push SEMICOLON tokens;
            if tok = ELIF then Queue.push ELSE tokens;
            Queue.push (if tok = ELIF then IF else tok) tokens;
            last := tok in
        Stack.push 0 ident_st;

        fun lb ->
            c := 0;
            let eat () =
                c := col_num lb; (* Important! to make sure the column corresponds
                                    to the beginning of the token *)
                let tok = next_tokens lb in
                (*pp_tok Format.std_formatter (ATOM (AInt !c));
                pp_tok Format.std_formatter tok;*)
                tok in

            if Queue.is_empty tokens then begin
                let rec act action = function
                    | NEWLINE -> act true (eat ())
                    (* TODO: newline before EOF ? *)
                    | next when action = false -> if next = EOF then act true EOF
                                                  else emit next
                    | next when action = true ->
                        if next = EOF then c := 0;
                        let m = Stack.top ident_st in
                        if !c > m then begin
                            if not (end_of_cont !last) && not (beg_of_cont next) then begin
                                emit LBRACE;
                                (* Queue.push SEMICOLON tokens; lecture du sujet *)
                            end;
                            
                            if !last = LBRACE then
                                Stack.push !c ident_st;

                            emit next;
                        end else begin
                            while !c < Stack.top ident_st do
                                ignore(Stack.pop ident_st);
                                if next <> RBRACE then begin
                                    emit RBRACE;
                                end
                            done;

                            if !c > Stack.top ident_st then raise (Lexing_error "Indentation error");

                            if not (end_of_cont !last) && not (beg_of_cont next) then
                                (*pp_tok Format.std_formatter (ATOM (AInt !c));
                                pp_tok Format.std_formatter !last;
                                pp_tok Format.std_formatter (ATOM (AInt !c));
                                pp_tok Format.std_formatter next;*)
                                emit SEMICOLON;
                            emit next;
                        end 
                    | _ -> raise (Lexing_error "plz no warning")
                in act false (eat ())
            end;

            let ans = Queue.pop tokens in
            pp_tok Format.std_formatter ans;
            ans
}