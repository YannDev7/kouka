{
    open Parser
    open Lexing

    exception Lexing_error of string

    let kwd_or_id =
        let kws = Hashtbl.create 42 in
        List.iter (fun (name, token) -> Hashtbl.add kws name token) 
            [
                ("fun", FUN)
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

let digit = ['0'-'9']
let lower = ['a'-'z'] | '_'
let upper = ['A'-'Z']
let other = lower | upper | digit | '-'
let ident = lower other* (* TODO: wrong lol *)
let space = [' ' '\t'] (* TODO: handle indentation *)
let comment_line = "//" [^'\n']*

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
    | ident as id { pp_lexbuf lexbuf; kwd_or_id id }
    | eof { EOF }
and comment = parse
    | "*/" { next_tokens lexbuf }
    | "\n" { new_line lexbuf; comment lexbuf }
    | _ { comment lexbuf }
    | eof { failwith "Comment without end..." }