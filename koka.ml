(* UWU *)

open Format
open Lexing
open Parser
open Lexer
open Exception
open Pp
open Typing

let usage = "usage: koka [options] file.koka"

let parse_only = ref false
let type_only = ref false

let spec =
  [
    "--parse-only", Arg.Set parse_only, "  stop after parsing";
    "--type-only", Arg.Set type_only, "  stop after typing"
  ]
let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".koka") then
      raise (Arg.Bad "no .koka extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with Some f -> f | None -> Arg.usage spec usage; exit 1
  
let report (b,e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let _f = Parser.file Lexer.next_token lb in
    close_in c;
    if !parse_only then exit 0;
    (*Interp.file f*)
    pp_file Format.std_formatter _f;

    ignore(let tast = Typing.type_file _f in ());

    if !type_only then exit 0;
  with
    | Lexer.Lexing_error s ->
	report (lexeme_start_p lb, lexeme_end_p lb);
	eprintf "lexical error: %s@." s;
	exit 1
    | Parser.Error ->
	report (lexeme_start_p lb, lexeme_end_p lb);
	eprintf "syntax error@.";
	exit 1
    | Block_not_end_expr ->
  report (lexeme_start_p lb, lexeme_end_p lb);
  eprintf "syntax error, block not ending with expr@.";
  exit 1
    | Typing.Error (pos, s) ->
  report pos;
  eprintf "typing error, %s@." s;
  exit 1
    | e ->
	eprintf "Anomaly: %s\n@." (Printexc.to_string e);
	exit 2



