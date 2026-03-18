open Core
open Chatml_lang

type diagnostic =
  { message : string
  ; span : Source.span option
  }

let position_of_lex (pos : Lexing.position) : Source.position =
  { line = pos.pos_lnum; column = pos.pos_cnum - pos.pos_bol; offset = pos.pos_cnum }
;;

let span_of_lex (startp : Lexing.position) (endp : Lexing.position) : Source.span =
  { left = position_of_lex startp; right = position_of_lex endp }
;;

let diagnostic_of_lexbuf (lexbuf : Lexing.lexbuf) (message : string) : diagnostic =
  { message; span = Some (span_of_lex lexbuf.lex_start_p lexbuf.lex_curr_p) }
;;

let format_diagnostic (source_text : string) (diagnostic : diagnostic) : string =
  match diagnostic.span with
  | None -> Printf.sprintf "Parse error: %s" diagnostic.message
  | Some span ->
    let source = Source.read (Source.make source_text) span in
    let caret_count = Int.max 1 (span.right.column - span.left.column) in
    Printf.sprintf
      "line %i, characters %i-%i:\n%i|    %s%s\n      %s\n\nParse error: %s"
      span.left.line
      span.left.column
      span.right.column
      span.left.line
      source
      (String.make (span.left.column + 3) ' ')
      (String.make caret_count '^')
      diagnostic.message
;;

let parse_program (source_text : string) : (program, diagnostic) result =
  let lexbuf = Lexing.from_string source_text in
  try
    let stmts = Chatml_parser.program Chatml_lexer.token lexbuf in
    Ok { stmts; source_text }
  with
  | Chatml_parser.Error -> Error (diagnostic_of_lexbuf lexbuf "Syntax error")
  | Failure msg -> Error (diagnostic_of_lexbuf lexbuf msg)
;;

let parse_program_exn (source_text : string) : program =
  match parse_program source_text with
  | Ok prog -> prog
  | Error diagnostic -> failwith (format_diagnostic source_text diagnostic)
;;
