open Core
open Chatml_lang

type diagnostic = Chatml_diagnostic.t

module Unit = struct
  type t =
    { source : Chatml_source.t
    ; program : program
    }

  let source t = t.source
  let program t = t.program
end

let position_of_lex (pos : Lexing.position) : Source.position =
  { line = pos.pos_lnum; column = pos.pos_cnum - pos.pos_bol; offset = pos.pos_cnum }
;;

let span_of_lex (startp : Lexing.position) (endp : Lexing.position) : Source.span =
  { left = position_of_lex startp; right = position_of_lex endp }
;;

let diagnostic_of_lexbuf (lexbuf : Lexing.lexbuf) (message : string) : diagnostic =
  Chatml_diagnostic.create
    ~span:(span_of_lex lexbuf.lex_start_p lexbuf.lex_curr_p)
    message
;;

let format_diagnostic source_text diagnostic =
  Chatml_diagnostic.render_legacy
    diagnostic
    ~source:source_text
    ~label:"Parse error"
    ~checkpoint:(fun () -> ())
;;

exception Lexer_failure of string

let parse source_text ~checkpoint : (program, diagnostic) result =
  Chatml_checkpoint.protect checkpoint ~f:(fun checkpoint ->
    let lexbuf = Lexing.from_string source_text in
    let remaining = ref 0 in
    let token lexbuf =
      decr remaining;
      if !remaining <= 0
      then (
        remaining := 64;
        checkpoint ());
      try Chatml_lexer.token lexbuf with
      | Failure message -> raise (Lexer_failure message)
    in
    try
      let stmts = Chatml_parser.program token lexbuf in
      checkpoint ();
      Ok ({ stmts; source_text } : program)
    with
    | Chatml_parser.Error -> Error (diagnostic_of_lexbuf lexbuf "Syntax error")
    | Lexer_failure msg -> Error (diagnostic_of_lexbuf lexbuf msg))
;;

let parse_program source_text = parse source_text ~checkpoint:(fun () -> ())

let parse_captured source ~checkpoint =
  checkpoint ();
  parse (Chatml_source.contents source) ~checkpoint
  |> Result.map ~f:(fun program -> { Unit.source; program })
  |> Result.map_error ~f:(fun diagnostic ->
    Chatml_owned_source.attach source diagnostic ~checkpoint)
;;

let parse_program_exn (source_text : string) : program =
  match parse_program source_text with
  | Ok prog -> prog
  | Error diagnostic -> failwith (format_diagnostic source_text diagnostic)
;;
