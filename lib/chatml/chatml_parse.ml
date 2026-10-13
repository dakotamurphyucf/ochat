open Core
open Chatml_lang

type diagnostic = Chatml_diagnostic.t

type site_error =
  | Syntax of diagnostic
  | Sites of Chatml_syntax_sites.Error.t

module Parser = Chatml_parser.Make (Chatml_syntax_sites.Disabled)

module Unit = struct
  type t =
    { source : Chatml_source.t
    ; program : program
    ; sites : Chatml_syntax_sites.t option
    }

  let source t = t.source
  let program t = t.program
  let sites t = t.sites
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
exception Parser_failure

let program_entrypoint token lexbuf =
  try Parser.program token lexbuf with
  | Parser.Error -> raise Parser_failure
;;

let interface_entrypoint token lexbuf =
  try Parser.module_interface token lexbuf with
  | Parser.Error -> raise Parser_failure
;;

let parse_with
      ?(poll_tokens = 64)
      ?(chunked_input = false)
      entrypoint
      source_text
      ~checkpoint
  =
  Chatml_checkpoint.protect checkpoint ~f:(fun checkpoint ->
    let lexbuf =
      if not chunked_input
      then Lexing.from_string source_text
      else (
        let next = ref 0 in
        Lexing.from_function (fun destination requested ->
          checkpoint ();
          let available = String.length source_text - !next in
          let count = Int.min 4096 (Int.min requested available) in
          Stdlib.Bytes.blit_string source_text !next destination 0 count;
          next := !next + count;
          count))
    in
    let remaining = ref 0 in
    let token lexbuf =
      decr remaining;
      if !remaining <= 0
      then (
        remaining := poll_tokens;
        checkpoint ());
      try Chatml_lexer.token lexbuf with
      | Failure message -> raise (Lexer_failure message)
    in
    try
      let syntax = entrypoint token lexbuf in
      checkpoint ();
      Ok syntax
    with
    | Parser_failure -> Error (diagnostic_of_lexbuf lexbuf "Syntax error")
    | Lexer_failure message -> Error (diagnostic_of_lexbuf lexbuf message))
;;

module Top_level_prefix = struct
  type t =
    { full_source : Chatml_source.t
    ; end_byte : int
    }

  type error = Invalid_cursor

  let full_source t = t.full_source
  let end_byte t = t.end_byte

  let find source ~cursor_byte ~checkpoint =
    Chatml_checkpoint.protect checkpoint ~f:(fun checkpoint ->
      checkpoint ();
      if cursor_byte < 0 || cursor_byte > Chatml_source.byte_length source
      then Error Invalid_cursor
      else (
        let latest = ref None in
        let module Sink = struct
          let enabled = false
          let add _ ~name:_ ~span:_ = assert false

          let completed_program_statement ~end_byte =
            checkpoint ();
            if end_byte <= cursor_byte then latest := Some end_byte
          ;;

          let completed_program_separator ~end_byte =
            checkpoint ();
            if end_byte <= cursor_byte && Option.is_some !latest
            then latest := Some end_byte
          ;;
        end
        in
        let module Prefix_parser = Chatml_parser.Make (Sink) in
        let entrypoint token lexbuf =
          try Prefix_parser.program token lexbuf with
          | Prefix_parser.Error -> raise Parser_failure
        in
        ignore
          (parse_with
             ~poll_tokens:1
             ~chunked_input:true
             entrypoint
             (Chatml_source.contents source)
             ~checkpoint
           : (Chatml_lang.stmt_node list, diagnostic) result);
        checkpoint ();
        Ok (Option.map !latest ~f:(fun end_byte -> { full_source = source; end_byte }))))
  ;;
end

let parse_program_with_checkpoint source_text ~checkpoint =
  parse_with program_entrypoint source_text ~checkpoint
  |> Result.map ~f:(fun stmts -> ({ stmts; source_text } : program))
;;

let parse_program source_text =
  parse_program_with_checkpoint source_text ~checkpoint:(fun () -> ())
;;

let parse_captured source ~checkpoint =
  checkpoint ();
  parse_program_with_checkpoint (Chatml_source.contents source) ~checkpoint
  |> Result.map ~f:(fun program -> { Unit.source; program; sites = None })
  |> Result.map_error ~f:(fun diagnostic ->
    Chatml_owned_source.attach source diagnostic ~checkpoint)
;;

module Interface = struct
  type t =
    { source : Chatml_source.t
    ; signature : Chatml_module_signature.t
    ; sites : Chatml_syntax_sites.t option
    }

  let source t = t.source
  let signature t = t.signature
  let sites t = t.sites
end

let parse_interface source_text =
  parse_with interface_entrypoint source_text ~checkpoint:(fun () -> ())
;;

let parse_captured_interface source ~checkpoint =
  checkpoint ();
  parse_with interface_entrypoint (Chatml_source.contents source) ~checkpoint
  |> Result.map ~f:(fun signature -> { Interface.source; signature; sites = None })
  |> Result.map_error ~f:(fun diagnostic ->
    Chatml_owned_source.attach source diagnostic ~checkpoint)
;;

let with_sites source ~limits ~checkpoint ~parse ~make =
  let exception Rejected_syntax of diagnostic in
  try
    Chatml_syntax_sites.collect source ~limits ~checkpoint ~f:(fun sink ->
      match parse sink (Chatml_source.contents source) ~checkpoint with
      | Ok syntax -> syntax
      | Error diagnostic -> raise (Rejected_syntax diagnostic))
    |> Result.map_error ~f:(fun error -> Sites error)
    |> Result.map ~f:(fun (syntax, sites) -> make syntax sites)
  with
  | Rejected_syntax diagnostic ->
    Error (Syntax (Chatml_owned_source.attach source diagnostic ~checkpoint))
;;

let parse_captured_with_sites source ~limits ~checkpoint =
  let parse (module Sink : Chatml_syntax_sites.Sink) text ~checkpoint =
    let module Parser = Chatml_parser.Make (Sink) in
    let entrypoint token lexbuf =
      try Parser.program token lexbuf with
      | Parser.Error -> raise Parser_failure
    in
    parse_with entrypoint text ~checkpoint
    |> Result.map ~f:(fun stmts -> ({ stmts; source_text = text } : program))
  in
  with_sites source ~limits ~checkpoint ~parse ~make:(fun program sites ->
    { Unit.source; program; sites = Some sites })
;;

let parse_captured_interface_with_sites source ~limits ~checkpoint =
  let parse (module Sink : Chatml_syntax_sites.Sink) text ~checkpoint =
    let module Parser = Chatml_parser.Make (Sink) in
    let entrypoint token lexbuf =
      try Parser.module_interface token lexbuf with
      | Parser.Error -> raise Parser_failure
    in
    parse_with entrypoint text ~checkpoint
  in
  with_sites source ~limits ~checkpoint ~parse ~make:(fun signature sites ->
    { Interface.source; signature; sites = Some sites })
;;

let parse_program_exn (source_text : string) : program =
  match parse_program source_text with
  | Ok prog -> prog
  | Error diagnostic -> failwith (format_diagnostic source_text diagnostic)
;;
