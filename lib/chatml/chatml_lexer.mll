

(** ChatML lexical analyser.

    This module is fed to {!ocamllex} which generates the concrete
    implementation [chatml_lexer.ml].  At runtime it is in charge of
    transforming the character stream contained in a {!Lexing.lexbuf}
    into the token stream consumed by {!module:Chatml_parser}.

    The recognised language is a small ML‐flavoured DSL whose concrete
    syntax is shared with the rest of the compiler in this
    repository.  The lexer therefore keeps track of the following
    categories:

    - keywords such as [let], [in] or [module]
    - identifiers (lower/upper-case and polymorphic variants)
    - literals — integers, floats, booleans and strings
    - operators and punctuation ([+], [->], [:=] …)
    - nested comments delimited by [( \* ... \* )] just like OCaml

    Location tracking is delegated to {!Lexing}.  Every time the lexer
    consumes a newline it calls {!Lexing.new_line} so that subsequent
    positions are reported correctly by Menhir when the parser builds
    spans.

    {1 Entry points}

    The interface exposed to the rest of the compiler is minimal; most
    users will only need {!val:token}.

    {1 Error handling}

    Malformed inputs are reported through [Failure] exceptions raised
    with a descriptive message.  The lexer never returns a special
    error token because Menhir’s incremental API makes exception based
    reporting simpler in this context.
*)


{
  open Chatml_tokens
  open Lexing

  (** ocamllex does not allow us to peek arbitrarily far, so we implement
     a second rule [string_lit] that consumes characters until it reaches
     the closing quote, interpreting a *very small* subset of OCaml
     escapes (/n, /t, /\, /'"').  Anything else after a backslash is
     kept verbatim so that we do not accidentally reject valid inputs
     that the interpreter might still handle. *)

  let _buffer_add_escaped buf c =
    Buffer.add_char buf c

  type incomplete =
    | Comment_eof
    | String_eof

  exception Incomplete of incomplete

  let charge_lexeme charge lexbuf =
    charge (Lexing.lexeme_end lexbuf - Lexing.lexeme_start lexbuf)

}

let white = [' ' '\t'] +
let newline = '\n' | '\r' | "\r\n"
rule token_checked charge = parse
    (* Whitespace and comments *)
| white  { charge_lexeme charge lexbuf; token_checked charge lexbuf }
| newline { charge_lexeme charge lexbuf; new_line lexbuf; token_checked charge lexbuf }
| "(*"                     { charge_lexeme charge lexbuf; comment charge 1 lexbuf }

    (* Keywords *)
| "fun"                    { FUN }
| "if"                     { IF }
| "then"                   { THEN }
| "else"                   { ELSE }
| "while"                  { WHILE }
| "do"                     { DO }
| "done"                   { DONE }
| "let"                    { LET }
| "let*"                   { LETSTAR }
| "let+"                   { LETPLUS }
| "in"                     { IN }
| "match"                  { MATCH }
| "with"                   { WITH }
| "module"                 { MODULE }
| "struct"                 { STRUCT }
| "end"                    { END }
| "open"                   { OPEN }
| "ref"                    { REF }
| "rec"                    { REC }
| "and"                    { AND }
| "type"                   { TYPE }
| "sig"                    { SIG }
| "val"                    { VAL }
| "where"                  { WHERE }

    (* Operators / punctuation *)
| "->"                     { ARROW }
| "<-"                     { LEFTARROW }
| ":"                      { COLON }
| ":="                     { COLONEQ }
| "!="                     { BANGEQ }
| "="                      { EQ }
| "=="                     { EQEQ }
| "<=."                    { LTEQDOT }
| "<="                     { LTEQ }
| "<."                     { LTDOT }
| "<"                      { LT }
| ">=."                    { GTEQDOT }
| ">="                     { GTEQ }
| ">."                     { GTDOT }
| ">"                      { GT }
| "++"                     { PLUSPLUS }
| "+."                     { PLUSDOT }
| "+"                      { PLUS }
| "-."                     { MINUSDOT }
| "-"                      { MINUS }
| "*."                     { STARDOT }
| "*"                      { STAR }
| "/."                     { SLASHDOT }
| "/"                      { SLASH }
| "("                      { LPAREN }
| "(|"                     { TUPLE_OPEN }
| "|)"                     { TUPLE_CLOSE }
| ")"                      { RPAREN }
| "{"                      { LBRACE }
| "}"                      { RBRACE }
| "["                      { LBRACKET }
| "]"                      { RBRACKET }
| ";;"                     { SEMISEMI }
| ";"                      { SEMI }
| ","                      { COMMA }
| "."                      { DOT }
| "|"                      { BAR }
| "_"                      { UNDERSCORE }
| "!"                      { BANG }

    (* Boolean literals. *)
| "true"                   { BOOL true }
| "false"                  { BOOL false }

    (* Integer literal *)
| ['0'-'9']+ as digits     { INT (int_of_string digits) }

    (* Float literal (simple) *)
| ['0'-'9']+ "." ['0'-'9']+ as flt { FLOAT (float_of_string flt) }

    (* String literal – supports basic OCaml-style escapes (\n, \t, \'"', \\)
       and can span multiple lines. *)
| '"' { charge_lexeme charge lexbuf; STRING (string_lit charge (Buffer.create 32) lexbuf) }

    (* Polymorphic variant: starts with backtick, followed by letters, digits, underscores. *)
| '`' ['A'-'Z''a'-'z''0'-'9' '_']+ as tickid {
    let name = String.sub tickid 1 (String.length tickid - 1) in
    TICKIDENT name
    }

    (* Lexically scoped alias/signature parameter, distinct from variant tags. *)
| '\'' ['a'-'z' '_']['a'-'z''A'-'Z''0'-'9' '_']* as parameter {
    TYPEVAR (String.sub parameter 1 (String.length parameter - 1))
    }

    (* Identifier: starts letter or underscore, followed by same + digits. *)
| ['a'-'z''A'-'Z' '_']['a'-'z''A'-'Z''0'-'9' '_']* as id {
    if id.[0] >= 'A' && id.[0] <= 'Z' then UIDENT id
    else LIDENT id
    }

| eof                       { EOF }

| _ as c {
    let pos = Lexing.lexeme_start_p lexbuf in
    failwith (Printf.sprintf "Unknown token '%c' at line %d, char %d"
        c pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol))
    }

and comment charge depth = parse
| "(*" { charge_lexeme charge lexbuf; comment charge (depth + 1) lexbuf }
| "*)" { charge_lexeme charge lexbuf; if depth = 1 then token_checked charge lexbuf else comment charge (depth - 1) lexbuf }
| newline { charge_lexeme charge lexbuf; new_line lexbuf; comment charge depth lexbuf }
| eof  { raise (Incomplete Comment_eof) }
| _    { charge_lexeme charge lexbuf; comment charge depth lexbuf }

and string_lit charge buf = parse
| '"'                           { charge_lexeme charge lexbuf; Buffer.contents buf }
| "\\n"                         { charge_lexeme charge lexbuf; Buffer.add_char buf '\n'; string_lit charge buf lexbuf }
| "\\t"                         { charge_lexeme charge lexbuf; Buffer.add_char buf '\t'; string_lit charge buf lexbuf }
| "\\\\"                       { charge_lexeme charge lexbuf; Buffer.add_char buf '\\'; string_lit charge buf lexbuf }
| "\\\""                         { charge_lexeme charge lexbuf; Buffer.add_char buf '"'; string_lit charge buf lexbuf }
| newline                     {
    charge_lexeme charge lexbuf;
    Buffer.add_char buf '\n';
    new_line lexbuf;
    string_lit charge buf lexbuf }
| eof                          { raise (Incomplete String_eof) }
| _                            { charge_lexeme charge lexbuf; Buffer.add_char buf (Lexing.lexeme_char lexbuf 0); string_lit charge buf lexbuf }

{
let token lexbuf =
  try token_checked (fun _ -> ()) lexbuf with
  | Incomplete Comment_eof -> failwith "Unterminated comment"
  | Incomplete String_eof -> failwith "Unterminated string literal"
}
