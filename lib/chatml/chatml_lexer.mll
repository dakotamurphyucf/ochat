

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
  open Chatml_parser
  open Lexing

  (** ocamllex does not allow us to peek arbitrarily far, so we implement
     a second rule [string_lit] that consumes characters until it reaches
     the closing quote, interpreting a *very small* subset of OCaml
     escapes (/n, /t, /\, /'"').  Anything else after a backslash is
     kept verbatim so that we do not accidentally reject valid inputs
     that the interpreter might still handle. *)

  let _buffer_add_escaped buf c =
    Buffer.add_char buf c

}

let white = [' ' '\t'] +
let newline = '\n' | '\r' | "\r\n"
rule token = parse
    (* Whitespace and comments *)
| white  { token lexbuf }
| newline { new_line lexbuf; token lexbuf }
| "(*"                     { comment lexbuf }

    (* Keywords *)
| "fun"                    { FUN }
| "if"                     { IF }
| "then"                   { THEN }
| "else"                   { ELSE }
| "while"                  { WHILE }
| "do"                     { DO }
| "done"                   { DONE }
| "let"                    { LET }
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

    (* Operators / punctuation *)
| "->"                     { ARROW }
| "<-"                     { LEFTARROW }
| ":="                     { COLONEQ }
| "!="                     { BANGEQ }
| "="                      { EQ }
| "=="                     { EQEQ }
| "<"                      { LT }
| ">"                      { GT }
| "<="                     { LTEQ }
| ">="                     { GTEQ }
| "+"                      { PLUS }
| "++"                     { PLUSPLUS }
| "-"                      { MINUS }
| "*"                      { STAR }
| "/"                      { SLASH }
| "("                      { LPAREN }
| ")"                      { RPAREN }
| "{"                      { LBRACE }
| "}"                      { RBRACE }
| "["                      { LBRACKET }
| "]"                      { RBRACKET }
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
| '"' { STRING (string_lit (Buffer.create 32) lexbuf) }

    (* Polymorphic variant: starts with backtick, followed by letters, digits, underscores. *)
| '`' ['A'-'Z''a'-'z''0'-'9' '_']+ as tickid {
    let name = String.sub tickid 1 (String.length tickid - 1) in
    TICKIDENT name
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

and comment = parse
| "*)" { token lexbuf }
| eof  { failwith "Unterminated comment" }
| _    { comment lexbuf }

and string_lit buf = parse
| '"'                           { Buffer.contents buf }
| "\\n"                         { Buffer.add_char buf '\n'; string_lit buf lexbuf }
| "\\t"                         { Buffer.add_char buf '\t'; string_lit buf lexbuf }
| "\\\\"                       { Buffer.add_char buf '\\'; string_lit buf lexbuf }
| "\\\""                         { Buffer.add_char buf '"'; string_lit buf lexbuf }
| newline                     {
    Buffer.add_char buf '\n';
    new_line lexbuf;
    string_lit buf lexbuf }
| eof                          { failwith "Unterminated string literal" }
| _                            { Buffer.add_char buf (Lexing.lexeme_char lexbuf 0); string_lit buf lexbuf }
