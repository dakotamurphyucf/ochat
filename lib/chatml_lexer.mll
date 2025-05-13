(***************************************************************************)
(*
    chatml_lexer.mll

    A minimal lexer for our DSL. 
    Produces: chatml_lexer.ml
*)
(***************************************************************************)

{
    open Chatml_parser
    open Lexing
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

    (* String literal (very naive, no escaping) *)
| '"' ([^ '"' '\n']* as s) '"'   { STRING s }

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