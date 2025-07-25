(** ChatML lexical analyser.

    This interface restricts the public surface of the lexer to the
    single [token] function.  All helper rules generated by
    {!ocamllex} such as [comment] or [string_lit] remain
    implementation-private. *)

(** [token lb] returns the next {!Chatml_parser.token} extracted from
    [lb].  See the implementation for the full set of recognised
    tokens.

    @raise Failure if the lexer encounters an unknown character or an
    unterminated comment / string.

    Example:
    {[ let tokens =
         let lb = Lexing.from_string "let answer = 42" in
         let rec aux acc =
           match token lb with
           | Chatml_parser.EOF -> List.rev acc
           | tok               -> aux (tok :: acc)
         in
         aux [] ]}
 *)
val token : Lexing.lexbuf -> Chatml_parser.token
