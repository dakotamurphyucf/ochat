(** Lightweight JSON highlighting for structured tool-call arguments. *)

(** [highlight text] returns one line of styled spans per input line.

    The lexer accepts incomplete JSON so streamed function arguments remain
    highlighted before the provider sends the closing delimiters. *)
val highlight : string -> Highlight_tm_engine.span list list
