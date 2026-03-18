open Chatml_lang

type diagnostic =
  { message : string
  ; span : Source.span option
  }

val format_diagnostic : string -> diagnostic -> string

val parse_program : string -> (program, diagnostic) result

val parse_program_exn : string -> program
