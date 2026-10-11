(** Parsing entrypoints for ChatML source text. *)

open Chatml_lang

(** Structured parser diagnostic. *)
type diagnostic =
  { message : string
  ; span : Source.span option
  }

(** Render a parser diagnostic using the original source text. *)
val format_diagnostic : string -> diagnostic -> string

(** Parse a full ChatML program, returning either the AST or a structured
    diagnostic. *)
val parse_program : string -> (program, diagnostic) result

(** Exception-raising wrapper around {!parse_program}. *)
val parse_program_exn : string -> program

(** Parse a companion [.chatmli] interface. This produces located signature
    syntax only; the module compiler must check names, constraints and the
    implementation before admitting any public interface. *)
val parse_interface : string -> (Chatml_module_signature.t, diagnostic) result
