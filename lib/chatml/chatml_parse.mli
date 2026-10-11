(** Parsing entrypoints for ChatML source text. *)

open Chatml_lang

(** Structured parser diagnostic. *)
type diagnostic = Chatml_diagnostic.t

module Unit : sig
  (** An unchanged parser AST paired with the exact immutable bytes from which
      it was parsed. No ambient source owner or rewritten offsets. *)
  type t

  val source : t -> Chatml_source.t
  val program : t -> program
end

(** Parse captured bytes. Failures retain a validated site when the lexer span
    is available. Checkpoints run during token consumption and site validation;
    their exceptions propagate unchanged. *)
val parse_captured
  :  Chatml_source.t
  -> checkpoint:(unit -> unit)
  -> (Unit.t, diagnostic) result

(** Render a parser diagnostic using the original source text. *)
val format_diagnostic : string -> diagnostic -> string

(** Parse a full ChatML program, returning either the AST or a structured
    diagnostic. *)
val parse_program : string -> (program, diagnostic) result

(** Exception-raising wrapper around {!parse_program}. *)
val parse_program_exn : string -> program
