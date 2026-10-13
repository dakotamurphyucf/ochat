(** Parsing entrypoints for ChatML source text. *)

open Chatml_lang

(** Structured parser diagnostic. *)
type diagnostic = Chatml_diagnostic.t

module Unit : sig
  (** Parser syntax paired with exact immutable bytes and optional identifier
      metadata. Existing node spans are unchanged; no ambient source owner or
      rewritten offsets. *)
  type t

  val source : t -> Chatml_source.t
  val program : t -> program
  val sites : t -> Chatml_syntax_sites.t option
end

module Interface : sig
  (** Exact captured companion interface and its unchanged parsed syntax. This
      carries no checked schemes, inference cells or runtime authority. *)
  type t

  val source : t -> Chatml_source.t
  val signature : t -> Chatml_module_signature.t
  val sites : t -> Chatml_syntax_sites.t option
end

(** Metadata admission is separate from ordinary parse diagnostics. Existing
    entrypoints below retain no site table and produce unlocated syntax names. *)
type site_error =
  | Syntax of diagnostic
  | Sites of Chatml_syntax_sites.Error.t

module Top_level_prefix : sig
  (** Witnessed by reductions of the existing outer [program_stmts] grammar
      while parsing actual implementation bytes. Nested module statements and
      companion signatures do not establish a prefix. *)
  type t

  type error = Invalid_cursor

  (** Keep only the latest completed top-level end at or before [cursor_byte].
      Parse/lex failures after that boundary preserve it. Every scanned input
      chunk, token and completed reduction polls [checkpoint]; its exceptions
      propagate unchanged. No recovery tokens or alternate grammar are used. *)
  val find
    :  Chatml_source.t
    -> cursor_byte:int
    -> checkpoint:(unit -> unit)
    -> (t option, error) result

  val full_source : t -> Chatml_source.t
  val end_byte : t -> int
end

(** Parse the exact captured bytes with invocation-owned token coordinates on
    formerly unlocated name nodes. Bounds cover metadata only. Failed parsing or
    collection publishes no partial unit; checkpoints escape unchanged. *)
val parse_captured_with_sites
  :  Chatml_source.t
  -> limits:Chatml_syntax_sites.Limits.t
  -> checkpoint:(unit -> unit)
  -> (Unit.t, site_error) result

val parse_captured_interface_with_sites
  :  Chatml_source.t
  -> limits:Chatml_syntax_sites.Limits.t
  -> checkpoint:(unit -> unit)
  -> (Interface.t, site_error) result

(** Parse a captured interface using the same checkpoint-protected token path
    as programs. Failures own interface bytes; checkpoint exceptions escape. *)
val parse_captured_interface
  :  Chatml_source.t
  -> checkpoint:(unit -> unit)
  -> (Interface.t, diagnostic) result

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

(** Parse a companion [.chatmli] interface. This produces located signature
    syntax only; the module compiler must check names, constraints and the
    implementation before admitting any public interface. *)
val parse_interface : string -> (Chatml_module_signature.t, diagnostic) result
