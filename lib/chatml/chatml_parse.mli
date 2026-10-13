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

module Submission : sig
  (** Admission for one complete REPL submission. Uses the production program
      grammar and lexer on the exact captured bytes. No statement can be typed or
      evaluated from this result until [Complete] is returned. Existing batch
      parser entrypoints keep their current syntax and diagnostic behavior. *)

  module Limits : sig
    type t

    (** Positive source-byte, token (including physical EOF), and parser/lexer
        work ceilings; [sites] separately bounds retained identifier metadata.
        Work includes tokenization of long whitespace/comment/string regions and
        Menhir incremental transitions. Counts are checked before increment or
        allocation, and no limit can be reset during this parse. *)
    val create
      :  max_source_bytes:int
      -> max_tokens:int
      -> max_work_steps:int
      -> sites:Chatml_syntax_sites.Limits.t
      -> t Core.Or_error.t
  end

  module Need_more : sig
    type reason =
      | Parser_eof
      | Comment_eof
      | String_eof
    [@@deriving equal, sexp_of]

    type t

    val reason : t -> reason

    (** Zero-width physical EOF location bound to the exact source ID/digest.
        Parser_eof means the real parser rejected only the real EOF after
        accepting every preceding token; the lexer cases come from explicit
        unfinished lexical states. No guessed closing tokens or string-message
        matching participate. *)
    val at : t -> Chatml_source.Location.t
  end

  module Rejection : sig
    type reason =
      | Syntax of diagnostic
      | Source_byte_limit
      | Token_limit
      | Work_limit
      | Sites of Chatml_syntax_sites.Error.t

    type t

    val source : t -> Chatml_source.t
    val reason : t -> reason
  end

  type t =
    | Complete of Unit.t
    | Need_more of Need_more.t
    | Rejected of Rejection.t

  (** Parse the whole source once with source-bound identifier sites. [Need_more]
      is a viable unfinished prefix, not a generic error near EOF; bad tokens or
      an impossible parser continuation are [Rejected (Syntax ...)]. A complete
      AST and sites publish together. Rejected/incomplete parses publish neither
      AST nor site table. Poll [checkpoint] throughout, including long lexical
      regions and final publication; its exceptions and backtraces escape
      unchanged. This API performs no typecheck, resolver use or effects. *)
  val parse : Chatml_source.t -> limits:Limits.t -> checkpoint:(unit -> unit) -> t
end

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
