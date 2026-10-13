open Core

(** Portable, lexical-only highlighting metadata for exact captured ChatML and
    ChatMD bytes. No compiler success, inferred type, resolved symbol or execution
    authority is implied by a span. This module has no Notty, TextMate, Eio,
    filesystem or transport dependency. Wire admission/version negotiation belong
    to the authoring protocol, not to this in-memory interface. *)

module Role : sig
  (** Version 1 lexical roles. Roles describe source spelling, not semantic
      meaning. In particular [Identifier] does not certify a symbol reference. *)
  type t =
    | Tag_name
    | Attribute_name
    | Attribute_value
    | Keyword
    | Identifier
    | Type_variable
    | Variant_tag
    | String_literal
    | Number_literal
    | Boolean_literal
    | Comment
    | Raw_delimiter
    | Raw_text
    | Script_body
    | Operator
    | Punctuation
  [@@deriving compare, equal, sexp_of]
end

module Source_owner : sig
  module Chatmd_view : sig
    type t =
      | Authored
      | Effective
    [@@deriving compare, equal, sexp_of]
  end

  (** Immutable revision owner. A ChatMD parser input may have distinct authored
      and effective bytes; the view and its SHA-256 digest prevent applying spans
      from one to the other. The input identity also binds role, logical path,
      both content digests and lengths. Neither identity is a file capability.
      Values are produced by [scan_chatml]/[scan_chatmd], never by a caller-supplied
      digest alone. *)
  type t = private
    | Chatml of
        { source_id : Chatml_source.Id.t
        ; source_digest : Chatml_source.Digest.t
        }
    | Chatmd of
        { input_identity : string
        ; view : Chatmd_view.t
        ; view_sha256 : string
        }

  val equal : t -> t -> bool
  val sexp_of_t : t -> Sexp.t
end

module Range : sig
  (** Nonempty half-open byte range of one exact source revision. CR, LF and CRLF
      remain unnormalized bytes. This is not a UTF-16 or Unicode scalar range.
      Invalid UTF-8 does not cause source rejection; use Chatml_editor_position
      for a validated editor-coordinate projection when available. *)
  type t

  val owner : t -> Source_owner.t
  val start_byte : t -> int
  val end_byte : t -> int
end

module Span : sig
  (** A validated source-bound lexical range and role. Spans are immutable and
      cannot be re-associated with a different source owner. *)
  type t

  val range : t -> Range.t
  val role : t -> Role.t
end

module Limits : sig
  (** Host-selected positive ceilings. Lexical work units count byte inspections,
      bounded lookahead and emitted spans; delimiter nesting and retained issues
      have separate ceilings. Admission checks source and owner sizes before
      scanning. The existing ChatMD input-identity hash is separately bounded
      by these input and owner byte ceilings and polls [checkpoint]. *)
  type t

  val create
    :  max_source_bytes:int
    -> max_identity_bytes:int
    -> max_spans:int
    -> max_issues:int
    -> max_work_units:int
    -> max_nesting:int
    -> t Or_error.t
end

module Status : sig
  type reason =
    | Span_limit
    | Issue_limit
    | Work_limit
    | Nesting_limit
  [@@deriving compare, equal, sexp_of]

  (** [Reached_eof] says only that the lexical scan reached EOF, not that the
      parser accepted the input. [Truncated] retains only a prefix through
      [next_byte]; no facts about the unscanned suffix are published. *)
  type t =
    | Reached_eof
    | Truncated of
        { next_byte : int
        ; reason : reason
        }
  [@@deriving compare, equal, sexp_of]
end

module Issue : sig
  type kind =
    | Open_comment
    | Open_string
    | Open_raw_block
    | Open_tag
    | Open_script
  [@@deriving compare, equal, sexp_of]

  (** A lexical observation, not a compiler diagnostic. Unmatched closing tags
      and conditional ChatMD tag validity remain parser-owned. *)
  type t

  val kind : t -> kind
  val range : t -> Range.t
end

module Error : sig
  type t =
    | Source_too_large
    | Identity_too_large
  [@@deriving compare, equal, sexp_of]
end

(** One immutable lexical result. Spans are ordered and nonoverlapping within
    the scanned prefix; unstyled gaps retain their source bytes. The snapshot
    borrows immutable captured strings but no mutable registry or callback. *)
type t

val version : t -> int
val source_owner : t -> Source_owner.t
val source_byte_length : t -> int

(** Exact borrowed bytes of the scanned view; no copy or normalization. *)
val source_contents : t -> string

val spans : t -> Span.t list
val issues : t -> Issue.t list
val status : t -> Status.t

(** Scan the exact captured ChatML bytes using the selected lexer vocabulary in
    a tolerant, bounded pass. Incomplete comments/strings return lexical spans
    and issues without calling the parser or claiming successful compilation.
    No source text is repaired, executed or fetched. *)
val scan_chatml
  :  Chatml_source.t
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

(** Scan the selected ChatMD byte view. [Authored] is appropriate for an
    editable authored document; [Effective] is the parser input after admitted
    preparation. OCH-259 owns any map between those views and an extracted
    inline ChatML source. External [src] bodies are highlighted against their
    own captured source, not synthesized within this snapshot. *)
val scan_chatmd
  :  Chatmd_parser_input.t
  -> view:Source_owner.Chatmd_view.t
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

(** Verify that [span] was emitted by this very snapshot. Snapshot identity is
    deliberate here: equal source bytes/revisions scanned under a different
    budget do not make another snapshot's span a member of this result. *)
val contains_span : t -> Span.t -> bool
