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

module Cursor : sig
  module Raw_slice : sig
    (** An exact, owner-bound half-open authored-byte slice. Unlike a highlight
        [Range.t], the replacement prefix may be empty at EOF or a delimiter.
        It is not a UTF-16 coordinate or a parsed attribute value. Its bounded
        contents are retained once during cursor construction; repeated
        [contents] calls do not copy or rescan source bytes. *)
    type t

    val owner : t -> Source_owner.t
    val start_byte : t -> int
    val end_byte : t -> int
    val contents : t -> string
  end

  module Completed_attribute : sig
    type t

    (** Exact raw spelling and the strict lexer's decoded pair are separate.
        The raw value includes surrounding quotes when quoted; [None] means a
        flag attribute. Duplicate names remain in source order. *)
    val raw_name : t -> Raw_slice.t

    val raw_value : t -> Raw_slice.t option
    val decoded : t -> Chatmd_ast.attribute
  end

  module Value_state : sig
    type t

    val quote : t -> Chatmd_attribute_lex.Quote.t option
    val trailing_matching_quote_escape : t -> bool
    val open_entity : t -> bool
  end

  module Kind : sig
    type t =
      | Tag_name of { closing : bool }
      | Attribute_name of { tag : Chatmd_ast.tag }
      | Attribute_value of
          { tag : Chatmd_ast.tag
          ; attribute : string
          ; value_state : Value_state.t
          }
      | Text
      | Script_body
      | Opaque
  end

  type t

  (** This owner is always the actual Authored view of the admitted input,
      including its identity and digest. The cursor is between bytes in
      [0, authored_byte_length]. [raw_prefix] ends exactly at [cursor_byte] and
      is the current name/value spelling to replace; it may be zero width.
      For a quoted current value it starts AFTER the opening quote, so applying
      a suggestion cannot erase that delimiter. If the cursor is after a
      completed name and whitespace, after [=] but before a value, or after a
      closed quote, the prefix is zero width; [uncertain_discriminator] is true
      for the first two states and false after the closed quote.
      Text, script-body and opaque contexts use a zero-width prefix.
      [scope] is the last certain lexical scope before this construct, not a
      successful parse or declaration-validation result. *)
  val owner : t -> Source_owner.t

  val cursor_byte : t -> int
  val raw_prefix : t -> Raw_slice.t
  val kind : t -> Kind.t
  val scope : t -> Chatmd_tag_scope.t

  (** Only lexically completed attributes before the cursor. Values follow
      the shared strict decoder; raw slices remain available independently.
      A partial current attribute is excluded. *)
  val completed_attributes : t -> Completed_attribute.t list

  (** True when an unfinished name/value, quote, escape, or entity could still
      change a discriminator, or an earlier malformed official fragment makes
      scope untrustworthy. Pass this to the schema's prefix classifier with
      the decoded completed attributes; never treat raw spelling as certified
      decoded metadata. [Opaque] is never a valid suggestion scope. *)
  val uncertain_discriminator : t -> bool

  module Unavailable : sig
    type t =
      | Invalid_cursor
      | Beyond_scanned_prefix of
          { next_byte : int
          ; reason : Status.reason
          }
    [@@deriving equal, sexp_of]
  end

  type result =
    | Available of t
    | Unavailable of Unavailable.t
end

(** Reuse the bounded ChatMD highlighter's scanner and budget on the exact
    Authored prefix through [cursor_byte]. No second XML parser, synthetic
    closing text, source loading or semantic claim. Unknown tag lexemes stay
    TEXT and do not enter official scope; a known child after an unknown opener
    is still recognized. RAW/comment/script or mismatched contexts remain
    visibly lexical/opaque. Self-closing tags never enter scope. Invalid UTF-8
    remains byte-exact. If limits stop scanning before the cursor, return typed
    [Beyond_scanned_prefix]; callback failures and cancellation propagate.
    Source and identity admission errors match [scan_chatmd]. *)
val scan_chatmd_cursor
  :  Chatmd_parser_input.t
  -> cursor_byte:int
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (Cursor.result, Error.t) Result.t

(** Verify that [span] was emitted by this very snapshot. Snapshot identity is
    deliberate here: equal source bytes/revisions scanned under a different
    budget do not make another snapshot's span a member of this result. *)
val contains_span : t -> Span.t -> bool
