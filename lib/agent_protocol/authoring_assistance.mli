(** Portable lexical and early authoring DTOs. Byte scans and suggestions never
    certify parser/type success or grant source/catalog access. The host binds
    these values to its current admitted context and actual captured bytes;
    this library depends on neither the compiler, highlighter implementation,
    filesystem, Eio nor terminal UI. *)

module Context = Authoring_context_core.Context
module Source = Authoring_common.Source

module Limits : sig
  (** Fixed portable result ceilings, also constrained by the initialized
      host's existing request/result byte limits. Counting includes labels,
      insert text, owners, issues and spans before JSON construction. *)
  val max_result_bytes : int

  val max_spans : int
  val max_issues : int
  val max_suggestions : int
  val max_label_bytes : int
end

module Lexical : sig
  module View : sig
    type t =
      | Chatml
      | Chatmd_authored
      | Chatmd_effective
    [@@deriving compare, equal, sexp_of]
  end

  module Request : sig
    (** [source] is the current captured document/ChatML revision. ChatMD
        effective-byte highlighting is selected explicitly; its response owner
        retains a distinct view digest/length. No UTF-8 position conversion is
        required for lexical bytes. *)
    type t

    val create
      :  context:Context.Expected.t
      -> source:Source.t
      -> view:View.t
      -> (t, Error.t) result

    val context : t -> Context.Expected.t
    val source : t -> Source.t
    val view : t -> View.t
    val equal : t -> t -> bool
    val sexp_of_t : t -> Core.Sexp.t
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) result
  end

  module Owner : sig
    type chatmd_view =
      | Authored
      | Effective
    [@@deriving compare, equal, sexp_of]

    type t = private
      | Chatml of Source.t
      | Chatmd of
          { captured_source : Source.t
          ; input_identity : Chatml_source.Digest.t
          ; view : chatmd_view
          ; view_sha256 : Chatml_source.Digest.t
          ; view_byte_length : int
          }

    (** The host derives the ChatMD input identity from actual parser input.
        Authored digest and length must equal [captured_source]; Effective may
        differ. A client cannot apply Effective spans to the authored edit
        buffer. *)
    val chatml : Source.t -> t

    val chatmd
      :  captured_source:Source.t
      -> input_identity:Chatml_source.Digest.t
      -> view:chatmd_view
      -> view_sha256:Chatml_source.Digest.t
      -> view_byte_length:int
      -> (t, Error.t) result

    val byte_length : t -> int
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) result
  end

  module Range : sig
    (** Nonempty half-open byte span inside one exact Owner. Invalid UTF-8 is
        permitted; CRLF remains byte-exact. *)
    type t

    val create : owner:Owner.t -> start_byte:int -> end_byte:int -> (t, Error.t) result
    val owner : t -> Owner.t
    val start_byte : t -> int
    val end_byte : t -> int
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) result
  end

  module Role : sig
    (** Version-1 roles of the portable highlighter, without inferred symbols. *)
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

  module Span : sig
    type t

    val create : range:Range.t -> role:Role.t -> t
    val range : t -> Range.t
    val role : t -> Role.t
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) result
  end

  module Issue : sig
    type kind =
      | Open_comment
      | Open_string
      | Open_raw_block
      | Open_tag
      | Open_script
    [@@deriving compare, equal, sexp_of]

    type t

    val create : kind:kind -> range:Range.t -> t
    val kind : t -> kind
    val range : t -> Range.t
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) result
  end

  module Status : sig
    type reason =
      | Span_limit
      | Issue_limit
      | Work_limit
      | Nesting_limit
    [@@deriving compare, equal, sexp_of]

    type t =
      | Reached_eof
      | Truncated of
          { next_byte : int
          ; reason : reason
          }
    [@@deriving compare, equal, sexp_of]
  end

  module Result : sig
    (** Spans are ordered/nonoverlapping and issues lie inside the scanned
        prefix. [Request.view] and [Owner] must select the same ChatML or
        ChatMD authored/effective view and exact captured source. Every range
        ends at or before [next_byte] when Truncated, or the owner's byte
        length on Reached_eof. The latter is LEXICAL completion only. Aggregate
        span/issue/owner/label bytes and JSON nodes/depth are checked before
        whole-result construction and again during decoding. *)
    type t

    val create
      :  request:Request.t
      -> owner:Owner.t
      -> spans:Span.t list
      -> issues:Issue.t list
      -> status:Status.t
      -> (t, Error.t) result

    val request : t -> Request.t
    val owner : t -> Owner.t
    val spans : t -> Span.t list
    val issues : t -> Issue.t list
    val status : t -> Status.t
    val sexp_of_t : t -> Core.Sexp.t
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) result
  end
end

module Assistance : sig
  module Request : sig
    (** The host derives parser/embedded script context and any separately
        authorized catalog at this exact byte cursor. A document-only context
        with no Build can still ask for basic local markup help. A client path
        or environment name is never resolver authority. *)
    type t

    val create
      :  context:Context.Expected.t
      -> source:Source.t
      -> cursor_byte:int
      -> max_items:int
      -> (t, Error.t) result

    val context : t -> Context.Expected.t
    val source : t -> Source.t
    val cursor_byte : t -> int
    val max_items : t -> int
    val equal : t -> t -> bool
    val sexp_of_t : t -> Core.Sexp.t
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) result
  end

  module Selected_catalog : sig
    (** One authentic selected Build for the particular embedded script, when
        a suggestion actually needs admitted closure or compiler contract.
        Basic grammar and parsed environment-name help require no Build. The
        handler rechecks source/catalog authority before publication; this
        comparison stamp grants no path access. *)
    type t

    val create
      :  environment:string
      -> input_fingerprint:Chatml_source.Digest.t
      -> compiler_contract:Chatml_source.Digest.t
      -> (t, Error.t) result

    val environment : t -> string
    val input_fingerprint : t -> Chatml_source.Digest.t
    val compiler_contract : t -> Chatml_source.Digest.t
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) result
  end

  (** Reuse the semantic protocol's exact source-bound, zero-width-capable byte
      range. The host admits replacement only against the current AUTHORED
      source, projecting embedded effective coordinates through the real map;
      the range itself does not grant edit authority. *)
  module Range = Authoring_analysis.Byte_range

  module Suggestion : sig
    type kind =
      | Tag
      | Attribute
      | Value
      | Reference
      | Path
      | Template
      | Environment
    [@@deriving compare, equal, sexp_of]

    type evidence =
      | Lexical
      | Parsed_document of Source.t
      | Parsed_project of Source.t
      | Installed_builtin of
          { id : string
          ; sha256 : Chatml_source.Digest.t
          }
      | Selected_catalog of Selected_catalog.t
      | Host_root of Id.Authoring_root.t
    [@@deriving sexp_of]

    (** Admitted evidence matrix: Tag/Attribute/Value use Lexical or
        Parsed_document; Reference uses Parsed_document or Selected_catalog;
        Path uses Parsed_project for a declared path, or Host_root or
        Selected_catalog for a separately authorized member; Template uses
        Lexical static syntax, Parsed_document or Installed_builtin;
        Environment uses Parsed_project or Selected_catalog. Parsed_document
        refers to genuine captured parser input. Parsed_project identifies the
        actual captured project declaration whose environment names were
        decoded, even when selecting one would fail. Installed_builtin comes
        from a found embedded asset, not a caller-named catalog. The host
        authenticates every source/root/asset stamp before emission and must
        not present declared paths as verified files. No suggestion certifies
        a semantic binding. *)
    type t

    val create
      :  kind:kind
      -> label:string
      -> insert_text:string
      -> replace:Range.t
      -> evidence:evidence
      -> (t, Error.t) result

    val kind : t -> kind
    val label : t -> string
    val insert_text : t -> string
    val evidence : t -> evidence
    val replace : t -> Range.t
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) result
  end

  module Status : sig
    type unavailable =
      | No_local_context
      | Catalog_unavailable
      | Source_map_unavailable
    [@@deriving compare, equal, sexp_of]

    type truncation =
      | Item_limit
      | Work_limit
      | Result_bytes
    [@@deriving compare, equal, sexp_of]

    type t =
      | Complete
      | Truncated of truncation
      | Unavailable of unavailable
    [@@deriving compare, equal, sexp_of]
  end

  module Result : sig
    (** Empty Complete is an available empty set, not Unavailable. The client
        checks echoed context/generation/source, every replacement range and
        selected method feature before applying an item. *)
    type t

    val create
      :  request:Request.t
      -> items:Suggestion.t list
      -> status:Status.t
      -> (t, Error.t) result

    val request : t -> Request.t
    val items : t -> Suggestion.t list
    val status : t -> Status.t
    val sexp_of_t : t -> Core.Sexp.t
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) result
  end
end
