# Protocol types and implementation reference

Generated from the current public interfaces. Do not hand-edit the excerpts.
The [protocol guide](protocol.md) explains operation semantics and authorization.
Each section includes the complete typed contract and its implementation.
Modules with wire codecs define their tags/defaults there, not through OCaml
constructor spelling.

## activity

[implementation](../../lib/agent_protocol/activity.ml) · [interface](../../lib/agent_protocol/activity.mli)

```ocaml
(** Neutral transient tool activity. A call alias is scoped to an actual source
    and attempt; it never substitutes for a host history or invocation ID. *)
module Key : sig
  type parent =
    { scope : Transcript.Scope.Key.t
    ; call_alias : string
    }
  [@@deriving compare, equal, hash, sexp_of]

  type t = private
    { scope : Transcript.Scope.Key.t
    ; call_alias : string
    ; parent : parent option
    }
  [@@deriving compare, equal, hash, sexp_of]

  val create
    :  scope:Transcript.Scope.Key.t
    -> call_alias:string
    -> parent:parent option
    -> (t, Error.t) result
end

module Progress : sig
  type channel =
    | Assistant
    | Reasoning
    | Stdout
    | Stderr
    | Activity
  [@@deriving compare, equal, sexp_of]

  type update =
    | Append of string
    | Replace of string
  [@@deriving equal, sexp_of]

  type t =
    { channel : channel
    ; update : update
    }
  [@@deriving equal, sexp_of]
end

module Tool : sig
  type classification =
    | Subagent
    | Shell_script
  [@@deriving equal, sexp_of]

  type outcome =
    | Returned
    | Raised
    | Cancelled
  [@@deriving equal, sexp_of]

  type descriptor = private
    { key : Key.t
    ; call_entry_id : History_entry.Id.t option
    ; name : string
    ; kind : History_entry.Payload.Call_kind.t
    ; input : string
    ; classification : classification option
    }
  [@@deriving sexp_of]

  val descriptor
    :  Key.t
    -> call_entry_id:History_entry.Id.t option
    -> name:string
    -> kind:History_entry.Payload.Call_kind.t
    -> input:string
    -> classification:classification option
    -> (descriptor, Error.t) result

  type event =
    | Started of descriptor
    | Progress of
        { key : Key.t
        ; progress : Progress.t
        }
    | Finished of
        { key : Key.t
        ; outcome : outcome
        ; output : History_entry.Payload.Output.t option
        }
  [@@deriving sexp_of]

  (** Nested activity records the actual parent scope and call alias. Only existing
      actual parent attribution is supplied; no guessed parent or host ID. *)
  val key : event -> Key.t

  val to_json : event -> Jsonaf.t
  val of_json : Jsonaf.t -> (event, Error.t) result

  type channel_text =
    { channel : Progress.channel
    ; text : string
    ; complete : bool
    }
  [@@deriving sexp_of]

  type state =
    | Running
    | Finished of
        { outcome : outcome
        ; output : History_entry.Payload.Output.t option
        }
  [@@deriving sexp_of]

  type summary = private
    { key : Key.t
    ; descriptor : descriptor option
    ; channels : channel_text list
    ; state : state
    }
  [@@deriving sexp_of]

  (** Missing descriptors and incomplete channel prefixes remain explicit after
      a live gap. The client ordering owner performs bounded accumulation. *)
  val summary
    :  Key.t
    -> descriptor:descriptor option
    -> channels:channel_text list
    -> state:state
    -> (summary, Error.t) result

  val summary_to_json : summary -> Jsonaf.t
  val summary_of_json : Jsonaf.t -> (summary, Error.t) result
end
```

## activity_query

[implementation](../../lib/agent_protocol/activity_query.ml) · [interface](../../lib/agent_protocol/activity_query.mli)

```ocaml
(** Bounded cross-session query. The scan bound is checked against the authorized
    matching catalog before immutable state IO. Cursor remains a top-level field;
    reasons and the bound are part of its authenticated query binding. *)
type t = private
  { server_id : Id.Server.t
  ; catalog : Session.List_request.t
  ; reasons : Session_activity.Reason.t list
  ; scan_limit : int
  }
[@@deriving sexp]

val create
  :  server_id:Id.Server.t
  -> catalog:Session.List_request.t
  -> reasons:Session_activity.Reason.t list
  -> scan_limit:int
  -> (t, Error.t) result

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
```

## audit

[implementation](../../lib/agent_protocol/audit.ml) · [interface](../../lib/agent_protocol/audit.mli)

```ocaml
(** Redacted, cursor-paged server and session audit records. *)

type level =
  | Info
  | Warning
  | Error
[@@deriving compare, equal, sexp]

type t =
  { sequence : int64
  ; timestamp : Timestamp.t
  ; level : level
  ; name : string
  ; session_id : Id.Session.t option
  ; principal_id : Id.Principal.t option
  ; payload : Jsonaf.t
  ; redacted : bool
  }
[@@deriving sexp]

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result

module Read_request : sig
  type t =
    { page : Page.Request.t
    ; session_id : Id.Session.t option
    ; principal_id : Id.Principal.t option
    ; minimum_level : level option
    ; name_prefix : string option
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end
```

## authoring

[implementation](../../lib/agent_protocol/authoring.ml) · [interface](../../lib/agent_protocol/authoring.mli)

```ocaml
(** Thin public facade for the portable authoring wire contracts. DTO modules
    are split by ownership so codecs do not become one monolithic component. *)

module Features = Authoring_common.Features
module Encoding = Authoring_common.Encoding
module Method = Authoring_common.Method
module Failure = Authoring_common.Failure
module Limits = Authoring_common.Limits
module Capabilities = Authoring_common.Capabilities
module Source = Authoring_common.Source
module Position = Authoring_common.Position
module Site = Authoring_common.Site
module Context = Authoring_context.Context
module Document = Authoring_context.Document
module Source_read = Authoring_context.Source_read
module Bundle = Authoring_bundle.Bundle
module Analysis = Authoring_analysis
module Assistance = Authoring_assistance
module Wire = Authoring_wire
```

## authoring_analysis

[implementation](../../lib/agent_protocol/authoring_analysis.ml) · [interface](../../lib/agent_protocol/authoring_analysis.mli)

```ocaml
(** Portable analysis and query DTOs. These values are comparison evidence, not
    source access or proof that a host ran the checker. The host constructs them
    only from its current admitted closure and the public semantic facade; the
    client checks them against its current context and requested evidence. No
    compiler, Eio, runtime or UI dependency belongs in this library.

    Textual wire labels, messages, names and environment identifiers require
    valid UTF-8 and return a typed constructor/decoder error otherwise. This
    never changes compiler acceptance of arbitrary captured source bytes or
    their native byte sites. A host must explicitly render unrepresentable
    diagnostic text as escaped bytes or return a typed unavailable projection;
    it cannot silently normalize or discard source evidence. *)

module Context = Authoring_context_core.Context
module Source = Authoring_common.Source

module Limits : sig
  (** Fixed protocol ceilings; a host may select smaller request/result limits
      through the existing authoring capabilities. Every constructor checks
      retained counts and aggregate label bytes before building JSON. Decoders
      also enforce the outer encoded-byte/node/depth budget. *)
  val max_result_bytes : int

  val max_diagnostics : int
  val max_syntax_sites : int
  val max_query_items : int
  val max_label_bytes : int
  val max_display_bytes : int
end

module Byte_range : sig
  (** Exact half-open byte site. Zero-width sites are allowed for cursors and
      diagnostics. Construction checks 0 <= start <= finish <= source length;
      a later host projection checks the actual captured source ID/digest. *)
  type t

  val create : source:Source.t -> start_byte:int -> end_byte:int -> (t, Error.t) result
  val source : t -> Source.t
  val start_byte : t -> int
  val end_byte : t -> int
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Site : sig
  (** The compiler-owned [native] location is always retained. An embedded
      ChatMD projection is optional and never replaces or relabels that native
      source. [Authored_exact] alone is safe for a precise editor selection;
      an approximate declaration is a related display site only. An external
      member is unresolved until the host separately proves captured custody. *)
  type projection =
    | Authored_exact of Byte_range.t
    | External_origin of
        { member : string
        ; body_digest : Chatml_source.Digest.t
        ; start_byte : int
        ; end_byte : int
        }
    | Approximate_declaration of Byte_range.t

  type t

  val create : native:Byte_range.t -> projection:projection option -> (t, Error.t) result
  val native : t -> Byte_range.t
  val projection : t -> projection option
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Target : sig
  (** A request selects a project or one parser-assigned script ID in the
      current document context. The client does not supply a Build, contract,
      graph or parser fingerprint before its first analysis. The host resolves
      a script ID against the actual current Parsed declaration registry;
      missing or ambiguous selection never guesses a path. *)
  type selector = private
    | Project
    | Selected_script of string
  [@@deriving compare, equal, sexp_of]

  val project : selector
  val selected_script : string -> (selector, Error.t) result
  val selector_to_json : selector -> Jsonaf.t
  val selector_of_json : Jsonaf.t -> (selector, Error.t) result

  (** Host-derived comparison stamp for one genuinely selected declaration.
      The two source revisions and parser/declaration fingerprints are distinct:
      a document edit, changed preprocessor output or different selected graph
      cannot silently reuse the old result. None is a source grant. *)
  type script_stamp

  val script_stamp
    :  document_source:Source.t
    -> parser_root_fingerprint:Chatml_source.Digest.t
    -> script_id:string
    -> declaration_input_fingerprint:Chatml_source.Digest.t
    -> declaration_input_byte_length:int
    -> declaration_start_byte:int
    -> declaration_end_byte:int
    -> selected_script_source:Source.t
    -> (script_stamp, Error.t) result

  val document_source : script_stamp -> Source.t
  val parser_root_fingerprint : script_stamp -> Chatml_source.Digest.t
  val script_id : script_stamp -> string
  val declaration_input_fingerprint : script_stamp -> Chatml_source.Digest.t
  val declaration_input_byte_length : script_stamp -> int
  val declaration_start_byte : script_stamp -> int
  val declaration_end_byte : script_stamp -> int
  val selected_script_source : script_stamp -> Source.t

  type resolved =
    | Project_input
    | Script_input of script_stamp

  val selector : resolved -> selector
  val resolved_to_json : resolved -> Jsonaf.t
  val resolved_of_json : Jsonaf.t -> (resolved, Error.t) result
end

module Capture : sig
  (** Full input identity. [input_fingerprint] includes the selected exact
      source closure, entry, provenance, environment and compiler contract.
      It is meaningful for Complete and Failed, without implying success.
      The selected contract and environment come from authentic host admission,
      not client labels. [context] binds the whole overlay generation. *)
  type t

  val create
    :  context:Context.Expected.t
    -> target:Target.resolved
    -> input_fingerprint:Chatml_source.Digest.t
    -> environment:string
    -> required_contract:Chatml_source.Digest.t
    -> selected_contract:Chatml_source.Digest.t
    -> (t, Error.t) result

  val context : t -> Context.Expected.t
  val target : t -> Target.resolved
  val input_fingerprint : t -> Chatml_source.Digest.t
  val environment : t -> string
  val required_contract : t -> Chatml_source.Digest.t
  val selected_contract : t -> Chatml_source.Digest.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Snapshot_id : sig
  (** Host-minted, bounded snapshot-incarnation label. Equal bytes do not make
      independently published snapshots share an owner. Not a bearer token. *)
  type t [@@deriving compare, equal, sexp_of]

  val create : string -> (t, Error.t) result
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Diagnostic : sig
  module Related : sig
    type t

    val create : site:Site.t -> message:string -> (t, Error.t) result
    val site : t -> Site.t
    val message : t -> string
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) result
  end

  type t

  (** Actual Chatml_diagnostic has a message, optional owned site and related
      sites, not a severity or machine code. A missing site is distinct from a
      zero-width site; no legacy unowned span becomes a source claim. *)
  val create
    :  message:string
    -> site:Site.t option
    -> related:Related.t list
    -> (t, Error.t) result

  val message : t -> string
  val site : t -> Site.t option
  val related : t -> Related.t list
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Syntax_site : sig
  (** Parser-owned same-run syntax only, never a certified binding, type or
      reference. These rows are allowed on Failed, including earlier units that
      parsed before a later unit failed. *)
  type role =
    | Value_reference
    | Value_binder
    | Module_binder
    | Module_reference
    | Type_binder
    | Type_parameter_binder
    | Type_reference
    | Signature_value_binder
    | Field_label
    | Variant_tag
    | Constraint_name
    | Constraint_parameter_reference
  [@@deriving compare, equal, sexp_of]

  type t

  val create : role:role -> name:string -> site:Site.t -> (t, Error.t) result
  val role : t -> role
  val name : t -> string
  val site : t -> Site.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Analyze : sig
  module Request : sig
    type t

    val create
      :  context:Context.Expected.t
      -> target:Target.selector
      -> (t, Error.t) result

    val context : t -> Context.Expected.t
    val target : t -> Target.selector
    val equal : t -> t -> bool
    val sexp_of_t : t -> Core.Sexp.t
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) result
  end

  type parser_phase =
    | Implementation
    | Interface
  [@@deriving compare, equal, sexp_of]

  type failure_phase =
    | Parse of
        { source : Source.t
        ; phase : parser_phase
        }
    | Implementation of Source.t
    | Companion of Source.t
    | Host_requirements
    | Dependency
  [@@deriving equal, sexp_of]

  type unavailable =
    | Document_basis
    | No_checked_closure
    | No_selected_script
    | Ambiguous_script_id
    | No_validated_project
    | Incomplete_or_malformed_document
    | Dynamic_only_source
    | Unsupported_target
  [@@deriving compare, equal, sexp_of]

  (** A Failed result carries no Snapshot_id, certified item, graph order or
      type root. [syntax] and diagnostics come from exactly this check run.
      Complete binds a real host-owned immutable snapshot and validated graph;
      its diagnostics may include warnings. Every list and string has both
      per-field and aggregate result bounds. *)
  type t = private
    | Complete of
        { capture : Capture.t
        ; snapshot : Snapshot_id.t
        ; validated_graph : Chatml_source.Digest.t
        ; diagnostics : Diagnostic.t list
        ; truncated_diagnostics : bool
        }
    | Failed of
        { capture : Capture.t
        ; phase : failure_phase
        ; syntax : Syntax_site.t list
        ; diagnostics : Diagnostic.t list
        ; truncated_syntax : bool
        ; truncated_diagnostics : bool
        }
    | Unavailable of
        { context : Context.Expected.t
        ; target : Target.selector
        ; reason : unavailable
        }

  val complete
    :  capture:Capture.t
    -> snapshot:Snapshot_id.t
    -> validated_graph:Chatml_source.Digest.t
    -> diagnostics:Diagnostic.t list
    -> truncated_diagnostics:bool
    -> (t, Error.t) result

  val failed
    :  capture:Capture.t
    -> phase:failure_phase
    -> syntax:Syntax_site.t list
    -> diagnostics:Diagnostic.t list
    -> truncated_syntax:bool
    -> truncated_diagnostics:bool
    -> (t, Error.t) result

  (** A raw document has no project Analysis_key, even when a separately
      authorized static Build is attached. A genuinely selected and admitted
      script can still be analyzed under that Document context. A project
      without an admitted checked closure is unavailable. These are not
      compiler failures; denial, stale context and exhausted work use protocol
      errors. Reason/target combinations are validated by this constructor. *)
  val unavailable
    :  context:Context.Expected.t
    -> target:Target.selector
    -> reason:unavailable
    -> (t, Error.t) result

  val capture : t -> Capture.t option
  val sexp_of_t : t -> Core.Sexp.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Prefix : sig
  (** Full and prefix owners remain separate. [full_source] is the current
      source revision; [prefix_source] is the parser-confirmed prefix revision
      under the same source ID, normally with another digest/length. The prefix
      graph can be queried provisionally only, never run or relabelled Complete.
      Unavailable carries a reason, not an empty successful prefix. *)
  type unavailable =
    | No_complete_statement
    | Unsupported_source_phase
    | Unchecked_source
    | Prefix_rejected of Analyze.failure_phase
    | Invalid_rebased_metadata
  [@@deriving equal, sexp_of]

  type provisional

  val create
    :  capture:Capture.t
    -> full_source:Source.t
    -> prefix_source:Source.t
    -> cursor_byte:int
    -> end_byte:int
    -> prefix_graph:Chatml_source.Digest.t
    -> snapshot:Snapshot_id.t
    -> (provisional, Error.t) result

  val capture : provisional -> Capture.t
  val full_source : provisional -> Source.t
  val prefix_source : provisional -> Source.t
  val cursor_byte : provisional -> int
  val end_byte : provisional -> int
  val prefix_graph : provisional -> Chatml_source.Digest.t
  val snapshot : provisional -> Snapshot_id.t

  module Request : sig
    type t

    val create
      :  context:Context.Expected.t
      -> target:Target.selector
      -> source:Source.t
      -> cursor_byte:int
      -> (t, Error.t) result

    val context : t -> Context.Expected.t
    val target : t -> Target.selector
    val source : t -> Source.t
    val cursor_byte : t -> int
    val equal : t -> t -> bool
    val sexp_of_t : t -> Core.Sexp.t
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) result
  end

  type t = private
    | Provisional of provisional
    | Unavailable of
        { context : Context.Expected.t
        ; target : Target.selector
        ; source : Source.t
        ; cursor_byte : int
        ; reason : unavailable
        }

  val provisional : provisional -> t

  val unavailable
    :  context:Context.Expected.t
    -> target:Target.selector
    -> source:Source.t
    -> cursor_byte:int
    -> reason:unavailable
    -> (t, Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
  val sexp_of_t : t -> Core.Sexp.t
end

module Query : sig
  (** Every response carries one of these CLOSED evidence forms. A Failed
      analysis cannot be used as Certified or Provisional. Syntax_only is
      parsed same-run syntax, never a name/type fact. *)
  type evidence =
    | Certified of
        { capture : Capture.t
        ; snapshot : Snapshot_id.t
        ; validated_graph : Chatml_source.Digest.t
        }
    | Provisional of Prefix.provisional
    | Syntax_only of Capture.t

  type namespace =
    | Value
    | Type
    | Type_parameter
  [@@deriving compare, equal, sexp_of]

  type kind =
    | References
    | Definitions
    | Type_at
    | Candidates of namespace
    | Configured_modules
    | Syntax_at
  [@@deriving compare, equal, sexp_of]

  module Request : sig
    type t

    val create
      :  evidence:evidence
      -> source:Source.t
      -> byte_offset:int
      -> kind:kind
      -> max_items:int
      -> (t, Error.t) result

    val evidence : t -> evidence
    val source : t -> Source.t
    val byte_offset : t -> int
    val kind : t -> kind
    val max_items : t -> int
    val equal : t -> t -> bool
    val sexp_of_t : t -> Core.Sexp.t
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) result
  end

  type unavailable =
    | Not_checked
    | No_recorded_context
    | No_recorded_type
    | No_nominal_members
    | No_syntax
  [@@deriving compare, equal, sexp_of]

  type 'a page =
    { items : 'a list
    ; truncated : bool
    }

  type 'a found =
    | Available of 'a
    | Unavailable of unavailable

  type 'a selected =
    | Selected of 'a
    | Missing of unavailable
    | Ambiguous of 'a page

  module Display : sig
    (** Bounded, escaped IDE presentation of one actual frozen type root. It
        names a versioned display format and never claims to be a portable
        executable/inferable type graph. The host renders the selected PUBLIC
        binding root for completion/signature results; a principal declaration
        root remains separate and cannot replace a restricted public scheme. *)
    type format = Chatml_display_v1 [@@deriving compare, equal, sexp_of]

    type kind =
      | Type
      | Signature
    [@@deriving compare, equal, sexp_of]

    type completeness =
      | Complete
      | Truncated of [ `Work_limit | `Byte_limit ]
    [@@deriving compare, equal, sexp_of]

    type t

    val create
      :  format:format
      -> kind:kind
      -> completeness:completeness
      -> text:string
      -> (t, Error.t) result

    val format : t -> format
    val kind : t -> kind
    val completeness : t -> completeness
    val text : t -> string
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) result
  end

  module Item : sig
    type symbol_kind =
      | Value
      | Parameter
      | Module
      | Type_alias
      | Type_parameter
    [@@deriving compare, equal, sexp_of]

    type origin =
      | Authored of Site.t
      | Implicit_specification_parameter of Site.t
      | Host of
          { contract : Chatml_source.Digest.t
          ; member_path : string list
          }

    type symbol

    val symbol
      :  id:int
      -> name:string
      -> kind:symbol_kind
      -> origin:origin
      -> principal_signature:Display.t option
      -> specification_ids:int list
      -> (symbol, Error.t) result

    val symbol_id : symbol -> int
    val symbol_name : symbol -> string
    val symbol_kind : symbol -> symbol_kind
    val symbol_origin : symbol -> origin
    val principal_signature : symbol -> Display.t option
    val symbol_specification_ids : symbol -> int list

    type target =
      | Symbol of int
      | Configured_module of string
      | Structural_label
      | Unlocated_module_member
      | Language_type of string

    type reference

    val reference
      :  selection:Site.t
      -> target:target
      -> inferred_type:Display.t option
      -> (reference, Error.t) result

    val reference_selection : reference -> Site.t
    val reference_target : reference -> target
    val reference_inferred_type : reference -> Display.t option

    type binding

    (** The selected scheme and actual nominal member bindings are projected
        from one Snapshot.Binding view. Member traversal has explicit depth,
        edge and aggregate-byte caps; a truncated available member page stays
        distinct from both an unavailable fact and an available empty page. *)

    val binding
      :  symbol:int
      -> name:string
      -> selected_signature:Display.t option
      -> value_members:binding page found
      -> type_members:binding page found
      -> (binding, Error.t) result

    val binding_symbol : binding -> int
    val binding_name : binding -> string
    val binding_selected_signature : binding -> Display.t option
    val binding_value_members : binding -> binding page found
    val binding_type_members : binding -> binding page found

    type configured_module

    (** One authentic export fact has separate value and type namespaces. An
        unavailable export fact cannot be represented as one populated namespace
        and one fabricated empty namespace. *)
    type exports

    val exports : values:binding page -> types:binding page -> (exports, Error.t) result
    val export_values : exports -> binding page
    val export_types : exports -> binding page

    val configured_module
      :  name:string
      -> module_key:string
      -> declared_at:Site.t
      -> exports:exports found
      -> shadowed_by:int option
      -> (configured_module, Error.t) result

    val configured_module_name : configured_module -> string
    val configured_module_key : configured_module -> string
    val configured_module_declared_at : configured_module -> Site.t
    val configured_module_exports : configured_module -> exports found
    val configured_module_shadowed_by : configured_module -> int option
    val symbol_to_json : symbol -> Jsonaf.t
    val symbol_of_json : Jsonaf.t -> (symbol, Error.t) result
    val reference_to_json : reference -> Jsonaf.t
    val reference_of_json : Jsonaf.t -> (reference, Error.t) result
    val binding_to_json : binding -> Jsonaf.t
    val binding_of_json : Jsonaf.t -> (binding, Error.t) result
    val configured_module_to_json : configured_module -> Jsonaf.t
    val configured_module_of_json : Jsonaf.t -> (configured_module, Error.t) result
  end

  type payload =
    | References_result of Item.reference page found
    | Definitions_result of Item.symbol page found
    | Type_result of Display.t selected
    | Candidates_result of Item.binding page selected
    | Configured_modules_result of Item.configured_module page selected
    | Syntax_result of Syntax_site.t page found

  type t

  val create : request:Request.t -> payload:payload -> (t, Error.t) result
  val request : t -> Request.t
  val payload : t -> payload
  val sexp_of_t : t -> Core.Sexp.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end
```

## authoring_assistance

[implementation](../../lib/agent_protocol/authoring_assistance.ml) · [interface](../../lib/agent_protocol/authoring_assistance.mli)

```ocaml
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

  module Selected_tool : sig
    (** Comparison evidence for one binding in the host's ACTUALLY selected
        tool registry. Both digests come from the existing SHA-256
        Tool_capability reference/selected-registry fingerprints, admitted
        through Digest.of_hex. The host resolves the same capability within
        the current selected registry before publishing a suggestion. This
        carries no implementation, invocation or permission grant. *)
    type t

    val create
      :  capability_id:Id.Capability.t
      -> binding_fingerprint:Chatml_source.Digest.t
      -> selected_registry_fingerprint:Chatml_source.Digest.t
      -> (t, Error.t) result

    val capability_id : t -> Id.Capability.t
    val binding_fingerprint : t -> Chatml_source.Digest.t
    val selected_registry_fingerprint : t -> Chatml_source.Digest.t
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
      | Selected_tool of Selected_tool.t
      | Host_root of Id.Authoring_root.t
    [@@deriving sexp_of]

    (** Admitted evidence matrix: Tag/Attribute/Value use Lexical or
        Parsed_document; Reference uses Parsed_document, Selected_catalog or
        Selected_tool;
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
      (** A relevant source-derived candidate needs bytes that cannot be
          represented as valid UTF-8 insert text in this cursor context.
          Earlier valid items remain; unrelated invalid metadata does not
          truncate another suggestion family. No source bytes are rewritten. *)
      | Unrepresentable_text
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

    module Builder : sig
      (** Incremental bounded result admission for producers. [Full] means
          only aggregate output capacity was reached. Invalid ownership or
          suggestion structure returns the original typed error. The builder
          reserves the largest finite status envelope before accepting items,
          so [finish] performs one final shared validation without trimming. *)
      type completed = t

      type t

      type addition =
        | Added
        | Full of Status.truncation

      val create : request:Request.t -> (t, Error.t) result
      val add : t -> Suggestion.t -> (addition, Error.t) result
      val finish : t -> status:Status.t -> (completed, Error.t) result
    end

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
```

## authoring_bundle

[implementation](../../lib/agent_protocol/authoring_bundle.ml) · [interface](../../lib/agent_protocol/authoring_bundle.mli)

```ocaml
(** Immutable bundle-upload wire values. These DTOs validate portable syntax and
    finite framing; they do not authorize an upload, prove source custody, close
    a project graph, or publish a bundle. The host reauthorizes the authenticated
    principal on every call. Storage (OCH-263) and closure validation (OCH-264)
    independently enforce their own limits and evidence. Explicit [None] fields
    are absent in JSON; explicit null, duplicate and unknown fields are errors. *)

module Bundle : sig
  module Wire_limits : sig
    (** Fixed codec ceilings. The initialized host may advertise and enforce
        lower principal/store limits; these constants do not reserve quota. *)
    val max_members : int

    val max_manifest_bytes : int
    val max_member_bytes : int
    val max_total_declared_bytes : int
    val max_chunk_bytes : int
  end

  module Owner : sig
    (** Echoed comparison coordinates from the authenticated upload owner.
        These labels carry no upload, read or execution authority. *)
    type t = private
      { host : Id.Server.t
      ; principal : Id.Principal.t
      }
    [@@deriving equal, sexp_of]

    val create : host:Id.Server.t -> principal:Id.Principal.t -> t
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) result
  end

  module Member : sig
    (** One qualified source ID, SHA-256 digest and exact byte length. An open
        editor revision cannot be placed in an immutable upload manifest. *)
    type t [@@deriving equal, sexp_of]

    val create : source:Authoring_common.Source.t -> (t, Error.t) result
    val source : t -> Authoring_common.Source.t
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) result
  end

  module Manifest : sig
    (** Canonical complete member inventory, including the exact qualified root
        project source. [selection] names its entry and environment. The root
        source must occur exactly once. IDs are distinct; exact, ASCII-case
        and file-versus-directory prefix collisions within one namespace are
        rejected using [Source.Portable_path.first_collision]. Source bytes are
        never inferred from paths. Canonical order is by typed Source.Id comparison.
        Construction bounds member count and aggregate declared bytes before
        retaining the inventory; host policy may impose lower ceilings. *)
    type t [@@deriving equal, sexp_of]

    val create
      :  project_source:Chatml_source.Id.t
      -> selection:Authoring_context.Context.Selection.t
      -> members:Member.t list
      -> (t, Error.t) result

    val project_source : t -> Chatml_source.Id.t
    val selection : t -> Authoring_context.Context.Selection.t
    val members : t -> Member.t list

    (** Digest of a canonical versioned encoding containing the qualified
        project source, exact target/optional environment and every sorted
        member's qualified ID, digest and byte length. Never a proof that any
        source body was received or compiled. *)
    val fingerprint : t -> Chatml_source.Digest.t

    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) result
  end

  module Begin : sig
    (** The optional parent is current-read provenance only. It is not an
        upload prerequisite. Host authorization binds the request to the
        authenticated principal and server; an upload-only caller may set it
        to None. Reusing [idempotency_key] with an identical complete request
        recovers the same upload without a second quota reservation. *)
    type request = private
      { parent_context : Authoring_context.Context.Expected.t option
      ; idempotency_key : Idempotency_key.t
      ; manifest : Manifest.t
      }
    [@@deriving sexp_of]

    type response [@@deriving sexp_of]

    val create_request
      :  ?parent_context:Authoring_context.Context.Expected.t
      -> idempotency_key:Idempotency_key.t
      -> manifest:Manifest.t
      -> unit
      -> request

    (** The host echoes exact request affinity. [expires_at] is host clock
        output, never a client lease extension. [max_chunk_bytes] is positive
        and no larger than the protocol's fixed decoded-chunk ceiling. *)
    val create_response
      :  owner:Owner.t
      -> upload_id:Id.Authoring_upload.t
      -> idempotency_key:Idempotency_key.t
      -> manifest_fingerprint:Chatml_source.Digest.t
      -> parent_context:Authoring_context.Context.Expected.t option
      -> max_chunk_bytes:int
      -> expires_at:Timestamp.t
      -> (response, Error.t) result

    val owner : response -> Owner.t
    val upload_id : response -> Id.Authoring_upload.t
    val idempotency_key : response -> Idempotency_key.t
    val manifest_fingerprint : response -> Chatml_source.Digest.t
    val parent_context : response -> Authoring_context.Context.Expected.t option
    val max_chunk_bytes : response -> int
    val expires_at : response -> Timestamp.t
    val request_to_json : request -> Jsonaf.t
    val request_of_json : Jsonaf.t -> (request, Error.t) result
    val response_to_json : response -> Jsonaf.t
    val response_of_json : Jsonaf.t -> (response, Error.t) result
  end

  module Chunk : sig
    (** One exact member byte offset and bounded binary payload. The wire uses
        base64 and checks encoded size before decoding, decoded size before
        retention, and offset + length without overflow. A repeat of already
        accepted identical bytes is safe; changed or partial-overlap repeats
        conflict in storage. The source ID is never a filesystem path. *)
    type request = private
      { upload_id : Id.Authoring_upload.t
      ; source : Chatml_source.Id.t
      ; byte_offset : int
      ; bytes : string
      }
    [@@deriving sexp_of]

    type response [@@deriving sexp_of]

    val create_request
      :  upload_id:Id.Authoring_upload.t
      -> source:Chatml_source.Id.t
      -> byte_offset:int
      -> bytes:string
      -> (request, Error.t) result

    val create_response
      :  owner:Owner.t
      -> upload_id:Id.Authoring_upload.t
      -> source:Chatml_source.Id.t
      -> accepted_until_byte:int
      -> (response, Error.t) result

    val owner : response -> Owner.t
    val upload_id : response -> Id.Authoring_upload.t
    val source : response -> Chatml_source.Id.t
    val accepted_until_byte : response -> int
    val request_to_json : request -> Jsonaf.t
    val request_of_json : Jsonaf.t -> (request, Error.t) result
    val response_to_json : response -> Jsonaf.t
    val response_of_json : Jsonaf.t -> (response, Error.t) result
  end

  module Rejection : sig
    (** Coarse safe status codes, with no source path, private policy or
        compiler diagnostic body. Detailed authorized diagnostics are separate. *)
    type t =
      | Invalid_closure
      | Unsupported_environment
      | Resource_limit
      | Internal_failure
    [@@deriving compare, equal, sexp_of]

    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) result
  end

  module Receipt : sig
    (** An immutable publication projection. Constructing/decoding this data
        does not mint the OCH-264 validated-closure proof or grant read/run
        authority. Only the proof-gated host publisher may expose Published. *)
    type t [@@deriving equal, sexp_of]

    val create
      :  owner:Owner.t
      -> upload_id:Id.Authoring_upload.t
      -> bundle_id:Id.Authoring_bundle.t
      -> manifest_fingerprint:Chatml_source.Digest.t
      -> project_source:Chatml_source.Id.t
      -> graph:Chatml_source.Digest.t
      -> compiler_contract:Chatml_source.Digest.t
      -> environment:string
      -> expires_at:Timestamp.t
      -> (t, Error.t) result

    val owner : t -> Owner.t
    val upload_id : t -> Id.Authoring_upload.t
    val bundle_id : t -> Id.Authoring_bundle.t
    val manifest_fingerprint : t -> Chatml_source.Digest.t
    val project_source : t -> Chatml_source.Id.t
    val graph : t -> Chatml_source.Digest.t
    val compiler_contract : t -> Chatml_source.Digest.t
    val environment : t -> string
    val expires_at : t -> Timestamp.t
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) result
  end

  module Status : sig
    type request = private { upload_id : Id.Authoring_upload.t } [@@deriving sexp_of]

    module Progress : sig
      type t [@@deriving equal, sexp_of]

      val create
        :  source:Chatml_source.Id.t
        -> accepted_until_byte:int
        -> (t, Error.t) result

      val source : t -> Chatml_source.Id.t
      val accepted_until_byte : t -> int
    end

    type state =
      | Staging of Progress.t list
      | Validating
      | Published of Receipt.t
      | Rejected of Rejection.t
      | Expired
    [@@deriving sexp_of]

    (** A status response always echoes the upload ID. Staging progress is
        bounded, ordered by qualified source ID and has no duplicate source.
        Published receipts must echo that same owner and upload ID. Status never renews
        expiry or retries commit/publication. *)
    type t [@@deriving sexp_of]

    val create_request : upload_id:Id.Authoring_upload.t -> request

    val create
      :  owner:Owner.t
      -> upload_id:Id.Authoring_upload.t
      -> state:state
      -> (t, Error.t) result

    val owner : t -> Owner.t
    val upload_id : t -> Id.Authoring_upload.t
    val state : t -> state
    val request_to_json : request -> Jsonaf.t
    val request_of_json : Jsonaf.t -> (request, Error.t) result
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) result
  end

  module Commit : sig
    (** An incomplete attempt is an error and records no commit key. Once
        complete, the key binds upload, manifest and exact payload digest;
        repeated same-key requests recover the same sealed claim/result.
        A different key conflicts. A successful response may still be pending
        closure validation and does not itself assert publication. *)
    type request = private
      { upload_id : Id.Authoring_upload.t
      ; idempotency_key : Idempotency_key.t
      }
    [@@deriving sexp_of]

    type outcome =
      | Accepted_for_validation
      | Published of Receipt.t
      | Rejected of Rejection.t
    [@@deriving sexp_of]

    (** A Published receipt must echo this response's owner and upload ID. *)
    type response [@@deriving sexp_of]

    val create_request
      :  upload_id:Id.Authoring_upload.t
      -> idempotency_key:Idempotency_key.t
      -> request

    val create_response
      :  owner:Owner.t
      -> upload_id:Id.Authoring_upload.t
      -> idempotency_key:Idempotency_key.t
      -> outcome:outcome
      -> (response, Error.t) result

    val owner : response -> Owner.t
    val upload_id : response -> Id.Authoring_upload.t
    val idempotency_key : response -> Idempotency_key.t
    val outcome : response -> outcome
    val request_to_json : request -> Jsonaf.t
    val request_of_json : Jsonaf.t -> (request, Error.t) result
    val response_to_json : response -> Jsonaf.t
    val response_of_json : Jsonaf.t -> (response, Error.t) result
  end
end
```

## authoring_codec

[implementation](../../lib/agent_protocol/authoring_codec.ml) · [interface](../../lib/agent_protocol/authoring_codec.mli)

```ocaml
(** Internal strict, bounded primitives shared by the authoring DTO codecs. *)

(** Largest exact integer shared with common JSON clients. Constructors and
    decoders keep all revision/generation counters within this bound. *)
val maximum_wire_integer : int

(** Conservative escaped-JSON byte, node and depth preflight over an already parsed
    value, before family-specific decoding. The new analysis/assistance family
    uses the smaller of its fixed limit and the negotiated result limit; the
    host checks request/result sizes against its current capability as well.
    This does not replace constructors' pre-allocation count/label budgets. *)
val admit_json : max_bytes:int -> Jsonaf.t -> (unit, Error.t) result

(** Conservative escaped-wire upper bound, with the same node/depth checks as
    [admit_json]. Callers can debit many bounded DTO pieces before forming a
    whole response; punctuation overcount is allowed and fails closed. *)
val encoded_size : max_bytes:int -> Jsonaf.t -> (int, Error.t) result

(** A single conservative budget for DTO pieces. [reserve] accounts for the
    fixed enclosing objects, arrays and punctuation before any large response
    is assembled. [charge_json] counts bytes and nodes across all pieces and
    checks their depth at the position they occupy in the complete JSON tree. *)
module Budget : sig
  type t

  val create : max_bytes:int -> (t, Error.t) result
  val reserve : t -> bytes:int -> nodes:int -> (unit, Error.t) result
  val charge_json : t -> parent_depth:int -> Jsonaf.t -> (unit, Error.t) result
end

val invalid : string -> ('a, Error.t) result
val fields : allowed:string list -> Jsonaf.t -> (Json_codec.fields, Error.t) result
val required : Json_codec.fields -> string -> (Jsonaf.t, Error.t) result

val required_as
  :  Json_codec.fields
  -> string
  -> (Jsonaf.t -> ('a, Error.t) result)
  -> ('a, Error.t) result

val optional_as
  :  Json_codec.fields
  -> string
  -> (Jsonaf.t -> ('a, Error.t) result)
  -> ('a option, Error.t) result

val string : max_bytes:int -> Jsonaf.t -> (string, Error.t) result
val nonnegative : Jsonaf.t -> (int, Error.t) result
val positive : Jsonaf.t -> (int, Error.t) result
val integer : int -> Jsonaf.t
val boolean : bool -> Jsonaf.t

val list
  :  max_items:int
  -> (Jsonaf.t -> ('a, Error.t) result)
  -> Jsonaf.t
  -> ('a list, Error.t) result

val array : ('a -> Jsonaf.t) -> 'a list -> Jsonaf.t
val optional_field : string -> 'a option -> ('a -> Jsonaf.t) -> (string * Jsonaf.t) list

(** Source IDs encode the arbitrary-byte package namespace as bounded base64;
    the portable relative member remains a UTF-8 JSON string. *)
val source_id : Jsonaf.t -> (Chatml_source.Id.t, Error.t) result

val validate_source_id : Chatml_source.Id.t -> (unit, Error.t) result
val source_id_json : Chatml_source.Id.t -> Jsonaf.t
val digest : Jsonaf.t -> (Chatml_source.Digest.t, Error.t) result
val digest_json : Chatml_source.Digest.t -> Jsonaf.t
val portable_member : Jsonaf.t -> (string, Error.t) result
val portable_member_string : string -> (string, Error.t) result
val utf8_text : max_bytes:int -> Jsonaf.t -> (string, Error.t) result
val bytes_base64 : max_bytes:int -> Jsonaf.t -> (string, Error.t) result
val bytes_base64_json : string -> Jsonaf.t
```

## authoring_common

[implementation](../../lib/agent_protocol/authoring_common.ml) · [interface](../../lib/agent_protocol/authoring_common.mli)

```ocaml
(** Versioned, transport-neutral authoring DTOs. All identities and source sites
    are comparison coordinates, never read, write, analysis or execution grants.
    The host reauthorizes the authenticated actor on every method, including
    cached reads. Codecs validate bounded wire values before domain admission.
    Every optional field in this family uses absence for [None] and a concrete
    value for [Some]; explicit JSON null is invalid, including response fields.
    Unknown and duplicate object fields are invalid. Integers must be exact
    bounded JSON integers, never floats or exponent notation.
    Method failures use the existing typed protocol error channel:
    [Permission_denied] for unauthorized access, [Lease_stale] for a lost
    context lease, [Conflict] for stale source/context revisions,
    [Invalid_request] with a bounded position-reason tag for invalid editor
    coordinates, [Resource_limit] for work/admission ceilings and [Interrupted]
    for cancellation. These wire codes do not reveal whether a denied source
    exists. Later semantic queries return unavailable facts as typed results
    rather than protocol errors.
    This module has no compiler, Eio, filesystem, runtime or UI dependency. *)

module Features : sig
  (** Initialization advertises [core] only after a host installs the
      source/context handlers. Codecs and {!Command.supported_methods} alone
      do not make any method available. Bundle and later analysis features
      are selected separately only after their handlers are installed. *)
  val core : string

  (** Separately negotiated immutable upload access. An upload-only principal
      need not select [core] or open a source context. *)
  val bundle : string

  (** Selected separately from source access, and advertised only when the
      corresponding analyze/query or lexical/assistance handlers are installed. *)
  val semantic : string

  (** Lexical scanning and early assistance may be installed independently.
      Neither feature implies a selected Build or semantic analysis. *)
  val lexical : string

  val assistance : string
end

module Encoding : sig
  type t = Chatml_editor_position.Encoding.t =
    | Utf8_bytes
    | Utf16_units
  [@@deriving compare, equal, sexp_of]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end

module Method : sig
  type t =
    | Capabilities
    | Roots
    | Host_list
    | Host_read
    | Context_open
    | Context_refresh
    | Context_close
    | Source_list
    | Source_read
    | Document_version
    | Document_replace
    | Document_close
    | Bundle_begin
    | Bundle_chunk
    | Bundle_commit
    | Bundle_status
    | Analyze
    | Prefix
    | Query
    | Lexical
    | Assistance
  [@@deriving compare, equal, sexp_of]

  (** Closed stable [authoring.*] method names. A method's presence in this
      enumeration or the decoder does not advertise an installed handler. *)
  val to_string : t -> string

  val of_string : string -> (t, Error.t) Result.t
end

module Failure : sig
  (** Bounded authoring-specific reasons in [Protocol_error.data]. The ordinary
      protocol code remains authoritative; malformed or mismatched detail
      decodes as [Other] instead of changing that code's meaning. No path,
      source existence, supplied bytes or private host policy is exposed. *)
  type invalid_position =
    | Invalid_utf8
    | Out_of_bounds
    | Inside_scalar
    | Inside_surrogate
    | Inside_crlf
    | Foreign_source
  [@@deriving compare, equal, sexp_of]

  type reason =
    | Unauthorized
    | Context_lease_stale
    | Stale_context
    | Stale_source
    | Invalid_position of invalid_position
    | Source_unavailable
    | Environment_unavailable
    | Work_limit
    | Cancelled
    | Other
  [@@deriving compare, equal, sexp_of]

  (** [attach] admits only reason/code pairs defined by this protocol family.
      [classify] never string-matches messages. Server code may create an
      ordinary error when no more specific safe detail applies. *)
  val attach : Error.t -> reason -> (Error.t, Error.t) Result.t

  val classify : Error.t -> reason
end

module Limits : sig
  (** Largest exact JSON counter admitted for generations and editor revisions. *)
  val maximum_wire_integer : int

  (** Actual per-host method limits, not claims about OCaml heap or wall time.
      [max_request_bytes] is also bounded by the generic initialize limit.
      Constructors check positivity, representable integer bounds and local
      cross-field consistency. They do not prove a host can honor a limit;
      the handler selects ceilings at initialization and checks each request
      against them and its current principal quota before allocation. *)
  type t = private
    { max_sources : int
    ; max_source_bytes : int
    ; max_context_source_bytes : int
    ; max_request_bytes : int
    ; max_result_bytes : int
    ; max_path_candidates : int
    }
  [@@deriving sexp_of]

  val create
    :  max_sources:int
    -> max_source_bytes:int
    -> max_context_source_bytes:int
    -> max_request_bytes:int
    -> max_result_bytes:int
    -> max_path_candidates:int
    -> (t, Error.t) Result.t

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end

module Capabilities : sig
  (** Actual installed handler methods, selected position encodings and policy
      bounds. [encodings] is empty for an upload-only principal with no source
      position service; it never advertises an unavailable editor conversion.
      A codec may know a method that is absent here. [authoring.capabilities]
      requires selected [Features.core] or [Features.bundle], according to the
      installed handler and authenticated principal. *)
  type t = private
    { methods : string list
    ; encodings : Encoding.t list
    ; limits : Limits.t
    }
  [@@deriving sexp_of]

  val create
    :  methods:string list
    -> encodings:Encoding.t list
    -> limits:Limits.t
    -> (t, Error.t) Result.t

  (** Unknown future [authoring.*] method names are retained but do not make
      an older client callable; [supports] tests only its typed known methods. *)
  val supports : t -> Method.t -> bool

  val supports_encoding : t -> Encoding.t -> bool
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end

module Source : sig
  (** Exact captured source revision. Optional [editor_revision] is absent for
      host/bundle base bytes and present as a positive monotonically increasing
      client revision for an open overlay. Wire [null] is invalid: absence and
      a value are the only admitted states. Byte length is nonnegative and
      digest is SHA-256; neither claims that the bytes are currently readable. *)
  type t [@@deriving equal, sexp_of]

  val create
    :  id:Chatml_source.Id.t
    -> digest:Chatml_source.Digest.t
    -> byte_length:int
    -> editor_revision:int option
    -> (t, Error.t) Result.t

  val id : t -> Chatml_source.Id.t
  val digest : t -> Chatml_source.Digest.t
  val byte_length : t -> int
  val editor_revision : t -> int option
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end

module Position : sig
  (** Zero-based coordinates. Construction rejects negatives; applying a
      position to a source additionally validates CRLF, scalar, surrogate and
      EOF boundaries with {!Chatml_editor_position}. *)
  type t = Chatml_editor_position.Position.t

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end

module Site : sig
  (** Exact source owner plus ordered half-open editor coordinates in one
      explicit encoding. A site is accepted for a query only after the service
      matches its source ID, digest, byte length and editor revision to the
      current context and projects through the captured position index. An
      [Approximate] site denotes declaration-level attribution, never an exact
      embedded byte map; OCH-259 owns that provenance projection. *)
  type precision =
    | Exact
    | Approximate
  [@@deriving compare, equal, sexp_of]

  type t [@@deriving sexp_of]

  val create
    :  source:Source.t
    -> encoding:Encoding.t
    -> start:Position.t
    -> finish:Position.t
    -> precision:precision
    -> (t, Error.t) Result.t

  val source : t -> Source.t
  val encoding : t -> Encoding.t
  val start : t -> Position.t
  val finish : t -> Position.t
  val precision : t -> precision
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end
```

## authoring_context

[implementation](../../lib/agent_protocol/authoring_context.ml) · [interface](../../lib/agent_protocol/authoring_context.mli)

```ocaml
(** Compatibility umbrella for the three cohesive authoring core DTO families. *)
module Context = Authoring_context_core.Context

module Document = Authoring_document.Document
module Source_read = Authoring_source_read.Source_read
```

## authoring_context_core

[implementation](../../lib/agent_protocol/authoring_context_core.ml) · [interface](../../lib/agent_protocol/authoring_context_core.mli)

```ocaml
open Authoring_common

module Context : sig
  module Selection : sig
    type target =
      | Entry of string
      | Library
    [@@deriving equal, sexp_of]

    (** [environment] absent selects the declared default; explicit JSON null
        is rejected. Names are bounded, nonempty, exact bytes. *)
    type t = private
      { target : target
      ; environment : string option
      }
    [@@deriving equal, sexp_of]

    val create : target:target -> environment:string option -> (t, Error.t) Result.t
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) Result.t
  end

  type origin =
    | Host_project of
        { root : Id.Authoring_root.t
        ; dependency_roots : Id.Authoring_root.t list
        ; project_member : string
        ; selection : Selection.t
        }
    | Uploaded_project of Id.Authoring_bundle.t
    | Host_document of
        { root : Id.Authoring_root.t
        ; source : Source.t
        ; default_environment : string option
        }
    | Supplied_document of
        { source : Source.t
        ; default_environment : string option
        }
  [@@deriving equal, sexp_of]

  module Open_request : sig
    (** [Supplied_document] transports exact bytes by base64 and requires an
        independent document-admission grant. It creates a captured base
        source, not a host file or uploaded project. Other origins have no
        supplied content. No project graph/check success is required for either
        document-only origin. *)
    type t = private
      | Host_project of
          { root : Id.Authoring_root.t
          ; dependency_roots : Id.Authoring_root.t list
          ; project_member : string
          ; selection : Selection.t
          }
      | Uploaded_project of Id.Authoring_bundle.t
      | Host_document of
          { root : Id.Authoring_root.t
          ; member : string
          ; default_environment : string option
          }
      | Supplied_document of
          { member : string
          ; default_environment : string option
          ; contents : string
          }
    [@@deriving sexp_of]

    val create_host_project
      :  root:Id.Authoring_root.t
      -> dependency_roots:Id.Authoring_root.t list
      -> project_member:string
      -> selection:Selection.t
      -> (t, Error.t) Result.t

    val create_uploaded_project : Id.Authoring_bundle.t -> t

    val create_host_document
      :  root:Id.Authoring_root.t
      -> member:string
      -> default_environment:string option
      -> (t, Error.t) Result.t

    val create_supplied_document
      :  member:string
      -> default_environment:string option
      -> contents:string
      -> (t, Error.t) Result.t

    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) Result.t
  end

  type static_selection =
    { environment : string
    ; compiler_contract : Chatml_source.Digest.t
    }
  [@@deriving equal, sexp_of]

  (** A document-only static selection is host-admitted from an actual
      immutable catalog/compiler contract, never client-provided signatures.
      Absence still permits lexical and basic declaration syntax help; paths,
      templates and script references require separate source/catalog grants. *)

  type basis =
    | Project of
        { base_graph : Chatml_source.Digest.t
        ; selection : static_selection
        }
    | Document of
        { source : Source.t
        ; selection : static_selection option
        }
  [@@deriving equal, sexp_of]

  (** One connection-scoped context view. The host and nonce are selected by
      the server; [id] alone is not a transferable capability. Generation is
      nonnegative and advances on each edit/close. A document basis can be
      admitted without a successful project build; optional static selection
      exists only after the host authenticates that environment. It is not a
      certified analysis or a synthetic project graph. [origin] records the
      admitted base capture; [basis] reflects the current overlay revision.
      Host-document refresh may replace the base source revision under the same
      authorized root/member locator. A supplied document keeps its original
      base capture until a new context is opened. Generation is bounded by
      {!Authoring_common.Limits.maximum_wire_integer}.
      The host, not this DTO constructor, mints the fingerprint from exact
      custody/selection/source identity. *)
  type t [@@deriving sexp_of]

  val create
    :  id:Id.Authoring_context.t
    -> host:Id.Server.t
    -> origin:origin
    -> generation:int
    -> basis:basis
    -> basis_fingerprint:Chatml_source.Digest.t
    -> editable_sources:Source.t list
    -> (t, Error.t) Result.t

  val id : t -> Id.Authoring_context.t
  val host : t -> Id.Server.t
  val origin : t -> origin
  val generation : t -> int
  val basis : t -> basis
  val basis_fingerprint : t -> Chatml_source.Digest.t
  val editable_sources : t -> Source.t list
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t

  type view = t

  module Expected : sig
    (** CAS input for operations on one current context generation. A stale
        response cannot be applied after another document changes. *)
    type t = private
      { context_id : Id.Authoring_context.t
      ; host : Id.Server.t
      ; generation : int
      ; basis_fingerprint : Chatml_source.Digest.t
      }
    [@@deriving equal, sexp_of]

    val of_view : view -> t
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) Result.t
  end

  module Refresh_request : sig
    type policy =
      | Reject_open_documents
      | Discard_open_documents
    [@@deriving equal, sexp_of]

    type t = private
      { expected : Expected.t
      ; policy : policy
      }
    [@@deriving sexp_of]

    val create : expected:Expected.t -> policy:policy -> t
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) Result.t
  end

  module Close_request : sig
    type t = private { expected : Expected.t } [@@deriving sexp_of]

    val create : expected:Expected.t -> t
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) Result.t
  end

  module Close_result : sig
    (** Final generation of a closed context; no active view is returned. *)
    type t = private
      { id : Id.Authoring_context.t
      ; host : Id.Server.t
      ; generation : int
      }
    [@@deriving sexp_of]

    val create
      :  id:Id.Authoring_context.t
      -> host:Id.Server.t
      -> generation:int
      -> (t, Error.t) Result.t

    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) Result.t
  end
end
```

## authoring_document

[implementation](../../lib/agent_protocol/authoring_document.ml) · [interface](../../lib/agent_protocol/authoring_document.mli)

```ocaml
open Authoring_common
module Context = Authoring_context_core.Context

module Document : sig
  module Version : sig
    (** Source CAS receipt: exact source revision and whole context generation.
        [overlay_open] distinguishes a base version after a close. The last
        accepted editor revision remains in the CAS even after closing an
        overlay, so a later replacement cannot reuse it. *)
    type t [@@deriving sexp_of]

    val create
      :  context:Context.Expected.t
      -> source:Source.t
      -> last_editor_revision:int option
      -> overlay_open:bool
      -> (t, Error.t) Result.t

    val context : t -> Context.Expected.t
    val source : t -> Source.t
    val last_editor_revision : t -> int option
    val overlay_open : t -> bool
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) Result.t
  end

  module Version_request : sig
    (** Read current source CAS after any context edit. The server authorizes
        and resolves the source under this context; a caller cannot assert a
        fresh version using a source digest. *)
    type t = private
      { context : Context.Expected.t
      ; source_id : Chatml_source.Id.t
      }
    [@@deriving sexp_of]

    val create
      :  context:Context.Expected.t
      -> source_id:Chatml_source.Id.t
      -> (t, Error.t) Result.t

    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) Result.t
  end

  module Replace_request : sig
    (** Full exact UTF-8 text only. Arbitrary byte content uses bundle chunks;
        this method never repairs invalid UTF-8 or writes a host file. *)
    type t = private
      { expected : Version.t
      ; next_editor_revision : int
      ; contents : string
      }
    [@@deriving sexp_of]

    val create
      :  expected:Version.t
      -> next_editor_revision:int
      -> contents:string
      -> (t, Error.t) Result.t

    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) Result.t
  end

  module Close_request : sig
    type t = private { expected : Version.t } [@@deriving sexp_of]

    val create : expected:Version.t -> t
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) Result.t
  end

  module Result : sig
    type t = private
      { context : Context.t
      ; version : Version.t
      }
    [@@deriving sexp_of]

    val create : context:Context.t -> version:Version.t -> (t, Error.t) Core.Result.t
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) Core.Result.t
  end
end
```

## authoring_guidance

[implementation](../../lib/agent_protocol/authoring_guidance.ml) · [interface](../../lib/agent_protocol/authoring_guidance.mli)

```ocaml
(** Host-owned provenance for guidance, never inferred from message text. Source
    identities are content hashes, not paths, credentials or executable grants. *)
type source =
  | Installed of string
  | Authored of string
[@@deriving equal, sexp]

type purpose =
  | Primer
  | Preload
  | Reference
  | Rediscovery
[@@deriving equal, sexp]

type topic =
  { id : string
  ; document_sha256 : string
  ; source : source
  ; complete : bool
  }
[@@deriving equal, sexp]

type t = private
  { version : int
  ; context_identity : string
  ; policy_fingerprint : string
  ; payload_sha256 : string
  ; purpose : purpose
  ; topics : topic list
  ; fragments : fragment list [@sexp.list]
  ; surface_id : string option [@sexp.option]
  }

and part =
  { index : int
  ; item_sha256 : string
  }

and fragment =
  { topic_id : string
  ; total_parts : int
  ; parts : part list
  }
[@@deriving equal, sexp]

(** [context_identity] binds installed language/runtime, target and effective
    capability identities. [policy_fingerprint] comes from resolved author policy.
    The payload digest binds the complete provider item, including its role.
    Rediscovery pointers must not claim to contain complete topic content. *)
val create
  :  context_identity:string
  -> policy_fingerprint:string
  -> purpose:purpose
  -> topics:topic list
  -> payload:Jsonaf.t
  -> (t, Error.t) result

(** Version-2 reference provenance. Every topic has exactly one coverage record;
    indexes are ordered and unique, and per-page [complete] requires every part.
    At most 1024 topics and 4096 parts are recorded, matching query-receipt bounds.
    Version-1 primer/preload/reference/pointer
    records remain readable and do not acquire fragment metadata. The host must
    derive this evidence from a verified query receipt and the actual provider
    item; model-supplied labels are not an authority to create provenance. *)
val create_reference
  :  context_identity:string
  -> policy_fingerprint:string
  -> topics:topic list
  -> fragments:fragment list
  -> payload:Jsonaf.t
  -> (t, Error.t) result

(** Version-3 references preserve the queried compiler surface. Version-1/2
    records remain readable without inventing a surface for legacy evidence. *)
val create_surface_reference
  :  surface_id:string
  -> context_identity:string
  -> policy_fingerprint:string
  -> topics:topic list
  -> fragments:fragment list
  -> payload:Jsonaf.t
  -> (t, Error.t) result

(** A surface-specific pointer contains no topic content or fragment coverage. *)
val create_surface_rediscovery
  :  surface_id:string
  -> context_identity:string
  -> policy_fingerprint:string
  -> topics:topic list
  -> payload:Jsonaf.t
  -> (t, Error.t) result

val validate : t -> (unit, Error.t) result
val valid_topic : topic -> bool
val topic_to_json : topic -> Jsonaf.t
val topic_of_json : Jsonaf.t -> (topic, Error.t) result
val matches_payload : t -> Jsonaf.t -> bool
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
```

## authoring_reference

[implementation](../../lib/agent_protocol/authoring_reference.ml) · [interface](../../lib/agent_protocol/authoring_reference.mli)

```ocaml
(** Persisted evidence of an exact documentation response, not an execution grant
    or proof of model delivery. Decode only as part of trusted persisted state. *)
type part =
  { index : int
  ; item_sha256 : string
  }
[@@deriving equal, sexp]

type topic =
  { topic : Authoring_guidance.topic
  ; total_parts : int
  ; parts : part list
  }
[@@deriving equal, sexp]

type t = private
  { version : int
  ; query_identity : string
  ; host_identity : string
  ; capability_fingerprint : string
  ; scope : string
  ; surface_id : string
  ; corpus_identity : string
  ; response_sha256 : string
  ; topics : topic list
  }
[@@deriving equal, sexp]

val create
  :  query_identity:string
  -> host_identity:string
  -> capability_fingerprint:string
  -> scope:string
  -> surface_id:string
  -> corpus_identity:string
  -> response_sha256:string
  -> topics:topic list
  -> (t, Error.t) result

(** Version/closed-shape, identity and coverage validation. At most 1024 topics,
    4096 total recorded parts and 1 MiB encoded metadata. Per-topic indexes must
    be strictly increasing and within the declared total. Complete means all
    parts occur in this response, never just that this is the last query page. *)
val validate : t -> (unit, Error.t) result

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result

(** Digest of the actual response JSON. Neither comparison nor decoding gives a
    caller permission to attach the receipt to an invocation/history entry. *)
val matches_response : t -> Jsonaf.t -> bool

val matches_output : t -> Jsonaf.t -> bool
val scope_for : session_id:Id.Session.t -> generation:int -> string
```

## authoring_source_read

[implementation](../../lib/agent_protocol/authoring_source_read.ml) · [interface](../../lib/agent_protocol/authoring_source_read.mli)

```ocaml
open Authoring_common
module Context = Authoring_context_core.Context

module Source_read : sig
  (** Host-root IDs are connection-local candidates, not path grants. Host
      browse/read observes a granted root afresh; context inventory/read uses
      exact captured effective bytes at a generation, including an open
      overlay. Only explicit context refresh reobserves host bytes for that
      context. Path strings are validated portable members. *)
  module Root : sig
    type t = private
      { id : Id.Authoring_root.t
      ; display_name : string
      }
    [@@deriving sexp_of]

    val create : id:Id.Authoring_root.t -> display_name:string -> (t, Error.t) Result.t
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) Result.t
  end

  module Roots_result : sig
    (** Authorized roots available to this connection at the time of read.
        A subsequent open reauthorizes the chosen root. *)
    type t = private { roots : Root.t list } [@@deriving sexp_of]

    val create : roots:Root.t list -> (t, Error.t) Result.t
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) Result.t
  end

  module Host_list_request : sig
    type t = private
      { root : Id.Authoring_root.t
      ; directory : string option
      }
    [@@deriving sexp_of]

    val create
      :  root:Id.Authoring_root.t
      -> directory:string option
      -> (t, Error.t) Result.t

    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) Result.t
  end

  module Host_list_result : sig
    (** Complete bounded nonrecursive candidates from one granted root.
        Entries do not prove file type or grant a subsequent read. If native
        enumeration exceeds its cap, the whole request fails Resource_limit. *)
    type t = private
      { root : Id.Authoring_root.t
      ; members : string list
      }
    [@@deriving sexp_of]

    val create : root:Id.Authoring_root.t -> members:string list -> (t, Error.t) Result.t
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) Result.t
  end

  module Host_read_request : sig
    type t = private
      { root : Id.Authoring_root.t
      ; member : string
      }
    [@@deriving sexp_of]

    val create : root:Id.Authoring_root.t -> member:string -> (t, Error.t) Result.t
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) Result.t
  end

  module Host_read_result : sig
    (** New read observation with host-constructed Source ID/digest/length.
        Content is exact base64 bytes and does not mutate any open context. *)
    type t [@@deriving sexp_of]

    val create
      :  root:Id.Authoring_root.t
      -> member:string
      -> source:Source.t
      -> contents:string
      -> (t, Error.t) Result.t

    val root : t -> Id.Authoring_root.t
    val member : t -> string
    val source : t -> Source.t
    val contents : t -> string
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) Result.t
  end

  module List_request : sig
    type t = private
      { context : Context.Expected.t
      ; package_namespace : string option
      ; directory : string option
      }
    [@@deriving sexp_of]

    val create
      :  context:Context.Expected.t
      -> package_namespace:string option
      -> directory:string option
      -> (t, Error.t) Result.t

    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) Result.t
  end

  module Read_request : sig
    type t = private
      { context : Context.Expected.t
      ; source_id : Chatml_source.Id.t
      }
    [@@deriving sexp_of]

    val create
      :  context:Context.Expected.t
      -> source_id:Chatml_source.Id.t
      -> (t, Error.t) Result.t

    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) Result.t
  end

  module List_result : sig
    (** A finite authorized inventory prefix sorted by full Source ID.
        [omitted] is the number of
        further entries in this exact observed manifest; [truncated] iff it
        is positive. A changed context requires a fresh list. *)
    type t [@@deriving sexp_of]

    val create
      :  context:Context.Expected.t
      -> sources:Source.t list
      -> omitted:int
      -> (t, Error.t) Result.t

    val context : t -> Context.Expected.t
    val sources : t -> Source.t list
    val omitted : t -> int
    val truncated : t -> bool
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) Result.t
  end

  module Read_result : sig
    type t [@@deriving sexp_of]

    (** Wire contents are base64 of exact source bytes. Source ID/digest/length
        must match decoded bytes before this value is admitted. The context
        echo lets a client reject a response from a superseded generation. *)
    val create
      :  context:Context.Expected.t
      -> source:Source.t
      -> contents:string
      -> (t, Error.t) Result.t

    val context : t -> Context.Expected.t
    val source : t -> Source.t
    val contents : t -> string
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) Result.t
  end
end
```

## authoring_wire

[implementation](../../lib/agent_protocol/authoring_wire.ml) · [interface](../../lib/agent_protocol/authoring_wire.mli)

```ocaml
(** Closed, portable dispatch for the authoring method family. The sum
    constructors keep each request and result tied to its method-specific DTO;
    accepted syntax alone does not advertise an installed server handler. *)

module Request : sig
  type t =
    | Capabilities
    | Roots
    | Host_list of Authoring_context.Source_read.Host_list_request.t
    | Host_read of Authoring_context.Source_read.Host_read_request.t
    | Context_open of Authoring_context.Context.Open_request.t
    | Context_refresh of Authoring_context.Context.Refresh_request.t
    | Context_close of Authoring_context.Context.Close_request.t
    | Source_list of Authoring_context.Source_read.List_request.t
    | Source_read of Authoring_context.Source_read.Read_request.t
    | Document_version of Authoring_context.Document.Version_request.t
    | Document_replace of Authoring_context.Document.Replace_request.t
    | Document_close of Authoring_context.Document.Close_request.t
    | Bundle_begin of Authoring_bundle.Bundle.Begin.request
    | Bundle_chunk of Authoring_bundle.Bundle.Chunk.request
    | Bundle_commit of Authoring_bundle.Bundle.Commit.request
    | Bundle_status of Authoring_bundle.Bundle.Status.request
    | Analyze of Authoring_analysis.Analyze.Request.t
    | Prefix of Authoring_analysis.Prefix.Request.t
    | Query of Authoring_analysis.Query.Request.t
    | Lexical of Authoring_assistance.Lexical.Request.t
    | Assistance of Authoring_assistance.Assistance.Request.t
  [@@deriving sexp_of]

  val method_name : t -> string
  val params : t -> Jsonaf.t
  val of_method_and_params : method_:string -> params:Jsonaf.t -> (t, Error.t) result
  val supported_methods : string list
end

module Response : sig
  type t =
    | Capabilities of Authoring_common.Capabilities.t
    | Roots of Authoring_context.Source_read.Roots_result.t
    | Host_list of Authoring_context.Source_read.Host_list_result.t
    | Host_read of Authoring_context.Source_read.Host_read_result.t
    | Context_open of Authoring_context.Context.t
    | Context_refresh of Authoring_context.Context.t
    | Context_close of Authoring_context.Context.Close_result.t
    | Source_list of Authoring_context.Source_read.List_result.t
    | Source_read of Authoring_context.Source_read.Read_result.t
    | Document_version of Authoring_context.Document.Version.t
    | Document_replace of Authoring_context.Document.Result.t
    | Document_close of Authoring_context.Document.Result.t
    | Bundle_begin of Authoring_bundle.Bundle.Begin.response
    | Bundle_chunk of Authoring_bundle.Bundle.Chunk.response
    | Bundle_commit of Authoring_bundle.Bundle.Commit.response
    | Bundle_status of Authoring_bundle.Bundle.Status.t
    | Analyze of Authoring_analysis.Analyze.t
    | Prefix of Authoring_analysis.Prefix.t
    | Query of Authoring_analysis.Query.t
    | Lexical of Authoring_assistance.Lexical.Result.t
    | Assistance of Authoring_assistance.Assistance.Result.t
  [@@deriving sexp_of]

  val method_name : t -> string
  val to_json : t -> Jsonaf.t
  val of_method_and_json : method_:string -> Jsonaf.t -> (t, Error.t) result
  val supported_methods : string list
end
```

## blob

[implementation](../../lib/agent_protocol/blob.ml) · [interface](../../lib/agent_protocol/blob.mli)

```ocaml
(** Server-managed blob metadata and typed message input references. *)

type kind =
  | File
  | Image
  | Audio
  | Binary
[@@deriving compare, equal, sexp]

module Metadata : sig
  type t =
    { id : Id.Blob.t
    ; kind : kind
    ; media_type : string
    ; byte_length : int64
    ; digest : string
    ; display_name : string option
    }
  [@@deriving sexp]

  val create
    :  id:Id.Blob.t
    -> kind:kind
    -> media_type:string
    -> byte_length:int64
    -> digest:string
    -> ?display_name:string
    -> unit
    -> (t, Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Input : sig
  type source =
    | Stored of Id.Blob.t
    | Inline_base64 of string
  [@@deriving sexp]

  type t =
    { kind : kind
    ; media_type : string
    ; byte_length : int64
    ; digest : string
    ; display_name : string option
    ; source : source
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

(** Bounded transport-neutral reads for server-owned session blobs. This is
    used by duplex transports that cannot use the HTTP streaming route.
    An absent attachment requests retained access under current principal
    visibility and method scopes. A supplied attachment must remain current.
    Encoding omits absence; explicit JSON null is invalid. No execution admission. *)
module Read_request : sig
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t option
    ; blob_id : Id.Blob.t
    ; offset : int64
    ; max_bytes : int
    }
  [@@deriving sexp]

  val max_chunk_bytes : int
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

(** One base64-encoded blob chunk. [next_offset] is the exact cursor for the
    next request and [eof] is true only after the advertised blob length has
    been reached. *)
module Chunk : sig
  type t =
    { blob : Metadata.t
    ; offset : int64
    ; next_offset : int64
    ; data_base64 : string
    ; eof : bool
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end
```

## command

[implementation](../../lib/agent_protocol/command.ml) · [interface](../../lib/agent_protocol/command.mli)

```ocaml
(** Typed method dispatch for transport-neutral protocol requests. *)

type t =
  | Protocol_initialize of Initialize.Request.t
  | Command_receipt of Command_receipt.Request.t
  | Provider_setup of Provider_operator.Setup_request.t
  | Provider_status of Provider_operator.Status_request.t
  | Provider_login_begin of Provider_operator.Login_request.t
  | Provider_login_challenge of Provider_operator.Challenge_request.t
  | Provider_login_cancel of Provider_operator.Cancel_request.t
  | Provider_logout of Provider_operator.Logout_request.t
  | Provider_select of Provider_operator.Select_request.t
  | Provider_configure_environment of Provider_operator.Environment_request.t
  | Protocol_ping of Ping.Request.t
  | Server_info
  | Server_health of Health.Request.t
  | Prompt_list of Prompt.List_request.t
  | Prompt_get of Prompt.Get_request.t
  | Prompt_dependencies_status
  | Prompt_dependencies_update of Prompt_dependency_selection.Update_request.t
  | Workspace_list of Workspace.List_request.t
  | Workspace_get of Workspace.Get_request.t
  | Blob_read of Blob.Read_request.t
  | Session_create of Session.Create_request.t
  | Session_seed of Session_seed.Request.t
  | Session_list of Session.List_request.t
  | Session_search of Search_query.t
  | Session_search_navigate of Search_navigation.Request.t
  | Activity_list of Activity_query.t
  | Session_work of Session_work.Query.t
  | Session_configuration_get of Session_configuration.Get_request.t
  | Session_configuration_update of Session_configuration.Update_request.t
  | Session_get of Session.Get_request.t
  | Session_inference_summary of Inference_query.Summary_request.t
  | Session_inference_observations of Inference_query.Request.t
  | Session_attach of Session.Attach_request.t
  | Session_detach of Session.Detach_request.t
  | Session_renew_owner of Session.Renew_owner_request.t
  | Session_start of Session.Start_request.t
  | Project_create of Organization_request.Create.t
  | Project_get of Organization_request.Project.Get.t
  | Project_list of Organization_request.List.t
  | Project_update of Organization_request.Project.Update.t
  | Project_delete of Organization_request.Project.Delete.t
  | Collection_create of Organization_request.Create.t
  | Collection_get of Organization_request.Collection.Get.t
  | Collection_list of Organization_request.List.t
  | Collection_update of Organization_request.Collection.Update.t
  | Collection_delete of Organization_request.Collection.Delete.t
  | Session_update_metadata of Session_metadata.Request.t
  | Session_update_organization of Session_organization.Request.t
  | Session_stop of Session.Stop_request.t
  | Session_cancel_operation of Session.Cancel_operation_request.t
  | Session_runs of Run_query.Request.t
  | Session_run of Run_query.Lookup_request.t
  | Session_run_start of Run_start.t
  | Session_send_message of Session.Send_message_request.t
  | Session_pending_inputs of Pending_query.Request.t
  | Session_pending_input of Pending_query.Lookup_request.t
  | Session_cancel_pending_input of Pending_control.Cancel_request.t
  | Session_replace_pending_input of Pending_control.Replace_request.t
  | Session_compact of Session.Compact_request.t
  | Session_edit_history of History_edit.Edit_request.t
  | Session_continue_history of History_edit.Continue_request.t
  | Session_delete_history of Session.Delete_history_request.t
  | Session_export of Session.Export_request.t
  | Session_reset of Session.Reset_request.t
  | Session_rebuild of Session.Rebuild_request.t
  | Session_upgrade_prompt of Session.Upgrade_prompt_request.t
  | Session_delete of Session.Delete_request.t
  | Session_restore of Session_lifecycle.Request.t
  | Session_resume of Session_lifecycle.Request.t
  | Permission_list of Permission.List_request.t
  | Permission_respond of Permission.Respond_request.t
  | Grant_list of Grant.List_request.t
  | Grant_revoke of Grant.Revoke_request.t
  | Audit_read of Audit.Read_request.t
  | Job_list of Job.List_request.t
  | Job_get of Job.Get_request.t
  | Job_cancel of Job.Cancel_request.t
  | Schedule_list of Schedule.List_request.t
  | Schedule_get of Schedule.Get_request.t
  | Schedule_create of Schedule.Create_request.t
  | Schedule_cancel of Schedule.Cancel_request.t
  | Ingress_submit of Ingress.Submit_request.t
  | Authoring of (Authoring_wire.Request.t[@sexp.opaque])
[@@deriving sexp]

(** [method_name t] returns the stable protocol method. *)
val method_name : t -> string

(** [params t] encodes the command parameters. *)
val params : t -> Jsonaf.t

(** [of_method_and_params ~method_ ~params] performs closed typed dispatch. *)
val of_method_and_params : method_:string -> params:Jsonaf.t -> (t, Error.t) result

(** [supported_methods] is the codec inventory: every method accepted by this
    typed decoder. It does not assert that a server has installed or advertised
    a handler for the method. *)
val supported_methods : string list
```

## command_receipt

[implementation](../../lib/agent_protocol/command_receipt.ml) · [interface](../../lib/agent_protocol/command_receipt.mli)

```ocaml
(** Read-only reconciliation of an original bounded request. The authenticated
    principal is implicit. Original params prove mode/authority and digest;
    lookup never admits, retries or replays the command. *)
module Request : sig
  type t =
    { method_name : string
    ; original_params : Jsonaf.t
    }
  [@@deriving sexp]

  (** Shared admission/receipt bounds apply to original parameters; the small
      receipt envelope does not consume their byte or nesting allowance. *)
  val validate_original_params : Jsonaf.t -> (unit, Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

(** Narrow result references, never a replay of a full original result.
    Create/attach recovery must use a fresh authorized attachment. *)
type committed =
  | Provider_setup of Provider_operator.Setup_result.t
  | Provider_login of Provider_operator.Flow_ref.t
  | Provider_cancel of Provider_operator.Flow_result.t
  | Provider_logout of Provider_operator.Logout_result.t
  | Provider_selection of Provider_operator.Selection_result.t
  | Provider_configuration of Provider_operator.Configuration_result.t
  | Prompt_dependencies_updated of Prompt_dependency_selection.Update_result.t
  | Created_session of Id.Session.t
  | Attached_session of Id.Session.t
  | Session_mutation of
      { session_id : Id.Session.t
      ; mutation : Mutation_result.t
      }
  | Configuration_updated of
      { session_id : Id.Session.t
      ; revision : int64
      }
  | Edited_history of
      { session_id : Id.Session.t
      ; history_id : History.Id.t
      ; content_revision : History.Content_revision.t
      ; archived_revision : int64
      ; continuation : History_edit.Continuation.t
      ; mutation : Mutation_result.t
      }
  | Continued_history of
      { session_id : Id.Session.t
      ; continuation : History_edit.Continuation.t
      ; mutation : Mutation_result.t
      }
  | Accepted_run of
      { session_id : Id.Session.t
      ; receipt : Run_receipt.t
      }
  | Sent_message of
      { session_id : Id.Session.t
      ; history_id : History.Id.t
      ; operation_id : Id.Operation.t option
      ; mutation : Mutation_result.t
      }
  | Project_mutation of
      { project_id : Id.Project.t
      ; revision : int64
      }
  | Deleted_project of
      { project_id : Id.Project.t
      ; revision : int64
      }
  | Collection_mutation of
      { collection_id : Id.Collection.t
      ; revision : int64
      }
  | Deleted_collection of
      { collection_id : Id.Collection.t
      ; revision : int64
      }
  | Deleted_session of Id.Session.t
  | Permission_response of Id.Permission.t * Mutation_result.t
  | Revoked_grant of Id.Grant.t * Mutation_result.t
  | Cancelled_job of Id.Job.t * Mutation_result.t
  | Schedule_mutation of Id.Schedule.t * Mutation_result.t
[@@deriving sexp]

type t =
  | Missing
  | Unavailable
  | Pending of
      { accepted_sequence : int64 option
      ; expires_at : Timestamp.t option
      }
  | Failed of Error.t
  | Committed of committed
[@@deriving sexp]

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
```

## completion

[implementation](../../lib/agent_protocol/completion.ml) · [interface](../../lib/agent_protocol/completion.mli)

```ocaml
(** Terminal background outcomes, separate from initial tool acknowledgements. *)
type t =
  | Succeeded of Jsonaf.t
  | Failed of Invocation.tool_error
  | Cancelled of string
  | Expired
[@@deriving equal, sexp]

type wake =
  | Request_turn
  | Next_turn
  | No_wake
[@@deriving compare, equal, sexp]

val validate : t -> (unit, Error.t) result
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
val wake_to_json : wake -> Jsonaf.t
val wake_of_json : Jsonaf.t -> (wake, Error.t) result
```

## completion_contract

[implementation](../../lib/agent_protocol/completion_contract.ml) · [interface](../../lib/agent_protocol/completion_contract.mli)

```ocaml
open Core

(** Host-captured standalone completion contract. It does not authorize execution
    or delivery: the runtime must rebind the pinned publisher/tool ceiling and
    verify the owning invocation's actual Pending acknowledgement. *)
type t =
  { tool_name : string
  ; tool_fingerprint : string
  ; capability_pins : (string * string) list
  ; completion_schema : Jsonaf.t option
  ; max_output_bytes : int
  ; max_output_depth : int
  }
[@@deriving equal, sexp]

val validate : t -> (unit, Protocol_error.t) result
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Protocol_error.t) result
```

## completion_projection

[implementation](../../lib/agent_protocol/completion_projection.ml) · [interface](../../lib/agent_protocol/completion_projection.mli)

```ocaml
(** Evidence retained by a host-managed standalone completion adapter. The hashes
    bind the original immutable invocation contract and terminal job storage; a
    rejected projection never replaces the job's business result. This DTO alone
    does not authorize publication. Admission must validate the materialized
    completion against both the stored result and the original contract. *)
type t =
  { job_attempt : int
  ; contract_sha256 : string
  ; result_sha256 : string
  ; rejected : bool
  ; result_reference : Job_result_reference.t option [@sexp.option]
    (** Version2: the accepted original result is retained, and the bounded
        delivery contains its reference. Invalid original results never expose it. *)
  }
[@@deriving equal, sexp]

val validate : t -> (unit, Error.t) result
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result

(** Hash the exact canonical protocol representation, including an artifact's
    owner, outcome and content digest when the result is stored externally. *)
val contract_digest : Completion_contract.t -> string

val result_digest : Stored_completion.t -> string

(** Fixed, bounded public error, without rejected business data. *)
val rejection : Invocation.tool_error
```

## delivery

[implementation](../../lib/agent_protocol/delivery.ml) · [interface](../../lib/agent_protocol/delivery.mli)

```ocaml
(** Durable notification intent. Committing a delivery is only valid in the
    actor transaction that inserts its matching history entry. *)
type source =
  | Moderator
  | Job_adapter
  | External_ingress
[@@deriving compare, equal, sexp]

(** Actual creating moderator source and execution. An absent owner identifies
    a legacy/host-adapter record, never implicit authority for a current script. *)
module Subscription_binding = Delivery_subscription_binding

type ownership =
  { source : Invocation.observer
  ; creator : Job.launch_owner
  ; subscription_binding : Subscription_binding.t option [@sexp.option]
    (** Captured terminal subscription ID/epoch. Historical absence remains None
        and supplies no exact subscription wake authority. New public values use
        an explicit version-6 envelope; versions 1–5 retain their encoding. *)
  }
[@@deriving equal, sexp]

type context =
  { id : Id.Delivery.t
  ; session_id : Id.Session.t
  ; generation : int
  ; invocation_id : Id.Invocation.t option
  ; work : Invocation.work option
  ; correlation : string
  ; source : source
  ; completion : Completion.t
  ; wake : Completion.wake
  ; created_at : Timestamp.t
  ; ownership : ownership option [@sexp.option]
  }
[@@deriving equal, sexp]

type status =
  | Pending
  | Committed of
      { history_id : History_entry.Id.t
      ; at : Timestamp.t
      }
  | Failed of Invocation.tool_error
[@@deriving equal, sexp]

(** A durable request to wake after history insertion. Accepted binds the actual
    foreground operation admitted by the actor; it does not claim model success.
    Discarded retains a bounded explanation for policy/lifecycle rejection. *)
type wake_disposition =
  | Pending_wake
  | Accepted_wake of Id.Operation.t
  | Discarded_wake of string
[@@deriving equal, sexp]

type t = private
  { context : context
  ; attempt : int
  ; status : status
  ; wake_disposition : wake_disposition option [@sexp.option]
    (** Envelope3, or Envelope4 with disclosure pins. Only committed Request_turn deliveries carry
        this receipt. Source ownership is independent of wake tracking.
        Historical absent values do not acquire a new wake. *)
  ; disclosure_pins : (string * string) list option [@sexp.option]
    (** Envelope4: immutable ordered configuration pins for the publisher's exact
        tool ceiling. None is historical/untracked, not authority to use the whole
        current registry. Some [] is an explicitly empty ceiling. *)
  ; completion_projection : Completion_projection.t option [@sexp.option]
    (** Envelope5: immutable original-result evidence for a standalone adapter.
        The host validates the contract and actual job before admission. *)
  }
[@@deriving equal, sexp]

val create
  :  ?disclosure_pins:(string * string) list
  -> ?completion_projection:Completion_projection.t
  -> context
  -> (t, Error.t) result

val validate : t -> (unit, Error.t) result

(** New execution services opt into durable wake tracking with [track_wake:true].
    It creates Pending_wake only for Request_turn, for moderators or approved host
    adapters. The default preserves legacy publication behavior; reading or
    recommitting an existing record never synthesizes a new wake. *)
val commit
  :  ?track_wake:bool
  -> t
  -> history_id:History_entry.Id.t
  -> now:Timestamp.t
  -> (t, Error.t) result

(** Settle once, idempotently for the same disposition. The host must commit
    acceptance with admission of the named operation, using Delivery_wake_changed;
    these pure transitions alone neither authorize nor start a turn. *)
val accept_wake : t -> operation_id:Id.Operation.t -> (t, Error.t) result

val discard_wake : t -> reason:string -> (t, Error.t) result
val fail : t -> Invocation.tool_error -> (t, Error.t) result
val retry : t -> max_attempts:int -> (t, Error.t) result
val validate_transition : previous:t option -> t -> (unit, Error.t) result
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result

(** Complete current durable record projection. Required-null option fields;
    independent of the public protocol's historical envelope variants. *)
module Storage : sig
  type binding_field =
    | Missing
    | Nullable

  (** Changes only a None binding's wire presence. A real captured binding always
      emits its validated ID/epoch, regardless of the original field presence. *)
  val to_json_with_binding_field : t -> binding_field:binding_field -> Jsonaf.t

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end
```

## delivery_subscription_binding

[implementation](../../lib/agent_protocol/delivery_subscription_binding.ml) · [interface](../../lib/agent_protocol/delivery_subscription_binding.mli)

```ocaml
(** Immutable subscription epoch captured at actual notification admission.
    An ID alone cannot identify a delivered occurrence after rearming. This
    record conveys no permission: the owner also checks delivery source, creator,
    generation and the exact committed history identity. *)
type t = private
  { subscription_id : Id.Subscription.t
  ; epoch : int
  }
[@@deriving equal, sexp]

val create : subscription_id:Id.Subscription.t -> epoch:int -> (t, Error.t) result
val validate : t -> (unit, Error.t) result
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
```

## developer_job

[implementation](../../lib/agent_protocol/developer_job.ml) · [interface](../../lib/agent_protocol/developer_job.mli)

```ocaml
(** Pure developer-owned job record. An ID or this value grants no execution,
    result-read, cancellation, or source authority. The developer actor admits
    it only against the current principal, host, epoch, capture and live launch
    parent, then persists it under the real daemon-owned run root. *)

open! Core

module Launch : sig
  type parent =
    | Run_root
    | Invocation of Id.Invocation.t
  [@@deriving equal, sexp_of]

  type t

  (** [parent_job] names the exact creating attempt, if any. A run-root launch
      need not fabricate a native invocation. The optional attempt and depth
      must be nonnegative. The recorded ancestry alone cannot derive the live
      scheduler depth. These are comparison coordinates; the actor proves the
      live run scope or active invocation and depth separately. *)
  val create
    :  parent:parent
    -> parent_job:(Id.Job.t * int) option
    -> nested_depth:int
    -> (t, Protocol_error.t) result

  val parent : t -> parent
  val parent_job : t -> (Id.Job.t * int) option
  val nested_depth : t -> int
end

module Result_value : sig
  (** Artifact metadata is a selected result descriptor, not read authority.
      The result store binds the final reference to owner/job/attempt, stores
      verified bytes and checks disclosure. No session artifact is reused. *)
  type t =
    | Inline of Completion.t
    | Artifact of
        { outcome : Stored_completion.outcome
        ; blob : Blob.Metadata.t
        }
  [@@deriving sexp_of]
end

module State : sig
  type t =
    { status : Job.status
    ; attempt : int
    ; started_at : Timestamp.t option
    ; next_run_at : Timestamp.t option
    ; completed_at : Timestamp.t option
    ; result : Result_value.t option
    ; retry_failure : Protocol_error.t option
    }
  [@@deriving sexp_of]

  (** Validates nonnegative attempt and retry coordinates. A new queued attempt
      has no prior execution state. A queued retry retains its prior attempt
      count, clears [started_at], sets [next_run_at] and records the previous
      failure in [retry_failure], separate from the terminal [result].
      Running and waiting attempts are positive, started, incomplete and have no
      result; a dependency wait requires an async tool, a deadline no earlier
      than creation and cannot target its own job ID. Success requires a claimed
      attempt, start, completion time and matching result; failure, cancellation
      and interruption may precede claim but require a completion time; only a
      claimed attempt has a current [started_at]. Inline
      completions and artifact metadata are validated. Wall-clock timestamps
      are not ordered against each other: the host clock may step backward.
      This proves record shape, not authority to make the transition. *)
  val validate
    :  job_id:Id.Job.t
    -> kind:Job.kind
    -> created_at:Timestamp.t
    -> t
    -> (unit, Protocol_error.t) result
end

type t

val create
  :  owner:Execution_owner.developer
  -> capture_fingerprint:Chatml_source.Digest.t
  -> id:Id.Job.t
  -> launch:Launch.t
  -> kind:Job.kind
  -> payload:Jsonaf.t
  -> retry_policy:Job.retry_policy
  -> created_at:Timestamp.t
  -> state:State.t
  -> (t, Protocol_error.t) result

(** Recheck the same immutable identity and lifecycle shape after a proposed
    actor-owned transition. The actor checks previous/current CAS revision,
    attempt, live parent and execution policy; this pure function is no grant. *)
val with_state : t -> State.t -> (t, Protocol_error.t) result

val owner : t -> Execution_owner.developer
val capture_fingerprint : t -> Chatml_source.Digest.t
val id : t -> Id.Job.t
val launch : t -> Launch.t
val kind : t -> Job.kind
val payload : t -> Jsonaf.t
val retry_policy : t -> Job.retry_policy
val created_at : t -> Timestamp.t
val state : t -> State.t

(** Exact immutable claim identity, excluding mutable lifecycle state. The
    comparison includes owner/capture/ID, launch, kind, payload, retry policy
    and creation timestamp. It grants no authority by itself. *)
val same_immutable : t -> t -> bool

(** Version-one developer-only record codec. It preserves the exact immutable
    launch/owner/capture/payload/retry fields and validated state. Neither a
    decoded record nor a result descriptor grants execution or disclosure. *)
val to_json : t -> Jsonaf.t

val of_json : Jsonaf.t -> (t, Protocol_error.t) result
```

## envelope

[implementation](../../lib/agent_protocol/envelope.ml) · [interface](../../lib/agent_protocol/envelope.mli)

```ocaml
(** Ochat JSON-RPC-style transport envelopes. *)

module Request_id : sig
  type t [@@deriving compare, sexp]

  (** [of_json json] accepts a string or number request identifier. *)
  val of_json : Jsonaf.t -> (t, Error.t) result

  (** [to_json t] returns the exact JSON identifier represented by [t]. *)
  val to_json : t -> Jsonaf.t
end

type request =
  { id : Request_id.t
  ; method_ : string
  ; params : Jsonaf.t
  }
[@@deriving sexp]

type notification =
  { method_ : string
  ; params : Jsonaf.t
  }
[@@deriving sexp]

type response =
  { id : Request_id.t
  ; outcome : (Jsonaf.t, Error.t) result
  }
[@@deriving sexp]

type t =
  | Request of request
  | Notification of notification
  | Response of response
[@@deriving sexp]

(** [request ~id ~method_ ?params ()] creates a request envelope. *)
val request : id:Request_id.t -> method_:string -> ?params:Jsonaf.t -> unit -> t

(** [notification ~method_ ?params ()] creates a notification envelope. *)
val notification : method_:string -> ?params:Jsonaf.t -> unit -> t

(** [success ~id result] creates a successful response envelope. *)
val success : id:Request_id.t -> Jsonaf.t -> t

(** [failure ~id error] creates a failed response envelope. *)
val failure : id:Request_id.t -> Error.t -> t

(** [to_json t] encodes [t] as an Ochat JSON-RPC envelope. *)
val to_json : t -> Jsonaf.t

(** [of_json json] decodes one Ochat JSON-RPC envelope. *)
val of_json : Jsonaf.t -> (t, Error.t) result
```

## error

[implementation](../../lib/agent_protocol/error.ml) · [interface](../../lib/agent_protocol/error.mli)

```ocaml
include module type of Protocol_error
```

## event

[implementation](../../lib/agent_protocol/event.ml) · [interface](../../lib/agent_protocol/event.mli)

```ocaml
(** Durable session events and recoverable operation-scoped live events. *)

module Durable : sig
  type kind =
    | Session_created
    | Session_state_changed
    | Session_updated
    | Attachment_owner_changed
    | History_message_deferred
    | History_appended
    | History_replaced
    | Moderator_overlay_changed
    | Moderator_notification
    | Permission_requested
    | Permission_resolved
    | Grant_created
    | Grant_revoked
    | Operation_started
    | Operation_completed
    | Operation_failed
    | Operation_cancelled
    | Operation_interrupted
    | Job_state_changed
    | Schedule_created
    | Schedule_state_changed
    | Schedule_cancelled
    | Prompt_upgraded
    | Workspace_state_changed
    | Session_error
  [@@deriving compare, equal, sexp]

  type visibility =
    | Full
    | Redacted
    | Hidden
  [@@deriving compare, equal, sexp]

  module Payload : sig
    type lifecycle_change =
      { desired_state : Session.desired_state
      ; observed_state : Session.observed_state
      }
    [@@deriving sexp]

    type prompt_upgrade =
      { prompt_id : Id.Prompt_definition.t
      ; previous_revision : Id.Prompt_revision.t
      ; current_revision : Id.Prompt_revision.t
      }
    [@@deriving sexp]

    type t =
      | Session_created of Session.t
      | Session_state_changed of lifecycle_change
      | Session_updated of Session.t
      | Attachment_owner_changed of Session.Attachment.t option
      | History_message_deferred of History.entry
      | History_appended of History.entry list
      | History_replaced of History.Window.t
      | Moderator_overlay_changed of Jsonaf.t
      | Moderator_notification of Jsonaf.t
      | Permission_requested of Permission.t
      | Permission_resolved of Permission.t
      | Grant_created of Grant.t
      | Grant_revoked of Grant.t
      | Operation_started of Operation.t
      | Operation_completed of Operation.t
      | Operation_failed of Operation.t
      | Operation_cancelled of Operation.t
      | Operation_interrupted of Operation.t
      | Job_state_changed of Job.t
      | Schedule_created of Schedule.t
      | Schedule_state_changed of Schedule.t
      | Schedule_cancelled of Schedule.t
      | Prompt_upgraded of prompt_upgrade
      | Workspace_state_changed of Workspace.t
      | Session_error of Error.t
    [@@deriving sexp]

    val kind : t -> kind
    val to_json : t -> Jsonaf.t
    val of_json : kind:kind -> Jsonaf.t -> (t, Error.t) result
  end

  type t =
    { session_id : Id.Session.t
    ; sequence : int64
    ; revision : int64
    ; timestamp : Timestamp.t
    ; kind : kind
    ; visibility : visibility
    ; payload : Jsonaf.t
    }
  [@@deriving sexp]

  (** [to_json t] encodes the parameters of a [session.event] notification. *)
  val to_json : t -> Jsonaf.t

  (** Optional complete status projection on [Session_updated]. Older clients
      ignore this field; an empty list explicitly clears previously held state. *)
  val with_extension_status : t -> Extension_status.t list -> t

  val extension_status : t -> (Extension_status.t list option, Error.t) result

  (** [with_replacement_snapshot event snapshot] adds complete replacement state
      to a [Session_updated] event and binds its session revision/sequence to the
      event. Other event kinds are unchanged. Filter the snapshot for the reader
      before publishing it. Older payloads omit this additive field. *)
  val with_replacement_snapshot : t -> Snapshot.t -> t

  (** [replacement_snapshot event] decodes optional replacement state and rejects
      mismatched session/revision/sequence anchors. *)
  val replacement_snapshot : t -> (Snapshot.t option, Error.t) result

  (** [of_json json] decodes a durable event projection. *)
  val of_json : Jsonaf.t -> (t, Error.t) result

  (** [of_payload ... payload] constructs a full durable wire event with a
      kind derived from the closed payload variant. *)
  val of_payload
    :  session_id:Id.Session.t
    -> sequence:int64
    -> revision:int64
    -> timestamp:Timestamp.t
    -> Payload.t
    -> t

  (** [to_notification t] wraps the event in a JSON-RPC notification. *)
  val to_notification : t -> Envelope.t
end

module Recoverable : sig
  type payload =
    | Transcript of Transcript.Stream.t
    | Tool_activity of Activity.Tool.event
  [@@deriving sexp_of]

  type t = private
    { session_id : Id.Session.t
    ; operation_id : Id.Operation.t
    ; operation_sequence : int64
    ; anchor_sequence : int64
    ; timestamp : Timestamp.t
    ; invocation_id : Id.Invocation.t option
    ; parent_invocation_id : Id.Invocation.t option
    ; payload : payload
    ; original_json : Jsonaf.t option
      (** Immutable admitted received envelope, retaining unknown fields for exact
          duplicate detection. Native events have [None]. *)
    }
  [@@deriving sexp_of]

  (** Positive operation sequence, nonnegative durable anchor; complete encoded
      envelope is bounded to 16 MiB. Invocation IDs are actual host IDs only. *)
  val create
    :  session_id:Id.Session.t
    -> operation_id:Id.Operation.t
    -> operation_sequence:int64
    -> anchor_sequence:int64
    -> timestamp:Timestamp.t
    -> invocation_id:Id.Invocation.t option
    -> parent_invocation_id:Id.Invocation.t option
    -> payload
    -> (t, Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
  val to_notification : t -> Envelope.t
end
```

## execution_owner

[implementation](../../lib/agent_protocol/execution_owner.ml) · [interface](../../lib/agent_protocol/execution_owner.mli)

```ocaml
(** Pure ownership coordinates for host-executed invocations. No constructor in
    this module authenticates a principal, admits a run, or grants an effect. *)

type conversation = private
  { session_id : Id.Session.t
  ; generation : int
  }
[@@deriving compare, equal, sexp_of]

type developer = private
  { host_id : Id.Server.t
  ; run_id : Id.Developer_run.t
  ; epoch : int
  ; principal_id : Id.Principal.t
  }
[@@deriving compare, equal, sexp_of]

type t =
  | Conversation of conversation
  | Developer of developer
[@@deriving compare, equal, sexp_of]

(** Revalidate every typed ID and nonnegative revision coordinate at an
    admission boundary. S-expression display is intentionally one-way. *)
val validate : t -> (unit, Error.t) result

val conversation : session_id:Id.Session.t -> generation:int -> (t, Error.t) result

(** The host issues these coordinates only after run/capture admission. The
    developer actor must still compare them with its current authenticated owner,
    epoch, source capture, and durable host ID at every effectful boundary.
    A host ID does not prove a live process lease. *)
val developer
  :  host_id:Id.Server.t
  -> run_id:Id.Developer_run.t
  -> epoch:int
  -> principal_id:Id.Principal.t
  -> (t, Error.t) result
```

## extension_capabilities

[implementation](../../lib/agent_protocol/extension_capabilities.ml) · [interface](../../lib/agent_protocol/extension_capabilities.mli)

```ocaml
(** Versioned host qualification, separate from record-codec support. Presence of
    this metadata does not enable any model-visible tool or authorize effects. *)

type host =
  | Daemon
  | Embedded_durable
  | Embedded_transient
  | Direct
[@@deriving compare, equal, sexp]

type journal_flush =
  | Synced
  | Buffered
  | Memory
[@@deriving compare, equal, sexp]

type t = private
  { host : host
  ; journal_flush : journal_flush
  ; available_features : string list
  }
[@@deriving sexp]

val known_features : string list

val create
  :  host:host
  -> journal_flush:journal_flush
  -> available_features:string list
  -> (t, Error.t) result

(** Filter only the extension namespace; preserve unrelated protocol features.
    Host options cannot advertise an extension that has not been qualified. *)
val filter_available : t -> string list -> string list

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
```

## extension_status

[implementation](../../lib/agent_protocol/extension_status.ml) · [interface](../../lib/agent_protocol/extension_status.mli)

```ocaml
(** Payload-free extension lifecycle summaries. These deliberately omit arguments,
    results, errors, schemas, capability identities and arbitrary correlation text.
    Servers must still filter them by the principal's security-view scope. *)

type kind =
  | Invocation
  | Subscription
  | Delivery
  | Moderator_execution
[@@deriving compare, equal, sexp]

type t = private
  { kind : kind
  ; id : string
  ; generation : int
  ; state : string
  }
[@@deriving equal, sexp]

val invocation : Invocation.t -> t
val subscription : Subscription.t -> t
val delivery : Delivery.t -> t
val moderator_execution : Moderator_execution.t -> t
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result

(** Rejects duplicate identities, including ambiguous updates in one event. *)
val list_of_json : Jsonaf.t -> (t list, Error.t) result
```

## grant

[implementation](../../lib/agent_protocol/grant.ml) · [interface](../../lib/agent_protocol/grant.mli)

```ocaml
(** Durable invocation grants and revocation requests. *)

type scope =
  | Exact_session
  | Prefix_session
  | Durable_exact
[@@deriving compare, equal, sexp]

type state =
  | Active
  | Revoked
  | Expired
[@@deriving compare, equal, sexp]

type t =
  { id : Id.Grant.t
  ; session_id : Id.Session.t
  ; principal_id : Id.Principal.t
  ; tool_name : string
  ; identity_digest : string
  ; scope : scope
  ; state : state
  ; created_at : Timestamp.t
  ; expires_at : Timestamp.t option
  ; revoked_at : Timestamp.t option
  ; revocation_reason : string option
  }
[@@deriving sexp]

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result

module List_request : sig
  type t =
    { page : Page.Request.t
    ; session_id : Id.Session.t option
    ; principal_id : Id.Principal.t option
    ; state : state option
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Revoke_request : sig
  type t =
    { grant_id : Id.Grant.t
    ; session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; reason : string
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Revoke_result : sig
  type nonrec t =
    { grant : t
    ; mutation : Mutation_result.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end
```

## health

[implementation](../../lib/agent_protocol/health.ml) · [interface](../../lib/agent_protocol/health.mli)

```ocaml
(** Public and administratively detailed daemon health projections. *)

type status =
  | Healthy
  | Degraded
  | Unhealthy
[@@deriving compare, equal, sexp]

module Request : sig
  type t = { include_details : bool } [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Component : sig
  type t =
    { name : string
    ; status : status
    ; message : string option
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Response : sig
  type t =
    { status : status
    ; ready : bool
    ; draining : bool
    ; checked_at : Timestamp.t
    ; components : Component.t list
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end
```

## history

[implementation](../../lib/agent_protocol/history.ml) · [interface](../../lib/agent_protocol/history.mli)

```ocaml
(** Presentation-neutral canonical and moderated transcript projections.
    Entry equality preserves exact JSON payload structure and object field order. *)

type delivery_id = Id.Delivery.t [@@deriving equal, sexp]

module Id : sig
  type t = History_entry.Id.t [@@deriving compare, equal, hash, sexp]

  val of_string : string -> (t, Error.t) result
  val to_string : t -> string
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Content_revision : sig
  type t [@@deriving compare, equal, sexp]

  val zero : t
  val of_int64 : int64 -> (t, Error.t) result
  val to_int64 : t -> int64
  val succ : t -> (t, Error.t) result

  (** Canonical decimal strings avoid precision loss in public and stored data. *)
  val to_json : t -> Jsonaf.t

  val of_json : Jsonaf.t -> (t, Error.t) result
end

type role =
  | System
  | User
  | Assistant
  | Tool
[@@deriving compare, equal, sexp]

type kind =
  | Message
  | Reasoning
  | Tool_call
  | Tool_output
  | Other
[@@deriving compare, equal, sexp]

(** Classification of the existing neutral semantic payload. No provider DTO
    lowering or execution occurs. Developer messages project as System while
    retaining their actual neutral role in the payload. *)
val classification : History_entry.Payload.Semantic.t -> role * kind

type provenance =
  | Canonical
  | Moderator_inserted
  | Moderator_replaced of Id.t
  | Runtime_notification of delivery_id
  | Runtime_authoring of Authoring_guidance.t
[@@deriving equal, sexp]

val provenance_to_json : provenance -> Jsonaf.t
val provenance_of_json : Jsonaf.t -> (provenance, Error.t) result

type entry =
  { id : Id.t
  ; content_revision : Content_revision.t
  ; role : role
  ; kind : kind
  ; payload : Jsonaf.t
  ; provenance : provenance
  ; redacted : bool
  }
[@@deriving equal, sexp]

val entry_to_json : entry -> Jsonaf.t

(** Validate host provenance metadata. This does not decode a provider item or
    claim the original guidance payload is still present; use the presence hook
    after applying effective-history edits to determine that. *)
val validate_entry : entry -> (unit, Error.t) result

val entry_of_json : Jsonaf.t -> (entry, Error.t) result

module Window_request : sig
  type position =
    | Tail of int
    | After of Id.t
    | Before of Id.t
    | Cursor of Page.Cursor.t
  [@@deriving sexp]

  type t =
    { position : position
    ; limit : int
    ; effective : bool
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Window : sig
  type t =
    { entries : entry list
    ; previous_cursor : Page.Cursor.t option
    ; next_cursor : Page.Cursor.t option
    ; reached_start : bool
    ; reached_end : bool
    ; structurally_complete : bool
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end
```

## history_edit

[implementation](../../lib/agent_protocol/history_edit.ml) · [interface](../../lib/agent_protocol/history_edit.mli)

```ocaml
(** A complete replacement of one canonical user-text occurrence. This intent
    grants no writer, runtime or archive authority. Text is bounded UTF-8; empty
    text is valid. The content revision identifies the exact saved occurrence. *)
module Mode : sig
  type t =
    | Save_only
    | Edit_and_continue
  [@@deriving equal, sexp]
end

module Unsupported_target : sig
  type t =
    | Not_plain_user_text
    | Overlay_override
    | Tool_pair_crosses_boundary
    | Initial_instruction
  [@@deriving equal, sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

type t [@@deriving sexp]

val create
  :  history_id:History.Id.t
  -> expected_content_revision:History.Content_revision.t
  -> text:string
  -> mode:Mode.t
  -> (t, Error.t) result

val history_id : t -> History.Id.t
val expected_content_revision : t -> History.Content_revision.t
val text : t -> string
val mode : t -> Mode.t
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result

module Edit_request : sig
  type intent = t [@@deriving sexp]

  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; expected_generation : int
    ; expected_revision : int64
    ; edit : intent
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  (** Decode validates nonnegative generation/session revision and complete edit. *)
  val to_json : t -> Jsonaf.t

  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Continue_request : sig
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; expected_generation : int
    ; expected_revision : int64
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Continuation : sig
  type unavailable =
    | Stopped
    | Runtime_unavailable
  [@@deriving equal, sexp]

  type t =
    | Not_requested
    | Started of Id.Operation.t
    | Not_started of unavailable
  [@@deriving equal, sexp]

  (** Started means a committed actual host Turn operation, never provider paid
      submission. Not_started creates no latent intent or runtime activation. *)
  val to_json : t -> Jsonaf.t

  val of_json : Jsonaf.t -> (t, Error.t) result
end
```

## host_invocation

[implementation](../../lib/agent_protocol/host_invocation.ml) · [interface](../../lib/agent_protocol/host_invocation.mli)

```ocaml
(** An owner-neutral invocation record for the shared admission path. The
    conversation arm retains the entire original [Invocation.t], including
    provider/history/routing/observation/handler/publication metadata. Existing
    conversation codecs and actor transitions remain authoritative. Developer
    records have no synthetic Session ID, model call, or provider history. *)

type context =
  { id : Id.Invocation.t
  ; owner : Execution_owner.t
  ; origin : Invocation.origin
  ; parent_invocation : Id.Invocation.t option
  ; parent_job : Id.Job.t option
  ; tool_name : string
  ; implementation_revision : string
  ; capability_fingerprint : string
  ; input : Jsonaf.t
  ; created_at : Timestamp.t
  ; deadline : Timestamp.t option
  }
[@@deriving equal, sexp_of]

type t

(** Lossless adapter. Projection returns the exact original conversation record,
    not a reconstruction from [context]. *)
val of_conversation : Invocation.t -> (t, Error.t) result

val to_conversation : t -> Invocation.t option

(** Pure data construction, not admission. Only a trusted developer actor may
    persist/dispatch it after checking the exact owner and selected capture.
    Model/Moderator origins and provider-history fields have no developer form. *)
val create_developer : context -> (t, Error.t) result

val context : t -> context
val status : t -> Invocation.status
val outcome : t -> Invocation.outcome option
val validate : t -> (unit, Error.t) result
val dispatch : t -> (t, Error.t) result
val resolve : t -> Invocation.outcome -> (t, Error.t) result
val cancel : t -> reason:string -> (t, Error.t) result

(** Persist only the developer arm in the daemon-owned run document. This
    version-one value encoding rejects conversation records, unknown fields and
    Published status; developer invocations have no provider publication. Decode
    reconstructs through validated pure transitions and grants no authority. *)
val developer_to_json : t -> (Jsonaf.t, Error.t) result

val developer_of_json : Jsonaf.t -> (t, Error.t) result
```

## host_job

[implementation](../../lib/agent_protocol/host_job.ml) · [interface](../../lib/agent_protocol/host_job.mli)

```ocaml
(** Lossless internal job envelope for the shared capacity and scheduler path.
    Conversation retains the exact original Job.t, including legacy fields and
    codecs; developer has a distinct validated record and no Session ID. *)
open! Core

type t

val of_conversation : Job.t -> (t, Protocol_error.t) result
val of_developer : Developer_job.t -> (t, Protocol_error.t) result
val to_conversation : t -> Job.t option
val to_developer : t -> Developer_job.t option

module Identity : sig
  type t [@@deriving compare, equal, sexp_of]

  include Comparator.S with type t := t

  (** Owner-qualified stable job ID. Epoch/generation belongs to the owner;
      attempt remains a separate exact reservation/claim coordinate. *)
  val owner : t -> Execution_owner.t

  val job_id : t -> Id.Job.t
end

val identity : t -> Identity.t
val owner : t -> Execution_owner.t
val id : t -> Id.Job.t
val kind : t -> Job.kind
val status : t -> Job.status
val attempt : t -> int
val nested_depth : t -> int

module Lifecycle : sig
  (** One read-only classification used by capacity and the later scheduler for
      both records. [classify] requires a validated nonnegative attempt; call
      [lifecycle] for an admitted Host_job. It does not replace actor-owned
      state transitions. *)
  type t =
    | New_queued
    | Queued_retry
    | Claimed_or_waiting
    | Terminal
  [@@deriving equal, sexp_of]

  val classify : status:Job.status -> attempt:int -> t
end

val lifecycle : t -> Lifecycle.t
```

## id

[implementation](../../lib/agent_protocol/id.ml) · [interface](../../lib/agent_protocol/id.mli)

```ocaml
(** Opaque identifiers used by the Ochat agent protocol. *)

module Generator : sig
  type t

  (** [create ~bytes] creates an identifier generator backed by [bytes].
      [bytes length] must return exactly [length] bytes. *)
  val create : bytes:(int -> string) -> t

  (** [secure] uses the process cryptographic random generator. *)
  val secure : t
end

module type S = sig
  type t [@@deriving compare, equal, hash, sexp]

  (** [create ()] creates a cryptographically random identifier. *)
  val create : unit -> t

  (** [create_with generator] creates an identifier using [generator]. *)
  val create_with : Generator.t -> t

  (** [of_string value] validates and parses [value]. *)
  val of_string : string -> (t, Error.t) result

  (** [to_string t] returns the opaque wire representation of [t]. *)
  val to_string : t -> string

  (** [to_json t] encodes [t] as a JSON string. *)
  val to_json : t -> Jsonaf.t

  (** [of_json json] decodes and validates a JSON string identifier. *)
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Server : S
module Session : S
module Run : S

(* Developer run ownership, distinct from conversation Run and Session.
   An ID is a comparison coordinate, never authority by possession. *)
module Developer_run : S
module Attachment : S
module Operation : S
module Event_cursor : S
module Transaction : S
module Job : S
module Invocation : S
module Moderator_execution : S
module Subscription : S
module Delivery : S
module Capability : S
module Ingress_event : S
module Schedule : S
module Permission : S
module Grant : S
module Workspace_definition : S
module Workspace_instance : S
module Prompt_definition : S
module Prompt_revision : S
module Principal : S
module Blob : S
module Idempotency_record : S
module Project : S
module Collection : S

(** Authoring coordinates are separate from conversation/run/job identities.
    They are never authority by possession; every use is reauthorized against
    the authenticated principal, host and connection lease. *)
module Authoring_root : S

module Authoring_context : S
module Authoring_analysis : S
module Authoring_upload : S
module Authoring_bundle : S
```

## idempotency_key

[implementation](../../lib/agent_protocol/idempotency_key.ml) · [interface](../../lib/agent_protocol/idempotency_key.mli)

```ocaml
(** Client-generated keys used to make mutating commands safely repeatable. *)

type t [@@deriving compare, equal, hash, sexp]

(** [of_string encoded] validates a nonempty, bounded wire key. *)
val of_string : string -> (t, Error.t) result

(** [to_string t] returns the validated wire representation. *)
val to_string : t -> string

(** [of_json json] decodes a JSON string key. *)
val of_json : Jsonaf.t -> (t, Error.t) result

(** [to_json t] encodes a JSON string key. *)
val to_json : t -> Jsonaf.t
```

## inference_query

[implementation](../../lib/agent_protocol/inference_query.ml) · [interface](../../lib/agent_protocol/inference_query.mli)

```ocaml
(** Additive protocol2 inference reads. No OpenAI, private Target, raw request,
    credentials, instructions/tool bodies or arbitrary diagnostic messages.
    These are retained-window totals, NEVER lifetime claims after retirement. *)
module Features : sig
  (** Optional method/display support, independent of authorization. These are
      ordinary initialize features, not extension capability catalog entries. *)
  val observations : string

  val configuration : string
  val diagnostics : string
  val all : string list
end

module Summary_request : sig
  type t = { session_id : Id.Session.t } [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end

module Coverage : sig
  type limit_kind =
    | Attempt_count
    | Turn_count
    | Retained_bytes
    | Protected_future_data
  [@@deriving equal, sexp_of]

  type tracking_status =
    | Available
    | Limited of limit_kind
  [@@deriving equal, sexp_of]

  type t = private
    { before_tracking_unknown : bool
    ; retired_attempts : int64
    ; untracked_attempts : int64
    ; retired_turns : int64
    ; untracked_turns : int64
    ; tracking_status : tracking_status
    }

  val create
    :  before_tracking_unknown:bool
    -> retired_attempts:int64
    -> untracked_attempts:int64
    -> retired_turns:int64
    -> untracked_turns:int64
    -> tracking_status:tracking_status
    -> (t, Error.t) Result.t

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end

module Metric : sig
  type sum =
    | Tokens of int64
    | Overflow
  [@@deriving equal, sexp_of]

  type unknown_counts =
    { not_reported : int64
    ; explicit_null : int64
    ; interrupted : int64
    ; not_submitted : int64
    ; before_tracking : int64
    }

  type t = private
    { actual : sum
    ; actual_attempts : int64
    ; estimated : sum
    ; estimated_attempts : int64
    ; mixed_estimators : bool
    ; unknown : unknown_counts
    }

  (** Nonnegative per-component contributions over retained attempts. Estimated
      contributions remain separately labelled; mixed_estimators prevents a
      fabricated uniform algorithm claim. Missing usage contributes explicit
      unknown reasons, not zero. No component total adds cache/reasoning subsets
      to input/output. Pricing/currency does not participate. *)
  val create
    :  actual:sum
    -> actual_attempts:int64
    -> estimated:sum
    -> estimated_attempts:int64
    -> mixed_estimators:bool
    -> unknown:unknown_counts
    -> (t, Error.t) Result.t
end

module Summary : sig
  type turns =
    { pending : int64
    ; completed : int64
    ; failed : int64
    ; cancelled : int64
    ; interrupted : int64
    }

  type components =
    { input : Metric.t
    ; output : Metric.t
    ; reported_total : Metric.t
    ; cached_input : Metric.t
    ; cache_write_input : Metric.t
    ; reasoning_output : Metric.t
    }

  type t [@@deriving sexp]

  (** Bounded 8 KiB optional session summary. Component values are independent
      metrics, not six quantities to sum. Attempt and actual host-turn counts
      refer to their retained windows; coverage records discarded/untracked
      evidence and pre-tracking uncertainty. Counts/revisions nonnegative. *)
  val create
    :  retained_attempts:int64
    -> turns:turns
    -> components:components
    -> coverage:Coverage.t
    -> accounting_revision:int64
    -> (t, Error.t) Result.t

  val retained_attempts : t -> int64
  val turns : t -> turns
  val components : t -> components
  val coverage : t -> Coverage.t
  val accounting_revision : t -> int64
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end

module Attempt : sig
  type t

  (** Usage/Context/state rows require transcript scope AND session visibility.
      Detailed Configuration/account alias/diagnostics additionally require
      Diagnostics. The encoder never exposes hidden full
      Attempt_record JSON as a shortcut. Ordinal orders actual durable admission;
      Operation/Invocation are real host associations or absent. *)
  val create
    :  ordinal:int64
    -> generation:int
    -> scope:Transcript.Scope.t
    -> operation_id:Id.Operation.t option
    -> invocation_id:Id.Invocation.t option
    -> accounting_id:Inference.Observation.Observation_id.t
    -> state:Inference.Observation.Attempt_record.state
    -> usage:Inference.Observation.t option
    -> context:Inference.Observation.t option
    -> configuration:Inference.Observation.Configuration.t option
    -> diagnostics:Inference.Observation.t list option
    -> omitted_diagnostics:int64 option
    -> (t, Error.t) Result.t

  val ordinal : t -> int64
  val generation : t -> int
  val scope : t -> Transcript.Scope.t
  val operation_id : t -> Id.Operation.t option
  val invocation_id : t -> Id.Invocation.t option
  val accounting_id : t -> Inference.Observation.Observation_id.t
  val state : t -> Inference.Observation.Attempt_record.state
  val usage : t -> Inference.Observation.t option
  val context : t -> Inference.Observation.t option
  val configuration : t -> Inference.Observation.Configuration.t option

  (** Actual selected transport, disclosed with configuration. Initial nomination
      in Configuration is never presented as evidence of actual WS dispatch. *)
  val transport_selection : t -> Inference.Observation.t option

  val with_transport_selection
    :  t
    -> Inference.Observation.t option
    -> (t, Error.t) Result.t

  val diagnostics : t -> Inference.Observation.t list option
  val omitted_diagnostics : t -> int64 option
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end

module Request : sig
  type t =
    { session_id : Id.Session.t
    ; page : Page.Request.t
    ; include_configuration : bool
    ; include_diagnostics : bool
    }
  [@@deriving sexp]

  (** Default page128; actual request <=advertised max_page_size (normally1000)
      checked by host, because generic Page.Request permits a larger ceiling.
      Cursors bind principal/session/generation/accounting_revision/filter/order;
      changed accounting returns explicit Conflict/restart, never mixed pages. *)
  val create
    :  session_id:Id.Session.t
    -> page:Page.Request.t
    -> include_configuration:bool
    -> include_diagnostics:bool
    -> (t, Error.t) Result.t

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end

module Response : sig
  type t [@@deriving sexp]

  (** Fully measured result within the inference-query per-RPC envelope policy
      (default16 MiB), using the allowance left after measuring the actual RPC ID
      and envelope wrapper, and including the actual signed cursor. HTTP batch
      aggregate and unrelated response limits are unchanged; input limits do not
      supply this response policy. Byte admission happens before
      next-row append/serialized allocation; a valid row that cannot fit returns
      a structured error, never a non-advancing cursor loop. *)
  val create
    :  summary:Summary.t
    -> attempts:Attempt.t Page.t
    -> max_bytes:int
    -> (t, Error.t) Result.t

  val summary : t -> Summary.t
  val attempts : t -> Attempt.t Page.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> max_bytes:int -> (t, Error.t) Result.t

  module Builder : sig
    type response = t
    type t

    (** [max_bytes] is the actual remaining result-body allowance after measuring
        the RPC/envelope wrapper, at most 16 MiB. Immutable builder caches bounded
        row measurements; no complete array is rebuilt on each append. *)
    val create : summary:Summary.t -> max_bytes:int -> (t, Error.t) Result.t

    (** None means capacity with prior builder unchanged; an empty page that
        cannot fit its first valid row errors. Cursor is the actual next cursor
        after this candidate, included in the admission BEFORE append. At most
        1000 rows. No cursor can silently point past an unappended row. *)
    val add
      :  t
      -> Attempt.t
      -> next_cursor:Page.Cursor.t option
      -> (t option, Error.t) Result.t

    val finish : t -> (response, Error.t) Result.t
  end
end
```

## ingress

[implementation](../../lib/agent_protocol/ingress.ml) · [interface](../../lib/agent_protocol/ingress.mli)

```ocaml
(** Scoped external DATA admission. A registration ID is not a credential.
    The server authenticates the producer separately; callers cannot supply it. *)
module Submit_request : sig
  type t =
    { session_id : Id.Session.t
    ; registration_id : Id.Capability.t
    ; namespace : string
    ; idempotency_key : Idempotency_key.t
    ; payload : Jsonaf.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

(** Durable acceptance acknowledgement, not handler execution or model completion.
    Matching retries return the same event identity, digest and acceptance time. *)
module Acknowledgement : sig
  type t =
    { session_id : Id.Session.t
    ; registration_id : Id.Capability.t
    ; event_id : Id.Ingress_event.t
    ; idempotency_key : Idempotency_key.t
    ; payload_sha256 : string
    ; accepted_at : Timestamp.t
    }
  [@@deriving equal, sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end
```

## initialize

[implementation](../../lib/agent_protocol/initialize.ml) · [interface](../../lib/agent_protocol/initialize.mli)

```ocaml
(** Connection initialization and protocol capability negotiation. *)

module Implementation : sig
  type t =
    { name : string
    ; version : string
    }
  [@@deriving sexp]

  val create : name:string -> version:string -> (t, Error.t) result
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

type event_encoding =
  | Json
  | Ndjson
[@@deriving compare, equal, sexp]

module Request : sig
  type t =
    { implementation : Implementation.t
    ; protocol_min : Version.t
    ; protocol_max : Version.t
    ; features : string list
    ; event_encodings : event_encoding list
    ; max_inbound_event_bytes : int
    ; client_instance_id : string option
    }
  [@@deriving sexp]

  val create
    :  implementation:Implementation.t
    -> protocol_min:Version.t
    -> protocol_max:Version.t
    -> features:string list
    -> event_encodings:event_encoding list
    -> max_inbound_event_bytes:int
    -> ?client_instance_id:string
    -> unit
    -> (t, Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Limits : sig
  type t =
    { max_request_bytes : int
    ; max_event_bytes : int
    ; max_page_size : int
    ; max_attachments_per_connection : int
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Event_retention : sig
  type t =
    { minimum_age_ms : int
    ; maximum_events : int
    ; oldest_replayable_sequence : int64 option
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Timing : sig
  type t =
    { heartbeat_interval_ms : int
    ; owner_lease_duration_ms : int
    ; owner_renew_after_ms : int
    ; disconnect_grace_default_ms : int
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Response : sig
  type t =
    { protocol_name : string
    ; selected_version : Version.t
    ; implementation : Implementation.t
    ; server_id : Id.Server.t
    ; enabled_features : string list
    ; extensions : Extension_capabilities.t option [@sexp.option]
    ; principal : Principal.t
    ; limits : Limits.t
    ; event_retention : Event_retention.t
    ; timing : Timing.t
    ; server_time : Timestamp.t
    }
  [@@deriving sexp]

  val create
    :  protocol_name:string
    -> selected_version:Version.t
    -> implementation:Implementation.t
    -> server_id:Id.Server.t
    -> enabled_features:string list
    -> extensions:Extension_capabilities.t option
    -> principal:Principal.t
    -> limits:Limits.t
    -> event_retention:Event_retention.t
    -> timing:Timing.t
    -> server_time:Timestamp.t
    -> (t, Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end
```

## invocation

[implementation](../../lib/agent_protocol/invocation.ml) · [interface](../../lib/agent_protocol/invocation.mli)

```ocaml
(** Versioned tool invocation records. These pure transitions do not grant
    authority: the actor must admit the caller and validate referenced work
    ownership before committing a resolution. Derived equality preserves exact
    JSON structure, including object field order, for immutable identity/results. *)

type origin =
  | Model
  | Moderator
  | Script
  | Delegated_agent
  | External_adapter
[@@deriving compare, equal, sexp]

type work =
  | Job of Id.Job.t
  | Subscription of Id.Subscription.t
[@@deriving compare, equal, sexp]

type tool_error =
  { code : string
  ; message : string
  ; retryable : bool
  ; details : Jsonaf.t
  }
[@@deriving equal, sexp]

type outcome =
  | Complete of Jsonaf.t
  | Pending of work * Jsonaf.t
  | Fail of tool_error
  | Cancelled of string
[@@deriving equal, sexp]

type context =
  { id : Id.Invocation.t
  ; session_id : Id.Session.t
  ; generation : int
  ; origin : origin
  ; provider_call_id : string option
  ; call_entry_id : History.Id.t option [@sexp.option]
    (** Host-only canonical call occurrence binding. Absent in legacy records;
        required by the actor's canonical publication service. Only model origin
        may carry this field. The ChatML context ABI is unchanged. *)
  ; parent_invocation : Id.Invocation.t option
  ; parent_job : Id.Job.t option
  ; tool_name : string
  ; implementation_revision : string
  ; capability_fingerprint : string
  ; input : Jsonaf.t
  ; created_at : Timestamp.t
  ; deadline : Timestamp.t option
  }
[@@deriving equal, sexp]

type status =
  | Admitted
  | Dispatching
  | Resolved of outcome
  | Published of outcome
[@@deriving equal, sexp]

(** Shared value codecs for developer-only invocation persistence. They do not
    encode a conversation context or grant transition authority. *)
val origin_to_json : origin -> Jsonaf.t

val origin_of_json : Jsonaf.t -> (origin, Error.t) result
val status_to_json : status -> Jsonaf.t
val status_of_json : Jsonaf.t -> (status, Error.t) result

type call_kind =
  | Function
  | Custom
[@@deriving sexp, equal]

type payload_fingerprint =
  { sha256 : string
  ; byte_length : int
  }
[@@deriving sexp, equal]

type preparation =
  | Passed
  | Invalid_input
  | Pre_tool_rejected
  | Pre_tool_failed
  | Session_ended
  (** Stopped before execution, potentially after rewriting the call. Original
        and final routing may differ; successful outcomes are forbidden. *)
[@@deriving equal, sexp]

(** Host-retained routing provenance. Fingerprints describe exact raw bytes;
    canonical_payload describes the separately redacted/displayed call. No extra
    plaintext arguments are retained. The final target is context.tool_name.
    [Passed] records completion of original-input/pre-tool preparation, not
    final-target authorization or successful execution. This is audit evidence,
    not authority or a claim that the handler executed. *)
type routing =
  { kind : call_kind
  ; original_name : string
  ; original_payload : payload_fingerprint
  ; final_payload : payload_fingerprint
  ; canonical_payload : payload_fingerprint option [@sexp.option]
  ; preparation : preparation
  }
[@@deriving equal, sexp]

(** Stable source identity, independent of process-local tool capability IDs. *)
type observer =
  { script_id : string
  ; source_sha256 : string
  }
[@@deriving equal, sexp]

type observation_status =
  | Awaiting
  | Observing
  | Observed
  | Observation_failed of string
[@@deriving equal, sexp]

(** Runtime actions requested by an observation handler. These are scheduling
    intents, separate from native tool outcomes and observer execution. *)
type follow_up =
  { request_turn : bool
  ; request_compaction : bool
  ; end_session : string option
  }
[@@deriving equal, sexp]

type follow_up_status =
  | Pending_follow_up of follow_up
  | Compaction_accepted_follow_up of follow_up
  | Applied_follow_up of follow_up
  | Discarded_follow_up of follow_up * string
[@@deriving equal, sexp]

type handler_intent =
  { follow_up : follow_up_status
  ; compaction_operation_id : Id.Operation.t option [@sexp.option]
  }
[@@deriving equal, sexp]

type observation =
  { observer : observer
  ; status : observation_status
  ; follow_up : follow_up_status option [@sexp.option]
    (** Codec 6. Present only after acknowledgement; retained after application. *)
  ; compaction_operation_id : Id.Operation.t option [@sexp.option]
    (** Codec 8. The compaction whose outcome controls the dependent turn.
        Retained after application/discard. Absent on legacy receipts. *)
  }
[@@deriving equal, sexp]

type t = private
  { context : context
  ; status : status
  ; output_entry_id : History.Id.t option [@sexp.option]
  ; routing : routing option [@sexp.option]
  ; publication_discarded : string option [@sexp.option]
    (** Durable reason that no provider result will be published. The recorded
        outcome is preserved. Present only on resolved model invocations; codec 4. *)
  ; observation : observation option [@sexp.option]
    (** Non-authorizing nested script/moderator observation intent, fixed at admission.
        Handling disposition is independent of the tool outcome; codec 5. *)
  ; parent_event : Id.Moderator_execution.t option [@sexp.option]
    (** Direct ordinary-event owner; exclusive with invocation/job parents. Codec 9.
        Requires matching moderator observation intent and actor admission. *)
  ; handler_intent : handler_intent option [@sexp.option]
    (** Codec10. Actions requested by the tool implementation, recorded atomically
        with its original outcome. Independent of post-tool observation intent. *)
  ; completion_contract : Completion_contract.t option [@sexp.option]
    (** Schema11. Immutable eventual-result policy captured from a standalone
        model tool at admission. Presence alone never requests a delivery. *)
  ; authoring_reference : Authoring_reference.t option [@sexp.option]
    (** Host-produced receipt bound to the successful disclosed output. Its
        existence alone does not mean documentation reached provider history. *)
  }
[@@deriving equal, sexp]

(** Routing, when present, is fixed at admission and uses JSON codec version 3.
    Legacy records without routing remain readable. *)
val create
  :  ?routing:routing
  -> ?observer:observer
  -> ?completion_contract:Completion_contract.t
  -> ?parent_event:Id.Moderator_execution.t
  -> context
  -> (t, Error.t) result

val validate : t -> (unit, Error.t) result

(** Attach actions to a locally resolved invocation before committing it. The
    actor transition permits first attachment only with Dispatching -> Resolved;
    this pure function grants no authority to schedule the action. *)
val record_handler_intent : t -> requests:follow_up -> (t, Error.t) result

val apply_handler_intent : t -> (t, Error.t) result
val accept_handler_compaction : t -> operation_id:Id.Operation.t -> (t, Error.t) result
val discard_handler_intent : t -> reason:string -> (t, Error.t) result
val dispatch : t -> (t, Error.t) result

(** Records one outcome for a dispatched invocation after checking its owner
    and generation. A duplicate resolution fails, even for identical output.
    Referenced job/subscription ownership requires actor service validation. *)
val resolve
  :  ?authoring_reference:Authoring_reference.t
  -> t
  -> session_id:Id.Session.t
  -> generation:int
  -> outcome
  -> (t, Error.t) result

(** Attach trusted query evidence while preparing a successful resolution for its
    first commit. This does not mutate persisted state: [validate_transition]
    rejects adding a receipt to an already committed resolution. An identical
    prepared annotation is idempotent; replacing it or annotating publication fails. *)
val record_authoring_reference : t -> Authoring_reference.t -> (t, Error.t) result

(** Host cancellation may resolve admitted or dispatched work. It never
    replaces an already recorded outcome and does not cancel a pending job. *)
val cancel : t -> reason:string -> (t, Error.t) result

(** Marks delivery of the initial result. Idempotent after publication; no
    provider-history insertion or external effects are performed here. *)
val publish : t -> (t, Error.t) result

(** Publish a canonically bound model invocation with a retained output receipt.
    Repeating the same occurrence is idempotent; another occurrence is rejected.
    The actor must validate and atomically append the actual history entry.
    Routing records use JSON codec 3. Without routing, bound records use codec 2
    and unbound legacy records retain codec 1. *)
val publish_with_history : t -> output_entry_id:History.Id.t -> (t, Error.t) result

(** Record removal/unavailability of the canonical call without changing the
    outcome or fabricating a provider output. The host must prove that the call
    is not retained. Idempotent for the same reason; cannot later publish. *)
val discard_publication : t -> reason:string -> (t, Error.t) result

(** Pure transitions: the host must exclusively claim and durably save Observing
    before running the observer. Only terminal invocation outcomes are eligible.
    Claim is not idempotent: after interruption an Observing receipt must fail,
    never replay potentially effectful handler execution. *)
val claim_observation : t -> (t, Error.t) result

(** The host must save this receipt atomically with the prospective moderator
    state/effects. Failure leaves the tool outcome intact. These functions do not
    run handlers, authorize callers or install an observation drain. *)
val complete_observation : ?follow_up:follow_up -> t -> (t, Error.t) result

(** Mark requested runtime actions durably accepted. The host must commit this
    receipt atomically with the scheduling/stop transition, after releasing live
    moderator ownership. It does not claim that an operation finished. Idempotent;
    never reruns the handler or changes its outcome. Pending actions survive
    interruption between observation acknowledgement and scheduling.
    This primitive does not install a dispatcher or infer actions from snapshots. *)
val apply_observation_follow_up : t -> (t, Error.t) result

(** Codec 8 intermediate receipt: compaction is durably scheduled, and the
    requested turn remains pending. Save atomically with compaction admission.
    Repeating acceptance is idempotent and must not start another compaction. *)
val accept_observation_compaction
  :  t
  -> operation_id:Id.Operation.t
  -> (t, Error.t) result

(** Terminally discard unaccepted actions, e.g. when a session is stopped.
    Preserve the request and native outcome; never rearm it on restart.
    Repeating the same reason is idempotent. Save with the stop transition. *)
val discard_observation_follow_up : t -> reason:string -> (t, Error.t) result

(** May also discard an Awaiting observation whose owner is no longer available.
    Repeating the same failure is idempotent; successful handling is immutable. *)
val fail_observation : t -> reason:string -> (t, Error.t) result

(** Checks a proposed durable replacement, including immutable context and
    outcome. New records must be admitted; transitions cannot skip dispatch
    except for host cancellation. *)
val validate_transition : previous:t option -> t -> (unit, Error.t) result

val outcome_to_json : outcome -> Jsonaf.t
val validate_outcome : outcome -> (unit, Error.t) result
val work_to_json : work -> Jsonaf.t
val work_of_json : Jsonaf.t -> (work, Error.t) result
val outcome_of_json : Jsonaf.t -> (outcome, Error.t) result
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result

(** Complete current durable record projection. Required-null option fields;
    independent of the public protocol's historical envelope variants. *)
module Storage : sig
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end
```

## job

[implementation](../../lib/agent_protocol/job.ml) · [interface](../../lib/agent_protocol/job.mli)

```ocaml
(** Durable background job state and control requests. *)

type kind =
  | Model_call
  | Nested_agent
  | Scheduled_event
  | Async_tool
  | Shell_process
  | Compaction
[@@deriving compare, equal, sexp]

(** A saved target invocation returned Pending backed by owned work. The parent
    waits without a worker, retaining its original deadline across restart.
    Job targets retain waiting-status JSON version 1 and legacy [job_id]
    S-expression encoding. Subscription targets use JSON version 2 with tagged
    [work]; their records must bind the actual creating parent job attempt. *)
type dependency =
  { invocation_id : Id.Invocation.t
  ; work : Invocation.work
  ; deadline : Timestamp.t
  ; completion_schema : Jsonaf.t option [@sexp.option]
    (** Captured from the admitted target definition, never supplied by a script. *)
  ; max_output_bytes : int
  ; max_output_depth : int
  }
[@@deriving equal, sexp]

type status =
  | Queued
  | Running
  | Waiting_permission of Id.Permission.t
  | Waiting_completion of dependency
  | Succeeded
  | Failed of Error.t
  | Cancelled
  | Interrupted of string
[@@deriving sexp]

type retry_policy =
  | Never
  | Safe_retry of
      { max_attempts : int
      ; backoff_ms : int
      }
  | Idempotent of
      { key : Idempotency_key.t
      ; max_attempts : int
      ; backoff_ms : int
      }
[@@deriving sexp]

(** Shared closed value codecs for owner-neutral job persistence. These encode
    only the named value; a developer job never acquires a Session ID by using
    them. Callers still validate the containing job and its authority. *)
val kind_to_json : kind -> Jsonaf.t

val kind_of_json : Jsonaf.t -> (kind, Error.t) result
val status_to_json : status -> Jsonaf.t
val status_of_json : Jsonaf.t -> (status, Error.t) result
val retry_to_json : retry_policy -> Jsonaf.t
val retry_of_json : Jsonaf.t -> (retry_policy, Error.t) result

type discard_reason = Authority_changed [@@deriving equal, sexp]

type delivery =
  | Not_required
  | Pending
  | Delivered of Timestamp.t
  | Discarded of
      { at : Timestamp.t
      ; reason : discard_reason
      }
[@@deriving equal, sexp]

type launch_owner =
  | Invocation of Id.Invocation.t
  | Moderator_event of Id.Moderator_execution.t
[@@deriving equal, sexp]

type launch =
  { owner : launch_owner
  ; parent_job : (Id.Job.t * int) option [@sexp.option]
  ; nested_depth : int
  ; moderator_source : Invocation.observer option [@sexp.option]
    (** Optional schema2 source captured under the creating moderator's live
        borrow. Absence never acquires the source of a subsequently loaded script. *)
  }
[@@deriving equal, sexp]

type t =
  { id : Id.Job.t
  ; session_id : Id.Session.t
  ; generation : int
  ; kind : kind
  ; payload : Jsonaf.t
  ; status : status
  ; retry_policy : retry_policy
  ; attempt : int
  ; created_at : Timestamp.t
  ; started_at : Timestamp.t option
  ; next_run_at : Timestamp.t option
  ; completed_at : Timestamp.t option
  ; result : Jsonaf.t option
  ; delivery : delivery
  ; launch : launch option [@sexp.option]
    (** Optional versioned launch provenance. Legacy jobs omit this field.
        Host admission binds the invocation/event owner and actual parent attempt;
        user scripts cannot choose their nesting depth. *)
  ; progress : Job_progress.t option [@sexp.option]
    (** Transient read projection only. Durable job records omit progress, and
        terminal results never depend on retaining these display updates. *)
  }
[@@deriving sexp]

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result

(** Validate artifact result ownership and discarded-delivery lifecycle, including
    values restored from non-JSON snapshots. Legacy result representations remain
    unchanged. *)
val validate_result : t -> (unit, Error.t) result

(** A discarded delivery preserves its terminal job exactly and cannot be revived.
    This checks incremental journal changes; decoding a retained snapshot does not
    require the previous pending record. *)
val validate_delivery_transition : previous:t option -> t -> (unit, Error.t) result

(** Read the inline completion or explicit artifact descriptor without loading
    any bytes. Validates exact artifact session/job/generation/attempt ownership. *)
val terminal_result : t -> (Stored_completion.t option, Error.t) result

(** Interpret terminal results at the completion/delivery boundary. Async_tool
    results must contain a valid Completion envelope matching their terminal
    status. Other kinds retain the legacy raw-success/error-status encoding;
    JSON that resembles an envelope is still ordinary model output. Nonterminal
    jobs return None, including queued retries retaining an earlier failure.
    Artifact results require a host loader; absence fails explicitly rather than
    treating a reference as the business result. This read neither changes delivery
    ownership nor executes work. *)
val terminal_completion
  :  ?load_artifact:(Job_artifact.t -> (Completion.t, Error.t) result)
  -> t
  -> (Completion.t option, Error.t) result

module List_request : sig
  type t =
    { session_id : Id.Session.t
    ; page : Page.Request.t
    ; status : string option
    ; kind : kind option
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Get_request : sig
  type t =
    { session_id : Id.Session.t
    ; job_id : Id.Job.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Cancel_request : sig
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; job_id : Id.Job.t
    ; expected_generation : int option [@sexp.option]
    ; expected_attempt : int option [@sexp.option]
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Cancel_result : sig
  type nonrec t =
    { job : t
    ; mutation : Mutation_result.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end
```

## job_artifact

[implementation](../../lib/agent_protocol/job_artifact.ml) · [interface](../../lib/agent_protocol/job_artifact.mli)

```ocaml
(** Versioned reference to a complete serialized terminal result. The reference
    binds its blob to one session, job generation and attempt; it is not authority
    to read a different session or bypass capability/disclosure checks. *)
type t = private
  { session_id : Id.Session.t
  ; job_id : Id.Job.t
  ; generation : int
  ; attempt : int
  ; blob : Blob.Metadata.t
  }
[@@deriving sexp]

val media_type : string

val create
  :  session_id:Id.Session.t
  -> job_id:Id.Job.t
  -> generation:int
  -> attempt:int
  -> blob:Blob.Metadata.t
  -> (t, Error.t) result

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
val allowed_use : t -> string
```

## job_progress

[implementation](../../lib/agent_protocol/job_progress.ml) · [interface](../../lib/agent_protocol/job_progress.mli)

```ocaml
(** Best-effort transient display state. It is not a terminal result, canonical
    history or a durable replay stream. Channels retain bounded text suffixes. *)
type channel =
  | Assistant
  | Reasoning
  | Stdout
  | Stderr
  | Activity
[@@deriving compare, equal, sexp]

type item =
  { channel : channel
  ; text : string
  ; truncated : bool
  }
[@@deriving sexp]

type t =
  { sequence : int
  ; channels : item list
  }
[@@deriving sexp]

val max_update_bytes : int
val max_channel_bytes : int
val max_updates : int
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
```

## job_result_reference

[implementation](../../lib/agent_protocol/job_result_reference.ml) · [interface](../../lib/agent_protocol/job_result_reference.mli)

```ocaml
(** Bounded reference to an exact retained terminal result, whether inline or
    artifact-backed. A reference does not grant read authority. Consumers use
    the owning session/job service, which rechecks current access and identity. *)
type t = private
  { session_id : Id.Session.t
  ; job_id : Id.Job.t
  ; generation : int
  ; attempt : int
  ; outcome : Stored_completion.outcome
  ; byte_length : int64
  ; sha256 : string
  ; artifact : Job_artifact.t option
  }
[@@deriving sexp]

val equal : t -> t -> bool
val validate : t -> (unit, Error.t) result
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
val of_job : Job.t -> (t, Error.t) result
val validate_job : t -> Job.t -> (unit, Error.t) result

(** Project metadata from an actual retained completion and exact occurrence.
    Validates artifact identity/length/digest; neither reads content nor grants
    authority. Unlike success-only run completion, every terminal outcome remains
    truthful in this reference. *)
val of_completion
  :  Stored_completion.t
  -> session_id:Id.Session.t
  -> job_id:Id.Job.t
  -> generation:int
  -> attempt:int
  -> (t, Error.t) result
```

## json_codec

[implementation](../../lib/agent_protocol/json_codec.ml) · [interface](../../lib/agent_protocol/json_codec.mli)

```ocaml
(** Strict JSON decoding helpers for stable protocol codecs. *)

type fields

(** [fields json] returns object fields and rejects duplicate names. *)
val fields : Jsonaf.t -> (fields, Error.t) result

(** [required fields name] returns required field [name]. *)
val required : fields -> string -> (Jsonaf.t, Error.t) result

(** [required_as fields name decode] decodes required field [name] with [decode]. *)
val required_as
  :  fields
  -> string
  -> (Jsonaf.t -> ('a, Error.t) result)
  -> ('a, Error.t) result

(** [optional fields name] returns optional field [name]. *)
val optional : fields -> string -> Jsonaf.t option

(** [optional_as fields name decode] decodes optional field [name] with [decode]. *)
val optional_as
  :  fields
  -> string
  -> (Jsonaf.t -> ('a, Error.t) result)
  -> ('a option, Error.t) result

(** [to_alist fields] returns fields in their decoded order. *)
val to_alist : fields -> (string * Jsonaf.t) list

(** [string json] decodes a JSON string. *)
val string : Jsonaf.t -> (string, Error.t) result

(** [bool json] decodes a JSON boolean. *)
val bool : Jsonaf.t -> (bool, Error.t) result

(** [list decode json] decodes a JSON array with [decode]. *)
val list : (Jsonaf.t -> ('a, Error.t) result) -> Jsonaf.t -> ('a list, Error.t) result

(** [bounded_int ~min ~max json] decodes an integer in [[min, max]]. *)
val bounded_int : min:int -> max:int -> Jsonaf.t -> (int, Error.t) result

(** [bounded_int64 ~min ~max json] decodes an integer in [[min, max]]. *)
val bounded_int64
  :  min:Core.Int64.t
  -> max:Core.Int64.t
  -> Jsonaf.t
  -> (Core.Int64.t, Error.t) result

(** [enum ~name values json] decodes a closed string enum. *)
val enum : name:string -> (string * 'a) list -> Jsonaf.t -> ('a, Error.t) result

(** [validate_required_features ~supported ~required] rejects unknown required features. *)
val validate_required_features
  :  supported:Core.String.Set.t
  -> required:string list
  -> (unit, Error.t) result

(** [validate_limits ~max_depth ~max_bytes json] enforces structural depth and
    encoded-size limits. *)
val validate_limits : max_depth:int -> max_bytes:int -> Jsonaf.t -> (unit, Error.t) result

(** [canonical json] recursively sorts object fields and rejects duplicates. *)
val canonical : Jsonaf.t -> (Jsonaf.t, Error.t) result

(** [canonical_string json] returns the stable encoding of [canonical json]. *)
val canonical_string : Jsonaf.t -> (string, Error.t) result
```

## method_result

[implementation](../../lib/agent_protocol/method_result.ml) · [interface](../../lib/agent_protocol/method_result.mli)

```ocaml
(** Typed successful results for every Protocol 1.0 method. *)

module Server_info : sig
  type t =
    { server_id : Id.Server.t
    ; implementation : Initialize.Implementation.t
    ; protocol_version : Version.t
    ; features : string list
    ; transports : string list
    ; limits : Initialize.Limits.t
    ; unsafe_development_auth : bool
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Session_mutation : sig
  type t =
    { session : Session.t
    ; mutation : Mutation_result.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module History_continue : sig
  type t =
    { session : Session.t
    ; mutation : Mutation_result.t
    ; continuation : History_edit.Continuation.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module History_edit : sig
  type t =
    { session : Session.t
    ; mutation : Mutation_result.t
    ; history_id : History.Id.t
    ; content_revision : History.Content_revision.t
    ; archived_revision : int64
    ; continuation : History_edit.Continuation.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Attach : sig
  type replay =
    | Current
    | Events of Event.Durable.t list
    | Snapshot of Snapshot.t
  [@@deriving sexp]

  type t =
    { attachment : Session.Attachment.t
    ; replay : replay
    ; latest_event_sequence : int64
    ; reclaim_token : string option
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Create : sig
  type t =
    { session : Session.t
    ; mutation : Mutation_result.t
    ; attachment : Attach.t option
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Send_message : sig
  type disposition =
    | Started
    | Deferred
  [@@deriving compare, equal, sexp]

  type t =
    { history_id : History.Id.t
    ; disposition : disposition
    ; operation_id : Id.Operation.t option
    ; mutation : Mutation_result.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Export : sig
  type t =
    { blob : Blob.Metadata.t
    ; session_revision : int64
    ; latest_event_sequence : int64
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Delete : sig
  type t =
    { session_id : Id.Session.t
    ; deleted_at : Timestamp.t
    ; archive : Blob.Metadata.t option
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

type t =
  | Provider_setup of Provider_operator.Setup_result.t
  | Provider_status of Provider_operator.Status_result.t
  | Provider_login_begin of Provider_operator.Flow_ref.t
  | Provider_login_challenge of Provider_operator.Private_challenge.t
  | Provider_login_cancel of Provider_operator.Flow_result.t
  | Provider_logout of Provider_operator.Logout_result.t
  | Provider_select of Provider_operator.Selection_result.t
  | Provider_configure_environment of Provider_operator.Configuration_result.t
  | Protocol_initialize of Initialize.Response.t
  | Command_receipt of Command_receipt.t
  | Protocol_ping of Ping.Response.t
  | Server_info of Server_info.t
  | Server_health of Health.Response.t
  | Prompt_list of Prompt.t Page.t
  | Prompt_get of Prompt.t
  | Prompt_dependencies_status of Prompt_dependency_selection.Status.t
  | Prompt_dependencies_update of Prompt_dependency_selection.Update_result.t
  | Workspace_list of Workspace.t Page.t
  | Workspace_get of Workspace.t
  | Blob_read of Blob.Chunk.t
  | Session_create of Create.t
  | Session_seed of Session_seed.Result.t
  | Session_list of Session_catalog.t Page.t
  | Session_search of Search_page.t
  | Session_search_navigate of Search_navigation.Response.t
  | Activity_list of Session_activity.t Page.t
  | Session_work of Session_work.t Page.t
  | Session_configuration_get of Session_configuration.t
  | Session_configuration_update of Session_configuration.t
  | Session_get of Snapshot.t
  | Session_inference_summary of Inference_query.Summary.t
  | Session_inference_observations of Inference_query.Response.t
  | Session_attach of Attach.t
  | Session_detach of Mutation_result.t
  | Session_renew_owner of Session.Owner_lease.t * Mutation_result.t
  | Session_start of Session_mutation.t
  | Project_create of Organization_group.Project.t
  | Project_get of Organization_group.Project.t
  | Project_list of Organization_group.Project.t Page.t
  | Project_update of Organization_group.Project.t
  | Project_delete of Organization_result.Project_deleted.t
  | Collection_create of Organization_group.Collection.t
  | Collection_get of Organization_group.Collection.t
  | Collection_list of Organization_group.Collection.t Page.t
  | Collection_update of Organization_group.Collection.t
  | Collection_delete of Organization_result.Collection_deleted.t
  | Session_update_metadata of Session_mutation.t
  | Session_update_organization of Session_mutation.t
  | Session_stop of Session_mutation.t
  | Session_cancel_operation of Session_mutation.t
  | Session_run_start of Run_receipt.t
  | Session_send_message of Send_message.t
  | Session_runs of Run_query.View.t Page.t
  | Session_run of Run_query.Outcome.t
  | Session_pending_inputs of Pending_query.View.t
  | Session_pending_input of Pending_query.Outcome.t
  | Session_cancel_pending_input of Pending_control.Result.t
  | Session_replace_pending_input of Pending_control.Result.t
  | Session_compact of Session_mutation.t
  | Session_edit_history of History_edit.t
  | Session_continue_history of History_continue.t
  | Session_delete_history of Session_mutation.t
  | Session_export of Export.t
  | Session_reset of Session_mutation.t
  | Session_rebuild of Session_mutation.t
  | Session_upgrade_prompt of Session_mutation.t
  | Session_delete of Delete.t
  | Session_restore of Session_lifecycle.Result.t
  | Session_resume of Session_lifecycle.Result.t
  | Permission_list of Permission.t Page.t
  | Permission_respond of Permission.Respond_result.t
  | Grant_list of Grant.t Page.t
  | Grant_revoke of Grant.Revoke_result.t
  | Audit_read of Audit.t Page.t
  | Job_list of Job.t Page.t
  | Job_get of Job.t
  | Job_cancel of Job.Cancel_result.t
  | Schedule_list of Schedule.t Page.t
  | Schedule_get of Schedule.t
  | Schedule_create of Schedule.Mutation_response.t
  | Schedule_cancel of Schedule.Mutation_response.t
  | Ingress_submit of Ingress.Acknowledgement.t
  | Authoring of (Authoring_wire.Response.t[@sexp.opaque])
[@@deriving sexp]

(** [method_name t] returns the request method associated with [t]. *)
val method_name : t -> string

(** [to_json t] encodes the method-specific result object.
    Private provider challenges require Public.Result authorized projection;
    generic internal encoding raises Invalid_argument without challenge content. *)
val to_json : t -> Jsonaf.t

(** [of_json ~method_ json] decodes the successful result for [method_]. *)
val of_json : method_:string -> Jsonaf.t -> (t, Error.t) result

(** [supported_methods] is the typed success-codec inventory, not a host
    capability or installed handler list. *)
val supported_methods : string list
```

## moderator_execution

[implementation](../../lib/agent_protocol/moderator_execution.ml) · [interface](../../lib/agent_protocol/moderator_execution.mli)

```ocaml
(** Durable execution receipts for ordinary moderator events. These are distinct
    from model operations, tool invocations and conversation deliveries. Records
    confer no authority; the actor owns source/checkpoint admission and execution. *)
type phase =
  | Session_start
  | Session_resume
  | Turn_start
  | Message_appended
  | Pre_tool_call
  | Post_tool_response
  | Turn_end
  | Internal_event
[@@deriving equal, sexp]

type job_attempt =
  { job_id : Id.Job.t
  ; attempt : int
  ; deadline : Timestamp.t option [@sexp.option]
  }
[@@deriving equal, sexp]

type context =
  { id : Id.Moderator_execution.t
  ; session_id : Id.Session.t
  ; generation : int
  ; source : Invocation.observer
  ; operation_id : Id.Operation.t option
  ; job : job_attempt option [@sexp.option]
    (** Background tool event provenance. Mutually exclusive with operation_id;
        only an actor-owned claimed job attempt may admit this context. *)
  ; phase : phase
  ; event : Jsonaf.t
    (** Captured engine event data. The actor validates its encoding and phase
        against the selected event; this pure codec only checks JSON bounds. *)
  ; checkpoint_sha256 : string
  ; created_at : Timestamp.t
  }
[@@deriving equal, sexp]

type status =
  | Running
  | Completed of string
  | Failed of Invocation.tool_error
  | Interrupted of string
[@@deriving equal, sexp]

type intent =
  | Pending
  | Waiting_compaction of Id.Operation.t
  | Applied
  | Discarded of string
[@@deriving equal, sexp]

(** Separate disposition for a consumed failed queue head. The original execution
    outcome remains unchanged. Commit this with the resulting checkpoint. *)
type retirement =
  { checkpoint_sha256 : string
  ; reason : string
  }
[@@deriving equal, sexp]

(** Cross-session provenance for a parent policy check of an already admitted
    child invocation. The host must validate the private delegation and actual
    child invocation; these identifiers grant no authority by themselves. *)
type delegation =
  { child_session_id : Id.Session.t
  ; child_generation : int
  ; child_invocation_id : Id.Invocation.t
  ; admission_sha256 : string
  }
[@@deriving equal, sexp]

module Decision : sig
  type t =
    | Approve
    | Reject of string
    | Rewrite_args of Jsonaf.t
    | Redirect of string * Jsonaf.t
  [@@deriving equal, sexp]

  val validate : t -> (unit, Error.t) result
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

type t = private
  { context : context
  ; status : status
  ; requests : Invocation.follow_up option
  ; intent : intent option
  ; compaction_operation_id : Id.Operation.t option
    (** Retained after application/discard for provenance and recovery checks. *)
  ; retirement : retirement option [@sexp.option]
  ; delegation : delegation option [@sexp.option]
  ; decision : Decision.t option [@sexp.option]
  }
[@@deriving equal, sexp]

val create : context -> (t, Error.t) result

(** Only pre-tool checks without parent operation/job ownership may carry this
    provenance. Completed checks require a decision in the same receipt as the
    committed parent checkpoint. Failed/interrupted checks carry no decision.
    JSON codec4; ordinary/job receipts keep their existing codec2/3 encoding. *)
val create_delegated : delegation:delegation -> context -> (t, Error.t) result

val validate : t -> (unit, Error.t) result

val complete
  :  ?decision:Decision.t
  -> t
  -> checkpoint_sha256:string
  -> requests:Invocation.follow_up
  -> (t, Error.t) result

val fail : t -> Invocation.tool_error -> (t, Error.t) result
val interrupt : t -> reason:string -> (t, Error.t) result
val retire : t -> checkpoint_sha256:string -> reason:string -> (t, Error.t) result

(** Atomically pair intent transitions with their actual actor scheduling or
    stop transition. Waiting_compaction retains the exact dependent operation. *)
val accept_compaction : t -> operation_id:Id.Operation.t -> (t, Error.t) result

val apply_intent : t -> (t, Error.t) result
val discard_intent : t -> reason:string -> (t, Error.t) result
val validate_transition : previous:t option -> t -> (unit, Error.t) result
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
```

## mutation_result

[implementation](../../lib/agent_protocol/mutation_result.ml) · [interface](../../lib/agent_protocol/mutation_result.mli)

```ocaml
(** Revision and durable-event position returned by accepted session mutations. *)

type t =
  { revision : int64
  ; latest_event_sequence : int64
  }
[@@deriving sexp]

(** [to_fields t] encodes fields for embedding in a result object. *)
val to_fields : t -> (string * Jsonaf.t) list

(** [of_fields fields] decodes nonnegative revision and event sequence fields. *)
val of_fields : Json_codec.fields -> (t, Error.t) result

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
```

## operation

[implementation](../../lib/agent_protocol/operation.ml) · [interface](../../lib/agent_protocol/operation.mli)

```ocaml
(** Foreground operation metadata shared by transports and clients. *)

type turn_start_reason =
  | User_submit
  | Moderator_request
  | Idle_followup
  | Recovery_retry
  | Administrative
[@@deriving compare, equal, sexp]

type kind =
  | Turn of turn_start_reason
  | Compaction
[@@deriving compare, equal, sexp]

type state =
  | Starting
  | Running
  | Cancelling
  | Completed
  | Failed of Error.t
  | Cancelled
  | Interrupted of
      { reason : string
      ; retryable : bool
      }
[@@deriving sexp]

type t =
  { id : Id.Operation.t
  ; generation : int
  ; kind : kind
  ; state : state
  ; started_at : Timestamp.t
  ; updated_at : Timestamp.t
  }
[@@deriving sexp]

(** [to_json t] encodes an operation summary. *)
val to_json : t -> Jsonaf.t

(** [of_json json] decodes and validates an operation summary. *)
val of_json : Jsonaf.t -> (t, Error.t) result
```

## organization_group

[implementation](../../lib/agent_protocol/organization_group.ml) · [interface](../../lib/agent_protocol/organization_group.mli)

```ocaml
(** Host-owned logical organization; never an execution workspace or authority grant.
    Names are nonempty UTF-8 without controls, bounded to 1024 bytes. *)
module Name : sig
  type t [@@deriving equal, sexp]

  val create : string -> (t, Error.t) result
  val to_string : t -> string
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module type S = sig
  type id [@@deriving equal, sexp]

  type t = private
    { id : id
    ; creator_principal_id : Id.Principal.t
    ; name : Name.t
    ; revision : int64
    ; created_at : Timestamp.t
    ; updated_at : Timestamp.t
    }
  [@@deriving equal, sexp]

  val create
    :  id:id
    -> creator_principal_id:Id.Principal.t
    -> name:Name.t
    -> revision:int64
    -> created_at:Timestamp.t
    -> updated_at:Timestamp.t
    -> (t, Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Project : S with type id = Id.Project.t
module Collection : S with type id = Id.Collection.t
```

## organization_request

[implementation](../../lib/agent_protocol/organization_request.ml) · [interface](../../lib/agent_protocol/organization_request.mli)

```ocaml
(** CRUD requests are host-qualified. The initialized connection host must match.
    Updates/deletes require a nonnegative expected revision and a bounded key.
    List order is created_at ascending, then typed ID ascending. *)
module Create : sig
  type t =
    { host_id : Id.Server.t
    ; name : Organization_group.Name.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module List : sig
  type t =
    { host_id : Id.Server.t
    ; creator_principal_id : Id.Principal.t option
    ; page : Page.Request.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module type S = sig
  type id [@@deriving sexp]

  module Get : sig
    type t =
      { host_id : Id.Server.t
      ; id : id
      }
    [@@deriving sexp]

    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) result
  end

  module Update : sig
    type t =
      { host_id : Id.Server.t
      ; id : id
      ; expected_revision : int64
      ; name : Organization_group.Name.t
      ; idempotency_key : Idempotency_key.t
      }
    [@@deriving sexp]

    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) result
  end

  module Delete : sig
    type t =
      { host_id : Id.Server.t
      ; id : id
      ; expected_revision : int64
      ; idempotency_key : Idempotency_key.t
      }
    [@@deriving sexp]

    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) result
  end
end

module Project : S with type id = Id.Project.t
module Collection : S with type id = Id.Collection.t
```

## organization_result

[implementation](../../lib/agent_protocol/organization_result.ml) · [interface](../../lib/agent_protocol/organization_result.mli)

```ocaml
(** Exact organization mutation results; deletion retains the ID forever. *)
module Project_deleted : sig
  type t = private
    { id : Id.Project.t
    ; revision : int64
    ; deleted_at : Timestamp.t
    }
  [@@deriving equal, sexp]

  val create
    :  id:Id.Project.t
    -> revision:int64
    -> deleted_at:Timestamp.t
    -> (t, Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Collection_deleted : sig
  type t = private
    { id : Id.Collection.t
    ; revision : int64
    ; deleted_at : Timestamp.t
    }
  [@@deriving equal, sexp]

  val create
    :  id:Id.Collection.t
    -> revision:int64
    -> deleted_at:Timestamp.t
    -> (t, Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

type t =
  | Project_created of Organization_group.Project.t
  | Project_updated of Organization_group.Project.t
  | Project_deleted of Project_deleted.t
  | Collection_created of Organization_group.Collection.t
  | Collection_updated of Organization_group.Collection.t
  | Collection_deleted of Collection_deleted.t
[@@deriving equal, sexp]

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
```

## page

[implementation](../../lib/agent_protocol/page.ml) · [interface](../../lib/agent_protocol/page.mli)

```ocaml
(** Opaque cursor pagination shared by list methods. *)

module Cursor : sig
  type t [@@deriving compare, equal, sexp]

  (** [of_string encoded] validates a nonempty opaque cursor. *)
  val of_string : string -> (t, Error.t) result

  (** [to_string t] returns the opaque cursor bytes. *)
  val to_string : t -> string

  (** [of_json json] decodes a cursor string. *)
  val of_json : Jsonaf.t -> (t, Error.t) result

  (** [to_json t] encodes a cursor string. *)
  val to_json : t -> Jsonaf.t
end

module Request : sig
  type t =
    { limit : int
    ; cursor : Cursor.t option
    }
  [@@deriving sexp]

  (** [create ~limit ?cursor ()] creates a page request with a positive limit. *)
  val create : limit:int -> ?cursor:Cursor.t -> unit -> (t, Error.t) result

  (** [to_fields t] encodes request fields for embedding in method parameters. *)
  val to_fields : t -> (string * Jsonaf.t) list

  (** [of_fields fields] decodes [limit] and optional [cursor]. *)
  val of_fields : Json_codec.fields -> (t, Error.t) result

  (** [to_json t] encodes a page request object. *)
  val to_json : t -> Jsonaf.t

  (** [of_json json] decodes a page request object. *)
  val of_json : Jsonaf.t -> (t, Error.t) result
end

type 'a t =
  { items : 'a list
  ; next_cursor : Cursor.t option
  }
[@@deriving sexp]

(** [to_json encode_item t] encodes a page response. *)
val to_json : ('a -> Jsonaf.t) -> 'a t -> Jsonaf.t

(** [of_json decode_item json] decodes a page response. *)
val of_json : (Jsonaf.t -> ('a, Error.t) result) -> Jsonaf.t -> ('a t, Error.t) result
```

## pending_control

[implementation](../../lib/agent_protocol/pending_control.ml) · [interface](../../lib/agent_protocol/pending_control.mli)

```ocaml
(** Controls for one still-pending occurrence. The authenticated submitting
    principal is captured by the host, never supplied in request parameters.
    Current writer/transcript authority and persisted submitting ownership must
    both permit the mutation. These requests grant neither provider interruption
    nor canonical-history deletion authority. *)
module Cancel_request : sig
  type t = private
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; expected_generation : int
    ; expected_pending_revision : Pending_input.Revision.t
    ; history_id : History.Id.t
    ; expected_content_revision : History.Content_revision.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val create
    :  session_id:Id.Session.t
    -> attachment_id:Id.Attachment.t
    -> expected_generation:int
    -> expected_pending_revision:Pending_input.Revision.t
    -> history_id:History.Id.t
    -> expected_content_revision:History.Content_revision.t
    -> idempotency_key:Idempotency_key.t
    -> (t, Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Replace_request : sig
  type t = private
    { target : Cancel_request.t
    ; text : string
    }
  [@@deriving sexp]

  (** Validates bounded UTF-8 text using the shared canonical edit contract.
      Empty text is valid. Host admission separately rejects non-plain targets. *)
  val create : target:Cancel_request.t -> text:string -> (t, Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Result : sig
  type t =
    { pending_revision : Pending_input.Revision.t
    ; outcome : Pending_query.Outcome.t
    ; mutation : Mutation_result.t
    }
  [@@deriving sexp]

  (** An adopted winner returns its actual public canonical occurrence; it is
      never cancelled or edited by this result. Unknown/expired outcomes remain
      unavailable and confer no retry permission. *)
  val to_json : t -> Jsonaf.t

  val of_json : Jsonaf.t -> (t, Error.t) result
end
```

## pending_input

[implementation](../../lib/agent_protocol/pending_input.ml) · [interface](../../lib/agent_protocol/pending_input.mli)

```ocaml
(** Pending input extends the existing durable deferred history queue. It grants
    no writer, scheduler or provider authority. All constructors and decoders
    validate generation, occurrence identity and temporal binding. *)
module Revision : sig
  type t [@@deriving compare, equal, sexp]

  val zero : t
  val of_int64 : int64 -> (t, Error.t) result
  val to_int64 : t -> int64
  val succ : t -> (t, Error.t) result
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Timing : sig
  type t =
    | Safe_boundary
    | After_current_operation
  [@@deriving equal, sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Terminal_proof : sig
  (** Bounded proof of the actual matching terminal root operation. Retains its
      typed identity/generation/outcome, never raw failure or interruption prose. *)
  type t [@@deriving equal, sexp]

  val of_operation : Operation.t -> (t, Error.t) result
  val operation_id : t -> Id.Operation.t
  val generation : t -> int
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Binding : sig
  type t = private
    | Safe_boundary
    | Await_idle
    | After_root of
        { operation_id : Id.Operation.t
        ; generation : int
        ; terminal : Terminal_proof.t option
        }
  [@@deriving equal, sexp]

  (** Validated payload-free safe binding for authored/legacy entries. Admission
      still validates generation and complete wrapper ownership independently. *)
  val safe_boundary : t

  (** Capture only an actual active Turn root. Compaction or no root waits for
      safe idle admission; the client cannot supply an inferred operation ID. *)
  val create
    :  Timing.t
    -> generation:int
    -> operation:Operation.t option
    -> (t, Error.t) result

  (** Only an exact matching terminal proof releases an after-root barrier.
      Unrelated terminal operations leave the binding unchanged. *)
  val release : t -> Terminal_proof.t -> (t, Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

type t [@@deriving equal, sexp]

val create
  :  entry:History.entry
  -> generation:int
  -> binding:Binding.t
  -> (t, Error.t) result

val entry : t -> History.entry
val history_id : t -> History.Id.t
val generation : t -> int
val binding : t -> Binding.t
val with_binding : t -> Binding.t -> (t, Error.t) result
val with_entry : t -> History.entry -> (t, Error.t) result
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
```

## pending_query

[implementation](../../lib/agent_protocol/pending_query.ml) · [interface](../../lib/agent_protocol/pending_query.mli)

```ocaml
(** Authorized nonactivating inspection of pending input. The host enforces
    current session/transcript visibility and page bounds before reading. Private
    stored ownership, custody and raw operation errors are never projected. *)
module Request : sig
  type t = private
    { session_id : Id.Session.t
    ; page : Page.Request.t
    }
  [@@deriving sexp]

  val create : session_id:Id.Session.t -> page:Page.Request.t -> (t, Error.t) result
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Lookup_request : sig
  type t =
    { session_id : Id.Session.t
    ; history_id : History.Id.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Retirement_reason : sig
  type t =
    | Source_reset
    | Source_replaced
    | Canonical_history_retired
  [@@deriving equal, sexp]
end

module Item : sig
  type t = private
    { history : Public_history.t
    ; generation : int
    ; binding : Pending_input.Binding.t
    }
  [@@deriving sexp]

  (** Projection of the known timing and an already authorized public history
      occurrence. Never grants permission to reconstruct canonical input. *)
  val create
    :  history:Public_history.t
    -> generation:int
    -> binding:Pending_input.Binding.t
    -> (t, Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Outcome : sig
  (** [Adopted.current=None] means the retained adoption is known but no current
      canonical occurrence is available. It never claims the input was rejected.
      [Unavailable] includes expired disposition evidence and unknown identity;
      it is never permission to replay the original submission. *)
  type t = private
    | Pending of Item.t
    | Adopted of
        { history_id : History.Id.t
        ; admitted_content_revision : History.Content_revision.t
        ; current : Public_history.t option
        }
    | Cancelled of History.Id.t
    | Retired of History.Id.t * Retirement_reason.t
    | Unavailable of History.Id.t
  [@@deriving sexp]

  val pending : Item.t -> t

  val adopted
    :  history_id:History.Id.t
    -> admitted_content_revision:History.Content_revision.t
    -> current:Public_history.t option
    -> (t, Error.t) result

  val cancelled : History.Id.t -> t
  val retired : History.Id.t -> reason:Retirement_reason.t -> t
  val unavailable : History.Id.t -> t
  val history_id : t -> History.Id.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module View : sig
  type t = private
    { pending_revision : Pending_input.Revision.t
    ; page : Item.t Page.t
    }
  [@@deriving sexp]

  val create
    :  pending_revision:Pending_input.Revision.t
    -> page:Item.t Page.t
    -> (t, Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end
```

## permission

[implementation](../../lib/agent_protocol/permission.ml) · [interface](../../lib/agent_protocol/permission.mli)

```ocaml
(** Durable tool-authorization requests and compare-and-set responses. *)

type state =
  | Pending
  | Approved
  | Denied
  | Expired
  | Cancelled
[@@deriving compare, equal, sexp]

type choice =
  | Approve_once
  | Approve_session
  | Approve_prefix
  | Durable_exact
  | Deny
[@@deriving compare, equal, sexp]

(** An operation-owned legacy request or a request from an actual persisted
    tool invocation. Invocation ownership does not require a model operation. *)
type owner =
  | Operation of Id.Operation.t
  | Invocation of Id.Invocation.t
[@@deriving equal, sexp]

type t =
  { id : Id.Permission.t
  ; session_id : Id.Session.t
  ; generation : int
  ; owner : owner
  ; call_id : string
  ; tool_name : string
  ; runtime_identity : string option
  ; invocation_display : string
  ; rationale : string option
  ; effects : string list
  ; choices : choice list
  ; created_at : Timestamp.t
  ; expires_at : Timestamp.t option
  ; state : state
  ; resolution : resolution option
  }
[@@deriving sexp]

and resolution =
  { choice : choice
  ; principal_id : Id.Principal.t option
  ; resolved_at : Timestamp.t
  ; reason : string option
  }
[@@deriving sexp]

module Resolution : sig
  type t = resolution [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

val to_json : t -> Jsonaf.t

(** Accept legacy [operation_id] or [invocation_id], exactly one. The S-expression
    reader also accepts the old [operation_id] record field. *)
val of_json : Jsonaf.t -> (t, Error.t) result

module List_request : sig
  type t =
    { session_id : Id.Session.t
    ; page : Page.Request.t
    ; state : state option
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Respond_request : sig
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; permission_id : Id.Permission.t
    ; permission_generation : int
    ; choice : choice
    ; reason : string option
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Respond_result : sig
  type nonrec t =
    { permission : t
    ; mutation : Mutation_result.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end
```

## ping

[implementation](../../lib/agent_protocol/ping.ml) · [interface](../../lib/agent_protocol/ping.mli)

```ocaml
(** Protocol liveness request and readiness response. *)

module Request : sig
  type t = { payload : Jsonaf.t option } [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Response : sig
  type t =
    { payload : Jsonaf.t option
    ; server_time : Timestamp.t
    ; ready : bool
    ; draining : bool
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end
```

## principal

[implementation](../../lib/agent_protocol/principal.ml) · [interface](../../lib/agent_protocol/principal.mli)

```ocaml
(** Authenticated identity and authorization summary. *)

type t =
  { id : Id.Principal.t
  ; authentication_kind : string
  ; scopes : Scope.Set.t
  ; attributes : (string * string) list
  }
[@@deriving sexp]

(** [create ~id ~authentication_kind ~scopes ~attributes] creates a principal.
    Authentication kinds use lowercase dotted identifiers and attribute keys
    must be unique and nonempty. *)
val create
  :  id:Id.Principal.t
  -> authentication_kind:string
  -> scopes:Scope.Set.t
  -> attributes:(string * string) list
  -> (t, Error.t) result

(** [has_scope t scope] reports whether [t] carries [scope]. *)
val has_scope : t -> Scope.t -> bool

(** [to_json t] encodes the transport-safe principal summary. *)
val to_json : t -> Jsonaf.t

(** [of_json json] decodes a principal summary. *)
val of_json : Jsonaf.t -> (t, Error.t) result
```

## projection_codec

[implementation](../../lib/agent_protocol/projection_codec.ml) · [interface](../../lib/agent_protocol/projection_codec.mli)

```ocaml
(** Shared admission boundary for protocol 2 transcript envelopes. *)
val limits : Document_schema.Limits.t

val validate : Jsonaf.t -> (unit, Error.t) result
val string_result : ('a, string) result -> ('a, Error.t) result
val optional : string -> 'a option -> ('a -> Jsonaf.t) -> (string * Jsonaf.t) list
```

## prompt

[implementation](../../lib/agent_protocol/prompt.ml) · [interface](../../lib/agent_protocol/prompt.mli)

```ocaml
(** Transport projections for configured ChatMD prompts. *)

type availability =
  | Available
  | Unavailable of { reason : string }
[@@deriving compare, equal, sexp]

type t =
  { id : Id.Prompt_definition.t
  ; name : string
  ; description : string option
  ; enabled : bool
  ; availability : availability
  ; current_revision : Id.Prompt_revision.t option
  ; allowed_workspaces : Id.Workspace_definition.t list
  ; permission_profile : string
  ; runtime_policy : string option
  }
[@@deriving sexp]

(** [to_json t] encodes a path-redacted prompt summary. *)
val to_json : t -> Jsonaf.t

(** [of_json json] decodes a prompt summary. *)
val of_json : Jsonaf.t -> (t, Error.t) result

module List_request : sig
  type t =
    { page : Page.Request.t
    ; enabled : bool option
    ; available : bool option
    }
  [@@deriving sexp]

  (** [to_json t] encodes prompt-list filters and pagination. *)
  val to_json : t -> Jsonaf.t

  (** [of_json json] decodes prompt-list filters and pagination. *)
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Get_request : sig
  type t = { prompt_id : Id.Prompt_definition.t } [@@deriving sexp]

  (** [to_json t] encodes a prompt lookup request. *)
  val to_json : t -> Jsonaf.t

  (** [of_json json] decodes a prompt lookup request. *)
  val of_json : Jsonaf.t -> (t, Error.t) result
end
```

## prompt_dependency_selection

[implementation](../../lib/agent_protocol/prompt_dependency_selection.ml) · [interface](../../lib/agent_protocol/prompt_dependency_selection.mli)

```ocaml
open! Core
module Error = Protocol_error

(** Bounded operator control of the host's installed dependency selection.
    These values contain no source authority, repository/pin overrides or native
    paths. Both methods require [configuration.admin]; construction grants none. *)
module Generation : sig
  type t [@@deriving compare, equal, sexp]

  (** Nonnegative generation of the actual persisted dependency-history index. *)
  val of_int : int -> (t, Error.t) Result.t

  val to_int : t -> int
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
  val to_json : t -> Jsonaf.t
end

module Status_request : sig
  (** The status method accepts exactly an empty JSON object. *)
  val of_json : Jsonaf.t -> (unit, Error.t) Result.t

  val to_json : unit -> Jsonaf.t
end

module Status : sig
  module History : sig
    type t =
      | Tracked of Generation.t
      | Missing
      | Invalid
    [@@deriving equal, sexp]
  end

  module Publication : sig
    type t =
      | Available
      | Busy
      | Uncertain
      | Closed
    [@@deriving equal, sexp]
  end

  type t =
    { history : History.t
    ; publication : Publication.t
    }
  [@@deriving equal, sexp]

  (** Last installed history and current admission state. Busy/Uncertain do not
      establish the generation currently on disk or authorize a new acquisition.
      A read neither repairs missing history nor retries publication. *)
  val to_json : t -> Jsonaf.t

  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end

module Update_request : sig
  type t [@@deriving equal, sexp]

  (** Admit 1..1024 distinct typed definition IDs, then canonicalize typed-ID
      ordering. Count admission precedes derived allocations. Unknown/disabled
      targets and expected-generation affinity remain actual host checks before
      acquisition. The bounded existing key identifies this exact mutation. *)
  val create
    :  definitions:Id.Prompt_definition.t list
    -> expected_generation:Generation.t
    -> idempotency_key:Idempotency_key.t
    -> (t, Error.t) Result.t

  val definitions : t -> Id.Prompt_definition.t list
  val expected_generation : t -> Generation.t
  val idempotency_key : t -> Idempotency_key.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end

module Update_result : sig
  module Target : sig
    type outcome =
      | Ready of Id.Prompt_revision.t
      | Unavailable of { reason : string }
    [@@deriving equal, sexp]

    type t =
      { definition_id : Id.Prompt_definition.t
      ; outcome : outcome
      }
    [@@deriving equal, sexp]
  end

  type t [@@deriving equal, sexp]

  (** Exactly one installed index transition: next generation is previous + 1,
      checked without overflow. Admit 1..1024 distinct target IDs and canonicalize
      their order. Unavailable reasons must be nonempty, NUL-free and <=4096 UTF-8
      bytes; the host supplies redacted diagnostics, never raw credentials/paths.
      Publication success does not claim all targets' fresh sources were usable:
      unavailable targets retain their previous successful dependency history. *)
  val create
    :  previous_generation:Generation.t
    -> generation:Generation.t
    -> targets:Target.t list
    -> (t, Error.t) Result.t

  val previous_generation : t -> Generation.t
  val generation : t -> Generation.t
  val targets : t -> Target.t list

  (** Validate the exact target set and previous generation against the request;
      clients reject a successful response that describes another mutation. *)
  val matches_request : t -> Update_request.t -> bool

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end
```

## protocol_error

[implementation](../../lib/agent_protocol/protocol_error.ml) · [interface](../../lib/agent_protocol/protocol_error.mli)

```ocaml
(** Stable errors exposed by the Ochat agent protocol. *)

open Core

type code =
  | Invalid_request
  | Method_not_found
  | Unauthenticated
  | Permission_denied
  | Session_not_found
  | Prompt_not_found
  | Workspace_not_found
  | Organization_not_found
  | Invalid_state
  | Already_resolved
  | Resource_limit
  | Workspace_unavailable
  | Prompt_unavailable
  | Manifest_unauthorized
  | Approval_required
  | Idempotency_conflict
  | Snapshot_required
  | Operation_not_found
  | Persistence_error
  | Interrupted
  | Conflict
  | Pending_input_conflict
  | Internal_error
  | Incompatible_protocol
  | Cursor_expired
  | Blob_unavailable
  | Lease_stale
  | Configuration_invalid
  | Store_locked
  | Store_schema_too_new
  | Migration_required
  | Journal_corrupt
  | Server_shutting_down
  | Command_queue_full
[@@deriving compare, equal, sexp]

type t =
  { code : code
  ; message : string
  ; retryable : bool
  ; data : Jsonaf.t
  }
[@@deriving sexp]

(** [create code ~message ~retryable ?data ()] creates a transport-safe error. *)
val create : code -> message:string -> retryable:bool -> ?data:Jsonaf.t -> unit -> t

(** [invalid_request ?data message] creates a non-retryable invalid-request error. *)
val invalid_request : ?data:Jsonaf.t -> string -> t

(** [code_to_string code] returns the stable wire representation of [code]. *)
val code_to_string : code -> string

(** [code_of_string value] parses a stable error code. *)
val code_of_string : string -> (code, t) result

(** [to_json t] encodes [t] using the stable protocol field names. *)
val to_json : t -> Jsonaf.t

(** [of_json json] decodes a protocol error and rejects duplicate required fields. *)
val of_json : Jsonaf.t -> (t, t) result
```

## provider_operator

[implementation](../../lib/agent_protocol/provider_operator.ml) · [interface](../../lib/agent_protocol/provider_operator.mli)

```ocaml
open! Core

type protocol_error = Protocol_error.t

(** Nonsecret provider administration DTOs. No filesystem paths, credential bytes,
    arbitrary environment names, or inferred login authority occur in requests. *)
module Profile_id : sig
  type t [@@deriving compare, equal, sexp]

  val of_string : string -> (t, protocol_error) result
  val to_string : t -> string
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, protocol_error) result
end

module Source_id : module type of Profile_id
module Flow_id : module type of Profile_id
module Revision : module type of Profile_id

module Limits : sig
  type t

  val create
    :  max_flows:int
    -> max_profiles:int
    -> max_flow_seconds:int
    -> (t, protocol_error) result

  val default : t
  val max_flows : t -> int
  val max_profiles : t -> int
  val max_flow_seconds : t -> int
end

module Operation : sig
  type t =
    | Setup
    | Status
    | Login
    | Challenge
    | Cancel
    | Logout
    | Select
    | Configure_environment
  [@@deriving equal, sexp]
end

module Error : sig
  type t =
    | Denied
    | Missing_profile
    | Invalid_request
    | Busy
    | Closed
    | Flow_expired
    | Flow_interrupted
    | Challenge_unavailable
    | Submission_uncertain
    | Network
    | Account_denied
    | Model_denied
    | Store_unavailable
    | Unsupported
  [@@deriving equal, sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, protocol_error) result
end

module Login_mode : sig
  type t =
    | Browser
    | Device
  [@@deriving equal, sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, protocol_error) result
end

module Flow_ref : sig
  type t =
    { server_id : Id.Server.t
    ; profile : Profile_id.t
    ; flow_id : Flow_id.t
    ; expires_at : Timestamp.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, protocol_error) result
end

module Flow_result : sig
  type phase =
    | Pending
    | Completed
    | Failed of Error.t
    | Cancelled
    | Interrupted
    | Expired
  [@@deriving equal, sexp]

  type t =
    { flow : Flow_ref.t
    ; phase : phase
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, protocol_error) result
end

module Private_challenge : sig
  (** Secret-bearing owner-only live challenge. Debug sexp is always redacted;
      sexp decoding is forbidden. Never store in receipts, events, audit or status. *)
  type t [@@deriving sexp]

  val browser : authorization_uri:Uri.t -> (t, protocol_error) result
  val device : verification_uri:Uri.t -> user_code:string -> (t, protocol_error) result
  val with_browser_uri : t -> f:(Uri.t -> 'a) -> 'a option

  val with_device_prompt
    :  t
    -> f:(verification_uri:Uri.t -> user_code:string -> 'a)
    -> 'a option

  module Authorized_transport : sig
    (** Only after current scope AND exact flow ownership/expiry authorization. *)
    val to_json : t -> Jsonaf.t

    val of_json : Jsonaf.t -> (t, protocol_error) result
  end
end

module Setup_request : sig
  type t = { idempotency_key : Idempotency_key.t } [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, protocol_error) result
end

(** Setup revision is the nonsecret host registry incarnation, not the default
    selection revision. Selection CAS comes only from Status_result.selection. *)
module Setup_result : sig
  type t =
    { server_id : Id.Server.t
    ; revision : Revision.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, protocol_error) result
end

module Status_request : sig
  type t = { profile : Profile_id.t option } [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, protocol_error) result
end

module Selection_result : sig
  type t =
    { profile : Profile_id.t
    ; revision : Revision.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, protocol_error) result
end

module Status_result : sig
  type availability =
    | Missing
    | Configured
    | Disabled
    | Renewal_required
    | Renewal_uncertain
    | Secret_unavailable
    | Store_unavailable
  [@@deriving equal, sexp]

  type failure =
    | Network
    | Account_denied
    | Model_denied
    | Submission_uncertain
  [@@deriving equal, sexp]

  type profile =
    { profile : Profile_id.t
    ; account : string option
    ; availability : availability
    ; last_failure : failure option
    ; auth_epoch : int64 option
    ; credential_revision : Revision.t option
    }
  [@@deriving sexp]

  type t =
    { server_id : Id.Server.t
    ; setup_required : bool
    ; profiles : profile list
    ; flows : Flow_result.t list
    ; selection : Selection_result.t option
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, protocol_error) result
end

module Login_request : sig
  type t =
    { profile : Profile_id.t
    ; mode : Login_mode.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, protocol_error) result
end

module Challenge_request : sig
  type t = { flow : Flow_ref.t } [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, protocol_error) result
end

module Cancel_request : sig
  type t =
    { flow : Flow_ref.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, protocol_error) result
end

module Logout_request : sig
  type t =
    { profile : Profile_id.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, protocol_error) result
end

module Logout_result : sig
  type drain =
    | Drained
    | Pending
  [@@deriving equal, sexp]

  type t =
    { profile : Profile_id.t
    ; auth_epoch : int64
    ; drain : drain
    ; cleanup_pending : bool
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, protocol_error) result
end

module Select_request : sig
  type t =
    { profile : Profile_id.t
    ; expected_revision : Revision.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, protocol_error) result
end

module Environment_request : sig
  type t =
    { profile : Profile_id.t
    ; source : Source_id.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, protocol_error) result
end

module Configuration_result : sig
  type t =
    { profile : Profile_id.t
    ; auth_epoch : int64
    ; revision : Revision.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, protocol_error) result
end
```

## public

[implementation](../../lib/agent_protocol/public.ml) · [interface](../../lib/agent_protocol/public.mli)

```ocaml
(** Explicit public read boundary. Private snapshot/history/event/result types
    also serve durable storage and must not be changed to redact a client view. *)
module History = Public_history

module Snapshot = Public_snapshot
module Durable = Public_durable_event
module Result = Public_result
```

## public_durable_event

[implementation](../../lib/agent_protocol/public_durable_event.ml) · [interface](../../lib/agent_protocol/public_durable_event.mli)

```ocaml
(** Typed public durable events. Hidden entries preserve sequence slots without
    exposing content. Internal events and their persistence codecs stay intact. *)
module Shared_payload : sig
  type t [@@deriving sexp_of]

  (** Exhaustive admission of the shared, non-history payload alternatives.
      Rejects all history variants and the untyped internal moderator overlay.
      Native children pass the same domain codec admission as received payloads.
      Optional replacement/status fields are projected separately. *)
  val of_internal : Event.Durable.Payload.t -> (t, Error.t) result

  val value : t -> Event.Durable.Payload.t
end

type overlay =
  { effective_history : Public_history.Window.t option
  ; halted : bool
  ; halt_reason : string option
  }
[@@deriving sexp_of]

type payload =
  | History_message_deferred of Public_history.t
  | History_appended of Public_history.t list
  | History_replaced of Public_history.Window.t
  | Moderator_overlay_changed of overlay
  | Shared of Shared_payload.t
[@@deriving sexp_of]

type body =
  | Full of payload
  | Filtered of payload
  | Hidden
[@@deriving sexp_of]

type t = private
  { session_id : Id.Session.t
  ; sequence : int64
  ; revision : int64
  ; timestamp : Timestamp.t
  ; kind : Event.Durable.kind
  ; body : body
  ; extension_status : Extension_status.t list option
  ; replacement_snapshot : Public_snapshot.t option
  }
[@@deriving sexp_of]

(** Copies only the event envelope. Checks kind, counters, payload session
    ownership and replacement anchors. Extension summaries are unique, validated
    and cannot name a future generation of their session update. Hidden events
    must contain neither status nor snapshot extras. *)
val of_internal_envelope
  :  Event.Durable.t
  -> body:body
  -> extension_status:Extension_status.t list option
  -> replacement_snapshot:Public_snapshot.t option
  -> (t, Error.t) result

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
val to_notification : t -> Envelope.t
```

## public_history

[implementation](../../lib/agent_protocol/public_history.ml) · [interface](../../lib/agent_protocol/public_history.mli)

```ocaml
(** Read-only history projections. Public views never authorize canonical input
    or a history edit. Full payloads retain their immutable neutral evidence;
    visible and redacted bodies cannot be converted back into canonical entries. *)

module Visible : sig
  type part =
    | Text of string
    | Refusal of string
    | Image of
        { uri : string
        ; detail : string History_entry.Payload.Presence.t
        }
    | Redacted_part of { kind : string }
  [@@deriving equal, sexp_of]

  type t = private
    | Message of
        { form : History_entry.Payload.Semantic.message_form
        ; role : History_entry.Payload.Role.t
        ; content : part list
        ; phase : string History_entry.Payload.Presence.t
        }
    | Reasoning of { readable_summary : string list }
  [@@deriving equal, sexp_of]

  (** Whitelist known readable fields. Arbitrary raw content, annotations,
      logprobs and provider metadata are never copied. Unknown message parts keep
      only their position and structural kind. Other semantic families return
      [None], requiring explicit redaction. *)
  val of_semantic : History_entry.Payload.Semantic.t -> t option

  val header : t -> Transcript.Header.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Redaction : sig
  type t = private { disclosed_header : Transcript.Header.t option }
  [@@deriving equal, sexp_of]

  (** A structural header may be disclosed independently of content. *)
  val create : disclosed_header:Transcript.Header.t option -> t
end

type body =
  | Full of History_entry.Payload.t
  | Visible of Visible.t
  | Redacted of Redaction.t
[@@deriving sexp_of]

type t = private
  { id : History.Id.t
  ; content_revision : History.Content_revision.t
  ; provenance : History.provenance
  ; body : body
  }
[@@deriving sexp_of]

val full
  :  ?content_revision:History.Content_revision.t
  -> History_entry.t
  -> provenance:History.provenance
  -> (t, Error.t) result

val visible
  :  ?content_revision:History.Content_revision.t
  -> History.Id.t
  -> provenance:History.provenance
  -> Visible.t
  -> (t, Error.t) result

val redacted
  :  ?content_revision:History.Content_revision.t
  -> History.Id.t
  -> provenance:History.provenance
  -> Redaction.t
  -> (t, Error.t) result

val header : t -> Transcript.Header.t option
val full_payload : t -> History_entry.Payload.t option

(** Exact full-payload JSON comparison retains field order and numeric spelling. *)
val equal : t -> t -> bool

(** Reject repeated host IDs within a public history sequence. *)
val validate_unique_ids : t list -> (unit, Error.t) result

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result

module Window : sig
  type entry = t

  type t =
    { entries : entry list
    ; previous_cursor : Page.Cursor.t option
    ; next_cursor : Page.Cursor.t option
    ; reached_start : bool
    ; reached_end : bool
    ; structurally_complete : bool
    }
  [@@deriving sexp_of]

  (** Checks complete envelope bounds and unique host identities. *)
  val validate : t -> (unit, Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end
```

## public_result

[implementation](../../lib/agent_protocol/public_result.ml) · [interface](../../lib/agent_protocol/public_result.mli)

```ocaml
(** Public success values. Only get/create/attach replace private inline history
    containers. The internal Method_result codec remains the durable receipt
    codec; projecting a result never changes a cached internal success. *)
module Non_history : sig
  type t [@@deriving sexp_of]

  (** Exhaustive method whitelist excluding get/create/attach. This excludes
      inline snapshot/history containers, not authority or disclosure: an export
      result can still reference an artifact containing history. *)
  val of_internal : Method_result.t -> (t, Error.t) result

  val value : t -> Method_result.t
end

module Attach : sig
  type replay =
    | Current
    | Events of Public_durable_event.t list
    | Snapshot of Public_snapshot.t
  [@@deriving sexp_of]

  type t =
    { attachment : Session.Attachment.t
    ; replay : replay
    ; latest_event_sequence : int64
    ; reclaim_token : string option
    }
  [@@deriving sexp_of]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Create : sig
  type t =
    { session : Session.t
    ; mutation : Mutation_result.t
    ; attachment : Attach.t option
    }
  [@@deriving sexp_of]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

type t =
  | Private_provider_challenge of Provider_operator.Private_challenge.t
  | Session_get of Public_snapshot.t
  | Session_attach of Attach.t
  | Session_create of Create.t
  | Non_history of Non_history.t
[@@deriving sexp_of]

val method_name : t -> string
val to_json : t -> Jsonaf.t
val of_json : method_:string -> Jsonaf.t -> (t, Error.t) result

(** Checks complete response bounds and the same container and child invariants
    as wire admission. Validation retains the original result unchanged. *)
val validate : t -> (unit, Error.t) result
```

## public_snapshot

[implementation](../../lib/agent_protocol/public_snapshot.ml) · [interface](../../lib/agent_protocol/public_snapshot.mli)

```ocaml
(** Validated client read state, separate from the persisted internal snapshot.
    These fields and their codecs do not change session storage version 1. *)
module Fields : sig
  type t =
    { session : Session.t
    ; lifecycle : Session_lifecycle.Observation.t option [@sexp.option]
    ; canonical_history : Public_history.Window.t
    ; archived_revisions : int64 list
    ; effective_history : Public_history.Window.t option
    ; deferred_entries : Public_history.t list
    ; permissions : Permission.t list
    ; grants : Grant.t list
    ; jobs : Job.t list
    ; extension_status : Extension_status.t list
    ; schedules : Schedule.t list
    ; active_tool_calls : Activity.Tool.summary list
      (** All currently running foreground tool calls, unique by activity key. *)
    ; active_agent_calls : Activity.Tool.summary list
      (** Exact classified subset of [active_tool_calls], including shell scripts.
          Nonempty activity requires the snapshot's active foreground operation. *)
    ; halted : bool
    ; halt_reason : string option
    ; failure : Error.t option
    ; revision : int64
    ; latest_event_sequence : int64
    }
  [@@deriving sexp_of]
end

type t [@@deriving sexp_of]

(** Checks positions against the session summary, ownership and counter bounds,
    and extension generations. Fields remain immutable after admission. *)
val create : Fields.t -> (t, Error.t) result

val fields : t -> Fields.t
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
```

## run

[implementation](../../lib/agent_protocol/run.ml) · [interface](../../lib/agent_protocol/run.mli)

```ocaml
(** Durable host run lifecycle. Session liveness and work execution are separate;
    successful finish does not stop a session or terminate unrelated jobs. *)
module Mode : sig
  type t =
    | Single_turn
    | Workflow
  [@@deriving compare, equal, sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Terminal : sig
  type t =
    | Completed of Run_result_reference.t option
    | Failed of Error.code option
    | Cancelled
    | Limited
    | Interrupted
  [@@deriving equal, sexp]

  val validate : t -> (unit, Error.t) result
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Lifecycle : sig
  type t =
    | Admitted
    | Active
    | Waiting of Run_wake.t
    | Terminal of Terminal.t
  [@@deriving equal, sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

type t = private
  { id : Id.Run.t
  ; session : Session_ref.t
  ; principal_id : Id.Principal.t
  ; source : Run_source.t
  ; mode : Mode.t
  ; lifecycle : Lifecycle.t
  ; revision : int64
  ; owned_work : Run_work.t list
  ; relinquished_work : Run_work.t list
  ; terminal_work : Run_work.Terminal.t list
  ; created_at : Timestamp.t
  ; updated_at : Timestamp.t
  }
[@@deriving equal, sexp]

(** Finite record/ownership bounds; duplicates and contradictory ownership fail.
    Decoders and sexp admission share constructor validation. *)
val create
  :  id:Id.Run.t
  -> session:Session_ref.t
  -> principal_id:Id.Principal.t
  -> source:Run_source.t
  -> mode:Mode.t
  -> lifecycle:Lifecycle.t
  -> revision:int64
  -> owned_work:Run_work.t list
  -> relinquished_work:Run_work.t list
  -> terminal_work:Run_work.Terminal.t list
  -> created_at:Timestamp.t
  -> updated_at:Timestamp.t
  -> (t, Error.t) result

val validate : t -> (unit, Error.t) result
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result

(** Immutable identity/source/mode and terminal receipts; revision advances once.
    Prior terminal work/disposition evidence cannot disappear or change, including
    when an underlying job advances to a later attempt. *)
val validate_transition : previous:t -> t -> (unit, Error.t) result
```

## run_action

[implementation](../../lib/agent_protocol/run_action.ml) · [interface](../../lib/agent_protocol/run_action.mli)

```ocaml
(** Revisioned authored decision, collected transactionally with the actual
    moderator checkpoint. Reading/constructing an action conveys no authority. *)
type t =
  | Continue
  | Wait of Run_wake.t
  | Finish of
      { terminal : Run.Terminal.t
      ; relinquish : Run_work.t list
        (** Pending independent work transferred to retained session ownership.
              Actor denies active callback/permission/action obligations and
              already-recorded adverse outcomes. Evidence remains immutable. *)
      }
[@@deriving equal, sexp]

val validate : t -> (unit, Error.t) result
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result

(** No action is neutral; identical requests coalesce. Incompatible surviving
    actions reject before checkpoint persistence. Task.catch discarded actions
    do not participate. *)
val combine : t option -> t option -> (t option, Error.t) result
```

## run_limits

[implementation](../../lib/agent_protocol/run_limits.ml) · [interface](../../lib/agent_protocol/run_limits.mli)

```ocaml
(** Hard protocol safety ceilings, independent of lower actor/runtime quotas.
    4096 occurrences matches the existing bounded retained-work observation
    ceiling; records/results cannot retain an unbounded second work graph.
    Document bytes count encoded JSON UTF-8 bytes; depth counts JSON containers. *)
val max_occurrences : int

val max_document_bytes : int
val max_depth : int
val check_count : int -> (unit, Error.t) result
val list : (Jsonaf.t -> ('a, Error.t) result) -> Jsonaf.t -> ('a list, Error.t) result
```

## run_query

[implementation](../../lib/agent_protocol/run_query.ml) · [interface](../../lib/agent_protocol/run_query.mli)

```ocaml
(** Current-authority, nonactivating shared run inspection. Full work and receipt
    views require transcript/security scopes plus session visibility and either
    the original run principal or actual session administrator authority. *)
module Request : sig
  type t = private
    { session : Session_ref.t
    ; page : Page.Request.t
    }

  val create : session:Session_ref.t -> page:Page.Request.t -> (t, Error.t) result
  val sexp_of_t : t -> Sexplib0.Sexp.t
  val t_of_sexp : Sexplib0.Sexp.t -> t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Lookup_request : sig
  type t =
    { session : Session_ref.t
    ; run_id : Id.Run.t
    }

  val sexp_of_t : t -> Sexplib0.Sexp.t
  val t_of_sexp : Sexplib0.Sexp.t -> t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module View : sig
  type t = private
    { run : Run.t
    ; result_references : Run_result_reference.t list
    ; pending_action : Run_action.t option
    ; admission_receipt : Run_receipt.t option
    ; terminal_receipt : Run_receipt.t option
    ; session_revision : int64
    ; event_sequence : int64
    }

  (** Correlates every receipt/action with immutable run/source identity. A
      completed terminal result reference remains evidence when its content has
      expired; lookup never rewrites completion to a missing-result failure. *)
  val create
    :  run:Run.t
    -> result_references:Run_result_reference.t list
    -> pending_action:Run_action.t option
    -> admission_receipt:Run_receipt.t option
    -> terminal_receipt:Run_receipt.t option
    -> session_revision:int64
    -> event_sequence:int64
    -> (t, Error.t) result

  val sexp_of_t : t -> Sexplib0.Sexp.t
  val t_of_sexp : Sexplib0.Sexp.t -> t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Outcome : sig
  (** Unknown/expired or foreign-principal evidence is unavailable, never a claim
      that the host terminated a run. Session visibility failures remain errors. *)
  type t = private
    | Available of View.t
    | Unavailable of Id.Run.t

  val available : View.t -> t
  val unavailable : Id.Run.t -> t
  val sexp_of_t : t -> Sexplib0.Sexp.t
  val t_of_sexp : Sexplib0.Sexp.t -> t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end
```

## run_receipt

[implementation](../../lib/agent_protocol/run_receipt.ml) · [interface](../../lib/agent_protocol/run_receipt.mli)

```ocaml
(** Immutable local admission/action receipt. A receipt is evidence, never current
    execution authority. Lost transport replies are reconciled without execution. *)
module Kind : sig
  type t =
    | Admission
    | Action
    | Terminal
  [@@deriving compare, equal, sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

type t = private
  { run_id : Id.Run.t
  ; principal_id : Id.Principal.t
  ; source : Run_source.t
  ; key : Idempotency_key.t
  ; request_sha256 : string
  ; kind : Kind.t
  ; run_revision : int64
  ; session_revision : int64
  ; committed_at : Timestamp.t
  }
[@@deriving equal, sexp]

val create
  :  run_id:Id.Run.t
  -> principal_id:Id.Principal.t
  -> source:Run_source.t
  -> key:Idempotency_key.t
  -> request_sha256:string
  -> kind:Kind.t
  -> run_revision:int64
  -> session_revision:int64
  -> committed_at:Timestamp.t
  -> (t, Error.t) result

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
```

## run_result_reference

[implementation](../../lib/agent_protocol/run_result_reference.ml) · [interface](../../lib/agent_protocol/run_result_reference.mli)

```ocaml
(** Authorized bounded references, never retained plaintext tool/provider output.
    Read uses owning service/current transcript projection and rechecks authority. *)
type t = private
  | Job of Job_result_reference.t
  | Operation of
      { operation_id : Id.Operation.t
      ; generation : int
      ; history_ids : History.Id.t list
      ; revision : int64
      }
[@@deriving equal, sexp]

val validate : t -> (unit, Error.t) result

(** Wraps an admitted exact job outcome reference without changing its outcome. *)
val of_job_result : Job_result_reference.t -> (t, Error.t) result

val of_job : Job.t -> (t, Error.t) result

val operation
  :  operation_id:Id.Operation.t
  -> generation:int
  -> history_ids:History.Id.t list
  -> revision:int64
  -> (t, Error.t) result

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
```

## run_source

[implementation](../../lib/agent_protocol/run_source.ml) · [interface](../../lib/agent_protocol/run_source.mli)

```ocaml
(** Captured actor installation identity, independent of checkpoint revisions.
    Epoch is monotone within a session and changes on replacement/removal/reset.
    A matching runtime restore does not grant or extend source authority. *)
type t = private
  { observer : Invocation.observer
  ; generation : int
  ; installation_epoch : int64
  }
[@@deriving equal, sexp]

val create
  :  observer:Invocation.observer
  -> generation:int
  -> installation_epoch:int64
  -> (t, Error.t) result

val validate : t -> (unit, Error.t) result
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
```

## run_start

[implementation](../../lib/agent_protocol/run_start.ml) · [interface](../../lib/agent_protocol/run_start.mli)

```ocaml
(** Explicit host-authorized run start; no caller-supplied source/run identity.
    The actor captures current compiled source and current authorization. *)
module Input : sig
  type t =
    | User_submission of Session.Message_content.t
    | Authored_start
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

type t = private
  { session_id : Id.Session.t
  ; attachment_id : Id.Attachment.t
  ; generation : int
  ; expected_revision : int64
  ; mode : Run.Mode.t
  ; input : Input.t
  ; key : Idempotency_key.t
  }
[@@deriving sexp]

(** Both modes accept [Authored_start]; the actor still requires a genuine
    unconsumed source-bound startup. Single_turn does not recreate a consumed
    startup or authorize caller-supplied source identity. *)
val create
  :  session_id:Id.Session.t
  -> attachment_id:Id.Attachment.t
  -> generation:int
  -> expected_revision:int64
  -> mode:Run.Mode.t
  -> input:Input.t
  -> key:Idempotency_key.t
  -> (t, Error.t) result

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
```

## run_wake

[implementation](../../lib/agent_protocol/run_wake.ml) · [interface](../../lib/agent_protocol/run_wake.mli)

```ocaml
(** Exact host-issued delivered occurrence. A whole schedule/subscription ID is
    insufficient: the actor validates source/install/generation and actual owner
    against its bound timer count, job attempt or subscription delivery/epoch. *)
module Occurrence : sig
  type t =
    | Job_completion of
        { job_id : Id.Job.t
        ; attempt : int
        }
    | Delivered_timer of
        { schedule_id : Id.Schedule.t
        ; delivery_count : int
        ; creator : Job.launch_owner
        ; subscription : (Id.Subscription.t * int) option
        }
    | Subscription_delivery of
        { subscription_id : Id.Subscription.t
        ; epoch : int
        ; delivery_id : Id.Delivery.t
        ; creator : Job.launch_owner
        }
  [@@deriving equal, sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

type t = private
  { run_id : Id.Run.t
  ; source : Run_source.t
  ; occurrence : Occurrence.t
  }
[@@deriving equal, sexp]

val validate : t -> (unit, Error.t) result

val create
  :  run_id:Id.Run.t
  -> source:Run_source.t
  -> occurrence:Occurrence.t
  -> (t, Error.t) result

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
```

## run_work

[implementation](../../lib/agent_protocol/run_work.ml) · [interface](../../lib/agent_protocol/run_work.mli)

```ocaml
(** Exact retained owned occurrence. This identity does not describe its outcome;
    the actor consults the actual work owner before wait/finish/custody transfer. *)
module Key : sig
  type t =
    | Operation of Id.Operation.t
    | Retained of Session_work.Key.t
  [@@deriving compare, equal, sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

type t = private
  { key : Key.t
  ; generation : int
  }
[@@deriving compare, equal, sexp]

include Core.Comparator.S with type t := t

val validate : t -> (unit, Error.t) result
val create : key:Key.t -> generation:int -> (t, Error.t) result
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result

module Terminal : sig
  type outcome =
    | Succeeded
    | Failed
    | Cancelled
    | Limited
    | Interrupted
    | Unconfirmed
  [@@deriving compare, equal, sexp]

  type work = t

  (* Immutable evidence captured from the actual owner before a later retry or
      relinquishment could replace its live status. Never rewritten to success. *)
  type t = private
    { work : work
    ; outcome : outcome
    ; revision : int64
    }
  [@@deriving equal, sexp]

  val validate : t -> (unit, Error.t) result
  val create : work:work -> outcome:outcome -> revision:int64 -> (t, Error.t) result
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end
```

## schedule

[implementation](../../lib/agent_protocol/schedule.ml) · [interface](../../lib/agent_protocol/schedule.mli)

```ocaml
(** Durable one-shot ChatML event schedules. *)

type misfire =
  | Deliver_once_immediately
  | Skip_if_expired
  | Fail
[@@deriving compare, equal, sexp]

type status =
  | Scheduled
  | Delivering
  | Delivered
  | Cancelled
  | Failed of Error.t
[@@deriving sexp]

type due =
  | At of Timestamp.t
  | After_ms of int
[@@deriving sexp]

(** Host-captured moderator authority and optional one-time subscription epoch
    binding. A legacy schedule without this record grants no script authority. *)
type ownership =
  { source : Invocation.observer
  ; creator : Job.launch_owner
  ; subscription : (Id.Subscription.t * int) option [@sexp.option]
  }
[@@deriving equal, sexp]

type t =
  { id : Id.Schedule.t
  ; session_id : Id.Session.t
  ; generation : int
  ; payload : Jsonaf.t
  ; created_at : Timestamp.t
  ; next_due_at : Timestamp.t
  ; misfire : misfire
  ; status : status
  ; delivery_count : int
  ; last_delivery_at : Timestamp.t option
  ; ownership : ownership option [@sexp.option]
    (** Owned schedules use a version-2 JSON envelope. Legacy records retain
        their original flat encoding; old readers reject the owned envelope. *)
  ; delivery_cancellation : string option [@sexp.option]
    (** Immutable cancellation of an enqueued callback, separate from the retained
        Delivered status/count/time. Present only on owned Delivered records with
        one delivery. These records require JSON envelope version 3; prior records
        keep their existing encoding and cannot silently lose a cancellation. *)
  }
[@@deriving sexp]

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result

(** Validate owned records and their lifecycle without changing the legacy
    unowned schedule contract. Binding is allowed once while still scheduled;
    source, creator, identity, payload and due time stay immutable. An enqueued
    owned callback may acquire one immutable delivery cancellation without changing
    the schedule's delivered status, count, timestamp or payload. *)
val validate : t -> (unit, Error.t) result

val validate_transition : previous:t option -> t -> (unit, Error.t) result

module List_request : sig
  type t =
    { session_id : Id.Session.t
    ; page : Page.Request.t
    ; status : string option
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Get_request : sig
  type t =
    { session_id : Id.Session.t
    ; schedule_id : Id.Schedule.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Create_request : sig
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; payload : Jsonaf.t
    ; due : due
    ; misfire : misfire
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Cancel_request : sig
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; schedule_id : Id.Schedule.t
    ; expected_generation : int option [@sexp.option]
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Mutation_response : sig
  type nonrec t =
    { schedule : t
    ; mutation : Mutation_result.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

(** Complete current durable record projection. Required-null option fields;
    independent of the public protocol's historical envelope variants. *)
module Storage : sig
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end
```

## scope

[implementation](../../lib/agent_protocol/scope.ml) · [interface](../../lib/agent_protocol/scope.mli)

```ocaml
(** Authorization scopes carried by authenticated principals. *)

type t =
  | List_prompts
  | List_workspaces
  | Create_sessions
  | View_session_transcript
  | Send_messages
  | Own_sessions
  | Answer_approvals
  | View_security_state
  | Manage_grants
  | Read_audit
  | Stop_sessions
  | Delete_sessions
  | Administer_configuration
  | Diagnostics
  | Submit_ingress
  | Provider_view
  | Provider_manage
  | Provider_select
  | View_organization
  | Manage_organization
[@@deriving compare, equal, sexp]

include Core.Comparable.S with type t := t

(** [to_string t] returns the stable lowercase dotted wire name. *)
val to_string : t -> string

(** [of_string encoded] parses a stable authorization scope. *)
val of_string : string -> (t, Error.t) result

(** [to_json t] encodes one scope as a JSON string. *)
val to_json : t -> Jsonaf.t

(** [of_json json] decodes one scope. *)
val of_json : Jsonaf.t -> (t, Error.t) result

(** [set_to_json scopes] encodes scopes in stable sorted order. *)
val set_to_json : Set.t -> Jsonaf.t

(** [set_of_json json] decodes a scope array and rejects duplicates. *)
val set_of_json : Jsonaf.t -> (Set.t, Error.t) result
```

## search_hit

[implementation](../../lib/agent_protocol/search_hit.ml) · [interface](../../lib/agent_protocol/search_hit.mli)

```ocaml
(** A disclosed occurrence in current canonical history; identity is host-owned.
    Revisions are observations, not authorization or proof the hit still exists.
    The snippet contains only readable user/assistant text. Part index is the
    zero-based public message-content position, including non-text parts. *)
type t [@@deriving sexp_of]

val create
  :  session:Session_ref.t
  -> generation:int
  -> session_revision:int64
  -> history_id:History.Id.t
  -> content_revision:History.Content_revision.t
  -> part_index:int
  -> snippet:Search_snippet.t
  -> (t, Error.t) result

val session : t -> Session_ref.t
val generation : t -> int
val session_revision : t -> int64
val history_id : t -> History.Id.t
val content_revision : t -> History.Content_revision.t
val part_index : t -> int
val snippet : t -> Search_snippet.t
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
```

## search_navigation

[implementation](../../lib/agent_protocol/search_navigation.ml) · [interface](../../lib/agent_protocol/search_navigation.mli)

```ocaml
(** Navigation is a fresh authorized lookup, never authority conveyed by a hit.
    The supplied snippet is ignored. The query reapplies current catalog filters;
    its pagination cursor is not part of navigation. *)
module Request : sig
  type t

  val create : query:Search_query.t -> hit:Search_hit.t -> (t, Error.t) result
  val query : t -> Search_query.t
  val hit : t -> Search_hit.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
  val sexp_of_t : t -> Sexplib.Sexp.t
  val t_of_sexp : Sexplib.Sexp.t -> t
end

(** Plain text from one readable canonical entry. Text is at most 2048 UTF-8
    bytes; [truncated] explicitly records omitted text. No raw provider data,
    reasoning, image URI or tool payload is represented. *)
module Entry : sig
  type t

  val create
    :  history_id:History.Id.t
    -> content_revision:History.Content_revision.t
    -> text:string
    -> truncated:bool
    -> (t, Error.t) result

  val history_id : t -> History.Id.t
  val content_revision : t -> History.Content_revision.t
  val text : t -> string
  val truncated : t -> bool
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Response : sig
  type t = private
    | Current of
        { hit : Search_hit.t
        ; context : Entry.t list
        }
    | Changed of History.Content_revision.t
    | Unavailable

  (** Up to five chronological canonical positions around the target. Excluded
      positions are omitted, not replaced by more distant entries. The context
      includes the target and has unique IDs; its revision agrees with [hit]. *)
  val current : hit:Search_hit.t -> context:Entry.t list -> (t, Error.t) result

  val changed : History.Content_revision.t -> t
  val unavailable : t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
  val sexp_of_t : t -> Sexplib.Sexp.t
  val t_of_sexp : Sexplib.Sexp.t -> t
end
```

## search_page

[implementation](../../lib/agent_protocol/search_page.ml) · [interface](../../lib/agent_protocol/search_page.mli)

```ocaml
(** Bounded traversal progress, not a relevance-ranked or exact-count result.
    A partial page must carry a continuation and make observable scan progress;
    it may contain zero hits. Only a complete traversal sets reached_end. *)
type t [@@deriving sexp]

val create
  :  hits:Search_hit.t list
  -> next_cursor:Page.Cursor.t option
  -> reached_end:bool
  -> scanned_entries:int
  -> scanned_sessions:int
  -> (t, Error.t) result

val hits : t -> Search_hit.t list
val next_cursor : t -> Page.Cursor.t option
val reached_end : t -> bool
val scanned_entries : t -> int
val scanned_sessions : t -> int
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
```

## search_query

[implementation](../../lib/agent_protocol/search_query.ml) · [interface](../../lib/agent_protocol/search_query.mli)

```ocaml
(** Conversation search reuses catalog selectors. Page limit counts hits (1–100),
    while scan_limit bounds examined canonical entries (1–512), including misses.
    Ordering is Created_at/Ascending with the catalog's stable session-ID ties,
    then canonical entry order. A zero-hit partial page can have a continuation.
    Query JSON is bounded to 64 KiB with at most 128 label filters.
    Cursors belong to search and cannot be reused for catalog listing. *)
type t [@@deriving sexp]

val create
  :  server_id:Id.Server.t
  -> term:Search_term.t
  -> catalog:Session.List_request.t
  -> scan_limit:int
  -> (t, Error.t) result

val server_id : t -> Id.Server.t
val term : t -> Search_term.t
val catalog : t -> Session.List_request.t
val scan_limit : t -> int
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
```

## search_snippet

[implementation](../../lib/agent_protocol/search_snippet.ml) · [interface](../../lib/agent_protocol/search_snippet.mli)

```ocaml
(** Plain text with one highlighted literal occurrence. Offsets and lengths count
    UTF-8 bytes, relative to [text], and both ends are Unicode scalar boundaries.
    At most 2048 bytes; no HTML or terminal rendering interpretation is implied.
    Truncation flags describe omitted source text, never an incomplete search. *)
type t [@@deriving equal, sexp_of]

val create
  :  text:string
  -> highlight_start:int
  -> highlight_length:int
  -> truncated_before:bool
  -> truncated_after:bool
  -> (t, Error.t) result

val text : t -> string
val highlight_start : t -> int
val highlight_length : t -> int
val truncated_before : t -> bool
val truncated_after : t -> bool
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
```

## search_term

[implementation](../../lib/agent_protocol/search_term.ml) · [interface](../../lib/agent_protocol/search_term.mli)

```ocaml
(** A nonempty UTF-8 literal of at most 256 bytes. ASCII letters match without
    regard to case; every other UTF-8 byte matches exactly. No normalization,
    regular expressions, tokenization or locale-sensitive folding. *)
type t [@@deriving equal, sexp_of]

val create : string -> (t, Error.t) result
val text : t -> string
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
```

## session

[implementation](../../lib/agent_protocol/session.ml) · [interface](../../lib/agent_protocol/session.mli)

```ocaml
(** Session specifications, lifecycle projections, attachments, and core requests. *)

type execution_host =
  | Daemon
  | Embedded
[@@deriving compare, equal, sexp]

type stop_mode =
  | Graceful
  | Cancel
[@@deriving compare, equal, sexp]

type liveness =
  | Detached
  | Owner_bound of
      { disconnect_grace_ms : int
      ; stop_mode : stop_mode
      }
  | Process_bound
[@@deriving compare, equal, sexp]

type persistence =
  | Durable
  | Transient
[@@deriving compare, equal, sexp]

type desired_state =
  | Running
  | Stopped
[@@deriving compare, equal, sexp]

type observed_state =
  | Stopped
  | Queued_for_slot
  | Starting
  | Recovering
  | Idle
  | Running_turn of Id.Operation.t
  | Compacting of Id.Operation.t
  | Waiting_for_permission of Id.Permission.t
  | Stopping
  | Failed of Error.t
[@@deriving sexp]

type attachment_mode =
  | Owner_read_write
  | Read_write
  | Read_only
[@@deriving compare, equal, sexp]

(** Stable lifecycle codecs used by snapshots and durable event payloads. *)
val desired_state_to_string : desired_state -> string

val desired_state_of_json : Jsonaf.t -> (desired_state, Error.t) result
val observed_state_to_json : observed_state -> Jsonaf.t
val observed_state_of_json : Jsonaf.t -> (observed_state, Error.t) result

module Prompt_ref : sig
  type t =
    | Catalog of Id.Prompt_definition.t
    | Local_path of string
    | Generated of Id.Prompt_revision.t
    (** Pinned generated definition, identified in session summaries. This is not
        an execution grant; ordinary session.create cannot admit this reference.
        Generated creation requires the scoped delegation service. *)
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Workspace_request : sig
  type t =
    | Configured of Id.Workspace_definition.t
    | Current
    | Local_path of string
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Spec : sig
  type t =
    { execution_host : execution_host
    ; prompt : Prompt_ref.t
    ; workspace : Workspace_request.t
    ; liveness : liveness
    ; persistence : persistence
    ; permission_profile : string option
    ; start_immediately : bool
    ; display_name : string option
    ; labels : (string * string) list
    }
  [@@deriving sexp]

  (** [create ...] validates host/liveness/persistence combinations and metadata. *)
  val create
    :  execution_host:execution_host
    -> prompt:Prompt_ref.t
    -> workspace:Workspace_request.t
    -> liveness:liveness
    -> persistence:persistence
    -> ?permission_profile:string
    -> start_immediately:bool
    -> ?display_name:string
    -> labels:(string * string) list
    -> unit
    -> (t, Error.t) result

  val to_json : t -> Jsonaf.t

  (** Structural decoding retains historical metadata strings; authored creation
      additionally uses [create], including at Create_request decoding. *)
  val of_json : Jsonaf.t -> (t, Error.t) result
end

(** Optional status-visible aggregate. This local sexp codec preserves presence;
    value decoding uses the same bounded validated safe summary JSON codec. It is
    not a generic deserializer for provider/private Presence payloads. *)
module Inference_summary : sig
  type t = Inference_query.Summary.t History_entry.Payload.Presence.t

  val sexp_of_t : t -> Core.Sexp.t
  val t_of_sexp : Core.Sexp.t -> t
end

type t =
  { id : Id.Session.t
  ; creator : Id.Principal.t option
  ; created_at : Timestamp.t
  ; updated_at : Timestamp.t
  ; generation : int
  ; spec : Spec.t
  ; desired_state : desired_state
  ; observed_state : observed_state
  ; prompt_revision : Id.Prompt_revision.t option
  ; workspace_instance : Id.Workspace_instance.t option
  ; active_operation : Operation.t option
  ; revision : int64
  ; metadata_revision : int64 [@sexp.default 0L]
  ; organization : Session_organization.Values.t
        [@sexp.default Session_organization.Values.empty]
    (** Nonnegative organization counter. Streaming changes advance [revision]
        while leaving this counter intact. Legacy summaries default to zero. *)
  ; latest_event_sequence : int64
  ; inference_summary : Inference_summary.t
        [@sexp.default History_entry.Payload.Presence.Absent]
  }
[@@deriving sexp]

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result

module Owner_lease : sig
  type t =
    { generation : int64
    ; expires_at : Timestamp.t
    ; disconnect_grace_until : Timestamp.t option
    ; principal_id : Id.Principal.t option [@sexp.option]
    ; reclaim_token_sha256 : string option [@sexp.option]
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Attachment : sig
  type t =
    { id : Id.Attachment.t
    ; session_id : Id.Session.t
    ; mode : attachment_mode
    ; owner_lease : Owner_lease.t option
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Create_request : sig
  type t =
    { spec : Spec.t
    ; requested_mode : attachment_mode option
    ; subscribe : bool
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module List_request : sig
  type t =
    { page : Page.Request.t
    ; desired_state : desired_state option
    ; prompt_id : Id.Prompt_definition.t option
    ; workspace_id : Id.Workspace_definition.t option
    ; owner_principal_id : Id.Principal.t option
    ; organization : Session_organization.Query.t
    ; labels : (string * string) list
    ; sort : Session_catalog_query.Sort.t
    ; archive : Session_catalog_query.Archive_filter.t
    ; creator_principal_id : Id.Principal.t option
    ; active_owner_principal_id : Id.Principal.t option
    }
  [@@deriving sexp]

  (** Validate native selectors and sort label keys without a JSON round trip. *)
  val normalize : t -> (t, Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Get_request : sig
  type t =
    { session_id : Id.Session.t
    ; history : History.Window_request.t option
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Attach_request : sig
  type t =
    { session_id : Id.Session.t
    ; requested_mode : attachment_mode
    ; subscribe : bool
    ; after_sequence : int64 option
    ; reclaim_token : string option
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Detach_request : sig
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Renew_owner_request : sig
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; lease_generation : int64
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Start_request : sig
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; queue_if_limited : bool
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Stop_request : sig
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; mode : stop_mode
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Cancel_operation_request : sig
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; operation_id : Id.Operation.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Message_content : sig
  type kind =
    | Plain_text
    | Chatmd
  [@@deriving compare, equal, sexp]

  type t =
    { kind : kind
    ; text : string
    ; attachments : Blob.Input.t list
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Send_message_request : sig
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; content : Message_content.t
    ; timing : Pending_input.Timing.t [@sexp.default Pending_input.Timing.Safe_boundary]
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Compact_request : sig
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; expected_revision : int64 option
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Delete_history_request : sig
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; history_id : History.Id.t
    ; expected_revision : int64
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

(** An absent attachment requests retained access under current principal
    visibility and method scopes. A supplied attachment must remain current.
    Encoding omits absence; explicit JSON null is invalid. No execution admission. *)
module Export_request : sig
  type format =
    | Chatmd
    | Json
  [@@deriving compare, equal, sexp]

  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t option
    ; format : format
    ; revision : int64 option
    ; history : History.Window_request.t option
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Reset_request : sig
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; expected_revision : int64
    ; keep_history : bool
    ; keep_tasks : bool
    ; keep_cache : bool
    ; keep_workspace : bool
    ; keep_grants : bool
    ; keep_labels : bool
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Rebuild_request : sig
  type prompt_choice =
    | Pinned
    | Current_catalog
  [@@deriving compare, equal, sexp]

  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; expected_revision : int64
    ; prompt_choice : prompt_choice
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Upgrade_prompt_request : sig
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; expected_revision : int64
    ; target_revision : Id.Prompt_revision.t
    ; allow_migration : bool
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Delete_request : sig
  type policy =
    | Archive
    | Remove
  [@@deriving compare, equal, sexp]

  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; expected_revision : int64
    ; policy : policy
    ; confirmation : string
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end
```

## session_activity

[implementation](../../lib/agent_protocol/session_activity.ml) · [interface](../../lib/agent_protocol/session_activity.mli)

```ocaml
(** Payload-free attention observed from existing session/work owners. Reading is
    not acknowledgement; unresolved rows never disappear because a client read them. *)
module Reason : sig
  type t =
    | Approval
    | Input_required
    | Failure
    | Completion_pending
  [@@deriving compare, equal, sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Attention : sig
  type entity =
    | Permission of Id.Permission.t
    | Operation of Id.Operation.t
    | Work of Session_work.Key.t
    | Session
  [@@deriving compare, equal, sexp]

  type t = private
    { entity : entity
    ; reason : Reason.t
    ; unresolved : bool
    ; expired : bool
    }
  [@@deriving sexp]

  val create
    :  entity:entity
    -> reason:Reason.t
    -> unresolved:bool
    -> expired:bool
    -> (t, Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
  val compare_key : t -> t -> int
end

module Transient : sig
  (** Unloaded/restarted snapshots have no durable active-call/progress authority. *)
  type t =
    | Unavailable
    | Live of
        { tool_calls : int
        ; agent_calls : int
        }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

type t = private
  { summary : Session_activity_summary.t
  ; attention : Attention.t list
  ; work_count : int
  ; transient : Transient.t
  ; usage : Inference_query.Summary.t
  }
[@@deriving sexp]

val create
  :  summary:Session_activity_summary.t
  -> attention:Attention.t list
  -> work_count:int
  -> transient:Transient.t
  -> usage:Inference_query.Summary.t
  -> (t, Error.t) result

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
```

## session_activity_summary

[implementation](../../lib/agent_protocol/session_activity_summary.ml) · [interface](../../lib/agent_protocol/session_activity_summary.mli)

```ocaml
(** Safe command-center session projection. Explicitly excludes Session.Spec,
    raw errors, prompt/workspace configuration, tool arguments/results and
    operation interruption text. Metadata is intentionally visible. *)
module Observed : sig
  type t =
    | Stopped
    | Queued_for_slot
    | Starting
    | Recovering
    | Idle
    | Running_turn
    | Compacting
    | Waiting_for_permission
    | Stopping
    | Failed
  [@@deriving compare, equal, sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Operation : sig
  (** Exact foreground lifecycle without raw failure/interruption payloads. *)
  module Status : sig
    type t =
      | Starting
      | Running
      | Cancelling
      | Completed
      | Failed
      | Cancelled
      | Interrupted
    [@@deriving compare, equal, sexp]

    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) result
  end

  type t = private
    { id : Id.Operation.t
    ; generation : int
    ; kind : Operation.kind
    ; status : Status.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

type t = private
  { session : Session_ref.t
  ; display_name : string option
  ; labels : (string * string) list
  ; creator : Id.Principal.t option
  ; created_at : Timestamp.t
  ; updated_at : Timestamp.t
  ; generation : int
  ; revision : int64
  ; metadata_revision : int64
  ; latest_event_sequence : int64
  ; execution_host : Session.execution_host
  ; liveness : Session.liveness
  ; persistence : Session.persistence
  ; desired_state : Session.desired_state
  ; observed : Observed.t
  ; archived : bool
  ; effective_organization : Session_organization.Values.t
  ; active_owner_principal_id : Id.Principal.t option
  ; active_operation : Operation.t option
  }
[@@deriving sexp]

(** Copies only named safe fields from the already-authorized checked catalog.
    Validates revisions/generation and metadata structural invariants. *)
val of_catalog : Session_catalog.t -> server_id:Id.Server.t -> (t, Error.t) result

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
```

## session_catalog

[implementation](../../lib/agent_protocol/session_catalog.ml) · [interface](../../lib/agent_protocol/session_catalog.mli)

```ocaml
(** Catalog projection: archive state is registry-owned, and active ownership
    is evaluated at query time. Neither field is persisted in Session.t. Missing
    legacy wire fields decode to [false]/[None], never to creator ownership. *)
type t =
  { session : Session.t
  ; active_owner_principal_id : Id.Principal.t option
  ; archived : bool
  ; lifecycle_revision : Session_lifecycle.Revision.t
  ; admission : Session_lifecycle.Result.Admission.t
  ; effective_organization : Session_organization.Values.t
  }
[@@deriving sexp]

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
```

## session_catalog_query

[implementation](../../lib/agent_protocol/session_catalog_query.ml) · [interface](../../lib/agent_protocol/session_catalog_query.mli)

```ocaml
(** Catalog ordering uses bytewise display-name comparison and ascending typed
    session-id tie breaking, independently of the primary direction. *)
module Sort : sig
  type field =
    | Created_at
    | Updated_at
    | Display_name
  [@@deriving compare, equal, sexp]

  type direction =
    | Ascending
    | Descending
  [@@deriving compare, equal, sexp]

  type t =
    { field : field
    ; direction : direction
    }
  [@@deriving compare, equal, sexp]

  val default : t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Archive_filter : sig
  type t =
    | Active
    | Archived
    | All
  [@@deriving compare, equal, sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end
```

## session_configuration

[implementation](../../lib/agent_protocol/session_configuration.ml) · [interface](../../lib/agent_protocol/session_configuration.mli)

```ocaml
open! Core
module Error = Protocol_error

(** Nonsecret configuration intent. Patches preserve unknown private target data;
    constructing a patch grants no profile/account or tool authority. *)
module Patch : sig
  type t [@@deriving sexp]

  (** [settings] are explicit overrides, including absent/null/value. Duplicate
      names, empty patches and oversized data reject at construction/decoding. *)
  val create
    :  ?model:string
    -> ?profile:string
    -> settings:Inference.Request.Setting.t list
    -> unit
    -> (t, Error.t) Result.t

  val model : t -> string option
  val profile : t -> string option
  val settings : t -> Inference.Request.Setting.t list
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end

(** Effective is a host capture, never a second mutable settings store. Preparing
    is scoped to a live root request and is cleared on failure/cancellation.
    All Configuration values use the existing explicit safe whitelist. *)
type phase =
  | Preparing
  | Effective
  | Retained
[@@deriving equal, sexp]

type capture =
  { operation_id : Id.Operation.t
  ; revision : int64
  ; phase : phase
  ; configuration : (Inference.Observation.Configuration.t[@sexp.opaque])
  }
[@@deriving sexp]

type t =
  { revision : int64
  ; selected : (Inference.Observation.Configuration.t option[@sexp.opaque])
  ; capture : capture option
  ; pending : bool
  }
[@@deriving sexp]

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) Result.t

module Get_request : sig
  type t = { session_id : Id.Session.t } [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end

module Update_request : sig
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; expected_generation : int
    ; expected_revision : int64
    ; patch : Patch.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end
```

## session_lifecycle

[implementation](../../lib/agent_protocol/session_lifecycle.ml) · [interface](../../lib/agent_protocol/session_lifecycle.mli)

```ocaml
open! Core

(** Host-qualified lifecycle requests, independent of actor/store implementation.
    Restoring retains all session/workspace content but grants no execution. *)
module Revision : sig
  type t [@@deriving compare, equal, sexp]

  val zero : t
  val one : t
  val of_int64 : int64 -> (t, Protocol_error.t) Result.t
  val to_int64 : t -> int64
  val succ : t -> (t, Protocol_error.t) Result.t
end

module Expected : sig
  type t [@@deriving equal, sexp]

  val create
    :  reference:Session_ref.t
    -> generation:int
    -> session_revision:int64
    -> lifecycle_revision:Revision.t
    -> (t, Protocol_error.t) Result.t

  val reference : t -> Session_ref.t
  val generation : t -> int
  val session_revision : t -> int64
  val lifecycle_revision : t -> Revision.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Protocol_error.t) Result.t
end

module Request : sig
  (** Method determines Restore or Resume. The identity/generation/revisions and
      original idempotency key are part of the immutable original request digest.
      No attachment is required for an archived unloaded session. *)
  type t [@@deriving sexp]

  val create : expected:Expected.t -> idempotency_key:Idempotency_key.t -> t
  val expected : t -> Expected.t
  val idempotency_key : t -> Idempotency_key.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Protocol_error.t) Result.t
end

module Result : sig
  module Status : sig
    type t =
      | Active
      | Archived
      | Removed
    [@@deriving equal, sexp]

    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Protocol_error.t) Core.Result.t
  end

  module Admission : sig
    type t =
      | Automatic
      | Explicit_resume_required
    [@@deriving equal, sexp]

    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Protocol_error.t) Core.Result.t
  end

  module Action : sig
    type t =
      | Archive
      | Restore
      | Resume
      | Remove
    [@@deriving equal, sexp]
  end

  module Disposition : sig
    type t =
      | Applied
      | Already_current
    [@@deriving equal, sexp]
  end

  (** Validating create/decoder enforces legal status/action/admission pairs,
      nonnegative anchors and nonzero archived/removed lifecycle revisions.
      Exposes actual current disposition rather than inventing replacement state. *)
  type t [@@deriving sexp]

  val expected : t -> Expected.t
  val latest_event_sequence : t -> int64
  val status : t -> Status.t
  val admission : t -> Admission.t
  val action : t -> Action.t
  val disposition : t -> Disposition.t
  val completed_at : t -> Timestamp.t

  val create
    :  reference:Session_ref.t
    -> generation:int
    -> session_revision:int64
    -> latest_event_sequence:int64
    -> lifecycle_revision:Revision.t
    -> status:Status.t
    -> admission:Admission.t
    -> action:Action.t
    -> disposition:Disposition.t
    -> completed_at:Timestamp.t
    -> (t, Protocol_error.t) Core.Result.t

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Protocol_error.t) Core.Result.t
end

module Observation : sig
  (** Current nonactivating observation; never an execution capability. *)
  type t [@@deriving sexp]

  val create
    :  expected:Expected.t
    -> status:Result.Status.t
    -> admission:Result.Admission.t
    -> (t, Protocol_error.t) Core.Result.t

  val expected : t -> Expected.t
  val status : t -> Result.Status.t
  val admission : t -> Result.Admission.t
  val matches_session : t -> Session.t -> bool
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Protocol_error.t) Core.Result.t
end
```

## session_metadata

[implementation](../../lib/agent_protocol/session_metadata.ml) · [interface](../../lib/agent_protocol/session_metadata.mli)

```ocaml
(** Organization metadata confers no execution authority. Stored values retain
    historical strings under their document bounds; label keys are unique.
    Authored patches separately bound names to 1024 bytes, 128 label operations,
    keys to 256 bytes and values to 4096 bytes and reject malformed text. *)
module Values : sig
  type t = private
    { display_name : string option
    ; labels : (string * string) list
    }
  [@@deriving equal, sexp]

  val create
    :  display_name:string option
    -> labels:(string * string) list
    -> (t, Error.t) Result.t
end

module Patch : sig
  type name_change =
    | Keep
    | Set of string
    | Clear
  [@@deriving equal, sexp]

  type t [@@deriving sexp]

  (** Duplicate keys and overlapping set/remove operations are invalid. *)
  val create
    :  name:name_change
    -> set_labels:(string * string) list
    -> remove_labels:string list
    -> (t, Error.t) Result.t

  val apply : t -> Values.t -> (Values.t, Error.t) Result.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end

module Request : sig
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; expected_metadata_revision : int64
    ; patch : Patch.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end
```

## session_organization

[implementation](../../lib/agent_protocol/session_organization.ml) · [interface](../../lib/agent_protocol/session_organization.mli)

```ocaml
(** Host-qualified historical membership, independent of execution workspace,
    prompt, provider, grants and session-content authority. IDs in [Values] need
    not remain live; current existence/visibility belongs to the host projection. *)
module Values : sig
  type t = private
    { project_id : Id.Project.t option
    ; collection_ids : Id.Collection.t list
    }
  [@@deriving equal, sexp]

  val empty : t

  (** Reject duplicates and more than 128 collections. Normalize collection IDs
      by [Id.Collection.compare]; project and collection namespaces remain typed. *)
  val create
    :  project_id:Id.Project.t option
    -> collection_ids:Id.Collection.t list
    -> (t, Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Patch : sig
  type project_change =
    | Keep
    | Set of Id.Project.t
    | Clear
  [@@deriving equal, sexp]

  type t [@@deriving sexp]

  (** Each operation list is bounded to 128 distinct IDs; overlapping add/remove
      is invalid. Apply enforces the total 128 collection bound. *)
  val create
    :  project:project_change
    -> add_collections:Id.Collection.t list
    -> remove_collections:Id.Collection.t list
    -> (t, Error.t) result

  (** Clear/remove absent IDs are no-ops; never resolves current host objects. *)
  val apply : t -> previous:Values.t -> (Values.t, Error.t) result

  (** Set/add references requiring group ownership authorization before fresh
      admission, receipt lookup or idempotent replay. Cleanup has no references. *)
  val requested_groups : t -> Values.t

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Request : sig
  type t =
    { host_id : Id.Server.t
    ; session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; expected_metadata_revision : int64
    ; patch : Patch.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  (** Decode rejects negative revision and malformed/bounded patch values. *)
  val to_json : t -> Jsonaf.t

  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Query : sig
  module Project : sig
    type t =
      | Any
      | Unassigned
      | Project of Id.Project.t
    [@@deriving equal, sexp]
  end

  type t = private
    { project : Project.t
    ; collection_all_of : Id.Collection.t list
    }
  [@@deriving equal, sexp]

  val default : t

  (** Reject duplicate or more than 128 collection filters; normalize typed IDs. *)
  val create
    :  project:Project.t
    -> collection_all_of:Id.Collection.t list
    -> (t, Error.t) result

  (** [Any] and no collections needs no additional organization read grant. *)
  val requires_organization_view : t -> bool

  val matches : t -> effective:Values.t -> bool
  val to_fields : t -> (string * Jsonaf.t) list
  val of_fields : Json_codec.fields -> (t, Error.t) result
end
```

## session_ref

[implementation](../../lib/agent_protocol/session_ref.ml) · [interface](../../lib/agent_protocol/session_ref.mli)

```ocaml
(** Persistent host-qualified identity; carries no connection or execution grant. *)
type t [@@deriving compare, equal, sexp_of]

val create : server_id:Id.Server.t -> session_id:Id.Session.t -> t
val server_id : t -> Id.Server.t
val session_id : t -> Id.Session.t
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result

(** Snapshot lifecycle observations use this decoder. Both string IDs are
    admitted through their validated constructors; decoding grants no authority. *)
val t_of_sexp : Sexplib0.Sexp.t -> t
```

## session_seed

[implementation](../../lib/agent_protocol/session_seed.ml) · [interface](../../lib/agent_protocol/session_seed.mli)

```ocaml
open! Core
module Error = Protocol_error

(** New stopped conversation with an already-converted neutral transcript.
    This command conveys no runtime, source, workspace or tool authority. *)
module Transcript : sig
  type t [@@deriving sexp]

  (** Validate complete neutral payloads, distinct IDs and ordered relations.
      Entries must be nonredacted Canonical entries at content revision zero;
      their role/kind must agree with the neutral payload owner. Provider item IDs,
      opaque call IDs, field order and numeric spellings remain unchanged. Missing
      archived calls remain legal under the existing relation validator.

      Bound input before derived allocations. Positive [max_entries] and
      [max_work_steps] cover this invocation, including semantic validation;
      [limits] bounds compact JSON bytes, depth, fields and nodes. Checkpoint
      exceptions propagate unchanged. This operation invokes no Converter.
      The actual new session namespace is checked separately by its state owner. *)
  val create
    :  History.entry list
    -> next_history_sequence:int64
    -> limits:Document_schema.Limits.t
    -> max_entries:int
    -> max_work_steps:int
    -> checkpoint:(unit -> unit)
    -> (t, Error.t) Result.t

  val entries : t -> History.entry list
  val next_history_sequence : t -> int64
  val to_json : t -> Jsonaf.t

  (** Strict admission under the existing 16 MiB transport/durable document
      ceiling. The sexp reader applies the same invariant checks. No client
      provenance becomes a private invocation, authorization or operation receipt. *)
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end

module Code_selection : sig
  type t =
    | Catalog of
        { definition_id : Id.Prompt_definition.t
        ; expected_revision : Id.Prompt_revision.t
        }
    | Retained of Session_lifecycle.Expected.t
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end

module Request : sig
  type t [@@deriving sexp]

  (** Validate an authored Catalog prompt selector and Configured workspace,
      [start_immediately=false] and ordinary Spec metadata/host combinations.
      Catalog selection must agree with [spec.prompt]; Retained selection is
      checked against the actual retained definition by the host. No initial
      attachment or subscription exists in this request.

      The original command binds the complete seed, selector, Spec and key;
      existing request and receipt ceilings still apply. There is no hidden
      upload table or truncated history. A different key is a new conversation. *)
  val create
    :  Session.Spec.t
    -> code:Code_selection.t
    -> transcript:Transcript.t
    -> idempotency_key:Idempotency_key.t
    -> (t, Error.t) Result.t

  val spec : t -> Session.Spec.t
  val code : t -> Code_selection.t
  val transcript : t -> Transcript.t
  val idempotency_key : t -> Idempotency_key.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end

module Result : sig
  type t = { session_id : Id.Session.t } [@@deriving sexp]

  (** Persisted stopped identity. Activation/attachment is a subsequent separately
      authorized command. The ordinary receipt summary is Created_session. *)
  val to_json : t -> Jsonaf.t

  val of_json : Jsonaf.t -> (t, Error.t) Core.Result.t
end
```

## session_work

[implementation](../../lib/agent_protocol/session_work.ml) · [interface](../../lib/agent_protocol/session_work.mli)

```ocaml
(** Payload-free work occurrence projections. Current security-view and transcript
    visibility are required by the server; decoding grants no control capability. *)
module Key : sig
  type t =
    | Job of
        { id : Id.Job.t
        ; attempt : int
        }
    | Schedule of Id.Schedule.t
    | Invocation of Id.Invocation.t
    | Subscription of Id.Subscription.t
    | Delivery of Id.Delivery.t
    | Moderator_execution of Id.Moderator_execution.t
  [@@deriving compare, equal, sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Status : sig
  type t =
    | Accepted
    | Running
    | Waiting_approval
    | Waiting_work
    | Succeeded
    | Failed
    | Cancelled
    | Interrupted
    | Unsupported
  [@@deriving compare, equal, sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Delivery_state : sig
  type t =
    | Not_applicable
    | Pending
    | Acknowledged
    | Discarded
  [@@deriving compare, equal, sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

type t = private
  { session : Session_ref.t
  ; generation : int
  ; key : Key.t
  ; status : Status.t
  ; delivery : Delivery_state.t
  ; revision : int64
  }
[@@deriving sexp]

val create
  :  session:Session_ref.t
  -> generation:int
  -> key:Key.t
  -> status:Status.t
  -> delivery:Delivery_state.t
  -> revision:int64
  -> (t, Error.t) result

val compare_key : t -> t -> int
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result

module Query : sig
  type t =
    { session : Session_ref.t
    ; page : Page.Request.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end
```

## snapshot

[implementation](../../lib/agent_protocol/snapshot.ml) · [interface](../../lib/agent_protocol/snapshot.mli)

```ocaml
(** Principal-projected session state used by all connected clients. *)

type t =
  { session : Session.t
  ; lifecycle : Session_lifecycle.Observation.t option [@sexp.option]
  ; canonical_history : History.Window.t
  ; archived_revisions : int64 list [@sexp.list]
  ; effective_history : History.Window.t option
  ; deferred_entries : History.entry list
  ; permissions : Permission.t list
  ; grants : Grant.t list
  ; jobs : Job.t list
  ; extension_status : Extension_status.t list [@sexp.list]
  ; schedules : Schedule.t list
  ; active_tool_calls : Jsonaf.t list
  ; active_agent_calls : Jsonaf.t list
  ; halted : bool
  ; halt_reason : string option
  ; failure : Error.t option
  ; revision : int64
  ; latest_event_sequence : int64
  }
[@@deriving sexp]

(** [to_json t] encodes a rendering-neutral client snapshot. *)
val to_json : t -> Jsonaf.t

(** [of_json json] rejects snapshots whose top-level revision differs from the
    embedded session summary. *)
val of_json : Jsonaf.t -> (t, Error.t) result
```

## stored_completion

[implementation](../../lib/agent_protocol/stored_completion.ml) · [interface](../../lib/agent_protocol/stored_completion.mli)

```ocaml
(** Explicit async-job completion storage. Inline values retain their existing
    Completion encoding. Artifact envelopes occur only at this storage boundary,
    never by interpreting an arbitrary business JSON object's shape. *)
type outcome =
  | Succeeded
  | Failed
  | Cancelled
  | Expired
[@@deriving equal, sexp]

type t =
  | Inline of Completion.t
  | Artifact of
      { outcome : outcome
      ; reference : Job_artifact.t
      }
[@@deriving sexp]

val outcome : t -> outcome
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result

(** Bind an actual validated completion to a prepared artifact, verifying its
    serialized length and digest. Does not grant read or publication authority. *)
val artifact : Job_artifact.t -> Completion.t -> (t, Error.t) result

(** Compare a validated completion with an inline value or the artifact's outcome,
    byte length and digest. Does not read files or run effects. *)
val matches : t -> Completion.t -> (bool, Error.t) result

(** Load and revalidate an artifact's exact outcome and content. The host loader
    must enforce ownership, disclosure, bounded reads and raw byte verification. *)
val materialize
  :  load:(Job_artifact.t -> (Completion.t, Error.t) result)
  -> t
  -> (Completion.t, Error.t) result
```

## stream_error

[implementation](../../lib/agent_protocol/stream_error.ml) · [interface](../../lib/agent_protocol/stream_error.mli)

```ocaml
(** Attachment-scoped terminal notification for a failed event subscription.
    This has no durable sequence and cannot substitute for a missing event.
    The server supplies a sanitized error; the client must obtain a fresh
    snapshot before treating the session projection as current again. *)
type t = private
  { session_id : Id.Session.t
  ; attachment_id : Id.Attachment.t
  ; error : Error.t
  }
[@@deriving sexp_of]

val create : session_id:Id.Session.t -> attachment_id:Id.Attachment.t -> Error.t -> t
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
val to_notification : t -> Envelope.t
```

## subscription

[implementation](../../lib/agent_protocol/subscription.ml) · [interface](../../lib/agent_protocol/subscription.mli)

```ocaml
(** Durable moderator subscription identity and terminal state. Workflow-specific
    state lives in the moderator. These functions do not execute timers or jobs. *)
type context =
  { id : Id.Subscription.t
  ; session_id : Id.Session.t
  ; generation : int
  ; invocation_id : Id.Invocation.t
  ; source : Invocation.observer option [@sexp.option]
    (** Codec 2 binds the creating moderator source. Codec 1 records decode with
        None and must not acquire current-moderator authority implicitly. *)
  ; parent_job : (Id.Job.t * int) option [@sexp.option]
    (** Codec 3 pins the actor-owned creating job attempt. Absence on older
        records must not be upgraded to the current attempt implicitly. *)
  ; kind : string
  ; created_at : Timestamp.t
  ; deadline : Timestamp.t
  ; completion_schema : Jsonaf.t option
  ; wake : Completion.wake
  ; ingress_capability : Id.Capability.t option
  }
[@@deriving equal, sexp]

type t = private
  { context : context
  ; epoch : int
  ; timer_id : Id.Schedule.t option
  ; job_id : Id.Job.t option
  ; result : Completion.t option
  ; completed_at : Timestamp.t option
  }
[@@deriving equal, sexp]

val create : context -> (t, Error.t) result
val validate : t -> (unit, Error.t) result

val arm
  :  t
  -> expected_epoch:int
  -> timer_id:Id.Schedule.t option
  -> job_id:Id.Job.t option
  -> (t, Error.t) result

(** First terminal commit wins. A repeat returns the retained winner and [false].
    A stale epoch cannot complete a still-active subscription. Expiry cannot be
    recorded before the deadline. Actual work cancellation is a host action. *)
val finish
  :  t
  -> expected_epoch:int
  -> now:Timestamp.t
  -> Completion.t
  -> (t * bool, Error.t) result

val validate_transition : previous:t option -> t -> (unit, Error.t) result
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result

(** Complete current durable record projection. Required-null option fields;
    independent of the public protocol's historical envelope variants. *)
module Storage : sig
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end
```

## timestamp

[implementation](../../lib/agent_protocol/timestamp.ml) · [interface](../../lib/agent_protocol/timestamp.mli)

```ocaml
(** RFC 3339 UTC timestamps used by the agent protocol. *)

type t [@@deriving compare, equal, sexp]

(** [now ()] returns the current wall-clock time. *)
val now : unit -> t

(** [of_time_ns time] converts [time] without losing nanosecond precision. *)
val of_time_ns : Core.Time_ns.t -> t

(** [to_time_ns t] returns the underlying absolute time. *)
val to_time_ns : t -> Core.Time_ns.t

(** [diff_ns t since] returns the signed nanosecond difference without narrowing
    it to a Time_ns span. The difference of two timestamps fits in int64. *)
val diff_ns : t -> t -> int64

(** [add_ms t delay_ms] adds a nonnegative millisecond delay. Rejects negative
    delays and unrepresentable endpoints without wrapping or float rounding. *)
val add_ms : t -> int -> (t, Error.t) result

(** [of_string encoded] parses an RFC 3339 timestamp with an uppercase UTC [Z] suffix. *)
val of_string : string -> (t, Error.t) result

(** [to_string t] returns an RFC 3339 UTC timestamp. *)
val to_string : t -> string

(** [of_json json] decodes an RFC 3339 UTC JSON string. *)
val of_json : Jsonaf.t -> (t, Error.t) result

(** [to_json t] encodes [t] as an RFC 3339 UTC JSON string. *)
val to_json : t -> Jsonaf.t
```

## version

[implementation](../../lib/agent_protocol/version.ml) · [interface](../../lib/agent_protocol/version.mli)

```ocaml
(** Ochat agent protocol versions and feature negotiation. *)

type t =
  { major : int
  ; minor : int
  }
[@@deriving compare, equal, sexp]

(** [initial] is the initial Ochat agent protocol version, [1.0]. *)
val initial : t

(** [current] is protocol [2.0], with neutral public transcript projections.
    This release rejects version-1 connections explicitly; storage is separate. *)
val current : t

(** Minimum negotiated version for ingress submission and its scope vocabulary. *)
val ingress_minimum : t

(** [create ~major ~minor] creates a non-negative protocol version. *)
val create : major:int -> minor:int -> (t, Error.t) result

(** [negotiate ~client_min ~client_max ~supported] selects the highest mutually
    supported version. The client range must stay within one major version. *)
val negotiate : client_min:t -> client_max:t -> supported:t list -> (t, Error.t) result

(** [validate_feature feature] accepts lowercase dotted feature identifiers. *)
val validate_feature : string -> (string, Error.t) result

(** [to_json t] encodes [t] using explicit [major] and [minor] fields. *)
val to_json : t -> Jsonaf.t

(** [of_json json] decodes a protocol version. *)
val of_json : Jsonaf.t -> (t, Error.t) result
```

## workspace

[implementation](../../lib/agent_protocol/workspace.ml) · [interface](../../lib/agent_protocol/workspace.mli)

```ocaml
(** Transport projections for configured workspace definitions. *)

type kind =
  | Physical
  | Temporary
[@@deriving compare, equal, sexp]

type temporary_location =
  | System_tmp
  | Session_dir
[@@deriving compare, equal, sexp]

type cleanup =
  | On_session_stop
  | On_session_delete
  | Retain
[@@deriving compare, equal, sexp]

type access =
  | Read_only
  | Shared_write
  | Exclusive
[@@deriving compare, equal, sexp]

type overflow =
  | Reject
  | Queue
[@@deriving compare, equal, sexp]

type availability =
  | Available
  | Unavailable of { reason : string }
[@@deriving compare, equal, sexp]

type prompt_limit =
  { prompt_id : Id.Prompt_definition.t
  ; max_root_agents : int
  ; overflow : overflow
  }
[@@deriving sexp]

type t =
  { id : Id.Workspace_definition.t
  ; name : string
  ; kind : kind
  ; temporary_location : temporary_location option
  ; cleanup : cleanup option
  ; access : access
  ; conflict_domain : string option
  ; prompt_limits : prompt_limit list
  ; availability : availability
  }
[@@deriving sexp]

(** [to_json t] encodes a workspace summary without native filesystem paths. *)
val to_json : t -> Jsonaf.t

(** [of_json json] decodes and validates a workspace summary. *)
val of_json : Jsonaf.t -> (t, Error.t) result

module List_request : sig
  type t =
    { page : Page.Request.t
    ; kind : kind option
    ; access : access option
    ; available : bool option
    }
  [@@deriving sexp]

  (** [to_json t] encodes workspace-list filters and pagination. *)
  val to_json : t -> Jsonaf.t

  (** [of_json json] decodes workspace-list filters and pagination. *)
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Get_request : sig
  type t = { workspace_id : Id.Workspace_definition.t } [@@deriving sexp]

  (** [to_json t] encodes a workspace lookup request. *)
  val to_json : t -> Jsonaf.t

  (** [of_json json] decodes a workspace lookup request. *)
  val of_json : Jsonaf.t -> (t, Error.t) result
end
```
