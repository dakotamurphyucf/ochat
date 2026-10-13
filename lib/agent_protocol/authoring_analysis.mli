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
