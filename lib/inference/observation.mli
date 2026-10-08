open! Core

(** Pure neutral inference observations. No OpenAI, protocol/session IDs, I/O,
    pricing, credentials, canonical publication or durable-retention policy.
    An inference terminal is not a successfully committed host turn. *)
module Error : sig
  type t =
    | Json of Document_schema.Error.t
    | Invalid_field of
        { field : string
        ; reason : string
        }
    | Conflicting_revision
    | Conflicting_scope
    | Conflicting_kind
    | Retention_limit
  [@@deriving equal, sexp_of]
end

module Admission : sig
  (** Default compact JSON bounds: usage/context/configuration observation 8 KiB,
      diagnostic observation 1 KiB, complete attempt row 64 KiB. The entire
      envelope, including identities, participates. Explicit caller limits may
      be stricter or larger; native count and vocabulary invariants still apply.
      Encoded size is measured before allocating serialized output. *)
  val observation : Document_schema.Limits.t

  val diagnostic : Document_schema.Limits.t
  val attempt : Document_schema.Limits.t
end

module Observation_id : sig
  type t [@@deriving compare, equal, hash, sexp_of]

  include Comparator.S with type t := t

  (** Nonempty UTF8, at most 512 compact JSON bytes. Allocated by the actual
      observer, stable across authoritative revisions; never inferred from text. *)
  val of_string : string -> (t, Error.t) Result.t

  val to_string : t -> string
end

module Estimator : sig
  type method_ =
    | O200k_serialized_history
    | Utf8_heuristic
  [@@deriving equal, sexp_of]

  type t

  (** Positive estimator implementation version. This is an explicit algorithm
      label, not arbitrary provider text or evidence of billable consumption. *)
  val create : method_:method_ -> version:int -> (t, Error.t) Result.t

  val method_ : t -> method_
  val version : t -> int
  val equal : t -> t -> bool
end

module Count : sig
  type unknown_reason =
    | Not_reported
    | Explicit_null
    | Interrupted
    | Not_submitted
    | Before_tracking
  [@@deriving equal, sexp_of]

  type view =
    | Unknown of unknown_reason
    | Estimated of
        { tokens : int64
        ; estimator : Estimator.t
        }
    | Actual of int64

  type t

  (** Counts are nonnegative token counts. Actual zero, Estimated zero and every
      Unknown reason remain distinct. No missing value is normalized to zero. *)
  val create : view -> (t, Error.t) Result.t

  val view : t -> view
  val equal : t -> t -> bool
end

module Usage : sig
  module Component : sig
    type t =
      | Input
      | Output
      | Reported_total
      | Cached_input
      | Cache_write_input
      | Reasoning_output
    [@@deriving compare, equal, sexp_of]
  end

  type counts =
    { input : Count.t
    ; output : Count.t
    ; reported_total : Count.t
    ; cached_input : Count.t
    ; cache_write_input : Count.t
    ; reasoning_output : Count.t
    }

  type inclusion =
    { subset : Component.t
    ; included_in : Component.t
    }

  type t

  (** A complete authoritative snapshot, not an additive increment. Exactly six
      component states preserve mixed absent/null/actual detail. Inclusion edges
      are unique, non-reflexive and acyclic, at most twelve; when both endpoints
      are Actual, subset <= container, including transitive reachability through
      unknown intermediate components. Estimated and Actual are not interchangeable
      evidence. No total=input+output assertion is invented; reported_total is
      separately retained. Cached/reasoning subsets must never be summed again
      with their declared containers. No unchecked aggregate arithmetic occurs. *)
  val create : counts:counts -> inclusions:inclusion list -> (t, Error.t) Result.t

  val counts : t -> counts
  val count : t -> Component.t -> Count.t
  val inclusions : t -> inclusion list
  val equal : t -> t -> bool
end

module Context_estimate : sig
  type capacity =
    | Unknown
    | Declared of int64

  type t

  (** Preparation identity is bounded nonempty UTF8 (512 encoded bytes), allocated
      by the host, NEVER a fingerprint/digest of private low-entropy input. It
      identifies the actually estimated preparation. Count must be Estimated or
      Unknown, never Actual; a declared capacity is positive host/profile evidence,
      not a model catalog guess. Capacity and context pressure are not usage or
      billing. A changed prepared input requires a new observation identity. *)
  val create
    :  preparation_id:string
    -> count:Count.t
    -> capacity:capacity
    -> (t, Error.t) Result.t

  val preparation_id : t -> string
  val count : t -> Count.t
  val capacity : t -> capacity
  val equal : t -> t -> bool
end

module Transport_policy : sig
  type t =
    | Http_sse
    | Prefer_websocket
    | Require_websocket
  [@@deriving equal, sexp_of]
end

module Configuration : sig
  module Name : sig
    type t =
      | Instructions
      | Max_output_tokens
      | Parallel_tool_calls
      | Temperature
      | Top_p
      | Reasoning_effort
      | Reasoning_summary
      | Text_verbosity
      | Text_format
      | Tool_choice
      | Prompt_cache_key
      | Prompt_cache_retention
    [@@deriving compare, equal, sexp_of]
  end

  type value =
    | Tokens of int64
    | Boolean of bool
    | Temperature of float
    | Probability of float
    | Reasoning_effort of [ `None | `Minimal | `Low | `Medium | `High | `Xhigh ]
    | Reasoning_summary of [ `Auto | `Concise | `Detailed ]
    | Verbosity of [ `Low | `Medium | `High ]
    | Text_format of [ `Text | `Json_object | `Json_schema ]
    | Tool_choice of [ `Auto | `None | `Required ]
    | Cache_retention of [ `In_memory | `Hours_24 ]

  type selection =
    | Omitted
    | Explicit_null
    | Value of value
    | Withheld

  type setting = private
    { name : Name.t
    ; selection : selection
    ; provenance : Request.Setting.provenance option
    }

  type feature =
    | Text_input
    | Image_input
    | Document_input
    | Function_tools
    | Custom_tools
    | Opaque_replay
    | Setting of Name.t
  [@@deriving compare, equal, sexp_of]

  type support =
    | Supported
    | Unsupported
    | Unknown
  [@@deriving equal, sexp_of]

  type transport =
    | Http_sse
    | Websocket
    | In_process
    | Unknown_transport
  [@@deriving equal, sexp_of]

  type t

  (** Explicit safe projection of the selected Target. Includes bounded declared
      adapter/profile/revision/account/model labels, NEVER endpoint, credentials,
      instructions, cache key, tool names/schemas, unknown setting JSON or raw
      prepared JSON. At most 64 unique feature declarations. Supported/Unknown
      are declared preparation evidence, not live probes. Values use only the
      closed vocabulary above and finite validated numeric domains. Unknown or
      unrecognized values become Withheld; their original text never enters this
      value. Capability declaration order has no identity meaning and is
      canonicalized after duplicate/count validation. Structured setting subfields
      preserve absent versus null; provenance
      is copied only from an actual selected setting. Unknown setting names are
      represented only by withheld_settings count, never by arbitrary labels.
      Reading this value requires Diagnostics AND existing session visibility;
      that runtime authority cannot be granted by constructing this pure value. *)
  val of_target
    :  ?transport_policy:Transport_policy.t
    -> Request.Target.t
    -> preparation_id:string
    -> transport:transport
    -> capabilities:(feature * support) list
    -> limits:Document_schema.Limits.t
    -> (t, Error.t) Result.t

  val adapter : t -> string
  val profile : t -> string
  val profile_revision : t -> string option
  val account : t -> string option
  val model : t -> string
  val preparation_id : t -> string

  (** Initial nomination, never evidence of actual dispatch after fallback. *)
  val transport : t -> transport

  val transport_policy : t -> Transport_policy.t option
  val settings : t -> setting list
  val withheld_settings : t -> int
  val capabilities : t -> (feature * support) list
  val equal : t -> t -> bool
  val to_json : t -> Jsonaf.t

  (** Complete original JSON admission precedes closed safe decoding; no unknown
      private fields or provider strings survive into this projection. *)
  val of_json : Jsonaf.t -> limits:Document_schema.Limits.t -> (t, Error.t) Result.t
end

module Transport_selection : sig
  type transport =
    | Http_sse
    | Websocket
  [@@deriving equal, sexp_of]

  type fallback_reason =
    | Unsupported
    | Session_busy
    | Connection
    | Upgrade
  [@@deriving equal, sexp_of]

  type t

  val create
    :  accounting_id:Observation_id.t
    -> requested:Transport_policy.t
    -> selected:transport
    -> fallback:fallback_reason option
    -> (t, Error.t) Result.t

  val accounting_id : t -> Observation_id.t
  val requested : t -> Transport_policy.t
  val selected : t -> transport
  val fallback : t -> fallback_reason option
  val equal : t -> t -> bool
end

module Diagnostic : sig
  module Http_rejection : sig
    (** Closed display evidence only; no body/message/code/parameter string survives. *)
    type reason =
      | Missing_required_parameter
      | Unsupported_parameter
      | Invalid_parameter
      | Unclassified
    [@@deriving equal, sexp_of]

    type parameter =
      | Instructions
      | Store
      | Model
      | Input
      | Tools
      | Stream
      | Text
      | Reasoning
      | Truncation
      | Other
    [@@deriving equal, sexp_of]

    type t

    val create
      :  status:int
      -> reason:reason
      -> parameter:parameter option
      -> (t, Error.t) result

    val status : t -> int
    val reason : t -> reason
    val parameter : t -> parameter option
    val equal : t -> t -> bool
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) result
  end

  (** Closed summary of rejected response Content-Type headers. No header values
      or unknown media names are retained. Media is unique and bounded to four. *)
  module Response_content_type : sig
    type shape =
      | Absent
      | Single
      | Duplicate_same
      | Duplicate_conflicting
    [@@deriving equal, sexp_of]

    type media =
      | Event_stream
      | Json
      | Html
      | Other
    [@@deriving equal, sexp_of]

    type t

    val create : shape:shape -> media:media list -> (t, Error.t) result
    val shape : t -> shape
    val media : t -> media list
    val equal : t -> t -> bool
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) result
  end

  module Protocol_violation : sig
    type stage =
      | Feed
      | Eof
      | Terminal
    [@@deriving equal, sexp_of]

    type decode =
      | Missing
      | Wrong_type
      | Invalid
      | Duplicate
      | Limit
    [@@deriving equal, sexp_of]

    type tracker =
      | Origin_mismatch
      | Sequence_conflict
      | Sequence_regression
      | Item_conflict
      | Part_conflict
      | Response_conflict
      | Event_after_terminal
      | Terminal_mismatch
      | Truncated
    [@@deriving equal, sexp_of]

    module Item_conflict : sig
      type event =
        | Response
        | Item_added
        | Item_done
        | Part_added
        | Part_done
        | Delta
        | Text_done
        | Annotation_added
        | Terminal
        | Error
        | Unknown
      [@@deriving equal, sexp_of]

      type cause =
        | Identity_changed
        | Identity_reused
        | Duplicate_added
        | Added_after_final
        | Descriptor_changed
        | Final_snapshot_changed
      [@@deriving equal, sexp_of]

      type field =
        | Id
        | Type
        | Name
        | Call_id
        | Namespace
        | Async
        | Caller
        | Phase
        | Status
        | Content
        | Summary
        | Encrypted_content
        | Arguments
        | Input
        | Role
        | Other
      [@@deriving compare, equal, sexp_of]

      type t [@@deriving equal, sexp_of]

      val create
        :  event:event
        -> cause:cause
        -> fields:field list
        -> (t, Error.t) Result.t

      val event : t -> event
      val cause : t -> cause
      val fields : t -> field list
      val to_json : t -> Jsonaf.t
      val of_json : Jsonaf.t -> (t, Error.t) Result.t
    end

    type kind =
      | Framing
      | Decode of decode
      | Tracker of tracker
      | Item_conflict of Item_conflict.t
    [@@deriving equal, sexp_of]

    type t =
      { stage : stage
      ; kind : kind
      }
    [@@deriving equal, sexp_of]

    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) Result.t
  end

  type phase =
    | Preparation
    | Authentication
    | Dispatch
    | Stream
    | Observation
  [@@deriving equal, sexp_of]

  type limit_kind =
    | Request_bytes
    | Response_body_bytes
    | Transfer_framing_bytes
    | Sse_frame_bytes
    | Json_depth
    | Json_nodes
    | Json_fields
    | Observation_bytes
    | Diagnostic_entries
    | Diagnostic_bytes
  [@@deriving equal, sexp_of]

  type reason =
    | Authentication of Event.Terminal.auth_failure
    | Connection
    | Timeout
    | Http_status of int
    | Http_rejection of Http_rejection.t
    | Response_content_type of Response_content_type.t
    | Malformed_protocol
    | Protocol_violation of Protocol_violation.t
    | Unsupported_input
    | Provider_failure
    | Local_result_invalid
    | Limit of limit_kind
    | Conflicting_observation
  [@@deriving equal, sexp_of]

  type t

  (** No arbitrary message/code/provider body/exception/endpoint input exists.
      HTTP status is 100..599; elapsed_ms, when actually measured, is nonnegative.
      Failure to retain a display diagnostic must not interrupt canonical work.
      Public diagnostics require Diagnostics plus existing session visibility. *)
  val create
    :  phase:phase
    -> reason:reason
    -> delivery:Event.Terminal.delivery option
    -> elapsed_ms:int64 option
    -> (t, Error.t) Result.t

  val phase : t -> phase
  val reason : t -> reason
  val delivery : t -> Event.Terminal.delivery option
  val elapsed_ms : t -> int64 option
  val message : t -> string
  val equal : t -> t -> bool
end

module Key : sig
  type t =
    { scope : Transcript.Scope.Key.t
    ; observation : Observation_id.t
    }
  [@@deriving compare, equal, hash, sexp_of]

  include Comparator.S with type t := t
end

type payload =
  | Usage of Usage.t
  | Context_estimate of Context_estimate.t
  | Configuration of Configuration.t
  | Transport_selection of Transport_selection.t
  | Diagnostic of Diagnostic.t

type t

(** Scope is an ACTUAL dispatch identity including full parent semantics. Revision
    is nonnegative. All native invariants and the full encoded envelope are
    validated under limits; to_json contains only the safe closed projection.
    Decoding rejects unknown owned vocabulary/fields rather than retaining raw
    potentially private JSON in a public observation. Durable outer document
    carriers retain future extensions separately at their existing owner boundary.
    Usage visibility is session visibility; configuration/diagnostics additionally
    require Diagnostics. Context estimates never masquerade as billable usage. *)
val create
  :  scope:Transcript.Scope.t
  -> id:Observation_id.t
  -> revision:int64
  -> payload:payload
  -> limits:Document_schema.Limits.t
  -> (t, Error.t) Result.t

val scope : t -> Transcript.Scope.t
val id : t -> Observation_id.t
val key : t -> Key.t
val revision : t -> int64
val payload : t -> payload
val equal : t -> t -> bool
val encoded_bytes : t -> int
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> limits:Document_schema.Limits.t -> (t, Error.t) Result.t
val validate : t -> limits:Document_schema.Limits.t -> (unit, Error.t) Result.t

module Latest : sig
  type observation = t
  type t

  type disposition =
    | Added
    | Replaced
    | Duplicate
    | Stale
  [@@deriving equal, sexp_of]

  (** Bounded immutable working set, NOT a durable accounting/retirement policy.
      Positive bounds; no implicit eviction. Retained bytes sum complete encoded
      observations. Owners must look up archived identities before deciding that
      a missing live key is new; eviction never proves exact lifetime totals. *)
  val create : max_observations:int -> max_retained_bytes:int -> (t, Error.t) Result.t

  (** Larger revision replaces the COMPLETE prior snapshot, even if a count falls
      or becomes unknown. Same revision/equal value is Duplicate; same revision/
      differing value is conflict. Older revision is Stale. Full Scope semantics
      and payload family must match at every revision, including stale receipts;
      Context preparation identity must also match. Configuration is captured
      immutable selection: ANY changed value conflicts at every revision, while
      an identical value may advance revision. Error leaves t unchanged.
      Replacements are never summed as additional consumption. *)
  val observe : t -> observation -> (t * disposition, Error.t) Result.t

  val find : t -> Key.t -> observation option
  val observations : t -> observation list
  val retained_bytes : t -> int
end

module Attempt_record : sig
  type observation = t

  type interruption =
    | Cancelled
    | Host_interrupted
  [@@deriving equal, sexp_of]

  type state =
    | Prepared
    | Running
    | Terminal of Event.Terminal.t
    | Interrupted of
        { reason : interruption
        ; delivery : Event.Terminal.delivery
        }

  type t

  (** Safe bounded read row for ONE actual attempt, at most 64 observations. State
      is explicit actual host evidence, never inferred from usage. Interruption
      preserves cancellation/lost-host state without manufacturing a provider
      terminal or swallowing cancellation. A terminal, if present, must
      carry the same full Scope; it ends inference only. Latest observations must
      have unique keys and the same full Scope, at most one designated Usage
      observation, whose ID must match the host-allocated accounting_id even when
      no usage has arrived yet. Every Configuration observation must equal the row
      configuration. At most 16 diagnostic observations and 16 KiB encoded diagnostics;
      omitted_diagnostics is an explicit nonnegative overflow count. Complete row
      admission includes configuration/terminal/observations/identities. Never
      contains the private Request.Target or raw provider payload. This does not
      assert a host-turn commit or exact lifetime/session coverage. *)
  val create
    :  scope:Transcript.Scope.t
    -> accounting_id:Observation_id.t
    -> configuration:Configuration.t
    -> state:state
    -> observations:observation list
    -> omitted_diagnostics:int64
    -> limits:Document_schema.Limits.t
    -> (t, Error.t) Result.t

  val scope : t -> Transcript.Scope.t
  val accounting_id : t -> Observation_id.t
  val configuration : t -> Configuration.t
  val state : t -> state
  val observations : t -> observation list
  val omitted_diagnostics : t -> int64
  val encoded_bytes : t -> int

  (** Reuses immutable constructor domain invariants. An equal complete admission
      profile reuses the row's proof; any different profile admits its complete
      canonical JSON again, including all byte/depth/field/node bounds. A row
      constructed with permissive limits does not bypass a stricter owner bound. *)
  val validate : t -> limits:Document_schema.Limits.t -> (unit, Error.t) Result.t

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> limits:Document_schema.Limits.t -> (t, Error.t) Result.t
end
