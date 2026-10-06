(** Design-only OCH-49 migration interfaces; no runtime implementation/library.
    Core/Jsonaf semantic contracts do not import OpenAI, protocol or UI modules. *)
open! Core

module Field_presence : sig
  type 'a t =
    | Absent
    | Null
    | Value of 'a
  [@@deriving equal, sexp_of]
end

module type Identifier = sig
  type t [@@deriving equal, compare, sexp_of]

  val of_string : string -> (t, string) Result.t
  val to_string : t -> string
end

(** Existing host ID implementations are reused/extracted, not reallocated. *)
module History_id : Identifier

module Session_id : Identifier
module Operation_id : Identifier
module Invocation_id : Identifier
module Attempt_id : Identifier
module Observation_id : Identifier
module Source_id : Identifier
module Adapter_id : Identifier
module Profile_id : Identifier
module Account_ref : Identifier
module Model_id : Identifier
module Binding_id : Identifier
module Schema_revision : Identifier
module Fingerprint : Identifier

module Origin : sig
  type t

  val create
    :  adapter:Adapter_id.t
    -> profile:Profile_id.t
    -> account:Account_ref.t option
    -> endpoint_fingerprint:Fingerprint.t
    -> model:Model_id.t
    -> replay_version:int
    -> (t, string) Result.t

  val adapter : t -> Adapter_id.t
  val profile : t -> Profile_id.t
  val account : t -> Account_ref.t option
  val endpoint_fingerprint : t -> Fingerprint.t
  val model : t -> Model_id.t
end

module Provider_metadata : sig
  (** Metadata never substitutes for host identity or native authority. *)
  type t =
    { item_id : string option
    ; response_id : string option
    ; call_id : string option
    }
end

module Asset : sig
  (** Admitted immutable content/version; a mutable pathname or provider-only
      file ID cannot prove fresh-connection recovery. Host resolves before capture. *)
  type t

  val identity : t -> Fingerprint.t
  val media_type : t -> string
end

module Content : sig
  type t =
    | Text of string
    | Image of Asset.t
    | Document of Asset.t
end

module Role : sig
  type t =
    | System
    | Developer
    | User
    | Assistant
    | Tool
  [@@deriving equal, sexp_of]
end

module Call : sig
  type kind =
    | Function
    | Custom
  [@@deriving equal, sexp_of]

  type t

  (** Preserve exact original input separately from parsed or rewritten execution
      input. Nonempty wire name; provider call ID is never the sole dedupe key. *)
  val create
    :  kind:kind
    -> wire_name:string
    -> input_bytes:string
    -> metadata:Provider_metadata.t
    -> (t, string) Result.t

  val kind : t -> kind
  val input_bytes : t -> string
  val wire_name : t -> string
end

module Call_occurrence : sig
  (** Current one-call-per-entry mapping reuses History_entry.Id. Richer adapters
      split into distinct host entries while retaining source grouping. Existing
      Invocation.id remains the execution identity, not another job ID. *)
  type t

  val of_history_id : History_id.t -> t
  val history_id : t -> History_id.t
  val equal : t -> t -> bool
end

module Tool_output : sig
  type t =
    | Text of string
    | Content of Content.t list
end

module Semantic_item : sig
  type assistant_metadata =
    { phase : string option
    ; refusal : string option
    ; annotations : Jsonaf.t list
    }

  type view =
    | Message of
        { role : Role.t
        ; content : Content.t list
        ; assistant : assistant_metadata option
        }
    | Call of Call.t
    | Result of
        { occurrence : Call_occurrence.t
        ; output : Tool_output.t
        }
    | Reasoning of { readable_summary : string list }
    | Unknown of { provider_kind : string }

  type t

  val create : view -> (t, string) Result.t
  val view : t -> view
end

module Captured_item : sig
  (** Immutable raw envelope and one derived semantic view. JSON values/unknown
      fields are preserved, not original network spelling. Opaque reasoning stays
      private in this envelope. Editing produces Authored, invalidating replay. *)
  type t

  val capture
    :  origin:Origin.t
    -> raw:Jsonaf.t
    -> decode:(Jsonaf.t -> (Semantic_item.t, string) Result.t)
    -> (t, string) Result.t

  val raw : t -> Jsonaf.t
  val semantic : t -> Semantic_item.t
  val origin : t -> Origin.t
end

module Item : sig
  type t =
    | Authored of Semantic_item.t
    | Captured of Captured_item.t

  val semantic : t -> Semantic_item.t
end

module History : sig
  type entry =
    { id : History_id.t
    ; item : Item.t
    }

  (** Immutable effective request view; existing canonical store/provenance and
      allocator remain authoritative. Display-redacted history is not admissible. *)
  type t

  val entries : t -> entry list
  val fingerprint : t -> Fingerprint.t
end

module Tool_spec : sig
  type format =
    | Json_schema of Jsonaf.t
    | Custom_format of Jsonaf.t

  type t

  (** Existing binding plus captured schema; this grants no execution authority. *)
  val create
    :  binding:Binding_id.t
    -> wire_name:string
    -> schema_revision:Schema_revision.t
    -> format:format
    -> description:string option
    -> (t, string) Result.t

  val binding : t -> Binding_id.t
  val wire_name : t -> string
end

module Setting : sig
  type key =
    | Temperature
    | Max_output_tokens
    | Reasoning_effort
    | Parallel_tool_calls

  type value =
    | Float of float
    | Positive_int of int
    | String of string
    | Bool of bool

  type provenance =
    | Execution_override
    | Captured_prompt
    | Profile_default
    | Adapter_default

  type t

  (** Validates key/value domain, finite numbers and field-specific null policy.
      Absence can retain an unresolved provider default. Null is not universally
      reset/clear/default. Request construction rejects duplicate setting keys. *)
  val create
    :  key:key
    -> value:value Field_presence.t
    -> provenance:provenance
    -> (t, string) Result.t
end

module Output_format : sig
  type t =
    | Text
    | Json_object
    | Json_schema of
        { name : string
        ; schema : Jsonaf.t
        ; strict : bool
        }
end

module Transport : sig
  type t =
    | Sse
    | Websocket
  [@@deriving equal, sexp_of]

  type selection =
    | Require of t
    | Prefer_websocket
end

module Capability : sig
  type feature =
    | Text_inference
    | Image_input
    | Document_input
    | Function_tools
    | Custom_tools
    | Structured_output
    | Opaque_replay
    | Setting of Setting.key
    | Transport of Transport.t

  type evidence =
    | Adapter_baseline
    | Profile_declaration
    | Maintained_model_metadata

  type support =
    | Supported of evidence list
    | Unsupported of
        { reason : string
        ; evidence : evidence list
        }
    | Unknown

  type t

  (** Intersect required constraints; explicit prohibition wins. A declaration
      cannot create a missing encoder. Arbitrary model IDs use a declared text
      baseline; optional explicit features require affirmative support. No probe. *)
  val resolve : t -> model:Model_id.t -> feature:feature -> support
end

module Target : sig
  type t

  (** Captured nonsecret intent; restore resolves current credentials for that
      same identity. Profile edits do not rewrite captured session settings. *)
  val create
    :  adapter:Adapter_id.t
    -> profile:Profile_id.t
    -> account:Account_ref.t option
    -> endpoint_fingerprint:Fingerprint.t
    -> model:Model_id.t
    -> settings:Setting.t list
    -> transport:Transport.selection
    -> (t, string) Result.t

  val profile : t -> Profile_id.t
  val model : t -> Model_id.t
end

module Request : sig
  type t

  (** Output-format absence preserves provider omission, distinct from explicit
      Text. Null is accepted only by a documented profile/field contract. *)
  val create
    :  attempt:Attempt_id.t
    -> source:Source_id.t
    -> target:Target.t
    -> history:History.t
    -> tools:Tool_spec.t list
    -> output_format:Output_format.t Field_presence.t
    -> (t, string) Result.t
end

module Preparation_error : sig
  type t =
    | Invalid_setting of
        { field : string
        ; reason : string
        }
    | Unsupported of
        { feature : Capability.feature
        ; reason : string
        }
    | Unknown_support of Capability.feature
    | Incompatible_replay of
        { entry : History_id.t
        ; reason : string
        }
    | Tool_name_collision of string
    | Asset_unavailable of Fingerprint.t
end

module Prepared_request : sig
  (** Immutable capture after moderation and before dispatch. No credentials or
      subsequent mutable-session reads. Admission receipt stays actor-owned. *)
  type t

  val attempt : t -> Attempt_id.t
  val source : t -> Source_id.t
  val target : t -> Target.t
  val fingerprint : t -> Fingerprint.t
end

module Adapter : sig
  type t

  val prepare
    :  t
    -> capabilities:Capability.t
    -> Request.t
    -> (Prepared_request.t, Preparation_error.t) Result.t
end

module Usage : sig
  type count =
    | Unknown
    | Estimated of int64
    | Actual of int64

  (** Nonnegative counts; reasoning/cache detail may be included in output/input
      already. Dedupe by attempt/observation identity. Consumption is not context
      capacity, price, committed-turn accounting or a goal-completion signal. *)
  type t

  (** Authoritative snapshot keyed by observation identity within an attempt.
      Nonnegative revision increases when replacing that snapshot. Same identity
      and revision must mean identical data; conflicts reject. Older revisions
      are stale, duplicates do not add counts, newer revisions replace counts. *)
  val observation_id : t -> Observation_id.t

  val revision : t -> int64
  val attempt : t -> Attempt_id.t
  val input_tokens : t -> count
  val output_tokens : t -> count
end

module Incomplete_reason : sig
  type t =
    | Output_limit
    | Filtered
    | Steered
    | Other of string
end

module Terminal : sig
  type delivery =
    | Definitely_not_submitted
    | Possibly_submitted
    | Response_started

  type t =
    | Completed
    | Incomplete of Incomplete_reason.t
    | Provider_failed of
        { code : string option
        ; message : string
        }
    | Transport_lost of
        { delivery : delivery
        ; message : string
        }

  (** Refusal is item metadata; partial output survives. Completed ends inference,
      not an operation, pending job, goal or script-owned headless run. *)
end

module Event : sig
  type item_key

  type descriptor =
    | Message of
        { role : Role.t
        ; assistant : Semantic_item.assistant_metadata option
        }
    | Call of
        { kind : Call.kind
        ; wire_name : string option
        }
    | Reasoning
    | Unknown of string

  type part_kind =
    | Text
    | Refusal
    | Reasoning_summary
    | Function_arguments
    | Custom_input
    | Opaque

  type payload =
    | Item_started of
        { item : item_key
        ; descriptor : descriptor
        ; metadata : Provider_metadata.t
        }
    | Part_started of
        { item : item_key
        ; part_index : int
        ; kind : part_kind
        }
    | Delta of
        { item : item_key
        ; part_index : int
        ; kind : part_kind
        ; bytes : string
        }
    | Item_finalized of
        { item : item_key
        ; payload : Item.t
        }
    | Usage of Usage.t
    | Terminal of Terminal.t

  type t =
    { attempt : Attempt_id.t
    ; source : Source_id.t
    ; payload : payload
    }

  (** Per-attempt keys correlate events, not host allocation. Existing turn loop
      folds/reconciles deltas, allocates/commits entries and admits tools. *)
end

module Auth : sig
  type owner_generation
  type lease
  type resolver

  type error =
    | Missing
    | Login_required
    | Denied
    | Renewal_failed of string

  val resolve : resolver -> sw:Eio.Switch.t -> target:Target.t -> (lease, error) Result.t
  val owner_generation : lease -> owner_generation

  (** Host temporary lease; no public secret or generation serializers. Silent
      renewal cannot start browser login. Invalidation never permits blind retry. *)
end

module Driver : sig
  type t
  type error = Auth of Auth.error

  (** Bounded synchronous delivery under caller Eio scope, one terminal only on
      [Ok] return. [Error (Auth _)] emits no events. Callback failures/cancellation
      propagate. No lazy stream escapes,
      tool executor, actor commit, hidden retry or provider-owned conversation.
      SSE/optional WS share this seam; OpenAI lowering requires store=false and
      complete fresh-context reconstruction. *)
  val run
    :  t
    -> sw:Eio.Switch.t
    -> auth:Auth.resolver
    -> prepared:Prepared_request.t
    -> on_event:(Event.t -> unit)
    -> (Terminal.t, error) Result.t
end
