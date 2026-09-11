# Protocol types and codec reference

Generated from the current public interfaces. Do not hand-edit the excerpts.
The [protocol guide](protocol.md) explains operation semantics and authorization.
Each section includes the complete typed contract and a link to its JSON codec;
wire tags/defaults are defined by that codec, not OCaml constructor spelling.

## audit

[JSON codec](../../lib/agent_protocol/audit.ml) · [interface](../../lib/agent_protocol/audit.mli)

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

## authoring_guidance

[JSON codec](../../lib/agent_protocol/authoring_guidance.ml) · [interface](../../lib/agent_protocol/authoring_guidance.mli)

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

val validate : t -> (unit, Error.t) result
val matches_payload : t -> Jsonaf.t -> bool
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
```

## blob

[JSON codec](../../lib/agent_protocol/blob.ml) · [interface](../../lib/agent_protocol/blob.mli)

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
    used by duplex transports that cannot use the HTTP streaming route. *)
module Read_request : sig
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
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

[JSON codec](../../lib/agent_protocol/command.ml) · [interface](../../lib/agent_protocol/command.mli)

```ocaml
(** Typed method dispatch for transport-neutral protocol requests. *)

type t =
  | Protocol_initialize of Initialize.Request.t
  | Protocol_ping of Ping.Request.t
  | Server_info
  | Server_health of Health.Request.t
  | Prompt_list of Prompt.List_request.t
  | Prompt_get of Prompt.Get_request.t
  | Workspace_list of Workspace.List_request.t
  | Workspace_get of Workspace.Get_request.t
  | Blob_read of Blob.Read_request.t
  | Session_create of Session.Create_request.t
  | Session_list of Session.List_request.t
  | Session_get of Session.Get_request.t
  | Session_attach of Session.Attach_request.t
  | Session_detach of Session.Detach_request.t
  | Session_renew_owner of Session.Renew_owner_request.t
  | Session_start of Session.Start_request.t
  | Session_stop of Session.Stop_request.t
  | Session_cancel_operation of Session.Cancel_operation_request.t
  | Session_send_message of Session.Send_message_request.t
  | Session_compact of Session.Compact_request.t
  | Session_delete_history of Session.Delete_history_request.t
  | Session_export of Session.Export_request.t
  | Session_reset of Session.Reset_request.t
  | Session_rebuild of Session.Rebuild_request.t
  | Session_upgrade_prompt of Session.Upgrade_prompt_request.t
  | Session_delete of Session.Delete_request.t
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
[@@deriving sexp]

(** [method_name t] returns the stable protocol method. *)
val method_name : t -> string

(** [params t] encodes the command parameters. *)
val params : t -> Jsonaf.t

(** [of_method_and_params ~method_ ~params] performs closed typed dispatch. *)
val of_method_and_params : method_:string -> params:Jsonaf.t -> (t, Error.t) result

(** [supported_methods] contains every method accepted by the closed dispatcher. *)
val supported_methods : string list
```

## completion

[JSON codec](../../lib/agent_protocol/completion.ml) · [interface](../../lib/agent_protocol/completion.mli)

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

[JSON codec](../../lib/agent_protocol/completion_contract.ml) · [interface](../../lib/agent_protocol/completion_contract.mli)

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

[JSON codec](../../lib/agent_protocol/completion_projection.ml) · [interface](../../lib/agent_protocol/completion_projection.mli)

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

[JSON codec](../../lib/agent_protocol/delivery.ml) · [interface](../../lib/agent_protocol/delivery.mli)

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
type ownership =
  { source : Invocation.observer
  ; creator : Job.launch_owner
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
```

## envelope

[JSON codec](../../lib/agent_protocol/envelope.ml) · [interface](../../lib/agent_protocol/envelope.mli)

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

[JSON codec](../../lib/agent_protocol/error.ml) · [interface](../../lib/agent_protocol/error.mli)

```ocaml
include module type of Protocol_error
```

## event

[JSON codec](../../lib/agent_protocol/event.ml) · [interface](../../lib/agent_protocol/event.mli)

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
  type kind =
    | Provider_stream
    | Sourced_stream
    | History_correlated_stream
    | Tool_started
    | Tool_progress
    | Tool_trace
    | Tool_finished
    | Agent_call_classified
    | Agent_call_progress
    | Activity
    | Compaction_progress
  [@@deriving compare, equal, sexp]

  type t =
    { session_id : Id.Session.t
    ; operation_id : Id.Operation.t
    ; operation_sequence : int64
    ; anchor_sequence : int64
    ; timestamp : Timestamp.t
    ; kind : kind
    ; payload : Jsonaf.t
    }
  [@@deriving sexp]

  (** [to_json t] encodes the parameters of a [session.live_event] notification. *)
  val to_json : t -> Jsonaf.t

  (** [of_json json] decodes a recoverable live event. *)
  val of_json : Jsonaf.t -> (t, Error.t) result

  (** [to_notification t] wraps the live event in a JSON-RPC notification. *)
  val to_notification : t -> Envelope.t
end
```

## extension_capabilities

[JSON codec](../../lib/agent_protocol/extension_capabilities.ml) · [interface](../../lib/agent_protocol/extension_capabilities.mli)

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

[JSON codec](../../lib/agent_protocol/extension_status.ml) · [interface](../../lib/agent_protocol/extension_status.mli)

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

[JSON codec](../../lib/agent_protocol/grant.ml) · [interface](../../lib/agent_protocol/grant.mli)

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

[JSON codec](../../lib/agent_protocol/health.ml) · [interface](../../lib/agent_protocol/health.mli)

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

[JSON codec](../../lib/agent_protocol/history.ml) · [interface](../../lib/agent_protocol/history.mli)

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

type provenance =
  | Canonical
  | Moderator_inserted
  | Moderator_replaced of Id.t
  | Runtime_notification of delivery_id
  | Runtime_authoring of Authoring_guidance.t
[@@deriving equal, sexp]

type entry =
  { id : Id.t
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

## id

[JSON codec](../../lib/agent_protocol/id.ml) · [interface](../../lib/agent_protocol/id.mli)

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
```

## idempotency_key

[JSON codec](../../lib/agent_protocol/idempotency_key.ml) · [interface](../../lib/agent_protocol/idempotency_key.mli)

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

## ingress

[JSON codec](../../lib/agent_protocol/ingress.ml) · [interface](../../lib/agent_protocol/ingress.mli)

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

[JSON codec](../../lib/agent_protocol/initialize.ml) · [interface](../../lib/agent_protocol/initialize.mli)

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

[JSON codec](../../lib/agent_protocol/invocation.ml) · [interface](../../lib/agent_protocol/invocation.mli)

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
  :  t
  -> session_id:Id.Session.t
  -> generation:int
  -> outcome
  -> (t, Error.t) result

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
```

## job

[JSON codec](../../lib/agent_protocol/job.ml) · [interface](../../lib/agent_protocol/job.mli)

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

[JSON codec](../../lib/agent_protocol/job_artifact.ml) · [interface](../../lib/agent_protocol/job_artifact.mli)

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

[JSON codec](../../lib/agent_protocol/job_progress.ml) · [interface](../../lib/agent_protocol/job_progress.mli)

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

[JSON codec](../../lib/agent_protocol/job_result_reference.ml) · [interface](../../lib/agent_protocol/job_result_reference.mli)

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
```

## json_codec

[JSON codec](../../lib/agent_protocol/json_codec.ml) · [interface](../../lib/agent_protocol/json_codec.mli)

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

[JSON codec](../../lib/agent_protocol/method_result.ml) · [interface](../../lib/agent_protocol/method_result.mli)

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
  | Protocol_initialize of Initialize.Response.t
  | Protocol_ping of Ping.Response.t
  | Server_info of Server_info.t
  | Server_health of Health.Response.t
  | Prompt_list of Prompt.t Page.t
  | Prompt_get of Prompt.t
  | Workspace_list of Workspace.t Page.t
  | Workspace_get of Workspace.t
  | Blob_read of Blob.Chunk.t
  | Session_create of Create.t
  | Session_list of Session.t Page.t
  | Session_get of Snapshot.t
  | Session_attach of Attach.t
  | Session_detach of Mutation_result.t
  | Session_renew_owner of Session.Owner_lease.t * Mutation_result.t
  | Session_start of Session_mutation.t
  | Session_stop of Session_mutation.t
  | Session_cancel_operation of Session_mutation.t
  | Session_send_message of Send_message.t
  | Session_compact of Session_mutation.t
  | Session_delete_history of Session_mutation.t
  | Session_export of Export.t
  | Session_reset of Session_mutation.t
  | Session_rebuild of Session_mutation.t
  | Session_upgrade_prompt of Session_mutation.t
  | Session_delete of Delete.t
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
[@@deriving sexp]

(** [method_name t] returns the request method associated with [t]. *)
val method_name : t -> string

(** [to_json t] encodes the method-specific result object. *)
val to_json : t -> Jsonaf.t

(** [of_json ~method_ json] decodes the successful result for [method_]. *)
val of_json : method_:string -> Jsonaf.t -> (t, Error.t) result

(** [supported_methods] contains every method with a typed success decoder. *)
val supported_methods : string list
```

## moderator_execution

[JSON codec](../../lib/agent_protocol/moderator_execution.ml) · [interface](../../lib/agent_protocol/moderator_execution.mli)

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

type t = private
  { context : context
  ; status : status
  ; requests : Invocation.follow_up option
  ; intent : intent option
  ; compaction_operation_id : Id.Operation.t option
    (** Retained after application/discard for provenance and recovery checks. *)
  ; retirement : retirement option [@sexp.option]
  }
[@@deriving equal, sexp]

val create : context -> (t, Error.t) result
val validate : t -> (unit, Error.t) result

val complete
  :  t
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

[JSON codec](../../lib/agent_protocol/mutation_result.ml) · [interface](../../lib/agent_protocol/mutation_result.mli)

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

[JSON codec](../../lib/agent_protocol/operation.ml) · [interface](../../lib/agent_protocol/operation.mli)

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

## page

[JSON codec](../../lib/agent_protocol/page.ml) · [interface](../../lib/agent_protocol/page.mli)

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

## permission

[JSON codec](../../lib/agent_protocol/permission.ml) · [interface](../../lib/agent_protocol/permission.mli)

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

[JSON codec](../../lib/agent_protocol/ping.ml) · [interface](../../lib/agent_protocol/ping.mli)

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

[JSON codec](../../lib/agent_protocol/principal.ml) · [interface](../../lib/agent_protocol/principal.mli)

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

## prompt

[JSON codec](../../lib/agent_protocol/prompt.ml) · [interface](../../lib/agent_protocol/prompt.mli)

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

## protocol_error

[JSON codec](../../lib/agent_protocol/protocol_error.ml) · [interface](../../lib/agent_protocol/protocol_error.mli)

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

## schedule

[JSON codec](../../lib/agent_protocol/schedule.ml) · [interface](../../lib/agent_protocol/schedule.mli)

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
```

## scope

[JSON codec](../../lib/agent_protocol/scope.ml) · [interface](../../lib/agent_protocol/scope.mli)

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

## session

[JSON codec](../../lib/agent_protocol/session.ml) · [interface](../../lib/agent_protocol/session.mli)

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
  val of_json : Jsonaf.t -> (t, Error.t) result
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
  ; latest_event_sequence : int64
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
    ; labels : (string * string) list
    }
  [@@deriving sexp]

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

module Export_request : sig
  type format =
    | Chatmd
    | Json
  [@@deriving compare, equal, sexp]

  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
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

## snapshot

[JSON codec](../../lib/agent_protocol/snapshot.ml) · [interface](../../lib/agent_protocol/snapshot.mli)

```ocaml
(** Principal-projected session state used by all connected clients. *)

type t =
  { session : Session.t
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

[JSON codec](../../lib/agent_protocol/stored_completion.ml) · [interface](../../lib/agent_protocol/stored_completion.mli)

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

## subscription

[JSON codec](../../lib/agent_protocol/subscription.ml) · [interface](../../lib/agent_protocol/subscription.mli)

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
```

## timestamp

[JSON codec](../../lib/agent_protocol/timestamp.ml) · [interface](../../lib/agent_protocol/timestamp.mli)

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

[JSON codec](../../lib/agent_protocol/version.ml) · [interface](../../lib/agent_protocol/version.mli)

```ocaml
(** Ochat agent protocol versions and feature negotiation. *)

type t =
  { major : int
  ; minor : int
  }
[@@deriving compare, equal, sexp]

(** [initial] is the initial Ochat agent protocol version, [1.0]. *)
val initial : t

(** [current] is protocol [1.1], adding scoped ingress submission. Servers retain
    [1.0] negotiation without exposing the new closed scope variant to old clients. *)
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

[JSON codec](../../lib/agent_protocol/workspace.ml) · [interface](../../lib/agent_protocol/workspace.mli)

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
