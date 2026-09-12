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
