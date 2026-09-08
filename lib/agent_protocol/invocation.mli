(** Versioned tool invocation records. These pure transitions do not grant
    authority: the actor must admit the caller and validate referenced work
    ownership before committing a resolution. *)

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
[@@deriving compare, sexp]

type tool_error =
  { code : string
  ; message : string
  ; retryable : bool
  ; details : Jsonaf.t
  }
[@@deriving sexp]

type outcome =
  | Complete of Jsonaf.t
  | Pending of work * Jsonaf.t
  | Fail of tool_error
  | Cancelled of string
[@@deriving sexp]

type context =
  { id : Id.Invocation.t
  ; session_id : Id.Session.t
  ; generation : int
  ; origin : origin
  ; provider_call_id : string option
  ; parent_invocation : Id.Invocation.t option
  ; parent_job : Id.Job.t option
  ; tool_name : string
  ; implementation_revision : string
  ; capability_fingerprint : string
  ; input : Jsonaf.t
  ; created_at : Timestamp.t
  ; deadline : Timestamp.t option
  }
[@@deriving sexp]

type status =
  | Admitted
  | Dispatching
  | Resolved of outcome
  | Published of outcome
[@@deriving sexp]

type t = private
  { context : context
  ; status : status
  }
[@@deriving sexp]

val create : context -> (t, Error.t) result
val validate : t -> (unit, Error.t) result
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

(** Checks a proposed durable replacement, including immutable context and
    outcome. New records must be admitted; transitions cannot skip dispatch
    except for host cancellation. *)
val validate_transition : previous:t option -> t -> (unit, Error.t) result

val outcome_to_json : outcome -> Jsonaf.t
val outcome_of_json : Jsonaf.t -> (outcome, Error.t) result
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
