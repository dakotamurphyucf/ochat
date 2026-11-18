(** Native parent-chain DATA and exact missing-to-present metadata proposals.
    Execution/current original/source remain actual Actor checks at writer. *)
type t = Policy_canonical_usage_carrier.t

type accounting = t

module Native_prefix = Policy_native_ancestry

(** True unrelatedness requires the exact native parent chain. Missing named
    rows, mismatched kind, delegated seed without Cause, or lost caused member
    rejects, including accounting=None. Current generation is exact. Recorded
    Event/Job execution-source observers must match; legitimately absent optional
    evidence stays absent. An Invocation observation observer is a deferred sink,
    retained exactly but checked only when claiming that notification. Causes
    record and require a full installation epoch. None proves unrelated ancestry, never
    current installation, original permission or execution authority. No complete
    Context is constructed for an actual unrelated root. *)
val cause
  :  accounting option
  -> prefix:Native_prefix.t
  -> member:Policy_cause_data.Member.t
  -> (Policy_cause_data.t option, Agent_protocol.Error.t) result

(** The callback returns the complete Context of SAME immutable prefix; force
    once only for known caused registration. It is not an authority callback. *)
val invocation_birth
  :  accounting option
  -> prefix:Native_prefix.t
  -> before:(unit -> (Policy_cause_data.Context.t, Agent_protocol.Error.t) result)
  -> Agent_protocol.Invocation.t
  -> (Policy_canonical_usage_update.Update.t option, Agent_protocol.Error.t) result

val model_job_birth
  :  accounting option
  -> prefix:Native_prefix.t
  -> before:(unit -> (Policy_cause_data.Context.t, Agent_protocol.Error.t) result)
  -> parent:Policy_cause_data.Member.t option
  -> Agent_protocol.Job.t
  -> (Policy_canonical_usage_update.Update.t option, Agent_protocol.Error.t) result

val async_job_birth
  :  accounting option
  -> prefix:Native_prefix.t
  -> before:(unit -> (Policy_cause_data.Context.t, Agent_protocol.Error.t) result)
  -> Agent_protocol.Job.t
  -> (Policy_canonical_usage_update.Update.t option, Agent_protocol.Error.t) result

val job_event_birth
  :  accounting option
  -> prefix:Native_prefix.t
  -> before:(unit -> (Policy_cause_data.Context.t, Agent_protocol.Error.t) result)
  -> Agent_protocol.Moderator_execution.t
  -> (Policy_canonical_usage_update.Update.t option, Agent_protocol.Error.t) result

(** Queue occurrence birth is separate from Callback_job and requires an actual
    captured occurrence. Nonstructural legacy/manual receipts retain their
    existing native family framing; callers do not guess a queue producer.
    Exact retained terminal ancestry preserves its prior Cause without making
    the old Job live. Explicit unknown producer evidence, missing terminal
    evidence, absent accounting or absent canonical Job membership may record
    a factual claim without metadata. Present contradictory or partial canonical
    evidence still rejects. Unavailable captured ancestry never means unrelatedness,
    and later new-work queries still reject. *)
val queued_event_birth
  :  accounting option
  -> prefix:Native_prefix.t
  -> before:(unit -> (Policy_cause_data.Context.t, Agent_protocol.Error.t) result)
  -> Agent_protocol.Moderator_execution.t
  -> (Policy_canonical_usage_update.Update.t option, Agent_protocol.Error.t) result

module Standalone : sig
  module Boundary : sig
    type t =
      | Event of
          { event : Agent_protocol.Moderator_execution.t
          ; descriptor : Policy_standalone_source.t
          }
      | Invocation of
          { invocation : Agent_protocol.Invocation.t
          ; descriptor : Policy_standalone_source.t
          }
      | Job of
          { job : Agent_protocol.Job.t
          ; descriptor : Policy_standalone_source.t
          }
    (* Supplied only from SAME activated claim, actual executing Invocation/Job,
       observation continuation or claimed queue borrow. No ID-only lookup, public marker or guessed PC=None path.
       Exact full native row, descriptor, source and current prefix validate
       BEFORE a matching ancestry boundary can terminate. A positive native
       boundary REQUIRES descriptor.source.installation=Some actual full source
       equal prefix.current_source. Observer-only descriptors remain factual
       Caller association and cannot select this branch. Public/native row and
       execution-source validation is unchanged; no legacy malformed-hash exception.
       An Invocation deferred observation sink need not equal its execution source. *)
  end

  module Resolution : sig
    type t =
      | Unrelated
      | Standalone_unrelated of Policy_standalone_source.t
      | Caused of Policy_cause_data.t
  end

  val resolve
    :  Policy_canonical_usage_carrier.t option
    -> prefix:Policy_native_ancestry.t
    -> member:Policy_cause_data.Member.t
    -> boundary:Boundary.t
    -> (Resolution.t, Agent_protocol.Error.t) result

  module Birth : sig
    type t

    val update : t -> Policy_canonical_usage_update.Update.t option
    val classification : t -> Policy_standalone_source.Classification.t
  end

  val invocation_birth
    :  Policy_canonical_usage_carrier.t option
    -> prefix:Policy_native_ancestry.t
    -> before:(unit -> (Policy_cause_data.Context.t, Agent_protocol.Error.t) result)
    -> boundary:Boundary.t
    -> Agent_protocol.Invocation.t
    -> (Birth.t, Agent_protocol.Error.t) result

  val model_job_birth
    :  Policy_canonical_usage_carrier.t option
    -> prefix:Policy_native_ancestry.t
    -> before:(unit -> (Policy_cause_data.Context.t, Agent_protocol.Error.t) result)
    -> parent:Policy_cause_data.Member.t option
    -> boundary:Boundary.t
    -> Agent_protocol.Job.t
    -> (Birth.t, Agent_protocol.Error.t) result

  val async_job_birth
    :  Policy_canonical_usage_carrier.t option
    -> prefix:Policy_native_ancestry.t
    -> before:(unit -> (Policy_cause_data.Context.t, Agent_protocol.Error.t) result)
    -> boundary:Boundary.t
    -> Agent_protocol.Job.t
    -> (Birth.t, Agent_protocol.Error.t) result

  val job_event_birth
    :  Policy_canonical_usage_carrier.t option
    -> prefix:Policy_native_ancestry.t
    -> before:(unit -> (Policy_cause_data.Context.t, Agent_protocol.Error.t) result)
    -> boundary:Boundary.t
    -> Agent_protocol.Moderator_execution.t
    -> (Birth.t, Agent_protocol.Error.t) result
  (* These use the ONE existing native ancestry walk and registration/projector.
     Reaching the supplied exact boundary produces a descriptor for the NEW
     subject; an unrelated other root returns Unknown, never a positive fact.
     Actual caused ancestry remains Caused and cannot be erased by a boundary.
     Registration alone forces the existing lazy complete Context. *)
end
