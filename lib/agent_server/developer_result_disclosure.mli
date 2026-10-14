(** Host-only disclosure of persisted developer job results. The existing
    Developer_run_store and Blob_store remain the sole authority and byte store;
    this service owns no scheduler, publisher, durable document or Blob reader.
    An ID, selection, stored artifact descriptor or read request is never a
    bearer grant. *)
open! Core

module Selection : sig
  type t

  (** Pure comparison coordinates selected from the exact current capture by a
      developer job adapter, or from an explicitly authorized historical run.
      The capture digest is supplied rather than silently replaced with the
      run's current capture. Validate encoded owner/job IDs and nonnegative
      attempt. The Store later proves actual persisted membership. *)
  val create
    :  owner:Agent_protocol.Execution_owner.developer
    -> capture_fingerprint:Chatml_source.Digest.t
    -> job_id:Agent_protocol.Id.Job.t
    -> attempt:int
    -> (t, Agent_protocol.Error.t) result

  val owner : t -> Agent_protocol.Execution_owner.developer
  val capture_fingerprint : t -> Chatml_source.Digest.t
  val job_id : t -> Agent_protocol.Id.Job.t
  val attempt : t -> int
end

module Policy : sig
  type t

  (** Explicit trusted host/operator decision for this authenticated actor and
      selected persisted result. No existing generic protocol scope grants
      developer-result disclosure. The callback must be nonyielding; its
      unexpected exception/backtrace propagates unchanged. Cross-principal
      transfer of a request is rejected independently of this policy. *)
  val create : authorize:(Operator_authorization.t -> Selection.t -> bool) -> t

  val deny_all : t
end

module Limits : sig
  type t

  (** Pure bounded decoder policy. [max_result_bytes] is positive and no larger
      than the protocol's canonical Completion ceiling. [json] supplies the
      existing node/depth/field/byte limits; this constructor never inspects a
      Blob_store. [create] below checks the actual Blob upload/read ceiling. *)
  val create
    :  max_result_bytes:int
    -> json:Document_schema.Limits.t
    -> (t, Agent_protocol.Error.t) result
end

module Result : sig
  type t =
    | Pending of Agent_protocol.Job.status
    | Terminal_without_result of Agent_protocol.Job.status
    | Completion of Agent_protocol.Completion.t
  [@@deriving sexp_of]

  (** Pending is only Queued/Running/Waiting; Terminal_without_result is only
      Failed/Cancelled/Interrupted without an installed result. Success must
      have a result. An uncertain/corrupt read is an error, never one of these
      cases. The script adapter may render Pending as JSON null. *)
end

type t
type request

(** Check that [runs] is the exact physical developer Store bound to [blobs],
    and that the decoder ceiling fits Blob_store.max_upload_bytes. No new root
    or result capability is minted. *)
val create
  :  runs:Agent_store.Developer_run_store.t
  -> blobs:Agent_store.Blob_store.t
  -> policy:Policy.t
  -> limits:Limits.t
  -> (t, Agent_protocol.Error.t) result

(** Capture the original authenticated request actor, service issuer and
    selection. Check original currentness and policy now; repeat them at read.
    A historical context can be created after restart by a newly authenticated
    current actor. It does not require an old Root_owner token or an Active
    epoch. This request cannot transfer to another service or principal, and
    its creation proves no persisted job or result bytes. *)
val begin_request
  :  t
  -> original_actor:Operator_authorization.t
  -> Selection.t
  -> (request, Agent_protocol.Error.t) result

(** Read without waiting for job completion for one exact selection. Authenticate both the original
    actor retained in [request] and [current_actor], require principal
    continuity and the explicit policy for both. Borrow the issuing Store's
    existing Result_root, read the exact persisted host/run/epoch/principal/
    capture/job/attempt through [Result_root.find_job_snapshot], and validate
    its State. No root, job, descriptor or byte is accepted from the caller.

    An authentic nonterminal job yields Pending. A terminal failure/cancel/
    interruption without a result yields Terminal_without_result. For an
    artifact, use Developer_result_reference.of_terminal_job for the single
    canonical allowed_use comparison, then verify actual durable Blob target,
    creator, ID, media type, length, digest and bytes against the persisted
    descriptor. Use Blob_store.open_result/load_result_verified with that exact
    physical root; validate canonical bytes under [Limits.json] before
    Completion.of_json, and compare exact outcome and terminal disposition.
    Inline Completion receives the same persisted-state validation.

    All Store document reads use its configured byte/work/checkpoint limits;
    Blob reading and JSON decoding use the service's whole-result limits and
    preserve cancellation/exception identity. After any yielding Store/Blob/
    decoder work, recheck the same persisted job/attempt/result, check [sw],
    and make one final nonyielding original/current actor + policy decision
    immediately before returning. Changes or uncertainty reject; no raw Blob
    handle, bytes, mutable job, closure or native path crosses this boundary.
    A Blob call never occurs while the Store lease is held. *)
val read
  :  t
  -> request
  -> sw:Eio.Switch.t
  -> current_actor:Operator_authorization.t
  -> (Result.t, Agent_protocol.Error.t) result
