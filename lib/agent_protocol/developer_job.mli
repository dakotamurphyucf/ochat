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
      must be nonnegative. These are comparison coordinates; the actor proves a
      live run scope or active invocation separately. *)
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
