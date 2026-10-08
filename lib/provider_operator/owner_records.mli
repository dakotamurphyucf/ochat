open! Core
module P = Agent_protocol
module DTO = P.Provider_operator
module M = Credential_registry_model

(** Bounded durable nonsecret owner/reconciliation metadata. This store contains
    no credential, URL, challenge, code, provider body or refresh continuity. *)
module Error : sig
  type t =
    | Corrupt
    | Full
    | Conflict
    | Missing
    | Busy
    | Storage of Private_storage.Error.t
  [@@deriving sexp_of]
end

module Record : sig
  type t

  val create
    :  incarnation:M.Id.t
    -> owner:P.Id.Principal.t
    -> operation:M.Id.t
    -> binding:M.Id.t
    -> key:P.Idempotency_key.t
    -> mode:DTO.Login_mode.t
    -> flow:DTO.Flow_ref.t
    -> t

  val owner : t -> P.Id.Principal.t
  val operation : t -> M.Id.t
  val binding : t -> M.Id.t
  val key : t -> P.Idempotency_key.t
  val mode : t -> DTO.Login_mode.t
  val result : t -> DTO.Flow_result.t
end

type t

(** Borrows validated private directory. Every read/mutation acquires its own
    nonblocking metadata lock; never held across network, worker join or registry
    calls. Expected incarnation prevents reopening somebody else's owner records. *)
val create
  :  Private_storage.Directory.t
  -> incarnation:M.Id.t
  -> maximum_records:int
  -> (t, Error.t) Result.t

val list : t -> (Record.t list, Error.t) Result.t

(** Same owner/key returns original record only for identical mode/profile/binding.
    Persist before creating candidate/network work. No terminal record eviction
    while its receipt may need ownership reconciliation. *)
val begin_ : t -> Record.t -> (Record.t, Error.t) Result.t

val find : t -> DTO.Flow_ref.t -> (Record.t, Error.t) Result.t

val set_phase
  :  t
  -> DTO.Flow_ref.t
  -> phase:DTO.Flow_result.phase
  -> (Record.t, Error.t) Result.t

(** Terminal publication may wait for the metadata lease for a strictly positive
    monotonic budget of at most 60 seconds. Busy means admission exhausted; the
    transition executes once after admission and never retries ambiguous writes.
    Native work remains bounded and joined rather than falsely timed out. *)
val set_phase_wait
  :  t
  -> DTO.Flow_ref.t
  -> phase:DTO.Flow_result.phase
  -> clock:_ Eio.Time.Mono.t
  -> maximum_wait:Time_ns.Span.t
  -> (Record.t, Error.t) Result.t

(** Stable per-flow worker lease, separate from metadata lock. Acquire BEFORE
    record publication and retain until network/candidate cleanup is joined.
    Busy proves another local process still owns this acquisition. *)
val claim
  :  t
  -> DTO.Flow_ref.t
  -> sw:Eio.Switch.t
  -> (Private_storage.Lock.t, Error.t) Result.t

val is_live : t -> DTO.Flow_ref.t -> (bool, Error.t) Result.t

(** Explicit original-operation reconciliation only. Allows Interrupted or
    Failed Submission_uncertain to become a proven terminal outcome; never Pending.
    Ordinary set_phase is monotonic and refuses contradictory terminal results. *)
val reconcile_phase
  :  t
  -> DTO.Flow_ref.t
  -> phase:DTO.Flow_result.phase
  -> (Record.t, Error.t) Result.t
