open! Core

(** The actor's one process-local inventory of genuinely admitted Job loans.
    Missing rows are unavailable; a present Admission may contain genuine legacy
    None. This module has no codec and cannot restore credentials from Jobs. *)
type t

module Moderator_binding : sig
  (** Physical topology captured by the real creator. Source-only construction
      does not lend a Manager; later resource handoff needs its actual owner.
      No captured moderator does not prevent ordinary Model execution. A captured
      launch observer must match the Captured source exactly. *)
  type t =
    | No_captured_moderator
    | Captured of
        { source : Agent_protocol.Run_source.t
        ; manager : Chat_response.Moderator_manager.t option
        }
end

module Row : sig
  type t

  (** Terminal custody cannot be reused by active execution. *)
  val admission : t -> (Execution_custody.Admission.t, Agent_protocol.Error.t) result

  val binding : t -> Moderator_binding.t
end

val empty : t
val is_empty : t -> bool
val retire : t -> unit

(** Exact persisted current Job, including its acknowledged attempt. A row for
    the same ID with contradictory identity or phase is an error, never absence.
    The actor must separately check physical scope and current authorization. *)
val find : t -> job:Agent_protocol.Job.t -> (Row.t option, Agent_protocol.Error.t) result

module Fresh : sig
  (** An independent speculative loan with no inventory basis. Keep it in the
      existing staged Job entry while other actor commits may advance inventory. *)
  type t

  val stage
    :  job:Agent_protocol.Job.t
    -> binding:Moderator_binding.t
    -> parent:Execution_custody.Admission.t
    -> (t, Agent_protocol.Error.t) result

  val binding : t -> Moderator_binding.t

  (** Pre-ACK cancellation/unselection closes the speculative loan. Once an
      actual Update install acknowledges it, later outer cleanup is a no-op. *)
  val discard : t -> unit
end

module Update : sig
  type inventory := t
  type t

  (** Before persistence, require the exact inventory used to prepare this update.
      This checks process-local ownership, not execution authority. *)
  val check_basis : t -> current:inventory -> (unit, Agent_protocol.Error.t) result

  (** Final nonyielding check immediately before persistence: repeats the basis,
      exact selected record, staged phase and original Admission checks after any
      awaited host policy/capacity work. Pure claim/retry/defer plans have no new
      Fresh loan and do not reject truthful state facts merely on expiry. *)
  val check_before_commit
    :  t
    -> current:inventory
    -> (unit, Agent_protocol.Error.t) result

  (** Close only newly transferred speculative loans on pre-ACK failure. Claim
      and retry updates share the original Admission and own no new loan. *)
  val discard : t -> unit

  (** Nonyielding, infallible installation after the real ACK and basis check.
      Removed loans retire; a preserved Admission survives row replacement.
      Terminal children are installed before their old active loans retire. *)
  val install : t -> inventory
end

(** At the genuine owner commit, prepare ONE candidate against current inventory
    for all selected still-Queued Jobs. Exact staged records, current loans and
    duplicate IDs validate before persistence. Cancelled/unselected tickets must
    not be supplied. Failure leaves the caller owning every Fresh loan. *)
val prepare_selected
  :  t
  -> selected:(Agent_protocol.Job.t * Fresh.t) list
  -> (Update.t, Agent_protocol.Error.t) result

(** Stage a fresh queued attempt zero with Pending/Not_required delivery and one
    independent loan from an actual
    parent Admission. The actor owns cleanup until its genuine creation ACK.
    Binding values/Job DTOs alone grant no authority. Duplicate IDs reject. *)
val prepare_fresh
  :  t
  -> job:Agent_protocol.Job.t
  -> binding:Moderator_binding.t
  -> parent:Execution_custody.Admission.t
  -> (Update.t, Agent_protocol.Error.t) result

(** Pure phase updates for the genuine claim/retry owners only. They validate the
    exact previous Job and immutable creation. Claim advances one attempt from
    Queued to Running; retry returns that same attempt to Queued. Neither lends
    authority, and missing historical rows remain missing. *)
val prepare_claim
  :  t
  -> previous:Agent_protocol.Job.t
  -> claimed:Agent_protocol.Job.t
  -> (Update.t, Agent_protocol.Error.t) result

val prepare_retry
  :  t
  -> previous:Agent_protocol.Job.t
  -> queued:Agent_protocol.Job.t
  -> (Update.t, Agent_protocol.Error.t) result

(** The actual defer owner preserves one Running attempt while entering
    Waiting_completion. The actor independently validates the real dependency;
    this pure plan grants neither new work nor dependency ownership. *)
val prepare_defer
  :  t
  -> previous:Agent_protocol.Job.t
  -> waiting:Agent_protocol.Job.t
  -> (Update.t, Agent_protocol.Error.t) result

(** Actual initialization/preparation owners only: bind their exact acknowledged
    constructor subset to its first source or first physical Manager. Existing
    admissions/continuations are preserved without renewal. Missing rows remain
    unavailable and skip (completed Not_required work legitimately has no row).
    Contradictory present rows and foreign captured sources/Managers reject.
    None means there is no binding change and no custody ACK is needed. *)
val prepare_constructor_handoff
  :  t
  -> jobs:Agent_protocol.Job.t list
  -> binding:Moderator_binding.t
  -> (Update.t option, Agent_protocol.Error.t) result

(** Negative-only resource reconciliation for the intended final Manager of one
    actual replacement. Captured physical loans survive only that exact resource;
    source-only/no-moderator rows keep their original topology, never gain it.
    Reconcile once against the final resource, not an intermediate cleared slot.
    Actual Actor setters install/retire the returned candidate nonyieldingly. *)
val retain_runtime : t -> manager:Chat_response.Moderator_manager.t option -> t

(** Actual committed administrative Stop owners retire every live phase,
    including unchanged Graceful Jobs and Jobs admitted while already stopped.
    This plan has no speculative children and changes no durable Job facts. *)
val prepare_retirement : t -> Update.t

(** Genuine terminal/control owners supply exact previous and final persisted
    records, after artifact normalization. Each Job transfers its own Admission
    to a completed-fact Continuation; missing historical rows stay missing.
    Known expiry never prevents truthful terminal persistence. The Update owns
    speculative children until ACK; discard preserves all prior loans. *)
val prepare_terminals
  :  t
  -> completed:(Agent_protocol.Job.t * Agent_protocol.Job.t) list
  -> (Update.t, Agent_protocol.Error.t) result

(** Actual completed delivery owners only: stage an independent completed-fact
    continuation from the exact terminal record. Known expiry stays explicit;
    missing inventory stays unavailable. A present row must retain this exact
    full source and physical Manager; source-only/no-moderator/foreign bindings
    reject rather than gain a resource. Actor proves installed currentness and
    producer facts separately. Caller owns Current until queue ACK or retirement. *)
val transfer_delivery
  :  t
  -> manager:Chat_response.Moderator_manager.t
  -> source:Agent_protocol.Run_source.t
  -> job:Agent_protocol.Job.t
  -> (Execution_custody.Continuation.t option, Agent_protocol.Error.t) result

(** Negative-only reconciliation at ordinary state installation. Keep only exact
    existing active Jobs and already-planned terminal Pending rows. Changes,
    supplied terminal DTOs and removal retire;
    this path cannot promote a supplied status into a new loan. *)
val retain : t -> jobs:Agent_protocol.Job.t list -> t

val install_retained : t -> incoming:t -> t
