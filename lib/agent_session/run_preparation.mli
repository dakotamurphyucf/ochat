open! Core

(** Ephemeral actor-issued admission custody. No serialization or executable
    authority. The actor keeps its [Owner.t] private; callers cannot construct a
    preparation for that owner from session IDs, revisions or receipt data.
    All operations run in that actor's serialized mailbox. *)
module Owner : sig
  type t

  val create : unit -> t
end

type t

(** Issuance validates the original CAS, principal and digest against the actual
    actor state. [authorize] is non-yielding current original host policy. *)
val create
  :  owner:Owner.t
  -> state:Session_state.t
  -> principal_id:Agent_protocol.Id.Principal.t
  -> request:Agent_protocol.Run_start.t
  -> request_sha256:string
  -> authorize:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> (t, Agent_protocol.Error.t) result

(** Internal normalized request under the same private actor owner. Headless
    data alone cannot create custody for that owner. The actor requires actual
    command authorization before issuing this preparation. *)
val create_request
  :  owner:Owner.t
  -> state:Session_state.t
  -> principal_id:Agent_protocol.Id.Principal.t
  -> request:Run_admission_request.t
  -> request_sha256:string
  -> authorize:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> (t, Agent_protocol.Error.t) result

(** Same real actor-owner custody with the original authenticated object. No
    principal is supplied separately. The normal actor admission still checks
    current writer/Run policy and installs only its own private issuer. *)
val create_authenticated_request
  :  owner:Owner.t
  -> state:Session_state.t
  -> actor:Operator_authorization.t
  -> request:Run_admission_request.t
  -> request_sha256:string
  -> authorize:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> (t, Agent_protocol.Error.t) result

(** Exact resource identity; not equality of durable session IDs. *)
val belongs_to : t -> owner:Owner.t -> bool

val request : t -> Run_admission_request.t
val principal_id : t -> Agent_protocol.Id.Principal.t
val original_actor : t -> Operator_authorization.t option
val request_sha256 : t -> string

(** Checks the exact issuing actor, current original authorization and tracked
    revision/generation. Closed/invalidated/foreign preparations reject. *)
val check
  :  t
  -> owner:Owner.t
  -> state:Session_state.t
  -> (unit, Agent_protocol.Error.t) result

(** Callback-free ownership and current revision/generation check. This does not
    run original authorization and grants no effect permission. The actor uses it
    only after selecting an exact already-owned result ACK, or after all yielding
    admission policy checks before a fresh effect. Closed or displaced ownership
    rejects. [check] remains required for new construction/admission authority. *)
val check_basis
  :  t
  -> owner:Owner.t
  -> state:Session_state.t
  -> (unit, Agent_protocol.Error.t) result

(** Nonyielding exact issuing owner/revision/generation and original-proof
    currentness. Does not invoke host policy; use after its separate refresh. *)
val check_current
  :  t
  -> owner:Owner.t
  -> state:Session_state.t
  -> (unit, Agent_protocol.Error.t) result

module Authorization : sig
  (** Actual authenticated policy captured under the issuing private owner.
      Independent of the preparation's revision tracker after legitimate handoff;
      the original and host policy remain mandatory for new execution. *)
  type t

  val actor : t -> Operator_authorization.t
  val check : t -> Session_state.t -> (unit, Agent_protocol.Error.t) result
end

module Custody : sig
  (** Shared process-local owner, original authorization, acknowledged session
      revision and actual constructor Job subset. This contains no admission
      request, Run binding, source selection or deferred eligibility. The same
      issuing Actor serializes every operation. No constructor is exposed. *)
  type t

  val belongs_to : t -> owner:Owner.t -> bool
  val principal_id : t -> Agent_protocol.Id.Principal.t
  val original_actor : t -> Operator_authorization.t option

  (** Exact owner/current session revision and original host policy. This alone
      does not validate Run/source/Goal associations or grant execution. *)
  val check
    :  t
    -> owner:Owner.t
    -> state:Session_state.t
    -> (unit, Agent_protocol.Error.t) result

  (** Original-proof currentness after callback-free custody preflight. No host
      policy callback; the caller must refresh it separately for new effects. *)
  val check_current
    :  t
    -> owner:Owner.t
    -> state:Session_state.t
    -> (unit, Agent_protocol.Error.t) result

  (** Callback-free owner/session/generation/revision check. Closed custody
      rejects; this does not authorize new effects or factual writes by itself. *)
  val check_basis
    :  t
    -> owner:Owner.t
    -> state:Session_state.t
    -> (unit, Agent_protocol.Error.t) result

  val capture_authorization
    :  t
    -> owner:Owner.t
    -> state:Session_state.t
    -> (Authorization.t, Agent_protocol.Error.t) result

  (** Only after the exact durable owned session ACK. Advances precisely one
      revision and closes/clears the constructor subset on failure. The one
      acknowledged revision is also the attached INITIAL deferred attempt's
      factual basis and survives closure; common callers cannot leave a second
      revision tracker stale. No source, request CAS or Run binding is rebased. *)
  val advance
    :  t
    -> owner:Owner.t
    -> previous:Session_state.t
    -> current:Session_state.t
    -> (unit, Agent_protocol.Error.t) result

  (** Idempotent irreversible release. Clears the actual constructor subset,
      retaining the last acknowledged revision for existing initial historical
      evidence. It cannot issue deferred eligibility or a source completion. *)
  val close : t -> unit

  module Constructor_jobs : sig
    type custody := t

    (** The exact shared constructor subset plan. Issuance requires the private
        Actor owner/current custody and fresh actual Job, or its selected owned
        checkpoint. The initial adapter below uses this SAME implementation. *)
    type t

    val prepare_add
      :  custody
      -> owner:Owner.t
      -> state:Session_state.t
      -> job:Agent_protocol.Job.t
      -> (t, Agent_protocol.Error.t) result

    val prepare_handoff
      :  custody
      -> owner:Owner.t
      -> state:Session_state.t
      -> (Agent_protocol.Id.Job.t list * t, Agent_protocol.Error.t) result

    val check_before_commit
      :  t
      -> owner:Owner.t
      -> state:Session_state.t
      -> (unit, Agent_protocol.Error.t) result

    (** Install only after the exact Job/checkpoint ACK and custody advance.
        Already-closed custody stays closed and receives no subset update. *)
    val acknowledge : t -> unit
  end
end

(** Projection of the SAME physical custody, not a new owner or conversion to a
    different admission family. Initial request/digest/deferred evidence remains
    attached only to this initial preparation. *)
val custody : t -> Custody.t

val capture_authorization
  :  t
  -> owner:Owner.t
  -> state:Session_state.t
  -> (Authorization.t, Agent_protocol.Error.t) result

(** Called only after a durable commit carrying this exact preparation. Advances
    one acknowledged revision; a different or skipped basis invalidates custody.
    Ordinary commits close the actor's live preparation instead. *)
val advance
  :  t
  -> owner:Owner.t
  -> previous:Session_state.t
  -> current:Session_state.t
  -> (unit, Agent_protocol.Error.t) result

(** Idempotent release on all normal/error/cancellation/uncertain-write exits. *)
val close : t -> unit

module Source_completion : sig
  type preparation := t

  (** The actual Factory's completion of one Actor-issued closed deferred
      attempt. Both private owners are required. This is immutable source-lifetime
      evidence, not a no-effects observation, retry grant or execution scope.
      The fixed Factory CPS issues it only after successful cleanup of every
      actually acquired target and parent lease. A never-acquired branch is
      distinct from a missing required cleanup result. No decoder exists. *)
  type t

  val preparation : t -> preparation

  (** Callback-free association with the issuing Actor's private owner, closed
      attempt and actual current generation. The Actor separately authenticates
      its live Store/writer and exact retained request absence. This never runs
      original effect authorization or reopens a preparation. *)
  val check_basis
    :  t
    -> owner:Owner.t
    -> state:Session_state.t
    -> (unit, Agent_protocol.Error.t) result
end

module Closed_attempt : sig
  (** Factual closure by the actual Actor's existing private Owner. It retains
      exact request/principal/digest, source issuer identity and irreversible
      exclusion/write disposition. Logical closure alone proves no resource join
      or no-effects outcome. No constructor or decoder is exposed. *)
  type t

  (** The actual Factory retains [source_owner] privately and invokes this only
      after its fixed outer source CPS has successfully joined every acquired
      target and parent lease. A fresh/foreign issuer, excluded attempt or unknown
      writer rejects. Neither a Boolean nor a caller cleanup callback can replace
      that issuer. Belongs to the issuing Eio domain. *)
  val complete
    :  t
    -> source_owner:Runtime_source_owner.t
    -> (Source_completion.t, Agent_protocol.Error.t) result
end

module Deferred : sig
  type preparation := t

  (** Provenance attached when the actual Actor issues a NEW preparation. An
      ordinary preparation cannot be upgraded. The existing preparation remains
      the sole admission owner; this cell adds no registry or execution authority.
      Exclusion is irreversible, including after logical closure. *)
  type t

  module Exclusion : sig
    type t =
      | Ordinary_execution
      | Admission_attempted
      | Activation_consumed
      | Uncertain_write
      | Displaced
    [@@deriving equal, sexp_of]
  end

  (** Called only while issuing a fresh preparation under the Actor's existing
      private Owner and its captured Factory identity. Original authenticated
      request/current policy and CAS are checked as in ordinary issuance. The
      Actor/host also establish their actual quiescent execution/source lifetime;
      this constructor is not evidence of a Runtime_owner resource lease. *)
  val create_authenticated_request
    :  owner:Owner.t
    -> source_owner:Runtime_source_owner.Identity.t
    -> state:Session_state.t
    -> actor:Operator_authorization.t
    -> request:Run_admission_request.t
    -> request_sha256:string
    -> authorize:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
    -> (t, Agent_protocol.Error.t) result

  val preparation : t -> preparation

  (** Returns the cell already attached at issuance, never retrofits provenance
      or reconstructs it from durable request facts. *)
  val of_preparation : preparation -> t option

  (** Negative-only and monotone. This must precede ordinary effect/resource
      escape or actual admission writer entry, even when an ordinary caller omits
      the preparation. It cannot grant permission or clear an attempted write. *)
  val exclude : t -> reason:Exclusion.t -> unit

  (** Exact nonyielding issuing owner/current preparation and still-eligible
      basis. This runs no original policy and grants no effect permission. *)
  val check_basis
    :  t
    -> owner:Owner.t
    -> state:Session_state.t
    -> (unit, Agent_protocol.Error.t) result

  (** Actual Actor-owned factual seal. Closes the same preparation and retains
      its exact eligibility/disposition, never renewing original authority.
      Foreign owners reject. Same-owner displaced, excluded or uncertain attempts
      still close factually; completion and eligibility checks reject them. Public
      [close] alone cannot synthesize this half. No source completion is implied. *)
  val close
    :  t
    -> owner:Owner.t
    -> state:Session_state.t
    -> (Closed_attempt.t, Agent_protocol.Error.t) result

  type deferred = t

  module Replacement : sig
    (** Immutable validated issuance DATA, not installed Actor permission or a
        second live retry cell. Preparation runs the actual new authorization
        without consuming the old completion. The Actor must then repeat its
        genuine old Store/read checks and new original before [commit]. *)
    type t

    val prepare
      :  Source_completion.t
      -> owner:Owner.t
      -> source_owner:Runtime_source_owner.Identity.t
      -> state:Session_state.t
      -> actor:Operator_authorization.t
      -> request:Run_admission_request.t
      -> request_sha256:string
      -> authorize:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
      -> (t, Agent_protocol.Error.t) result

    (** Repeat new original currentness, bracketed by both pure bases. No old
        observation callback is stored in the newly issued preparation. *)
    val check_current
      :  t
      -> owner:Owner.t
      -> state:Session_state.t
      -> (unit, Agent_protocol.Error.t) result

    (** Callback-free exact old/new validation then irreversible old exclusion.
        The Actor publishes the returned actual Deferred in a nonfailing tail.
        Repeated commit rejects the consumed old completion. *)
    val commit
      :  t
      -> owner:Owner.t
      -> state:Session_state.t
      -> (deferred, Agent_protocol.Error.t) result
  end

  module Decision : sig
    type deferred = t

    type t =
      | Retained of Agent_protocol.Run_receipt.t
      | Prepare of deferred
  end
end

module Constructor_jobs : sig
  type preparation := t
  type t = Custody.Constructor_jobs.t

  (** Prepare bookkeeping for one genuine fresh constructor Job. Issuance needs
      the Actor's existing private owner secret and current preparation; a caller
      holding only the returned preparation cannot select arbitrary Job IDs.
      Retained IDs plus this fresh admission bound the subset without a new quota.
      This is not execution authority or a persistent registry. *)
  val prepare_add
    :  preparation
    -> owner:Owner.t
    -> state:Session_state.t
    -> job:Agent_protocol.Job.t
    -> (t, Agent_protocol.Error.t) result

  (** Capture the exact acknowledged retained subset and prepare its consumption
      at that owner's actual checkpoint ACK. Missing/removed facts are not
      reconstructed. Failed writes leave the prior subset unchanged. *)
  val prepare_handoff
    :  preparation
    -> owner:Owner.t
    -> state:Session_state.t
    -> (Agent_protocol.Id.Job.t list * t, Agent_protocol.Error.t) result

  (** Repeat exact revision/selection and original preparation checks immediately
      before persistence, after any yielded host policy work. *)
  val check_before_commit
    :  t
    -> owner:Owner.t
    -> state:Session_state.t
    -> (unit, Agent_protocol.Error.t) result

  (** Nonyielding install only after the exact Job/checkpoint ACK and preparation
      revision advance. Never reopens a closed preparation or renews permission. *)
  val acknowledge : t -> unit
end

module Decision : sig
  type preparation = t

  type t =
    | Retained of Agent_protocol.Run_receipt.t
    | Prepare of preparation
end
