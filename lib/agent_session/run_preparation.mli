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

module Constructor_jobs : sig
  type preparation := t
  type t

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
