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

(** Exact resource identity; not equality of durable session IDs. *)
val belongs_to : t -> owner:Owner.t -> bool

val request : t -> Agent_protocol.Run_start.t
val principal_id : t -> Agent_protocol.Id.Principal.t
val request_sha256 : t -> string

(** Checks the exact issuing actor, current original authorization and tracked
    revision/generation. Closed/invalidated/foreign preparations reject. *)
val check
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

module Decision : sig
  type preparation = t

  type t =
    | Retained of Agent_protocol.Run_receipt.t
    | Prepare of preparation
end
