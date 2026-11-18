open! Core

(** Shared effect admission. This is a checked adapter, not a source,
    capture, selection, transport or execution owner. *)
module Effect = Instruction_native_admission.Effect

type t

(** Retains the actual native admission unchanged. Every later check delegates
    to its exact token/binding/execution epoch and original operator proof. *)
val of_native : Instruction_native_admission.t -> t

(** Host-only original operator route. IDs and [authorize] are not substitutes
    for [actor]. [authorize] must be a nonyielding current visibility predicate
    over the authoritative state, following Goal_command_admission's pattern.

    Write effects require Some actual connection-owned attachment; read effects
    require None. The constructor validates this invariant and current scope.
    The Actor separately checks its real writer attachment before receipt
    lookup and again before persistence; this value grants no attachment lease.
    Expected generation is retained, never replaced with a later state value. *)
val of_operator
  :  Operator_authorization.t
  -> session_id:Agent_protocol.Id.Session.t
  -> generation:int
  -> effect_kind:Effect.t
  -> attachment_id:Agent_protocol.Id.Attachment.t option
  -> authorize:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> (t, Agent_protocol.Error.t) result

(** Original proof/current scope, exact effect/session/generation and host
    visibility are checked on the supplied state. Native checks remain native.
    Check current original proof again after the host predicate. *)
val check_now
  :  t
  -> Session_state.t
  -> effect_kind:Effect.t
  -> (unit, Agent_protocol.Error.t) result

(** Recover only the exact retained native admission for compatibility adapters.
    Operator admission never produces a native token. *)
val native : t -> Instruction_native_admission.t option

val actor : t -> Operator_authorization.t

(** Only the operator write variant returns Some. The sole Actor consumes this
    as a request to run its existing real [write_attachment], never as proof. *)
val writer_attachment : t -> Agent_protocol.Id.Attachment.t option
