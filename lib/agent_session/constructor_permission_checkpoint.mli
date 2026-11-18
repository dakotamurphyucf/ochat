(** Exact selected permission DATA changes for an already-owned constructor
    Invocation. The selected list must be nonempty. The actor supplies the actual writer-generated rows, optional new
    grant and waiter-derived lifecycle. Every unrelated row and its order remain
    unchanged. This does not authenticate an owner, authorize an effect, renew an
    original proof, or admit a stopped constructor. *)
type t

val prepare
  :  previous:Session_state.t
  -> incoming:Session_state.t
  -> invocation_id:Agent_protocol.Id.Invocation.t
  -> permissions:Agent_protocol.Permission.t list
  -> grant:Agent_protocol.Grant.t option
  -> lifecycle:Session_state.Lifecycle.t
  -> (t, Agent_protocol.Error.t) result

(** Apply the validated selected DATA projection; grants no currentness. *)
val apply : t -> Session_state.t -> Session_state.t
