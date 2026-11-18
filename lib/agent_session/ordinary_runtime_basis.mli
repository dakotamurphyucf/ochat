(** Expected DATA for one ordinary retained-runtime attachment. Capturing this
    value grants no execution, source, constructor, writer or ACK authority.
    Check only against the actual Actor State at its existing worker install. *)
type t

val capture : Session_state.t -> (t, Agent_protocol.Error.t) result

(** Nonyielding source/generation/Stop comparison. Session display name and
    labels, canonical usage and history progress do not invalidate an otherwise
    exact source. Every other Spec field remains part of resource identity.
    Actual original, runtime admission and physical worker/Manager checks remain
    independent and mandatory. *)
val check : t -> Session_state.t -> (unit, Agent_protocol.Error.t) result
