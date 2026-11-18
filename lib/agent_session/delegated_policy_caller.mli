(** Borrowed identity of one actual child execution for delegated tool policy.
    The representation remains abstract in the private custody module. The
    manifest alias is intentional: trusted internal issuers and public transport
    fields share ONE nominal type, without a conversion or unsafe coercion. *)
type t = Delegated_policy_caller_custody.t

(** Same-domain negative currentness only. No RPC, new admission or permission. *)
val check_current : t -> (unit, Agent_protocol.Error.t) result

(** Close only this borrowed view. Never retires the lending admission or
    independently ACK-installed claim/producer children. Foreign-domain close
    changes nothing; it cannot install any retirement proof. *)
val close : t -> unit
