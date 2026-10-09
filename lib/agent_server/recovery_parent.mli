(** Immutable selected-recovery ancestry observation; never runtime authority. *)
type t

module Disposition : sig
  type t =
    | Retired
    | Retained of { stop_epoch : int64 }
end

(** Positive [max_depth] comes from the Factory's validated delegation limit.
    [authorize_independent] is its existing explicit retained lifetime policy. *)
val create
  :  store:Agent_store.Session_store.t
  -> registry:Session_registry.t
  -> max_depth:int
  -> read_owned:
       (Agent_store.Session_store.Handle.t
        -> (Agent_session.Session_state.t, Agent_protocol.Error.t) result)
  -> authorize_independent:
       (Agent_store.Delegation_store.record -> (unit, Agent_protocol.Error.t) result)
  -> (t, Agent_protocol.Error.t) result

(** Existing retained leases/readers validate exact private linkage, generation,
    source/policy fingerprint and stop epoch without activating ancestors. Known
    stopped, deleted, revoked or changed authority is Retired; unavailable/corrupt
    reads remain errors. Independent edges reuse the supplied existing policy and
    end owned-liveness traversal. Bounded repeated IDs reject before another lease.
    Returned Retained permits preserving an intent only, never execution. *)
val inspect
  :  t
  -> reference:Agent_store.Delegation_store.Reference.t
  -> expected_stop_epoch:int64
  -> (Disposition.t, Agent_protocol.Error.t) result
