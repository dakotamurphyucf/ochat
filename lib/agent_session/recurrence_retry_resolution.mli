open! Core

(** Immutable reference DATA claiming that a particular staged attempt was
    resolved without a Run or effects. This is not a no-effects proof. Neither
    construction nor restoration permits rotation, replay or initialization.
    The actual effect-deferred owner must issue a separate opaque observation
    after authenticating physical closure, including on recovery. *)
type t

val create
  :  intent:Recurrence_execution_intent.t
  -> attempt:Recurrence_execution_attempt.t
  -> target_revision:int64
  -> resolved_at:Agent_protocol.Timestamp.t
  -> (t, Agent_protocol.Error.t) result

val attempt_sha256 : t -> string
val target_revision : t -> int64
val resolved_at : t -> Agent_protocol.Timestamp.t

(** Exact immutable association only. Observed revision is nonnegative but need
    not reach the original CAS: a rejected CAS may observe an earlier revision.
    No ordering or current permission is inferred from the audit timestamp. *)
val validate
  :  t
  -> intent:Recurrence_execution_intent.t
  -> attempt:Recurrence_execution_attempt.t
  -> (unit, Agent_protocol.Error.t) result

val equal : t -> t -> bool
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result

(** Complete compatible JSON: 4 KiB, depth 24, 64 fields and 128 nodes. *)
val limits : Document_schema.Limits.t
