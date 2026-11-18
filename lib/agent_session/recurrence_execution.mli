open! Core

(** Bounded immutable occurrence/attempt history. These are stored facts, never
    an execution scope, no-effects observation or target authority. Live owners
    must authenticate positive observations before committing data transitions. *)
type t

module Attempt : sig
  type t

  val request : t -> Recurrence_execution_attempt.t
  val rejection : t -> Agent_protocol.Error.code option
  val uncertainty : t -> Agent_protocol.Error.code option
  val retry_resolution : t -> Recurrence_retry_resolution.t option
  val origin : t -> Recurrence_run_origin.t option
  val terminal : t -> Recurrence_run_reference.t option
end

val create
  :  intent:Recurrence_execution_intent.t
  -> attempt:Recurrence_execution_attempt.t
  -> (t, Agent_protocol.Error.t) result

val intent : t -> Recurrence_execution_intent.t
val attempts : t -> Attempt.t list
val current_attempt : t -> Attempt.t

(** First audit fact is immutable; exact duplicate is a no-op. Rejection and
    uncertainty can coexist and remain retained after resolution/admission.
    Neither means no effects or releases pending capacity. Only the current
    unresolved attempt accepts a new audit fact. Codes use a bounded ASCII wire
    profile of at most 128 bytes; complete compatible metadata is preserved. *)
val record_rejection
  :  t
  -> code:Agent_protocol.Error.code
  -> (t, Agent_protocol.Error.t) result

val mark_uncertain
  :  t
  -> code:Agent_protocol.Error.code
  -> (t, Agent_protocol.Error.t) result

(** Pure data transitions, not safe retry authorization. There is deliberately no
    effectful actor rotation API until the actual no-effects observation issuer
    exists. Next ordinal is contiguous and its key cannot equal any prior key.
    Original intent/control revision, callers, requests and all old facts remain
    immutable. Same-session controller/target is supported. *)
val resolve_retry
  :  t
  -> Recurrence_retry_resolution.t
  -> (t, Agent_protocol.Error.t) result

val append_attempt
  :  t
  -> Recurrence_execution_attempt.t
  -> (t, Agent_protocol.Error.t) result

(** Associate only the exact current unresolved attempt. A stale admission for a
    resolved predecessor is a contradiction, never the current attempt. Actual
    actor import must authenticate the target's retained origin/admission. *)
val associate : t -> Recurrence_run_origin.t -> (t, Agent_protocol.Error.t) result

(** Adds exact terminal reference data after association. The actual owner must
    validate the full terminal payload before import; a decoded cached outcome
    alone grants nothing. Exact duplicate is a no-op, contradiction rejects. *)
val settle : t -> Recurrence_run_reference.t -> (t, Agent_protocol.Error.t) result

(** Claimed durable pending classification. A terminal reference releases pending
    capacity, including Interrupted, but does not itself rearm. Resolved/no-Run
    data remains held/pending until a separate authorized retry or disposition. *)
val is_pending : t -> bool

(** Complete immutable-prefix/evidence/unknown-custody fence. This validates data
    evolution only; actor authentication and actual retained target lookup remain
    separate obligations. Compatible metadata may be added within capacity;
    retained unknown values cannot change or disappear. At most one new attempt
    may be appended per update. *)
val validate_transition : t -> incoming:t -> (unit, Agent_protocol.Error.t) result

(** Positive mandatory growth for the current attempt's terminal or resolution
    path, measured with complete subproof quotas and audit slots. It never grants
    proof and excludes optional future retries. Constructors/decoders/transitions
    admit actual row plus this reserve under the 512 KiB complete row profile.
    Enclosing frontier/catalog/host/journal and depth limits remain separate;
    their owners must compose this growth with all independent obligations. *)
val reserved_growth : t -> (Capacity_growth.t, Agent_protocol.Error.t) result

val equal : t -> t -> bool
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
val limits : Document_schema.Limits.t

(** Private storage-accounting projection at the actual proof-slot paths. Includes
    complete future proof quotas and maximum depth; never decode or publish it
    as execution evidence. Original input/unknown custody remains present. *)
val capacity_projection : t -> Jsonaf.t
