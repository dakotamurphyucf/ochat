open Core

(** Expiring per-invocation collector. It accepts only actual private query
    responses; protocol-decoded metadata cannot be recorded as a fresh read. *)
type t

val collect
  :  Agent_protocol.Invocation.context
  -> (unit -> 'a)
  -> 'a * Agent_protocol.Authoring_reference.t list

val capture : invocation_id:Agent_protocol.Id.Invocation.t -> t option

(** Moderator handlers prepare and commit their result before returning. Keep
    collection alive through that callback and annotate only its exact final
    disclosed resolution. Observation callbacks must not open a new read scope
    for a previously resolved invocation. *)
val with_scope : Agent_protocol.Invocation.context -> (t -> 'a) -> 'a

val annotate
  :  t
  -> Agent_protocol.Invocation.t
  -> (Agent_protocol.Invocation.t, Agent_protocol.Error.t) result

(** The native borrow supplies its exact current selection and invocation. Nested
    callbacks have separate collectors; borrowed captures can cross domain/fiber
    handoffs while alive. Late or foreign writes fail. Up to 32 distinct responses
    and 1 MiB aggregate metadata are retained; identical receipts deduplicate. *)
val record
  :  t
  -> invocation_id:Agent_protocol.Id.Invocation.t
  -> capability_fingerprint:string
  -> Chat_response.Authoring_context.response
  -> (unit, Agent_protocol.Error.t) result
