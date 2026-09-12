open Core

(** Expiring per-invocation collector. It accepts only actual private query
    responses; protocol-decoded metadata cannot be recorded as a fresh read. *)
type t

val collect
  :  Agent_protocol.Invocation.context
  -> (unit -> 'a)
  -> 'a * Agent_protocol.Authoring_reference.t list

val capture : invocation_id:Agent_protocol.Id.Invocation.t -> t option

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
