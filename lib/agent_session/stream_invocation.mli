open Core

(** Bounded initial failure for a recorded preparation rejection, or [None]
    when preparation passed. Does not run policy or implementation callbacks. *)
val rejection_outcome
  :  Agent_protocol.Invocation.preparation
  -> Agent_protocol.Invocation.outcome option

(** Shared bounded input parsing and immutable routing evidence for persisted
    model invocations. Canonical display payloads are fingerprinted separately
    from original/final execution input. This does not authorize or execute. *)
val parse_input
  :  kind:Chat_response.Tool_call.Kind.t
  -> payload:string
  -> (Jsonaf.t, string) result

val create
  :  input:Operation_worker.Input.t
  -> request:Chat_response.In_memory_stream.Tool_dispatch.request
  -> implementation_revision:string
  -> capability_fingerprint:string
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> value:Jsonaf.t
  -> (Agent_protocol.Invocation.t, Agent_protocol.Error.t) result
