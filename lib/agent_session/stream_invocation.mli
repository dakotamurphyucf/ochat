open Core

val preparation
  :  Chat_response.In_memory_stream.Tool_dispatch.rejection option
  -> Agent_protocol.Invocation.preparation

type cache

val cache : unit -> cache

(** Retain intent with its canonical call before observer/dispatch callbacks.
    Cache retains the invocation and request digests, not copies of complete history. Reuse
    validates immutable request data; dispatch may additionally observe a later
    session halt. Protected publication keeps a saved intent available locally
    even when cancellation arrives while the actor acknowledges it. *)
val prepare
  :  cache
  -> capabilities:Operation_worker.Capabilities.t
  -> create:
       (Chat_response.In_memory_stream.Tool_dispatch.request
        -> (Agent_protocol.Invocation.t, Agent_protocol.Error.t) result)
  -> Chat_response.In_memory_stream.Tool_dispatch.request
  -> (Agent_protocol.Invocation.t, Agent_protocol.Error.t) result

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

(** Stable identity available to an owned preparation policy before native
    admission. Binds the actual session/generation/operation and allocated
    canonical history ID, not provider-supplied tool call IDs. Rewrites retain
    this identity. It is provenance, not evidence that admission has committed. *)
val id_for_call
  :  input:Operation_worker.Input.t
  -> call_id:History_entry.Id.t
  -> Agent_protocol.Id.Invocation.t

val create
  :  completion_contract:Agent_protocol.Completion_contract.t option
  -> input:Operation_worker.Input.t
  -> request:Chat_response.In_memory_stream.Tool_dispatch.request
  -> implementation_revision:string
  -> capability_fingerprint:string
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> value:Jsonaf.t
  -> (Agent_protocol.Invocation.t, Agent_protocol.Error.t) result
