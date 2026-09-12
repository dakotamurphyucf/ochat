type context = private
  { version : int
  ; scope : string
  ; identity : string
  ; policy : string
  }
[@@deriving equal, sexp]

(** Persisted at the trusted model-input boundary, including manual helper-only
    configurations. It records identities, never documentation prose or secrets. *)
val capture : Authoring_materialization.t -> context

val validate_context
  :  context
  -> session_id:Agent_protocol.Id.Session.t
  -> generation:int
  -> (unit, Agent_protocol.Error.t) result

(** Label an actual model tool output when its verified query receipt matches the
    owning context. The caller must first validate the exact output against the
    invocation outcome. Internal script reads are never promoted to model history. *)
val encode
  :  context:context option
  -> Agent_protocol.Invocation.t
  -> Agent_protocol.History.entry
  -> (Agent_protocol.History.entry, Agent_protocol.Error.t) result

(** Validate retained output provenance against its immutable invocation receipt.
    This does not authorize a model call; call occurrences must remain Canonical.
    Normal output/outcome and call-pair checks remain mandatory. *)
val validate_output
  :  Agent_protocol.Invocation.t
  -> Agent_protocol.History.entry
  -> (unit, Agent_protocol.Error.t) result
