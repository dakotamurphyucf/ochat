open Core

type t =
  { deltas : Session_delta.t list
  ; appended : Agent_protocol.History.entry list
  ; next_sequence : int
  }

(** Pure recovery plan for a quiescent session. Interrupts unfinished invocations,
    publishes retained outcomes against exact canonical calls, or records why
    removed/unbound calls cannot receive output. Existing outputs are validated
    and reused. Does not run scripts, policy callbacks, providers or post hooks.
    Waiting nested observations survive. An observation claimed before interruption
    is marked failed independently of the tool outcome; handlers are never replayed.
    If a compaction was active, its dependent follow-up turn is discarded in the
    same recovery plan. Independent requests and other operation bindings survive.
    Ordinary event executions left Running are interrupted without replay. Their
    waiting-compaction intent is discarded only for the interrupted active
    compaction, with its binding retained; a turn after committed compaction
    success survives reload. Pending/waiting
    intent in an older generation is also discarded. Other completed event
    outcomes and current-generation pending intent survive.

    [first_sequence] must be beyond the durable allocation high-water mark. The
    host must reserve through [next_sequence] and commit all deltas/history events
    atomically before restoring execution. Failed persistence leaves no effects;
    planning again from the same state yields the same IDs. *)
val plan
  :  state:Session_state.t
  -> namespace:string
  -> first_sequence:int
  -> reason:string
  -> (t, Agent_protocol.Error.t) result

(** Same atomic planning contract as [plan], restricted to model invocations
    without a parent job. Call only at a quiescent foreground boundary; independent
    script and background-job invocations are left unchanged. *)
val plan_foreground
  :  state:Session_state.t
  -> namespace:string
  -> first_sequence:int
  -> reason:string
  -> (t, Agent_protocol.Error.t) result
