open! Core

(** Pure, bounded projection of an already-executed retained REPL Task.
    The caller supplies the effective Host/Store receipt limit and cooperative
    poll; this module grants no Task execution, source access or persistence.
    A diagnostic location uses the exact owned Site's source length, including
    when it names an earlier snippet. An oversized result becomes a bounded
    Limited receipt; it is never persisted as a language value. *)
val limited_fallback : Agent_store.Developer_run_store.Command.Receipt.value

val project
  :  limits:Document_schema.Limits.t
  -> poll:(unit -> unit)
  -> Chatml_repl_execution.Task_result.t
  -> (Agent_store.Developer_run_store.Command.Receipt.t, Agent_protocol.Error.t) result
