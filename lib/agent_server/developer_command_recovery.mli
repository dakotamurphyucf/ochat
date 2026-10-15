open! Core
module Store = Agent_store.Developer_run_store

(** Private daemon startup adapter for the one Store Recovery window owned by
    Developer_command_lifecycle. The caller has already joined any former
    physical callback and holds the exact Run_claim. This module interrupts
    abandoned inference attempts before their jobs through Store's sole writer.
    It does not finish the window, release the claim, create a Root or replay
    language/provider effects. An uncertain mutation remains in the same
    window for an exact later reconciliation; no replacement outcome is chosen. *)
val recover
  :  Store.Recovery.t
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> (unit, Agent_protocol.Error.t) result
