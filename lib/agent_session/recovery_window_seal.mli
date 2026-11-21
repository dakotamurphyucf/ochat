open! Core
module P = Agent_protocol

(** Derive canonical membership for the atomic rollover transaction. The caller
    supplies the exact prestate archive and chooses which existing canonical
    rows to carry, in their new input order. This does no I/O and never changes
    the session. The actor must sync the checked archive before publishing the
    result with the matching intent, history and terminal receipt. *)
val prepare
  :  Session_state.t
  -> predecessor:P.Recovery_window.Scope.t
  -> successor:P.Recovery_window.Scope.t
  -> archive:Session_state.Compaction_archive.t
  -> retained:P.History.entry list
  -> (Recovery_window_ledger.t, P.Error.t) result
