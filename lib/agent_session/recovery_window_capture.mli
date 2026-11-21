(** Canonical capture at native actor/replay boundaries. The already admitted
    history operations supply order and payload identity. The ledger records
    references only. All disabled paths preserve absence. *)
open! Core

module P = Agent_protocol
module Ledger = Recovery_window_ledger

val bootstrap
  :  Session_state.t
  -> owner:P.Session_ref.t
  -> window:P.Id.Context_window.t
  -> (Ledger.t, P.Error.t) result

(** Extend the active window. Store the new member's actual positions in the
    publication identified by the enclosing transaction. *)
val append
  :  Session_state.t
  -> P.History.entry list
  -> (Ledger.t option, P.Error.t) result

(** Reconcile a live replacement, removing retired live locators from the
    active window. Existing archive locators and sealed membership stay intact.
    Current visibility is checked by the recovery service. *)
val replace
  :  Session_state.t
  -> history:P.History.entry list
  -> retired:P.History.Id.t list
  -> (Ledger.t option, P.Error.t) result

(** Ordinary summary compaction preserves the current window and moves removed
    exact live members to the supplied committed prestate's physical locators.
    It admits exactly one new summary occurrence in the replacement. *)
val summarize
  :  Session_state.t
  -> archive:Session_state.Compaction_archive.t
  -> history:P.History.entry list
  -> (Ledger.t, P.Error.t) result

(** Compute the ledger for a native replacement. A new source generation
    admits a fresh root over that generation's current canonical rows. The
    caller retains the old generation in its existing administrative archive. *)
val replacement
  :  Session_state.t
  -> incoming:Session_state.t
  -> (Ledger.t option, P.Error.t) result

(** A generation-changing durable replacement must retain its exact prior
    generation through the existing Reset or Rebuild archive reference. *)
val check_replacement_archive
  :  Session_state.t
  -> incoming:Session_state.t
  -> (unit, P.Error.t) result

(** A replacement may retire a rollover ledger only with a fresh administrative
    archive at the exact prior revision. The ordered document replay checks
    the complete transaction prestate and persists the archive before the
    journal. The pure reducer may see the same reference already staged by
    the transaction's pending-input retirement. *)
val rollover_replacement_archive
  :  ?pending_archive_staged:bool
  -> Session_state.t
  -> incoming:Session_state.t
  -> (Session_state.Compaction_archive.t option, P.Error.t) result
