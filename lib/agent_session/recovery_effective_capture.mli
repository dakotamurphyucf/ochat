(** Bridge between the trusted root preparation and the canonical State owner. *)
open! Core

module P = Agent_protocol
module Record = Recovery_effective_record
module Ledger = Recovery_effective_ledger

(** [None] means the exact record would exceed retained evidence capacity. *)
val prepare
  :  Session_state.t
  -> operation:P.Id.Operation.t
  -> Inference_runtime.Recovery_input.t
  -> (Record.t option, P.Error.t) result

(** Replay repeats the exact state/source check using the body-free journal
    witness. The real Prepared lifetime exists only at initial admission. *)
val apply : Session_state.t -> Record.t -> (Ledger.t, P.Error.t) result

val moderator_changed
  :  Ledger.t option
  -> previous:Jsonaf.t option
  -> next:Jsonaf.t option
  -> Ledger.t option

val replacement : Session_state.t -> incoming:Session_state.t -> Ledger.t option

(** Defense-in-depth source check for a preauthorized live read. The recovery
    service must still apply current visibility and disclose under its gate. *)
val resolve_live
  :  Session_state.t
  -> source_revision:int64
  -> P.Recovery_window.Citation.t
  -> (P.History.entry option, P.Error.t) result

val resolve_record : Session_state.t -> source_revision:int64 -> Record.t option

(** Redirect still-live effective evidence to the exact ordinary compaction or
    rollover prestate. Already proven archive locators remain immutable. The
    owning transaction must independently verify and publish the archive. *)
val relocate
  :  Session_state.t
  -> archive:Session_state.Compaction_archive.t
  -> (Ledger.t option, P.Error.t) result

(** Reconstruct one selected version after the host has verified the exact
    reachable archive frame and current visibility and authorization. *)
val resolve_archived
  :  Session_state.t
  -> Record.t
  -> P.Recovery_window.Citation.t
  -> (P.History.entry option, P.Error.t) result
