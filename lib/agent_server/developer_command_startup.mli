open! Core
module Store = Agent_store.Developer_run_store

(** Private physical startup sequence over the existing Store claim and Recovery
    window. The caller remains the sole owner of their lifecycle and must keep
    every acquired claim until a conclusive release. No command is reserved and
    no execution grant is issued here. *)
module Acquired : sig
  type t

  val handle : t -> Store.handle
  val claim : t -> Store.Run_claim.t
  val window : t -> Store.Recovery.t
end

(** Observe the exact persisted latest epoch, reissue only its same-epoch
    handle using [previous=Some observation], then acquire its physical claim
    and open Recovery. [on_claim] runs immediately after acquisition, before
    the next yielding Store call, so the caller retains custody even if opening
    the window fails or raises. A missing run returns [None]. *)
val open_existing
  :  Store.t
  -> run_id:Agent_protocol.Id.Developer_run.t
  -> on_claim:(Store.Run_claim.t -> unit)
  -> (Acquired.t option, Store.Error.t) result

(** Acquire the exact already-admitted handle before any current-epoch command
    reservation. The caller has proved that no same-run callback is active,
    holds the run admission Slot, and retains the claim through [on_claim]
    before this helper opens Recovery. No epoch is appended here. *)
val open_admitted
  :  Store.t
  -> handle:Store.handle
  -> on_claim:(Store.Run_claim.t -> unit)
  -> (Acquired.t, Store.Error.t) result

(** The host adapter settles authentic abandoned job/inference inventory. This
    helper only finishes the exact existing Recovery window after that adapter
    succeeds; release of the physical claim belongs to the lifecycle. *)
val finish
  :  Acquired.t
  -> recover:
       (claim:Store.Run_claim.t
        -> handle:Store.handle
        -> window:Store.Recovery.t
        -> (unit, Agent_protocol.Error.t) result)
  -> map_store_error:(Store.Error.t -> Agent_protocol.Error.t)
  -> (unit, Agent_protocol.Error.t) result
