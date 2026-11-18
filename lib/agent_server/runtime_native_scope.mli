open! Core
module B = Agent_session.Runtime_builder
module P = Agent_protocol

(** Private same-domain Factory driver. Cell attaches this owner before any
    yielding preparation. This unit depends on the REAL public Prepared_native;
    it does not import Runtime_owner (which already depends on Cell). *)
type t

val create : sw:Eio.Switch.t -> t

(** Same-domain factual health only. Accepts a prepared, retained driver; rejects
    interrupted or drained drivers. It supplies no current source/consumer
    authority and must not gate release of an already acquired loan. *)
val check_open : t -> (unit, P.Error.t) result

(** Pure association with the actual unit published by this same driver. The
    Cell checks this after prepare returns, before publishing a native lease.
    Matching source data alone cannot substitute for this physical identity. *)
val check_prepared : t -> prepared:B.Prepared_native.t -> (unit, P.Error.t) result

(** Invoke f once in one owned fiber. f enters the actual ancestor borrows, then
    invokes retain once. retain installs close_attempt BEFORE invoking prepare.
    It publishes the actual prepared unit, or delivers the original preparation
    error/exception with raw backtrace, without dropping cleanup custody.

    After publication/failure, retain stays inside those SAME ancestor borrows.
    Each explicit close request invokes close_attempt once. Error is delivered
    to that requester and parks for a later explicit request; no automatic retry.
    Only successful dependent cleanup lets retain return and ancestors unwind.
    The close_attempt runs outside the separate child switch it stops/joins.
    The Factory must attach this holder before calling prepare and perform the
    actual Cell failure cleanup if the preparing caller is cancelled. The driver
    belongs to the Factory switch, not the preparing caller's cancel context.

    Registered Owned retirement must drain the dependent Cell through the real
    before_unload path before cancelling its ancestor loan. Unexpected driver
    interruption makes this holder unavailable and reports incomplete cleanup;
    it never invokes a common closer outside the Cell election. *)
val prepare
  :  t
  -> f:
       (retain:
          (prepare:(unit -> (B.Prepared_native.t, P.Error.t) result)
           -> close_attempt:(unit -> (unit, P.Error.t) result)
           -> (unit, P.Error.t) result)
        -> (unit, P.Error.t) result)
  -> (B.Prepared_native.t, P.Error.t) result

(** One explicit dependent cleanup attempt. A failed local attempt retains the
    driver inside its current ancestor borrows. On local success, join the actual
    driver's exit. This success is NOT a parent Cleanup.Joined certificate: the
    fixed Factory closer must inspect the actual recorded reported parent cleanup
    before completing the Cell election. Never re-enter f or its ancestor CPS. *)
val close : t -> (unit, P.Error.t) result

(** Pure domain-checked observation of the actual started driver exit promise.
    False after a failed local close while its ancestor scopes remain retained;
    true only after f and those scopes have returned. No cleanup success or
    authority is inferred from exit. Fresh/unstarted holders reject. The fixed
    Factory composite uses this to return a failed live attempt for explicit
    retry, rather than deadlocking by waiting on a still-retained driver. *)
val driver_has_exited : t -> (bool, P.Error.t) result

(** Observe the original f result after its actual exit; raised exceptions retain
    the original raw backtrace. Factory preserves this primary independently of
    the reported cleanup. Observing it never retries setup, f, or cleanup. *)
val driver_result : t -> (unit, P.Error.t) result

(** Observed callbacks use the SAME driver/request/ready/exit promises. Supplied
    actual carriers are retained before projection; an unknown actual invocation
    escape owns one boundary. Local failure parks exactly as [prepare]. This data
    grants no parent cleanup authority. *)
val prepare_observed
  :  t
  -> f:
       (retain:
          (prepare:
             (unit
              -> (B.Prepared_native.t, P.Error.t) result Agent_session.Close_observation.t)
           -> close_attempt:
                (unit -> (unit, P.Error.t) result Agent_session.Close_observation.t)
           -> (unit, P.Error.t) result Agent_session.Close_observation.t)
        -> (unit, P.Error.t) result Agent_session.Close_observation.t)
  -> (B.Prepared_native.t, P.Error.t) result Agent_session.Close_observation.t

(** SAME actual close request or completed driver join. A positive local close
    may retain original driver diagnostics; it does not certify ancestor cleanup.
    Repeated failed-request observers share its exact carrier, never retry it. *)
val close_observed : t -> (unit, P.Error.t) result Agent_session.Close_observation.t

(** SAME original started-driver result and carriers. Never invokes setup or
    cleanup and does not certify that a failed/aborted driver settled custody. *)
val driver_observation : t -> (unit, P.Error.t) result Agent_session.Close_observation.t
