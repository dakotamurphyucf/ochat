open! Core

(** Private cleanup custody for one actual native Cell resource. This module
    grants no source, constructor, policy or allocator authority. Factory remains
    responsible for admissions and the Cell's final-reference election. *)
type t

(** Shared factual parent repair for the two actual Factory/lifetime consumers.
    The original Cleanup and exact provider Owner remain immutable. This helper
    owns no election, source/constructor permission or new cleanup interpreter. *)
module Parent_cleanup_repair : sig
  type t

  module Current : sig
    (** Disjoint original families: failed pin/positive outer, or joined pin/
        failed outer. Each is an actual saved current observation, never both. *)
    type t =
      | Pin of (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t
      | Outer of (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t
  end

  val create : Runtime_owner.Native_borrow_outcome.Cleanup.t -> owner:Runtime_owner.t -> t

  val observation
    :  t
    -> current:Current.t option
    -> (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t

  (** Caller owns the actual close election and has proved dependent local/graph
      exit. [pin] must be its existing eligible SAME pin witness. Otherwise only
      the exact normally returned outer holder can run existing factual retry,
      with current Complete + sealed-original proof before positive publication.
      A successful outer repair also observes the SAME sticky Released pin's
      existing report handoff; no physical work replays. First original failure
      must have been delivered before calling this operation. Error preserves
      the caller's old current/original failure; actual new failures are DATA. *)
  val retry
    :  t
    -> pin:Runtime_owner.Native_release.t option
    -> current:Current.t option
    -> (Current.t option, Agent_protocol.Error.t) result
end

module Child : sig
  type t

  (** Capture is nonyielding and runs before the real child switch is published.
      It transfers factual partial custody even when the creating fiber is canceled.
      The same child Switch retains its actual immutable release report. Owned
      requested cancellation normalizes only after Complete/no-hook-failures;
      a queued hook raising that same cancellation remains an actual failure.
      An already-cancelled/finished supplied Switch is checked synchronously
      before fork; fork retains its existing domain validation. One stable child
      Switch carrier preserves the selected raw aggregate plus supporting actual
      release parts, or the release parts alone when they select the result.
      These supporting parts are not a unique-leaf count. *)
  val create
    :  sw:Eio.Switch.t
    -> capture:(t -> unit)
    -> (t, Agent_protocol.Error.t) result

  val switch : t -> Eio.Switch.t
  val construction : t -> Agent_session.Runtime_builder.Prepared_native.Construction.t
end

module Parent : sig
  type t

  (** Single-assignment, nonyielding capture before the actual loan callback can
      fail. The same creating domain records its immutable partitioned outcome
      exactly once, before primary projection; acquired pin identity must match.
      [owner] is the actual provider of that returned outcome, retained only for
      later exact factual outer-holder repair. Neither operation retries or
      grants permission. *)
  val capture_release : t -> Runtime_owner.Native_release.t -> unit

  val record
    :  t
    -> owner:Runtime_owner.t
    -> 'a Runtime_owner.Native_borrow_outcome.t
    -> unit
end

val create : sw:Eio.Switch.t -> t
val scope : t -> Runtime_native_scope.t

(** Existing nonyielding Factory capture points, with their order unchanged.
    The sole current construction supplies each actual resource once. *)
val started : t -> unit

val capture_child : t -> Child.t -> unit
val capture_tracking : t -> Graph_tracking.t -> unit
val parent : t -> Parent.t

(** Capture before the first Independent ancestry yield. Retains the SAME
    graph and every actual acquired ancestor cleanup holder, including their
    fixed own-control observations and immutable original views. After driver
    exit the first negative is delivered before a later explicit eligible retry;
    partial retry progress remains retained on refusal. Positive cleanup also
    requires actual graph/lease custody to join. Never re-enters preparation. *)
val independent : t -> Independent_resources.Cleanup.t

(** The existing driver close-attempt callback: common Construction first; on
    failure retain the child/ancestor extent. Then actual child join and factual
    graph seal/finish, preserving the original error/exception and raw trace. *)
val close_local : t -> (unit, Agent_protocol.Error.t) result

(** Cell invokes the sole composite only after all refs join. A failed retained
    driver returns without self-wait or retry; a later explicit attempt retries
    the SAME actual legs. A missing parent report remains a finite refusal.
    Fresh parent cleanup failure is surfaced before any later same-pin or exact
    returned outer-holder factual retry; neither re-enters preparation. *)
val close : t -> (unit, Agent_protocol.Error.t) result

(** SAME actual local attempt, observed before raw projection. A failed common
    release still short-circuits child/graph cleanup; successful independent legs
    retain their original observations and are not replayed for transport. *)
val close_local_observed
  :  t
  -> (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t

(** SAME composite election/order as [close]. Actual paired parent cleanup is
    separate from callback primary, preadmission and driver exit; a source seal/
    fence failure remains negative even if its legacy optional report is absent.
    Retains all actual groups without inferring custody from diagnostic presence. *)
val close_observed
  :  t
  -> (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t
