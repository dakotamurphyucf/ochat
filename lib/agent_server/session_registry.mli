open! Core

(** Loaded-session registry. Actor state remains authoritative. *)

type entry =
  { actor : Agent_session.Session_actor.t
  ; history_ids : Agent_session.History_id_source.t
  ; runtime : Runtime_owner.t
  ; durable_events : Agent_session.Durable_event_log.t
  ; capacity : Session_capacity.t option
  ; store_handle : Agent_store.Session_store.Handle.t option
  ; expire_permissions : now:Agent_protocol.Timestamp.t -> unit
  ; collect_results :
      unit
      -> ( Agent_store.Job_result_store.Publisher.collection_stats option
           , Agent_protocol.Error.t )
           result
  ; close : unit -> unit
  }

type t

type stats =
  { loaded : int
  ; indexed : int
  }
[@@deriving sexp]

val create : unit -> t

(** Installs the durable loader used by [load]. Each target ID is reserved while
    the callback runs outside the global mutex. Recursive loads of other IDs are
    permitted; same-ID recursion conflicts. The callback owns all provisional
    resources until returning an entry and must preserve primary failures during
    cleanup. It must return the actual checked same-session owner. A failed provisional close
    retains that owner and both failure diagnostics for shutdown retry; its issuing
    ID remains unavailable for fresh ownership until cleanup completes. *)
val install_loader
  :  t
  -> (Agent_store.Session_index.Entry.t -> (entry, Agent_protocol.Error.t) result)
  -> unit

(** Install an immutable durable reader separate from the activating loader. The
    callback must not repair, write state/journal/recovery metadata, construct a
    runtime, resolve provider credentials or retain resources. Temporary store
    lock metadata changes are permitted. The callback runs outside the global
    registry mutex under an admitted read lifetime. It must not recursively close
    or shut down the registry: shutdown joins that lifetime before store release. *)
val install_reader
  :  t
  -> (Agent_store.Session_index.Entry.t
      -> (Agent_session.Session_state.t, Agent_protocol.Error.t) result)
  -> unit

(** Captures one loaded/indexed binding under the short registry mutex, observes
    outside it, then verifies the same current binding and open owner. A replacement
    or concurrent close returns Conflict/Server_shutting_down, never a substitute.
    Loaded actors return one immutable state without runtime activation. Indexed summaries are authorized BEFORE
    reader IO, then the recovered actual session identity and summary are checked
    and authorized again. No actor/cache/index registration or tail repair occurs.
    Recovery cancellation propagates and protected cleanup releases its read
    lifetime. Authorization callbacks must not recursively shut down the registry. *)
val read_state
  :  t
  -> authorize:(Agent_protocol.Session.t -> (unit, Agent_protocol.Error.t) result)
  -> Agent_protocol.Id.Session.t
  -> (Agent_session.Session_state.t, Agent_protocol.Error.t) result

(** Same immutable authorization/IO boundary as [read_state]. Loaded state and
    transient calls are captured atomically in one mailbox turn; stored reads
    advertise Unavailable rather than inventing empty live-call state. *)
val read_observation
  :  t
  -> authorize:(Agent_protocol.Session.t -> (unit, Agent_protocol.Error.t) result)
  -> now:Agent_protocol.Timestamp.t
  -> Agent_protocol.Id.Session.t
  -> (Activity_service.Observation.t, Agent_protocol.Error.t) result

val index : t -> Agent_store.Session_index.Entry.t -> unit
val index_all : t -> Agent_store.Session_index.Entry.t list -> unit

val add
  :  t
  -> session_id:Agent_protocol.Id.Session.t
  -> entry
  -> (unit, Agent_protocol.Error.t) result

(** Read one immutable loaded-entry snapshot without entering the loader lock.
    Allows a child loader to consult an already loaded parent. This does not load
    an ancestor or retain its runtime; callers still need actor validation and a
    runtime lease. Mutations and indexed loads remain serialized. *)
val find : t -> Agent_protocol.Id.Session.t -> entry option

(** Lock-free shutdown observation for dependency cleanup callbacks. *)
val is_closing : t -> bool

(** Returns a loaded entry or reconstructs an indexed stopped session on
    demand from its durable store. Filesystem/actor work runs under per-ID exclusion
    outside the global mutex. Shutdown or failed binding/identity admission closes
    the provisional entry before releasing that exclusion; a closed runtime is
    never reinstalled. *)
val load : t -> Agent_protocol.Id.Session.t -> (entry, Agent_protocol.Error.t) result

val entries : t -> entry list
val stats : t -> stats
val load_all : t -> (entry list, Agent_protocol.Error.t) result
val remove : t -> Agent_protocol.Id.Session.t -> entry option
val summaries : t -> Agent_protocol.Session.t list

(** Closes actors for stopped sessions with no attachments, runnable work, active
    schedules, runtime or resource borrows, retaining their durable index entries
    for lazy reload. Each candidate is reserved and its actual actor fenced before
    inactivity is rechecked. Eviction permanently closes runtime/resource admission
    and closes the entry outside the global mutex before committing its unchanged
    durable projection. Failed closure retains the retired owner for recovery. *)
val unload_inactive : t -> index_entries:Agent_store.Session_index.Entry.t list -> int

(** Reject new registration/loading/read lifetimes, drain admitted reservations
    and readers outside the global mutex before the loaded graph snapshot, then
    join all runtime dependency cleanup while
    keeping loaded lookups and actors alive, then close actors and their stores.
    A failed runtime cleanup retains the loaded graph for a shutdown retry.
    After runtime joins, each entry remains retained until its own close succeeds;
    successful closure detaches only that same owner. Concurrent shutdown calls
    serialize through a private coordinator, never through the registry mutex.
    Once begun, cancellation cannot skip actor/writer closure after runtime
    cleanup; protected cleanup can exceed the host's grace deadline.
    Failed provisional or retired eviction cleanup stays owned for retry. A
    repeated cleanup exception retains its original diagnostic/backtrace and
    prevents successful shutdown/store release; no closed runtime is reopened. *)
val shutdown : t -> unit

(** Nonactivating catalog snapshot. Indexed unloaded sessions have no current owner;
    archived records remain discoverable but cannot be loaded for execution.
    Actor observations run outside the global mutex under a read lifetime; complete
    binding revalidation rejects changed membership instead of returning a partial
    catalog. No filesystem/mailbox wait or resource join occurs under that mutex. *)
val catalog
  :  t
  -> now:Agent_protocol.Timestamp.t
  -> indexed_entries:Agent_store.Session_index.Entry.t list
  -> (Agent_protocol.Session_catalog.t list, Agent_protocol.Error.t) result

(** Proposed additions to Session_registry. A short registry mutation reserves
    this ID, then releases the global mutex before mailbox or filesystem awaits. *)
module Lifecycle_reservation : sig
  type t

  type target =
    | Loaded of entry
    | Indexed of Agent_store.Session_index.Entry.t
    | Absent

  val target : t -> target
end

(** Excludes add/load/index replacement/removal/eviction for the reserved ID.
    Distinct sessions continue normally. Cancellation releases reservation with
    its original loaded/indexed projection unless caller committed retirement.
    Shutdown joins reservations outside the global mutex before closing actors.
    Read-only loaded snapshots remain available to cleanup/dependency callbacks.
    Absent reserves the ID for startup terminal cleanup; it grants no session
    authority. User mutations must separately admit original retained authority. *)
val with_lifecycle
  :  t
  -> Agent_protocol.Id.Session.t
  -> (Lifecycle_reservation.t -> ('a, Agent_protocol.Error.t) Result.t)
  -> ('a, Agent_protocol.Error.t) Result.t

(** Acquire the registry mutex, then verify the current issuing reservation and
    exact same-session Installed witness/retained Handle before installing its
    checked projection. No IO or mailbox awaits occur in the commit body. *)
val commit_lifecycle
  :  t
  -> Lifecycle_reservation.t
  -> handle:Agent_store.Session_store.Handle.t
  -> Agent_store.Session_store.Lifecycle.Installed.t
  -> (entry option, Agent_protocol.Error.t) Result.t

(** Publish absence only from the reserved owner's terminal removal capability. *)
val commit_removal
  :  t
  -> Lifecycle_reservation.t
  -> Agent_store.Session_store.Lifecycle.Removal.t
  -> (entry option, Agent_protocol.Error.t) Result.t

(** After permanent owner retirement without lifecycle effects, fetch the
    unchanged available projection from the actual store outside the registry
    mutex, then detach this owner under its still-current reservation. Uncertain
    store projection fails closed and leaves owner registered/fenced for retry. *)
val commit_retired
  :  t
  -> Lifecycle_reservation.t
  -> store:Agent_store.Session_store.t
  -> (entry option, Agent_protocol.Error.t) Result.t

(** Read the current per-ID exclusion without awaiting an actor or loader. Host
    creation coordinators check this under their own creation lock before reserve. *)
val lifecycle_reserved : t -> Agent_protocol.Id.Session.t -> bool

module Cleanup_owner : sig
  (** Actual retained close/abort authority; no fabricated loaded entry. *)
  type t

  val entry : entry -> t

  val handle
    :  store:Agent_store.Session_store.t
    -> Agent_store.Session_store.Handle.t
    -> t

  val fence : entry -> Agent_session.Session_actor.Lifecycle_fence.t -> t
end

module Cleanup_failure : sig
  (** Expected cleanup rejection or original exception/backtrace. *)
  type t

  val rejected : Agent_protocol.Error.t -> t
  val raised : exn -> Stdlib.Printexc.raw_backtrace -> t
  val error : t -> Agent_protocol.Error.t
  val exception_and_backtrace : t -> (exn * Stdlib.Printexc.raw_backtrace) option
end

(** Retain exact actual capability before issuing reservation releases, alongside
    original primary and secondary diagnostics. Same-ID fresh ownership stays
    excluded after projection detached the old entry. Shutdown retries outside
    mutex, removes only successful actual cleanup and prevents store release on
    repeated failure. *)
val retain_cleanup
  :  t
  -> Lifecycle_reservation.t
  -> owner:Cleanup_owner.t
  -> primary:Cleanup_failure.t
  -> failure:Cleanup_failure.t
  -> unit

val cleanup_owner : Cleanup_owner.t -> (unit, Agent_protocol.Error.t) Result.t

(** Shutdown preserves a retained expected cleanup failure through this exception. *)
exception Cleanup_failed of Cleanup_failure.t

(** Own all partial concrete reconstruction capabilities through the callback.
    On success ownership transfers into the actual returned entry's close closure.
    On failure close the same recovery owner and retain unfinished resources plus
    primary/secondary diagnostics. Startup and already-reserved loaders share it. *)
val with_recovery_owner
  :  t
  -> Session_recovery_owner.t
  -> (unit -> (entry, Agent_protocol.Error.t) Result.t)
  -> (entry, Agent_protocol.Error.t) Result.t

(** Startup rollback closes the actual entry while preserving its loaded binding;
    detach only successful close. Cleanup failure retains exact owner and both
    diagnostics, excludes the issuing ID, and permits unrelated recovery cleanup.
    This is not a fresh-session remove or lifecycle publication. *)
val rollback_recovered : t -> primary:Cleanup_failure.t -> entry list -> unit

(** Retained projection rebuilding borrows no actor. Close exact Handle preserving
    reconciliation primary; failed release joins the existing registry cleanup
    ownership before the exclusive startup owner can release its Store. *)
val with_recovery_handle
  :  t
  -> store:Agent_store.Session_store.t
  -> Agent_store.Session_store.Handle.t
  -> (unit -> (unit, Agent_protocol.Error.t) Result.t)
  -> (unit, Agent_protocol.Error.t) Result.t

(** Pure ownership check under a short registry snapshot: true only when failed
    cleanup retains this exact Handle capability. It does not grant admission. *)
val retains_cleanup_handle : t -> Agent_store.Session_store.Handle.t -> bool
