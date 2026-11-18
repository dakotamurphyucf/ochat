open! Core

(** Loaded-session registry. Actor state remains authoritative. *)

module Entry_close : sig
  type t

  val legacy : (unit -> unit) -> t
  val reported : (unit -> Session_disposal_report.t) -> t
end

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
  ; close : Entry_close.t
  }

(** Invoke this actual transported entry closer outside Registry locks. Successful
    disposal is cached; reported reads preserve all immutable diagnostic groups.
    Legacy delivery reports each actual group's diagnostic once. A retained
    closer remains retained even after its diagnostics have been delivered. *)
val close_entry_reported : entry -> Session_disposal_report.t

val close_entry : entry -> unit

(** Recovery publication carries execution selection alongside the same actual
    entry. Metadata recovery remains unselected until that actual Owner records
    an activation handoff. This is negative registration DATA, not admission. *)
module Recovered : sig
  type t

  val ordinary : entry -> t
  val metadata : entry -> t
  val entry : t -> entry
end

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
val install_recovery_loader
  :  t
  -> (Agent_store.Session_index.Entry.t -> (Recovered.t, Agent_protocol.Error.t) result)
  -> unit

(** Compatibility adapter for genuinely ordinary recovered entries. Metadata-only
    recovery must use [install_recovery_loader]; projecting its entry here would
    discard its required execution exclusion. *)
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

module Read_origin : sig
  type t =
    | Selected
    | Loaded_unselected
    | Indexed
end

(** [read_state_with_execution] uses the same admitted immutable read and
    distinguishes indexed reads from loaded bindings and captures whether a
    loaded binding is selected for ordinary execution.
    A change in selection across the snapshot returns Conflict. Selection
    is an advisory observation and does not grant execution authority. *)
val read_state_with_execution
  :  t
  -> authorize:(Agent_protocol.Session.t -> (unit, Agent_protocol.Error.t) result)
  -> Agent_protocol.Id.Session.t
  -> (Agent_session.Session_state.t * Read_origin.t, Agent_protocol.Error.t) result

module Recurrence_read : sig
  type t

  val state : t -> Agent_session.Session_state.t
  val snapshot : t -> Agent_session.Recurrence_query.snapshot
  val origin : t -> Read_origin.t
  val same_basis : t -> t -> bool
end

(** Loaded query and state share one mailbox turn. An indexed callback runs
    under the same immutable Registry read admission and must use a genuine,
    temporary Store Handle, with no runtime load, activation or recovery repair. *)
val read_recurrences
  :  t
  -> actor:Operator_authorization.t
  -> authorize:(Agent_protocol.Session.t -> (unit, Agent_protocol.Error.t) result)
  -> read_indexed:
       (Agent_store.Session_index.Entry.t
        -> ( Agent_session.Session_state.t * Agent_session.Recurrence_query.snapshot
             , Agent_protocol.Error.t )
             result)
  -> request:Agent_session.Recurrence_query.request
  -> Agent_protocol.Id.Session.t
  -> (Recurrence_read.t, Agent_protocol.Error.t) result

(** Final denial-only binding and live Actor observation check. Synchronous,
    called on the Registry's domain after a fresh read and before disclosure.
    An indexed read has no retained Handle after its checked read lifetime. *)
val check_recurrence_read
  :  t
  -> Recurrence_read.t
  -> (unit, Agent_protocol.Error.t) result

(** Startup-only immutable indexed read. Requires no loaded entries, no installed
    loader or active lifecycle reservations, and a live registry. The supplied
    entry must equal the actual indexed projection. A read lease excludes owner
    shutdown through [f], which runs outside the registry mutex; the same startup
    conditions and exact entry are checked before delivering its result.

    The caller owns the exclusive host/store startup scope, before publishing
    connections or starting recovery/actors/schedulers. [f] may read this entry's
    retained files under their existing owner; it must not mutate this registry,
    install a loader, activate sessions, or retain a borrowed native Handle. No
    source or runtime authority is conveyed by the entry. Original exceptions and
    cancellation propagate through protected read-lease cleanup. *)
val with_indexed_retention_read
  :  t
  -> Agent_store.Session_index.Entry.t
  -> f:(Agent_store.Session_index.Entry.t -> ('a, Agent_protocol.Error.t) Result.t)
  -> ('a, Agent_protocol.Error.t) Result.t

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

(** Publish the exact recovered entry and its execution selection under the same
    registration checks as [add]. *)
val add_recovered
  :  t
  -> session_id:Agent_protocol.Id.Session.t
  -> Recovered.t
  -> (unit, Agent_protocol.Error.t) result

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

(** Negative factual-ownership check for a genuine retained ACK/Set caller.
    Requires this Registry's SAME loaded Actor/runtime/Handle tuple and current
    retained Handle. Global closing does not invalidate historical facts. Pending
    cleanup is permitted ONLY for Cleanup_owner.Entry with this exact tuple;
    foreign Entry, Handle, Fence and Recovery cleanup remain excluded. Recovery
    exposes owns_actor/handle but no actual runtime identity, so those projections
    do not prove complete custody here. Supplies no execution, lifecycle, Store
    write or source authority; actual opaque native proofs remain mandatory.
    Runs on the creating Eio domain without entering the registry mutex; retained
    Handle observation and identity/list comparisons are nonyielding. Call outside
    Actor mailbox and ownership-state/registry locks. *)
val check_retained_factual_owner : t -> entry -> (unit, Agent_protocol.Error.t) result

(** Lock-free shutdown observation for dependency cleanup callbacks. *)
val is_closing : t -> bool

(** Returns a loaded entry or reconstructs an indexed stopped session on
    demand from its durable store. Filesystem/actor work runs under per-ID exclusion
    outside the global mutex. Shutdown or failed binding/identity admission closes
    the provisional entry before releasing that exclusion; a closed runtime is
    never reinstalled. *)
val load : t -> Agent_protocol.Id.Session.t -> (entry, Agent_protocol.Error.t) result

(** Retain a loaded entry for one bounded read, lazily reconstructing an indexed
    session without loading its execution runtime. Wait cancellably for existing
    per-ID lifecycle admission, then recheck membership and shutdown. The callback
    runs once after the loaded tuple is current, outside the global mutex, under
    per-ID exclusion. Cancellation or exceptions release that exclusion. Failed
    cleanup remains an error. Existing owned execution and committed metadata
    observation may continue on the exact tuple during the callback.
    Callers must establish authority before admission and revalidate it before
    disclosure. The callback must not reserve/load/remove this same ID, shut down
    the registry, or wait for work requiring this ID's lifecycle admission. Do not
    retain admission across heartbeat waits or execute lifecycle mutations here. *)
val with_loaded_read
  :  t
  -> Agent_protocol.Id.Session.t
  -> (entry -> ('a, Agent_protocol.Error.t) Result.t)
  -> ('a, Agent_protocol.Error.t) Result.t

(** Retain the exact loaded Actor, Owner and Handle for work in its quiescent
    Actor callback. Admission is fail-fast and provides per-ID exclusion from
    lifecycle work. A missing or mismatched tuple returns [Ok None]. The callback
    runs outside the registry mutex and returns its outcome inside [Ok (Some _)].
    Closing, failed cleanup and another reservation prevent admission. Committed
    native metadata observation of the same tuple remains available. The caller
    supplies quiescent Actor admission and limits the callback to persistence
    work on that owner. *)
val with_loaded_retention
  :  t
  -> actor:Agent_session.Session_actor.t
  -> runtime:Runtime_owner.t
  -> handle:Agent_store.Session_store.Handle.t
  -> (unit -> 'a)
  -> ('a option, Agent_protocol.Error.t) Result.t

val entries : t -> entry list
val stats : t -> stats
val load_all : t -> (entry list, Agent_protocol.Error.t) result
val remove : t -> Agent_protocol.Id.Session.t -> entry option
val summaries : t -> Agent_protocol.Session.t list

(** Closes actors for stopped sessions with no attachments, runnable work, active
    schedules, runtime or resource borrows, retaining their durable index entries
    for lazy reload. Passive loaded observations use a read lease and revalidate
    the exact readable owner without activating indexed sessions or reserving
    running sessions. Only witnessed inactive candidates matching their durable
    projection enter exclusive admission; each is then reserved and its actor fenced before
    inactivity is rechecked. Eviction permanently closes runtime/resource admission
    and closes the entry outside the global mutex before committing its unchanged
    durable projection. Failed closure retains the retired owner for recovery.

    Each call makes one pass over a snapshot of loaded session IDs using the
    supplied durable index. Closing a later candidate can make an earlier ancestor
    eligible; further passes require a fresh durable index. *)
val unload_inactive : t -> index_entries:Agent_store.Session_index.Entry.t list -> int

(** Reject new registration/loading/read lifetimes, drain admitted reservations
    and readers outside the global mutex before the loaded graph snapshot, then
    join all runtime dependency cleanup while keeping loaded lookups and actors
    alive. Retained failed Entry owners join that same graph, once per actual
    owner; none of their resource-close callbacks run before every runtime joins.
    Only then close actors and their stores.
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

(** Negative-only observation for continuing an already owned invocation.
    Closing, failed cleanup, a missing or unavailable Handle, a changed
    Actor/Owner/Handle tuple, and exclusive lifecycle, loading or loaded
    retention reservations block it. A qualified read of the same tuple is
    compatible. The caller must retain and authorize its own execution. *)
val blocks_owned_execution : t -> entry -> bool

(** Negative-only observation for reading committed native ancestry metadata.
    Also permits an exact loaded retention reservation, whose work operates on
    persistence behind the Actor mailbox. The caller must read Actor state and
    revalidate the tuple. Runtime and source authorization remain with their
    respective owners. *)
val blocks_native_metadata_read : t -> entry -> bool

(** Final nonyielding, creating-domain check for committed native metadata.
    Checks the same exact loaded tuple and compatible reservation phases.
    Contention or an exclusive reservation rejects without awaiting the
    Registry mutex. The mutex is released before returning. This supplies
    neither permission nor a lease across a subsequent wait. *)
val check_native_metadata_publication
  :  t
  -> entry
  -> (unit, Agent_protocol.Error.t) result

(** Negative-only observation of retained failed cleanup for this session ID,
    independent of a transient lifecycle/read reservation. It supplies no
    admission or cleanup-completion proof. Same creating Eio domain only. *)
val has_pending_cleanup : t -> Agent_protocol.Id.Session.t -> bool

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

(** Own the actual not-yet-published Entry through initialization. A failure
    closes the SAME runtime and entry under cancellation protection. If cleanup
    fails, retain that owner and both diagnostics in existing failed cleanup;
    same-ID fresh admission stays excluded until actual retry succeeds. Preserve
    the primary Error or exception/backtrace unchanged. This does not publish an
    executable binding or authorize fresh work. *)
val close_provisional
  :  t
  -> session_id:Agent_protocol.Id.Session.t
  -> entry
  -> (unit -> ('a, Agent_protocol.Error.t) Result.t)
  -> ('a, Agent_protocol.Error.t) Result.t

(** Own all partial concrete reconstruction capabilities through the callback.
    On success ownership transfers into the actual returned entry's close closure.
    On failure close the same recovery owner and retain unfinished resources plus
    primary/secondary diagnostics. Startup and already-reserved loaders share it. *)
val with_recovery_owner
  :  t
  -> Session_recovery_owner.t
  -> (unit -> (entry, Agent_protocol.Error.t) Result.t)
  -> (entry, Agent_protocol.Error.t) Result.t

(** Same concrete recovery-owner check and joined failure cleanup as
    [with_recovery_owner], retaining the returned entry's execution selection.
    The wrapper cannot substitute another Actor or Handle for the actual owner. *)
val with_recovered_owner
  :  t
  -> Session_recovery_owner.t
  -> (unit -> (Recovered.t, Agent_protocol.Error.t) Result.t)
  -> (Recovered.t, Agent_protocol.Error.t) Result.t

(** Startup rollback closes the actual entry while preserving its loaded binding;
    detach only successful close. Cleanup failure retains exact owner and both
    diagnostics, excludes the issuing ID, and permits unrelated recovery cleanup.
    This is not a fresh-session remove or lifecycle publication. *)
val rollback_recovered : t -> primary:Cleanup_failure.t -> entry list -> unit

(** Retained projection rebuilding and immutable session reads borrow no actor.
    Close the exact Handle while preserving the primary failure; failed release
    joins the existing registry cleanup ownership before its owner can release
    the Store. *)
val with_recovery_handle
  :  t
  -> store:Agent_store.Session_store.t
  -> Agent_store.Session_store.Handle.t
  -> (unit -> ('a, Agent_protocol.Error.t) Result.t)
  -> ('a, Agent_protocol.Error.t) Result.t

(** Pure ownership check under a short registry snapshot: true only when failed
    cleanup retains this exact Handle capability. It does not grant admission. *)
val retains_cleanup_handle : t -> Agent_store.Session_store.Handle.t -> bool

(** The caller owns the issuing per-ID [with_lifecycle] reservation. *)
val select_indexed
  :  t
  -> Lifecycle_reservation.t
  -> store:Agent_store.Session_store.t
  -> handle:Agent_store.Session_store.Handle.t
  -> current:Agent_store.Session_store.Lifecycle.Current.t
  -> authorize:(Agent_protocol.Session.t -> (unit, Agent_protocol.Error.t) Result.t)
  -> recover:
       (unit
        -> ( entry * Agent_store.Session_store.Lifecycle.Current.t
             , Agent_protocol.Error.t )
             Result.t)
  -> (entry, Agent_protocol.Error.t) Result.t

(** Before invoking recover, validate issuing Indexed reservation, exact indexed
    basis, Store/Handle ownership and current exact-Handle witness in one short
    mutex body. No filesystem/mailbox work under this mutex. Invoke recover under
    protected ownership transfer; the callback consumes valid Handle on invocation,
    including partial construction/activation failure, using Factory's concrete
    recovery owner. Caller retains Handle only if callback was never invoked.
    Successful recovery returns actual entry plus fresh Current for its same Handle
    after legitimate recovery publication; the original witness cannot remain
    current across that publication. Validate fresh currentness/availability and
    exact entry owner before installing under short mutex. [authorize] runs on
    both original and recovered canonical summaries OUTSIDE the registry mutex.
    The final policy check is followed immediately by a nonwaiting pure registry
    commit; contention rejects rather than yielding across that authorization.
    Failed provisional cleanup remains registry-owned with original and secondary
    diagnostics. *)

(** Call only from the registry/owners' creating Eio domain. Current ordinary
    execution selections. Metadata-acquired owners remain in [entries] for
    reads, controls, factual completion and shutdown, but are absent here until
    this exact owner's actual activation handoff. No ID-only selection. *)
val execution_entries : t -> entry list

(** Call only from the registry/owners' creating Eio domain. Pure exact
    binding/Actor/Owner/Handle selection observation. Takes no mutex, invokes no
    callback and grants no effect permission. *)
val execution_selected : t -> entry -> bool

(** Same owned Handle/Current reconstruction/publication as [select_indexed],
    retaining a metadata-acquired canonical binding instead of selecting ordinary
    execution. Authorization runs outside the registry mutex; final publication
    checks only pure reservation, Handle, Current and physical ownership facts.
    The same Actor/Owner remains owned across joined source failures and retries.
    Does not install a runtime or enqueue an autonomous start. Each attempt reads
    the actual checked canonical index before obtaining Current; failed recovery
    never restores its old indexed snapshot. Secondary postfailure refresh errors
    preserve the primary and do not certify resource cleanup or no effects. *)
val acquire_indexed_target
  :  t
  -> Lifecycle_reservation.t
  -> store:Agent_store.Session_store.t
  -> handle:Agent_store.Session_store.Handle.t
  -> authorize:(Agent_protocol.Session.t -> (unit, Agent_protocol.Error.t) result)
  -> recover:
       (unit
        -> ( entry * Agent_store.Session_store.Lifecycle.Current.t
             , Agent_protocol.Error.t )
             result)
  -> (entry, Agent_protocol.Error.t) result

(** Explicit selection of the SAME loaded metadata owner under the issuing
    lifecycle reservation and exact owned Handle/Current. The caller retains its
    actual Actor lifecycle fence through this checked publication. No public
    caller-authored selected Boolean or substitute owner is accepted. *)
val select_loaded
  :  t
  -> Lifecycle_reservation.t
  -> store:Agent_store.Session_store.t
  -> handle:Agent_store.Session_store.Handle.t
  -> current:Agent_store.Session_store.Lifecycle.Current.t
  -> entry
  -> authorize:(Agent_protocol.Session.t -> (unit, Agent_protocol.Error.t) result)
  -> (unit, Agent_protocol.Error.t) result

(** Same actual graph-wide runtime barrier, then adopted resource release.
    Recovery owners participate in the barrier alongside all loaded/failed entries.
    Retained keeps unfinished owners and their dependencies alive. *)
val shutdown_reported : t -> Session_disposal_report.t

(** Retain a SAME actual failed Recovery owner/report after its real close attempt;
    no cleanup execution or fresh owner creation. Called outside Registry locks. *)
val retain_recovery_owner_reported
  :  t
  -> Session_recovery_owner.t
  -> Session_disposal_report.t
  -> unit
