open! Core

(** Concrete resource owner for retained-session reconstruction. Each adopted
    capability belongs to this exact retained Handle. No runtime activation,
    filesystem migration or fresh-session transaction is performed here. *)
type t

(** Takes ownership of Handle and optional admitted capacity after validating the
    Store capability. Before writer/actor allocation, a failed recovery can still
    retain and retry this exact owner. Successful creation transfers ownership
    exactly once; failure leaves Handle/capacity with the caller. *)
val create
  :  store:Agent_store.Session_store.t
  -> handle:Agent_store.Session_store.Handle.t
  -> job_capacity:Job_capacity.t
  -> capacity:Session_capacity.t option
  -> (t, Agent_protocol.Error.t) Result.t

val session_id : t -> Agent_protocol.Id.Session.t
val handle : t -> Agent_store.Session_store.Handle.t
val owns_actor : t -> Agent_session.Session_actor.t -> bool

(** Whether this owner adopted this exact runtime instance. Together with Actor
    and Handle identity, this distinguishes aliases from independent cleanup owners. *)
val owns_runtime : t -> Runtime_owner.t -> bool

(** Adopt each actual successfully allocated capability before a subsequent
    yielding step can fail. Duplicate/out-of-order adoption is an invariant error;
    caller transfers resources exactly once and never closes them independently. *)
val adopt_capacity_exn : t -> Session_capacity.t -> unit

val adopt_writer_exn : t -> Agent_store.Commit_writer.t -> unit
val adopt_actor_exn : t -> Agent_session.Session_actor.t -> unit
val adopt_retention_exn : t -> Session_retention.t -> unit
val adopt_runtime_exn : t -> Runtime_owner.t -> unit

(** Begin irreversible closure and join the adopted retention/runtime. [disposed]
    on this report certifies only those joins, not Actor/writer/Handle disposal.
    A Retained report keeps all later stages owned. Actual child diagnostics are
    forwarded unchanged; reported observation does not consume legacy delivery.
    Calls serialize with [close_reported] and protect actual cleanup from caller
    cancellation. Only a completed positive preparation is cached. *)
val prepare_close_reported : t -> Session_disposal_report.t

(** Run the same ordered retention/runtime, jobs, Actor, capacity, writer and
    Handle cleanup. [disposed] is true only after every adopted stage completes.
    Successful stages never replay; a failed retry owns a fresh diagnostic. A
    completed report is cached with its immutable diagnostics. Retained reports
    never authorize disposal, including after legacy diagnostic delivery. *)
val close_reported : t -> Session_disposal_report.t

(** Legacy projection of [prepare_close_reported]. Delivers an actual diagnostic
    once with its original raw backtrace. Already-delivered diagnostics do not
    turn retained custody into success. *)
val prepare_close : t -> unit

(** Legacy projection of [close_reported]. Only an actual Handle-release Error
    returns [Error]; preparation/runtime and raised stage failures raise with
    their original provenance/backtrace. Completed repeated close succeeds once
    its immutable report's diagnostics have been delivered.

    [capture_report], when supplied, is a trusted nonyielding observer which must
    return normally. It receives this invocation's actual report exactly once,
    before legacy delivery, while the same close coordinator still protects its
    origin. It must not recursively close this owner. The callback is not retained;
    Registry uses it only to assign the actual report to an invocation-local cell. *)
val close
  :  ?capture_report:(Session_disposal_report.t -> unit)
  -> t
  -> (unit, Agent_protocol.Error.t) Result.t

(** Optional close-time snapshots are cache maintenance, not commit authority.
    Their expected failure remains observable while durable journal-backed resource
    closure proceeds; no snapshot failure is interpreted as successful repair. *)
val record_checkpoint_failure : t -> Agent_protocol.Error.t -> unit

val checkpoint_failure : t -> Agent_protocol.Error.t option
val is_closing : t -> bool

(** Serialize the entry checkpoint and resource close sequence. The callback must
    not recursively acquire this coordinator. Resource close also has its own
    serialized stage coordinator for retained-owner retries. *)
val with_entry_close : t -> (unit -> 'a) -> 'a
