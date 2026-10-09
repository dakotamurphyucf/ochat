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

(** Adopt each actual successfully allocated capability before a subsequent
    yielding step can fail. Duplicate/out-of-order adoption is an invariant error;
    caller transfers resources exactly once and never closes them independently. *)
val adopt_capacity_exn : t -> Session_capacity.t -> unit

val adopt_writer_exn : t -> Agent_store.Commit_writer.t -> unit
val adopt_actor_exn : t -> Agent_session.Session_actor.t -> unit
val adopt_runtime_exn : t -> Runtime_owner.t -> unit

(** Begin irreversible closure and join the actual runtime. Failure leaves the
    owner closing with its unjoined runtime retained; no actor/writer closes. *)
val prepare_close : t -> unit

(** Runtime join succeeds before actor/writer/Handle closure. Protected ordered
    cleanup records success of each actual resource stage, so retries cannot close
    a released capacity or writer twice. Expected Handle release errors return;
    unexpected exceptions and original backtraces propagate. Failed stage retains
    all unfinished capabilities. No failed close grants execution admission. *)
val close : t -> (unit, Agent_protocol.Error.t) Result.t

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
