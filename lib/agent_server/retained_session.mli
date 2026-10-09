open! Core

(** Shared retained-session artifact ownership, independent of runtime selection.
    Uses the registry's existing per-ID reservation and Factory immutable reader. *)
type t

val create
  :  store:Agent_store.Session_store.t
  -> registry:Session_registry.t
  -> read_owned:
       (Agent_store.Session_store.Handle.t
        -> (Agent_session.Session_state.t, Agent_protocol.Error.t) Result.t)
  -> t

(** Authorize index summary before IO, then actual state before callback. Loaded
    sessions borrow their actual checked Handle and observe their actor; indexed
    sessions open one owned Handle and perform immutable recovery without actor,
    workspace qualification, provider credentials or repair. The callback may use
    existing export/blob adapters while ownership is retained; it must not select,
    mutate lifecycle, recursively acquire this ID, or shut down the host. Borrowed
    handles are never closed; owned handles close preserving primary failures.
    No capability/handle may escape the callback. *)
val with_state
  :  t
  -> session_id:Agent_protocol.Id.Session.t
  -> authorize:(Agent_protocol.Session.t -> (unit, Agent_protocol.Error.t) Result.t)
  -> f:
       (Agent_store.Session_store.Handle.t
        -> Agent_session.Session_state.t
        -> ('a, Agent_protocol.Error.t) Result.t)
  -> ('a, Agent_protocol.Error.t) Result.t
