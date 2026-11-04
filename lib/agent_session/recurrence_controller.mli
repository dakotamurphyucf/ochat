(** Actual store/Handle binding for controller lifecycle observations. This owner
    neither reconstructs a witness from a scalar revision nor grants operator,
    source or target execution authority. Mailbox/lifecycle fences remain required. *)
type t

type observation

val create
  :  Agent_store.Session_store.t
  -> handle:Agent_store.Session_store.Handle.t
  -> entry_projection:(Session_state.t -> Agent_store.Session_index.Entry.t)
  -> (t, Agent_protocol.Error.t) result

val session : t -> Agent_protocol.Session_ref.t

(** Issue through current_lifecycle against actual store, retained Handle and the
    complete current-state scheduling projection. IO/cancellation remains owned
    by that established store adapter. No stale index hint or receipt history is
    accepted as lifecycle authority. *)
val observe : t -> Session_state.t -> (observation, Agent_protocol.Error.t) result

val revision : observation -> Agent_protocol.Session_lifecycle.Revision.t
val archived : observation -> bool
val automatic : observation -> bool
val is_current : observation -> bool

(** Actual shared root identity, never an ID-based reconstruction or grant. *)
val same_store : t -> t -> bool

(** Resolve through this controller's existing shared delegation owner while its
    genuine Handle is owned. Returns immutable data, including revoked records;
    the caller must check disposition, state and a fresh lifecycle observation. *)
val resolve_delegation
  :  t
  -> Agent_store.Delegation_store.Reference.t
  -> (Agent_store.Delegation_store.record, Agent_protocol.Error.t) result
