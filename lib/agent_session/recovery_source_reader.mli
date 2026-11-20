open! Core
module P = Agent_protocol

(** One exact canonical citation in the owning Store. [current] checks the
    original caller's current source authority and returns the current actor
    state. [project] is a trusted, synchronous bounded projection. It must apply
    the requested view and part policy, and cannot publish intermediate data.
    Both success and source errors are released only after a fresh check.
    A later asynchronous delivery must recheck again at its own write boundary. *)
type error =
  | Denied
  | Unavailable
  | Withdrawn
  | Expired
  | Stale
  | Missing
  | Corrupt
  | Unsupported
  | Limit
[@@deriving equal, sexp_of]

(** [visible] can further narrow the admitted source. A false result hides
    the selection before I/O and after projection. *)
val read
  :  ?visible:(Session_state.t -> bool)
  -> Agent_store.Session_store.t
  -> handle:Agent_store.Session_store.Handle.t
  -> current:(unit -> (Session_state.t, P.Error.t) result)
  -> max_frame_bytes:int
  -> P.Recovery_window.Citation.t
  -> project:(P.History.entry -> ('a, error) result)
  -> ('a, error) result

(** Resolve one exact effective preparation member. [source_revision] identifies
    the durable preparation rather than the archive revision. The same current
    authority and visibility checks surround opening the source and projection.
    [check_replaced_source] can require that the original canonical row also
    qualifies for the requested disclosure when a moderator replaced it. *)
val read_effective
  :  ?visible:(Session_state.t -> bool)
  -> ?check_replaced_source:(P.History.entry -> (unit, error) result)
  -> Agent_store.Session_store.t
  -> handle:Agent_store.Session_store.Handle.t
  -> current:(unit -> (Session_state.t, P.Error.t) result)
  -> max_frame_bytes:int
  -> source_revision:int64
  -> P.Recovery_window.Citation.t
  -> project:(P.History.entry -> ('a, error) result)
  -> ('a, error) result

(** Trusted retained-source operation on an admitted complete row. [check]
    revalidates the same source coordinate without I/O; [current] must be an
    issuer-bound nonyielding check. [f] may use the current scope for bounded
    local Store I/O, and the source is checked again after [f] returns.
    Neither the check nor the unprotected row may escape the callback. *)
val with_entry
  :  ?visible:(Session_state.t -> bool)
  -> Agent_store.Session_store.t
  -> handle:Agent_store.Session_store.Handle.t
  -> current:(unit -> (Session_state.t, P.Error.t) result)
  -> max_frame_bytes:int
  -> P.Recovery_window.Citation.t
  -> f:(P.History.entry -> check:(unit -> (unit, error) result) -> ('a, error) result)
  -> ('a, error) result
