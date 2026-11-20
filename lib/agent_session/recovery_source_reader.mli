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

val read
  :  Agent_store.Session_store.t
  -> handle:Agent_store.Session_store.Handle.t
  -> current:(unit -> (Session_state.t, P.Error.t) result)
  -> max_frame_bytes:int
  -> P.Recovery_window.Citation.t
  -> project:(P.History.entry -> ('a, error) result)
  -> ('a, error) result
