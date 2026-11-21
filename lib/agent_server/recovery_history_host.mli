open! Core
module P = Agent_protocol
module A = Agent_session
module Store = Agent_store.Session_store

type authorize_view =
  P.Principal.t
  -> A.Session_state.t
  -> A.Recovery_item_text.view
  -> selection:A.Recovery_item_text.selection option
  -> (unit, P.Error.t) result

type release = unit -> bool
type staged
type source_result = ((staged, P.Error.t) result, A.Recovery_source_reader.error) result

(** Runs one physical-source history request. The host consumes [with_source]
    once and supplies its exact retained owner predicate. The release hook,
    when requested, receives a fresh nonyielding and no-I/O delivery check. *)
val query
  :  Store.t
  -> now:(unit -> Mtime.t)
  -> max_payload_bytes:int
  -> with_source:
       (P.Session_ref.t
        -> authorize:(A.Session_state.t -> (unit, P.Error.t) result)
        -> f:
             (Store.Handle.t
              -> observation:A.Session_actor.Resource_observation.t
              -> current:(unit -> (A.Session_state.t, P.Error.t) result)
              -> check_owner:(unit -> (unit, P.Error.t) result)
              -> source_result)
        -> source_result)
  -> access:A.Session_actor.Recovery_history_access.request
  -> ?authorize_view:authorize_view
  -> ?cursor:P.Page.Cursor.t
  -> ?on_release:(release -> unit)
  -> A.Recovery_history_query.t
  -> (Jsonaf.t, P.Error.t) result
