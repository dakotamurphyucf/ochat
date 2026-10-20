(** Read-only trace pages on one initialized physical Connection. This client
    owns no source, recorder, execution, notification reader or mutation intent.
    Every page is independently authorized by the Host; cursors and cached
    capabilities are comparison data, never access authority. *)

type t

(** Requires the selected trace feature and installed closed Capabilities on
    this physical Initialize generation. *)
val create : Connection.t -> (t, Agent_protocol.Error.t) result

(** Immutable negotiated data. This accessor does not assert that the
    underlying physical connection is still initialized. *)
val capabilities : t -> Agent_protocol.Developer_trace_wire.Capabilities.t

(** Bind a new physical connection only to the same server, principal and
    authentication kind. No cursor is transferred or sent; the old owner is
    unchanged. Each subsequent page checks fresh Host authorization. *)
val reconnect : t -> Connection.t -> (t, Agent_protocol.Error.t) result

(** One closed bounded trace page. The retained result must repeat the exact
    request locator and full [after] cursor, and contain no more events than
    the requested limit. The wire decoder checks internal page invariants.
    [Page.resume] is the caller's polling position for an Incomplete trace,
    even when [Page.next] is absent. A page that cannot advance fails with
    Resource_limit rather than returning a repeating [next]. *)
val page
  :  t
  -> Agent_protocol.Developer_trace_wire.Page_request.t
  -> (Agent_protocol.Developer_trace_wire.Availability.t, Agent_protocol.Error.t) result
