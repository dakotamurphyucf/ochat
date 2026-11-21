(** Shared recovery operations for an initialized daemon connection. Callers
    can inspect without attaching. Activation requires the current owner's
    attachment and the generation they intend to enable. *)
val enable
  :  Connection.t
  -> Agent_protocol.Recovery_query.Enable.t
  -> (Agent_protocol.Recovery_window.Scope.t, Agent_protocol.Error.t) result

val list
  :  Connection.t
  -> Agent_protocol.Recovery_query.List_windows.t
  -> ( Agent_protocol.Recovery_query.Summary.t Agent_protocol.Page.t
       , Agent_protocol.Error.t )
       result

(** List or search visible items. Continue on the same host with the returned
    opaque cursor, including when the current page contains no rows. *)
val query
  :  Connection.t
  -> Agent_protocol.Recovery_history_request.t
  -> (Agent_protocol.Recovery_history_page.t, Agent_protocol.Error.t) result

(** Read an exact UTF-8 byte range in an ordinary visible message. *)
val read_text
  :  Connection.t
  -> Agent_protocol.Recovery_text_range.t
  -> (Agent_protocol.Recovery_text_range.Reply.t, Agent_protocol.Error.t) result

(** Read one authorized ordinary original chunk. Read continuations pin the
    full original SHA-256 and continue at the returned exact byte offset. *)
val read_asset
  :  Connection.t
  -> Agent_protocol.Recovery_asset_read.t
  -> (Agent_protocol.Recovery_asset_read.Reply.t, Agent_protocol.Error.t) result

(** Inspect bounded window and rollover status under current transcript
    authorization. A key selects only this principal's receipt. Supply the
    current connection's writer attachment to request fresh-action availability. *)
val inspect_rollover
  :  Connection.t
  -> Agent_protocol.Recovery_rollover_control.Inspect.t
  -> (Agent_protocol.Recovery_rollover_control.Status.t, Agent_protocol.Error.t) result

(** Start or reconcile an exact durable intent via its current writer. After
    lost results, retain the original expected cut, key, and force value. A
    reinitialized connection may supply a newly acquired writer attachment. *)
val request_rollover
  :  Connection.t
  -> Agent_protocol.Recovery_rollover_control.Request.t
  -> (Agent_protocol.Recovery_rollover_control.Receipt.t, Agent_protocol.Error.t) result
