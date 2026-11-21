(** Shared recovery operations for an initialized host connection. Callers
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

(** Owner-writer operations over [recovery.note_share_control]. A reconnect
    must obtain its own current attachment. Retrying the exact application key
    and intent re-enters source authorization and the canonical receipt. *)
module Note_shares : sig
  module Wire = Agent_protocol.Recovery_note_share_control

  val inspect
    :  Connection.t
    -> Wire.Source.t
    -> ?path:Wire.Path.t
    -> unit
    -> (Wire.Metadata.t, Agent_protocol.Error.t) result

  val create
    :  Connection.t
    -> Wire.Source.t
    -> expected:Wire.Basis.t
    -> selection:Wire.Selection.t
    -> recipient:Agent_protocol.Session_ref.t
    -> expires_at:Agent_protocol.Timestamp.t
    -> key:Agent_protocol.Idempotency_key.t
    -> (Wire.Receipt.t, Agent_protocol.Error.t) result

  val revoke
    :  Connection.t
    -> Wire.Source.t
    -> expected_share_revision:int64
    -> Wire.Reference.t
    -> key:Agent_protocol.Idempotency_key.t
    -> (Wire.Receipt.t, Agent_protocol.Error.t) result
end

(** Inspect or resolve the retained follow-up using this connection's owner writer.
    If delivery is uncertain, inspect its current phase before choosing another action. *)
val followup
  :  Connection.t
  -> Agent_protocol.Recovery_followup.t
  -> (Agent_protocol.Recovery_followup.Reply.t, Agent_protocol.Error.t) result
