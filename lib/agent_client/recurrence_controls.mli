(** A headless view of the host-owned recurrence catalog, scheduler and command
    receipts. A view may omit an attachment. Commands use the caller's current
    writer attachment on the same connection and session. *)

type t

val create
  :  Connection.t
  -> session:Agent_protocol.Session_ref.t
  -> ?attachment:(unit -> Agent_protocol.Id.Attachment.t)
  -> unit
  -> t

val list
  :  t
  -> ( Agent_protocol.Recurrence_query.Definition.t list
         Agent_protocol.Recurrence_query.Result.outcome
       , Agent_protocol.Error.t )
       result

val get
  :  t
  -> Agent_protocol.Id.Recurrence.t
  -> ( Agent_protocol.Recurrence_query.Definition.t
         Agent_protocol.Recurrence_query.Result.outcome
       , Agent_protocol.Error.t )
       result

val status
  :  t
  -> Agent_protocol.Id.Recurrence.t
  -> ( Agent_protocol.Recurrence_query.Status.t
         Agent_protocol.Recurrence_query.Result.outcome
       , Agent_protocol.Error.t )
       result

val history
  :  t
  -> Agent_protocol.Id.Recurrence.t
  -> ( Agent_protocol.Recurrence_query.History.t
         Agent_protocol.Recurrence_query.Result.outcome
       , Agent_protocol.Error.t )
       result

(** After an ambiguous reply or reconnect, resubmit the original intent and key
    with the current writer attachment. The client never resubmits a command
    automatically. A receipt confirms only metadata commitment. *)
val command
  :  t
  -> Agent_protocol.Recurrence_control.Intent.t
  -> (Agent_protocol.Recurrence_control.Receipt.t, Agent_protocol.Error.t) result
