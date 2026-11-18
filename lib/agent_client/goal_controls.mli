(** Typed operator calls over an initialized connection. The host checks each
    current principal and attachment and owns all durable commands and execution. *)
type t

(** Read inspection can omit [attachment]. A write needs a currently attached
    writer on this same connection and session. The callback acquires the
    current attachment ID from the caller's existing handle after reattach. *)
val create
  :  Connection.t
  -> session:Agent_protocol.Session_ref.t
  -> ?attachment:(unit -> Agent_protocol.Id.Attachment.t)
  -> unit
  -> t

val list : t -> (Agent_protocol.Goal_query.Result.outcome, Agent_protocol.Error.t) result

val get
  :  t
  -> Agent_protocol.Id.Goal.t
  -> (Agent_protocol.Goal_query.Result.outcome, Agent_protocol.Error.t) result

(** After an ambiguous reply or reconnect, use the exact same immutable intent
    and key with the new writer attachment. Use revisions read after attaching
    for a new intent. This client does not automatically resubmit mutations. *)
val command
  :  t
  -> Agent_protocol.Goal_command.t
  -> (Agent_protocol.Goal_command_receipt.t, Agent_protocol.Error.t) result

val start
  :  t
  -> Agent_protocol.Goal_control.Start.t
  -> (Agent_protocol.Goal_control.Start_result.t, Agent_protocol.Error.t) result

val resume
  :  t
  -> Agent_protocol.Goal_resume_request.t
  -> (Agent_protocol.Goal_control.Resume_result.t, Agent_protocol.Error.t) result
