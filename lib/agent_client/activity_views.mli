(** Transport-neutral observation helpers. Reads do not acknowledge attention.
    Every cancellation requires a retained occurrence row; these helpers never
    expose the legacy witness-free cancel-current-ID behavior. Connections retain
    their existing transport/retry ownership; this module owns no resources. *)
type t

val create : Connection.t -> server_id:Agent_protocol.Id.Server.t -> t

val list_page
  :  t
  -> Agent_protocol.Activity_query.t
  -> ( Agent_protocol.Session_activity.t Agent_protocol.Page.t
       , Agent_protocol.Error.t )
       result

val work_page
  :  t
  -> Agent_protocol.Session_work.Query.t
  -> (Agent_protocol.Session_work.t Agent_protocol.Page.t, Agent_protocol.Error.t) result

val cancel_job
  :  t
  -> Agent_protocol.Session_work.t
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> idempotency_key:Agent_protocol.Idempotency_key.t
  -> (Agent_protocol.Job.Cancel_result.t, Agent_protocol.Error.t) result

val cancel_schedule
  :  t
  -> Agent_protocol.Session_work.t
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> idempotency_key:Agent_protocol.Idempotency_key.t
  -> (Agent_protocol.Schedule.Mutation_response.t, Agent_protocol.Error.t) result
