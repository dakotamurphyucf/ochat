(** Complete pure admission for one host-selected pending mutation. The returned
    archive is bounded private custody to publish before the journal record. *)
type t

val prepare
  :  Session_state.t
  -> change:Pending_plan.Change.t
  -> retention:Pending_disposition.Retention.t
  -> archive:Session_state.Compaction_archive.t option
  -> limits:Document_schema.Limits.t
  -> (t, Agent_protocol.Error.t) result

val delta : t -> Session_delta.t
val plan : t -> Pending_plan.t
val expiry_archive : t -> Pending_archive.t option
