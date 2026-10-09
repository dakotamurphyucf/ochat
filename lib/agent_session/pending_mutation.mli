(** Replayable host-selected intent for the existing queue. Records the exact
    generation and pending revision; stores no actor or runtime capability. *)
type t [@@deriving sexp]

val create
  :  Session_state.t
  -> change:Pending_plan.Change.t
  -> retention:Pending_disposition.Retention.t
  -> t

val prepare
  :  t
  -> Session_state.t
  -> limits:Document_schema.Limits.t
  -> (Pending_plan.t, Agent_protocol.Error.t) result

val to_jsonaf
  :  t
  -> limits:Document_schema.Limits.t
  -> (Jsonaf.t, Agent_protocol.Error.t) result

val of_jsonaf
  :  Jsonaf.t
  -> limits:Document_schema.Limits.t
  -> (t, Agent_protocol.Error.t) result

val shape : Document_schema.Shape.t
