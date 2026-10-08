(** Pure metadata patch policy. The actor separately validates current write
    attachment authority. Execution state and streaming revisions are preserved. *)
val apply
  :  Session_state.t
  -> expected_metadata_revision:int64
  -> patch:Agent_protocol.Session_metadata.Patch.t
  -> (Session_delta.t option, Agent_protocol.Error.t) result
