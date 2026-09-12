(** Combine already-current, payload-verified guidance. The caller must select
    actual effective entries in the same context and policy before calling this.
    This validates metadata but does not establish history presence or authority.

    Version-1 whole topics remain complete. Version-2 references require every
    index, matching total counts and non-conflicting item hashes, grouped by exact
    topic/source/version. Duplicate pages never fill missing indexes. Rediscovery
    pointers contribute no content. *)
val complete
  :  Agent_protocol.Authoring_guidance.t list
  -> (Agent_protocol.Authoring_guidance.topic list, Agent_protocol.Error.t) result
