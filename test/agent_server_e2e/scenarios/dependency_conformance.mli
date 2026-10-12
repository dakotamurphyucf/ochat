(** Real dependency-history status, update, original-key receipt/replay and stale
    generation rejection on an initialized administrative transport. The supplied
    definition is the actual enabled fixture catalog member. No source authority,
    backend, fake actor or mutable writer is supplied by this script. *)
val check
  :  definition:Agent_protocol.Id.Prompt_definition.t
  -> request:
       (Agent_protocol.Command.t
        -> (Agent_protocol.Public.Result.t, Agent_protocol.Error.t) result)
  -> key_prefix:string
  -> unit
