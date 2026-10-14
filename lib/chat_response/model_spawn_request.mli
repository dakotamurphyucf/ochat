(** Compatibility surface for the one lower pure recipe decoder. Conversation
    and developer callers share its exact grammar and values. *)
include module type of Agent_protocol.Model_spawn_request
