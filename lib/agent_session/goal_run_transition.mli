(** Join the sealed native intent with the actual candidate Action receipt.
    The actor checks current host authority/callback custody before calling;
    supplied IDs or pure intent records never establish that authority. *)
val prepare_request
  :  Session_state.t
  -> scope:Run_scope.t
  -> prepared:Run_transition.t
  -> intent:Agent_protocol.Goal_achievement.Intent.t
  -> (Session_delta.t list, Agent_protocol.Error.t) result
