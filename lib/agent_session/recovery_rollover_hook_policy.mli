(** Normalize a checked installed handler decision. Local selections are exact
    members of the supplied qualified checkpoint. Canonical ranges require
    current eligible membership. A share remains a remote routing proposal
    until the actual successor reader verifies the source and recipient. *)
val resolve
  :  Session_state.t
  -> event:Chat_response.Context_rollover_hook.t
  -> execution:Agent_protocol.Id.Moderator_execution.t
  -> view_transcript:bool
  -> Chat_response.Context_rollover_hook.Decision.t
  -> (Recovery_rollover_hooks.change, Agent_protocol.Error.t) result

val checkpoint_offer
  :  Recovery_rollover.Checkpoint.t option
  -> Chat_response.Context_rollover_hook.checkpoint

val choose_followup
  :  Recovery_rollover_hooks.t option
  -> default:Recovery_rollover_continuation.Hook.decision
  -> Recovery_rollover_continuation.Hook.decision
