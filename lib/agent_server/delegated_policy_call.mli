(** Pure validation of ordered ancestor-policy rewrites. This module neither
    authenticates a caller nor authorizes an effect; those checks remain with
    the original Factory and Actor before and after every policy step. *)

type t = Chat_response.Moderation.Tool_call.t

module Step : sig
  type t =
    | Rejected
    | Continue of Chat_response.Moderation.Tool_call.t
end

(** Validate the current selected tool, model kind and argument schema using the
    same path as an actual Approve. Return its canonical kind/payload without
    inventing a policy decision or receipt for an absent moderator. *)
val validate
  :  t
  -> request:Agent_session.Script_tool_calls.preparation
  -> (t, Agent_protocol.Error.t) result

(** Apply one actual policy decision. Approval and rewrites retain the existing
    selected capability, model-tool kind and argument-schema checks. Rejection
    performs no rewrite validation. Error codes and messages match the original
    Factory route. No I/O, state mutation or resource acquisition occurs. *)
val apply
  :  t
  -> request:Agent_session.Script_tool_calls.preparation
  -> decision:Agent_protocol.Moderator_execution.Decision.t
  -> (Step.t, Agent_protocol.Error.t) result

(** Describe the final accepted call relative to the original call. The caller
    must already have evaluated every required policy and repeated its current
    authorization checks. This comparison does not certify that traversal. *)
val finish : t -> original:t -> Chat_response.Moderation.Tool_moderation.t option
