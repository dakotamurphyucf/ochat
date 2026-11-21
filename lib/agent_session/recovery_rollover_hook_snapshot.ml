open! Core

let prepare previous ~(outcome : Chat_response.Moderation.Outcome.t) ~snapshot =
  let module S = Session.Moderator_state.Identity_snapshot in
  let open Result.Let_syntax in
  let invalid message = Error (Agent_protocol.Error.invalid_request message) in
  if
    not
      (List.is_empty outcome.overlay_ops
       && Option.is_none outcome.tool_moderation
       && List.is_empty outcome.ui_notifications
       && List.is_empty outcome.runtime_requests
       && List.is_empty outcome.emitted_events
       && Option.is_none outcome.run_action)
  then
    invalid "only moderator state and a typed decision may change during context rollover"
  else (
    let%bind prior =
      Result.of_option
        previous
        ~error:
          (Agent_protocol.Error.invalid_request
             "rollover hook has no moderator checkpoint")
      >>= Moderator_checkpoint.of_jsonaf
    in
    let before = Moderator_checkpoint.snapshot prior in
    let unchanged_runtime = { snapshot with S.current_state = before.current_state } in
    let same a b = Jsonaf.exactly_equal (S.to_jsonaf a) (S.to_jsonaf b) in
    if not (same before unchanged_runtime)
    then invalid "rollover hook changed moderator queue, overlay or source identity"
    else if same before snapshot
    then Ok None
    else (
      let%map next = Moderator_checkpoint.update prior ~snapshot ~queue:Keep in
      Some (Moderator_checkpoint.to_jsonaf next)))
;;
