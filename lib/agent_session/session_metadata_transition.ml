open! Core

let apply state ~expected_metadata_revision ~patch =
  let open Result.Let_syntax in
  if
    not
      (Int64.equal
         state.Session_state.identity.metadata_revision
         expected_metadata_revision)
  then
    Error
      (Agent_protocol.Error.create
         Conflict
         ~message:"session metadata revision does not match"
         ~retryable:false
         ())
  else (
    let%bind previous =
      Agent_protocol.Session_metadata.Values.create
        ~display_name:state.identity.display_name
        ~labels:state.identity.labels
    in
    let%bind next = Agent_protocol.Session_metadata.Patch.apply patch previous in
    if Agent_protocol.Session_metadata.Values.equal previous next
    then Ok None
    else if Int64.equal expected_metadata_revision Int64.max_value
    then
      Error (Agent_protocol.Error.invalid_request "session metadata revision exhausted")
    else
      Ok
        (Some
           (Session_delta.Metadata_changed (next, Int64.succ expected_metadata_revision))))
;;
