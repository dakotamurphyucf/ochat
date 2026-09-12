open Core
module P = Agent_protocol

let run operation json =
  let denied () =
    P.Invocation.
      { code = "agent.management.denied"
      ; message = "No active session-management invocation."
      ; retryable = false
      ; details = `Null
      }
  in
  let result =
    let open Result.Let_syntax in
    let%bind borrowed =
      Native_tool_invocation.borrow () |> Result.map_error ~f:(fun _ -> denied ())
    in
    let%bind services =
      Script_tool_calls.current_native_services ()
      |> Result.map_error ~f:(fun _ -> denied ())
    in
    let adapter =
      Session_management.create
        ~borrowed
        ~allowed:[ operation ]
        ~creation:(Script_tool_calls.generated_creation_service services)
        ~sessions:(Script_tool_calls.managed_session_service services)
        ~authoring:(Script_tool_calls.authoring_services services)
    in
    Session_management.run adapter operation json
  in
  match result with
  | Ok value -> P.Invocation.Complete value
  | Error error -> Fail error
;;
