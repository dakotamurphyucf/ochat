open Core
module P = Agent_protocol

let status_name = "agent_status"

let status_registration () =
  let module Definition = struct
    type input = Jsonaf.t

    let name = status_name
    let type_ = "function"

    let description =
      Some
        "Inspect a child session you created: lifecycle, current operation and \
         permission-wait count. Does not approve requests or prove a particular message \
         has completed. Session IDs alone do not grant access."
    ;;

    let parameters =
      `Object
        [ "type", `String "object"
        ; "properties", `Object [ "session_id", `Object [ "type", `String "string" ] ]
        ; "required", `Array [ `String "session_id" ]
        ; "additionalProperties", `False
        ]
    ;;

    let input_of_string = Jsonaf.of_string
  end
  in
  let implementation =
    Ochat_function.create_function
      (module Definition)
      ~strict:true
      (fun json ->
         let unavailable message =
           P.Invocation.
             { code = "agent.management.denied"
             ; message
             ; retryable = false
             ; details = `Null
             }
         in
         let outcome =
           let open Result.Let_syntax in
           let%bind id =
             match json with
             | `Object [ ("session_id", `String id) ] ->
               P.Id.Session.of_string id
               |> Result.map_error ~f:(fun _ -> unavailable "Invalid session ID.")
             | _ -> Error (unavailable "Expected a session_id object.")
           in
           let%bind borrowed =
             Native_tool_invocation.borrow ()
             |> Result.map_error ~f:(fun _ ->
               unavailable "No active management invocation.")
           in
           let%bind services =
             Script_tool_calls.current_native_services ()
             |> Result.map_error ~f:(fun _ -> unavailable "No active management service.")
           in
           let%bind service =
             Script_tool_calls.managed_session_service services
             |> Result.of_option
                  ~error:
                    P.Invocation.
                      { code = "capability_unavailable"
                      ; message = "Session management requires a durable Ochat host."
                      ; retryable = false
                      ; details = `Null
                      }
           in
           service.status borrowed id
         in
         let outcome =
           match outcome with
           | Ok value -> P.Invocation.Complete value
           | Error error -> Fail error
         in
         Openai.Responses.Tool_output.Output.Text
           (P.Invocation.outcome_to_json outcome |> Jsonaf.to_string))
  in
  Chat_response.Agent_runtime.
    { implementation
    ; implementation_revision =
        Chatmd_shell_spec.Source_ref.digest "ochat.agent-status.native.v1"
    ; result_contract = Invocation_v1
    ; authoring_metadata = None
    }
;;
