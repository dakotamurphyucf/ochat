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
         let outcome = Session_management_native.run Status json in
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
