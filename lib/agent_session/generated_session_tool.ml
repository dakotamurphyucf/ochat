open Core
module Q = Generated_session_request
module I = Agent_protocol.Invocation

let name = "agent_create"

let registration () =
  let module Definition = struct
    type input = Jsonaf.t

    let name = name

    let description =
      Some
        "Create a persisted child from a captured ChatMD source bundle and an explicit \
         subset of your tools. Model and reasoning settings go in ChatMD config. Default \
         lifetime is owned and default start is stopped. Reuse the idempotency key for \
         retries. Returned IDs and management metadata are identifiers, not \
         authorization. Requires a durable host. Authoring topics: \
         runtime.delegation.generated, runtime.authority.tool-selection."
    ;;

    let type_ = "function"
    let parameters = Q.parameters
    let input_of_string = Jsonaf.of_string
  end
  in
  let implementation =
    Ochat_function.create_function
      (module Definition)
      ~strict:false
      (fun json ->
         let outcome = Session_management_native.run Create json in
         Openai.Responses.Tool_output.Output.Text
           (I.outcome_to_json outcome |> Jsonaf.to_string))
  in
  Chat_response.Agent_runtime.
    { implementation
    ; implementation_revision =
        Chatmd_shell_spec.Source_ref.digest "ochat.agent-create.native.v1"
    ; result_contract = Invocation_v1
    ; authoring_metadata =
        Some
          Chatmd_shell_spec.Authoring_metadata.
            { authoring = Some (Chat_response.Authoring_validation.help Generated_chatmd)
            ; helper = None
            }
    }
;;
