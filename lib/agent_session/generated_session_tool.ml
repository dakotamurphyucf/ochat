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
        "Create a custom persisted sub-agent with its own instructions, model, reasoning \
         settings and optional ChatML moderator. Use it for a specialist researcher, an \
         independent reviewer, an ongoing investigation, or a worker you will revisit \
         with follow-up messages. Supply a captured ChatMD source bundle and an explicit \
         subset of your tools; inherited shell, file and tool rules remain ceilings the \
         child cannot widen. Creation returns a session ID, not the child's final \
         answer. Use the available agent_send, agent_status, agent_wait, agent_read and \
         agent_stop tools to submit work, track it, retrieve outputs and end the \
         session. Default lifetime is owned and default start is stopped; choose \
         start_immediately=true when the child should start. Reuse the idempotency key \
         for retries."
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
