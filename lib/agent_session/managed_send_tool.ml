open Core
module P = Agent_protocol

let name = "agent_send"

let registration () =
  let module Definition = struct
    type input = Jsonaf.t

    let name = name
    let type_ = "function"

    let description =
      Some
        "Send a plaintext message to a child session you manage. Reuse idempotency_key \
         when retrying the same message. Returns a durable submission receipt, not a \
         completion promise. Busy sessions may defer the message. New messages to \
         stopped sessions reject; this tool never resumes them."
    ;;

    let parameters =
      `Object
        [ "type", `String "object"
        ; ( "properties"
          , `Object
              (List.map [ "session_id"; "message"; "idempotency_key" ] ~f:(fun name ->
                 name, `Object [ "type", `String "string" ])) )
        ; ( "required"
          , `Array [ `String "session_id"; `String "message"; `String "idempotency_key" ]
          )
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
         let outcome = Session_management_native.run Send json in
         Openai.Responses.Tool_output.Output.Text
           (P.Invocation.outcome_to_json outcome |> Jsonaf.to_string))
  in
  Chat_response.Agent_runtime.
    { implementation
    ; implementation_revision =
        Chatmd_shell_spec.Source_ref.digest "ochat.agent-send.native.v1"
    ; result_contract = Invocation_v1
    ; authoring_metadata = None
    }
;;
