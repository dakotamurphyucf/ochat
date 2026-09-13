open Core
module P = Agent_protocol

let name = "agent_stop"

module Definition = struct
  type input = Jsonaf.t

  let name = name
  let type_ = "function"

  let description =
    Some
      "Stop a child you manage while preserving its persisted history. Choose mode \
       graceful to let active work finish, or cancel to cancel active work. Supply a \
       unique idempotency_key and reuse the same key/mode for uncertain retries. The \
       receipt confirms durable stop admission, not completed cleanup; inspect progress \
       and status. Retrying an old key never stops a child again after restart. Use a \
       new key for a new stop or to escalate graceful to cancel."
  ;;

  let parameters =
    `Object
      [ "type", `String "object"
      ; ( "properties"
        , `Object
            [ "session_id", `Object [ "type", `String "string" ]
            ; "idempotency_key", `Object [ "type", `String "string" ]
            ; ( "mode"
              , `Object
                  [ "type", `String "string"
                  ; "enum", `Array [ `String "graceful"; `String "cancel" ]
                  ] )
            ] )
      ; ( "required"
        , `Array [ `String "session_id"; `String "idempotency_key"; `String "mode" ] )
      ; "additionalProperties", `False
      ]
  ;;

  let input_of_string = Jsonaf.of_string
end

let registration () =
  let implementation =
    Ochat_function.create_function
      (module Definition)
      ~strict:false
      (fun json ->
         let outcome = Session_management_native.run Stop json in
         Openai.Responses.Tool_output.Output.Text
           (P.Invocation.outcome_to_json outcome |> Jsonaf.to_string))
  in
  Chat_response.Agent_runtime.
    { implementation
    ; implementation_revision =
        Chatmd_shell_spec.Source_ref.digest "ochat.agent-stop.native.v1"
    ; result_contract = Invocation_v1
    ; authoring_metadata = None
    }
;;
