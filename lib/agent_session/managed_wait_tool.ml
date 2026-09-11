open Core
module P = Agent_protocol

let name = "agent_wait"

module Definition = struct
  type input = Jsonaf.t

  let name = name
  let type_ = "function"

  let description =
    Some
      "Wait for a managed child: receipt_id alone waits for that submission's terminal \
       outcome; cursor waits for new assistant output after that position (include the \
       same receipt_id used by agent_read when selecting a receipt). Output availability \
       does not mean the operation completed. timeout_ms defaults to 10000, maximum \
       30000; zero checks immediately. Timeout never stops or resumes the child. This \
       tool returns bounded metadata without consuming output; use agent_read with the \
       original cursor afterward. Expired cursors require a fresh read snapshot."
  ;;

  let parameters =
    `Object
      [ "type", `String "object"
      ; ( "properties"
        , `Object
            [ "session_id", `Object [ "type", `String "string" ]
            ; "receipt_id", `Object [ "type", `String "string" ]
            ; "cursor", `Object [ "type", `String "string" ]
            ; ( "timeout_ms"
              , `Object
                  [ "type", `String "integer"
                  ; "minimum", `Number "0"
                  ; "maximum", `Number "30000"
                  ] )
            ] )
      ; "required", `Array [ `String "session_id" ]
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
         let outcome = Session_management_native.run Wait json in
         Openai.Responses.Tool_output.Output.Text
           (P.Invocation.outcome_to_json outcome |> Jsonaf.to_string))
  in
  Chat_response.Agent_runtime.
    { implementation
    ; implementation_revision =
        Chatmd_shell_spec.Source_ref.digest "ochat.agent-wait.native.v1"
    ; result_contract = Invocation_v1
    ; authoring_metadata = None
    }
;;
