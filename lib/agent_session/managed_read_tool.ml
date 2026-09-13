open Core
module P = Agent_protocol

let name = "agent_read"

module Definition = struct
  type input = Jsonaf.t

  let name = name
  let type_ = "function"

  let description =
    Some
      "Read a bounded page of committed assistant output from a child you manage. \
       Optionally select receipt_id from agent_send. Continue with next_cursor, even \
       after caught_up, to see later output without consuming anyone else's results. A \
       completed receipt identifies operation completion; intermediate text does not. \
       Large output uses ordered output_fragment JSON-text chunks: concatenate text for \
       the same entry_id until complete, then decode it. Expired cursors require a fresh \
       bounded snapshot."
  ;;

  let parameters =
    `Object
      [ "type", `String "object"
      ; ( "properties"
        , `Object
            [ "session_id", `Object [ "type", `String "string" ]
            ; "receipt_id", `Object [ "type", `String "string" ]
            ; "cursor", `Object [ "type", `String "string" ]
            ; ( "limit"
              , `Object
                  [ "type", `String "integer"
                  ; "minimum", `Number "1"
                  ; "maximum", `Number "128"
                  ; "description", `String "Maximum records per page; defaults to 16."
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
         let outcome = Session_management_native.run Read json in
         Openai.Responses.Tool_output.Output.Text
           (P.Invocation.outcome_to_json outcome |> Jsonaf.to_string))
  in
  Chat_response.Agent_runtime.
    { implementation
    ; implementation_revision =
        Chatmd_shell_spec.Source_ref.digest "ochat.agent-read.native.v1"
    ; result_contract = Invocation_v1
    ; authoring_metadata = None
    }
;;
