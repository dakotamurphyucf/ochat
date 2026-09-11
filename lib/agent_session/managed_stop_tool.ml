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

let run json =
  let fail message =
    P.Invocation.
      { code = "agent.stop.invalid_request"; message; retryable = false; details = `Null }
  in
  let decode result =
    Result.map_error result ~f:(fun error -> fail error.P.Error.message)
  in
  let open Result.Let_syntax in
  let%bind fields = P.Json_codec.fields json |> decode in
  let%bind () =
    match
      List.for_all (P.Json_codec.to_alist fields) ~f:(fun (name, _) ->
        List.mem [ "session_id"; "idempotency_key"; "mode" ] name ~equal:String.equal)
    with
    | true -> Ok ()
    | false -> Error (fail "Unexpected session-stop field.")
  in
  let%bind child_id =
    P.Json_codec.required_as fields "session_id" P.Id.Session.of_json |> decode
  in
  let%bind key =
    P.Json_codec.required_as fields "idempotency_key" P.Idempotency_key.of_json |> decode
  in
  let%bind mode =
    P.Json_codec.required_as
      fields
      "mode"
      (P.Json_codec.enum
         ~name:"stop mode"
         [ "graceful", P.Session.Graceful; "cancel", Cancel ])
    |> decode
  in
  let%bind borrowed = Native_tool_invocation.borrow () |> decode in
  let%bind services =
    Script_tool_calls.current_native_services () |> Result.map_error ~f:fail
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
  service.stop borrowed child_id ~key ~mode
;;

let registration () =
  let implementation =
    Ochat_function.create_function
      (module Definition)
      ~strict:false
      (fun json ->
         let outcome =
           match run json with
           | Ok result -> P.Invocation.Complete result
           | Error error -> Fail error
         in
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
