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

let run json =
  let fail message =
    P.Invocation.
      { code = "agent.wait.invalid_request"; message; retryable = false; details = `Null }
  in
  let decode result =
    Result.map_error result ~f:(fun error -> fail error.P.Error.message)
  in
  let open Result.Let_syntax in
  let%bind fields = P.Json_codec.fields json |> decode in
  let%bind () =
    match
      List.for_all (P.Json_codec.to_alist fields) ~f:(fun (name, _) ->
        List.mem
          [ "session_id"; "receipt_id"; "cursor"; "timeout_ms" ]
          name
          ~equal:String.equal)
    with
    | true -> Ok ()
    | false -> Error (fail "Unexpected session-wait field.")
  in
  let%bind child_id =
    P.Json_codec.required_as fields "session_id" P.Id.Session.of_json |> decode
  in
  let%bind receipt_id =
    P.Json_codec.optional_as fields "receipt_id" P.History.Id.of_json |> decode
  in
  let%bind cursor =
    P.Json_codec.optional_as fields "cursor" P.Page.Cursor.of_json |> decode
  in
  let%bind timeout_ms =
    P.Json_codec.optional_as
      fields
      "timeout_ms"
      (P.Json_codec.bounded_int ~min:0 ~max:30000)
    |> decode
  in
  let%bind target =
    match cursor, receipt_id with
    | Some cursor, receipt_id ->
      Ok (Managed_session_service.Output { cursor; receipt_id })
    | None, Some id -> Ok (Managed_session_service.Receipt id)
    | None, None -> Error (fail "Supply receipt_id or an output cursor.")
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
  service.wait
    borrowed
    child_id
    ~target
    ~timeout_ms:(Option.value timeout_ms ~default:10000)
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
        Chatmd_shell_spec.Source_ref.digest "ochat.agent-wait.native.v1"
    ; result_contract = Invocation_v1
    ; authoring_metadata = None
    }
;;
