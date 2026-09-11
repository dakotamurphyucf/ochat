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

let run json =
  let fail message =
    P.Invocation.
      { code = "agent.read.invalid_request"; message; retryable = false; details = `Null }
  in
  let decode result =
    Result.map_error result ~f:(fun error -> fail error.P.Error.message)
  in
  let open Result.Let_syntax in
  let%bind fields = P.Json_codec.fields json |> decode in
  let%bind () =
    match
      List.find (P.Json_codec.to_alist fields) ~f:(fun (name, _) ->
        not
          (List.mem
             [ "session_id"; "receipt_id"; "cursor"; "limit" ]
             name
             ~equal:String.equal))
    with
    | None -> Ok ()
    | Some _ -> Error (fail "Unexpected output-read field.")
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
  let%bind limit =
    P.Json_codec.optional_as fields "limit" (P.Json_codec.bounded_int ~min:1 ~max:128)
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
  service.read
    borrowed
    child_id
    ~receipt_id
    ~cursor
    ~limit:(Option.value limit ~default:16)
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
        Chatmd_shell_spec.Source_ref.digest "ochat.agent-read.native.v1"
    ; result_contract = Invocation_v1
    ; authoring_metadata = None
    }
;;
