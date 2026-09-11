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
         let fail message =
           P.Invocation.
             { code = "agent.send.invalid_request"
             ; message
             ; retryable = false
             ; details = `Null
             }
         in
         let outcome =
           let open Result.Let_syntax in
           let%bind fields =
             P.Json_codec.fields json
             |> Result.map_error ~f:(fun _ -> fail "Expected unique message fields.")
           in
           let%bind () =
             match
               List.find (P.Json_codec.to_alist fields) ~f:(fun (name, _) ->
                 not
                   (List.mem
                      [ "session_id"; "message"; "idempotency_key" ]
                      name
                      ~equal:String.equal))
             with
             | None -> Ok ()
             | Some _ -> Error (fail "Unexpected message field.")
           in
           let text name =
             P.Json_codec.required_as fields name P.Json_codec.string
             |> Result.map_error ~f:(fun _ -> fail ("Expected field: " ^ name))
           in
           let%bind id =
             text "session_id"
             |> Result.bind ~f:(fun id ->
               P.Id.Session.of_string id
               |> Result.map_error ~f:(fun _ -> fail "Invalid session ID."))
           in
           let%bind key =
             text "idempotency_key"
             |> Result.bind ~f:(fun key ->
               P.Idempotency_key.of_string key
               |> Result.map_error ~f:(fun _ -> fail "Invalid idempotency key."))
           in
           let%bind message = text "message" in
           let%bind borrowed =
             Native_tool_invocation.borrow ()
             |> Result.map_error ~f:(fun _ -> fail "No active invocation.")
           in
           let%bind services =
             Script_tool_calls.current_native_services ()
             |> Result.map_error ~f:(fun _ -> fail "No active session service.")
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
           service.send borrowed id ~key ~message
         in
         let outcome =
           match outcome with
           | Ok value -> P.Invocation.Complete value
           | Error error -> Fail error
         in
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
