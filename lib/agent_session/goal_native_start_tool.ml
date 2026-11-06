open! Core
module P = Agent_protocol
module N = Native_tool_invocation
module A = Goal_native_admission
module C = Chat_response.Tool_capability
module D = Document_schema

module Schema = struct
  let text = `Object [ "type", `String "string" ]

  let enum values =
    `Object
      [ "type", `String "string"
      ; "enum", `Array (List.map values ~f:(fun value -> `String value))
      ]
  ;;

  let object_ ?(optional = []) fields =
    `Object
      [ "type", `String "object"
      ; "properties", `Object (fields @ optional)
      ; "required", `Array (List.map fields ~f:(fun (name, _) -> `String name))
      ; "additionalProperties", `False
      ]
  ;;

  let alternatives values = `Object [ "anyOf", `Array values ]

  let counter =
    `Object
      [ "type", `String "string"
      ; "minLength", `Number "1"
      ; "maxLength", `Number "19"
      ; "description", `String "Canonical nonnegative decimal string; no leading zeroes"
      ]
  ;;

  let nonnegative_integer = `Object [ "type", `String "integer"; "minimum", `Number "0" ]

  let source =
    object_
      [ "script_id", text
      ; "source_sha256", text
      ; "generation", nonnegative_integer
      ; "installation_epoch", counter
      ]
  ;;

  let blob source_fields =
    object_
      ~optional:[ "display_name", text ]
      ([ "kind", enum [ "file"; "image"; "audio"; "binary" ]
       ; "media_type", text
       ; "byte_length", nonnegative_integer
       ; "digest", text
       ]
       @ source_fields)
  ;;

  let attachment =
    alternatives
      [ blob [ "source", enum [ "stored" ]; "blob_id", text ]
      ; blob [ "source", enum [ "inline_base64" ]; "inline_base64", text ]
      ]
  ;;

  let content =
    object_
      [ "kind", enum [ "plain_text"; "chatmd" ]
      ; "text", text
      ; ( "attachments"
        , `Object
            [ "type", `String "array"
            ; "items", attachment
            ; "maxItems", `Number (Int.to_string P.Run_limits.max_occurrences)
            ] )
      ]
  ;;

  let input =
    alternatives
      [ object_ [ "kind", enum [ "authored_start" ] ]
      ; object_ [ "kind", enum [ "user_submission" ]; "content", content ]
      ]
  ;;

  let request =
    object_
      [ "kind", enum [ "goal_start_episode_v1" ]
      ; "generation", counter
      ; "source", source
      ; "goal_id", text
      ; "expected_goal_revision", counter
      ; "expected_catalog_revision", counter
      ; "input", input
      ; "key", text
      ]
  ;;
end

let input_limits =
  (* Each of at most 4096 actual Blob.Input entries has at most 8 members and 9
     nodes. These structural ceilings leave bounded envelope headroom; the real
     Input/request codecs still enforce all byte/count/depth/content invariants. *)
  D.Limits.create
    ~max_bytes:(P.Run_limits.max_document_bytes + 4096)
    ~max_depth:(P.Run_limits.max_depth + 1)
    ~max_fields:65536
    ~max_nodes:131072
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let decode_text_exn text =
  D.Json.decode ~limits:input_limits text
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let stage request =
  let open Result.Let_syntax in
  let%bind borrowed = N.borrow () in
  let%bind capabilities = N.borrowed_capabilities borrowed in
  let%bind binding =
    C.find capabilities ~name:(A.Effect.name Start)
    |> Result.map_error ~f:(fun _ ->
      P.Error.invalid_request "Goal Start binding is unavailable")
  in
  let%bind token = N.Admission.capture borrowed ~binding in
  let%bind admission = A.create token ~binding ~effect_kind:Start in
  let%bind services =
    Script_tool_calls.current_native_services ()
    |> Result.map_error ~f:P.Error.invalid_request
  in
  let check () =
    Script_tool_calls.check_current_metadata_admission
      services
      (A.invocation admission)
      binding
  in
  let%bind () = check () in
  let%bind service =
    Script_tool_calls.goal_metadata_service services
    |> Result.of_option
         ~error:
           (P.Error.create
              Method_not_found
              ~message:"Goal native staging service is unavailable"
              ~retryable:false
              ())
  in
  let%bind () = Goal_native_metadata_service.start service ~admission ~request in
  check ()
;;

let registration =
  let module Definition = struct
    type input = Jsonaf.t

    let name = A.Effect.name Start
    let type_ = "function"

    let description =
      Some
        "Stage a new Goal episode using exact projected generation/source/Goal/catalog \
         revisions, full typed input and stable key. Local success remains staged until \
         the owning callback ACK. Actual source reservation and admission happen later. \
         In Session_start use user_submission; consumed authored startup cannot replay."
    ;;

    let parameters = Schema.request
    let input_of_string = decode_text_exn
  end
  in
  let implementation =
    Ochat_function.create_function
      (module Definition)
      (fun json ->
         let result = Result.bind (P.Goal_native_start_request.of_json json) ~f:stage in
         let outcome =
           match result with
           | Ok () ->
             P.Invocation.Complete
               (`Object [ "staged", `True; "effect", `String (A.Effect.name Start) ])
           | Error error ->
             P.Invocation.Fail
               { code = P.Error.code_to_string error.code
               ; message = error.message
               ; retryable = error.retryable
               ; details = error.data
               }
         in
         Openai.Responses.Tool_output.Output.Text
           (Jsonaf.to_string (P.Invocation.outcome_to_json outcome)))
  in
  Chat_response.Agent_runtime.
    { implementation
    ; implementation_revision =
        Chatmd_shell_spec.Source_ref.digest "ochat.goal.start.native.v1"
    ; result_contract = Invocation_v1
    ; source_requirement = Ordinary
    ; authoring_metadata = None
    }
;;
