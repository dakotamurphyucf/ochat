open Core
module I = Agent_protocol.Invocation

let rejection_outcome preparation =
  let fail code message =
    Some (I.Fail { code; message; retryable = false; details = `Null })
  in
  match preparation with
  | I.Passed -> None
  | Invalid_input ->
    fail
      "invocation.invalid_input"
      "The original tool arguments do not satisfy its input schema."
  | Pre_tool_rejected ->
    fail "invocation.pre_tool_rejected" "Pre-tool moderation rejected this invocation."
  | Pre_tool_failed -> fail "invocation.pre_tool_failed" "Pre-tool moderation failed."
  | Session_ended -> fail "invocation.session_ended" "The session has ended."
;;

let parse_input ~kind ~payload =
  let value =
    match kind with
    | Chat_response.Tool_call.Kind.Function ->
      Chatmd_shell_spec.Tool_schema.parse_json payload
      |> Result.map_error ~f:(fun _ -> "invalid JSON arguments")
    | Custom -> Ok (`String payload)
  in
  Result.bind value ~f:(fun value ->
    I.validate_outcome (Complete value)
    |> Result.map_error ~f:(fun _ -> "invalid JSON arguments")
    |> Result.map ~f:(fun () -> value))
;;

let create
      ~input
      ~(request : Chat_response.In_memory_stream.Tool_dispatch.request)
      ~implementation_revision
      ~capability_fingerprint
      ~now
      ~value
  =
  let open Result.Let_syntax in
  let fingerprint payload =
    I.
      { sha256 = Chatmd_shell_spec.Source_ref.digest payload
      ; byte_length = String.length payload
      }
  in
  let%bind canonical_payload, call_id =
    match History_entry.item request.call with
    | Function_call call -> Ok (call.arguments, call.call_id)
    | Custom_tool_call call -> Ok (call.input, call.call_id)
    | _ ->
      Error
        (Agent_protocol.Error.invalid_request "invocation requires a canonical tool call")
  in
  let routing =
    I.
      { kind =
          (match request.kind with
           | Function -> Function
           | Custom -> Custom)
      ; original_name = request.original_name
      ; original_payload = fingerprint request.original_payload
      ; final_payload = fingerprint request.payload
      ; canonical_payload = Some (fingerprint canonical_payload)
      ; preparation =
          (match request.rejection with
           | None -> Passed
           | Some Invalid_input -> Invalid_input
           | Some Pre_tool -> Pre_tool_rejected
           | Some Pre_tool_failed -> Pre_tool_failed
           | Some Session_ended -> Session_ended)
      }
  in
  I.create
    ~routing
    { id = Agent_protocol.Id.Invocation.create ()
    ; session_id = input.Operation_worker.Input.session_id
    ; generation = input.session_generation
    ; origin = Model
    ; provider_call_id = Some call_id
    ; call_entry_id = Some (History_entry.id request.call)
    ; parent_invocation = None
    ; parent_job = None
    ; tool_name = request.name
    ; implementation_revision
    ; capability_fingerprint
    ; input = value
    ; created_at = now ()
    ; deadline = None
    }
;;
