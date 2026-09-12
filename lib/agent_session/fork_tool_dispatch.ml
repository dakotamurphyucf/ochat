open Core
module C = Chat_response.Tool_capability
module D = Chat_response.In_memory_stream.Tool_dispatch
module I = Agent_protocol.Invocation
module N = Native_tool_invocation
module S = Chatmd_shell_spec.Tool_schema

let require = function
  | Ok value -> value
  | Error error -> raise (Native_tool_dispatch.Dispatch_error error)
;;

let create ~input ~borrowed ~source ~parent_call_id ~now ~prepare ~execute ~for_fork =
  let selected = N.borrowed_capabilities borrowed |> require in
  let parent = N.borrowed_invocation borrowed in
  let check (request : D.request) =
    let open Result.Let_syntax in
    let%bind _ = N.borrowed_capabilities borrowed in
    match request.source, request.parent_call_id with
    | Some actual_source, Some actual_parent
      when String.equal actual_source source && String.equal actual_parent parent_call_id
      -> Ok ()
    | _ ->
      Error
        (Agent_protocol.Error.invalid_request
           "fork tool request belongs to another branch")
  in
  let validate_original ~kind ~name ~payload =
    let open Result.Let_syntax in
    let%bind binding =
      C.find selected ~name |> Result.map_error ~f:(fun error -> error.C.message)
    in
    let%bind () =
      match kind, (C.descriptor binding).type_ with
      | Chat_response.Tool_call.Kind.Function, "function" | Custom, "custom" -> Ok ()
      | _ -> Error "fork tool kind mismatch"
    in
    let%bind value = Stream_invocation.parse_input ~kind ~payload in
    let%bind schema =
      S.compile (C.reference binding).input_schema
      |> Result.map_error ~f:(fun _ -> "invalid fork tool schema")
    in
    S.validate schema value |> Result.map_error ~f:(fun _ -> "invalid fork tool input")
  in
  let prepare_call request =
    let open Result.Let_syntax in
    let%bind () =
      check request
      |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message)
    in
    prepare
      ~selected
      ~parent
      ~id:(Stream_invocation.id_for_call ~input ~call_id:(History_entry.id request.call))
      request
  in
  let run ?run_native (request : D.request) ~authorize =
    check request |> require;
    let binding =
      C.find selected ~name:request.name
      |> Result.map_error ~f:(fun error ->
        Agent_protocol.Error.invalid_request error.C.message)
      |> require
    in
    let reference = C.reference binding in
    let value =
      Stream_invocation.parse_input ~kind:request.kind ~payload:request.payload
    in
    let invocation =
      Stream_invocation.create
        ~completion_contract:None
        ~input
        ~request
        ~implementation_revision:reference.implementation_revision
        ~capability_fingerprint:(C.fingerprint selected)
        ~now
        ~value:(Result.ok value |> Option.value ~default:`Null)
      |> require
    in
    let invocation =
      I.create
        ?routing:
          (Option.map invocation.routing ~f:(fun routing ->
             { routing with canonical_payload = None }))
        ?observer:
          (Option.bind
             (Native_tool_moderation.capture ())
             ~f:Native_tool_moderation.observer)
        { invocation.context with
          origin = Delegated_agent
        ; provider_call_id = None
        ; call_entry_id = None
        ; parent_invocation = Some parent.context.id
        ; deadline = parent.context.deadline
        }
      |> require
    in
    let resolved : I.t =
      execute ~borrowed ~selected ~reference ~invocation ~run_native ~authorize |> require
    in
    let outcome =
      match resolved.status with
      | Resolved outcome -> outcome
      | _ -> assert false
    in
    Some
      D.
        { output = Text (Jsonaf.to_string (I.outcome_to_json outcome))
        ; commit_output = None
        ; runtime_requests = []
        }
  in
  D.
    { for_fork = Some for_fork
    ; commit_call =
        (fun request ->
          check request |> require;
          (* The child stream retains this call locally; dispatch below owns
             actor admission without appending to the root conversation. *)
          true)
    ; prepare_call = Some prepare_call
    ; validate_original
    ; run
    }
;;
