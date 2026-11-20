open Core
module C = Chat_response.Tool_capability
module I = Agent_protocol.Invocation
module D = Chat_response.In_memory_stream.Tool_dispatch
module S = Chatmd_shell_spec.Tool_schema

exception Dispatch_error of Agent_protocol.Error.t

let require = function
  | Ok value -> value
  | Error error -> raise (Dispatch_error error)
;;

let invalid_input =
  I.Fail
    { code = "invocation.invalid_input"
    ; message = "The tool arguments are invalid."
    ; retryable = false
    ; details = `Null
    }
;;

let create_current
      ~execution_check
      ~input
      ~(capabilities : Operation_worker.Capabilities.t)
      ~declared
      ~registry
      ~now
      ~is_halted
      ~admit
      ~prepare_output
  =
  let initial = declared in
  let references =
    List.filter (C.references initial) ~f:(fun reference ->
      match C.resolve initial ~id:reference.id ~fingerprint:reference.fingerprint with
      | Ok binding ->
        (match C.implementation binding with
         | Native _ -> true
         | Managed _ -> false)
      | Error _ -> false)
  in
  let protected =
    List.filter_map references ~f:(fun reference ->
      match C.resolve initial ~id:reference.id ~fingerprint:reference.fingerprint with
      | Ok binding
        when C.equal_result_contract (C.result_contract binding) Protected_recovery_v1 ->
        Some (reference.name, binding)
      | _ -> None)
    |> String.Map.of_alist_exn
  in
  let claims ~name = Map.mem protected name in
  let pending = Protected_delivery_queue.create () in
  let protection =
    Option.some_if
      (not (Map.is_empty protected))
      D.
        { claims
        ; take = (fun () -> Protected_delivery_queue.take pending)
        ; close = (fun () -> Protected_delivery_queue.close pending)
        }
  in
  let cache = Stream_invocation.cache () in
  let find name =
    List.find references ~f:(fun reference -> String.equal reference.C.name name)
  in
  let check_projection (request : D.request) =
    let denied () =
      raise
        (Dispatch_error
           (Agent_protocol.Error.invalid_request
              "protected invocation requires its body-free canonical call"))
    in
    if
      claims ~name:request.original_name
      && not (String.equal request.original_name request.name)
    then denied ();
    if claims ~name:request.name
    then
      let module Payload = History_entry.Payload in
      let payload = History_entry.payload request.call in
      match
        Payload.representation payload, Payload.Semantic.view (Payload.semantic payload)
      with
      | Authored, Call { input_bytes; _ } when String.equal input_bytes "{}" -> ()
      | _ -> denied ()
  in
  let prepare (reference : C.reference) (request : D.request) =
    if Option.is_some request.source || Option.is_some request.parent_call_id
    then
      raise
        (Dispatch_error
           (Agent_protocol.Error.create
              Permission_denied
              ~message:"native invocation requires its owning persisted session"
              ~retryable:false
              ()));
    Stream_invocation.prepare cache ~capabilities request ~create:(fun request ->
      let value =
        Stream_invocation.parse_input ~kind:request.kind ~payload:request.payload
      in
      Stream_invocation.create
        ~completion_contract:None
        ~input
        ~request
        ~implementation_revision:reference.C.implementation_revision
        ~capability_fingerprint:(C.fingerprint initial)
        ~now
        ~value:
          (if claims ~name:request.name
           then `Null
           else Result.ok value |> Option.value ~default:`Null))
    |> require
  in
  let commit_call request =
    check_projection request;
    match find request.D.name with
    | None -> false
    | Some reference ->
      ignore (prepare reference request : I.t);
      true
  in
  let validate_original ~kind ~name ~payload =
    match find name with
    | None -> Ok ()
    | Some reference ->
      let open Result.Let_syntax in
      let%bind binding =
        C.resolve initial ~id:reference.id ~fingerprint:reference.fingerprint
        |> Result.map_error ~f:(fun _ -> "invalid native registration")
      in
      let%bind () =
        match kind, (C.descriptor binding).type_ with
        | Chat_response.Tool_call.Kind.Function, "function" | Custom, "custom" -> Ok ()
        | _ -> Error "native tool kind mismatch"
      in
      let%bind value = Stream_invocation.parse_input ~kind ~payload in
      let%bind schema =
        S.compile reference.input_schema
        |> Result.map_error ~f:(fun _ -> "invalid native input schema")
      in
      S.validate schema value
      |> Result.map_error ~f:(fun _ -> "invalid native tool input")
  in
  let run ?run_native (request : D.request) ~authorize =
    check_projection request;
    match find request.name with
    | None -> None
    | Some reference ->
      if Option.is_some request.source || Option.is_some request.parent_call_id
      then
        raise
          (Dispatch_error
             (Agent_protocol.Error.create
                Permission_denied
                ~message:"native invocation requires its owning persisted session"
                ~retryable:false
                ()));
      let value =
        Stream_invocation.parse_input ~kind:request.kind ~payload:request.payload
      in
      let invocation = prepare reference request in
      let execute () =
        Chat_response.Runtime_request_scope.collect (fun () ->
          if Result.is_error value && Option.is_none request.rejection
          then
            capabilities.with_invocation ~invocation (fun ~dispatched:_ ~capabilities:_ ->
              Ok invalid_input)
            |> require
          else
            (match run_native with
             | None -> Native_tool_invocation.run_current
             | Some run_native -> Native_tool_invocation.run_in_driver_current ~run_native)
              ~execution_check
              ~capabilities
              ~registry
              ~reference
              ~invocation
              ~is_halted
              ~authorize:(fun dispatched binding ->
                let open Result.Let_syntax in
                let%map () = admit dispatched binding in
                authorize ())
              ~prepare_output
            |> require)
      in
      let (resolved, runtime_requests), selection =
        match Map.find protected request.name, value with
        | Some binding, Ok input ->
          Protected_native_call.with_input ~invocation ~binding ~input execute
        | _ -> execute (), None
      in
      let outcome =
        match resolved.status with
        | Resolved outcome -> outcome
        | _ -> assert false
      in
      let deliver =
        match outcome, selection with
        | I.Complete `Null, Some (Protected_native_call.Deliver make) ->
          Atomic.make (Some make)
        | _ -> Atomic.make None
      in
      Some
        D.
          { output = Text (Jsonaf.to_string (I.outcome_to_json outcome))
          ; runtime_requests
          ; commit_output =
              Some
                (fun entry ->
                  capabilities.publish_invocation_output
                    ~invocation_id:resolved.context.id
                    entry
                  |> require;
                  Option.iter (Atomic.exchange deliver None) ~f:(fun make ->
                    Protected_delivery_queue.add
                      pending
                      (make ~call:request.call ~output:entry)
                    |> require))
          }
  in
  D.
    { protection
    ; for_fork = None
    ; commit_call
    ; prepare_call = None
    ; validate_original
    ; run
    }
;;

let create =
  create_current ~execution_check:Native_tool_invocation.Execution_check.unavailable
;;
