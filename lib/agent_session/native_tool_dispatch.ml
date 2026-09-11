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

let create
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
  let cache = Stream_invocation.cache () in
  let find name =
    List.find references ~f:(fun reference -> String.equal reference.C.name name)
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
        ~value:(Result.ok value |> Option.value ~default:`Null))
    |> require
  in
  let commit_call request =
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
  let run (request : D.request) ~authorize =
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
      let resolved, runtime_requests =
        Chat_response.Runtime_request_scope.collect (fun () ->
          if Result.is_error value && Option.is_none request.rejection
          then
            capabilities.with_invocation ~invocation (fun ~dispatched:_ ->
              Ok invalid_input)
            |> require
          else
            Native_tool_invocation.run
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
      let outcome =
        match resolved.status with
        | Resolved outcome -> outcome
        | _ -> assert false
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
                  |> require)
          }
  in
  D.{ commit_call; prepare_call = None; validate_original; run }
;;
