open Core
module P = Agent_protocol
module I = P.Invocation
module Stream = Chat_response.In_memory_stream
module EC = Chat_response.Extension_compiler
module M = Chat_response.Moderator_manager

exception Dispatch_error of P.Error.t

let require = function
  | Ok value -> value
  | Error error -> raise (Dispatch_error error)
;;

let message result = Result.map_error result ~f:(fun (e : P.Error.t) -> e.message)
let fail code message = I.Fail { code; message; retryable = false; details = `Null }

let handler_failure = function
  | Chat_response.Moderator_invocation.Unhandled ->
    fail "invocation.unhandled" "The moderator did not resolve this tool invocation."
  | Duplicate_resolution ->
    fail
      "invocation.duplicate_resolution"
      "The moderator attempted to resolve this tool invocation more than once."
  | Wrong_id ->
    fail "invocation.wrong_id" "The resolution referenced a different tool invocation."
  | Invalid_output ->
    fail "invocation.invalid_output" "The moderator returned an invalid tool outcome."
  | Invalid_state ->
    fail "invocation.invalid_state" "The moderator returned invalid persistent state."
  | Suspended ->
    fail "invocation.suspended" "A moderator tool cannot retain a legacy UI continuation."
  | Handler_failed ->
    fail "invocation.handler_failed" "The moderator tool handler failed."
  | Session_ended -> fail "invocation.session_ended" "The session has ended."
;;

let parse_input = Stream_invocation.parse_input

let validate_input prepared value =
  Chat_response.Moderator_invocation.prepare_input
    ~prepared
    ~limits:(EC.execution_limits prepared)
    value
  |> Result.map ~f:(fun _ -> ())
;;

let prepare_request
      ~cache
      ~input
      ~capabilities
      ~now
      prepared
      (request : Stream.Tool_dispatch.request)
  =
  if Option.is_some request.source || Option.is_some request.parent_call_id
  then
    raise
      (Dispatch_error
         (P.Error.create
            Permission_denied
            ~message:"moderator tool requires its owning persisted session"
            ~retryable:false
            ()));
  Stream_invocation.prepare cache ~capabilities request ~create:(fun request ->
    let value = parse_input ~kind:request.kind ~payload:request.payload in
    Stream_invocation.create
      ~input
      ~request
      ~implementation_revision:(EC.fingerprint prepared)
      ~capability_fingerprint:
        (Chat_response.Tool_capability.fingerprint (EC.capabilities prepared))
      ~now
      ~value:(Result.ok value |> Option.value ~default:`Null))
  |> require
;;

let dispatch
      ~revalidate
      ~script_tools
      ~observe_nested
      ~cache
      ~definition
      ~manager
      ~input
      ~(capabilities : Operation_worker.Capabilities.t)
      ~available_tools
      ~session_meta
      ~now
      ~validate_work
      ~admit
      ~prepare_outcome
      (request : Stream.Tool_dispatch.request)
      ~authorize
  =
  match
    List.find (EC.prepared_tools definition) ~f:(fun t ->
      String.equal (EC.declaration t).name request.name)
  with
  | None -> None
  | Some prepared ->
    if Option.is_some request.source || Option.is_some request.parent_call_id
    then
      raise
        (Dispatch_error
           (P.Error.create
              Permission_denied
              ~message:"moderator tool requires its owning persisted session"
              ~retryable:false
              ()));
    (match (EC.declaration prepared).implementation with
     | Standalone _ ->
       raise
         (Dispatch_error
            (P.Error.create
               Invalid_state
               ~message:"standalone execution service is not installed"
               ~retryable:false
               ()))
     | Moderator _ -> ());
    let value = parse_input ~kind:request.kind ~payload:request.payload in
    let parse_error = Result.is_error value in
    let invocation = prepare_request ~cache ~input ~capabilities ~now prepared request in
    let recorded = ref None in
    let observed = ref None in
    let failure = ref None in
    let checked outcome f =
      match f () with
      | Ok _ as result -> result
      | Error _ as result ->
        failure := Some outcome;
        result
      | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
      | exception exn ->
        failure := Some outcome;
        raise exn
    in
    capabilities.with_moderator_invocation ~invocation (fun ~dispatched ~commit ->
      let save resolved snapshot =
        let open Result.Let_syntax in
        let%map () =
          checked
            (fail
               "invocation.commit_failed"
               "The moderator result could not be committed.")
            (fun () -> commit ~resolved ~snapshot)
        in
        recorded := Some resolved
      in
      let run () =
        if Option.is_some request.rejection
        then (
          failure
          := Stream_invocation.rejection_outcome
               (Stream_invocation.preparation request.rejection);
          Error "invocation rejected before execution")
        else if
          parse_error
          || Result.is_error (validate_input prepared invocation.context.input)
        then (
          failure
          := Some
               (fail
                  "invocation.invalid_input"
                  "The tool arguments do not satisfy its input schema.");
          Error "invalid tool arguments")
        else (
          let handle ?on_tool_call ?job_scope ?subscription_scope ?schedule_scope () =
            let jobs = Option.map job_scope ~f:Script_job_service.moderator_transaction in
            let subscriptions =
              Option.map
                subscription_scope
                ~f:Script_subscription_service.moderator_transaction
            in
            let schedules =
              Option.map schedule_scope ~f:Script_schedule_service.moderator_transaction
            in
            let validate_work =
              Script_tool_calls.validate_pending_work
                ~jobs:job_scope
                ~subscriptions:subscription_scope
                ~fallback:validate_work
            in
            M.handle_invocation_entries
              ?jobs
              ?subscriptions
              ?schedules
              ?on_tool_call
              manager
              ~invocation:dispatched
              ~history:request.history
              ~available_tools
              ~session_meta
              ~now_ms:
                (P.Timestamp.to_time_ns (now ())
                 |> Time_ns.to_int_ns_since_epoch
                 |> fun n -> n / 1_000_000)
              ~validate_work
              ~on_failure:(fun kind ->
                if Option.is_none !failure then failure := Some (handler_failure kind))
              ~authorize:(fun () ->
                let open Result.Let_syntax in
                let denied =
                  fail
                    "invocation.permission_denied"
                    "The tool invocation is not authorized by the current policy."
                in
                let%bind () = checked denied (fun () -> admit request) in
                let%bind () =
                  checked denied (fun () ->
                    authorize ();
                    Ok ())
                in
                checked denied (fun () -> revalidate request))
              ~prepare_resolution:(fun ~resolved ~outcome ~snapshot ->
                let open Result.Let_syntax in
                let%bind () =
                  match resolved.I.status with
                  | Resolved outcome ->
                    checked
                      (fail
                         "invocation.disclosure_rejected"
                         "The tool outcome did not pass the host output policy.")
                      (fun () -> prepare_outcome outcome)
                  | _ -> Error "handler did not resolve"
                in
                Ok
                  { M.persist = (fun () -> save resolved snapshot |> message)
                  ; install = (fun () -> observed := Some outcome)
                  })
            |> Result.map ~f:(fun _ -> ())
          in
          match script_tools with
          | None -> handle ()
          | Some tools ->
            Script_tool_calls.with_moderator_work
              tools
              ~owner:(P.Job.Invocation dispatched.context.id)
              ~selected:(EC.capabilities prepared)
              ~source:
                { script_id = (EC.script prepared).id
                ; source_sha256 = (EC.script prepared).source_sha256
                }
              ~originating:(Some (Direct (prepared, dispatched)))
              ~error:Fn.id
              (fun ~jobs:job_scope
                ~subscriptions:subscription_scope
                ~schedules:schedule_scope ->
                 Script_tool_calls.with_invocation
                   tools
                   ~prepared
                   ~capabilities
                   ~parent:dispatched
                   (fun on_tool_call ->
                      handle
                        ~on_tool_call
                        ?job_scope
                        ?subscription_scope
                        ?schedule_scope
                        ())))
      in
      let result =
        try run () with
        | Eio.Cancel.Cancelled _ as exn -> raise exn
        | _ -> Error "handler or authorization callback failed"
      in
      match result, !recorded with
      | Ok (), Some _ -> Ok ()
      | Error _, Some _ ->
        Error
          (P.Error.create
             Invalid_state
             ~message:"moderator failed after recording its resolution"
             ~retryable:false
             ())
      | Ok (), None | Error _, None ->
        let open Result.Let_syntax in
        let%bind snapshot =
          M.identity_snapshot manager
          |> Result.map_error ~f:(fun _ ->
            P.Error.create
              Invalid_state
              ~message:"cannot snapshot failed moderator invocation"
              ~retryable:false
              ())
        in
        let%bind resolved =
          I.resolve
            dispatched
            ~session_id:input.session_id
            ~generation:input.session_generation
            (Option.value !failure ~default:(handler_failure Handler_failed))
        in
        save resolved snapshot)
    |> require;
    let resolved = Option.value_exn !recorded in
    let observation_outcomes =
      match observe_nested with
      | false -> []
      | true ->
        let script = EC.script prepared in
        let result =
          Moderator_observation.drain
            ~capabilities
            ~observer:{ script_id = script.id; source_sha256 = script.source_sha256 }
            ~manager
            ~history:(fun () -> request.history)
            ~available_tools
            ~session_meta
            ~now
            ()
          |> require
        in
        result.outcomes
    in
    let outcome =
      match resolved.status with
      | Resolved outcome -> outcome
      | _ -> assert false
    in
    Some
      Stream.Tool_dispatch.
        { output = Text (Jsonaf.to_string (I.outcome_to_json outcome))
        ; runtime_requests =
            Option.value_map !observed ~default:[] ~f:(fun outcome ->
              outcome.Chat_response.Moderation.Outcome.runtime_requests)
            @ List.concat_map observation_outcomes ~f:(fun outcome ->
              outcome.Chat_response.Moderation.Outcome.runtime_requests)
        ; commit_output =
            Some
              (fun entry ->
                capabilities.publish_invocation_output
                  ~invocation_id:invocation.context.id
                  entry
                |> require)
        }
;;

let create
      ?(revalidate = fun _ -> Ok ())
      ?script_tools
      ?(observe_nested = false)
      ~definition
      ~manager
      ~input
      ~capabilities
      ~available_tools
      ~session_meta
      ~now
      ~validate_work
      ~admit
      ~prepare_outcome
      ()
  =
  let cache = Stream_invocation.cache () in
  let commit_call request =
    match
      List.find (EC.prepared_tools definition) ~f:(fun tool ->
        String.equal (EC.declaration tool).name request.Stream.Tool_dispatch.name)
    with
    | None -> false
    | Some prepared ->
      ignore (prepare_request ~cache ~input ~capabilities ~now prepared request : I.t);
      true
  in
  let validate_original ~kind ~name ~payload =
    match
      List.find (EC.prepared_tools definition) ~f:(fun tool ->
        String.equal (EC.declaration tool).name name)
    with
    | None -> Ok ()
    | Some prepared ->
      Result.bind (parse_input ~kind ~payload) ~f:(fun value ->
        validate_input prepared value
        |> Result.map_error ~f:(fun _ -> "invalid original tool input"))
  in
  Stream.Tool_dispatch.
    { commit_call
    ; validate_original
    ; run =
        dispatch
          ~revalidate
          ~script_tools
          ~observe_nested
          ~cache
          ~definition
          ~manager
          ~input
          ~capabilities
          ~available_tools
          ~session_meta
          ~now
          ~validate_work
          ~admit
          ~prepare_outcome
    }
;;
