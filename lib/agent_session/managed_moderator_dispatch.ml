open Core
module I = Agent_protocol.Invocation
module C = Chat_response.Tool_capability
module Managed = Chat_response.Managed_tool_registry
module EC = Chat_response.Extension_compiler
module M = Chat_response.Moderator_manager
module N = Native_tool_invocation
module Calls = Script_tool_calls

let fail code message = I.Fail { code; message; retryable = false; details = `Null }

let message result =
  Result.map_error result ~f:(fun error -> error.Agent_protocol.Error.message)
;;

let create
      ~definition
      ~manager
      ~history
      ~available_tools
      ~session_meta
      ~now
      tools
      ~execute
      ~native_execute
      ~selected
      ~reference
      ~invocation
      ~prepare_output
  =
  let recorded = ref None in
  let requests = ref [] in
  let failure =
    ref (fail "invocation.handler_failed" "The moderator tool handler failed.")
  in
  let checked outcome f =
    match f () with
    | Ok _ as result -> result
    | Error _ as result ->
      failure := outcome;
      result
    | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
    | exception _ ->
      failure := outcome;
      Error "moderator dispatch check failed"
  in
  let result =
    execute ~invocation (fun ~dispatched ~commit ->
      N.with_dispatched_scope
        ~execute:native_execute
        ~moderator_execute:execute
        ~selected
        ~invocation:dispatched
        (fun () ->
           let save resolved snapshot =
             let open Result.Let_syntax in
             let%map () = commit ~resolved ~snapshot in
             recorded := Some resolved
           in
           let run () =
             let open Result.Let_syntax in
             let check_live () =
               checked
                 (fail "invocation.session_ended" "The session has ended.")
                 (fun () ->
                    match Calls.is_halted tools with
                    | true -> Error "session ended"
                    | false -> Ok ())
             in
             let admit () =
               checked
                 (fail
                    "invocation.stale_binding"
                    "The selected moderator capability is no longer valid.")
                 (fun () ->
                    Managed.admit
                      definition
                      ~current:(Calls.current_capabilities tools)
                      ~selected
                      ~reference
                      ~invocation:dispatched
                    |> Result.map_error ~f:(fun error -> error.C.message))
             in
             let%bind () = check_live () in
             let%bind admission = admit () in
             let prepared = Managed.prepared admission in
             let%bind _ =
               checked
                 (fail
                    "invocation.invalid_input"
                    "The moderator tool arguments are invalid.")
                 (fun () ->
                    Chat_response.Moderator_invocation.prepare_input
                      ~prepared
                      ~limits:(EC.execution_limits prepared)
                      dispatched.context.input)
             in
             let%bind caller = N.borrow () |> message in
             N.with_managed_scope admission (fun borrowed ->
               Calls.with_managed_invocation
                 tools
                 ~execution:admission
                 ~borrowed
                 (fun on_tool_call ->
                    M.handle_invocation_entries
                      ~managed:admission
                      ~execution_context:(N.borrowed_execution_context caller)
                      ~on_tool_call
                      manager
                      ~invocation:dispatched
                      ~history:(history ())
                      ~available_tools
                      ~session_meta
                      ~now_ms:
                        (Agent_protocol.Timestamp.to_time_ns (now ())
                         |> Time_ns.to_int_ns_since_epoch
                         |> fun n -> n / 1_000_000)
                      ~validate_work:(fun _ ->
                        Error "background completion is not installed")
                      ~on_failure:(fun kind ->
                        (* Preserve a more specific host admission/disclosure failure. *)
                        match !failure with
                        | I.Fail { code = "invocation.handler_failed"; _ } ->
                          failure := Moderator_tool_dispatch.handler_failure kind
                        | _ -> ())
                      ~authorize:(fun () ->
                        let%bind () = check_live () in
                        let%bind () =
                          checked
                            (fail
                               "invocation.permission_denied"
                               "Tool execution was not authorized.")
                            (fun () ->
                               Calls.authorize
                                 tools
                                 dispatched
                                 (Managed.binding admission)
                               |> message)
                        in
                        let%bind () = check_live () in
                        Result.map (admit ()) ~f:ignore)
                      ~prepare_resolution:(fun ~resolved ~outcome ~snapshot ->
                        let%bind raw =
                          match resolved.I.status with
                          | Resolved outcome -> Ok outcome
                          | _ -> Error "moderator did not resolve"
                        in
                        let%bind disclosed =
                          checked
                            (fail
                               "invocation.disclosure_rejected"
                               "The moderator result could not be disclosed.")
                            (fun () ->
                               prepare_output
                                 (Openai.Responses.Tool_output.Output.Text
                                    (Jsonaf.to_string (I.outcome_to_json raw)))
                               |> message)
                        in
                        let%bind disclosed =
                          checked
                            (fail
                               "invocation.invalid_output"
                               "The moderator returned an invalid disclosed outcome.")
                            (fun () ->
                               let%bind outcome =
                                 match disclosed with
                                 | `String text ->
                                   I.outcome_of_json (Jsonaf.of_string text) |> message
                                 | _ -> Error "expected disclosed outcome text"
                               in
                               let%map () =
                                 match outcome with
                                 | Complete value ->
                                   Chatmd_shell_spec.Tool_schema.validate
                                     (EC.output_schema prepared)
                                     value
                                   |> Result.map_error ~f:(fun _ ->
                                     "disclosed output schema mismatch")
                                 | Fail _ | Cancelled _ -> Ok ()
                                 | Pending _ ->
                                   Error
                                     "background ownership validation is not installed"
                               in
                               outcome)
                        in
                        let%bind resolved =
                          I.resolve
                            dispatched
                            ~session_id:dispatched.context.session_id
                            ~generation:dispatched.context.generation
                            disclosed
                          |> message
                        in
                        let%map () =
                          checked
                            (fail
                               "invocation.commit_failed"
                               "The moderator result could not be committed.")
                            (fun () -> save resolved snapshot |> message)
                        in
                        fun () ->
                          requests
                          := outcome.Chat_response.Moderation.Outcome.runtime_requests)
                    |> Result.map ~f:ignore
                    |> Result.map_error ~f:Agent_protocol.Error.invalid_request))
             |> message
           in
           let result =
             try run () with
             | Eio.Cancel.Cancelled _ as exn -> raise exn
             | _ -> Error "moderator invocation failed"
           in
           match result, !recorded with
           | Ok (), Some _ ->
             (match !requests with
              | [] -> Ok ()
              | requests ->
                Chat_response.Runtime_request_scope.emit requests
                |> Result.map_error ~f:Agent_protocol.Error.invalid_request)
           | Error _, Some _ ->
             Error
               (Agent_protocol.Error.invalid_request
                  "moderator failed after committing its outcome")
           | Ok (), None | Error _, None ->
             let open Result.Let_syntax in
             let%bind snapshot =
               M.identity_snapshot manager
               |> Result.map_error ~f:Agent_protocol.Error.invalid_request
             in
             let%bind resolved =
               I.resolve
                 dispatched
                 ~session_id:dispatched.context.session_id
                 ~generation:dispatched.context.generation
                 !failure
             in
             save resolved snapshot))
  in
  let open Result.Let_syntax in
  let%bind () = result in
  Result.of_option
    !recorded
    ~error:(Agent_protocol.Error.invalid_request "moderator handoff saved no outcome")
;;
