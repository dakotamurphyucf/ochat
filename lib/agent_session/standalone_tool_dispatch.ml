open Core
module I = Agent_protocol.Invocation
module EC = Chat_response.Extension_compiler
module ABI = Chat_response.Moderator_invocation
module D = Chat_response.In_memory_stream.Tool_dispatch
module R = Chatml_host_runtime
module L = Chatml.Chatml_lang

exception Dispatch_error of Agent_protocol.Error.t

let require = function
  | Ok value -> value
  | Error error -> raise (Dispatch_error error)
;;

let fail code message = I.Fail { code; message; retryable = false; details = `Null }

let checked failure f =
  match f () with
  | Ok value -> Ok value
  | Error _ -> Error failure
  | exception
      ((Eio.Cancel.Cancelled _ | Eio.Time.Timeout | Chatml_execution.Budget_exhausted _)
       as exn) -> raise exn
  | exception _ -> Error failure
;;

let declared_execution_limits prepared =
  let limits = EC.execution_limits prepared in
  { Chatml_execution.default_limits with
    fuel = limits.fuel
  ; max_tasks = limits.max_tasks
  ; wall_seconds = Chatmd_shell_spec.Duration.to_seconds limits.wall_time
  ; max_value_bytes =
      Chatmd_shell_spec.Duration.bytes_to_int64 limits.max_value_bytes |> Int64.to_int_exn
  ; max_array_items = limits.max_array_items
  ; max_depth = limits.max_depth
  }
;;

let create
      ?observer
      ~env
      ~definition
      ~input
      ~(capabilities : Operation_worker.Capabilities.t)
      ~script_tools
      ~now
      ~is_halted
      ~execution_limits
      ~admit
      ~revalidate
      ~prepare_outcome
      ~moderate_tool
      ()
  =
  let cache = Stream_invocation.cache () in
  let find name =
    List.find (EC.prepared_tools definition) ~f:(fun prepared ->
      String.equal name (EC.declaration prepared).name
      &&
      match (EC.declaration prepared).implementation with
      | Standalone _ -> true
      | Moderator _ -> false)
  in
  let prepare prepared (request : D.request) =
    if Option.is_some request.source || Option.is_some request.parent_call_id
    then
      raise
        (Dispatch_error
           (Agent_protocol.Error.create
              Permission_denied
              ~message:"standalone invocation requires its owning persisted session"
              ~retryable:false
              ()));
    Stream_invocation.prepare cache ~capabilities request ~create:(fun request ->
      let open Result.Let_syntax in
      let%bind completion_contract =
        Standalone_completion_contract.capture
          ~prepared
          ~current_capabilities:(Script_tool_calls.current_capabilities script_tools)
      in
      let value =
        Stream_invocation.parse_input ~kind:request.kind ~payload:request.payload
      in
      Stream_invocation.create
        ~completion_contract:(Some completion_contract)
        ~input
        ~request
        ~implementation_revision:(EC.fingerprint prepared)
        ~capability_fingerprint:
          (Chat_response.Tool_capability.fingerprint (EC.capabilities prepared))
        ~now
        ~value:(Result.ok value |> Option.value ~default:`Null))
    |> require
  in
  let validate prepared ~kind ~payload =
    let open Result.Let_syntax in
    let%bind () =
      match kind with
      | Chat_response.Tool_call.Kind.Function -> Ok ()
      | Custom -> Error "standalone tools require function calls"
    in
    let%bind value = Stream_invocation.parse_input ~kind ~payload in
    ABI.prepare_input ~prepared ~limits:(EC.execution_limits prepared) value
    |> Result.map ~f:ignore
  in
  let commit_call request =
    match find request.D.name with
    | None -> false
    | Some prepared ->
      ignore (prepare prepared request : I.t);
      true
  in
  let validate_original ~kind ~name ~payload =
    match find name with
    | None -> Ok ()
    | Some prepared -> validate prepared ~kind ~payload
  in
  let run ?run_native:_ (request : D.request) ~authorize =
    match find request.name with
    | None -> None
    | Some prepared ->
      let invocation = prepare prepared request in
      let resolved, runtime_requests =
        Chat_response.Runtime_request_scope.collect (fun () ->
          capabilities.with_invocation ~invocation (fun ~dispatched ->
            let execute control =
              Script_tool_calls.with_job_scope
                script_tools
                ~owner:(Agent_protocol.Job.Invocation dispatched.context.id)
                ~selected:(EC.capabilities prepared)
                ~error:(fail "invocation.background_unavailable")
                (fun jobs ->
                   let open Result.Let_syntax in
                   let start_effects = ref [] in
                   let%bind () =
                     checked
                       (fail "invocation.invalid_input" "The tool arguments are invalid.")
                       (fun () ->
                          validate prepared ~kind:request.kind ~payload:request.payload)
                   in
                   let check_halted () =
                     checked
                       (fail "invocation.session_ended" "The session has ended.")
                       (fun () -> if is_halted () then Error () else Ok ())
                   in
                   let%bind () = check_halted () in
                   let%bind () =
                     checked
                       (fail
                          "invocation.permission_denied"
                          "Tool execution was not authorized.")
                       (fun () ->
                          let%bind () = admit request in
                          authorize ();
                          revalidate request)
                   in
                   let%bind () = check_halted () in
                   let%bind scope =
                     checked
                       (fail
                          "invocation.stale_binding"
                          "The standalone handler binding is invalid.")
                       (fun () ->
                          ABI.create_standalone
                            ~control
                            ~prepared
                            ~invocation:dispatched
                            ~limits:(EC.execution_limits prepared)
                            ~validate_work:(fun work ->
                              match jobs with
                              | None -> Error "background completion is not installed"
                              | Some jobs -> Script_job_service.validate_work jobs work))
                   in
                   let run on_tool_call =
                     let handlers =
                       { R.default_handlers with
                         on_tool_call =
                           (fun _ ~name ~args ->
                             let%bind args =
                               Chatml.Chatml_value_codec.export_json ?control args
                             in
                             let%map result = on_tool_call ~name ~args in
                             match result with
                             | Chat_response.Moderation.Capabilities.Tool_ok value ->
                               L.VVariant
                                 ( "Ok"
                                 , [ Chatml.Chatml_value_codec.import_json ?control value
                                   ] )
                             | Tool_error message ->
                               L.VVariant ("Error", [ L.VString message ]))
                       }
                     in
                     let config : R.runtime_config =
                       { surface = Chatml.Chatml_extension_surface.tool_v1
                       ; operations = R.default_operations ~handlers ()
                       }
                     in
                     let config =
                       match jobs with
                       | None -> config
                       | Some jobs -> Script_job_service.install ?control jobs config
                     in
                     let prepare_result =
                       Option.map jobs ~f:(fun _ ~value:_ ~local_effects ->
                         start_effects := local_effects;
                         Ok ignore)
                     in
                     let entrypoint =
                       match (EC.declaration prepared).implementation with
                       | Standalone { entrypoint; _ } -> entrypoint
                       | Moderator _ -> assert false
                     in
                     Chatml_execution.run_in_scope
                       ?prepare_result
                       ~control
                       ~config
                       ~program:(EC.program prepared)
                       ~entrypoint
                       ~arguments:[ ABI.context scope; ABI.input scope ]
                       ()
                   in
                   let%bind value =
                     Script_tool_calls.with_standalone
                       ?observer
                       script_tools
                       ~prepared
                       ~capabilities
                       ~parent:dispatched
                       ~moderate:(fun call ->
                         let%map outcome = moderate_tool dispatched call in
                         match outcome with
                         | None -> None
                         | Some outcome ->
                           Chat_response.Runtime_request_scope.emit
                             outcome.Chat_response.Moderation.Outcome.runtime_requests
                           |> Result.ok_or_failwith;
                           (match
                              Chat_response.Runtime_semantics.should_end_session
                                outcome.runtime_requests
                            with
                            | Some _ ->
                              Some
                                (Chat_response.Moderation.Tool_moderation.Reject
                                   "The session has ended.")
                            | None -> outcome.tool_moderation))
                       run
                     |> Result.map_error ~f:(fun error ->
                       fail error.Chatml_execution.code error.message)
                   in
                   let%bind outcome =
                     checked
                       (fail
                          "invocation.invalid_output"
                          "The standalone handler returned an invalid outcome.")
                       (fun () -> ABI.decode_outcome ?control scope value)
                   in
                   let%bind () =
                     checked
                       (fail
                          "invocation.disclosure_rejected"
                          "The tool outcome did not pass the host output policy.")
                       (fun () -> prepare_outcome outcome)
                   in
                   let%map () =
                     checked
                       (fail
                          "invocation.background_rejected"
                          "The background starts could not be committed.")
                       (fun () ->
                          match jobs with
                          | None -> Ok ()
                          | Some jobs ->
                            let%bind ordinary =
                              Script_job_service.select jobs !start_effects
                            in
                            (match ordinary with
                             | [] -> Ok ()
                             | _ ->
                               Error
                                 "standalone handler returned unsupported local effects"))
                   in
                   outcome)
            in
            match
              Stream_invocation.rejection_outcome
                (Stream_invocation.preparation request.rejection)
            with
            | Some outcome -> Ok outcome
            | None ->
              Ok
                (match
                   Chatml_execution.with_control
                     ~policy:(Bounded (execution_limits prepared))
                     ~env
                     execute
                   |> Result.map_error ~f:(fun error -> fail error.code error.message)
                   |> Result.join
                 with
                 | Ok outcome | Error outcome -> outcome))
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
  D.{ for_fork = None; commit_call; prepare_call = None; validate_original; run }
;;
