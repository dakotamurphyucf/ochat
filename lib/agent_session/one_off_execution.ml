open Core
module I = Agent_protocol.Invocation
module P = Chat_response.One_off_script
module C = Chat_response.Tool_capability
module N = Native_tool_invocation
module R = Chatml_host_runtime
module L = Chatml.Chatml_lang
module V = Chatml.Chatml_value_codec
module S = Chatmd_shell_spec.Chatmd_script_spec
module Duration = Chatmd_shell_spec.Duration

type result =
  { resolved : I.t
  ; runtime_requests : Chat_response.Moderation.Runtime_request.t list
  }

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

let run
      ?observer
      ?(allocation_bytes = Chatml_execution.default_limits.allocation_bytes)
      ?(max_invocation_depth = Chatml_execution.default_limits.max_invocation_depth)
      ~env
      ~prepared
      ~borrowed
      ~script_tools
      ~input
      ~(limits : S.limits)
      ~max_nested_calls
      ~now
      ~moderate_tool
      ~prepare_outcome
      ()
  =
  let open Result.Let_syntax in
  let wall_seconds = Duration.to_seconds limits.wall_time in
  let%bind () =
    if
      (not (Float.is_finite wall_seconds))
      || Float.(wall_seconds <= 0.)
      || limits.fuel <= 0
      || limits.max_tasks < 0
      || limits.max_depth <= 0
      || limits.max_array_items <= 0
      || allocation_bytes <= 0
      || max_nested_calls < 0
      || max_invocation_depth <= 0
      || Int64.(Duration.bytes_to_int64 limits.max_value_bytes <= 0L)
      || Int64.(Duration.bytes_to_int64 limits.max_output_bytes <= 0L)
    then Error (Agent_protocol.Error.invalid_request "invalid one-off execution policy")
    else Ok ()
  in
  let%bind borrowed =
    N.select_tools
      borrowed
      ~names:
        (List.map
           (C.references (P.capabilities prepared))
           ~f:(fun reference -> reference.name))
  in
  let%bind ceiling = N.borrowed_capabilities borrowed in
  let%bind () =
    P.revalidate prepared ~capabilities:ceiling
    |> Result.map_error ~f:(fun error ->
      Agent_protocol.Error.invalid_request error.C.message)
  in
  let parent = N.borrowed_invocation borrowed in
  let created_at = now () in
  let deadline =
    let requested =
      Time_ns.add
        (Agent_protocol.Timestamp.to_time_ns created_at)
        (Time_ns.Span.of_sec wall_seconds)
      |> Agent_protocol.Timestamp.of_time_ns
    in
    match parent.context.deadline with
    | Some inherited when Agent_protocol.Timestamp.compare inherited requested < 0 ->
      inherited
    | None | Some _ -> requested
  in
  let%bind invocation =
    I.create
      { id = Agent_protocol.Id.Invocation.create ()
      ; session_id = parent.context.session_id
      ; generation = parent.context.generation
      ; origin = Script
      ; provider_call_id = None
      ; call_entry_id = None
      ; parent_invocation = Some parent.context.id
      ; parent_job = None
      ; tool_name = "chatml.main"
      ; implementation_revision = P.fingerprint prepared
      ; capability_fingerprint = C.fingerprint ceiling
      ; input
      ; created_at
      ; deadline = Some deadline
      }
  in
  let requests = ref [] in
  let%map resolved =
    N.execute_borrowed borrowed ~invocation (fun ~dispatched ->
      let remaining =
        Time_ns.diff
          (Agent_protocol.Timestamp.to_time_ns deadline)
          (Agent_protocol.Timestamp.to_time_ns (now ()))
        |> Time_ns.Span.to_sec
      in
      let execution_limits : Chatml_execution.limits =
        { fuel = limits.fuel
        ; max_tasks = limits.max_tasks
        ; wall_seconds = remaining
        ; max_value_bytes =
            Duration.bytes_to_int64 limits.max_value_bytes |> Int64.to_int_exn
        ; max_array_items = limits.max_array_items
        ; max_depth = limits.max_depth
        ; allocation_bytes
        ; max_calls = max_nested_calls
        ; max_invocation_depth
        }
      in
      let execute control =
        Script_tool_calls.with_job_scope
          script_tools
          ~owner:(Agent_protocol.Job.Invocation dispatched.context.id)
          ~selected:(P.capabilities prepared)
          ~error:(fail "invocation.background_unavailable")
          (fun jobs ->
             let start_effects = ref [] in
             let%bind () =
               checked
                 (fail
                    "invocation.stale_binding"
                    "Selected script tools are no longer available.")
                 (fun () -> Script_tool_calls.validate_one_off script_tools prepared)
             in
             let%bind () =
               checked
                 (fail "invocation.session_ended" "The session has ended.")
                 (fun () ->
                    if Script_tool_calls.is_halted script_tools then Error () else Ok ())
             in
             let%bind argument =
               checked
                 (fail
                    "invocation.invalid_input"
                    "The one-off input exceeds its value limits.")
                 (fun () ->
                    let%bind () =
                      I.validate_outcome (Complete input)
                      |> Result.map_error ~f:(fun _ -> "invalid input")
                    in
                    let value = V.import_json ?control input in
                    let%map _ =
                      Chat_response.Moderator_invocation.snapshot_state ~limits value
                    in
                    value)
             in
             let%bind child_borrow =
               checked
                 (fail "invocation.inactive_scope" "The script invocation scope expired.")
                 N.borrow
             in
             let run on_tool_call =
               let handlers =
                 { R.default_handlers with
                   on_tool_call =
                     (fun _ ~name ~args ->
                       let%bind args = V.export_json ?control args in
                       let%map result = on_tool_call ~name ~args in
                       match result with
                       | Chat_response.Moderation.Capabilities.Tool_ok value ->
                         L.VVariant ("Ok", [ V.import_json ?control value ])
                       | Tool_error message -> L.VVariant ("Error", [ L.VString message ]))
                 }
               in
               let config : R.runtime_config =
                 { surface = Chatml.Chatml_extension_surface.one_off_v1
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
               Chatml_execution.run_in_scope
                 ?prepare_result
                 ~control
                 ~config
                 ~program:(P.program prepared)
                 ~entrypoint:P.entrypoint
                 ~arguments:[ argument ]
                 ()
             in
             let%bind value =
               Script_tool_calls.with_one_off
                 ?observer
                 script_tools
                 ~prepared
                 ~limits
                 ~max_nested_calls
                 ~borrowed:child_borrow
                 ~moderate:(fun call ->
                   let%map outcome = moderate_tool dispatched call in
                   match outcome with
                   | None -> None
                   | Some outcome ->
                     requests
                     := !requests
                        @ outcome.Chat_response.Moderation.Outcome.runtime_requests;
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
                    "The one-off program returned invalid JSON.")
                 (fun () ->
                    let%bind value = V.export_json ?control value in
                    let outcome = I.Complete value in
                    let%map () =
                      I.validate_outcome outcome
                      |> Result.map_error ~f:(fun _ -> "invalid output")
                    in
                    outcome)
             in
             let%bind () =
               if
                 Int64.(
                   of_int (String.length (Jsonaf.to_string (I.outcome_to_json outcome)))
                   > Duration.bytes_to_int64 limits.max_output_bytes)
               then
                 Error
                   (fail
                      "invocation.output_limit"
                      "The one-off result exceeds its output limit.")
               else Ok ()
             in
             let%bind () =
               checked
                 (fail
                    "invocation.disclosure_rejected"
                    "The one-off result could not be disclosed.")
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
                      let%bind ordinary = Script_job_service.select jobs !start_effects in
                      (match ordinary with
                       | [] -> Ok ()
                       | _ -> Error "one-off returned unsupported local effects"))
             in
             outcome)
      in
      let outcome =
        if Float.(remaining <= 0.)
        then fail "chatml.execution_timeout" "The script deadline elapsed."
        else (
          match
            Chatml_execution.with_control
              ~policy:(Bounded execution_limits)
              ~context:(N.borrowed_execution_context borrowed)
              ~env
              execute
            |> Result.map_error ~f:(fun error -> fail error.code error.message)
            |> Result.join
          with
          | Ok outcome | Error outcome -> outcome
          | exception Eio.Time.Timeout ->
            fail "chatml.execution_timeout" "The script deadline elapsed.")
      in
      Ok outcome)
  in
  { resolved; runtime_requests = !requests }
;;
