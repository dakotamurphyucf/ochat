open Core
module I = Agent_protocol.Invocation
module H = Agent_protocol.Host_invocation
module C = Chat_response.Tool_capability
module S = Chatmd_shell_spec.Tool_schema

let fail code message = I.Fail { code; message; retryable = false; details = `Null }

let checked failure f =
  match f () with
  | Ok value -> Ok value
  | Error _ -> Error failure
  | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
  | exception _ -> Error failure
;;

type 'a managed_target =
  { prepared : Chat_response.Extension_compiler.t
  ; run :
      (validate_work:(I.work -> (unit, string) result)
       -> outcome:I.outcome
       -> selected_jobs:Agent_protocol.Id.Job.t list
       -> ('a, I.outcome) result)
      -> ('a, I.outcome) result
  }

let run_policy
      ~run_native
      ~on_progress
      ~native_invocation
      ~managed_revision
      ~managed_admit
      ~execute
      ~registry
      ~(reference : C.reference)
      ~invocation
      ~check_halted
      ~context
      ~kind_matches
      ~preparation
      ~with_selected
      ~with_native_admission
      ~authorize
      ~prepare_output
      ~complete
  =
  execute ~invocation (fun ~dispatched ->
    let call : H.context = context dispatched in
    let execute_native () =
      let open Result.Let_syntax in
      let%bind () = check_halted () in
      let resolve () =
        checked
          (fail
             "invocation.stale_binding"
             "The selected tool capability is no longer valid.")
          (fun () ->
             let%bind selected = registry () in
             let%bind binding =
               C.resolve selected ~id:reference.id ~fingerprint:reference.fingerprint
             in
             let%bind implementation_revision = managed_revision binding in
             if
               kind_matches dispatched binding
               && String.equal call.tool_name reference.name
               && String.equal call.implementation_revision implementation_revision
               && String.equal call.capability_fingerprint (C.fingerprint selected)
             then Ok (binding, selected)
             else
               Error
                 C.
                   { code = "capability.stale_context"
                   ; message = "invocation context does not match selected capability"
                   })
      in
      let%bind binding, selected = resolve () in
      let target binding selected =
        match C.implementation binding, managed_admit with
        | Native implementation, _ -> Ok (`Native implementation)
        | Managed _, None ->
          Error
            (fail
               "invocation.managed_dispatch_required"
               "This tool requires its owned extension dispatcher.")
        | Managed _, Some admit ->
          checked
            (fail
               "invocation.stale_binding"
               "The managed tool definition is no longer valid.")
            (fun () ->
               admit binding selected dispatched
               |> Result.map ~f:(fun target -> `Managed target))
      in
      let%bind _ = target binding selected in
      with_selected selected (fun () ->
        let%bind () =
          checked
            (fail
               "invocation.invalid_input"
               "The tool arguments do not satisfy its input schema.")
            (fun () ->
               let%bind schema = S.compile reference.input_schema in
               S.validate schema call.input)
        in
        let%bind () =
          checked
            (fail "invocation.permission_denied" "Tool execution was not authorized.")
            (fun () -> authorize dispatched binding)
        in
        (* An approval wait may have replaced or narrowed the selected registry.
         Never dispatch the binding captured before that wait. *)
        let%bind () = check_halted () in
        let%bind binding, selected = resolve () in
        let%bind target = target binding selected in
        let validate_output ~validate_work ~selected_jobs output =
          let%bind value =
            checked
              (fail
                 "invocation.disclosure_rejected"
                 "The tool result could not be disclosed.")
              (fun () -> prepare_output output)
          in
          let%bind outcome =
            checked
              (fail "invocation.invalid_output" "The tool returned an invalid result.")
              (fun () ->
                 let%bind () = I.validate_outcome (I.Complete value) in
                 match C.result_contract binding with
                 | Native_output -> Ok (I.Complete value)
                 | Invocation_v1 ->
                   (match value with
                    | `String encoded ->
                      I.outcome_of_json (Jsonaf.of_string encoded)
                      |> Result.bind ~f:(function
                        | (I.Complete _ | Fail _ | Cancelled _) as outcome -> Ok outcome
                        | Pending (work, _) as outcome ->
                          validate_work work
                          |> Result.map_error ~f:Agent_protocol.Error.invalid_request
                          |> Result.map ~f:(fun () -> outcome))
                    | _ ->
                      Error
                        (Agent_protocol.Error.invalid_request
                           "expected disclosed native outcome text")))
          in
          let%bind () =
            checked
              (fail "invocation.invalid_output" "The tool returned an invalid result.")
              (fun () ->
                 let%bind () = I.validate_outcome outcome in
                 match target, outcome with
                 | `Managed managed, (Complete value | Pending (_, value)) ->
                   S.validate
                     (Chat_response.Extension_compiler.output_schema managed.prepared)
                     value
                   |> Result.map_error ~f:(fun _ ->
                     Agent_protocol.Error.invalid_request
                       "disclosed managed result does not match its schema")
                 | _ -> Ok ())
          in
          complete outcome ~selected_jobs
          |> Result.map_error ~f:(fun _ ->
            fail
              "invocation.selection_failed"
              "The validated tool outcome could not be selected.")
        in
        match target with
        | `Native implementation ->
          let%bind () = check_halted () in
          let%bind payload =
            match implementation.info.type_, call.input with
            | "function", input -> Ok (Jsonaf.to_string input)
            | "custom", `String input -> Ok input
            | "custom", _ ->
              Error (fail "invocation.invalid_input" "Custom tools require string input.")
            | _ ->
              Error
                (fail
                   "invocation.unsupported_kind"
                   "The registered tool kind is unsupported.")
          in
          let%bind output =
            with_native_admission dispatched binding (fun () ->
              checked
                (fail "invocation.handler_failed" "Tool execution failed.")
                (fun () ->
                   match run_native with
                   | Some run_native -> Ok (run_native implementation ~payload)
                   | None ->
                     let active = Atomic.make true in
                     let invocation =
                       match native_invocation, on_progress with
                       | Some original, _ ->
                         let scoped =
                           if Ochat_function.Invocation.is_observed original
                           then
                             Ochat_function.Invocation.create_strict_with_trace
                               ~progress:(fun update ->
                                 if Atomic.get active
                                 then Ochat_function.Invocation.emit original update)
                               ~trace:(fun trace ->
                                 if Atomic.get active
                                 then Ochat_function.Invocation.emit_trace original trace)
                           else Ochat_function.Invocation.silent
                         in
                         Option.value_map
                           (Ochat_function.Invocation.inference_parent original)
                           ~default:scoped
                           ~f:(fun parent ->
                             Ochat_function.Invocation.with_inference_parent
                               scoped
                               ~parent)
                       | None, None -> Ochat_function.Invocation.silent
                       | None, Some emit ->
                         Ochat_function.Invocation.create (fun update ->
                           if Atomic.get active then emit dispatched update)
                     in
                     Exn.protect
                       ~finally:(fun () -> Atomic.set active false)
                       ~f:(fun () ->
                         Ok (implementation.run_with_progress ~invocation payload))))
          in
          validate_output
            ~validate_work:(fun _ ->
              Error "native pending work requires an ownership validator")
            ~selected_jobs:[]
            output
        | `Managed managed ->
          let%bind () = check_halted () in
          let%bind result =
            checked (fail "invocation.handler_failed" "Tool execution failed.") (fun () ->
              Ok
                (managed.run (fun ~validate_work ~outcome ~selected_jobs ->
                   let%bind () =
                     checked
                       (fail
                          "invocation.invalid_output"
                          "The tool returned an invalid result.")
                       (fun () -> I.validate_outcome outcome)
                   in
                   validate_output
                     ~validate_work
                     ~selected_jobs
                     (Openai.Responses.Tool_output.Output.Text
                        (Jsonaf.to_string (I.outcome_to_json outcome))))))
          in
          result)
    in
    match Option.bind (preparation dispatched) ~f:Stream_invocation.rejection_outcome with
    | Some outcome -> complete outcome ~selected_jobs:[]
    | None ->
      (match execute_native () with
       | Ok value -> Ok value
       | Error outcome -> complete outcome ~selected_jobs:[]))
;;
