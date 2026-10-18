open! Core
module P = Agent_protocol
module C = Chat_response.Tool_capability
module O = Developer_root_owner
module N = Developer_native_executor
module Jobs = Developer_script_job_service
module Repl = Developer_repl_epoch
module Evaluator = Chatml_repl_execution
module Runtime = Chatml_host_runtime
module Codec = Chatml.Chatml_value_codec
module Lang = Chatml.Chatml_lang
module H = P.Host_invocation

let invalid message = P.Error.invalid_request message

let outcome_value outcome =
  let open Result.Let_syntax in
  let%bind () = P.Invocation.validate_outcome outcome in
  match outcome with
  | P.Invocation.Complete value -> Ok (Lang.VVariant ("Ok", [ Codec.import_json value ]))
  | Pending (Job _, acknowledgement) ->
    Ok (Lang.VVariant ("Ok", [ Codec.import_json acknowledgement ]))
  | Fail error -> Ok (Lang.VVariant ("Error", [ Lang.VString error.message ]))
  | Cancelled reason -> Ok (Lang.VVariant ("Error", [ Lang.VString reason ]))
  | Pending (Subscription _, _) ->
    Error (invalid "REPL Task Tool.call returned unsupported work")
;;

let run
      ~root
      ~jobs
      ~native
      ~authorize
      ~prepare_output
      ~borrowed
      ~origin
      ~handle
      ~started
      ~action_policy
      ~check_current
  =
  let open Result.Let_syntax in
  if not (Jobs.issued_by_root jobs root && N.issued_by_root native root)
  then Error (invalid "REPL Task resources belong to another root")
  else (
    let%bind () = Developer_command_lifecycle.Active.Started.consume_for started ~root in
    let%bind native = N.with_job_service native jobs in
    let check () =
      let%bind () = Repl.check_now borrowed in
      let%bind () =
        Repl.Task_origin.check_current
          origin
          ~actor:(Repl.current_actor borrowed)
          ~checkpoint:(fun () -> ())
      in
      let%bind () = check_current () in
      let%bind witness = O.current root in
      let%bind () = O.check_effect_now root witness in
      Repl.check_now borrowed
    in
    let call_tool ~name ~args =
      let%bind () = check () in
      let%bind args =
        Codec.export_json args
        |> Result.map_error ~f:(fun _ -> invalid "invalid REPL Task Tool.call input")
      in
      let%bind witness = O.current root in
      let selected = O.Current.selected witness in
      let%bind binding =
        C.find selected ~name
        |> Result.map_error ~f:(fun _ -> invalid "REPL Task Tool.call is not selected")
      in
      let reference = C.reference binding in
      let%bind () = O.check_effect_now root witness in
      let%bind () = Repl.check_now borrowed in
      let%bind invocation =
        H.create_developer
          { H.id = P.Id.Invocation.create ()
          ; owner = P.Execution_owner.Developer (O.owner root)
          ; origin = P.Invocation.Script
          ; parent_invocation = None
          ; parent_job = None
          ; tool_name = reference.name
          ; implementation_revision = reference.implementation_revision
          ; capability_fingerprint = C.fingerprint selected
          ; input = args
          ; created_at = O.native_now root
          ; deadline = None
          }
      in
      let%bind resolved =
        N.run_root native ~reference ~invocation ~authorize ~prepare_output
      in
      let%bind () = check () in
      let%bind outcome =
        H.outcome resolved
        |> Result.of_option ~error:(invalid "REPL Task Tool.call has no terminal outcome")
      in
      outcome_value outcome
    in
    let%map value, _ordinary =
      O.with_repl_task_borrow root borrowed (fun () ->
        Jobs.with_root_action
          jobs
          ~phase:Agent_store.Developer_run_store.Checkpoint.Active
          ~check_parent_now:(fun () -> Repl.check_now borrowed)
          (fun ~scope ~sw:_ ->
             let effects = ref [] in
             let handlers =
               { Runtime.default_handlers with
                 on_tool_call =
                   (fun _ ~name ~args ->
                     call_tool ~name ~args
                     |> Result.map_error ~f:(fun error -> error.P.Error.message))
               }
             in
             let config : Runtime.runtime_config =
               { surface = Repl.execution_surface borrowed
               ; operations = Runtime.default_operations ~handlers ()
               }
             in
             let config = Jobs.Scope.install scope config in
             let%bind () = check () in
             let%bind result =
               Evaluator.drive_retained
                 (Repl.evaluator borrowed)
                 handle
                 ~operations:config.operations
                 ~scoped_async_operation:Runtime.Model_spawn
                 ~prepare_result:(fun ~value:_ ~local_effects ->
                   effects := local_effects;
                   Ok Fn.id)
                 ~action_policy
                 ()
               |> Result.map_error ~f:(fun _ ->
                 invalid "REPL Task interpreter is unavailable")
             in
             let%map () = check () in
             ( result
             , match Evaluator.Task_result.outcome result with
               | Succeeded -> !effects
               | Failed _ -> [] )))
    in
    value)
;;
