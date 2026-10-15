open! Core
include Developer_command_portable
module P = Agent_protocol
module C = Chat_response.Tool_capability
module O = Developer_root_owner
module N = Developer_native_executor
module Jobs = Developer_script_job_service
module Capture = Chatml_developer_capture
module Compilation = Chatml_compilation
module Execution = Chatml_execution
module Runtime = Chatml_host_runtime
module Codec = Chatml.Chatml_value_codec
module Lang = Chatml.Chatml_lang
module H = P.Host_invocation
module Receipt = Agent_store.Developer_command_receipt

exception Fatal_effect of P.Error.t

module Live = struct
  type t =
    { root : O.run
    ; jobs : Jobs.t
    ; native : N.t
    ; authorize : H.t -> C.binding -> (unit, P.Error.t) result
    ; prepare_output :
        Openai.Responses.Tool_output.Output.t -> (Jsonaf.t, P.Error.t) result
    ; capture : Capture.t
    ; linked : Chatml_linked_program.t
    ; budgets : Developer_command_budgets.t
    ; env : Eio_unix.Stdenv.base
    }

  let invalid message = Error (P.Error.invalid_request message)

  let create ~root ~jobs ~native ~authorize ~prepare_output ~capture ~budgets ~env =
    let open Result.Let_syntax in
    if
      (not (phys_equal (O.capture root) capture))
      || (not (Jobs.issued_by_root jobs root))
      || not (N.issued_by_root native root)
    then invalid "live command resources belong to another physical root"
    else (
      let%bind native = N.with_job_service native jobs in
      match Capture.target capture with
      | Chatml.Chatml_compiler_contract.Target.One_off_v1 ->
        let%map linked =
          Compilation.compile_prepared_with_policy
            (Capture.prepared capture)
            ~policy:(Developer_command_budgets.compilation budgets)
            ~max_dependency_depth:(Developer_command_budgets.max_dependency_depth budgets)
            ~env
            ~target:Compilation.One_off_v1
            ()
          |> Result.map_error ~f:(fun _ ->
            P.Error.invalid_request "developer live script failed compilation")
        in
        { root; jobs; native; authorize; prepare_output; capture; linked; budgets; env }
      | Tool_v1 | Developer_tool_v1 | Moderator_v1 | Delegated_moderator_v1 ->
        invalid "developer live command requires One_off_v1")
  ;;

  let outcome_value ?control ~budget outcome =
    let open Result.Let_syntax in
    let%bind () = P.Invocation.validate_outcome outcome in
    let%bind () =
      P.Json_codec.validate_limits
        ~max_depth:budget.Chatml_execution.max_depth
        ~max_bytes:budget.max_value_bytes
        (P.Invocation.outcome_to_json outcome)
    in
    match outcome with
    | P.Invocation.Complete value ->
      Ok (Lang.VVariant ("Ok", [ Codec.import_json ?control value ]))
    | Pending (Job _, acknowledgement) ->
      Ok (Lang.VVariant ("Ok", [ Codec.import_json ?control acknowledgement ]))
    | Fail error -> Ok (Lang.VVariant ("Error", [ Lang.VString error.message ]))
    | Cancelled reason -> Ok (Lang.VVariant ("Error", [ Lang.VString reason ]))
    | Pending (Subscription _, _) ->
      invalid "developer live Tool.call returned unsupported work"
  ;;

  let call_tool ?control t ~name ~args =
    let open Result.Let_syntax in
    let%bind args =
      Codec.export_json ?control args
      |> Result.map_error ~f:(fun _ ->
        P.Error.invalid_request "invalid developer Tool.call input")
    in
    let%bind witness = O.current t.root in
    let selected = O.Current.selected witness in
    let%bind binding =
      C.find selected ~name
      |> Result.map_error ~f:(fun _ ->
        P.Error.invalid_request "developer Tool.call is not selected")
    in
    let reference = C.reference binding in
    let%bind () = O.check_effect_now t.root witness in
    let%bind invocation =
      H.create_developer
        { H.id = P.Id.Invocation.create ()
        ; owner = P.Execution_owner.Developer (O.owner t.root)
        ; origin = P.Invocation.Script
        ; parent_invocation = None
        ; parent_job = None
        ; tool_name = reference.name
        ; implementation_revision = reference.implementation_revision
        ; capability_fingerprint = C.fingerprint selected
        ; input = args
        ; created_at = O.native_now t.root
        ; deadline = None
        }
    in
    let%bind resolved =
      N.run_root
        t.native
        ~reference
        ~invocation
        ~authorize:t.authorize
        ~prepare_output:t.prepare_output
    in
    let%bind outcome =
      H.outcome resolved
      |> Result.of_option
           ~error:(P.Error.invalid_request "developer Tool.call has no terminal outcome")
    in
    outcome_value ?control ~budget:(Developer_command_budgets.execution t.budgets) outcome
  ;;

  let safe_failure code =
    P.Error.create
      P.Error.Invalid_request
      ~message:("developer live script " ^ code)
      ~retryable:false
      ()
  ;;

  let safe_limit () =
    P.Error.create
      P.Error.Resource_limit
      ~message:"developer live script reached its admitted limit"
      ~retryable:false
      ()
  ;;

  let runtime_error (error : Execution.error) =
    match error.code with
    | "chatml.execution_limit"
    | "chatml.execution_timeout"
    | "chatml.allocation_limit"
    | "chatml.value_limit"
    | "chatml.task_limit"
    | "chatml.call_limit"
    | "chatml.invocation_depth" -> safe_limit ()
    | _ -> safe_failure "failed"
  ;;

  let terminal_for_error (error : P.Error.t) =
    match error.code with
    | P.Error.Resource_limit -> Receipt.Live (Receipt.Live.Limited (safe_limit ()))
    | _ -> Receipt.Live (Receipt.Live.Failed (safe_failure "failed"))
  ;;

  let project_terminal budgets terminal =
    match Developer_command_portable.project_result budgets terminal with
    | Ok receipt -> Ok (receipt, false)
    | Error (Document_schema.Error.Limit_exceeded _) ->
      Developer_command_portable.project
        budgets
        (Receipt.Live (Receipt.Live.Limited (safe_limit ())))
      |> Result.map ~f:(fun receipt -> receipt, true)
    | Error error ->
      let _ = error in
      Error (safe_failure "could not retain result")
  ;;

  let run t ~started =
    let open Result.Let_syntax in
    let%bind () =
      Developer_command_lifecycle.Active.Started.consume_for started ~root:t.root
    in
    let%map receipt, _ordinary =
      Jobs.with_root_action
        t.jobs
        ~phase:Agent_store.Developer_run_store.Checkpoint.Active
        (fun ~scope ~sw:_ ->
           let effects = ref [] in
           let evaluated =
             try
               Execution.with_control
                 ~env:t.env
                 ~policy:(Bounded (Developer_command_budgets.execution t.budgets))
                 (fun control ->
                    let handlers =
                      { Runtime.default_handlers with
                        on_tool_call =
                          (fun _ ~name ~args ->
                            match call_tool ?control t ~name ~args with
                            | Ok value -> Ok value
                            | Error error -> raise (Fatal_effect error))
                      }
                    in
                    let config : Runtime.runtime_config =
                      { surface = Chatml.Chatml_extension_surface.one_off_v1
                      ; operations = Runtime.default_operations ~handlers ()
                      }
                    in
                    let config = Jobs.Scope.install ?control scope config in
                    let argument = Codec.import_json ?control (Capture.input t.capture) in
                    let result =
                      Execution.run_prepared_in_scope
                        t.linked
                        ~control
                        ~config
                        ~entrypoint:(Capture.entrypoint t.capture)
                        ~arguments:[ argument ]
                        ~prepare_result:(fun ~value:_ ~local_effects ->
                          effects := local_effects;
                          Ok Fn.id)
                        ()
                    in
                    match result with
                    | Error error -> Error (runtime_error error)
                    | Ok value ->
                      (match Codec.export_json ?control value with
                       | Error _ -> Error (safe_failure "returned invalid output")
                       | Ok output ->
                         P.Json_codec.validate_limits
                           ~max_depth:
                             (Developer_command_budgets.execution t.budgets).max_depth
                           ~max_bytes:
                             (Developer_command_budgets.max_output_bytes t.budgets)
                           output
                         |> Result.map ~f:(fun () -> output)
                         |> Result.map_error ~f:(fun _ -> safe_limit ())))
             with
             | Fatal_effect error -> Ok (Error error)
           in
           let terminal, selected =
             match evaluated with
             | Error error -> terminal_for_error (runtime_error error), []
             | Ok (Error error) -> terminal_for_error error, []
             | Ok (Ok value) -> Receipt.Live (Receipt.Live.Succeeded value), !effects
           in
           let%map receipt, truncated = project_terminal t.budgets terminal in
           receipt, if truncated then [] else selected)
    in
    receipt
  ;;
end
