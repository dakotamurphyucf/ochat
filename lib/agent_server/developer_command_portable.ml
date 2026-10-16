open! Core
module P = Agent_protocol
module Portable = Chatml_developer_test
module Capture = Chatml_developer_capture
module Receipt = Agent_store.Developer_command_receipt
module Diagnostic = Agent_store.Developer_command_diagnostic
module Fixture_value = Agent_store.Developer_command_fixture
module Budgets = Developer_command_budgets

let resource_limit message =
  P.Error.create P.Error.Resource_limit ~message ~retryable:false ()
;;

let persistence_error message =
  P.Error.create P.Error.Persistence_error ~message ~retryable:false ()
;;

let receipt_error = function
  | Document_schema.Error.Limit_exceeded _ ->
    resource_limit "developer command result exceeds its admitted limit"
  | Invalid_configuration _
  | Malformed _
  | Duplicate_key _
  | Unsupported_beta_format
  | Unsupported_format _
  | Invalid_field _
  | Unsupported_kind _
  | Unsupported_version _
  | Missing_conversion _
  | Wrong_kind _
  | Wrong_version _
  | Required_semantics_unknown _
  | Extension_conflict _ ->
    persistence_error "developer command result could not be retained"
;;

let project_result budgets value =
  Receipt.create value ~limits:(Budgets.receipt budgets) ~poll:Eio.Fiber.yield
;;

let project budgets value =
  Result.map_error (project_result budgets value) ~f:receipt_error
;;

let diagnostic capture (value : Portable.Diagnostic.t) =
  let location =
    Option.bind (Portable.Diagnostic.location value) ~f:(fun location ->
      let source_id = Chatml_source.Location.source location in
      match Chatml_prepared_program.find_source (Capture.prepared capture) source_id with
      | None -> None
      | Some source ->
        let digest = Chatml_source.Location.source_digest location in
        if not (Chatml_source.Digest.equal digest (Chatml_source.digest source))
        then None
        else (
          let span = Chatml_source.Location.span location in
          Some
            { Diagnostic.Location.source = source_id
            ; digest
            ; source_bytes = String.length (Chatml_source.contents source)
            ; start_byte = span.left.offset
            ; end_byte = span.right.offset
            }))
  in
  { Diagnostic.code = Portable.Diagnostic.code value
  ; message = Portable.Diagnostic.message value
  ; location
  }
;;

module Check = struct
  type t = { checked : Portable.Check.t }

  let run ~capture ~id ~budgets ~env =
    let open Result.Let_syntax in
    let%bind check_id =
      Portable.Check.Id.create (Receipt.Id.to_string id)
      |> Result.map_error ~f:(fun _ -> P.Error.invalid_request "invalid check identity")
    in
    let checked =
      Portable.Check.perform
        capture
        ~id:check_id
        ~policy:(Budgets.compilation budgets)
        ~max_dependency_depth:(Budgets.max_dependency_depth budgets)
        ~env
    in
    let value =
      match Portable.Check.outcome checked with
      | Portable.Check.Outcome.Ready -> Receipt.Check Receipt.Check.Ready
      | Rejected value ->
        Receipt.Check (Receipt.Check.Rejected (diagnostic capture value))
    in
    let%map receipt =
      match project_result budgets value with
      | Ok receipt -> Ok receipt
      | Error (Document_schema.Error.Limit_exceeded _) ->
        (match Portable.Check.outcome checked with
         | Portable.Check.Outcome.Ready ->
           Error (resource_limit "developer check result exceeds its admitted limit")
         | Rejected _ ->
           project budgets Developer_command_projection_fallback.check_rejected)
      | Error error -> Error (receipt_error error)
    in
    { checked }, receipt
  ;;

  let checked t = t.checked
end

module Fixture = struct
  let failure capture = function
    | Portable.Failure.Compile value ->
      Fixture_value.Failure.Compile (diagnostic capture value)
    | Fixture_mismatch { expected_step; actual_tool } ->
      Fixture_mismatch { expected_step; actual_tool }
    | Fixture_missing { next_step } -> Fixture_missing { next_step }
    | Unsupported_effect { operation } -> Unsupported_effect { operation }
    | Runtime value -> Runtime (diagnostic capture value)
    | Invalid_result value -> Invalid_result (diagnostic capture value)
    | Pending_without_owner -> Pending_without_owner
    | Admission_revoked -> Admission_revoked
    | Output_limit -> Output_limit
  ;;

  let level = function
    | Chatml_host_runtime.Debug -> Fixture_value.Log.Level.Debug
    | Info -> Info
    | Warn -> Warn
    | Error_level -> Error
  ;;

  let receipt capture (value : Portable.Receipt.t) =
    let outcome =
      match Portable.Receipt.outcome value with
      | Portable.Receipt.Succeeded output -> Fixture_value.Succeeded output
      | Tool_failed error -> Fixture_value.Tool_failed error
      | Failed problem -> Fixture_value.Failed (failure capture problem)
    in
    let logs =
      List.map (Portable.Receipt.logs value) ~f:(fun log ->
        { Fixture_value.Log.level = level (Portable.Receipt.Log.level log)
        ; message = Portable.Receipt.Log.message log
        })
    in
    Receipt.Fixture
      (Fixture_value.Completed
         { graph_fingerprint = Portable.Receipt.graph_fingerprint value
         ; fixture_digest = Portable.Receipt.fixture_digest value
         ; tool_selection_fingerprint = Portable.Receipt.tool_selection_fingerprint value
         ; matched_steps = Portable.Receipt.matched_steps value
         ; logs
         ; outcome
         })
  ;;

  let output_limited (value : Portable.Receipt.t) =
    Developer_command_projection_fallback.fixture_output_limit
      ~tool_selection_fingerprint:(Portable.Receipt.tool_selection_fingerprint value)
      ~graph_fingerprint:(Portable.Receipt.graph_fingerprint value)
      ~fixture_digest:(Portable.Receipt.fixture_digest value)
      ~matched_steps:(Portable.Receipt.matched_steps value)
  ;;

  let run ~capture ~check ~tool_selection ~admit ~id ~budgets ~env =
    let open Result.Let_syntax in
    let%bind run_id =
      Portable.Run_id.create (Receipt.Id.to_string id)
      |> Result.map_error ~f:(fun _ -> P.Error.invalid_request "invalid fixture identity")
    in
    let%bind () =
      if admit ()
      then Ok ()
      else
        Error
          (P.Error.create
             P.Error.Permission_denied
             ~message:"developer fixture admission changed"
             ~retryable:false
             ())
    in
    let%bind completed =
      (match tool_selection with
       | None ->
         Portable.run
           ~admit
           capture
           ~check
           ~id:run_id
           ~limits:(Budgets.execution budgets)
           ~max_output_bytes:(Budgets.max_output_bytes budgets)
           ~env
       | Some selection ->
         Portable.Tool_v1.run
           ~admit
           capture
           ~check
           ~selection
           ~id:run_id
           ~limits:(Budgets.execution budgets)
           ~max_output_bytes:(Budgets.max_output_bytes budgets)
           ~env)
      |> Result.map_error ~f:(function
        | Portable.Error.No_fixtures ->
          P.Error.invalid_request "developer fixture run requires admitted fixtures"
        | Mismatched_check ->
          P.Error.invalid_request "developer fixture check does not match capture"
        | Invalid_selection ->
          P.Error.invalid_request "developer fixture Tool_v1 selection changed"
        | Invalid_output_limit | Invalid_execution_limits ->
          P.Error.invalid_request "invalid developer fixture budget")
    in
    let candidate = receipt capture completed in
    match project_result budgets candidate with
    | Ok retained -> Ok retained
    | Error (Document_schema.Error.Limit_exceeded _) ->
      project budgets (output_limited completed)
    | Error error -> Error (receipt_error error)
  ;;
end

let run_kind ~kind ~capture ~tool_selection ~admit ~id ~budgets ~env =
  let open Result.Let_syntax in
  match kind with
  | Receipt.Kind.Check ->
    let%bind () =
      match tool_selection with
      | None -> Ok ()
      | Some _ -> Error (P.Error.invalid_request "check cannot execute Tool_v1 fixture")
    in
    let%map _checked, receipt = Check.run ~capture ~id ~budgets ~env in
    receipt
  | Fixture_run ->
    let%bind checked, _ = Check.run ~capture ~id ~budgets ~env in
    Fixture.run
      ~capture
      ~check:(Check.checked checked)
      ~tool_selection
      ~admit
      ~id
      ~budgets
      ~env
  | Live_run ->
    Error (P.Error.invalid_request "live developer command requires its physical Root")
;;
