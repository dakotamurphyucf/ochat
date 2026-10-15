open! Core
module P = Agent_protocol
module Store = Agent_store.Developer_run_store

type settings =
  { compilation : Chatml_compilation.policy
  ; max_dependency_depth : int
  ; execution : Chatml_execution.limits
  ; max_output_bytes : int
  ; receipt : Document_schema.Limits.t
  }

type t = { settings : settings }

module Configuration = struct
  type t = settings

  let create ~compilation ~max_dependency_depth ~execution ~max_output_bytes ~receipt =
    let compilation_valid =
      match compilation with
      | Chatml_compilation.Unrestricted -> true
      | Bounded limits -> Result.is_ok (Chatml_compilation.validate_limits limits)
    in
    if
      (not compilation_valid)
      || max_dependency_depth <= 0
      || (not (Chatml_execution.valid_limits execution))
      || max_output_bytes <= 0
    then Error (P.Error.invalid_request "invalid developer command budgets")
    else Ok { compilation; max_dependency_depth; execution; max_output_bytes; receipt }
  ;;
end

let within_document ~receipt ~document =
  Document_schema.Limits.max_bytes receipt <= Document_schema.Limits.max_bytes document
  && Document_schema.Limits.max_depth receipt <= Document_schema.Limits.max_depth document
  && Document_schema.Limits.max_fields receipt
     <= Document_schema.Limits.max_fields document
  && Document_schema.Limits.max_nodes receipt <= Document_schema.Limits.max_nodes document
;;

let terminal_fallbacks_fit ~receipt ~max_terminal_bytes =
  let module Receipt = Agent_store.Developer_command_receipt in
  let limited =
    P.Error.create
      P.Error.Resource_limit
      ~message:"developer command result exceeds its admitted limit"
      ~retryable:false
      ()
  in
  let fits value =
    match Receipt.create value ~limits:receipt ~poll:(fun () -> ()) with
    | Error _ -> false
    | Ok retained -> Receipt.encoded_bytes retained <= max_terminal_bytes
  in
  fits (Receipt.Check Receipt.Check.Ready)
  && fits (Receipt.Check Receipt.Check.Interrupted)
  && fits Developer_command_projection_fallback.check_rejected
  && fits (Receipt.Fixture Receipt.Fixture.Interrupted)
  && fits Developer_command_projection_fallback.fixture_max_size_probe
  && fits (Receipt.Live (Receipt.Live.Limited limited))
;;

let bind (configuration : Configuration.t) ~store =
  let open Result.Let_syntax in
  let%bind limits =
    Store.limits store
    |> Result.map_error ~f:(function
      | Store.Error.Closed ->
        P.Error.create
          P.Error.Server_shutting_down
          ~message:"developer command store is closed"
          ~retryable:true
          ()
      | Store.Error.Store _
      | Store.Error.Conflict
      | Store.Error.Prewrite_conflict
      | Store.Error.Foreign_or_stale
      | Store.Error.Limit_exceeded ->
        P.Error.invalid_request "developer command store limits unavailable")
  in
  let document = Store.Limits.document limits in
  let commands = Store.Limits.commands limits in
  let max_terminal_bytes =
    Agent_store.Developer_command_limits.max_terminal_bytes commands
  in
  let effective_receipt =
    Document_schema.Limits.create
      ~max_bytes:
        (Int.min
           (Document_schema.Limits.max_bytes configuration.receipt)
           max_terminal_bytes)
      ~max_depth:(Document_schema.Limits.max_depth configuration.receipt)
      ~max_fields:(Document_schema.Limits.max_fields configuration.receipt)
      ~max_nodes:(Document_schema.Limits.max_nodes configuration.receipt)
  in
  let%bind effective_receipt =
    Result.map_error effective_receipt ~f:(fun _ ->
      P.Error.invalid_request "invalid developer terminal receipt limits")
  in
  if
    (not (within_document ~receipt:configuration.receipt ~document))
    || configuration.max_output_bytes > max_terminal_bytes
    || not (terminal_fallbacks_fit ~receipt:effective_receipt ~max_terminal_bytes)
  then
    Error
      (P.Error.create
         P.Error.Resource_limit
         ~message:"developer command budget exceeds configured store limits"
         ~retryable:false
         ())
  else Ok { settings = { configuration with receipt = effective_receipt } }
;;

let compilation t = t.settings.compilation
let max_dependency_depth t = t.settings.max_dependency_depth
let execution t = t.settings.execution
let max_output_bytes t = t.settings.max_output_bytes
let receipt t = t.settings.receipt

let narrow_execution (host : Chatml_execution.limits) requested =
  let requested : Chatml_execution.limits = requested in
  { Chatml_execution.fuel = Int.min host.fuel requested.fuel
  ; max_tasks = Int.min host.max_tasks requested.max_tasks
  ; wall_seconds = Float.min host.wall_seconds requested.wall_seconds
  ; max_value_bytes = Int.min host.max_value_bytes requested.max_value_bytes
  ; max_array_items = Int.min host.max_array_items requested.max_array_items
  ; max_depth = Int.min host.max_depth requested.max_depth
  ; allocation_bytes = Int.min host.allocation_bytes requested.allocation_bytes
  ; max_calls = Int.min host.max_calls requested.max_calls
  ; max_invocation_depth =
      Int.min host.max_invocation_depth requested.max_invocation_depth
  }
;;

let for_original t original =
  let execution =
    Option.value_map
      (Developer_command_original.requested_execution original)
      ~default:t.settings.execution
      ~f:(narrow_execution t.settings.execution)
  in
  let max_output_bytes =
    Option.value_map
      (Developer_command_original.requested_max_output_bytes original)
      ~default:t.settings.max_output_bytes
      ~f:(Int.min t.settings.max_output_bytes)
  in
  if (not (Chatml_execution.valid_limits execution)) || max_output_bytes <= 0
  then Error (P.Error.invalid_request "invalid requested developer command budgets")
  else Ok { settings = { t.settings with execution; max_output_bytes } }
;;
