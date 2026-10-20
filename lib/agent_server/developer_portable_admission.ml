open! Core
module P = Agent_protocol
module D = Document_schema
module L = P.Developer_common.Limits
module R = P.Developer_repl_wire.Limits
module T = P.Developer_trace_wire.Limits

let invalid message = Error (P.Error.invalid_request message)

let covers outer inner =
  D.Limits.max_bytes outer >= D.Limits.max_bytes inner
  && D.Limits.max_depth outer >= D.Limits.max_depth inner
  && D.Limits.max_fields outer >= D.Limits.max_fields inner
  && D.Limits.max_nodes outer >= D.Limits.max_nodes inner
;;

let checked_add left right =
  if left < 0 || right < 0 || left > Int.max_value - right
  then None
  else Some (left + right)
;;

let checked_mul left right =
  if left < 0 || right < 0 || (right <> 0 && left > Int.max_value / right)
  then None
  else Some (left * right)
;;

let at_least_plus actual base extra =
  Option.value_map (checked_add base extra) ~default:false ~f:(fun required ->
    actual >= required)
;;

let response_covers_nested limits =
  let response = L.response limits in
  let receipt = L.receipt limits in
  let page = L.approval_page limits in
  (* View's non-receipt fields include one arbitrary 256-byte command ID
     (at most 1,536 escaped JSON bytes), one restricted-ASCII 256-byte key,
     three restricted-ASCII 96-byte IDs, two fixed fingerprints, finite epoch,
     phase and field names. Those remain below 4 KiB. Page adds one array
     field around an already bounded page. *)
  at_least_plus (D.Limits.max_bytes response) (D.Limits.max_bytes receipt) 4_096
  && at_least_plus (D.Limits.max_nodes response) (D.Limits.max_nodes receipt) 32
  && at_least_plus (D.Limits.max_fields response) (D.Limits.max_fields receipt) 32
  && at_least_plus (D.Limits.max_depth response) (D.Limits.max_depth receipt) 3
  && at_least_plus (D.Limits.max_bytes response) (D.Limits.max_bytes page) 512
  && at_least_plus (D.Limits.max_nodes response) (D.Limits.max_nodes page) 8
  && at_least_plus (D.Limits.max_fields response) (D.Limits.max_fields page) 8
  && at_least_plus (D.Limits.max_depth response) (D.Limits.max_depth page) 2
;;

let page_covers_broker limits broker =
  let pending = Developer_approval_owner.Limits.max_pending broker in
  let display = Developer_approval_owner.Limits.max_display_bytes broker in
  let page = L.approval_page limits in
  (* Each Pending has at most a 1,024-byte runtime ID, a 256-byte Command ID
     or two 96-byte Job/Invocation IDs, one 64-byte digest, bounded timestamp
     and fixed field names. 12 KiB covers those fields under sixfold JSON
     escaping. The variable display is charged separately. *)
  let required_bytes =
    let open Option.Let_syntax in
    let%bind escaped_display = checked_mul display 6 in
    let%bind per_pending = checked_add escaped_display (12 * 1_024) in
    let%bind all_pending = checked_mul pending per_pending in
    checked_add all_pending 256
  in
  let required_items scale base =
    let open Option.Let_syntax in
    let%bind items = checked_mul pending scale in
    checked_add items base
  in
  L.max_approval_page limits >= pending
  && L.max_approval_display_bytes limits >= display
  && Option.value_map required_bytes ~default:false ~f:(fun required ->
    D.Limits.max_bytes page >= required)
  && Option.value_map (required_items 32 16) ~default:false ~f:(fun required ->
    D.Limits.max_nodes page >= required)
  && Option.value_map (required_items 24 8) ~default:false ~f:(fun required ->
    D.Limits.max_fields page >= required)
  && D.Limits.max_depth page >= 5
  && L.max_approval_decision_reason_bytes limits
     <= Developer_approval_owner.Limits.max_decision_reason_bytes broker
;;

let check_capabilities limits =
  let open Result.Let_syntax in
  let%bind capabilities =
    P.Developer_common.Capabilities.create
      ~format_version:1
      ~methods:P.Developer_wire.Request.supported_methods
      ~limits
  in
  P.Developer_wire.Response.to_json
    (P.Developer_wire.Response.Capabilities capabilities)
    ~limits
  |> Result.map ~f:ignore
;;

let check_repl ~wire ~configuration ~budgets ~(initialize : P.Initialize.Limits.t) =
  let owner = Developer_repl_epoch.Configuration.limits configuration in
  let request = R.request wire in
  let result = R.result wire in
  let page = R.page wire in
  let response = R.response wire in
  let task_receipt = Developer_command_budgets.Configuration.receipt budgets in
  let open Result.Let_syntax in
  let%bind required_entries =
    Developer_repl_epoch.Limits.registry_required_entries owner
  in
  let%bind source_request_bytes =
    checked_mul (R.max_source_bytes wire) 6
    |> Option.bind ~f:(fun escaped -> checked_add escaped 1_024)
    |> Result.of_option
         ~error:(P.Error.invalid_request "REPL request source bound overflowed")
  in
  let%bind result_bytes =
    checked_mul (Developer_repl_epoch.Limits.max_receipt_bytes owner) 6
    |> Option.bind ~f:(fun escaped ->
      checked_mul (R.max_page_items wire) (R.max_reference_bytes wire)
      |> Option.bind ~f:(fun references -> checked_add escaped references))
    |> Option.bind ~f:(fun encoded -> checked_add encoded 2_048)
    |> Result.of_option
         ~error:(P.Error.invalid_request "REPL result source bound overflowed")
  in
  let max_summaries = Developer_repl_epoch.Limits.max_receipt_bytes owner / 64 in
  let%bind summary_nodes =
    checked_mul max_summaries 32
    |> Option.bind ~f:(fun nodes ->
      checked_mul (R.max_page_items wire) 2
      |> Option.bind ~f:(fun references -> checked_add nodes references))
    |> Option.bind ~f:(fun nodes -> checked_add nodes 64)
    |> Result.of_option
         ~error:(P.Error.invalid_request "REPL summary node bound overflowed")
  in
  let%bind summary_fields =
    checked_mul max_summaries 24
    |> Option.bind ~f:(fun fields -> checked_add fields (R.max_page_items wire))
    |> Option.bind ~f:(fun fields -> checked_add fields 64)
    |> Result.of_option
         ~error:(P.Error.invalid_request "REPL summary field bound overflowed")
  in
  let%bind page_items_with_cursor =
    checked_add (R.max_page_items wire) 1
    |> Result.of_option ~error:(P.Error.invalid_request "REPL page item bound overflowed")
  in
  let%bind page_nodes =
    checked_add (R.max_page_items wire) 8
    |> Result.of_option ~error:(P.Error.invalid_request "REPL page node bound overflowed")
  in
  let%bind reference_page_bytes =
    checked_mul (R.max_reference_bytes wire) page_items_with_cursor
    |> Option.bind ~f:(fun encoded -> checked_add encoded 512)
    |> Result.of_option
         ~error:(P.Error.invalid_request "REPL reference page bound overflowed")
  in
  if required_entries > R.max_registry_entries wire
  then invalid "REPL registry cannot retain installed Host tokens"
  else if
    R.max_reference_bytes wire < Developer_repl_epoch.Task_reference.max_encoded_bytes
  then invalid "REPL wire cannot represent an authentic Task reference"
  else if R.max_source_bytes wire > Developer_repl_epoch.Limits.max_source_bytes owner
  then invalid "REPL source exceeds the installed Host source limit"
  else if R.max_page_items wire > Developer_repl_epoch.Limits.max_task_origins owner
  then invalid "REPL page count exceeds installed Task origins"
  else if D.Limits.max_bytes request > initialize.max_request_bytes
  then invalid "REPL request exceeds Initialize capacity"
  else if D.Limits.max_bytes request < source_request_bytes
  then invalid "REPL request cannot represent its admitted source"
  else if D.Limits.max_bytes result < result_bytes
  then invalid "REPL result cannot represent installed compact receipt"
  else if
    D.Limits.max_nodes result < summary_nodes
    || D.Limits.max_fields result < summary_fields
    || D.Limits.max_depth result < 6
  then invalid "REPL result cannot represent installed summary structure"
  else if
    not
      (at_least_plus (D.Limits.max_bytes result) (D.Limits.max_bytes task_receipt) 2_048
       && at_least_plus (D.Limits.max_nodes result) (D.Limits.max_nodes task_receipt) 32
       && at_least_plus (D.Limits.max_fields result) (D.Limits.max_fields task_receipt) 32
       && at_least_plus (D.Limits.max_depth result) (D.Limits.max_depth task_receipt) 3)
  then invalid "REPL result cannot represent a durable Task receipt"
  else if R.max_output_bytes wire < Developer_repl_epoch.Limits.max_receipt_bytes owner
  then invalid "REPL wire output cannot represent installed compact receipt"
  else if D.Limits.max_bytes page < reference_page_bytes
  then invalid "REPL page cannot represent its admitted references and cursor"
  else if
    D.Limits.max_nodes page < page_nodes
    || D.Limits.max_fields page < 8
    || D.Limits.max_depth page < 3
  then invalid "REPL page cannot represent its admitted reference structure"
  else if
    not
      (at_least_plus (D.Limits.max_bytes response) (D.Limits.max_bytes result) 1_024
       && at_least_plus (D.Limits.max_nodes response) (D.Limits.max_nodes result) 16
       && at_least_plus (D.Limits.max_fields response) (D.Limits.max_fields result) 16
       && at_least_plus (D.Limits.max_depth response) (D.Limits.max_depth result) 2
       && at_least_plus (D.Limits.max_bytes response) (D.Limits.max_bytes page) 1_024
       && at_least_plus (D.Limits.max_nodes response) (D.Limits.max_nodes page) 16
       && at_least_plus (D.Limits.max_fields response) (D.Limits.max_fields page) 16
       && at_least_plus (D.Limits.max_depth response) (D.Limits.max_depth page) 2)
  then invalid "REPL response cannot represent installed result framing"
  else Ok ()
;;

let check_trace_cursor_progress wire =
  let module W = P.Developer_trace_wire in
  let open Result.Let_syntax in
  let fail = P.Error.invalid_request "trace cursor or resumed request cannot fit" in
  let%bind run_id =
    P.Id.Developer_run.of_string ("dvr_" ^ String.make 92 'a')
    |> Result.map_error ~f:(fun _ -> fail)
  in
  let%bind key =
    P.Idempotency_key.of_string (String.make 256 'a')
    |> Result.map_error ~f:(fun _ -> fail)
  in
  let%bind instance =
    P.Developer_repl_wire.Wire_id.Instance.of_string ("ri_" ^ String.make 93 'a')
    |> Result.map_error ~f:(fun _ -> fail)
  in
  let%bind epoch =
    P.Developer_repl_wire.Wire_id.Epoch.of_string ("re_" ^ String.make 93 'a')
    |> Result.map_error ~f:(fun _ -> fail)
  in
  let lookup = P.Developer_common.Lookup.create ~run_id ~key in
  let fingerprint = Chatml_source.Digest.of_contents "trace-cursor" ~checkpoint:ignore in
  [ W.Locator.Command lookup
  ; W.Locator.Repl_task lookup
  ; W.Locator.Repl_statement { instance; epoch; key }
  ]
  |> List.map ~f:(fun locator ->
    let%bind cursor =
      W.Cursor.create
        ~limits:wire
        ~locator
        ~execution_id:(String.make 128 'a')
        ~submission_fingerprint:fingerprint
        ~selection_fingerprint:fingerprint
        ~after_sequence:Int64.max_value
      |> Result.map_error ~f:(fun _ -> fail)
    in
    W.Page_request.create
      ~limits:wire
      ~locator
      ~after:(Some cursor)
      ~limit:(T.max_page_items wire)
    |> Result.map ~f:ignore
    |> Result.map_error ~f:(fun _ -> fail))
  |> Result.all_unit
;;

let check_trace ~wire ~retention ~(initialize : P.Initialize.Limits.t) =
  let recorder = Developer_trace_retention.Limits.recorder retention in
  let request = T.request wire in
  let page = T.page wire in
  let response = T.response wire in
  let open Result.Let_syntax in
  let%bind () = check_trace_cursor_progress wire in
  let%bind single_page_bytes =
    (* One retained event can consume the entire recorder normal budget, but
       its closed shape is independently bounded: six words, one native site
       (two 1,024-byte validated Source.Id components and 128 fixed bytes),
       and one 128-byte opaque correlation plus its length word. The last
       retained site is a separate page field and may repeat an older REPL
       closure source outside the current Header inventory. A page can
       contract to one event even when its requested item count is larger. *)
    let maximal_site = 128 + (2 * 1_024) in
    let maximal_event = 48 + maximal_site + 8 + 128 in
    let%bind native =
      checked_add
        (Developer_trace_retention.Limits.max_header_bytes retention)
        (Int.min (Chatml_trace.Recorder.Limits.max_normal_bytes recorder) maximal_event)
      |> Option.bind ~f:(fun total -> checked_add total maximal_site)
      |> Option.bind ~f:(fun total ->
        checked_add total (Chatml_trace.Recorder.Limits.max_terminal_bytes recorder))
      |> Result.of_option
           ~error:(P.Error.invalid_request "trace native capacity overflowed")
    in
    checked_mul native 6
    |> Option.bind ~f:(fun escaped ->
      checked_mul (T.max_cursor_bytes wire) 3
      |> Option.bind ~f:(fun cursors -> checked_add escaped cursors))
    |> Option.bind ~f:(fun charged -> checked_add charged 2_048)
    |> Result.of_option
         ~error:(P.Error.invalid_request "trace encoded page bound overflowed")
  in
  let%bind page_structure =
    checked_mul (Developer_trace_retention.Limits.max_source_refs_per_trace retention) 8
    |> Option.bind ~f:(fun count -> checked_add count 128)
    |> Result.of_option
         ~error:(P.Error.invalid_request "trace source node bound overflowed")
  in
  if T.max_page_items wire > initialize.max_page_size
  then invalid "trace page count exceeds Initialize capacity"
  else if
    not (at_least_plus initialize.max_request_bytes (D.Limits.max_bytes request) 512)
  then invalid "trace request and JSON-RPC envelope exceed Initialize capacity"
  else if not (at_least_plus initialize.max_event_bytes (D.Limits.max_bytes response) 512)
  then invalid "trace response and JSON-RPC envelope exceed Initialize capacity"
  else if D.Limits.max_bytes page < single_page_bytes
  then invalid "trace page cannot represent one maximal retained event"
  else if
    D.Limits.max_nodes page < page_structure
    || D.Limits.max_fields page < page_structure
    || D.Limits.max_depth page < 10
  then invalid "trace page cannot represent installed source and event structure"
  else if
    not
      (at_least_plus (D.Limits.max_bytes response) (D.Limits.max_bytes page) 512
       && at_least_plus (D.Limits.max_nodes response) (D.Limits.max_nodes page) 8
       && at_least_plus (D.Limits.max_fields response) (D.Limits.max_fields page) 8
       && at_least_plus (D.Limits.max_depth response) (D.Limits.max_depth page) 2)
  then invalid "trace response cannot represent page framing"
  else (
    let%bind capabilities =
      P.Developer_trace_wire.Capabilities.create
        ~methods:P.Developer_trace_wire.Request.supported_methods
        ~limits:wire
    in
    P.Developer_trace_wire.Response.to_json
      (P.Developer_trace_wire.Response.Capabilities capabilities)
      ~limits:wire
    |> Result.map ~f:ignore)
;;

let check ~policy ~(initialize : P.Initialize.Limits.t) ~budgets ~approval ~repl ~trace =
  let limits = Developer_endpoint.Policy.wire_limits policy in
  let request = L.request limits in
  let receipt = L.receipt limits in
  let open Result.Let_syntax in
  let%bind required_receipt =
    Developer_command_wire_receipt.required_limits
      ~stored:(Developer_command_budgets.Configuration.receipt budgets)
  in
  if D.Limits.max_bytes request > initialize.max_request_bytes
  then invalid "Developer request exceeds Initialize capacity"
  else if
    not
      (Int.equal
         (L.max_output_bytes limits)
         (Developer_command_budgets.Configuration.max_output_bytes budgets))
  then invalid "Developer output ceiling differs from installed Host budget"
  else if not (covers receipt required_receipt)
  then invalid "Developer receipt cannot represent installed Host results"
  else if not (response_covers_nested limits)
  then invalid "Developer response lacks View or approval-page framing capacity"
  else if Option.exists approval ~f:(fun broker -> not (page_covers_broker limits broker))
  then invalid "Developer approval policy cannot represent the installed broker"
  else (
    let%bind () = check_capabilities limits in
    let%bind () =
      match Developer_endpoint.Policy.repl_wire_limits policy, repl with
      | None, _ -> Ok ()
      | Some _, None -> invalid "REPL wire requires the installed REPL Host"
      | Some wire, Some configuration ->
        check_repl ~wire ~configuration ~budgets ~initialize
    in
    match Developer_endpoint.Policy.trace_wire_limits policy, trace with
    | None, _ -> Ok ()
    | Some _, None -> invalid "trace wire requires installed retention"
    | Some wire, Some retention -> check_trace ~wire ~retention ~initialize)
;;
