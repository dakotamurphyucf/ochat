open! Core
module P = Agent_protocol
module D = Document_schema
module L = P.Developer_common.Limits

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

let check ~policy ~(initialize : P.Initialize.Limits.t) ~budgets ~approval =
  let limits = Developer_endpoint.Policy.wire_limits policy in
  let request = L.request limits in
  let receipt = L.receipt limits in
  if D.Limits.max_bytes request > initialize.max_request_bytes
  then invalid "Developer request exceeds Initialize capacity"
  else if
    not
      (Int.equal
         (L.max_output_bytes limits)
         (Developer_command_budgets.Configuration.max_output_bytes budgets))
  then invalid "Developer output ceiling differs from installed Host budget"
  else if not (covers receipt (Developer_command_budgets.Configuration.receipt budgets))
  then invalid "Developer receipt cannot represent installed Host results"
  else if not (response_covers_nested limits)
  then invalid "Developer response lacks View or approval-page framing capacity"
  else if Option.exists approval ~f:(fun broker -> not (page_covers_broker limits broker))
  then invalid "Developer approval policy cannot represent the installed broker"
  else check_capabilities limits
;;
