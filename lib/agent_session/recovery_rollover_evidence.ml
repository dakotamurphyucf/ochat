open! Core
module P = Agent_protocol
module B = Recovery_rollover_batch
module R = Recovery_effective_record
module E = Recovery_effective_attempt
module H = Inference_ledger.Handle
module T = Inference.Event.Terminal

let conflict message = Error (P.Error.create Conflict ~message ~retryable:true ())

let records (state : Session_state.t) =
  Option.value_map
    state.recovery_effective
    ~default:[]
    ~f:Recovery_effective_ledger.records
;;

let check_terminal (state : Session_state.t) ~operation attempt =
  let open Result.Let_syntax in
  let%bind row =
    Inference_ledger.find state.inference_ledger ~ordinal:(E.ordinal attempt)
    |> Result.of_option
         ~error:(P.Error.invalid_request "settled attempt accounting is unavailable")
  in
  let handle = Inference_ledger.Row.handle row in
  let record = Inference_ledger.Row.record row in
  match E.phase attempt, Inference.Observation.Attempt_record.state record with
  | Settled { origin = Returned; delivery = Response_started }, Terminal terminal
    when E.matches_handle attempt handle
         && Option.equal P.Id.Operation.equal (H.operation_id handle) (Some operation)
         && Option.is_none (H.invocation_id handle)
         && T.equal_outcome (T.outcome terminal) Completed
         && T.equal_delivery (T.delivery terminal) Response_started ->
    Ok (handle, terminal)
  | _ -> conflict "rollover requires a complete returned foreground attempt"
;;

let current_record state ~scope ~operation =
  records state
  |> List.filter ~f:(fun record ->
    P.Id.Operation.equal (R.operation record) operation
    && P.Recovery_window.Scope.equal (R.scope record) scope)
  |> List.max_elt ~compare:(fun first second ->
    Int64.compare (R.source_revision first) (R.source_revision second))
;;

let require_attempt state ~scope ~operation =
  let open Result.Let_syntax in
  let%bind record =
    current_record state ~scope ~operation
    |> Result.of_option
         ~error:(P.Error.invalid_request "no current prepared rollover evidence")
  in
  let%bind () =
    match R.custody record with
    | Live -> Ok ()
    | Lost | Archived _ -> conflict "settled attempt no longer has a live source"
  in
  let%bind attempt =
    R.attempt record
    |> Result.of_option
         ~error:(P.Error.invalid_request "prepared rollover attempt is unavailable")
  in
  let%map handle, terminal = check_terminal state ~operation attempt in
  record, handle, terminal
;;

let semantics entry =
  Result.map (History_codec.of_protocol entry) ~f:(fun item ->
    History_entry.Payload.(Semantic.view (semantic (History_entry.payload item))))
;;

let original (entry : P.History.entry) =
  P.History.Content_revision.equal entry.content_revision P.History.Content_revision.zero
  && P.History.equal_provenance entry.provenance Canonical
  && not entry.redacted
;;

let pair_is_bound ~(call : P.History.entry) ~(output : P.History.entry) =
  let open Result.Let_syntax in
  let%bind call_view = semantics call in
  let%bind output_view = semantics output in
  match call_view, output_view with
  | Call { kind; _ }, Result { relation = Bound actual; kind = output_kind; _ }
    when original call
         && original output
         && P.History.Id.equal call.id actual
         && History_entry.Payload.Call_kind.equal kind output_kind -> Ok ()
  | _ -> conflict "settled tool result is not bound to its original committed call"
;;

let published state pair invocation =
  let context = invocation.P.Invocation.context in
  P.Id.Session.equal state.Session_state.identity.session_id context.session_id
  && Int.equal state.identity.generation context.generation
  && P.Invocation.equal_origin context.origin Model
  && Option.is_none context.parent_invocation
  && Option.is_none context.parent_job
  && Option.is_none invocation.parent_event
  && Option.equal P.History.Id.equal invocation.output_entry_id (Some pair.B.output)
  &&
  match invocation.status with
  | Published _ -> true
  | Resolved _ | Dispatching | Admitted -> false
;;
