open! Core
module P = Agent_protocol
module State = Session_state
module Ledger = Recovery_effective_ledger
module Record = Recovery_effective_record
module Attempt = Recovery_effective_attempt
module Inference_ledger = Inference_ledger
module H = Inference_ledger.Handle
module Terminal = Inference.Event.Terminal
module Accounting = Inference.Observation.Attempt_record
module Scope = P.Recovery_window.Scope
module X = Persistence_codec

type t =
  { source_revision : int64
  ; attempt : Attempt.t
  }

let invalid reason = Error (P.Error.invalid_request reason)
let evidence (state : State.t) = state.recovery_effective

let allocation_matches record handle =
  let owner = Scope.session (Record.scope record) in
  P.Id.Session.equal (H.session_id handle) (P.Session_ref.session_id owner)
  && Option.exists
       (H.operation_id handle)
       ~f:(P.Id.Operation.equal (Record.operation record))
  && Option.is_none (H.invocation_id handle)
;;

let allocate state ~source_revision handle ~tracking configuration =
  let open Option.Let_syntax in
  let%bind record = Recovery_effective_capture.resolve_record state ~source_revision in
  let%bind attempt = Attempt.allocate handle ~tracking |> Result.ok in
  Option.some_if
    (Option.is_none (Record.attempt record)
     && allocation_matches record handle
     && Attempt.belongs_to attempt (Record.scope record)
     && Record.matches_configuration record configuration)
    { source_revision; attempt }
;;

let advance state handle ~next =
  let open Option.Let_syntax in
  let%bind ledger = evidence state in
  List.find_map (Ledger.records ledger) ~f:(fun record ->
    let%bind attempt = Record.attempt record in
    if Attempt.matches_handle attempt handle && allocation_matches record handle
    then (
      let%bind changed = next attempt in
      if Attempt.equal changed attempt
      then None
      else Some { source_revision = Record.source_revision record; attempt = changed })
    else None)
;;

let acknowledge state handle =
  advance state handle ~next:(fun attempt ->
    match Attempt.phase attempt with
    | Allocated -> Some (Attempt.with_phase attempt Running)
    | Running | Settled _ -> None)
;;

let complete state handle completion =
  advance state handle ~next:(fun attempt ->
    match Attempt.phase attempt with
    | Allocated | Settled _ -> None
    | Running ->
      let settlement : Attempt.settlement =
        match Inference_client.Completion.outcome completion with
        | Returned terminal ->
          { origin = Returned; delivery = Terminal.delivery terminal }
        | Interrupted { delivery; _ } -> { origin = Interrupted; delivery }
      in
      Some (Attempt.with_phase attempt (Settled settlement)))
;;

let release state handle ~delivery =
  advance state handle ~next:(fun attempt ->
    match Attempt.phase attempt with
    | Settled _ -> None
    | Allocated -> Some (Attempt.recover attempt)
    | Running ->
      let delivery = Option.value delivery ~default:Terminal.Possibly_submitted in
      Some (Attempt.with_phase attempt (Settled { origin = Interrupted; delivery })))
;;

let recover state =
  Option.value_map (evidence state) ~default:[] ~f:(fun ledger ->
    List.filter_map (Ledger.records ledger) ~f:(fun record ->
      let open Option.Let_syntax in
      let%bind attempt = Record.attempt record in
      let changed = Attempt.recover attempt in
      if Attempt.equal changed attempt
      then None
      else Some { source_revision = Record.source_revision record; attempt = changed }))
;;

let fresh_occurrence (state : State.t) record attempt =
  Int64.equal
    (Attempt.ordinal attempt)
    (Inference_ledger.last_attempt_ordinal state.inference_ledger)
  &&
  match state.active_operation with
  | Some { id; generation; kind = Turn _; _ } ->
    Int.equal generation (Attempt.generation attempt)
    && P.Id.Operation.equal id (Record.operation record)
  | _ -> false
;;

let check_accounting state record attempt ~fresh =
  match
    Inference_ledger.find state.State.inference_ledger ~ordinal:(Attempt.ordinal attempt)
  with
  | None when fresh && Attempt.tracked attempt ->
    invalid "tracked recovery attempt lacks its actual accounting admission"
  | None -> Ok ()
  | Some row ->
    let handle = Inference_ledger.Row.handle row in
    let accounting = Inference_ledger.Row.record row in
    if
      Attempt.tracked attempt
      && Attempt.matches_handle attempt handle
      && allocation_matches record handle
      && ((not fresh)
          || Record.matches_configuration record (Accounting.configuration accounting))
      && Attempt.matches_accounting attempt accounting
    then Ok ()
    else invalid "recovery attempt differs from the actual accounting row"
;;

let apply state t =
  let open Result.Let_syntax in
  let%bind ledger =
    evidence state
    |> Result.of_option ~error:(P.Error.invalid_request "no recovery preparation ledger")
  in
  let%bind record =
    Recovery_effective_capture.resolve_record state ~source_revision:t.source_revision
    |> Result.of_option
         ~error:(P.Error.invalid_request "no prepared source for the recovery attempt")
  in
  let fresh = Option.is_none (Record.attempt record) in
  if
    (not (Attempt.belongs_to t.attempt (Record.scope record)))
    || Int64.(
         Attempt.ordinal t.attempt
         > Inference_ledger.last_attempt_ordinal state.inference_ledger)
    || (fresh && not (fresh_occurrence state record t.attempt))
  then invalid "recovery attempt is outside its actual source or allocation"
  else (
    let%bind () = check_accounting state record t.attempt ~fresh in
    Ledger.update_attempt ledger ~source_revision:t.source_revision t.attempt)
;;

let check_transaction ~(previous : State.t) ~(next : State.t) =
  if Option.equal phys_equal previous.recovery_effective next.recovery_effective
  then Ok ()
  else (
    let before = Inference_ledger.last_attempt_ordinal previous.inference_ledger in
    let after = Inference_ledger.last_attempt_ordinal next.inference_ledger in
    Option.value_map next.recovery_effective ~default:(Ok ()) ~f:(fun ledger ->
      List.fold_result (Ledger.records ledger) ~init:() ~f:(fun () record ->
        let previous_attempt =
          Option.bind
            (Recovery_effective_capture.resolve_record
               previous
               ~source_revision:(Record.source_revision record))
            ~f:Record.attempt
        in
        match previous_attempt, Record.attempt record with
        | None, Some attempt
          when not
                 (Int64.(after > before)
                  && Int64.equal after (Int64.succ before)
                  && Int64.equal after (Attempt.ordinal attempt)) ->
          invalid "recovery attempt lacks a same-transaction allocation"
        | _ -> Ok ())))
;;

let to_jsonaf t =
  `Object
    [ "source_revision", X.int64_json t.source_revision
    ; "attempt", Attempt.to_jsonaf t.attempt
    ]
;;

let of_jsonaf json =
  let open Result.Let_syntax in
  let%bind fields = X.object_ json in
  let%bind source_revision = X.required fields "source_revision" X.nonnegative_int64 in
  let%map attempt = X.required fields "attempt" Attempt.of_jsonaf in
  { source_revision; attempt }
;;

let shape =
  X.shape_exn [ "source_revision", Document_schema.Shape.value; "attempt", Attempt.shape ]
;;
