open! Core
module P = Agent_protocol
module L = Inference_ledger
module X = Persistence_codec
module O = Goal_descendant_occurrence
module F = Canonical_usage_fold
module Data = Goal_descendants_data

let invalid = Data.invalid

let ledger result =
  Result.map_error result ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (L.Error.sexp_of_t error)))
;;

let marks ledger =
  F.Marks.of_ledger ledger
  |> Result.map_error ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (F.Error.sexp_of_t error)))
;;

type t =
  | Attempt of L.Handle.t * L.tracking
  | Host_turn of P.Operation.t * L.Turn_handle.t * L.tracking

module Fact = struct
  type t =
    { occurrence : O.t
    ; tracking : L.tracking
    ; operation : P.Operation.t option
    }

  let occurrence t = t.occurrence
  let tracking t = t.tracking
  let operation t = t.operation

  let tracking_to_json = function
    | L.Tracked -> `String "tracked"
    | Untracked Attempt_count -> `String "untracked_attempt_count"
    | Untracked Turn_count -> `String "untracked_turn_count"
    | Untracked Retained_bytes -> `String "untracked_retained_bytes"
    | Untracked Protected_future_data -> `String "untracked_protected_future_data"
  ;;

  let tracking_of_json raw =
    let%bind.Result name = P.Json_codec.string raw in
    match name with
    | "tracked" -> Ok L.Tracked
    | "untracked_attempt_count" -> Ok (L.Untracked Attempt_count)
    | "untracked_turn_count" -> Ok (L.Untracked Turn_count)
    | "untracked_retained_bytes" -> Ok (L.Untracked Retained_bytes)
    | "untracked_protected_future_data" -> Ok (L.Untracked Protected_future_data)
    | _ -> invalid "unknown descendant canonical tracking"
  ;;

  let validate_shape t =
    let open Result.Let_syntax in
    let%bind () =
      match O.kind t.occurrence, t.tracking with
      | O.Kind.Attempt _, L.Untracked Turn_count | Host_turn _, Untracked Attempt_count ->
        invalid "descendant canonical tracking limit has the opposite dimension"
      | ( Attempt _
        , (Tracked | Untracked (Attempt_count | Retained_bytes | Protected_future_data)) )
      | ( Host_turn _
        , (Tracked | Untracked (Turn_count | Retained_bytes | Protected_future_data)) ) ->
        Ok ()
    in
    match O.kind t.occurrence, t.operation with
    | O.Kind.Attempt _, None -> Ok ()
    | Host_turn { operation_id }, Some operation ->
      if
        P.Id.Operation.equal operation_id operation.id
        && Int.equal operation.generation (O.Frame.generation (O.frame t.occurrence))
      then (
        match operation.kind, operation.state with
        | Turn _, (Starting | Running) -> Ok ()
        | Turn _, (Cancelling | Completed | Failed _ | Cancelled | Interrupted _)
        | Compaction, _ -> invalid "descendant Turn fact is not a fresh host start")
      else invalid "descendant Turn fact operation differs"
    | Attempt _, Some _ | Host_turn _, None ->
      invalid "descendant canonical fact dimension differs"
  ;;

  let to_json t =
    `Object
      [ "occurrence", O.to_json t.occurrence
      ; "tracking", tracking_to_json t.tracking
      ; "operation", X.option_json P.Operation.to_json t.operation
      ]
  ;;

  let of_json raw =
    let open Result.Let_syntax in
    let%bind fields = X.object_ raw in
    let%bind occurrence = X.required fields "occurrence" O.of_json in
    let%bind tracking = X.required fields "tracking" tracking_of_json in
    let%bind operation = X.required fields "operation" (X.nullable P.Operation.of_json) in
    let t = { occurrence; tracking; operation } in
    let%map () = validate_shape t in
    t
  ;;

  let shape =
    X.shape_exn
      [ "occurrence", O.shape
      ; "tracking", Document_schema.Shape.value
      ; "operation", Document_schema.Shape.value
      ]
  ;;
end

let increment before after =
  Int64.(before < Int64.max_value) && Int64.equal (Int64.succ before) after
;;

let attempt_matches occurrence handle =
  let frame = O.frame occurrence in
  P.Id.Session.equal
    (P.Session_ref.session_id (O.Frame.session frame))
    (L.Handle.session_id handle)
  && Int.equal (O.Frame.generation frame) (L.Handle.generation handle)
  &&
  match O.kind occurrence with
  | O.Kind.Attempt { ordinal; operation_id; invocation_id } ->
    Int64.equal ordinal (L.Handle.ordinal handle)
    && Option.equal P.Id.Operation.equal operation_id (L.Handle.operation_id handle)
    && Option.equal P.Id.Invocation.equal invocation_id (L.Handle.invocation_id handle)
  | Host_turn _ -> false
;;

let validate_fact ~before ~after ~fact =
  let open Result.Let_syntax in
  let%bind () = Fact.validate_shape fact in
  let%bind () = L.validate_update before ~incoming:after |> ledger in
  let occurrence = Fact.occurrence fact in
  let frame = O.frame occurrence in
  let%bind () =
    if
      P.Id.Session.equal (L.session_id before) (L.session_id after)
      && P.Id.Session.equal
           (L.session_id after)
           (P.Session_ref.session_id (O.Frame.session frame))
      && Int.equal (L.generation before) (L.generation after)
      && Int.equal (L.generation after) (O.Frame.generation frame)
    then Ok ()
    else invalid "descendant canonical session/generation differs"
  in
  let%bind before_marks = marks before in
  let%bind after_marks = marks after in
  let tracked = L.equal_tracking (Fact.tracking fact) Tracked in
  let%bind () =
    match Fact.tracking fact with
    | Tracked -> Ok ()
    | Untracked reason ->
      let expected : P.Inference_query.Coverage.limit_kind =
        match reason with
        | L.Limit.Attempt_count -> Attempt_count
        | Turn_count -> Turn_count
        | Retained_bytes -> Retained_bytes
        | Protected_future_data -> Protected_future_data
      in
      let coverage = P.Inference_query.Summary.coverage (L.summary after) in
      (match coverage.tracking_status with
       | Limited actual when P.Inference_query.Coverage.equal_limit_kind expected actual
         -> Ok ()
       | Available | Limited _ ->
         invalid "descendant untracked reason differs from actual canonical coverage")
  in
  match O.kind occurrence with
  | O.Kind.Attempt { ordinal; operation_id = _; invocation_id = _ } ->
    let%bind () =
      if
        increment before_marks.attempt after_marks.attempt
        && Int64.equal ordinal after_marks.attempt
        && Int64.equal before_marks.turn after_marks.turn
        && Int64.equal before_marks.untracked_turns after_marks.untracked_turns
        &&
        if tracked
        then Int64.equal before_marks.untracked_attempts after_marks.untracked_attempts
        else increment before_marks.untracked_attempts after_marks.untracked_attempts
      then Ok ()
      else invalid "descendant Attempt does not match one canonical increment"
    in
    (match Fact.tracking fact, L.find after ~ordinal with
     | Tracked, Some row when attempt_matches occurrence (L.Row.handle row) -> Ok ()
     | Untracked _, None -> Ok ()
     | Tracked, (Some _ | None) | Untracked _, Some _ ->
       invalid "descendant Attempt tracking/identity differs")
  | Host_turn { operation_id } ->
    let%bind () =
      if
        increment before_marks.turn after_marks.turn
        && Int64.equal before_marks.attempt after_marks.attempt
        && Int64.equal before_marks.untracked_attempts after_marks.untracked_attempts
        && (if tracked
            then Int64.equal before_marks.untracked_turns after_marks.untracked_turns
            else increment before_marks.untracked_turns after_marks.untracked_turns)
        && Option.is_none
             (L.find_turn_handle before ~operation_id ~generation:(L.generation before))
      then Ok ()
      else invalid "descendant Turn does not match one canonical increment"
    in
    (match
       ( Fact.tracking fact
       , L.find_turn_handle after ~operation_id ~generation:(L.generation after) )
     with
     | Tracked, Some _ | Untracked _, None -> Ok ()
     | Tracked, None | Untracked _, Some _ ->
       invalid "descendant Turn tracking/identity differs")
;;

let check ~before ~after ~canonical ~occurrence =
  let open Result.Let_syntax in
  let%bind fact =
    match canonical with
    | Attempt (handle, tracking) ->
      let retained_matches =
        match tracking, L.find after ~ordinal:(L.Handle.ordinal handle) with
        | L.Tracked, Some row -> L.Handle.equal handle (L.Row.handle row)
        | Untracked _, None -> true
        | Tracked, None | Untracked _, Some _ -> false
      in
      if attempt_matches occurrence handle && retained_matches
      then Ok { Fact.occurrence; tracking; operation = None }
      else invalid "descendant receipt differs from actual canonical Handle"
    | Host_turn (operation, handle, tracking) ->
      (match O.kind occurrence with
       | O.Kind.Host_turn { operation_id } ->
         if
           P.Id.Operation.equal operation_id (L.Turn_handle.operation_id handle)
           && P.Id.Operation.equal operation_id operation.id
           && Int.equal operation.generation (L.Turn_handle.generation handle)
         then Ok { Fact.occurrence; tracking; operation = Some operation }
         else invalid "descendant receipt differs from actual canonical Turn_handle"
       | Attempt _ -> invalid "descendant actual canonical dimension differs")
  in
  let%bind () = Fact.validate_shape fact in
  let%map () = validate_fact ~before ~after ~fact in
  fact
;;
