open! Core
module P = Agent_protocol
module R = Recurrence_rules
module I = Recurrence_identity

module Snapshot = struct
  type t =
    { wall : Time_ns.Alternate_sexp.t
    ; elapsed_ns : int64
    ; timing : R.Clock.snapshot
    }

  let wall t = t.wall
  let elapsed_ns t = t.elapsed_ns

  let create ~wall ~elapsed_ns =
    R.Clock.snapshot ~wall ~elapsed_ns
    |> Result.map_error ~f:(fun error ->
      P.Error.invalid_request (Sexp.to_string_hum (R.Timing_error.sexp_of_t error)))
    |> Result.map ~f:(fun timing -> { wall; elapsed_ns; timing })
  ;;
end

type t =
  { definition : I.Definition.t
  ; definition_sha256 : string
  ; cursor : R.Cursor.Interval.t
  ; rule : R.Rule.Interval.t
  ; timing : R.Clock.t
  ; completion_elapsed_ns : int64
  }

let timing result =
  Result.map_error result ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (R.Timing_error.sexp_of_t error)))
;;

let recover definition ~cursor ~(snapshot : Snapshot.t) =
  match Recurrence_definition.schedule definition with
  | Calendar _ -> Error (P.Error.invalid_request "calendar rules have no elapsed phase")
  | Interval { rule; _ } ->
    let%map.Result clock =
      R.Clock.recover snapshot.timing ~deadline:(R.Cursor.Interval.due_at cursor)
      |> timing
    in
    { definition = Recurrence_definition.key definition
    ; definition_sha256 = Recurrence_definition.digest definition
    ; cursor
    ; rule
    ; timing = clock
    ; completion_elapsed_ns = 0L
    }
;;

let from_completion definition ~cursor ~completed_at ~completion_elapsed_ns =
  let open Result.Let_syntax in
  match Recurrence_definition.schedule definition with
  | Calendar _ ->
    Error (P.Error.invalid_request "calendar rules have no completion phase")
  | Interval { rule; _ } ->
    (match R.Rule.Interval.mode rule with
     | Fixed_rate ->
       Error (P.Error.invalid_request "fixed-rate phase cannot restart from completion")
     | Fixed_delay ->
       let%bind captured = R.Clock.snapshot ~wall:completed_at ~elapsed_ns:0L |> timing in
       let%bind clock =
         R.Clock.after captured ~delay:(R.Rule.Interval.period rule) |> timing
       in
       if not (Time_ns.equal (R.Clock.deadline clock) (R.Cursor.Interval.due_at cursor))
       then
         Error (P.Error.invalid_request "completion deadline differs from exact cursor")
       else
         Ok
           { definition = Recurrence_definition.key definition
           ; definition_sha256 = Recurrence_definition.digest definition
           ; cursor
           ; rule
           ; timing = clock
           ; completion_elapsed_ns
           })
;;

let matches t ~definition ~cursor =
  I.Definition.equal t.definition (Recurrence_definition.key definition)
  && String.equal t.definition_sha256 (Recurrence_definition.digest definition)
  && R.Cursor.Interval.equal t.cursor cursor
;;

let timing_snapshot t (snapshot : Snapshot.t) =
  let elapsed = snapshot.elapsed_ns in
  let offset = t.completion_elapsed_ns in
  if Int64.equal offset 0L
  then Ok snapshot.timing
  else if
    Int64.(offset >= 0L && elapsed < offset)
    || Int64.(offset < 0L && elapsed > max_value + offset)
  then
    Error
      (P.Error.invalid_request
         "live completion elapsed time moved backward or overflowed")
  else R.Clock.snapshot ~wall:snapshot.wall ~elapsed_ns:Int64.(elapsed - offset) |> timing
;;

let due_count t ~snapshot =
  let%map.Result snapshot = timing_snapshot t snapshot in
  match R.Rule.Interval.mode t.rule with
  | Fixed_rate ->
    R.Clock.due_count t.timing ~period:(R.Rule.Interval.period t.rule) snapshot
  | Fixed_delay -> if R.Clock.is_due t.timing snapshot then 1L else 0L
;;

let lateness_ns t ~snapshot =
  let%map.Result snapshot = timing_snapshot t snapshot in
  R.Clock.lateness_ns t.timing snapshot
;;

let advance t ~count =
  let open Result.Let_syntax in
  let%bind consumed =
    R.Cursor.Interval.consume_count t.cursor ~rule:t.rule ~count |> timing
  in
  match consumed.next with
  | None -> Ok None
  | Some cursor ->
    let%map clock =
      R.Clock.advance t.timing ~period:(R.Rule.Interval.period t.rule) ~count |> timing
    in
    Some { t with cursor; timing = clock }
;;
