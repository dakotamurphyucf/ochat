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
    }
;;

let matches t ~definition ~cursor =
  I.Definition.equal t.definition (Recurrence_definition.key definition)
  && String.equal t.definition_sha256 (Recurrence_definition.digest definition)
  && R.Cursor.Interval.equal t.cursor cursor
;;

let due_count t ~(snapshot : Snapshot.t) =
  match R.Rule.Interval.mode t.rule with
  | Fixed_rate ->
    R.Clock.due_count t.timing ~period:(R.Rule.Interval.period t.rule) snapshot.timing
  | Fixed_delay -> if R.Clock.is_due t.timing snapshot.timing then 1L else 0L
;;

let lateness_ns t ~(snapshot : Snapshot.t) = R.Clock.lateness_ns t.timing snapshot.timing

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
