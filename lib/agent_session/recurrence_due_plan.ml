open! Core
module P = Agent_protocol
module R = Recurrence_rules
module F = Recurrence_definition
module C = Recurrence_cursor
module N = Recurrence_nominal
module H = Recurrence_frontier
module W = H.Work
module Phase = Recurrence_phase
module Snapshot = Phase.Snapshot
module D = Document_schema

type t =
  { definition_sha256 : string
  ; old_cursor : C.t
  ; old_frontier : H.t
  ; cursor : C.t
  ; frontier : H.t
  ; next_phase : Phase.t option
  ; selection_grace : Recurrence_selection_grace.t list
  }

type decision =
  | Unchanged of Phase.t option
  | Candidate of t

let cursor t = t.cursor
let frontier t = t.frontier
let next_phase t = t.next_phase
let selection_grace t = t.selection_grace
let invalid message = Error (P.Error.invalid_request message)

let timing r =
  Result.map_error r ~f:(fun e ->
    P.Error.invalid_request (Sexp.to_string_hum (R.Timing_error.sexp_of_t e)))
;;

let json h = Result.map (H.to_document h) ~f:D.Document.json

let same_frontier a b =
  match json a, json b with
  | Ok a, Ok b -> D.Json.equal a b
  | Error _, _ | _, Error _ -> false
;;

let matches t ~definition ~cursor ~frontier =
  String.equal t.definition_sha256 (F.digest definition)
  && C.equal t.old_cursor cursor
  && same_frontier t.old_frontier frontier
;;

let finish ~definition ~old_cursor ~old_frontier ~work ~cursor ~next_phase =
  let%map.Result frontier = W.finish work ~cursor in
  if C.equal old_cursor cursor && same_frontier old_frontier frontier
  then Unchanged next_phase
  else
    Candidate
      { definition_sha256 = F.digest definition
      ; old_cursor
      ; old_frontier
      ; cursor
      ; frontier
      ; next_phase
      ; selection_grace = []
      }
;;

let nanoseconds t = Time_ns.to_int63_ns_since_epoch t |> Int63.to_int64

let wall_lateness ~now ~due_at =
  if Time_ns.compare now due_at < 0
  then None
  else Some Int64.(nanoseconds now - nanoseconds due_at)
;;

let arm_interval work cursor =
  match C.position cursor with
  | Interval interval ->
    W.arm
      work
      ~slot:(R.Cursor.Interval.slot interval)
      ~due_at:(R.Cursor.Interval.due_at interval)
  | Calendar _ | Awaiting_completion _ | Exhausted -> W.retire_alarm work
;;

(* At most 32 explicit rows; arbitrarily long omitted runs remain arithmetic
   ranges. The interval cursor and captured phase, rather than callback duration,
   determine every selected nominal. *)
let select_interval
      definition
      ~(range : R.Cursor.Interval.due_range)
      ~lateness
      ~pending
      ~protected
  =
  let open Result.Let_syntax in
  let policy = F.policy definition in
  let period =
    match F.schedule definition with
    | Interval { rule; _ } -> R.Rule.Period.nanoseconds (R.Rule.Interval.period rule)
    | Calendar _ -> assert false
  in
  let expired =
    if Int64.(lateness <= Recurrence_policy.grace_ns policy)
    then 0L
    else
      Int64.(((lateness - Recurrence_policy.grace_ns policy - 1L) / period) + 1L)
      |> Int64.min range.count
  in
  let eligible = Int64.(range.count - expired) in
  let available = Int.max 0 (Recurrence_policy.max_pending policy - pending) in
  let first_selected, count_selected, policy_reason =
    match Recurrence_policy.misfire policy with
    | Skip when pending > 0 -> range.count, 0, H.Reason.Policy_skip
    | (Skip | Coalesce) when protected >= Recurrence_policy.max_pending policy ->
      range.count, 0, H.Reason.Pending_capacity
    | Skip | Coalesce ->
      if Int64.equal eligible 0L
      then range.count, 0, H.Reason.Policy_skip
      else Int64.pred range.count, 1, H.Reason.Superseded
    | Catch_up maximum ->
      ( expired
      , Int64.to_int_exn (Int64.min eligible (Int64.of_int (Int.min maximum available)))
      , H.Reason.Pending_capacity )
  in
  let%bind selected =
    List.init count_selected ~f:(fun index ->
      N.create_interval
        definition
        ~slot:Int64.(range.first_slot + first_selected + of_int index))
    |> Result.all
  in
  let omission a b reason =
    if Int64.(a > b)
    then []
    else
      [ W.
          { first_slot = Int64.(range.first_slot + a)
          ; last_slot = Int64.(range.first_slot + b)
          ; reason
          }
      ]
  in
  let omitted =
    omission 0L Int64.(expired - 1L) Expired
    @ omission expired Int64.(first_selected - 1L) policy_reason
    @ omission
        Int64.(first_selected + of_int count_selected)
        Int64.(range.count - 1L)
        policy_reason
  in
  Ok (selected, omitted)
;;

let prepare_rate definition ~old_cursor ~frontier ~interval ~phase ~snapshot ~work ~rule =
  let open Result.Let_syntax in
  let%bind phase =
    match phase with
    | None -> Ok None
    | Some phase ->
      if Phase.matches phase ~definition ~cursor:interval
      then Ok (Some phase)
      else invalid "live recurrence phase differs from current exact cursor/source"
  in
  let%bind range, lateness =
    match phase with
    | Some phase ->
      let count = Phase.due_count phase ~snapshot in
      if Int64.equal count 0L
      then Ok (None, None)
      else (
        let%map range = R.Cursor.Interval.consume_count interval ~rule ~count |> timing in
        Some range, Phase.lateness_ns phase ~snapshot)
    | None ->
      let now = Snapshot.wall snapshot in
      let%map range = R.Cursor.Interval.due_through interval ~rule ~now |> timing in
      range, wall_lateness ~now ~due_at:(R.Cursor.Interval.due_at interval)
  in
  let%bind cursor, work, next_phase =
    match range, lateness with
    | None, _ ->
      let%map next_phase =
        match phase with
        | Some p -> Ok (Some p)
        | None ->
          Phase.recover definition ~cursor:interval ~snapshot |> Result.map ~f:Option.some
      in
      old_cursor, work, next_phase
    | Some range, Some lateness ->
      let%bind selected, omitted =
        select_interval
          definition
          ~range
          ~lateness
          ~pending:(H.pending_count frontier)
          ~protected:(List.length (H.protected_children frontier))
      in
      let%bind work = W.consume_interval work ~range ~selected ~omitted in
      let%bind step = C.consume_interval old_cursor ~rule ~count:range.count in
      let cursor = step.next in
      let%map next_phase =
        match phase with
        | Some phase -> Phase.advance phase ~count:range.count
        | None ->
          (match C.position cursor with
           | Interval next ->
             Phase.recover definition ~cursor:next ~snapshot |> Result.map ~f:Option.some
           | Calendar _ | Awaiting_completion _ | Exhausted -> Ok None)
      in
      cursor, work, next_phase
    | Some _, None -> invalid "due interval count lacks matching lateness"
  in
  let%bind work = arm_interval work cursor in
  let work =
    W.set_issue
      work
      (match C.position cursor with
       | Exhausted -> Some H.Issue.Timing_exhausted
       | Interval _ | Calendar _ | Awaiting_completion _ -> None)
  in
  finish ~definition ~old_cursor ~old_frontier:frontier ~work ~cursor ~next_phase
;;

let prepare_delay
      definition
      ~old_cursor
      ~frontier
      ~interval
      ~phase
      ~snapshot
      ~work
      ~rule:_
  =
  let open Result.Let_syntax in
  let%bind live =
    match phase with
    | Some phase when Phase.matches phase ~definition ~cursor:interval -> Ok phase
    | Some _ -> invalid "fixed-delay phase differs from current source/cursor"
    | None -> Phase.recover definition ~cursor:interval ~snapshot
  in
  let lateness =
    match phase with
    | Some _ -> Phase.lateness_ns live ~snapshot
    | None ->
      wall_lateness
        ~now:(Snapshot.wall snapshot)
        ~due_at:(R.Cursor.Interval.due_at interval)
  in
  if Option.equal H.Issue.equal (H.issue frontier) (Some Initial_delay_missed)
  then Ok (Unchanged None)
  else (
    match lateness with
    | None ->
      let%bind work = arm_interval work old_cursor in
      finish
        ~definition
        ~old_cursor
        ~old_frontier:frontier
        ~work
        ~cursor:old_cursor
        ~next_phase:(Some live)
    | Some lateness
      when Int64.(lateness > Recurrence_policy.grace_ns (F.policy definition)) ->
      let%bind work = W.retire_alarm work in
      finish
        ~definition
        ~old_cursor
        ~old_frontier:frontier
        ~work:(W.set_issue work (Some Initial_delay_missed))
        ~cursor:old_cursor
        ~next_phase:None
    | Some _ ->
      let%bind nominal = N.create_interval definition ~slot:0L in
      let%bind work = W.consume_initial_delay work ~nominal in
      let%bind cursor =
        match F.schedule definition with
        | Interval { rule; _ } -> C.consume_initial_delay old_cursor ~rule
        | Calendar _ -> assert false
      in
      finish ~definition ~old_cursor ~old_frontier:frontier ~work ~cursor ~next_phase:None)
;;

let prepare_calendar definition ~old_cursor ~frontier ~snapshot ~zone ~work ~rule =
  let open Result.Let_syntax in
  let cutoff =
    Option.value (H.calendar_cutoff frontier) ~default:(Snapshot.wall snapshot)
  in
  let%bind work = W.begin_calendar work ~cutoff in
  let rec scan work cursor ahead remaining =
    match ahead with
    | Some nominal ->
      if Time_ns.(N.at nominal > cutoff)
      then (
        let%bind work = W.finish_calendar work ~lookahead:(Some nominal) in
        let%map work =
          W.arm
            work
            ~slot:(Recurrence_identity.Occurrence.slot (N.key nominal))
            ~due_at:(N.at nominal)
        in
        cursor, work)
      else (
        let%bind work = W.collect_calendar work ~nominal in
        let%bind cursor =
          C.consume_calendar cursor ~occurrence:(N.key nominal) ~nominal_at:(N.at nominal)
        in
        scan work cursor None remaining)
    | None ->
      if remaining = 0
      then Ok (cursor, work)
      else (
        match zone with
        | None -> Ok (cursor, W.set_issue work (Some Zone_unavailable))
        | Some zone ->
          (match C.search_calendar cursor ~rule ~zone ~max_steps:1 with
           | Error R.Timing_error.Out_of_coverage ->
             Ok (cursor, W.set_issue work (Some Coverage_unavailable))
           | Error (Invalid _ | Out_of_range _ | Zone_mismatch) as error ->
             Result.bind (timing error) ~f:(fun _ -> assert false)
           | Ok (Continue cursor) -> scan work cursor None (remaining - 1)
           | Ok (End cursor) ->
             let%map work = W.finish_calendar work ~lookahead:None in
             cursor, W.set_issue work (Some Timing_exhausted)
           | Ok (Found (occurrence, cursor)) ->
             let%bind slot =
               match C.position cursor with
               | Calendar { next_slot; _ } -> Ok next_slot
               | Interval _ | Awaiting_completion _ | Exhausted ->
                 invalid "calendar emission lost next slot"
             in
             let%bind nominal = N.create_calendar definition ~slot ~occurrence in
             let%bind work = W.set_lookahead work ~nominal in
             scan work cursor (Some nominal) (remaining - 1)))
  in
  let%bind cursor, work =
    scan
      work
      old_cursor
      (H.lookahead frontier)
      (Recurrence_policy.max_calendar_steps (F.policy definition))
  in
  finish ~definition ~old_cursor ~old_frontier:frontier ~work ~cursor ~next_phase:None
;;

let prepare_inner ~definition ~cursor ~frontier ~phase ~snapshot ~zone =
  let open Result.Let_syntax in
  let%bind work = W.start frontier ~definition ~cursor in
  match C.position cursor, F.schedule definition with
  | Interval interval, Interval { rule; _ } ->
    (match R.Rule.Interval.mode rule with
     | Fixed_rate ->
       prepare_rate
         definition
         ~old_cursor:cursor
         ~frontier
         ~interval
         ~phase
         ~snapshot
         ~work
         ~rule
     | Fixed_delay ->
       prepare_delay
         definition
         ~old_cursor:cursor
         ~frontier
         ~interval
         ~phase
         ~snapshot
         ~work
         ~rule)
  | Calendar _, Calendar rule ->
    if Option.is_some phase
    then invalid "calendar plan received unrelated elapsed phase"
    else
      prepare_calendar definition ~old_cursor:cursor ~frontier ~snapshot ~zone ~work ~rule
  | (Awaiting_completion _ | Exhausted), (Interval _ | Calendar _) -> Ok (Unchanged None)
  | (Interval _ | Calendar _), (Interval _ | Calendar _) ->
    invalid "cursor mode differs from due definition"
;;

(* Newly selected interval rows carry the exact already-computed live lateness
   into a bounded process-local anchor. Calendar admission remains wall based.
   No anchor is installed until the actual owner acknowledges the due candidate. *)
let prepare ~definition ~cursor ~frontier ~phase ~snapshot ~zone =
  let open Result.Let_syntax in
  let%bind decision =
    prepare_inner ~definition ~cursor ~frontier ~phase ~snapshot ~zone
  in
  match decision, F.schedule definition with
  | Unchanged _, _ | Candidate _, Calendar _ -> Ok decision
  | Candidate plan, Interval { rule; _ } ->
    let selected =
      List.filter (H.ready plan.frontier) ~f:(fun nominal ->
        not
          (List.exists (H.ready frontier) ~f:(fun old ->
             Recurrence_identity.Occurrence.equal (N.key nominal) (N.key old))))
    in
    let%map selection_grace =
      List.map selected ~f:(fun nominal ->
        let%bind lateness_ns =
          match phase, C.position cursor with
          | Some phase, Interval interval ->
            let count =
              Int64.(
                Recurrence_identity.Occurrence.slot (N.key nominal)
                - R.Cursor.Interval.slot interval)
            in
            let period = R.Rule.Period.nanoseconds (R.Rule.Interval.period rule) in
            (match Phase.lateness_ns phase ~snapshot with
             | Some late when Int64.(count >= 0L && count <= late / period) ->
               Ok Int64.(late - (count * period))
             | None | Some _ ->
               invalid "selected nominal lacks matching live interval lateness")
          | None, Interval _ ->
            Result.of_option
              (wall_lateness ~now:(Snapshot.wall snapshot) ~due_at:(N.at nominal))
              ~error:(P.Error.invalid_request "recovered interval selection is not due")
          | (Some _ | None), (Calendar _ | Awaiting_completion _ | Exhausted) ->
            invalid "interval selection lacks original interval cursor"
        in
        Recurrence_selection_grace.create
          ~occurrence:(N.key nominal)
          ~snapshot
          ~lateness_ns)
      |> Result.all
    in
    Candidate { plan with selection_grace }
;;
