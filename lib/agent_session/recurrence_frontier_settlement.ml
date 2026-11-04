open! Core
module P = Agent_protocol
module D = Document_schema
module I = Recurrence_identity
module F = Recurrence_definition
module C = Recurrence_cursor
module V = Recurrence_frontier_records
module S = V.State
module T = Recurrence_frontier_transition
module Execution = Recurrence_execution
module Intent = Recurrence_execution_intent
module Origin = Recurrence_run_origin
module Reference = Recurrence_run_reference
module Basis = Recurrence_delay_basis
module Settlement = Recurrence_settlement

type t =
  { state : S.t
  ; cursor : C.t
  ; result : Settlement.t
  }

let invalid message = Error (P.Error.invalid_request message)
let occurrence execution = Intent.occurrence (Execution.intent execution)
let same_occurrence key other = I.Occurrence.equal key other

let resume state ~definition ~cursor =
  let open Result.Let_syntax in
  match state.S.delay_basis with
  | None -> invalid "fixed-delay resume requires retained definitive completion basis"
  | Some basis ->
    let%bind () = Basis.validate basis ~definition in
    let%bind () =
      match C.position cursor with
      | Awaiting_completion expected
        when I.Occurrence.equal expected (Basis.occurrence basis) -> Ok ()
      | Interval _ | Calendar _ | Awaiting_completion _ | Exhausted ->
        invalid "fixed-delay resume basis does not match the awaiting occurrence"
    in
    (match Basis.next_interval basis ~definition with
     | Error _ ->
       Ok
         ( { state with issue = Some Delay_successor_exhausted }
         , cursor
         , Settlement.Next.Review_hold Successor_exhausted )
     | Ok next ->
       let%bind cursor =
         C.rearm_delay cursor ~occurrence:(Basis.occurrence basis) ~next
       in
       let state = T.retire_alarm state ~definition in
       let%map state =
         T.arm
           state
           ~definition
           ~slot:(Recurrence_rules.Cursor.Interval.slot next)
           ~due_at:(Recurrence_rules.Cursor.Interval.due_at next)
       in
       { state with issue = None }, cursor, Settlement.Next.Successor basis)
;;

let unavailable state ~definition key =
  let open Result.Let_syntax in
  let%bind mark = T.find_mark state definition in
  let slot = I.Occurrence.slot key in
  let nominal row =
    I.Occurrence.equal key (Recurrence_nominal.key (V.Stored_nominal.value row))
  in
  let retained =
    List.exists
      (state.S.ready @ T.candidates state @ Option.to_list state.lookahead)
      ~f:nominal
    || List.exists state.protected_children ~f:(fun row ->
      same_occurrence key (Recurrence_child_reservation.occurrence row))
    || List.exists state.history ~f:(fun detail ->
      I.Definition.equal detail.definition (I.Occurrence.definition key)
      && Int64.(detail.first_slot <= slot && slot <= detail.last_slot))
  in
  if retained
  then invalid "terminal import contradicts retained unadmitted disposition"
  else if Option.exists mark.through ~f:(fun through -> Int64.(slot <= through))
  then Ok ()
  else invalid "execution detail is absent above the consumed high-water"
;;

let import
      state
      ~definitions
      ~cursor
      ~origin
      ~terminal
      ~(automatic : Settlement.Automatic.t)
  =
  let open Result.Let_syntax in
  let key = Origin.occurrence origin in
  let%bind current =
    Result.of_option
      (List.last definitions)
      ~error:(P.Error.invalid_request "terminal import has no retained definition")
  in
  let%bind definition =
    Result.of_option
      (List.find definitions ~f:(fun definition ->
         I.Definition.equal (F.key definition) (I.Occurrence.definition key)))
      ~error:(P.Error.invalid_request "terminal import definition is not retained")
  in
  let%bind executions, imported =
    Recurrence_executions.import
      state.S.executions
      ~origin
      ~terminal
      ~max_history:(Recurrence_policy.max_history (F.policy current))
  in
  match imported with
  | Absent ->
    let%map () = unavailable state ~definition key in
    { state; cursor; result = Detail_not_retained key }
  | Retained execution -> Ok { state; cursor; result = Retained_exact execution }
  | Changed { execution; change = Associated } ->
    Ok
      { state = { state with executions }
      ; cursor
      ; result = Imported { execution; next = Evidence_only }
      }
  | Changed { execution; change = Settled retired } ->
    let%bind mark = T.find_mark state definition in
    let%bind settled = V.add mark.settled 1L in
    let state = T.update_mark { state with executions } { mark with settled } in
    let state =
      { state with
        protected_children =
          List.filter state.protected_children ~f:(fun row ->
            not
              (List.mem
                 retired
                 (Recurrence_child_reservation.occurrence row)
                 ~equal:I.Occurrence.equal))
      ; child_associations =
          List.filter state.child_associations ~f:(fun row ->
            not
              (List.mem
                 retired
                 (Recurrence_child_association.occurrence row)
                 ~equal:I.Occurrence.equal))
      }
    in
    let%bind terminal =
      Result.of_option
        terminal
        ~error:(P.Error.invalid_request "new settlement lacks exact terminal reference")
    in
    let is_current = I.Definition.equal (F.key definition) (F.key current) in
    let fixed_delay =
      match F.schedule definition with
      | Interval { rule; _ } ->
        Recurrence_rules.Rule.Interval.equal_mode
          (Recurrence_rules.Rule.Interval.mode rule)
          Fixed_delay
      | Calendar _ -> false
    in
    let%bind state, cursor, next =
      if not is_current
      then Ok (state, cursor, Settlement.Next.Evidence_only)
      else (
        match Reference.outcome terminal with
        | Interrupted ->
          Ok
            ( { state with issue = Some Execution_interrupted }
            , cursor
            , Settlement.Next.Review_hold Interrupted )
        | (Completed | Failed _ | Cancelled | Limited) when not fixed_delay ->
          Ok (state, cursor, Settlement.Next.Evidence_only)
        | Completed | Failed _ | Cancelled | Limited ->
          let%bind basis = Basis.create execution ~definition in
          let state =
            match state.delay_basis with
            | Some previous when not (Basis.equal previous basis) ->
              T.archive state current Retired_delay_basis (Basis.to_json previous)
            | None | Some _ -> state
          in
          let state = { state with delay_basis = Some basis } in
          (match automatic with
           | Historical_only -> Ok (state, cursor, Settlement.Next.Evidence_only)
           | Current_policy_allows -> resume state ~definition ~cursor))
    in
    let%bind state =
      match next with
      | Settlement.Next.Review_hold _ ->
        T.withdraw state ~definition:current ~preserve_lookahead:true
      | Evidence_only | Successor _ -> Ok state
    in
    Ok { state; cursor; result = Imported { execution; next } }
;;
