open! Core
module P = Agent_protocol

module Schedule_id = struct
  include P.Id.Schedule
  include Comparator.Make (P.Id.Schedule)
end

let invalid message = Error (P.Error.invalid_request message)

let rec replacement = function
  | Session_delta.Created _ -> true
  | Batch changes -> List.exists changes ~f:replacement
  | _ -> false
;;

let rec selected_schedules = function
  | Session_delta.Schedule_changed schedule -> [ schedule ]
  | Batch changes -> List.concat_map changes ~f:selected_schedules
  | _ -> []
;;

let selected_ids delta =
  selected_schedules delta
  |> List.map ~f:(fun schedule -> schedule.P.Schedule.id)
  |> List.dedup_and_sort ~compare:P.Id.Schedule.compare
;;

let terminal = function
  | P.Schedule.Delivered | Cancelled | Failed _ -> true
  | Scheduled | Delivering -> false
;;

let owns (run : P.Run.t) (schedule : P.Schedule.t) =
  List.exists run.owned_work ~f:(fun work ->
    Int.equal work.P.Run_work.generation schedule.generation
    &&
    match work.key with
    | Retained (Schedule id) -> P.Id.Schedule.equal id schedule.id
    | Operation _
    | Retained (Job _ | Invocation _ | Subscription _ | Delivery _ | Moderator_execution _)
      -> false)
;;

let requires_completion_sample (state : Session_state.t) ~delta =
  (not (replacement delta))
  && Option.exists state.run_state ~f:(fun index ->
    List.exists (selected_schedules delta) ~f:(fun schedule ->
      terminal schedule.P.Schedule.status
      && List.exists state.schedules ~f:(fun before ->
        P.Id.Schedule.equal before.id schedule.id && not (terminal before.status))
      && List.exists (Run_state.runs index) ~f:(fun run ->
        (match run.P.Run.lifecycle with
         | Admitted | Active | Waiting _ -> true
         | Terminal _ -> false)
        && owns run schedule)))
;;

let immutable (before : P.Schedule.t) (after : P.Schedule.t) =
  P.Id.Schedule.equal before.id after.id
  && P.Id.Session.equal before.session_id after.session_id
  && Int.equal before.generation after.generation
  && Jsonaf.exactly_equal before.payload after.payload
  && P.Timestamp.equal before.created_at after.created_at
  && P.Timestamp.equal before.next_due_at after.next_due_at
  && P.Schedule.equal_misfire before.misfire after.misfire
  && Option.equal
       P.Schedule_provenance.equal
       before.inference_provenance
       after.inference_provenance
  &&
  match before.ownership, after.ownership with
  | None, None -> true
  | Some before, Some after ->
    P.Invocation.equal_observer before.source after.source
    && P.Job.equal_launch_owner before.creator after.creator
    &&
      (match before.subscription, after.subscription with
      | None, _ -> true
      | Some (id, epoch), Some (other, other_epoch) ->
        P.Id.Subscription.equal id other && Int.equal epoch other_epoch
      | Some _, None -> false)
  | None, Some _ | Some _, None -> false
;;

let outcome (before : P.Schedule.t) (after : P.Schedule.t) =
  let open Result.Let_syntax in
  let%bind () = P.Schedule.validate before in
  let%bind () = P.Schedule.validate after in
  let%bind () =
    if immutable before after && Option.is_none before.delivery_cancellation
    then Ok ()
    else invalid "Schedule terminal evidence changes its retained producer identity"
  in
  match before.status, after.status with
  | (Scheduled | Delivering), Cancelled -> Ok P.Run_work.Terminal.Cancelled
  | (Scheduled | Delivering), Failed _ -> Ok Failed
  | (Scheduled | Delivering), Delivered
    when before.delivery_count < Int.max_value
         && Int.equal after.delivery_count (Int.succ before.delivery_count)
         && Option.is_some after.last_delivery_at -> Ok Succeeded
  | Scheduled, Delivered
    when Int.equal before.delivery_count after.delivery_count
         && Option.is_none before.last_delivery_at
         && Option.is_none after.last_delivery_at -> Ok Limited
  | _ ->
    invalid "Schedule terminal evidence has no actual completion, skip or cancellation"
;;

let capture_projected
      (state : Session_state.t)
      ~(next : Session_state.t)
      ~delta
      ~(run : P.Run.t)
      ~owned_work
  =
  let open Result.Let_syntax in
  let%bind () =
    if
      Option.exists state.run_state ~f:(fun index ->
        Option.exists (Run_state.find index run.id) ~f:(P.Run.equal run))
    then Ok ()
    else invalid "Schedule evidence requires its exact retained Run"
  in
  if replacement delta
  then Ok []
  else (
    let%bind () =
      if
        P.Id.Session.equal state.identity.session_id next.identity.session_id
        && Int.equal state.identity.generation next.identity.generation
      then Ok ()
      else invalid "Schedule evidence changes session or generation"
    in
    let selected = Set.of_list (module Schedule_id) (selected_ids delta) in
    Set.of_list (module P.Run_work) (run.owned_work @ owned_work)
    |> Set.to_list
    |> List.filter_map ~f:(fun work ->
      match work.P.Run_work.key with
      | Retained (Schedule id) when Set.mem selected id -> Some (work, id)
      | Operation _ | Retained _ -> None)
    |> List.fold_result ~init:[] ~f:(fun proofs (work, id) ->
      (* Absence is not a terminal record to backfill. Only the first real
         Scheduled creation leaf can establish a new producer in this exact
         callback candidate, whose full sequential delta was validated above. *)
      let before =
        match
          List.find state.schedules ~f:(fun schedule ->
            P.Id.Schedule.equal id schedule.id)
        with
        | Some _ as before -> before
        | None ->
          List.find (selected_schedules delta) ~f:(fun schedule ->
            P.Id.Schedule.equal id schedule.id
            &&
            match schedule.status with
            | Scheduled -> true
            | Delivering | Delivered | Cancelled | Failed _ -> false)
      in
      match before with
      | None -> Ok proofs
      | Some before when terminal before.status -> Ok proofs
      | Some before ->
        let%bind after =
          List.find next.schedules ~f:(fun schedule -> P.Id.Schedule.equal id schedule.id)
          |> Result.of_option
               ~error:(P.Error.invalid_request "selected Schedule disappeared")
        in
        if not (terminal after.status)
        then Ok proofs
        else (
          let%bind () =
            if
              P.Id.Session.equal before.session_id state.identity.session_id
              && Int.equal work.generation before.generation
              && Int.equal work.generation run.source.generation
              && Option.value_map before.ownership ~default:true ~f:(fun ownership ->
                P.Invocation.equal_observer ownership.source run.source.observer)
              && Option.value_map
                   before.inference_provenance
                   ~default:true
                   ~f:(fun provenance ->
                     P.Run_source.equal
                       (P.Schedule_provenance.source provenance)
                       run.source
                     &&
                     match P.Schedule_provenance.origin provenance with
                     | Some (Run id) -> P.Id.Run.equal id run.id
                     | None -> true
                     | Some Unbound -> false)
            then Ok ()
            else
              invalid
                "Schedule terminal evidence differs from its actual owned occurrence"
          in
          let%bind outcome = outcome before after in
          match
            List.find run.terminal_work ~f:(fun proof -> P.Run_work.equal proof.work work)
          with
          | Some proof ->
            if P.Run_work.Terminal.equal_outcome proof.outcome outcome
            then Ok proofs
            else invalid "Schedule terminal evidence contradicts an immutable outcome"
          | None ->
            if Int64.equal run.revision Int64.max_value
            then invalid "Run revision exhausted at Schedule completion"
            else (
              let%map proof =
                P.Run_work.Terminal.create
                  ~work
                  ~outcome
                  ~revision:(Int64.succ run.revision)
              in
              proof :: proofs))))
;;

let capture state ~delta ~run ~owned_work =
  let open Result.Let_syntax in
  let%bind next = Session_delta.apply state delta in
  let%map proofs = capture_projected state ~next ~delta ~run ~owned_work in
  next, proofs
;;

let loses_wait (run : P.Run.t) proofs =
  match run.lifecycle with
  | Waiting { occurrence = Delivered_timer { schedule_id; _ }; _ } ->
    List.exists proofs ~f:(fun proof ->
      (match proof.P.Run_work.Terminal.work.key with
       | Retained (Schedule id) -> P.Id.Schedule.equal id schedule_id
       | Operation _ | Retained _ -> false)
      && not (P.Run_work.Terminal.equal_outcome proof.outcome Succeeded))
  | Admitted | Active | Waiting _ | Terminal _ -> false
;;

let prepare ?startup_run (state : Session_state.t) ~delta ~now =
  if replacement delta
  then Ok delta
  else (
    let selected = selected_ids delta in
    if List.is_empty selected
    then Ok delta
    else
      let open Result.Let_syntax in
      let%bind candidate = Session_delta.apply state delta in
      match state.run_state, candidate.run_state with
      | None, _ | Some _, None -> Ok delta
      | Some previous_index, Some original ->
        let%map index =
          List.fold_result (Run_state.runs original) ~init:original ~f:(fun index run ->
            match run.P.Run.lifecycle, Run_state.find previous_index run.id with
            | _, None -> Ok index
            | _, Some previous
              when (match previous.P.Run.lifecycle with
                    | Terminal _ -> true
                    | Admitted | Active | Waiting _ -> false)
                   || Run_retirement.matches_interrupt ~previous run -> Ok index
            | (Admitted | Active | Waiting _ | Terminal _), Some previous ->
              let%bind proofs =
                capture_projected
                  state
                  ~next:candidate
                  ~delta
                  ~run:previous
                  ~owned_work:run.owned_work
              in
              if List.is_empty proofs
              then Ok index
              else if not (P.Run.equal previous run)
              then (
                let%map () =
                  if
                    List.for_all proofs ~f:(fun expected ->
                      List.exists
                        run.terminal_work
                        ~f:(P.Run_work.Terminal.equal expected))
                  then Ok ()
                  else invalid "Schedule evidence must share the existing Run transaction"
                in
                index)
              else if loses_wait run proofs
              then (
                let%bind () =
                  match run.lifecycle with
                  | Waiting wake -> Run_wake_owner.validate state ~run ~wake
                  | Admitted | Active | Terminal _ -> Ok ()
                in
                Run_retirement.interrupt_with_evidence
                  index
                  ~run_id:run.id
                  ~evidence:proofs
                  ~session_revision:(Int64.succ state.counters.revision)
                  ~now)
              else
                Run_work_settlement.prepare
                  ?startup_run
                  index
                  ~state:candidate
                  ~previous:run
                  ~owned_work:run.owned_work
                  ~terminal_work:(run.terminal_work @ proofs)
                  ~operation_id:None
                  ~now)
        in
        if Run_state.equal original index
        then delta
        else Session_delta.Batch [ delta; Run_state_changed index ])
;;
