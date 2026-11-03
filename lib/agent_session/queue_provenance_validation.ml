open! Core
module P = Agent_protocol

type t =
  { session_id : P.Id.Session.t
  ; generation : int
  ; revision : int64
  ; moderator : Jsonaf.t option
  ; runs : Run_state.t option
  ; jobs : P.Job.t list
  ; schedules : P.Schedule.t list
  ; invocations : P.Invocation.t list
  ; events : P.Moderator_execution.t list
  }

let invalid message = Error (P.Error.invalid_request message)
let runs t = Option.value_map t.runs ~default:[] ~f:Run_state.runs

let work_exists (run : P.Run.t) ~f =
  List.exists run.owned_work ~f
  || List.exists run.relinquished_work ~f
  || List.exists run.terminal_work ~f:(fun terminal -> f terminal.work)
;;

let same_identity t session generation =
  if P.Id.Session.equal t.session_id session && Int.equal t.generation generation
  then Ok ()
  else invalid "queued producer session or generation differs"
;;

let source_matches source observer =
  if P.Invocation.equal_observer source.P.Run_source.observer observer
  then Ok ()
  else invalid "queued producer source differs from captured lifetime"
;;

let origin_matches ~expected actual =
  match expected with
  | None -> Ok ()
  | Some expected ->
    if Option.equal P.Inference_origin.equal (Some expected) actual
    then Ok ()
    else invalid "queued producer inference origin is unavailable or contradictory"
;;

let validate_origin t source = function
  | None | Some P.Inference_origin.Unbound -> Ok ()
  | Some (Run id) ->
    (match List.find (runs t) ~f:(fun run -> P.Id.Run.equal run.id id) with
     | Some run
       when P.Id.Session.equal t.session_id (P.Session_ref.session_id run.session)
            && P.Run_source.equal source run.source -> Ok ()
     | Some _ | None -> invalid "queued origin names an unavailable or foreign source Run")
;;

let validate_schedule t (schedule : P.Schedule.t) =
  let open Result.Let_syntax in
  let%bind () = P.Schedule.validate schedule in
  match schedule.inference_provenance with
  | None -> Ok ()
  | Some provenance ->
    let%bind () =
      if P.Id.Session.equal t.session_id schedule.session_id
      then Ok ()
      else invalid "Schedule original provenance belongs to another session"
    in
    let source = P.Schedule_provenance.source provenance in
    let origin = P.Schedule_provenance.origin provenance in
    let owners =
      List.filter (runs t) ~f:(fun run ->
        work_exists run ~f:(fun work ->
          Int.equal work.P.Run_work.generation schedule.generation
          &&
          match work.key with
          | Retained (Schedule id) -> P.Id.Schedule.equal id schedule.id
          | Operation _
          | Retained
              (Job _ | Invocation _ | Subscription _ | Delivery _ | Moderator_execution _)
            -> false))
    in
    (match origin, owners with
     | None, _ -> Ok ()
     | Some Unbound, [] -> Ok ()
     | Some (Run id), [ owner ]
       when P.Id.Run.equal id owner.id
            && P.Id.Session.equal
                 schedule.session_id
                 (P.Session_ref.session_id owner.session)
            && P.Run_source.equal source owner.source -> Ok ()
     | Some _, _ ->
       invalid "Schedule original inference provenance contradicts retained Run custody")
;;

let validate_producer t source entry =
  let open Result.Let_syntax in
  let expected = Moderator_queue.Entry.origin entry in
  let%bind () = validate_origin t source expected in
  match Moderator_queue.Entry.producer entry with
  | None -> Ok ()
  | Some (Queue_producer.Source_setup { basis_revision }) ->
    if
      Int64.(basis_revision <= t.revision)
      && Option.value_map expected ~default:true ~f:(P.Inference_origin.equal Unbound)
    then Ok ()
    else invalid "source setup cannot attribute a future Run or revision"
  | Some (Event id) ->
    (match
       List.find t.events ~f:(fun event ->
         P.Id.Moderator_execution.equal id event.context.id)
     with
     | None -> invalid "queued event producer receipt was removed"
     | Some event ->
       let%bind () = same_identity t event.context.session_id event.context.generation in
       let%bind () = source_matches source event.context.source in
       let%bind () =
         match event.status with
         | Completed _ -> Ok ()
         | Running | Failed _ | Interrupted _ ->
           invalid "queued event producer is not completed"
       in
       let%bind actual =
         Invocation_inference_provenance.of_event ~runs:t.runs ~jobs:t.jobs event
       in
       origin_matches ~expected actual)
  | Some (Invocation id) ->
    (match
       List.find t.invocations ~f:(fun invocation ->
         P.Id.Invocation.equal id invocation.context.id)
     with
     | None -> invalid "queued invocation producer receipt was removed"
     | Some invocation ->
       let%bind () =
         same_identity t invocation.context.session_id invocation.context.generation
       in
       let%bind () =
         match invocation.observation with
         | Some observation -> source_matches source observation.observer
         | None -> invalid "queued invocation producer has no captured moderator source"
       in
       let%bind () =
         match invocation.status with
         | Resolved _ | Published _ -> Ok ()
         | Admitted | Dispatching -> invalid "queued invocation producer is not resolved"
       in
       origin_matches ~expected invocation.inference_origin)
  | Some (Job_completion { id; generation; attempt }) ->
    (match List.find t.jobs ~f:(fun job -> P.Id.Job.equal id job.id) with
     | None -> invalid "queued Job completion producer was removed"
     | Some job ->
       let%bind () = same_identity t job.session_id generation in
       let%bind () =
         if Int.equal job.generation generation && Int.equal job.attempt attempt
         then Ok ()
         else invalid "queued Job completion attempt differs"
       in
       let%bind () =
         match job.status, job.delivery with
         | (Succeeded | Failed _ | Cancelled | Interrupted _), Delivered _ -> Ok ()
         | _ -> invalid "queued Job completion is not durably delivered"
       in
       let%bind () =
         match job.launch with
         | Some { moderator_source = Some observer; _ } -> source_matches source observer
         | None | Some { moderator_source = None; _ } -> Ok ()
       in
       origin_matches ~expected job.inference_origin)
  | Some (Schedule_delivery { id; generation; delivery_count }) ->
    (match
       List.find t.schedules ~f:(fun schedule -> P.Id.Schedule.equal id schedule.id)
     with
     | None -> invalid "queued Schedule delivery producer was removed"
     | Some schedule ->
       let%bind () = same_identity t schedule.session_id generation in
       let%bind () =
         if
           Int.equal schedule.generation generation
           && Int.equal schedule.delivery_count delivery_count
           &&
           match schedule.status with
           | Delivered -> true
           | Scheduled | Delivering | Cancelled | Failed _ -> false
         then Ok ()
         else invalid "queued Schedule delivery occurrence differs"
       in
       (match schedule.inference_provenance with
        | Some provenance
          when P.Run_source.equal source (P.Schedule_provenance.source provenance) ->
          origin_matches ~expected (P.Schedule_provenance.origin provenance)
        | Some _ | None ->
          invalid "queued Schedule original source is unavailable or differs"))
;;

let validate t =
  let open Result.Let_syntax in
  let%bind () =
    List.fold_result t.schedules ~init:() ~f:(fun () schedule ->
      validate_schedule t schedule)
  in
  let%bind metadata = Moderator_checkpoint.queue t.moderator in
  match metadata with
  | None -> Ok ()
  | Some metadata ->
    let%bind snapshot = Moderator_checkpoint.decode t.moderator in
    let%bind () =
      match snapshot with
      | Some _ -> Ok ()
      | None -> invalid "queued metadata has no checkpoint"
    in
    let source = Moderator_queue.source metadata in
    let%bind () =
      match t.runs with
      | None -> invalid "queued metadata has no actual captured source installation"
      | Some index ->
        let%bind captured =
          Run_source_installation.captured
            (Run_state.installation index)
            ~generation:t.generation
        in
        if P.Run_source.equal captured source
        then Ok ()
        else invalid "queued metadata source installation differs"
    in
    List.fold_result (Moderator_queue.entries metadata) ~init:() ~f:(fun () entry ->
      validate_producer t source entry)
;;

let rotated ~previous incoming =
  match previous.runs, incoming.runs with
  | Some old, Some next ->
    let%bind.Result () = Run_state.validate_transition ~previous:old next in
    let before = Run_state.installation old in
    let after = Run_state.installation next in
    if
      Int64.(before.epoch < max_value)
      && Int64.equal (Int64.succ before.epoch) after.epoch
    then Ok ()
    else invalid "queued lifetime reset requires an installation rotation"
  | None, _ | Some _, None ->
    invalid "queued lifetime reset discarded its installation evidence"
;;

let validate_transition ~previous incoming =
  let open Result.Let_syntax in
  let%bind () =
    List.fold_result previous.schedules ~init:() ~f:(fun () old ->
      match old.P.Schedule.inference_provenance with
      | None -> Ok ()
      | Some _ ->
        (match
           List.find incoming.schedules ~f:(fun next ->
             P.Id.Schedule.equal old.id next.id)
         with
         | None ->
           (* A retained queue reference is checked by final-state validation; a
            terminal unreferenced schedule may still follow ordinary retention. *)
           Ok ()
         | Some next ->
           if
             Option.equal
               P.Schedule_provenance.equal
               old.inference_provenance
               next.inference_provenance
           then Ok ()
           else invalid "Schedule original provenance changed"))
  in
  let%bind () =
    List.fold_result incoming.schedules ~init:() ~f:(fun () next ->
      match
        List.find previous.schedules ~f:(fun old -> P.Id.Schedule.equal old.id next.id)
      with
      | None -> Ok ()
      | Some old ->
        if
          Option.equal
            P.Schedule_provenance.equal
            old.inference_provenance
            next.inference_provenance
        then Ok ()
        else invalid "Schedule original provenance cannot be adopted later")
  in
  let%bind old = Moderator_checkpoint.queue previous.moderator in
  let%bind next = Moderator_checkpoint.queue incoming.moderator in
  match old, next with
  | None, None -> Ok ()
  | None, Some next ->
    let%bind before = Moderator_checkpoint.decode previous.moderator in
    let%bind after = Moderator_checkpoint.decode incoming.moderator in
    let%bind before_queue =
      match before with
      | None -> Ok []
      | Some before ->
        let observer : P.Invocation.observer =
          { script_id = before.script_id; source_sha256 = before.script_source_hash }
        in
        if P.Invocation.equal_observer observer (Moderator_queue.source next).observer
        then Ok before.queued_internal_events
        else (
          let%map () = rotated ~previous incoming in
          [])
    in
    let%bind baseline =
      Moderator_queue.bootstrap ~source:(Moderator_queue.source next) ~queue:before_queue
    in
    (match after with
     | None -> invalid "queued adoption has no identity checkpoint"
     | Some after ->
       Moderator_queue.validate_transition
         ~previous:baseline
         ~before:before_queue
         ~incoming:next
         ~after:after.queued_internal_events)
  | Some old, Some next
    when P.Run_source.equal (Moderator_queue.source old) (Moderator_queue.source next) ->
    let%bind before = Moderator_checkpoint.decode previous.moderator in
    let%bind after = Moderator_checkpoint.decode incoming.moderator in
    (match before, after with
     | Some before, Some after ->
       Moderator_queue.validate_transition
         ~previous:old
         ~before:before.queued_internal_events
         ~incoming:next
         ~after:after.queued_internal_events
     | _ -> invalid "queued transition has no identity checkpoint")
  | Some _, (Some _ | None) ->
    invalid
      "captured queue lifetime reset requires explicit source/reset carrier handling"
;;
