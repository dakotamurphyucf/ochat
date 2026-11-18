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
  ; ingress_registrations : External_ingress.t list
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
    let%bind () = validate_origin t source origin in
    (* Causal attribution creates no retained work. Any actual Schedule owner
       remains authoritative and must agree with the original Run. *)
    (match origin, owners with
     | None, _ | Some Unbound, [] | Some (Run _), [] -> Ok ()
     | Some (Run id), [ owner ] when P.Id.Run.equal id owner.id -> Ok ()
     | Some _, _ ->
       invalid "Schedule original inference provenance contradicts retained Run custody")
;;

let validate_ingress t (registration : External_ingress.t) =
  let open Result.Let_syntax in
  let%bind () = External_ingress.validate registration in
  match registration.inference_provenance with
  | None -> Ok ()
  | Some provenance ->
    if
      P.Id.Session.equal t.session_id registration.context.session_id
      && registration.context.generation <= t.generation
    then
      validate_origin
        t
        (External_ingress.Provenance.source provenance)
        (External_ingress.Provenance.origin provenance)
    else invalid "ingress original provenance belongs to another session or generation"
;;

let retained_job_producer t source entry ~id ~generation ~attempt =
  let open Result.Let_syntax in
  let%bind retained =
    Retained_job_producer.capture
      ~session_id:t.session_id
      ~generation:t.generation
      ~runs:t.runs
      ~source
      ~origin:(Moderator_queue.Entry.origin entry)
      ~id
      ~job_generation:generation
      ~attempt
  in
  Retained_job_producer.check_payload
    retained
    ~sha256:(Moderator_queue.Entry.payload_sha256 entry)
;;

let validate_producer t source entry =
  let open Result.Let_syntax in
  let expected = Moderator_queue.Entry.origin entry in
  let%bind () = validate_origin t source expected in
  match Moderator_queue.Entry.producer entry with
  | None -> Ok ()
  | Some (Queue_producer.Ingress_delivery { registration_id; event_id }) ->
    let%bind registration =
      List.find t.ingress_registrations ~f:(fun registration ->
        P.Id.Capability.equal registration.context.id registration_id)
      |> Result.of_option
           ~error:(P.Error.invalid_request "queued ingress registration was removed")
    in
    let%bind () = validate_ingress t registration in
    let%bind () =
      same_identity t registration.context.session_id registration.context.generation
    in
    let%bind provenance =
      Result.of_option
        registration.inference_provenance
        ~error:(P.Error.invalid_request "queued ingress original provenance is absent")
    in
    let%bind () =
      if
        P.Run_source.equal source (External_ingress.Provenance.source provenance)
        && Option.equal
             P.Inference_origin.equal
             expected
             (External_ingress.Provenance.origin provenance)
      then Ok ()
      else invalid "queued ingress source or original inference origin changed"
    in
    let%bind receipt =
      List.find registration.receipts ~f:(fun receipt ->
        P.Id.Ingress_event.equal receipt.id event_id)
      |> Result.of_option
           ~error:(P.Error.invalid_request "queued ingress receipt was removed")
    in
    let%bind frame = External_ingress.delivery_frame registration receipt in
    let%bind payload =
      Chat_response.Ingress_delivery.capture frame
      |> Session.Snapshot.of_value
      |> Result.map_error ~f:P.Error.invalid_request
    in
    let%bind hashes = Queue_payload.digests [ payload ] in
    if List.equal String.equal hashes [ Moderator_queue.Entry.payload_sha256 entry ]
    then Ok ()
    else invalid "queued ingress payload differs from its exact retained receipt"
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
  | Some (Invocation_at_source { id; source = captured }) ->
    let%bind () = P.Run_source.validate captured in
    let%bind () =
      if P.Run_source.equal captured source
      then Ok ()
      else invalid "queued invocation producer differs from captured installation"
    in
    (match
       List.find t.invocations ~f:(fun invocation ->
         P.Id.Invocation.equal id invocation.context.id)
     with
     | None -> invalid "queued invocation producer receipt was removed"
     | Some invocation ->
       let%bind () = P.Invocation.validate invocation in
       let%bind () =
         same_identity t invocation.context.session_id invocation.context.generation
       in
       let%bind () =
         match invocation.observation with
         | None -> Ok ()
         | Some _ -> invalid "source-qualified invocation producer has observation intent"
       in
       let%bind () =
         match invocation.status with
         | Resolved _ | Published _ -> Ok ()
         | Admitted | Dispatching -> invalid "queued invocation producer is not resolved"
       in
       if Option.equal P.Inference_origin.equal expected invocation.inference_origin
       then Ok ()
       else invalid "source-qualified invocation producer changed its inference origin")
  | Some (Job_completion { id; generation; attempt }) ->
    (match List.find t.jobs ~f:(fun job -> P.Id.Job.equal id job.id) with
     | Some job when Int.equal job.generation generation && Int.equal job.attempt attempt
       ->
       let%bind () = same_identity t job.session_id generation in
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
       origin_matches ~expected job.inference_origin
     | Some _ | None ->
       (* Retry may replace the latest Job before an exact authored Wait consumes
          its old terminal occurrence. The durable retained frame, enqueue and
          Run owner are independent facts; latest-attempt equality is not that
          occurrence's identity and must never redirect it to the successor. *)
       retained_job_producer t source entry ~id ~generation ~attempt)
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

module Occurrence_identity = struct
  type t = string * string * int * int64 * int64 [@@deriving compare, hash, sexp]
end

let occurrence_identity occurrence =
  let module Q = P.Moderator_execution.Queue_occurrence in
  let source = Q.source occurrence in
  ( source.observer.script_id
  , source.observer.source_sha256
  , source.generation
  , source.installation_epoch
  , Q.ordinal occurrence )
;;

let installed_source t source =
  let open Result.Let_syntax in
  match t.runs with
  | None -> invalid "captured queue claim has no source installation"
  | Some index ->
    let%bind captured =
      Run_source_installation.captured
        (Run_state.installation index)
        ~generation:t.generation
    in
    if P.Run_source.equal source captured
    then Ok ()
    else invalid "captured queue claim differs from installed source"
;;

let previous_head t source =
  let open Result.Let_syntax in
  let%bind snapshot = Moderator_checkpoint.decode t.moderator in
  let%bind snapshot =
    match snapshot with
    | None -> invalid "captured queue claim has no previous checkpoint"
    | Some snapshot -> Ok snapshot
  in
  let%bind () =
    source_matches
      source
      { script_id = snapshot.script_id; source_sha256 = snapshot.script_source_hash }
  in
  let%bind metadata = Moderator_checkpoint.queue t.moderator in
  let%bind metadata =
    match metadata with
    | Some metadata ->
      if P.Run_source.equal source (Moderator_queue.source metadata)
      then Ok metadata
      else invalid "captured queue claim differs from previous queue lifetime"
    | None ->
      (* Number only the genuine previous queue. No incoming capture or drainer
         may lend producer/origin facts to this historical structural bootstrap. *)
      Moderator_queue.bootstrap ~source ~queue:snapshot.queued_internal_events
  in
  match snapshot.queued_internal_events, Moderator_queue.entries metadata with
  | head :: _, entry :: _ -> Ok (snapshot, head, entry)
  | [], _ | _, [] -> invalid "captured queue claim has no previous head"
;;

let validate_head_payload (event : P.Moderator_execution.t) occurrence ~head ~entry =
  let open Result.Let_syntax in
  let module Q = P.Moderator_execution.Queue_occurrence in
  let%bind () =
    if
      Int64.equal (Q.ordinal occurrence) (Moderator_queue.Entry.ordinal entry)
      && Option.equal
           P.Inference_origin.equal
           (Q.origin occurrence)
           (Moderator_queue.Entry.origin entry)
    then Ok ()
    else invalid "captured queue claim differs from head ordinal or origin"
  in
  let%bind () =
    match Q.producer occurrence with
    | Unknown -> Ok ()
    | Captured producer ->
      if Option.equal Queue_producer.equal producer (Moderator_queue.Entry.producer entry)
      then Ok ()
      else invalid "captured queue claim differs from head producer"
  in
  let%bind captured = Queue_payload.receipt_snapshot event in
  let%bind captured =
    match captured with
    | None -> invalid "captured queue claim has no snapshot payload"
    | Some captured -> Ok captured
  in
  let%bind hashes = Queue_payload.digests [ head; captured ] in
  match hashes with
  | [ expected; actual ]
    when String.equal expected actual
         && String.equal expected (Moderator_queue.Entry.payload_sha256 entry) -> Ok ()
  | _ -> invalid "captured queue claim differs from head payload"
;;

let validate_head t (event : P.Moderator_execution.t) occurrence =
  let open Result.Let_syntax in
  let module Q = P.Moderator_execution.Queue_occurrence in
  let%bind snapshot, head, entry = previous_head t (Q.source occurrence) in
  let%bind () =
    if String.equal event.context.checkpoint_sha256 (Moderator_checkpoint.digest snapshot)
    then Ok ()
    else invalid "captured queue claim differs from previous checkpoint"
  in
  validate_head_payload event occurrence ~head ~entry
;;

let validate_event_admission t (event : P.Moderator_execution.t) =
  let open Result.Let_syntax in
  match event.queue_occurrence with
  | None -> Ok ()
  | Some occurrence ->
    let%bind () = P.Moderator_execution.validate event in
    let%bind () =
      match event.status with
      | Running -> Ok ()
      | Completed _ | Failed _ | Interrupted _ ->
        invalid "fresh queue claim must start Running"
    in
    let%bind () = same_identity t event.context.session_id event.context.generation in
    let%bind () =
      installed_source t (P.Moderator_execution.Queue_occurrence.source occurrence)
    in
    let%bind () =
      if
        List.exists t.events ~f:(fun old ->
          Option.exists old.P.Moderator_execution.queue_occurrence ~f:(fun old ->
            Occurrence_identity.compare
              (occurrence_identity old)
              (occurrence_identity occurrence)
            = 0))
      then invalid "previous queue occurrence already has an Event receipt"
      else Ok ()
    in
    validate_head t event occurrence
;;

let validate_disposition incoming (event : P.Moderator_execution.t) occurrence =
  let open Result.Let_syntax in
  let module Q = P.Moderator_execution.Queue_occurrence in
  let%bind metadata = Moderator_checkpoint.queue incoming.moderator in
  let%bind metadata =
    match metadata with
    | Some metadata
      when P.Run_source.equal (Moderator_queue.source metadata) (Q.source occurrence) ->
      Ok metadata
    | Some _ | None -> invalid "captured queue claim lost its source frontier"
  in
  let consumed =
    match event.status, event.retirement with
    | Completed _, _ | (Failed _ | Interrupted _), Some _ -> true
    | Running, _ | (Failed _ | Interrupted _), None -> false
  in
  let ordinal = Q.ordinal occurrence in
  let%bind expected =
    if consumed
    then
      if Int64.equal ordinal Int64.max_value
      then invalid "queue occurrence frontier overflow"
      else Ok (Int64.succ ordinal)
    else Ok ordinal
  in
  let%bind () =
    if Int64.equal expected (Moderator_queue.head_ordinal metadata)
    then Ok ()
    else invalid "queue Event disposition differs from consumed frontier"
  in
  let checkpoint =
    match event.status, event.retirement with
    | Completed checkpoint, _ -> Some checkpoint
    | (Failed _ | Interrupted _), Some retirement -> Some retirement.checkpoint_sha256
    | Running, _ | (Failed _ | Interrupted _), None -> None
  in
  match checkpoint with
  | None -> Ok ()
  | Some checkpoint ->
    let%bind snapshot = Moderator_checkpoint.decode incoming.moderator in
    (match snapshot with
     | Some snapshot when String.equal checkpoint (Moderator_checkpoint.digest snapshot)
       -> Ok ()
     | Some _ | None ->
       invalid "queue Event disposition differs from committed checkpoint")
;;

let validate_event_retention ~previous incoming =
  let open Result.Let_syntax in
  let module E = P.Moderator_execution in
  List.fold_result previous.events ~init:() ~f:(fun () old ->
    match old.E.queue_occurrence with
    | None -> Ok ()
    | Some occurrence ->
      (match
         List.find incoming.events ~f:(fun event ->
           P.Id.Moderator_execution.equal old.context.id event.E.context.id)
       with
       | Some event ->
         if
           Option.equal
             E.Queue_occurrence.equal
             old.queue_occurrence
             event.queue_occurrence
         then Ok ()
         else invalid "retained queue occurrence capture changed"
       | None ->
         (match old.status, old.retirement with
          | Running, _ -> invalid "Running captured queue Event cannot disappear"
          | (Failed _ | Interrupted _), None ->
            let source = E.Queue_occurrence.source occurrence in
            (match previous.runs, incoming.runs with
             | Some before, Some after
               when Int64.(
                      (Run_state.installation after).epoch > source.installation_epoch)
                    && incoming.generation >= source.generation ->
               (* Only retained actual installation progression makes the old
                  terminal occurrence unreclaimable; decoded observer names do not. *)
               Run_state.validate_transition ~previous:before after
             | (None | Some _), (None | Some _) ->
               invalid "unretired captured queue Event still blocks its occurrence")
          | Completed _, _ | (Failed _ | Interrupted _), Some _ -> Ok ())))
;;

let validate_event_updates ~previous incoming =
  let open Result.Let_syntax in
  let%bind () = validate_event_retention ~previous incoming in
  List.fold_result incoming.events ~init:() ~f:(fun () event ->
    let module E = P.Moderator_execution in
    match
      List.find previous.events ~f:(fun old ->
        P.Id.Moderator_execution.equal old.E.context.id event.E.context.id)
    with
    | Some old ->
      let%bind () =
        if
          Option.equal
            E.Queue_occurrence.equal
            old.queue_occurrence
            event.queue_occurrence
        then Ok ()
        else invalid "queue occurrence cannot be adopted, removed or reassigned"
      in
      (match event.queue_occurrence with
       | None -> Ok ()
       | Some occurrence ->
         let%bind () = E.validate_transition ~previous:(Some old) event in
         let newly_settled =
           match old.status, event.status with
           | Running, (Completed _ | Failed _ | Interrupted _) -> true
           | Running, Running
           | ( (Completed _ | Failed _ | Interrupted _)
             , (Running | Completed _ | Failed _ | Interrupted _) ) ->
             Option.is_none old.retirement && Option.is_some event.retirement
         in
         if newly_settled then validate_disposition incoming event occurrence else Ok ())
    | None ->
      (match event.queue_occurrence with
       | None -> Ok ()
       | Some occurrence ->
         let%bind () =
           same_identity incoming event.context.session_id event.context.generation
         in
         let%bind () = installed_source incoming (E.Queue_occurrence.source occurrence) in
         let%bind () =
           match
             List.find previous.events ~f:(fun old ->
               Option.exists old.E.queue_occurrence ~f:(fun old ->
                 Occurrence_identity.compare
                   (occurrence_identity old)
                   (occurrence_identity occurrence)
                 = 0))
           with
           | None -> Ok ()
           | Some _ -> invalid "previous queue occurrence already has an Event receipt"
         in
         (* The final receipt may already be Completed after valid ordered
            Running/Completed leaves. Compare against ORIGINAL previous facts. *)
         let%bind () = validate_head previous event occurrence in
         validate_disposition incoming event occurrence))
;;

let validate_retained_position t metadata (event : P.Moderator_execution.t) occurrence =
  let open Result.Let_syntax in
  let module Q = P.Moderator_execution.Queue_occurrence in
  let source = Q.source occurrence in
  let%bind installation =
    match t.runs with
    | None -> invalid "captured queue Event lost its installation history"
    | Some index -> Ok (Run_state.installation index)
  in
  let%bind () =
    if source.generation <= t.generation
    then Ok ()
    else invalid "captured queue Event names a future generation"
  in
  if Int64.(source.installation_epoch > installation.epoch)
  then invalid "captured queue Event names a future installation"
  else if Int64.(source.installation_epoch < installation.epoch)
  then (
    (* An actual later retained epoch also covers source removal, where
       installation.source is None. Do not call captured before this branch.
       Terminal history needs no invented old queue or renewed permission. *)
    match event.status with
    | Running -> invalid "Running captured queue Event outlived its installation"
    | Completed _ | Failed _ | Interrupted _ -> Ok ())
  else (
    let%bind current =
      Run_source_installation.captured installation ~generation:t.generation
    in
    let%bind () =
      if P.Run_source.equal source current
      then Ok ()
      else invalid "captured queue Event differs from its installation identity"
    in
    let%bind metadata =
      match metadata with
      | Some metadata when P.Run_source.equal source (Moderator_queue.source metadata) ->
        Ok metadata
      | Some _ | None -> invalid "captured queue Event lost its current lifetime frontier"
    in
    match event.status, event.retirement with
    | Running, _ | (Failed _ | Interrupted _), None ->
      let%bind _, head, entry = previous_head t source in
      (* Unrelated later checkpoint edits can preserve this exact blocked head;
         final consistency does not compare the old context checkpoint hash. *)
      validate_head_payload event occurrence ~head ~entry
    | Completed _, _ | (Failed _ | Interrupted _), Some _ ->
      if Int64.(Q.ordinal occurrence < Moderator_queue.head_ordinal metadata)
      then Ok ()
      else invalid "terminal captured queue Event occurrence was not consumed")
;;

let validate_captured_events t =
  let open Result.Let_syntax in
  let%bind metadata = Moderator_checkpoint.queue t.moderator in
  let seen = Hash_set.create (module Occurrence_identity) in
  List.fold_result t.events ~init:() ~f:(fun () event ->
    match event.P.Moderator_execution.queue_occurrence with
    | None -> Ok ()
    | Some occurrence ->
      let%bind () = P.Moderator_execution.validate event in
      let%bind () =
        if
          P.Id.Session.equal t.session_id event.context.session_id
          && event.context.generation <= t.generation
        then Ok ()
        else
          invalid "captured queue Event belongs to another session or future generation"
      in
      let%bind () =
        validate_origin
          t
          (P.Moderator_execution.Queue_occurrence.source occurrence)
          (P.Moderator_execution.Queue_occurrence.origin occurrence)
      in
      let%bind _ =
        Invocation_inference_provenance.of_event ~runs:t.runs ~jobs:t.jobs event
      in
      let%bind () = validate_retained_position t metadata event occurrence in
      let identity = occurrence_identity occurrence in
      if Hash_set.mem seen identity
      then invalid "queue occurrence has multiple Event receipts"
      else (
        Hash_set.add seen identity;
        Ok ()))
;;

let validate t =
  let open Result.Let_syntax in
  let%bind () = validate_captured_events t in
  let%bind () =
    List.fold_result t.schedules ~init:() ~f:(fun () schedule ->
      validate_schedule t schedule)
  in
  let%bind () =
    List.fold_result t.ingress_registrations ~init:() ~f:(fun () registration ->
      validate_ingress t registration)
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
  let open Result.Let_syntax in
  let%bind () =
    if
      P.Id.Session.equal previous.session_id incoming.session_id
      && (Int.equal previous.generation incoming.generation
          || (previous.generation < Int.max_value
              && Int.equal (previous.generation + 1) incoming.generation))
    then Ok ()
    else invalid "queued lifetime reset belongs to another session or generation"
  in
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

let validate_queue_start incoming metadata ~before =
  let open Result.Let_syntax in
  let%bind snapshot = Moderator_checkpoint.decode incoming.moderator in
  let%bind snapshot =
    Result.of_option
      snapshot
      ~error:(P.Error.invalid_request "queued adoption has no identity checkpoint")
  in
  let%bind baseline =
    Moderator_queue.bootstrap ~source:(Moderator_queue.source metadata) ~queue:before
  in
  Moderator_queue.validate_transition
    ~previous:baseline
    ~before
    ~incoming:metadata
    ~after:snapshot.queued_internal_events
;;

let validate_queue_retirement ~previous incoming ~old ~next =
  let open Result.Let_syntax in
  let source = Moderator_queue.source old in
  let%bind () = installed_source previous source in
  let%bind snapshot = Moderator_checkpoint.decode previous.moderator in
  let%bind snapshot =
    Result.of_option
      snapshot
      ~error:
        (P.Error.invalid_request "retiring queue has no original identity checkpoint")
  in
  let observer : P.Invocation.observer =
    { script_id = snapshot.script_id; source_sha256 = snapshot.script_source_hash }
  in
  let%bind () = source_matches source observer in
  let%bind () = Moderator_queue.validate_queue old snapshot.queued_internal_events in
  let%bind () = rotated ~previous incoming in
  let%bind installation =
    match incoming.runs with
    | Some index -> Ok (Run_state.installation index)
    | None -> invalid "queue retirement lost its destination installation"
  in
  let%bind observer = Moderator_checkpoint.observer incoming.moderator in
  let%bind () =
    if Option.equal P.Invocation.equal_observer installation.source observer
    then Ok ()
    else invalid "queue retirement destination differs from its installed source"
  in
  let%bind () =
    if
      List.exists previous.events ~f:(fun event ->
        match event.P.Moderator_execution.status, event.queue_occurrence with
        | Running, Some occurrence ->
          P.Run_source.equal
            source
            (P.Moderator_execution.Queue_occurrence.source occurrence)
        | Running, None | (Completed _ | Failed _ | Interrupted _), _ -> false)
    then invalid "Running queue occurrence cannot be retired by a source change"
    else Ok ()
  in
  let%bind () =
    match next with
    | None -> Ok ()
    | Some metadata ->
      let%bind () = installed_source incoming (Moderator_queue.source metadata) in
      validate_queue_start incoming metadata ~before:[]
  in
  (* A new epoch supplies no producer evidence: authenticate the actual final
     references as well as the ordinal-zero structural frontier. *)
  validate incoming
;;

let validate_transition ~previous incoming =
  let open Result.Let_syntax in
  let%bind () =
    Moderator_checkpoint.validate_update ~previous:previous.moderator incoming.moderator
  in
  let%bind () = validate_event_updates ~previous incoming in
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
  let%bind () =
    List.fold_result incoming.ingress_registrations ~init:() ~f:(fun () next ->
      match
        List.find previous.ingress_registrations ~f:(fun old ->
          P.Id.Capability.equal old.context.id next.context.id)
      with
      | None -> Ok ()
      | Some old ->
        if
          Option.equal
            External_ingress.Provenance.equal
            old.inference_provenance
            next.inference_provenance
        then Ok ()
        else invalid "ingress original provenance cannot change or be adopted later")
  in
  let%bind old = Moderator_checkpoint.queue previous.moderator in
  let%bind next = Moderator_checkpoint.queue incoming.moderator in
  match old, next with
  | None, None -> Ok ()
  | None, Some next ->
    let%bind before = Moderator_checkpoint.decode previous.moderator in
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
    validate_queue_start incoming next ~before:before_queue
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
  | Some old, ((Some _ | None) as next) ->
    validate_queue_retirement ~previous incoming ~old ~next
;;

let validate_encoded_event raw =
  match Document_schema.Json.field raw ~name:"queue_occurrence" with
  | Absent | Null -> Ok ()
  | Value capture ->
    P.Moderator_execution.Queue_occurrence.validate_encoded_capacity capture
;;
