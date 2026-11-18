open! Core
module P = Agent_protocol
module Cause = Policy_cause_data
module Member = Cause.Member

type t =
  { session_id : P.Id.Session.t
  ; generation : int
  ; current_source : P.Run_source.t option
  ; events : P.Moderator_execution.t list
  ; invocations : P.Invocation.t list
  ; jobs : P.Job.t list
  ; runs : Run_state.t option
  }

let invalid message = Error (P.Error.invalid_request message)

let create ~session_id ~generation ~current_source ~events ~invocations ~jobs ~runs =
  let open Result.Let_syntax in
  let%bind () =
    if generation >= 0 then Ok () else invalid "negative policy native generation"
  in
  let%bind () =
    Option.value_map current_source ~default:(Ok ()) ~f:(fun source ->
      let%bind () = P.Run_source.validate source in
      if Int.equal source.generation generation
      then Ok ()
      else invalid "policy native source generation differs")
  in
  Ok { session_id; generation; current_source; events; invocations; jobs; runs }
;;

let of_context (context : Cause.Context.t) =
  { session_id = context.session_id
  ; generation = context.generation
  ; current_source = context.current_source
  ; events = context.events
  ; invocations = context.invocations
  ; jobs = context.jobs
  ; runs = context.runs
  }
;;

let check_context t (context : Cause.Context.t) =
  if
    P.Id.Session.equal t.session_id context.session_id
    && Int.equal t.generation context.generation
    && Option.equal P.Run_source.equal t.current_source context.current_source
    && List.equal
         (fun a b ->
            Jsonaf.exactly_equal
              (P.Moderator_execution.to_json a)
              (P.Moderator_execution.to_json b))
         t.events
         context.events
    && List.equal
         (fun a b ->
            Jsonaf.exactly_equal (P.Invocation.to_json a) (P.Invocation.to_json b))
         t.invocations
         context.invocations
    && Option.equal Run_state.equal t.runs context.runs
    && List.equal
         (fun a b -> Jsonaf.exactly_equal (P.Job.to_json a) (P.Job.to_json b))
         t.jobs
         context.jobs
  then Ok ()
  else invalid "policy native prefix differs from complete context"
;;

let check_owner t ~session_id ~generation =
  if P.Id.Session.equal t.session_id session_id && Int.equal t.generation generation
  then Ok ()
  else invalid "policy native work belongs to another session or generation"
;;

let check_retained_owner t ~session_id ~generation =
  if
    P.Id.Session.equal t.session_id session_id
    && generation >= 0
    && generation <= t.generation
  then Ok ()
  else
    invalid "retained policy native work belongs to another session or future generation"
;;

let unique values ~matches =
  let open Result.Let_syntax in
  let%bind found =
    List.fold_result values ~init:None ~f:(fun found value ->
      if not (matches value)
      then Ok found
      else (
        match found with
        | None -> Ok (Some value)
        | Some _ -> invalid "policy native work identity is duplicated"))
  in
  Result.of_option found ~error:(P.Error.invalid_request "policy native work is absent")
;;

let event t id =
  let open Result.Let_syntax in
  let%bind row =
    unique t.events ~matches:(fun row ->
      String.equal
        id
        (P.Id.Moderator_execution.to_string row.P.Moderator_execution.context.id))
  in
  let%bind () =
    check_retained_owner
      t
      ~session_id:row.context.session_id
      ~generation:row.context.generation
  in
  let%map _ = P.Moderator_execution.of_json (P.Moderator_execution.to_json row) in
  row
;;

let invocation t id =
  let open Result.Let_syntax in
  let%bind row =
    unique t.invocations ~matches:(fun row ->
      String.equal id (P.Id.Invocation.to_string row.P.Invocation.context.id))
  in
  let%bind () =
    check_retained_owner
      t
      ~session_id:row.context.session_id
      ~generation:row.context.generation
  in
  let%map () = P.Invocation.validate row in
  row
;;

let job t id =
  let open Result.Let_syntax in
  let%bind row =
    unique t.jobs ~matches:(fun row -> String.equal id (P.Id.Job.to_string row.P.Job.id))
  in
  let%bind () =
    check_retained_owner t ~session_id:row.session_id ~generation:row.generation
  in
  let%map _ = P.Job.of_json (P.Job.to_json row) in
  row
;;

let member_of_job (job : P.Job.t) =
  match job.kind with
  | Model_call -> Member.model_job job.id ~generation:job.generation
  | Async_tool -> Member.async_job job.id ~generation:job.generation
  | Nested_agent | Scheduled_event | Shell_process | Compaction ->
    invalid "unsupported policy native Job kind"
;;

let job_member t ~job_id =
  let%bind.Result row = job t (P.Id.Job.to_string job_id) in
  member_of_job row
;;

let invocation_parent t (row : P.Invocation.t) =
  let c = row.context in
  match row.parent_event, c.parent_invocation, c.parent_job with
  | Some id, None, None ->
    Member.event id ~generation:c.generation |> Result.map ~f:Option.some
  | None, Some id, None ->
    Member.invocation id ~generation:c.generation |> Result.map ~f:Option.some
  | None, None, Some job_id ->
    let%bind.Result parent = job_member t ~job_id in
    if Int.equal (Member.generation parent) c.generation
    then Ok (Some parent)
    else invalid "policy Invocation parent generation differs"
  | None, None, None -> Ok None
  | Some _, Some _, _ | Some _, None, Some _ | None, Some _, Some _ ->
    invalid "policy native Invocation has ambiguous parents"
;;

let job_parent t (row : P.Job.t) =
  let open Result.Let_syntax in
  match row.launch with
  | None -> Ok None
  | Some launch ->
    let%bind () =
      Job_launch.validate ~invocations:t.invocations ~events:t.events ~jobs:t.jobs row
    in
    (match P.Job.Birth_parent.view launch.owner with
     | Callback (Invocation id) ->
       Member.invocation id ~generation:row.generation |> Result.map ~f:Option.some
     | Callback (Moderator_event id) ->
       Member.event id ~generation:row.generation |> Result.map ~f:Option.some
     | Job_attempt { id; _ } -> job_member t ~job_id:id |> Result.map ~f:Option.some)
;;

module Queued_job = struct
  type t =
    { id : P.Id.Job.t
    ; generation : int
    ; attempt : int
    ; source : P.Run_source.t
    ; origin : P.Inference_origin.t option
    ; actual_job : P.Job.t option
    ; retained : Retained_job_producer.t option
    }

  let id t = t.id
  let generation t = t.generation
  let attempt t = t.attempt
  let source t = t.source
  let origin t = t.origin
  let actual_job t = t.actual_job
  let retained_delivery t = Option.map t.retained ~f:Retained_job_producer.delivery
end

let queued_job t (row : P.Moderator_execution.t) =
  let open Result.Let_syntax in
  match row.queue_occurrence with
  | None -> Ok None
  | Some occurrence ->
    (match P.Moderator_execution.Queue_occurrence.producer occurrence with
     | Unknown | Captured None -> invalid "queued policy producer is unavailable"
     | Captured (Some (Job_completion { id; generation; attempt })) ->
       let source = P.Moderator_execution.Queue_occurrence.source occurrence in
       let origin = P.Moderator_execution.Queue_occurrence.origin occurrence in
       let%bind () =
         if
           Int.equal generation row.context.generation
           && generation <= t.generation
           && P.Id.Session.equal row.context.session_id t.session_id
           && P.Invocation.equal_observer row.context.source source.observer
           && Int.equal generation source.generation
         then Ok ()
         else invalid "queued Job occurrence owner or source differs"
       in
       let%bind () =
         Retained_job_producer.check_receipt_subject
           ~session_id:t.session_id
           ~id
           ~generation
           ~attempt
           ~source
           row
       in
       let actual_job = List.find t.jobs ~f:(fun job -> P.Id.Job.equal job.P.Job.id id) in
       let%bind actual_job =
         match actual_job with
         | None -> Ok None
         | Some _ -> job t (P.Id.Job.to_string id) |> Result.map ~f:Option.some
       in
       let%bind retained =
         match actual_job with
         | Some actual
           when Int.equal actual.generation generation && Int.equal actual.attempt attempt
           ->
           let%bind _ = member_of_job actual in
           let%bind () =
             match actual.status, actual.delivery with
             | (Succeeded | Failed _ | Cancelled | Interrupted _), Delivered _ -> Ok ()
             | _ -> invalid "queued policy Job is not its terminal delivered attempt"
           in
           let%bind () =
             if
               Option.equal P.Inference_origin.equal origin actual.inference_origin
               && Option.for_all
                    (Option.bind actual.launch ~f:(fun launch ->
                       launch.P.Job.moderator_source))
                    ~f:(P.Invocation.equal_observer source.observer)
             then Ok ()
             else invalid "queued policy Job origin or source differs"
           in
           Ok None
         | Some _ | None ->
           let%bind retained =
             Retained_job_producer.find
               ~session_id:t.session_id
               ~generation:row.context.generation
               ~runs:t.runs
               ~source
               ~origin
               ~id
               ~job_generation:generation
               ~attempt
           in
           let%map () =
             Option.value_map retained ~default:(Ok ()) ~f:(fun retained ->
               Retained_job_producer.check_receipt retained row)
           in
           retained
       in
       Ok
         (Some
            { Queued_job.id; generation; attempt; source; origin; actual_job; retained })
     | Captured
         (Some
            ( Event _
            | Invocation _
            | Invocation_at_source _
            | Schedule_delivery _
            | Ingress_delivery _
            | Source_setup _ )) -> Ok None)
;;

module Event_parent = struct
  type t =
    | Root
    | Native of Member.t
    | Retained_job of Queued_job.t
    | Unavailable
end

let event_ancestry t (row : P.Moderator_execution.t) =
  let open Result.Let_syntax in
  match row.context.job, row.queue_occurrence with
  | Some _, Some _ -> invalid "policy Event has contradictory native parents"
  | Some parent, None ->
    let%bind actual = job t (P.Id.Job.to_string parent.job_id) in
    let%bind () =
      if
        parent.attempt > 0
        && parent.attempt <= actual.attempt
        && Int.equal actual.generation row.context.generation
      then Ok ()
      else invalid "policy Event parent Job attempt differs"
    in
    let%map parent = member_of_job actual in
    Event_parent.Native parent
  | None, None -> Ok Event_parent.Root
  | None, Some occurrence ->
    let source = P.Moderator_execution.Queue_occurrence.source occurrence in
    let origin = P.Moderator_execution.Queue_occurrence.origin occurrence in
    let%bind () =
      if
        Int.equal source.generation row.context.generation
        && P.Invocation.equal_observer source.observer row.context.source
      then Ok ()
      else invalid "policy queue occurrence source differs from its receipt"
    in
    (match P.Moderator_execution.Queue_occurrence.producer occurrence with
     | Unknown | Captured None -> Ok Event_parent.Unavailable
     | Captured (Some (Job_completion _)) ->
       let%bind selected = queued_job t row in
       let%bind selected =
         Result.of_option
           selected
           ~error:(P.Error.invalid_request "queued Job producer is absent")
       in
       (match selected.retained, selected.actual_job with
        | None, Some actual
          when Int.equal actual.generation selected.generation
               && Int.equal actual.attempt selected.attempt ->
          let%map member = member_of_job actual in
          Event_parent.Native member
        | Some _, _ -> Ok (Event_parent.Retained_job selected)
        | None, (None | Some _) -> Ok Event_parent.Unavailable)
     | Captured (Some (Event id)) ->
       let%bind actual = event t (P.Id.Moderator_execution.to_string id) in
       let%bind () =
         match actual.status with
         | Completed _
           when Int.equal actual.context.generation row.context.generation
                && P.Invocation.equal_observer actual.context.source source.observer ->
           Ok ()
         | _ -> invalid "queued Event producer is not the actual completed source"
       in
       let%map member = Member.event id ~generation:actual.context.generation in
       Event_parent.Native member
     | Captured (Some (Invocation id | Invocation_at_source { id; source = _ })) as
       captured ->
       let%bind actual = invocation t (P.Id.Invocation.to_string id) in
       let%bind () =
         match actual.status with
         | Resolved _ | Published _ -> Ok ()
         | Admitted | Dispatching -> invalid "queued Invocation producer is unresolved"
       in
       let%bind () =
         if
           Int.equal actual.context.generation row.context.generation
           && Option.equal P.Inference_origin.equal origin actual.inference_origin
         then Ok ()
         else invalid "queued Invocation producer generation or original differs"
       in
       let%bind () =
         match captured, actual.observation with
         | Captured (Some (Invocation _)), Some observed
           when P.Invocation.equal_observer observed.observer source.observer -> Ok ()
         | Captured (Some (Invocation_at_source { source = captured_source; _ })), None
           when P.Run_source.equal captured_source source -> Ok ()
         | _ -> invalid "queued Invocation producer source capture differs"
       in
       let%map member = Member.invocation id ~generation:actual.context.generation in
       Event_parent.Native member
     | Captured (Some (Schedule_delivery _ | Ingress_delivery _ | Source_setup _)) ->
       (* Exact external-family validation belongs to the State/queue owner.
          This is only a policy-ancestry root, never no-Goal or admission proof. *)
       Ok Event_parent.Root)
;;

let event_parent t row =
  let%bind.Result parent = event_ancestry t row in
  match parent with
  | Event_parent.Root -> Ok None
  | Native member -> Ok (Some member)
  | Retained_job _ ->
    invalid "retained queued Job ancestry requires its canonical typed membership"
  | Unavailable -> invalid "queued policy producer history is unavailable"
;;
