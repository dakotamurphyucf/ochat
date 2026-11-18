open! Core
module P = Agent_protocol
module S = Session.Moderator_state.Identity_snapshot

module Producer = struct
  type t =
    | Job_completion of P.Job.t
    | Run_job_completion of Run_job_delivery.t
    | Owned_schedule of P.Schedule.t
    | Ingress of
        { registration : External_ingress.t
        ; receipt : External_ingress.receipt
        }
end

module Snapshot = struct
  type t =
    | Known of S.t
    | Encoded of Jsonaf.t
end

type t =
  { checkpoint : Jsonaf.t
  ; source : P.Run_source.t
  ; plan : Legacy_queue_checkpoint.t
  ; initial_source_index : Run_state.t option
  }

let checkpoint t = t.checkpoint
let source t = t.source
let metadata t = Legacy_queue_checkpoint.metadata t.plan
let entry t = List.hd_exn (Legacy_queue_checkpoint.appended t.plan)
let initial_source_index t = t.initial_source_index
let same_snapshot left right = Sexp.equal (S.sexp_of_t left) (S.sexp_of_t right)
let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())

let capture_job (job : P.Job.t) ~origin =
  Moderator_queue.Capture.create
    ~producer:
      (Job_completion { id = job.id; generation = job.generation; attempt = job.attempt })
    ~origin
;;

let producer_value (state : Session_state.t) ~model_completion_format ~source = function
  | Producer.Job_completion job ->
    let open Result.Let_syntax in
    let%bind () =
      Extension_invariants.owner
        ~session_id:state.identity.session_id
        ~generation:state.identity.generation
        job.session_id
        job.generation
    in
    let%bind () =
      match job.status, job.delivery with
      | (Succeeded | Failed _ | Cancelled | Interrupted _), Pending -> Ok ()
      | (Queued | Running | Waiting_permission _ | Waiting_completion _), _
      | _, (Not_required | Delivered _ | Discarded _) ->
        conflict "external Job delivery requires its terminal Pending record"
    in
    let%bind value =
      match job.kind with
      | Model_call -> Model_job_event.render ~format:model_completion_format job
      | Async_tool ->
        let%map frame =
          Background_job_event.frame ~state ~observer:source.P.Run_source.observer job
        in
        Chat_response.Background_delivery.capture frame
      | Nested_agent | Scheduled_event | Shell_process | Compaction ->
        conflict "external Job delivery requires a Model or Async completion"
    in
    let%map capture = capture_job job ~origin:job.inference_origin in
    value, capture
  | Run_job_completion delivery ->
    let open Result.Let_syntax in
    let frame = Run_job_delivery.frame delivery in
    let%bind () =
      match Run_job_delivery.disposition delivery with
      | Pending when P.Run_source.equal source (Run_job_delivery.source delivery) -> Ok ()
      | Pending | Enqueued _ | Claimed _ | Retired _ ->
        conflict "external Run delivery is not pending under this installation"
    in
    let%map capture =
      Moderator_queue.Capture.create
        ~producer:
          (Job_completion
             { id = frame.job_id; generation = frame.generation; attempt = frame.attempt })
        ~origin:(Some (Run (Run_job_delivery.run_id delivery)))
    in
    Chat_response.Background_delivery.capture frame, capture
  | Owned_schedule schedule ->
    let open Result.Let_syntax in
    let%bind () =
      Extension_invariants.owner
        ~session_id:state.identity.session_id
        ~generation:state.identity.generation
        schedule.session_id
        schedule.generation
    in
    let%bind () =
      match schedule.status, schedule.ownership with
      | Delivering, Some ownership
        when P.Invocation.equal_observer source.observer ownership.source -> Ok ()
      | (Scheduled | Delivering | Delivered | Cancelled | Failed _), _ ->
        conflict "external timer delivery requires its claimed owned source"
    in
    let%bind () =
      if Int.equal schedule.delivery_count Int.max_value
      then conflict "external timer delivery count overflow"
      else Ok ()
    in
    let%bind value =
      Chat_response.Schedule_delivery.capture schedule
      |> Result.map_error ~f:P.Error.invalid_request
    in
    let%map capture =
      match schedule.inference_provenance with
      | None -> Ok Moderator_queue.Capture.unavailable
      | Some provenance ->
        if P.Run_source.equal source (P.Schedule_provenance.source provenance)
        then
          Moderator_queue.Capture.create
            ~producer:
              (Schedule_delivery
                 { id = schedule.id
                 ; generation = schedule.generation
                 ; delivery_count = schedule.delivery_count + 1
                 })
            ~origin:(P.Schedule_provenance.origin provenance)
        else conflict "external timer original source differs from installation"
    in
    value, capture
  | Ingress { registration; receipt } ->
    let open Result.Let_syntax in
    let%bind () =
      Extension_invariants.owner
        ~session_id:state.identity.session_id
        ~generation:state.identity.generation
        registration.context.session_id
        registration.context.generation
    in
    let%bind () =
      if P.Invocation.equal_observer source.observer registration.context.source
      then Ok ()
      else conflict "external ingress source differs from installation"
    in
    let%bind frame = External_ingress.delivery_frame registration receipt in
    let%map capture =
      match registration.inference_provenance with
      | None -> Ok Moderator_queue.Capture.unavailable
      | Some provenance ->
        if P.Run_source.equal source (External_ingress.Provenance.source provenance)
        then
          Moderator_queue.Capture.create
            ~producer:
              (Ingress_delivery
                 { registration_id = registration.context.id; event_id = receipt.id })
            ~origin:(External_ingress.Provenance.origin provenance)
        else conflict "external ingress original source differs from installation"
    in
    Chat_response.Ingress_delivery.capture frame, capture
;;

let prospective = function
  | Snapshot.Known snapshot -> Ok (snapshot, None)
  | Encoded raw ->
    let open Result.Let_syntax in
    let%bind () = Moderator_checkpoint.validate_encoded_capacity (Some raw) in
    let%bind decoded = Moderator_checkpoint.decode (Some raw) in
    let%map snapshot =
      Result.of_option
        decoded
        ~error:
          (P.Error.invalid_request "external delivery requires a prospective checkpoint")
    in
    ( snapshot
    , Option.some_if
        (not (Jsonaf.exactly_equal raw (Moderator_checkpoint.encode snapshot)))
        raw )
;;

let prepare
      ?(model_completion_format = Model_job_event.Format.Legacy)
      (state : Session_state.t)
      ~producer
      ~(before : S.t)
      ~snapshot
  =
  let open Result.Let_syntax in
  let%bind () = Moderator_checkpoint.validate_encoded_capacity state.moderator in
  let%bind installed = Moderator_checkpoint.decode state.moderator in
  let%bind () =
    if Option.exists installed ~f:(fun installed -> same_snapshot installed before)
    then Ok ()
    else conflict "moderator checkpoint changed before external delivery"
  in
  let%bind snapshot, supplied = prospective snapshot in
  let%bind index, source = Moderator_source_installation.capture state in
  let%bind value, capture =
    producer_value state ~model_completion_format ~source producer
  in
  let%bind value =
    Session.Snapshot.of_value value |> Result.map_error ~f:P.Error.invalid_request
  in
  let expected =
    { before with queued_internal_events = before.queued_internal_events @ [ value ] }
  in
  let%bind () =
    if same_snapshot expected snapshot
    then Ok ()
    else conflict "external delivery must append its exact retained payload only"
  in
  let%bind metadata = Moderator_checkpoint.queue state.moderator in
  let%bind metadata =
    match metadata with
    | None -> Moderator_queue.bootstrap ~source ~queue:before.queued_internal_events
    | Some metadata ->
      if P.Run_source.equal source (Moderator_queue.source metadata)
      then Ok metadata
      else conflict "external delivery queue installation changed"
  in
  let%bind plan =
    Legacy_queue_checkpoint.append_delivery ~metadata ~before ~snapshot ~capture
  in
  let metadata = Legacy_queue_checkpoint.metadata plan in
  let%bind checkpoint =
    match supplied with
    | None ->
      Moderator_checkpoint.update_with_queue state.moderator ~snapshot ~queue:metadata
    | Some raw ->
      let%bind supplied = Moderator_checkpoint.queue (Some raw) in
      let%bind () =
        if
          Option.exists supplied ~f:(fun supplied ->
            Jsonaf.exactly_equal
              (Moderator_queue.to_jsonaf supplied)
              (Moderator_queue.to_jsonaf metadata))
        then Ok ()
        else conflict "supplied external delivery metadata differs from exact append"
      in
      let%map () =
        Moderator_checkpoint.validate_update ~previous:state.moderator (Some raw)
      in
      raw
  in
  let initial_source_index =
    if Option.exists state.run_state ~f:(Run_state.equal index) then None else Some index
  in
  Ok { checkpoint; source; plan; initial_source_index }
;;
