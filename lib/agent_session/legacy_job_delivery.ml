open! Core
module P = Agent_protocol
module S = Session.Moderator_state.Identity_snapshot

type t =
  { index : Run_state.t
  ; source : P.Run_source.t
  ; plan : Legacy_queue_checkpoint.t
  ; checkpoint : Jsonaf.t
  }

let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())
let index t = t.index
let source t = t.source
let metadata t = Legacy_queue_checkpoint.metadata t.plan
let entry t = List.hd_exn (Legacy_queue_checkpoint.appended t.plan)
let checkpoint t = t.checkpoint
let same_snapshot left right = Sexp.equal (S.sexp_of_t left) (S.sexp_of_t right)

let payload state (job : P.Job.t) ~source =
  let open Result.Let_syntax in
  let%bind value =
    match job.kind with
    | Model_call -> Runtime_builder.model_job_event job
    | Async_tool ->
      let%map frame =
        Background_job_event.frame ~state ~observer:source.P.Run_source.observer job
      in
      Chat_response.Background_delivery.capture frame
    | Nested_agent | Scheduled_event | Shell_process | Compaction ->
      conflict "legacy Job delivery requires a Model or Async completion"
  in
  Session.Snapshot.of_value value |> Result.map_error ~f:P.Error.invalid_request
;;

let prepare (state : Session_state.t) ~job ~(before : S.t) ~snapshot ~capture =
  let open Result.Let_syntax in
  let%bind () = Moderator_checkpoint.validate_encoded_capacity state.moderator in
  let%bind installed = Moderator_checkpoint.decode state.moderator in
  let%bind () =
    if Option.exists installed ~f:(fun installed -> same_snapshot installed before)
    then Ok ()
    else conflict "legacy Job checkpoint changed before delivery"
  in
  let%bind () =
    match job.P.Job.status, job.delivery with
    | (Succeeded | Failed _ | Cancelled | Interrupted _), Pending -> Ok ()
    | (Queued | Running | Waiting_permission _ | Waiting_completion _), _
    | _, (Not_required | Delivered _ | Discarded _) ->
      conflict "legacy Job delivery requires a terminal Pending record"
  in
  let%bind snapshot = Moderator_checkpoint.decode snapshot in
  let%bind snapshot =
    Result.of_option
      snapshot
      ~error:
        (P.Error.invalid_request
           "legacy Job delivery requires its prospective checkpoint")
  in
  let%bind index, source = Moderator_source_installation.capture state in
  let%bind payload = payload state job ~source in
  let expected =
    { before with queued_internal_events = before.queued_internal_events @ [ payload ] }
  in
  let%bind () =
    if same_snapshot expected snapshot
    then Ok ()
    else conflict "legacy Job delivery must append its exact terminal payload only"
  in
  let%bind metadata = Moderator_checkpoint.queue state.moderator in
  let%bind metadata =
    match metadata with
    | None -> Moderator_queue.bootstrap ~source ~queue:before.queued_internal_events
    | Some metadata ->
      if P.Run_source.equal source (Moderator_queue.source metadata)
      then Ok metadata
      else conflict "legacy Job queue lifetime changed"
  in
  let%bind plan =
    Legacy_queue_checkpoint.append_delivery ~metadata ~before ~snapshot ~capture
  in
  let%map checkpoint =
    Moderator_checkpoint.update_with_queue
      state.moderator
      ~snapshot
      ~queue:(Legacy_queue_checkpoint.metadata plan)
  in
  { index; source; plan; checkpoint }
;;
