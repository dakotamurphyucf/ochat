open! Core
module P = Agent_protocol
module S = Session.Moderator_state.Identity_snapshot

type t =
  { metadata : Moderator_queue.t
  ; appended : Moderator_queue.Entry.t list
  }

let metadata t = t.metadata
let appended t = t.appended
let invalid message = Error (P.Error.invalid_request message)

let append_delivery ~metadata ~(before : S.t) ~(snapshot : S.t) ~capture =
  let open Result.Let_syntax in
  let%bind () =
    let expected =
      { before with queued_internal_events = snapshot.queued_internal_events }
    in
    if
      List.length snapshot.queued_internal_events
      = List.length before.queued_internal_events + 1
      && Sexp.equal (S.sexp_of_t expected) (S.sexp_of_t snapshot)
    then Ok ()
    else invalid "legacy delivery must append exactly one value without checkpoint edits"
  in
  let%map metadata =
    Moderator_queue.advance
      metadata
      ~before:before.queued_internal_events
      ~after:snapshot.queued_internal_events
      ~consume:Keep
      ~appended:[ capture ]
  in
  { metadata; appended = [ List.last_exn (Moderator_queue.entries metadata) ] }
;;

let prepare
      ~metadata
      ~(before : S.t)
      ~(snapshot : S.t)
      ~(consumption : Moderator_queue.Consumption.t)
      ~(completed : P.Moderator_execution.t)
      ~origin
  =
  let open Result.Let_syntax in
  let%bind () = P.Moderator_execution.validate completed in
  let%bind () =
    match completed.status with
    | Completed _ -> Ok ()
    | Running | Failed _ | Interrupted _ ->
      invalid "legacy emits require a completed Event"
  in
  let source = Moderator_queue.source metadata in
  let%bind () =
    if
      P.Invocation.equal_observer source.observer completed.context.source
      && Int.equal source.generation completed.context.generation
      && String.equal source.observer.script_id before.script_id
      && String.equal source.observer.source_sha256 before.script_source_hash
      && String.equal before.script_id snapshot.script_id
      && String.equal before.script_source_hash snapshot.script_source_hash
    then Ok ()
    else invalid "legacy callback changed its captured source"
  in
  let retained =
    List.length before.queued_internal_events
    -
    match consumption with
    | Keep -> 0
    | Consume_head -> 1
  in
  let fresh = List.length snapshot.queued_internal_events - retained in
  let%bind () =
    if fresh < 0 || fresh > 256
    then invalid "legacy callback fresh suffix limit"
    else Ok ()
  in
  let%bind capture =
    Moderator_queue.Capture.create ~producer:(Event completed.context.id) ~origin
  in
  let%map metadata =
    Moderator_queue.advance
      metadata
      ~before:before.queued_internal_events
      ~after:snapshot.queued_internal_events
      ~consume:consumption
      ~appended:(List.init fresh ~f:(fun _ -> capture))
  in
  { metadata; appended = List.drop (Moderator_queue.entries metadata) retained }
;;
