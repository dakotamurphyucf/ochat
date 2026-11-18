open! Core
module P = Agent_protocol

type t = Run_job_delivery.t

let invalid message = Error (P.Error.invalid_request message)
let delivery t = t
let frame = Run_job_delivery.frame

let find ~session_id ~generation ~runs ~source ~origin ~id ~job_generation ~attempt =
  let open Result.Let_syntax in
  let selected =
    match origin, runs with
    | Some (P.Inference_origin.Run run_id), Some index ->
      List.find (Run_state.job_deliveries index) ~f:(fun delivery ->
        let key = Run_job_delivery.key delivery in
        P.Id.Run.equal key.run_id run_id
        && P.Id.Job.equal key.job_id id
        && Int.equal key.generation job_generation
        && Int.equal key.attempt attempt)
    | (None | Some Unbound), _ | Some (Run _), None -> None
  in
  match selected with
  | None -> Ok None
  | Some delivery ->
    let frame = Run_job_delivery.frame delivery in
    let%bind () =
      if
        P.Id.Session.equal session_id frame.session_id
        && Int.equal generation job_generation
        && Int.equal frame.generation job_generation
      then Ok ()
      else invalid "queued producer session or generation differs"
    in
    let%bind () =
      if P.Run_source.equal source (Run_job_delivery.source delivery)
      then Ok ()
      else invalid "retained Job producer changed installation"
    in
    let%map () =
      match Run_job_delivery.disposition delivery with
      | Enqueued _ | Claimed _ | Retired { enqueued_at = Some _; _ } -> Ok ()
      | Pending | Retired { enqueued_at = None; _ } ->
        invalid "retained Job producer was never enqueued"
    in
    Some delivery
;;

let capture ~session_id ~generation ~runs ~source ~origin ~id ~job_generation ~attempt =
  let open Result.Let_syntax in
  let%bind () =
    match origin with
    | Some (P.Inference_origin.Run _) -> Ok ()
    | None | Some Unbound ->
      invalid "retained Job producer requires its exact original Run"
  in
  let%bind selected =
    find ~session_id ~generation ~runs ~source ~origin ~id ~job_generation ~attempt
  in
  Result.of_option
    selected
    ~error:(P.Error.invalid_request "queued Job completion has no exact retained attempt")
;;

let check_payload t ~sha256 =
  let open Result.Let_syntax in
  let%bind payload =
    Chat_response.Background_delivery.capture (frame t)
    |> Session.Snapshot.of_value
    |> Result.map_error ~f:P.Error.invalid_request
  in
  let%bind hashes = Queue_payload.digests [ payload ] in
  if List.equal String.equal hashes [ sha256 ]
  then Ok ()
  else invalid "retained Job producer payload differs from terminal evidence"
;;

let check_receipt t receipt =
  let open Result.Let_syntax in
  let%bind payload = Queue_payload.receipt_snapshot receipt in
  let%bind payload =
    Result.of_option
      payload
      ~error:(P.Error.invalid_request "queued Job receipt has no exact captured snapshot")
  in
  let%bind hashes = Queue_payload.digests [ payload ] in
  match hashes with
  | [ sha256 ] -> check_payload t ~sha256
  | [] | _ :: _ :: _ -> invalid "queued Job receipt payload count differs"
;;

let check_receipt_subject ~session_id ~id ~generation ~attempt ~source receipt =
  let open Result.Let_syntax in
  let%bind payload = Queue_payload.receipt_snapshot receipt in
  match payload with
  | None -> invalid "queued Job receipt has no exact captured snapshot"
  | Some payload ->
    let%bind value =
      Session.Snapshot.to_value payload |> Result.map_error ~f:P.Error.invalid_request
    in
    let%bind frame =
      Chat_response.Background_delivery.decode value
      |> Result.map_error ~f:P.Error.invalid_request
    in
    (match frame with
     | None -> Ok ()
     | Some frame ->
       if
         P.Id.Session.equal frame.session_id session_id
         && P.Id.Job.equal frame.job_id id
         && Int.equal frame.generation generation
         && Int.equal frame.attempt attempt
         && P.Invocation.equal_observer frame.source source.P.Run_source.observer
       then Ok ()
       else invalid "queued Job producer differs from its actual captured frame")
;;
