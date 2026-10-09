open! Core
module P = Agent_protocol

type t =
  { connection : Connection.t
  ; server_id : P.Id.Server.t
  }

let create connection ~server_id = { connection; server_id }
let invalid message = Error (P.Error.invalid_request message)

let check_host t server_id =
  if not (P.Id.Server.equal t.server_id server_id)
  then invalid "activity request belongs to another host"
  else (
    match Connection.initialization t.connection with
    | Some initialized when P.Id.Server.equal initialized.server_id t.server_id -> Ok ()
    | Some _ -> invalid "activity connection is initialized against another host"
    | None -> invalid "activity requires a successfully initialized connection")
;;

let list_page t (request : P.Activity_query.t) =
  let open Result.Let_syntax in
  let%bind () = check_host t request.server_id in
  match Connection.request_without_history t.connection (Activity_list request) with
  | Ok (Activity_list page) -> Ok page
  | Ok _ -> invalid "unexpected activity.list response"
  | Error _ as error -> error
;;

let work_page t (request : P.Session_work.Query.t) =
  let open Result.Let_syntax in
  let%bind () = check_host t (P.Session_ref.server_id request.session) in
  match Connection.request_without_history t.connection (Session_work request) with
  | Ok (Session_work page) -> Ok page
  | Ok _ -> invalid "unexpected session.work response"
  | Error _ as error -> error
;;

let cancel_job t (row : P.Session_work.t) ~attachment_id ~idempotency_key =
  let open Result.Let_syntax in
  let%bind () = check_host t (P.Session_ref.server_id row.session) in
  let%bind job_id, attempt =
    match row.key with
    | Job { id; attempt } -> Ok (id, attempt)
    | Schedule _ | Invocation _ | Subscription _ | Delivery _ | Moderator_execution _ ->
      invalid "work row is not a job"
  in
  let request =
    P.Job.Cancel_request.
      { session_id = P.Session_ref.session_id row.session
      ; attachment_id
      ; job_id
      ; expected_generation = Some row.generation
      ; expected_attempt = Some attempt
      ; idempotency_key
      }
  in
  match Connection.request_without_history t.connection (Job_cancel request) with
  | Ok (Job_cancel result) -> Ok result
  | Ok _ -> invalid "unexpected job.cancel response"
  | Error _ as error -> error
;;

let cancel_schedule t (row : P.Session_work.t) ~attachment_id ~idempotency_key =
  let open Result.Let_syntax in
  let%bind () = check_host t (P.Session_ref.server_id row.session) in
  let%bind schedule_id =
    match row.key with
    | Schedule id -> Ok id
    | Job _ | Invocation _ | Subscription _ | Delivery _ | Moderator_execution _ ->
      invalid "work row is not a schedule"
  in
  let request =
    P.Schedule.Cancel_request.
      { session_id = P.Session_ref.session_id row.session
      ; attachment_id
      ; schedule_id
      ; expected_generation = Some row.generation
      ; idempotency_key
      }
  in
  match Connection.request_without_history t.connection (Schedule_cancel request) with
  | Ok (Schedule_cancel result) -> Ok result
  | Ok _ -> invalid "unexpected schedule.cancel response"
  | Error _ as error -> error
;;
