open! Core
module P = Agent_protocol
module Q = P.Goal_query
module W = P.Goal_control

type t =
  { connection : Connection.t
  ; session : P.Session_ref.t
  ; attachment : (unit -> P.Id.Attachment.t) option
  }

let create connection ~session ?attachment () = { connection; session; attachment }

let check_host t =
  match Connection.initialization t.connection with
  | None ->
    Error (P.Error.invalid_request "Goal operations require an initialized connection")
  | Some initialized ->
    if P.Id.Server.equal initialized.server_id (P.Session_ref.server_id t.session)
    then Ok ()
    else Error (P.Error.invalid_request "Goal operation belongs to another host")
;;

let matches_query t result = P.Session_ref.equal t.session (Q.Result.session result)

let query t target =
  let%bind.Result () = check_host t in
  let attachment_id = Option.map t.attachment ~f:(fun attachment -> attachment ()) in
  Connection.request_without_history
    t.connection
    (P.Command.Goal_query { session = t.session; attachment_id; target })
;;

let list t =
  match query t All with
  | Ok (P.Method_result.Goal_query (List outcome)) when matches_query t outcome ->
    Ok outcome
  | Ok _ -> Error (P.Error.invalid_request "unexpected session.goals response")
  | Error _ as error -> error
;;

let get t id =
  let matches_goal (outcome : Q.Result.outcome) =
    match outcome with
    | Unavailable _ -> true
    | Available view ->
      (match view.goals with
       | [] -> true
       | [ row ] -> P.Id.Goal.equal row.goal.id id
       | _ :: _ :: _ -> false)
  in
  match query t (One id) with
  | Ok (P.Method_result.Goal_query (Get outcome))
    when matches_query t outcome && matches_goal outcome -> Ok outcome
  | Ok _ -> Error (P.Error.invalid_request "unexpected session.goal response")
  | Error _ as error -> error
;;

let control t create intent =
  let%bind.Result () = check_host t in
  let%bind.Result attachment_id =
    match t.attachment with
    | Some attachment -> Ok (attachment ())
    | None -> Error (P.Error.invalid_request "Goal mutation requires a writer attachment")
  in
  let%bind.Result request = create ~session:t.session ~attachment_id intent in
  Connection.request_without_history t.connection (P.Command.Goal_control request)
;;

let matches_principal t principal_id =
  Option.exists (Connection.initialization t.connection) ~f:(fun initialized ->
    P.Id.Principal.equal initialized.principal.id principal_id)
;;

let matches_receipt t ~goal_id ~key ~digest (receipt : P.Goal_command_receipt.t) =
  P.Id.Goal.equal receipt.goal_id goal_id
  && P.Idempotency_key.equal receipt.key key
  && String.equal receipt.request_sha256 digest
  && matches_principal t receipt.principal_id
;;

let command t (intent : P.Goal_command.t) =
  let%bind.Result digest = P.Goal_command.digest intent in
  let goal_id = P.Goal_command.Mutation.goal_id intent.mutation in
  match control t W.Request.command intent with
  | Ok (P.Method_result.Goal_control (Command receipt))
    when matches_receipt t ~goal_id ~key:intent.key ~digest receipt -> Ok receipt
  | Ok _ -> Error (P.Error.invalid_request "unexpected session.goal.command response")
  | Error _ as error -> error
;;

let start t (intent : W.Start.t) =
  let%bind.Result request_sha256 =
    W.Start.run_request_sha256 intent ~session_id:(P.Session_ref.session_id t.session)
  in
  let association_sha256 =
    W.Start.association_sha256
      ~goal_id:intent.goal_id
      ~expected_goal_revision:intent.expected_goal_revision
      ~expected_catalog_revision:intent.expected_catalog_revision
      ~expected_source:(Some intent.source)
  in
  let matches_admission (admission : W.Start_result.Admission.t) =
    let receipt = admission.receipt in
    P.Idempotency_key.equal receipt.key intent.key
    && P.Run_source.equal receipt.source intent.source
    && String.equal receipt.request_sha256 request_sha256
    && String.equal admission.association_sha256 association_sha256
    && matches_principal t receipt.principal_id
  in
  let matches = function
    | W.Start_result.Activated admission
    | Retained admission
    | Settled { admission; _ }
    | Activation_failed { admission; _ } -> matches_admission admission
    | Controlled { admission; control } ->
      matches_admission admission && P.Id.Goal.equal control.goal_id intent.goal_id
    | Admission_uncertain _ -> true
  in
  match control t W.Request.start intent with
  | Ok (P.Method_result.Goal_control (Start result)) when matches result -> Ok result
  | Ok _ -> Error (P.Error.invalid_request "unexpected session.goal.start response")
  | Error _ as error -> error
;;

let resume t intent =
  let%bind.Result digest = P.Goal_resume_request.digest intent in
  let goal_id = P.Goal_resume_request.goal_id intent in
  let key = P.Goal_resume_request.key intent in
  let run_id = P.Goal_resume_request.run_id intent in
  let matches_continuation = function
    | W.Resume_result.Started { generation; _ } ->
      Int.equal generation (P.Goal_resume_request.generation intent)
    | Waiting -> true
    | Settled { run_id = actual_run; _ } -> P.Id.Run.equal actual_run run_id
    | Controlled control -> P.Id.Goal.equal control.goal_id goal_id
  in
  let matches = function
    | W.Resume_result.Resumed { receipt; continuation } ->
      matches_receipt t ~goal_id ~key ~digest receipt && matches_continuation continuation
    | Retained receipt | Resume_failed { receipt; _ } ->
      matches_receipt t ~goal_id ~key ~digest receipt
    | Rejected _ | Uncertain _ -> true
  in
  match control t W.Request.resume intent with
  | Ok (P.Method_result.Goal_control (Resume result)) when matches result -> Ok result
  | Ok _ -> Error (P.Error.invalid_request "unexpected session.goal.resume response")
  | Error _ as error -> error
;;
