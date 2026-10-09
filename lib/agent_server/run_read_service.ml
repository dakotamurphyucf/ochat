open! Core
module P = Agent_protocol
module A = Agent_session

type t =
  { principal : P.Principal.t
  ; server_id : P.Id.Server.t
  ; read : P.Id.Session.t -> (A.Session_state.t, P.Error.t) result
  ; pagination : Pagination.t
  }

let denied () =
  Error
    (P.Error.create
       Permission_denied
       ~message:"run inspection requires current transcript and security visibility"
       ~retryable:false
       ())
;;

let create principal ~server_id ~read ~pagination =
  if
    P.Principal.has_scope principal View_session_transcript
    && P.Principal.has_scope principal View_security_state
  then Ok { principal; server_id; read; pagination }
  else denied ()
;;

let visible t (run : P.Run.t) =
  P.Id.Principal.equal run.principal_id t.principal.id
  || P.Principal.has_scope t.principal Administer_configuration
;;

let read t reference =
  let open Result.Let_syntax in
  if not (P.Id.Server.equal (P.Session_ref.server_id reference) t.server_id)
  then Error (P.Error.invalid_request "run reference belongs to another host")
  else (
    let session_id = P.Session_ref.session_id reference in
    let%bind state = t.read session_id in
    if
      P.Id.Session.equal state.A.Session_state.identity.session_id session_id
      && Authorization.session_visible_to t.principal (A.Session_state.summary state)
    then Ok state
    else denied ())
;;

let index state = Option.value state.A.Session_state.run_state ~default:A.Run_state.empty

let result_references state index (run : P.Run.t) =
  let open Result.Let_syntax in
  let owned_job id generation attempt =
    List.exists run.terminal_work ~f:(fun proof ->
      Int.equal proof.P.Run_work.Terminal.work.generation generation
      && P.Run_work.Key.equal proof.work.key (Retained (Job { id; attempt })))
  in
  let%bind current =
    state.A.Session_state.jobs
    |> List.filter ~f:(fun job -> owned_job job.P.Job.id job.generation job.attempt)
    |> List.fold_result ~init:[] ~f:(fun references job ->
      let%bind result = P.Job.terminal_result job in
      match result with
      | None -> Ok references
      | Some _ ->
        let%map reference = P.Run_result_reference.of_job job in
        reference :: references)
  in
  let%bind retained =
    A.Run_state.job_deliveries index
    |> List.filter ~f:(fun carrier ->
      P.Id.Run.equal (A.Run_job_delivery.run_id carrier) run.id
      && P.Run_source.equal (A.Run_job_delivery.source carrier) run.source)
    |> List.map ~f:(fun carrier ->
      let frame = A.Run_job_delivery.frame carrier in
      let%bind reference =
        P.Job_result_reference.of_completion
          frame.result
          ~session_id:frame.session_id
          ~job_id:frame.job_id
          ~generation:frame.generation
          ~attempt:frame.attempt
      in
      P.Run_result_reference.of_job_result reference)
    |> Result.all
  in
  let completed =
    match run.lifecycle with
    | Terminal (Completed (Some reference)) -> [ reference ]
    | Admitted | Active | Waiting _
    | Terminal (Completed None | Failed _ | Cancelled | Limited | Interrupted) -> []
  in
  Ok
    (List.fold
       (completed @ retained @ current)
       ~init:[]
       ~f:(fun references reference ->
         if List.exists references ~f:(P.Run_result_reference.equal reference)
         then references
         else reference :: references)
     |> List.rev)
;;

let view t reference state index (run : P.Run.t) =
  let open Result.Let_syntax in
  let%bind () =
    if
      P.Session_ref.equal run.session reference
      && P.Id.Server.equal (P.Session_ref.server_id run.session) t.server_id
    then Ok ()
    else Error (P.Error.invalid_request "retained run belongs to another session host")
  in
  let receipts =
    List.filter (A.Run_state.receipts index) ~f:(fun receipt ->
      P.Id.Run.equal receipt.P.Run_receipt.run_id run.id)
  in
  let receipt kind =
    List.find receipts ~f:(fun receipt -> P.Run_receipt.Kind.equal receipt.kind kind)
  in
  let%bind pending_action =
    List.fold (A.Run_state.intents index) ~init:(Ok None) ~f:(fun accumulated intent ->
      let%bind previous = accumulated in
      if
        P.Id.Run.equal intent.A.Run_intent.receipt.run_id run.id
        && A.Run_intent.Disposition.equal intent.disposition Pending
      then P.Run_action.combine previous (Some intent.action)
      else Ok previous)
  in
  let%bind result_references = result_references state index run in
  P.Run_query.View.create
    ~run
    ~result_references
    ~pending_action
    ~admission_receipt:(receipt Admission)
    ~terminal_receipt:(receipt Terminal)
    ~session_revision:state.counters.revision
    ~event_sequence:state.counters.event_sequence
;;

let list t (request : P.Run_query.Request.t) =
  let open Result.Let_syntax in
  let%bind state = read t request.session in
  let index = index state in
  let%bind values =
    List.filter (A.Run_state.runs index) ~f:(visible t)
    |> List.map ~f:(view t request.session state index)
    |> Result.all
  in
  Pagination.runs t.pagination t.principal request values
;;

let lookup t (request : P.Run_query.Lookup_request.t) =
  let open Result.Let_syntax in
  let%bind state = read t request.session in
  let index = index state in
  match A.Run_state.find index request.run_id with
  | None -> Ok (P.Run_query.Outcome.unavailable request.run_id)
  | Some run ->
    if visible t run
    then
      Result.map (view t request.session state index run) ~f:P.Run_query.Outcome.available
    else Ok (P.Run_query.Outcome.unavailable request.run_id)
;;
