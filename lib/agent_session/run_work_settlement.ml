open! Core
module P = Agent_protocol

let conflict message = P.Error.create Conflict ~message ~retryable:false ()

let prepare
      index
      ~(state : Session_state.t)
      ~(previous : P.Run.t)
      ~owned_work
      ~terminal_work
      ~operation_id
      ~now
  =
  let open Result.Let_syntax in
  let%bind () =
    match Run_state.find index previous.id, previous.lifecycle with
    | Some retained, (Admitted | Active | Waiting _) when P.Run.equal retained previous ->
      Ok ()
    | Some _, _ | None, _ ->
      Error (conflict "work settlement requires the exact retained live Run")
  in
  let%bind () =
    if
      Int64.equal previous.revision Int64.max_value
      || Int64.equal state.counters.revision Int64.max_value
    then Error (conflict "revision exhausted at work settlement")
    else Ok ()
  in
  let pending =
    List.filter (Run_state.intents index) ~f:(fun intent ->
      P.Id.Run.equal intent.Run_intent.receipt.run_id previous.id
      && Run_intent.Disposition.equal intent.disposition Pending)
  in
  let requested =
    List.find_map pending ~f:(fun intent ->
      match intent.action with
      | Finish { terminal; _ } -> Some terminal
      | Continue | Wait _ -> None)
  in
  let settled = List.length terminal_work = List.length owned_work in
  let outcome =
    List.fold
      terminal_work
      ~init:P.Run_work.Terminal.Succeeded
      ~f:(fun accumulated proof ->
        match proof.P.Run_work.Terminal.outcome, accumulated with
        | Failed, _ | _, Failed -> P.Run_work.Terminal.Failed
        | Cancelled, _ | _, Cancelled -> Cancelled
        | Limited, _ | _, Limited -> Limited
        | Interrupted, _ | _, Interrupted -> Interrupted
        | Unconfirmed, _ | _, Unconfirmed -> Unconfirmed
        | Succeeded, Succeeded -> Succeeded)
  in
  let awaiting_callback =
    match previous.lifecycle with
    | Waiting _ -> true
    | Admitted | Active | Terminal _ -> false
  in
  let terminal =
    if (not settled) || awaiting_callback
    then None
    else (
      match outcome with
      | Failed -> Some (P.Run.Terminal.Failed None)
      | Cancelled -> Some P.Run.Terminal.Cancelled
      | Limited -> Some P.Run.Terminal.Limited
      | Interrupted | Unconfirmed -> Some P.Run.Terminal.Interrupted
      | Succeeded ->
        (match requested, previous.mode with
         | Some terminal, _ -> Some terminal
         | None, Single_turn -> Some (P.Run.Terminal.Completed None)
         | None, Workflow -> None))
  in
  let lifecycle =
    Option.value_map terminal ~default:previous.lifecycle ~f:(fun terminal ->
      P.Run.Lifecycle.Terminal terminal)
  in
  let%bind run =
    P.Run.create
      ~id:previous.id
      ~session:previous.session
      ~principal_id:previous.principal_id
      ~source:previous.source
      ~mode:previous.mode
      ~lifecycle
      ~revision:(Int64.succ previous.revision)
      ~owned_work
      ~relinquished_work:previous.relinquished_work
      ~terminal_work
      ~created_at:previous.created_at
      ~updated_at:now
  in
  let%bind intents =
    match terminal with
    | None -> Ok []
    | Some _ ->
      List.map pending ~f:(fun intent -> Run_intent.consume intent ~operation_id)
      |> Result.all
  in
  let%bind index = Run_state.advance index ~run ~intents in
  match terminal with
  | None -> Ok index
  | Some terminal ->
    let%bind key =
      P.Idempotency_key.of_string
        ("run-terminal:" ^ P.Id.Run.to_string run.id ^ ":" ^ Int64.to_string run.revision)
    in
    let request_sha256 =
      P.Run.Terminal.to_json terminal
      |> Jsonaf.to_string
      |> Digestif.SHA256.digest_string
      |> Digestif.SHA256.to_hex
    in
    let%bind receipt =
      P.Run_receipt.create
        ~run_id:run.id
        ~principal_id:run.principal_id
        ~source:run.source
        ~key
        ~request_sha256
        ~kind:Terminal
        ~run_revision:run.revision
        ~session_revision:(Int64.succ state.counters.revision)
        ~committed_at:now
    in
    Run_state.commit index ~run ~receipt ~intent:None
;;
