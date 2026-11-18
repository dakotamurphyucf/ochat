open! Core
module P = Agent_protocol

type t = { delta : Session_delta.t }

let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())

let prepare
      (state : Session_state.t)
      ~(run : P.Run.t)
      ~request
      ~input
      ~(operation : P.Operation.t)
      ~pending
      ~now
  =
  let open Result.Let_syntax in
  let%bind index =
    Result.of_option
      state.run_state
      ~error:(P.Error.invalid_request "held root has no Run index")
  in
  let%bind () =
    match
      ( Run_state.find index run.id
      , run.lifecycle
      , request.Run_admission_request.input
      , operation.kind
      , operation.state )
    with
    | Some current, (Admitted | Active), User_submission _, Turn User_submit, Starting
      when P.Run.equal current run
           && Int.equal state.identity.generation operation.generation
           && Int.equal run.source.generation operation.generation
           && P.Id.Session.equal request.session_id state.identity.session_id
           && Pending_turn.accepted pending -> Ok ()
    | _ ->
      conflict "held root requires its exact live Run and actual accepted user Operation"
  in
  let%bind work =
    P.Run_work.create ~key:(Operation operation.id) ~generation:operation.generation
  in
  let%bind () =
    if List.mem run.owned_work work ~equal:P.Run_work.equal
    then conflict "held root Operation is already owned"
    else Ok ()
  in
  let entry = Runtime_input_preparation.Prepared_input.entry input in
  let%bind () =
    if List.exists (Pending_turn.entries pending) ~f:(P.History.equal_entry entry)
    then Ok ()
    else conflict "held root did not adopt its recorded parsed input"
  in
  let%map index =
    Run_work_settlement.prepare
      index
      ~state
      ~previous:run
      ~owned_work:(run.owned_work @ [ work ])
      ~terminal_work:run.terminal_work
      ~operation_id:None
      ~now
  in
  { delta = Session_delta.Run_state_changed index }
;;

let delta t = t.delta
