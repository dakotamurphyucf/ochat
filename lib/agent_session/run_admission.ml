open! Core
module P = Agent_protocol

module Scope = struct
  type t =
    { principal_id : P.Id.Principal.t
    ; observer : P.Invocation.observer
    ; startup_pending : unit -> bool
    ; authorize : Session_state.t -> (unit, P.Error.t) result
    }

  let create ~principal_id ~observer ~startup_pending ~authorize =
    let open Result.Let_syntax in
    let%bind _ = P.Id.Principal.of_json (P.Id.Principal.to_json principal_id) in
    let%map _ = P.Run_source.create ~observer ~generation:0 ~installation_epoch:1L in
    { principal_id; observer; startup_pending; authorize }
  ;;

  let principal_id t = t.principal_id
  let observer t = t.observer
  let authorize t state = t.authorize state
  let startup_pending t = t.startup_pending ()
end

type t =
  { run : P.Run.t
  ; receipt : P.Run_receipt.t
  ; index : Run_state.t
  }

let run t = t.run
let receipt t = t.receipt
let index t = t.index
let invalid message = Error (P.Error.invalid_request message)
let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())

let observer_equal left right =
  String.equal left.P.Invocation.script_id right.P.Invocation.script_id
  && String.equal left.source_sha256 right.source_sha256
;;

let current_installation state scope =
  let open Result.Let_syntax in
  let index = Option.value state.Session_state.run_state ~default:Run_state.empty in
  let installed = Run_state.installation index in
  match installed.source with
  | Some observer ->
    if observer_equal observer (Scope.observer scope)
    then Ok index
    else conflict "compiled run source differs from current installation"
  | None ->
    let%bind installation =
      Run_source_installation.apply installed ~change:(Replace (Scope.observer scope))
    in
    Run_state.replace_installation index ~installation ~retired_runs:[]
;;

let owned_operation state request operation =
  let open Result.Let_syntax in
  match request.P.Run_start.input, operation with
  | Authored_start, None -> Ok []
  | Authored_start, Some _ ->
    invalid "authored startup cannot fabricate an input operation"
  | User_submission _, None ->
    invalid "user run admission requires its actual input operation"
  | User_submission _, Some (operation : P.Operation.t) ->
    (match operation.kind, operation.state with
     | Turn User_submit, Starting
       when Int.equal operation.generation state.Session_state.identity.generation ->
       let%map work =
         P.Run_work.create ~key:(Operation operation.id) ~generation:operation.generation
       in
       [ work ]
     | ( Turn _
       , ( Starting
         | Running
         | Cancelling
         | Completed
         | Failed _
         | Cancelled
         | Interrupted _ ) )
     | ( Compaction
       , ( Starting
         | Running
         | Cancelling
         | Completed
         | Failed _
         | Cancelled
         | Interrupted _ ) ) ->
       invalid "run input operation does not match actual starting user turn")
;;

let prepare_checked
      (state : Session_state.t)
      ~expected_current_revision
      ~scope
      ~(request : P.Run_start.t)
      ~session
      ~run_id
      ~operation
      ~request_sha256
      ~now
  =
  let open Result.Let_syntax in
  let%bind () = Scope.authorize scope state in
  let%bind () =
    if
      P.Id.Session.equal request.session_id state.identity.session_id
      && P.Id.Session.equal (P.Session_ref.session_id session) state.identity.session_id
      && Int.equal request.generation state.identity.generation
      && Int64.equal expected_current_revision state.counters.revision
    then Ok ()
    else conflict "run admission session generation or revision changed"
  in
  let%bind current_observer = Moderator_checkpoint.observer state.moderator in
  let%bind () =
    match current_observer with
    | Some observer when observer_equal observer (Scope.observer scope) -> Ok ()
    | Some _ -> conflict "compiled run source no longer matches the actor checkpoint"
    | None -> conflict "run admission requires the installed compiled source checkpoint"
  in
  let%bind () =
    match request.input with
    | User_submission _ -> Ok ()
    | Authored_start ->
      (match
         state.lifecycle.desired, state.lifecycle.observed, state.active_operation
       with
       | Stopped, Stopped, None when Scope.startup_pending scope -> Ok ()
       | _ -> conflict "authored run start requires an actual pending startup")
  in
  let%bind index = current_installation state scope in
  let%bind source =
    Run_source_installation.captured
      (Run_state.installation index)
      ~generation:state.identity.generation
  in
  let%bind () =
    match request.mode with
    | Single_turn -> Ok ()
    | Workflow ->
      if
        List.exists (Run_state.runs index) ~f:(fun (run : P.Run.t) ->
          P.Run_source.equal run.source source
          && P.Run.Mode.equal run.mode Workflow
          &&
          match run.lifecycle with
          | Admitted | Active | Waiting _ -> true
          | Terminal _ -> false)
      then conflict "current source already owns an unfinished workflow run"
      else Ok ()
  in
  let%bind owned_work = owned_operation state request operation in
  let%bind run =
    P.Run.create
      ~id:run_id
      ~session
      ~principal_id:(Scope.principal_id scope)
      ~source
      ~mode:request.mode
      ~lifecycle:Admitted
      ~revision:0L
      ~owned_work
      ~relinquished_work:[]
      ~terminal_work:[]
      ~created_at:now
      ~updated_at:now
  in
  let%bind receipt =
    if Int64.equal state.counters.revision Int64.max_value
    then
      Error
        (P.Error.create
           Resource_limit
           ~message:"session revision exhausted"
           ~retryable:false
           ())
    else
      P.Run_receipt.create
        ~run_id
        ~principal_id:run.principal_id
        ~source
        ~key:request.key
        ~request_sha256
        ~kind:Admission
        ~run_revision:0L
        ~session_revision:Int64.(state.counters.revision + 1L)
        ~committed_at:now
  in
  let%map index = Run_state.commit index ~run ~receipt ~intent:None in
  { run; receipt; index }
;;

let prepare
      state
      ~scope
      ~(request : P.Run_start.t)
      ~session
      ~run_id
      ~operation
      ~request_sha256
      ~now
  =
  prepare_checked
    state
    ~expected_current_revision:request.expected_revision
    ~scope
    ~request
    ~session
    ~run_id
    ~operation
    ~request_sha256
    ~now
;;

let prepare_from_preparation
      state
      ~preparation
      ~owner
      ~scope
      ~session
      ~run_id
      ~operation
      ~now
  =
  let open Result.Let_syntax in
  let%bind () = Run_preparation.check preparation ~owner ~state in
  let%bind () =
    if
      P.Id.Principal.equal
        (Run_preparation.principal_id preparation)
        (Scope.principal_id scope)
    then Ok ()
    else conflict "run preparation principal does not match its source scope"
  in
  prepare_checked
    state
    ~expected_current_revision:state.Session_state.counters.revision
    ~scope
    ~request:(Run_preparation.request preparation)
    ~session
    ~run_id
    ~operation
    ~request_sha256:(Run_preparation.request_sha256 preparation)
    ~now
;;
