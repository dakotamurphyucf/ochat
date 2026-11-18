open! Core
module P = Agent_protocol

type t =
  { session_id : P.Id.Session.t
  ; generation : int
  ; stop_epoch : int64
  ; spec : Session_state.Spec.Resource_identity.t
  ; lifecycle : Moderator_lifecycle.t
  ; checkpoint : Jsonaf.t
  }

let unavailable () =
  P.Error.create
    Conflict
    ~message:"current installation has no completed policy-restoration basis"
    ~retryable:false
    ()
;;

let lifecycle t = t.lifecycle

let project (state : Session_state.t) =
  let open Result.Let_syntax in
  let%bind () =
    if state.identity.generation >= 0 && Int64.(state.stop_epoch >= 0L)
    then Ok ()
    else Error (unavailable ())
  in
  let%bind () =
    match state.runtime_initialization, state.halted, state.failure with
    | Ready, false, None -> Ok ()
    | (Ready | Pending _), _, _ -> Error (unavailable ())
  in
  let%bind index = Result.of_option state.run_state ~error:(unavailable ()) in
  let%bind source =
    Run_source_installation.captured
      (Run_state.installation index)
      ~generation:state.identity.generation
  in
  let%bind lifecycle =
    Result.of_option (Run_state.moderator_lifecycle index) ~error:(unavailable ())
  in
  let%bind () =
    Moderator_lifecycle.validate
      lifecycle
      ~session_id:state.identity.session_id
      ~generation:state.identity.generation
      ~current_source:source
      ~executions:state.moderator_executions
  in
  let%bind receipt =
    Result.of_option
      (List.find state.moderator_executions ~f:(fun receipt ->
         P.Id.Moderator_execution.equal
           receipt.P.Moderator_execution.context.id
           (Moderator_lifecycle.execution_id lifecycle)))
      ~error:(unavailable ())
  in
  let%bind () =
    match receipt.P.Moderator_execution.status with
    | Completed _ -> Ok ()
    | Running | Failed _ | Interrupted _ -> Error (unavailable ())
  in
  let%bind checkpoint = Result.of_option state.moderator ~error:(unavailable ()) in
  Ok
    { session_id = state.identity.session_id
    ; generation = state.identity.generation
    ; stop_epoch = state.stop_epoch
    ; spec = Session_state.Spec.Resource_identity.capture state.spec
    ; lifecycle
    ; checkpoint
    }
;;

let of_state (state : Session_state.t) =
  let open Result.Let_syntax in
  let%bind () = Session_state.validate state in
  let%bind basis = project state in
  let%bind checkpoint = Moderator_checkpoint.of_jsonaf basis.checkpoint in
  let snapshot = Moderator_checkpoint.snapshot checkpoint in
  let observer : P.Invocation.observer =
    { script_id = snapshot.script_id; source_sha256 = snapshot.script_source_hash }
  in
  if
    (not snapshot.halted)
    && P.Invocation.equal_observer
         observer
         (Moderator_lifecycle.source basis.lifecycle).observer
  then Ok basis
  else Error (unavailable ())
;;

let check_policy t ~state =
  let open Result.Let_syntax in
  let%bind current = project state in
  if
    P.Id.Session.equal t.session_id current.session_id
    && Int.equal t.generation current.generation
    && Int64.equal t.stop_epoch current.stop_epoch
    && Session_state.Spec.Resource_identity.equal t.spec current.spec
    && Moderator_lifecycle.equal t.lifecycle current.lifecycle
  then Ok ()
  else Error (unavailable ())
;;

let check_construction t ~state =
  let open Result.Let_syntax in
  let%bind () = check_policy t ~state in
  match state.Session_state.moderator with
  | Some checkpoint when Jsonaf.exactly_equal t.checkpoint checkpoint -> Ok ()
  | Some _ | None -> Error (unavailable ())
;;
