open! Core
module P = Agent_protocol
module Manager = Chat_response.Moderator_manager
module Startup = Runtime_builder.Authored_start
module Snapshot = Session.Moderator_state.Identity_snapshot

type t = Startup.t

let denied message = Error (P.Error.create Conflict ~message ~retryable:false ())

let matches_runtime t ~worker ~manager =
  Option.exists worker ~f:(phys_equal (Startup.worker t))
  && Option.equal phys_equal manager (Startup.manager t)
;;

(* Candidate selection is factual only. Restoring a different source or a failed
   callback must not turn an ordinary install into a new first-start admission. *)
let eligible t ~(state : Session_state.t) =
  match Startup.disposition t, Startup.manager t with
  | Extension_pending, Some manager ->
    Session_state.Runtime_initialization.equal state.runtime_initialization Ready
    && P.Session.equal_desired_state state.lifecycle.desired Running
    && Option.for_all state.run_state ~f:(fun index ->
      List.is_empty (Run_state.runs index))
    && Option.exists (Manager.invocation_observer manager) ~f:(fun observer ->
      not
        (List.exists state.moderator_executions ~f:(fun receipt ->
           Int.equal
             receipt.P.Moderator_execution.context.generation
             state.identity.generation
           && P.Invocation.equal_observer receipt.context.source observer
           && P.Moderator_execution.equal_phase receipt.context.phase Session_start)))
  | (Legacy_acknowledged | No_callback), _ | Extension_pending, None -> false
;;

let check_claim t ~(state : Session_state.t) ~worker ~manager =
  let open Result.Let_syntax in
  let%bind () =
    match Startup.disposition t with
    | Extension_pending when matches_runtime t ~worker ~manager -> Startup.check_pending t
    | Extension_pending | Legacy_acknowledged | No_callback ->
      denied "initial lifecycle requires its actual pending extension runtime"
  in
  let%bind manager =
    Result.of_option
      manager
      ~error:
        (P.Error.create
           Conflict
           ~message:"initial lifecycle Manager is absent"
           ~retryable:false
           ())
  in
  let%bind observer =
    Result.of_option
      (Manager.invocation_observer manager)
      ~error:
        (P.Error.create
           Conflict
           ~message:"initial lifecycle requires its compiled v1 source"
           ~retryable:false
           ())
  in
  let%bind retained = Runtime_builder.moderator_snapshot_observer state.moderator in
  let%bind source =
    match state.run_state with
    | None -> denied "initial lifecycle has no installed source index"
    | Some index ->
      Run_source_installation.captured
        (Run_state.installation index)
        ~generation:state.identity.generation
  in
  if
    eligible t ~state
    && (not state.halted)
    && Option.is_none state.failure
    && Option.equal P.Invocation.equal_observer retained (Some observer)
    && P.Invocation.equal_observer source.observer observer
    && Int.equal source.generation state.identity.generation
  then Ok ()
  else denied "initial lifecycle source, first-start or runtime state changed"
;;

let check_capture t ~state ~worker ~manager =
  let open Result.Let_syntax in
  let%bind () = check_claim t ~state ~worker ~manager in
  let%bind manager =
    Result.of_option
      manager
      ~error:
        (P.Error.create
           Conflict
           ~message:"initial lifecycle Manager is absent"
           ~retryable:false
           ())
  in
  let%bind saved = Moderator_checkpoint.decode state.Session_state.moderator in
  let%bind actual =
    Manager.identity_snapshot manager
    |> Result.map_error ~f:(fun message ->
      P.Error.create Conflict ~message ~retryable:false ())
  in
  match saved with
  | Some saved when Sexp.equal (Snapshot.sexp_of_t saved) (Snapshot.sexp_of_t actual) ->
    Ok ()
  | None | Some _ -> denied "initial lifecycle requires the complete installed checkpoint"
;;
