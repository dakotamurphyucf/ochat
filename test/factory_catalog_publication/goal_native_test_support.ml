open! Core
open Agent_server_test_support
open Retry_support
include Goal_native_callback_support

let set_passive daemon entry =
  Goal_admitted_guard_tests.command
    daemon
    entry
    ~key:"native-goal-needs-input"
    (Hold
       ( Goal.goal_id
       , Needs_input (G.Reason.of_string "native host may resume" |> protocol_ok) ))
  |> protocol_ok
  |> ignore;
  let current = state entry in
  assert (
    G.State.equal
      (Open
         (Held (Needs_input (G.Reason.of_string "native host may resume" |> protocol_ok))))
      (G.state (Goal.goal current)));
  current
;;

let ordinary ?(key = "native-goal-ordinary-owner") daemon entry =
  let current = state entry in
  let request =
    A.Run_admission_request.Headless.create
      ~session_id:current.identity.session_id
      ~generation:current.identity.generation
      ~expected_revision:current.counters.revision
      ~mode:Single_turn
      ~input:
        (User_submission
           { kind = Plain_text
           ; text = "ordinary native Goal command owner"
           ; attachments = []
           })
      ~key:(P.Idempotency_key.of_string key |> protocol_ok)
    |> protocol_ok
  in
  match Goal.ordinary daemon entry request with
  | Admitted receipt -> receipt
  | Rejected error | Uncertain error -> raise_s [%sexp (error : P.Error.t)]
;;

(* Legacy orchestration has no compiled Run admission observer. Its actual
   ordinary user-input route still installs authenticated input custody and
   the real worker Operation; the writer stays lexical through the test body. *)
let with_ordinary_owner daemon entry ~v1 f =
  if v1
  then f (Some (ordinary daemon entry))
  else (
    let writer, _ =
      Actor.attach entry.Registry.actor ~mode:Read_write ~subscribe:false |> protocol_ok
    in
    protect_cleanup
      ~finally:(fun () -> Actor.detach entry.actor writer.id |> protocol_ok)
      (fun () ->
         Actor.start entry.actor ~actor:original ~attachment_id:writer.id
         |> protocol_ok
         |> ignore;
         let id = A.History_id_source.allocate entry.history_ids |> protocol_ok in
         let message =
           Owner.parse_user_content
             entry.runtime
             ~id
             { kind = Plain_text
             ; text = "ordinary legacy native Goal command owner"
             ; attachments = []
             }
           |> protocol_ok
           |> A.History_codec.to_protocol
         in
         let submission =
           Actor.submit_message_authenticated
             entry.actor
             ~actor:original
             ~attachment_id:writer.id
             message
           |> protocol_ok
         in
         assert (Option.is_some submission.operation_id);
         f None))
;;

let await_quiet entry =
  let rec loop () =
    let current = state entry in
    if
      Option.is_none current.active_operation
      && List.for_all current.moderator_executions ~f:(fun execution ->
        match execution.P.Moderator_execution.status with
        | Completed _ | Failed _ | Interrupted _ -> true
        | Running -> false)
    then current
    else (
      Eio.Fiber.yield ();
      loop ())
  in
  loop ()
;;

let authorize actor current =
  if Operator_authorization.is_current actor
  then Staging.authorize_as (Operator_authorization.principal actor) current
  else
    Error
      (P.Error.create
         Permission_denied
         ~message:"native Goal host policy original expired"
         ~retryable:false
         ())
;;

let progress entry =
  (* Capture command DATA before entering the selected Progress effect. A Progress
     admission cannot impersonate the separate Get capability. *)
  let current = state entry in
  let goal = Goal.goal current in
  let edit =
    P.Goal_edit.create
      ~generation:current.identity.generation
      ~source:(G.source goal)
      ~goal_id:Goal.goal_id
      ~expected_goal_revision:(G.revision goal)
      ~expected_catalog_revision:
        (A.Goal_catalog.revision (Option.value_exn current.goal_catalog))
      ~key:(P.Idempotency_key.of_string "native-goal-progress" |> protocol_ok)
      ~mutation:
        (Record_progress
           (G.Progress.of_string "actual local native progress" |> protocol_ok))
    |> protocol_ok
  in
  fun admission -> Actor.stage_native_goal_edit entry.actor ~admission ~authorize ~edit
;;

let await_activation loaded runtime =
  (* Resume is an ordinary owner checkpoint. Join the real scheduler's startup
     before capturing the first new status command's revision; no retry or
     rebase of an already admitted command is permitted here. *)
  let rec await_activation () =
    match runtime.A.Runtime_builder.moderator_activation with
    | Some activation when activation.pending () ->
      Eio.Fiber.yield ();
      await_activation ()
    | Some _ | None -> ()
  in
  await_activation ();
  let current = state loaded in
  assert (Option.is_none current.failure);
  assert (not current.halted);
  assert (Option.is_none current.active_operation)
;;

let with_callback ?test_root ?(wrap_env = Fn.id) ~v1 f =
  let ordinal = ref 0 in
  let ordinary_entered, ordinary_entered_u = Eio.Promise.create () in
  let ordinary_release, ordinary_release_u = Eio.Promise.create () in
  let resumed_entered, resumed_entered_u = Eio.Promise.create () in
  let resumed_release, resumed_release_u = Eio.Promise.create () in
  let release promise resolver =
    if Option.is_none (Eio.Promise.peek promise) then Eio.Promise.resolve resolver ()
  in
  let release_ordinary () = release ordinary_release ordinary_release_u in
  let release_resumed () = release resumed_release resumed_release_u in
  let post_stream _ =
    incr ordinal;
    (match !ordinal with
     | 2 ->
       Eio.Promise.resolve ordinary_entered_u ();
       Eio.Promise.await ordinary_release
     | 3 ->
       Eio.Promise.resolve resumed_entered_u ();
       Eio.Promise.await resumed_release
     | _ -> ());
    completed_response "native Goal fixture actual provider"
  in
  Staging.with_recoverable_episode
    ?test_root
    ~wrap_env
    ~prompt_text:(prompt ~v1)
    ~post_stream
    (fun
        env
         sw
         root
         daemon
         policy
         entry
         runtime
         counters
         session
         admission
         old_live
         _held
       ->
       Exn.protect
         ~finally:(fun () ->
           release_ordinary ();
           release_resumed ())
         ~f:(fun () ->
           let held = set_passive daemon entry in
           let request = Staging.exact_request held ~key:"native-goal-command" in
           with_ordinary_owner daemon entry ~v1 (fun ordinary_receipt ->
             Eio.Promise.await ordinary_entered;
             let operation = Option.value_exn (state entry).active_operation in
             Option.iter ordinary_receipt ~f:(fun ordinary_receipt ->
               assert (
                 not
                   (P.Id.Run.equal ordinary_receipt.P.Run_receipt.run_id admission.run_id));
               assert (
                 Option.is_none
                   (A.Run_state.goal_link
                      (Option.value_exn (state entry).run_state)
                      ordinary_receipt.run_id)));
             let reopen f =
               let old_store = Daemon.store daemon in
               Daemon.shutdown daemon |> protocol_ok;
               assert (Agent_store.Session_store.is_closed old_store);
               let recovered =
                 Daemon.start
                   ~sw
                   ~env:(env :> Eio_unix.Stdenv.base)
                   ~config:(config root root (Filename.concat root "parent.chatmd"))
                   ~tool_dir:root
                   ~home:root
                   ~process_start_identity:None
                   ~options:
                     { Daemon.default_options with
                       inference_policy = policy
                     ; qualify_chatml_extensions = true
                     }
                   ()
                 |> protocol_ok
               in
               protect_cleanup
                 ~finally:(fun () -> Daemon.shutdown recovered |> protocol_ok)
                 (fun () ->
                    let loaded =
                      Registry.load (Daemon.registry recovered) held.identity.session_id
                      |> protocol_ok
                    in
                    assert (not (phys_equal loaded.Registry.actor entry.actor));
                    Owner.ensure_loaded_authenticated loaded.runtime ~actor:original
                    |> protocol_ok;
                    let actual =
                      Owner.with_loaded_runtime loaded.runtime (fun runtime -> Ok runtime)
                      |> protocol_ok
                    in
                    f recovered loaded actual)
             in
             f
               env
               daemon
               entry
               runtime
               counters
               session
               admission
               old_live
               held
               request
               operation
               ~release_ordinary
               ~resumed_entered
               ~release_resumed
               ~reopen)))
;;
