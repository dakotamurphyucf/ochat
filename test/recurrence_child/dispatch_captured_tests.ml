open! Core
open Support
module Cold = Activation_tests
module Host_configuration = Agent_server.Recurrence_execution_configuration
module Planning_configuration = Agent_server.Recurrence_automation_configuration
module Execution = A.Recurrence_execution
module Origin = A.Recurrence_run_origin
module Terminal = A.Recurrence_run_reference

let configured (config : Agent_server.Config.t) session_id =
  let planning =
    Planning_configuration.create
      ~principal_id:(P.Id.Principal.of_string "pri_485_captured_planner" |> protocol_ok)
      ~controllers:[ session_id ]
      ~expires_at:None
      ~zone_directory:None
    |> protocol_ok
  in
  let grant =
    Host_configuration.Controller.create
      ~session_id
      ~existing_targets:[]
      ~captured_children:true
    |> protocol_ok
  in
  let execution =
    Host_configuration.create
      ~principal_id:(P.Id.Principal.of_string "pri_485_captured_executor" |> protocol_ok)
      ~controllers:[ grant ]
      ~expires_at:None
    |> protocol_ok
  in
  { config with
    Agent_server.Config.server =
      { config.server with
        recurrence_automation = Some planning
      ; recurrence_execution = Some execution
      }
  }
;;

let await env f =
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
    let rec loop () =
      match f () with
      | Some result -> result
      | None ->
        Eio.Fiber.yield ();
        loop ()
    in
    loop ())
;;

let healthy child =
  let current = state child in
  Option.iter current.failure ~f:(fun error -> raise_s [%sexp (error : P.Error.t)]);
  List.iter current.jobs ~f:(fun job ->
    match job.P.Job.status with
    | Failed error -> raise_s [%sexp (error : P.Error.t)]
    | Cancelled | Interrupted _ -> raise_s [%sexp (job : P.Job.t)]
    | Queued | Running | Waiting_permission _ | Waiting_completion _ | Succeeded -> ());
  current
;;

let actual_execution parent =
  Entry.frontier
    (Catalog.find (Option.value_exn (state parent).recurrence_catalog) recurrence_id
     |> Option.value_exn)
  |> protocol_ok
  |> A.Recurrence_frontier.executions
  |> List.hd_exn
;;

let%expect_test
    "sole daemon driver reserves creates and activates its declared captured mode"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = T.temporary_root env in
    protect_cleanup
      ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true Eio.Path.(env#fs / root))
      (fun () ->
         let prompt = Filename.concat root "parent.chatmd" in
         let child_prompt = Filename.concat root "captured.chatmd" in
         Eio.Path.save
           ~create:(`Exclusive 0o600)
           Eio.Path.(env#fs / prompt)
           {|<developer>Configured captured recurrence controller.</developer>
<script id="controller" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = Task.pure(state)
</script>|};
         Eio.Path.save
           ~create:(`Exclusive 0o600)
           Eio.Path.(env#fs / child_prompt)
           Cold.cold_prompt;
         Eio.Path.save
           ~create:(`Exclusive 0o600)
           Eio.Path.(env#fs / root / "activation-recipe.chatmd")
           "<developer>Actual captured scheduler Model.call recipe.</developer>";
         Eio.Path.save
           ~create:(`Exclusive 0o600)
           Eio.Path.(env#fs / root / "activation-native.txt")
           "485 actual allowed captured read";
         let config = T.config root root prompt in
         let configured_prompt = List.hd_exn config.prompts in
         let config =
           { config with
             prompts =
               config.prompts
               @ [ { configured_prompt with
                     id = "cold-activation-target"
                   ; path = child_prompt
                   }
                 ]
           }
         in
         let providers = ref 0 in
         let bootstrap_policy =
           T.inference_policy
             ~default_model:"fixture-model"
             ~post_stream:(fun ~sw:_ ~inputs:_ ->
               failwith "captured bootstrap activated inference")
         in
         let parent_id =
           with_daemon
             ~policy:bootstrap_policy
             env
             root
             config
             ~provider_calls:providers
             (fun sw daemon ->
                let connection = T.connection daemon principal in
                protect_cleanup
                  ~finally:(fun () -> Agent_client.Connection.close connection)
                  (fun () ->
                     T.initialize connection;
                     let created, _ =
                       T.create_session ~start_immediately:true connection
                     in
                     let parent = loaded daemon created.id in
                     start_parent_settled env sw connection parent;
                     let target, _ =
                       Cold.cold_target
                         ~catalog_target:true
                         ~mode:Single_turn
                         env
                         daemon
                         parent
                         ~root_file:child_prompt
                     in
                     with_planner env sw daemon parent (fun () ->
                       ready_nominal parent target |> ignore);
                     let frontier =
                       Entry.frontier
                         (Catalog.find
                            (Option.value_exn (state parent).recurrence_catalog)
                            recurrence_id
                          |> Option.value_exn)
                       |> protocol_ok
                     in
                     [%test_eq: int]
                       1
                       (List.length (A.Recurrence_frontier.ready frontier));
                     assert (
                       List.is_empty (A.Recurrence_frontier.protected_children frontier));
                     assert (
                       List.is_empty (A.Recurrence_frontier.child_associations frontier));
                     assert (List.is_empty (A.Recurrence_frontier.executions frontier));
                     [%test_eq: int] 0 !providers;
                     created.id))
         in
         let entered, signal_entered = Eio.Promise.create () in
         let release, signal_release = Eio.Promise.create () in
         let release_provider () =
           if Option.is_none (Eio.Promise.peek release)
           then Eio.Promise.resolve signal_release ()
         in
         let policy =
           T.inference_policy
             ~default_model:"fixture-model"
             ~post_stream:(fun ~sw:_ ~inputs:_ ->
               Int.incr providers;
               if Int.equal !providers 1
               then (
                 Eio.Promise.resolve signal_entered ();
                 Eio.Promise.await release);
               if !providers > 2 then failwith "captured startup replayed provider";
               Cold.completed_response "485 actual captured result")
         in
         with_daemon
           ~policy
           env
           root
           (configured config parent_id)
           ~provider_calls:providers
           (fun _sw daemon ->
              protect_cleanup ~finally:release_provider (fun () ->
                Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 20. (fun () ->
                  Eio.Promise.await entered;
                  let parent = loaded daemon parent_id in
                  let execution = actual_execution parent in
                  let attempt = Execution.current_attempt execution in
                  let origin = Execution.Attempt.origin attempt |> Option.value_exn in
                  let admission = Origin.admission origin in
                  let child =
                    loaded daemon (P.Session_ref.session_id (Origin.session origin))
                  in
                  let current = healthy child in
                  let index = Option.value_exn current.run_state in
                  let run = A.Run_state.find index admission.run_id |> Option.value_exn in
                  assert (P.Run.Mode.equal run.mode Single_turn);
                  assert (
                    P.Id.Principal.equal
                      run.principal_id
                      (P.Id.Principal.of_string "pri_485_captured_executor" |> protocol_ok));
                  Origin.validate_run origin ~run ~admission |> protocol_ok;
                  assert (
                    Origin.equal
                      origin
                      (A.Run_state.recurrence_origin index run.id |> Option.value_exn));
                  assert (Option.is_none (Execution.Attempt.terminal attempt));
                  let association =
                    A.Recurrence_execution_intent.child (Execution.intent execution)
                    |> Option.value_exn
                  in
                  let reference = A.Recurrence_child_association.reference association in
                  assert (
                    Option.exists
                      current.spec.delegation
                      ~f:(Agent_store.Delegation_store.Reference.equal reference));
                  let record =
                    Agent_store.Delegation_store.resolve
                      (S.delegations (Daemon.store daemon))
                      reference
                    |> store_ok
                  in
                  assert (Agent_store.Delegation_store.equal_stage record.stage Linked);
                  [%test_eq: int] 1 !providers;
                  release_provider ();
                  let final =
                    await env (fun () ->
                      healthy child |> ignore;
                      let execution = actual_execution parent in
                      match
                        Execution.current_attempt execution |> Execution.Attempt.terminal
                      with
                      | None -> None
                      | Some reference -> Some (execution, reference))
                  in
                  let execution, terminal = final in
                  assert (not (Execution.is_pending execution));
                  assert (P.Id.Run.equal admission.run_id (Terminal.run_id terminal));
                  assert (Terminal.Outcome.equal (Terminal.outcome terminal) Completed);
                  let finished = healthy child in
                  let actual_run =
                    A.Run_state.find
                      (Option.value_exn finished.run_state)
                      admission.run_id
                    |> Option.value_exn
                  in
                  Terminal.validate
                    terminal
                    ~origin
                    ~run:actual_run
                    ~receipt:(Terminal.receipt terminal)
                  |> protocol_ok;
                  [%test_eq: int] 1 (List.length (Execution.attempts execution));
                  [%test_eq: int] 2 !providers;
                  assert (
                    Agent_server.Session_registry.execution_selected
                      (Daemon.registry daemon)
                      child);
                  let starts =
                    List.filter finished.moderator_executions ~f:(fun receipt ->
                      P.Moderator_execution.equal_phase
                        receipt.context.phase
                        Session_start)
                  in
                  [%test_eq: int] 1 (List.length starts);
                  match (List.hd_exn starts).status with
                  | Completed _ -> ()
                  | _ -> failwith "captured startup did not complete")))));
  print_endline
    "sole timer owns fresh reservation and real linked child; reciprocal Run exists \
     before first Model; declared Single_turn settles once";
  [%expect
    {| sole timer owns fresh reservation and real linked child; reciprocal Run exists before first Model; declared Single_turn settles once |}]
;;
