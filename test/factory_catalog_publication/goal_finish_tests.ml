open! Core
open Agent_server_test_support
open Retry_support
module Service = Agent_server.Goal_run_service
module Goal = Goal_service_tests
module G = P.Goal

module Winner = struct
  type t =
    | Complete
    | Cancel
  [@@deriving sexp_of]
end

let finish_action : P.Run_action.t = Finish { terminal = Completed None; relinquish = [] }

let evidence =
  G.Reason.of_string "Actual authored Turn_end completion evidence" |> protocol_ok
;;

let prompt_text =
  let intent =
    P.Goal_achievement.Intent.create
      ~expected_goal_revision:1L
      ~evidence
      ~action:finish_action
    |> protocol_ok
    |> P.Goal_achievement.Intent.to_json
    |> Jsonaf.to_string
  in
  sprintf
    {|<developer>Actual authored Goal finish owner.</developer>
<script id="goal_finish" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = match event with
| `Turn_end ->
  let intent = Json.parse(%S) in
  Task.map(Goal.finish(intent), fun ignored -> state + 1)
| _ -> Task.pure(state)
</script>|}
    intent
;;

let create_goal daemon entry =
  let before = state entry in
  let catalog = Option.value before.goal_catalog ~default:A.Goal_catalog.empty in
  let _, source = A.Goal_catalog_source.capture before |> protocol_ok in
  let budget =
    G.Budget.create
      ~admitted_turns:(Some 1L)
      ~admitted_attempts:(Some 1L)
      ~observed_tokens:None
      ~deadline:None
    |> protocol_ok
  in
  let request =
    A.Goal_command.create
      ~session_id:before.identity.session_id
      ~generation:before.identity.generation
      ~source:(Some source)
      ~expected_catalog_revision:(A.Goal_catalog.revision catalog)
      ~expected_goal_revision:None
      ~key:(P.Idempotency_key.of_string "goal-finish-create" |> protocol_ok)
      ~mutation:
        (Create
           { id = Goal.goal_id
           ; objective =
               G.Objective.of_string "Finish through actual authored scope" |> protocol_ok
           ; budget
           })
    |> protocol_ok
  in
  let session =
    P.Session_ref.create
      ~server_id:(Agent_store.Session_store.server_id (Daemon.store daemon))
      ~session_id:before.identity.session_id
  in
  Actor.goal_command entry.Registry.actor ~actor:original ~authorize ~session ~request
  |> protocol_ok
  |> ignore;
  let current = state entry in
  assert (G.State.equal (Open (Held Source_review)) (G.state (Goal.goal current)));
  assert (Option.is_none (G.episode (Goal.goal current)));
  [%test_eq: int64] 0L (G.revision (Goal.goal current));
  [%test_eq: int64] 0L (A.Goal_accounting.admitted_attempts (Goal.accounting current));
  [%test_eq: int64] 0L (A.Goal_accounting.admitted_turns (Goal.accounting current));
  session, current
;;

let terminal_receipt index admission =
  match
    List.filter (A.Run_state.receipts index) ~f:(fun receipt ->
      P.Id.Run.equal admission.P.Run_receipt.run_id receipt.P.Run_receipt.run_id
      && P.Run_receipt.Kind.equal Terminal receipt.kind)
  with
  | [ receipt ] -> receipt
  | _ -> failwith "Goal finish did not retain exactly one actual terminal receipt"
;;

let run (winner : Winner.t) =
  let armed = ref None in
  let reached, reached_u = Eio.Promise.create () in
  let release, release_u = Eio.Promise.create () in
  let released = ref false
  and hits = ref 0 in
  let release_worker () =
    if not !released
    then (
      released := true;
      Eio.Promise.resolve release_u ())
  in
  let counters : Retry_support.counters =
    { inference_ports = ref 0; providers = ref 0; transcripts = ref [] }
  in
  let base =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs ->
      Int.incr counters.providers;
      counters.transcripts
      := !(counters.transcripts)
         @ [ `Array (List.map inputs ~f:Res.Item.jsonaf_of_t) |> Jsonaf.to_string ];
      completed_response "actual Goal finish provider result")
  in
  let policy =
    { base with
      runtime_inference_ports =
        (fun actor ->
          Int.incr counters.inference_ports;
          base.runtime_inference_ports actor)
    }
  in
  Creation_tests.with_daemon
    ~prompt_text
    ~qualify_chatml_extensions:true
    ~policy
    ~wrap_env:(fun env ->
      Crash_fault_io.wrap
        env
        ~boundary:Before_rename
        ~matches:(fun path ->
          Option.exists !armed ~f:(fun directory ->
            String.equal directory (Filename.dirname path)
            && String.is_prefix (Filename.basename path) ~prefix:".cache.bin."
            && String.is_suffix path ~suffix:".tmp"))
        ~reached:(fun path ->
          armed := None;
          Int.incr hits;
          Eio.Promise.resolve reached_u path;
          Eio.Promise.await release))
    (fun env _sw _root daemon ->
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
         Source.with_loaded_entry daemon (fun ~entry ~runtime:_ ~revision:_ ->
           protect_cleanup ~finally:release_worker (fun () ->
             let writer, _ =
               Actor.attach entry.actor ~mode:Read_write ~subscribe:false |> protocol_ok
             in
             let session, before = create_goal daemon entry in
             let request =
               A.Run_admission_request.Headless.create
                 ~session_id:before.identity.session_id
                 ~generation:before.identity.generation
                 ~expected_revision:before.counters.revision
                 ~mode:Workflow
                 ~input:
                   (User_submission
                      { kind = Plain_text
                      ; text = "542 authored Goal finish input"
                      ; attachments = []
                      })
                 ~key:(P.Idempotency_key.of_string "goal-finish-episode" |> protocol_ok)
               |> protocol_ok
               |> fun request ->
               A.Goal_activation_request.create
                 ~goal_id:Goal.goal_id
                 ~expected_goal_revision:(G.revision (Goal.goal before))
                 ~expected_catalog_revision:
                   (A.Goal_catalog.revision (Option.value_exn before.goal_catalog))
                 ~request
               |> protocol_ok
             in
             let public =
               Service.create ~factory:(Daemon.factory daemon) ~authorize:(fun _ ->
                 authorize)
             in
             let call () =
               Service.start_episode public ~entry ~actor:original ~session ~request
             in
             let handle = Option.value_exn entry.store_handle in
             let response_directory =
               Agent_store.Session_store.Handle.responses_directory handle
             in
             armed := Some response_directory;
             let admission = call () |> protocol_ok |> Goal.activated in
             let rec await_barrier () =
               match Eio.Promise.peek reached with
               | Some path -> path
               | None ->
                 let current = state entry in
                 List.iter current.moderator_executions ~f:(fun receipt ->
                   match receipt.P.Moderator_execution.status with
                   | Completed _ | Running -> ()
                   | Failed _ | Interrupted _ ->
                     raise_s
                       [%sexp
                         "Goal finish callback failed before its cache barrier"
                       , (receipt : P.Moderator_execution.t)]);
                 let selected =
                   A.Run_state.find (Option.value_exn current.run_state) admission.run_id
                   |> Option.value_exn
                 in
                 (match selected.lifecycle with
                  | Admitted | Active | Waiting _ -> ()
                  | Terminal _ ->
                    raise_s
                      [%sexp
                        "Goal finish Run ended before its cache barrier"
                      , (selected : P.Run.t)]);
                 Eio.Fiber.yield ();
                 await_barrier ()
             in
             let path =
               Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. await_barrier
             in
             [%test_eq: string] response_directory (Filename.dirname path);
             [%test_eq: int] 1 !hits;
             (* run_turn has ACKed Turn_end/Goal.finish. The actual cache rename
               precedes Operation_worker.Completed, so root join cannot have won. *)
             let pending = state entry in
             let pending_goal = Goal.goal pending in
             assert (G.State.equal (Open Enabled) (G.state pending_goal));
             let achievement_request =
               G.achievement_request pending_goal |> Option.value_exn
             in
             assert (Option.is_none (G.request_settlement pending_goal));
             let operation = Option.value_exn pending.active_operation in
             let index = Option.value_exn pending.run_state in
             let selected = A.Run_state.find index admission.run_id |> Option.value_exn in
             (match selected.lifecycle with
              | Active -> ()
              | _ ->
                raise_s
                  [%sexp "Finish prematurely completed its Run", (selected : P.Run.t)]);
             assert (
               P.Goal_episode.equal
                 (P.Goal_achievement.Request.episode achievement_request)
                 (G.episode pending_goal |> Option.value_exn));
             let action_receipt =
               P.Goal_achievement.Request.receipt achievement_request
             in
             assert (P.Run_receipt.Kind.equal Action action_receipt.kind);
             assert (
               P.Run_action.equal
                 finish_action
                 (P.Goal_achievement.Request.action achievement_request));
             assert (
               G.Reason.equal
                 evidence
                 (P.Goal_achievement.Request.evidence achievement_request));
             assert (
               List.exists
                 (A.Run_state.receipts index)
                 ~f:(P.Run_receipt.equal action_receipt));
             assert (
               List.exists (A.Run_state.intents index) ~f:(fun intent ->
                 P.Run_receipt.equal action_receipt intent.A.Run_intent.receipt
                 && P.Run_action.equal finish_action intent.action));
             assert (
               not
                 (List.exists (A.Run_state.receipts index) ~f:(fun receipt ->
                    P.Id.Run.equal selected.id receipt.P.Run_receipt.run_id
                    && P.Run_receipt.Kind.equal Terminal receipt.kind)));
             [%test_eq: int] 1 !(counters.providers);
             [%test_eq: int64]
               1L
               (A.Goal_accounting.admitted_attempts (Goal.accounting pending));
             [%test_eq: int64]
               1L
               (A.Goal_accounting.admitted_turns (Goal.accounting pending));
             (match winner with
              | Complete -> ()
              | Cancel ->
                Actor.cancel_operation
                  entry.actor
                  ~attachment_id:writer.id
                  ~operation_id:operation.id
                |> protocol_ok
                |> ignore);
             release_worker ();
             let rec await_terminal () =
               let current =
                 Actor.with_moderator_checkpoint entry.actor (fun () ->
                   Actor.state entry.actor)
                 |> protocol_ok
               in
               let index = Option.value_exn current.run_state in
               let run = A.Run_state.find index admission.run_id |> Option.value_exn in
               match run.lifecycle with
               | Admitted | Active | Waiting _ ->
                 Eio.Fiber.yield ();
                 await_terminal ()
               | Terminal _ ->
                 assert (Option.is_none current.active_operation);
                 assert (Option.is_some (G.request_settlement (Goal.goal current)));
                 current, index, run
             in
             let final, index, selected =
               Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. await_terminal
             in
             let final_goal = Goal.goal final in
             let terminal = terminal_receipt index admission in
             assert (
               P.Goal_episode.equal
                 (G.episode pending_goal |> Option.value_exn)
                 (G.episode final_goal |> Option.value_exn));
             assert (
               P.Goal_achievement.Request.equal
                 achievement_request
                 (G.achievement_request final_goal |> Option.value_exn));
             let settlement = G.request_settlement final_goal |> Option.value_exn in
             assert (
               P.Run_receipt.equal terminal (P.Goal_episode.Settlement.receipt settlement));
             (match winner, selected.lifecycle, G.state final_goal with
              | Complete, Terminal (Completed None), Closed (Achieved achievement) ->
                assert (
                  P.Goal_achievement.Request.equal
                    achievement_request
                    (P.Goal_achievement.request achievement));
                assert (
                  P.Run_receipt.equal terminal (P.Goal_achievement.receipt achievement))
              | Cancel, Terminal Cancelled, Open (Held _) -> ()
              | _ ->
                raise_s
                  [%sexp
                    "actual Goal terminal winner changed"
                  , (winner : Winner.t)
                  , (selected : P.Run.t)
                  , (G.state final_goal : G.State.t)]);
             A.Goal_run_evidence.validate
               ~catalog:final.goal_catalog
               ~runs:final.run_state
             |> protocol_ok;
             List.iter
               P.Run_receipt.Kind.[ Admission; Action; Terminal ]
               ~f:(fun kind ->
                 [%test_eq: int]
                   1
                   (List.count (A.Run_state.receipts index) ~f:(fun receipt ->
                      P.Id.Run.equal selected.id receipt.P.Run_receipt.run_id
                      && P.Run_receipt.Kind.equal kind receipt.kind)));
             [%test_eq: int] 1 !(counters.providers);
             [%test_eq: int64]
               1L
               (A.Goal_accounting.admitted_attempts (Goal.accounting final));
             [%test_eq: int64]
               1L
               (A.Goal_accounting.admitted_turns (Goal.accounting final));
             let frames = Existing.frames env entry in
             let counts = !(counters.providers), !(counters.inference_ports) in
             (match call () |> protocol_ok with
              | Retained actual -> assert (P.Run_receipt.equal admission actual)
              | Attempted _ -> failwith "finished Goal replay acquired a new attempt");
             Existing.assert_same final entry;
             [%test_eq: string list] frames (Existing.frames env entry);
             [%test_eq: int * int]
               counts
               (!(counters.providers), !(counters.inference_ports));
             [%test_eq: int] 1 !hits;
             print_s [%sexp (winner : Winner.t)]))))
;;

let%expect_test "authored Goal.finish remains pending until the actual terminal winner" =
  List.iter Winner.[ Complete; Cancel ] ~f:run;
  [%expect
    {|
    Complete
    Cancel
  |}]
;;
