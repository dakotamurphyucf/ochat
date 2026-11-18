open! Core
open Agent_server_test_support
open Retry_support
module Service = Agent_server.Goal_run_service
module Goal = Goal_service_tests
module G = P.Goal

let prompt_text =
  {|<developer>Actual cold legacy Goal input owner.</developer>
<script id="goal_owner" language="chatml" kind="moderator">
let initial_state = 0
let on_event ctx state event = match event with
| `Session_start -> Task.pure(state)
| `Session_resume -> Task.pure(state + 1)
| _ -> Task.pure(state)
</script>|}
;;

let checkpoint (current : A.Session_state.t) =
  let snapshot =
    A.Moderator_checkpoint.decode current.moderator |> protocol_ok |> Option.value_exn
  in
  Session.Snapshot.to_jsonaf snapshot.current_state
;;

let completed_resume (current : A.Session_state.t) =
  match
    List.filter current.moderator_executions ~f:(fun receipt ->
      P.Moderator_execution.equal_phase receipt.context.phase Session_resume)
  with
  | [ resume ] ->
    (match resume.status with
     | Completed _ -> resume
     | Running | Failed _ | Interrupted _ ->
       raise_s
         [%sexp
           "actual cold Goal resume is incomplete", (resume : P.Moderator_execution.t)])
  | _ -> failwith "cold Goal input did not run exactly one actual Session_resume"
;;

let%expect_test "public Goal input reconstructs cold legacy once before its first effect" =
  let selected = ref None in
  let first_effect_checked = ref false in
  let counters : Retry_support.counters =
    { inference_ports = ref 0; providers = ref 0; transcripts = ref [] }
  in
  let base =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs ->
      let entry = Option.value_exn !selected in
      let current = state entry in
      let active_goal = Goal.goal current in
      assert (G.State.equal (Open Enabled) (G.state active_goal));
      let episode = G.episode active_goal |> Option.value_exn in
      let admission = P.Goal_episode.admission episode in
      let index = Option.value_exn current.run_state in
      let run = A.Run_state.find index admission.run_id |> Option.value_exn in
      let link = A.Run_state.goal_link index run.id |> Option.value_exn in
      assert (P.Id.Goal.equal Goal.goal_id (A.Run_goal_link.goal_id link));
      assert (P.Goal_episode.equal episode (A.Run_goal_link.episode link));
      assert (P.Goal_episode.matches_run episode run);
      assert (P.Run_source.equal (G.source active_goal) run.source);
      let resume = completed_resume current in
      assert (P.Invocation.equal_observer run.source.observer resume.context.source);
      [%test_eq: int] run.source.generation resume.context.generation;
      assert (
        Jsonaf.exactly_equal (Session.Snapshot.to_jsonaf (Int 1)) (checkpoint current));
      assert (Owner.is_loaded entry.runtime);
      first_effect_checked := true;
      Int.incr counters.providers;
      let transcript =
        `Array (List.map inputs ~f:Res.Item.jsonaf_of_t) |> Jsonaf.to_string
      in
      counters.transcripts := !(counters.transcripts) @ [ transcript ];
      assert (String.is_substring transcript ~substring:"542 cold Goal input");
      completed_response "542 cold Goal complete")
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
    (fun env _sw _root daemon ->
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
         Source.with_loaded_entry
           daemon
           (fun ~entry ~runtime:initial_runtime ~revision:_ ->
              selected := Some entry;
              let session, created = Goal.create_goal daemon entry in
              let old_startup =
                match
                  List.filter created.moderator_executions ~f:(fun receipt ->
                    P.Moderator_execution.equal_phase receipt.context.phase Session_start)
                with
                | [ startup ] -> startup
                | _ -> failwith "cold Goal fixture has no unique authentic Session_start"
              in
              (match old_startup.status with
               | Completed _ -> ()
               | Running | Failed _ | Interrupted _ ->
                 failwith "initial legacy startup is incomplete");
              assert (
                Jsonaf.exactly_equal
                  (Session.Snapshot.to_jsonaf (Int 0))
                  (checkpoint created));
              [%test_eq: int] 0 !(counters.providers);
              Owner.unload_and_wait entry.runtime |> protocol_ok;
              assert (not (Owner.is_loaded entry.runtime));
              let before = state entry in
              assert (Option.is_none before.active_operation);
              assert (Option.is_none (G.episode (Goal.goal before)));
              assert (G.equal (Goal.goal created) (Goal.goal before));
              let request =
                A.Run_admission_request.Headless.create
                  ~session_id:before.identity.session_id
                  ~generation:before.identity.generation
                  ~expected_revision:before.counters.revision
                  ~mode:Workflow
                  ~input:
                    (User_submission
                       { kind = Plain_text
                       ; text = "542 cold Goal input"
                       ; attachments = []
                       })
                  ~key:
                    (P.Idempotency_key.of_string "goal-public-cold-input" |> protocol_ok)
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
              let admission = call () |> protocol_ok |> Goal.activated in
              let final = Goal.await_input env entry counters in
              assert !first_effect_checked;
              [%test_eq: int] 1 !(counters.providers);
              assert (
                List.exists
                  final.moderator_executions
                  ~f:(P.Moderator_execution.equal old_startup));
              [%test_eq: int]
                1
                (List.count final.moderator_executions ~f:(fun receipt ->
                   P.Moderator_execution.equal_phase receipt.context.phase Session_start));
              let resume = completed_resume final in
              assert (
                Jsonaf.exactly_equal
                  (Session.Snapshot.to_jsonaf (Int 1))
                  (checkpoint final));
              let index = Option.value_exn final.run_state in
              let run = A.Run_state.find index admission.run_id |> Option.value_exn in
              let episode = G.episode (Goal.goal final) |> Option.value_exn in
              assert (P.Run_receipt.equal admission (P.Goal_episode.admission episode));
              assert (P.Goal_episode.matches_run episode run);
              let link = A.Run_state.goal_link index run.id |> Option.value_exn in
              assert (P.Goal_episode.equal episode (A.Run_goal_link.episode link));
              [%test_eq: string]
                (A.Goal_episode_admission.digest
                   (A.Goal_activation_request.intent request))
                (A.Run_goal_link.intent_sha256 link);
              let account = Goal.accounting final in
              [%test_eq: int64] 1L (A.Goal_accounting.admitted_turns account);
              [%test_eq: int64] 1L (A.Goal_accounting.admitted_attempts account);
              [%test_eq: int] 1 (List.length (A.Goal_accounting.bindings account));
              Owner.with_loaded_runtime entry.runtime (fun actual ->
                assert (not (phys_equal initial_runtime actual));
                Ok ())
              |> protocol_ok;
              let frames = Existing.frames env entry in
              let counts = !(counters.providers), !(counters.inference_ports) in
              (match call () |> protocol_ok with
               | Retained retained -> assert (P.Run_receipt.equal admission retained)
               | Attempted _ -> failwith "cold Goal replay acquired another preparation");
              Existing.assert_same final entry;
              [%test_eq: string list] frames (Existing.frames env entry);
              [%test_eq: int * int]
                counts
                (!(counters.providers), !(counters.inference_ports));
              assert (P.Moderator_execution.equal resume (completed_resume (state entry)));
              print_endline
                "actual Goal Create precedes cold unload; one Session_resume and episode \
                 precede input effects";
              print_endline
                "original startup survives; retained replay reconstructs nothing and \
                 adds no charge")));
  [%expect
    {|
    actual Goal Create precedes cold unload; one Session_resume and episode precede input effects
    original startup survives; retained replay reconstructs nothing and adds no charge
  |}]
;;
