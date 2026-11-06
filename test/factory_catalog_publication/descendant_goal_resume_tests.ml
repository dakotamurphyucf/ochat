open! Core
open Agent_server_test_support
open Retry_support
module Birth = Descendant_goal_birth_tests
module Direct = Descendant_turn_adapter_tests
module Held = Descendant_held_input_tests
module Goal = Goal_service_tests
module Staging = Goal_resume_tests
module Runtime = Goal_resume_runtime_tests
module Service = Agent_server.Goal_run_service
module G = P.Goal
module GD = A.Goal_descendants
module Ledger = A.Inference_ledger
module Resume_result = Actor.Goal_resume_result

let within env phase f =
  try Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 45. f with
  | Eio.Time.Timeout -> failwith ("573 actual Resume timed out: " ^ phase)
;;

let checkpoint entry =
  Actor.with_moderator_checkpoint entry.Registry.actor (fun () -> Actor.state entry.actor)
  |> protocol_ok
;;

let raise_failure failure =
  Option.iter !failure ~f:(fun (exn, backtrace) ->
    Exn.raise_with_original_backtrace exn backtrace)
;;

let await_turns env child failure completed =
  within env "actual continued Run completion" (fun () ->
    let rec await () =
      raise_failure failure;
      let current = checkpoint child in
      let turns =
        P.Inference_query.Summary.turns (Ledger.summary current.inference_ledger)
      in
      [%test_eq: int64] 0L turns.failed;
      [%test_eq: int64] 0L turns.cancelled;
      [%test_eq: int64] 0L turns.interrupted;
      if Option.is_none current.active_operation && Int64.equal turns.completed completed
      then current
      else (
        Eio.Fiber.yield ();
        await ())
    in
    await ())
;;

let hold_and_unload daemon child =
  Goal_admitted_guard_tests.command
    daemon
    child
    ~key:"573-real-hold"
    (Hold (Goal.goal_id, Operator_pause))
  |> protocol_ok
  |> ignore;
  let held = checkpoint child in
  let attachment, _ =
    Actor.attach child.actor ~mode:Read_write ~subscribe:false |> protocol_ok
  in
  protect_cleanup
    ~finally:(fun () -> Actor.detach child.actor attachment.id |> protocol_ok)
    (fun () ->
       Actor.stop child.actor ~attachment_id:attachment.id ~mode:Graceful
       |> protocol_ok
       |> ignore;
       Owner.unload_and_wait child.runtime |> protocol_ok);
  assert (not (Owner.is_loaded child.runtime));
  let stopped = checkpoint child in
  (match stopped.lifecycle.desired, stopped.lifecycle.observed with
   | Stopped, Stopped -> ()
   | _ -> failwith "actual child runtime did not stop");
  let episode = G.episode (Goal.goal held) |> Option.value_exn in
  let admission = P.Goal_episode.admission episode in
  assert (P.Run.equal (Runtime.run held admission) (Runtime.run stopped admission));
  assert (G.equal (Goal.goal held) (Goal.goal stopped));
  assert (A.Goal_accounting.equal (Goal.accounting held) (Goal.accounting stopped));
  stopped
;;

let inherited_reader = {|<tool type="inherited" name="read_file"/>|}

let parent_reader =
  {|<tool name="read_file"><read id="data" path="${workspace}"/></tool>|}
;;

let inherited_prompt decision =
  let source = Runtime.prompt V1 decision in
  assert (String.is_substring source ~substring:parent_reader);
  String.substr_replace_all source ~pattern:parent_reader ~with_:inherited_reader
;;

let assert_inherited_reader env daemon parent child =
  let store = Daemon.store daemon in
  let reference = Option.value_exn (state child).spec.delegation in
  let record =
    Agent_store.Delegation_store.resolve
      (Agent_store.Session_store.delegations store)
      reference
    |> Source_preparation_tests.store_ok
  in
  assert (List.Assoc.mem record.admission.capability_pins "read_file" ~equal:String.equal);
  let artifacts =
    Agent_store.Prompt_artifact_store.create
      ~env
      ~root:
        (Agent_store.Data_root.prompt_artifacts_path
           (Agent_store.Session_store.data_root store))
    |> Source_preparation_tests.store_ok
  in
  let artifact =
    A.Generated_definition.load_artifact
      ~artifact_store:artifacts
      ~revision_id:reference.revision_id
      ~manifest_sha256:record.admission.manifest_sha256
    |> Result.map_error ~f:(List.map ~f:Chatmd_shell_spec.Diagnostic.to_string)
    |> function
    | Ok artifact -> artifact
    | Error errors -> raise_s [%sexp (errors : string list)]
  in
  assert (String.is_substring artifact.root_chatmd ~substring:inherited_reader);
  assert (not (String.is_substring artifact.root_chatmd ~substring:parent_reader));
  let actual_read entry =
    Owner.with_loaded_runtime entry.Registry.runtime (fun runtime ->
      let native = Option.value_exn runtime.A.Runtime_builder.native_runtime in
      let capabilities =
        Lazy.force native.capabilities
        |> Result.map_error ~f:(fun error -> error.Chat_response.Tool_capability.message)
        |> Result.ok_or_failwith
      in
      Ok
        (Chat_response.Tool_capability.references capabilities
         |> List.find_exn ~f:(fun reference -> String.equal reference.name "read_file")))
    |> protocol_ok
  in
  let selected = actual_read parent in
  let inherited = actual_read child in
  assert (P.Id.Capability.equal selected.id inherited.id);
  [%test_eq: string] selected.fingerprint inherited.fingerprint
;;

let with_episode ?test_root ?(decision = Runtime.Decision.Continue) ?(wrap_env = Fn.id) f =
  let observe = ref None in
  let failure = ref None in
  let child_sources =
    match decision with
    | Continue -> [ "child.chatmd", Birth.child_source ]
    | Wait | Finish ->
      [ "child.chatmd", inherited_prompt decision; Runtime.resume_context_file, "{}\n" ]
  in
  let inherited_tools =
    match decision with
    | Continue -> []
    | Wait | Finish -> [ "read_file", parent_reader ]
  in
  let budget =
    G.Budget.create
      ~admitted_turns:None
      ~admitted_attempts:(Some 5L)
      ~observed_tokens:None
      ~deadline:None
    |> protocol_ok
  in
  Birth.with_native_goal_child
    ?test_root
    ~timeout:150.
    ~wrap_env
    ~budget
    ~child_sources
    ~inherited_tools
    ~post_creation_stream:(fun _ ->
      (try Option.iter !observe ~f:(fun f -> f ()) with
       | exn -> failure := Some (exn, Stdlib.Printexc.get_raw_backtrace ()));
      completed_response "573 actual same-Run Goal continuation")
    (fun env _sw daemon parent child scope reference counters ->
       Direct.start_child daemon child;
       (match decision with
        | Continue -> ()
        | Wait | Finish -> assert_inherited_reader env daemon parent child);
       let seed =
         Held.Probe.create
           ~attempt_ceiling:5L
           ~consumer:Goal
           ~parent
           ~child
           ~scope
           ~reference
           ()
       in
       observe := Some (fun () -> Held.Probe.before_submit seed);
       let admission = Held.goal_call daemon child () |> protocol_ok |> Goal.activated in
       Held.await_completed_input env child seed;
       raise_failure failure;
       [%test_eq: int] 3 !(counters.providers);
       Direct.confirmed parent child scope reference ~turns:1L ~attempts:1L |> ignore;
       let old_runtime =
         Owner.with_loaded_runtime child.runtime (fun actual -> Ok actual) |> protocol_ok
       in
       let stopped = hold_and_unload daemon child in
       let _, selected = Held.run stopped admission in
       (match selected.lifecycle with
        | Admitted | Active -> ()
        | Waiting _ | Terminal _ -> failwith "actual old Workflow cannot continue");
       let source = selected.source in
       let evidence =
         G.Reason.of_string "573 actual resumed Goal completed" |> protocol_ok
       in
       let finish =
         P.Goal_achievement.Intent.create
           ~expected_goal_revision:(Int64.succ (G.revision (Goal.goal stopped)))
           ~evidence
           ~action:(Finish { terminal = Completed None; relinquish = [] })
         |> protocol_ok
         |> P.Goal_achievement.Intent.to_json
       in
       (match decision with
        | Continue -> ()
        | Wait | Finish ->
          let root =
            Option.value_exn (state parent).spec.workspace_instance.configured_root
          in
          Eio.Path.save
            ~create:(`Or_truncate 0o600)
            Eio.Path.(Eio.Stdenv.fs env / root / Runtime.resume_context_file)
            (Jsonaf.to_string
               (`Object
                   [ "run_id", P.Id.Run.to_json selected.id
                   ; "source", P.Run_source.to_json source
                   ; "finish", finish
                   ])
             ^ "\n"));
       observe := None;
       let live = ref true in
       let actor = Staging.guarded Staging.resume_principal live in
       let request = Staging.exact_request stopped ~key:"573-cold-resume" in
       let session =
         P.Session_ref.create
           ~server_id:(Agent_store.Session_store.server_id (Daemon.store daemon))
           ~session_id:stopped.identity.session_id
       in
       let call () =
         Service.resume_episode
           (Runtime.public daemon)
           ~entry:child
           ~actor
           ~session
           ~request
       in
       f
         env
         daemon
         parent
         child
         scope
         reference
         counters
         admission
         stopped
         old_runtime
         observe
         failure
         live
         request
         call)
;;

let%expect_test
    "scoped cold Goal Resume joins once and confirms same-Run Turn before provider"
  =
  with_episode
    (fun
        env
         _daemon
         parent
         child
         scope
         reference
         counters
         admission
         stopped
         old_runtime
         observe
         failure
         _live
         request
         call
       ->
       let observed = ref None in
       let parent_own = Goal.accounting (state parent) in
       observe
       := Some
            (fun () ->
              let current = state child in
              let operation = Option.value_exn current.active_operation in
              assert (P.Operation.equal_kind operation.kind (Turn Administrative));
              Runtime.assert_episode stopped current admission;
              let _, run = Held.run current admission in
              assert (
                List.exists run.owned_work ~f:(fun work ->
                  Held.has_operation work operation));
              let account = Goal.accounting current in
              [%test_eq: int64] 2L (A.Goal_accounting.admitted_turns account);
              [%test_eq: int64] 2L (A.Goal_accounting.admitted_attempts account);
              let running =
                Ledger.rows current.inference_ledger
                |> List.filter ~f:(fun row ->
                  match
                    Inference.Observation.Attempt_record.state (Ledger.Row.record row)
                  with
                  | Running -> true
                  | Prepared | Terminal _ | Interrupted _ -> false)
                |> function
                | [ row ] -> row
                | [] | _ :: _ :: _ ->
                  failwith "actual continued provider lacks one Running handle"
              in
              let handle = Ledger.Row.handle running in
              assert (
                List.exists (A.Goal_accounting.bindings account) ~f:(fun binding ->
                  Ledger.Handle.equal handle (A.Goal_accounting.Binding.handle binding)
                  && P.Id.Run.equal run.id (A.Goal_accounting.Binding.run_id binding)));
              Direct.confirmed parent child scope reference ~turns:2L ~attempts:2L
              |> ignore;
              [%test_eq: int64]
                4L
                (A.Goal_descendant_branch.confirmed_prefix
                   (Direct.branch parent scope reference));
              assert (A.Goal_accounting.equal parent_own (Goal.accounting (state parent)));
              let callback = Runtime.completed_resume current admission in
              (match callback.status with
               | Completed _ -> ()
               | Running | Failed _ | Interrupted _ ->
                 failwith "provider preceded joined Resume");
              observed := Some operation);
       let receipt, operation =
         within env "public cold Resume" call |> protocol_ok |> Runtime.started
       in
       Staging.assert_receipt request stopped receipt;
       let final = await_turns env child failure 2L in
       raise_failure failure;
       let actual = Option.value_exn !observed in
       assert (P.Id.Operation.equal operation.id actual.id);
       [%test_eq: int] operation.generation actual.generation;
       assert (P.Operation.equal_kind operation.kind actual.kind);
       Runtime.assert_started stopped final admission operation;
       [%test_eq: int] 4 !(counters.providers);
       [%test_eq: int]
         1
         (List.count final.moderator_executions ~f:(fun event ->
            P.Moderator_execution.equal_phase event.context.phase Session_resume));
       Owner.with_loaded_runtime child.runtime (fun runtime ->
         assert (not (phys_equal old_runtime runtime));
         Ok ())
       |> protocol_ok;
       let before_parent = checkpoint parent in
       let before_child = checkpoint child in
       let parent_frames = Source_preparation_tests.frames env parent in
       let child_frames = Source_preparation_tests.frames env child in
       (match within env "retained Resume" call |> protocol_ok with
        | Retained known -> assert (A.Goal_command_receipt.equal receipt known)
        | Resumed _ | Resume_failed _ | Rejected _ | Uncertain _ ->
          failwith "exact Resume replay reran continuation");
       Source_preparation_tests.assert_same before_parent parent;
       Source_preparation_tests.assert_same before_child child;
       [%test_eq: string list] parent_frames (Source_preparation_tests.frames env parent);
       [%test_eq: string list] child_frames (Source_preparation_tests.frames env child);
       [%test_eq: int] 4 !(counters.providers));
  print_endline
    "real cold Resume callback joins once; same Run owns confirmed Turn and Attempt; \
     replay is factual";
  [%expect
    {| real cold Resume callback joins once; same Run owns confirmed Turn and Attempt; replay is factual |}]
;;

let%expect_test
    "scoped cold Resume retains real Wait or settlement without reserving a Turn"
  =
  List.iter [ Runtime.Decision.Wait; Finish ] ~f:(fun decision ->
    with_episode
      ~decision
      (fun
          env
           _daemon
           parent
           child
           scope
           reference
           counters
           admission
           stopped
           _old_runtime
           _observe
           failure
           _live
           request
           call
         ->
         let before_branch = Direct.branch parent scope reference in
         let parent_own = Goal.accounting (state parent) in
         let result = within env "cold no-Turn Resume" call |> protocol_ok in
         raise_failure failure;
         let current = checkpoint child in
         Runtime.assert_episode stopped current admission;
         let callback = Runtime.completed_resume current admission in
         (match callback.status with
          | Completed _ -> ()
          | Running | Failed _ | Interrupted _ -> failwith "no-Turn callback did not join");
         let receipt =
           match decision, result with
           | Wait, Resume_result.Resumed { receipt; continuation = Waiting wake } ->
             let _, run = Held.run current admission in
             (match run.lifecycle with
              | Waiting actual -> assert (P.Run_wake.equal wake actual)
              | Admitted | Active | Terminal _ -> failwith "actual Run.wait was discarded");
             assert (P.Id.Run.equal run.id wake.run_id);
             assert (P.Run_source.equal run.source wake.source);
             receipt
           | Finish, Resumed { receipt; continuation = Settled settlement } ->
             let episode = G.episode (Goal.goal stopped) |> Option.value_exn in
             let _, run = Held.run current admission in
             P.Goal_episode.validate_terminal
               episode
               ~run
               ~receipt:(P.Goal_episode.Settlement.receipt settlement)
             |> protocol_ok;
             (match G.state (Goal.goal current) with
              | Closed (Achieved _) -> ()
              | Open _ | Closed _ -> failwith "actual Goal.finish was discarded");
             receipt
           | ( (Wait | Finish | Continue)
             , (Resumed _ | Retained _ | Resume_failed _ | Rejected _ | Uncertain _) ) ->
             raise_s
               [%sexp "actual no-Turn result changed", (decision : Runtime.Decision.t)]
         in
         Staging.assert_receipt request stopped receipt;
         assert (Option.is_none current.active_operation);
         let _, previous_run = Held.run stopped admission in
         let _, run = Held.run current admission in
         assert (
           List.equal
             P.Run_work.equal
             (Runtime.operation_work previous_run)
             (Runtime.operation_work run));
         assert (
           A.Goal_accounting.equal (Goal.accounting stopped) (Goal.accounting current));
         let marks = Direct.marks child in
         [%test_eq: int64] 1L marks.turn;
         [%test_eq: int64] 1L marks.attempt;
         assert (GD.Branch.equal before_branch (Direct.branch parent scope reference));
         assert (A.Goal_accounting.equal parent_own (Goal.accounting (state parent)));
         [%test_eq: int] 3 !(counters.providers);
         let parent_before = checkpoint parent in
         let child_before = checkpoint child in
         (match call () |> protocol_ok with
          | Retained known -> assert (A.Goal_command_receipt.equal receipt known)
          | Resumed _ | Resume_failed _ | Rejected _ | Uncertain _ ->
            failwith "no-Turn command replay acquired runtime again");
         Source_preparation_tests.assert_same parent_before parent;
         Source_preparation_tests.assert_same child_before child;
         [%test_eq: int] 3 !(counters.providers)));
  print_endline
    "real cold callback Wait and Goal.finish preserve their outcomes with no inherited \
     Turn";
  [%expect
    {| real cold callback Wait and Goal.finish preserve their outcomes with no inherited Turn |}]
;;

let%expect_test
    "actual Resume source expires during ancestor Reserve and closes exact pending"
  =
  let module Barrier = Descendant_history_adapter_tests.Root_barrier in
  let barrier = Barrier.create () in
  with_episode
    ~wrap_env:(Barrier.wrap barrier)
    (fun
        env
         daemon
         parent
         child
         scope
         reference
         counters
         admission
         stopped
         _old_runtime
         observe
         failure
         live
         request
         call
       ->
       let before_branch = Direct.branch parent scope reference in
       let accounting = Option.value_exn (state child).goal_descendants in
       barrier.target
       <- Some
            ( (state parent).identity.session_id
            , scope
            , Barrier.Kind.Reserve (GD.session accounting) );
       let pending = ref None in
       let observed_child = ref None in
       let callback () = failwith "retired Resume source reached provider" in
       observe := Some callback;
       Eio.Switch.run (fun sw ->
         protect_cleanup
           ~finally:(fun () ->
             Barrier.release barrier;
             Option.iter !pending ~f:(fun promise ->
               within env "join retired Resume caller" (fun () ->
                 Eio.Promise.await_exn promise)
               |> ignore))
           (fun () ->
              let response = Eio.Fiber.fork_promise ~sw call in
              pending := Some response;
              within env "real parent Resume Reserve writer" (fun () ->
                Eio.Promise.await barrier.entered);
              let before = checkpoint child in
              observed_child := Some before;
              live := false;
              Barrier.release barrier;
              let result =
                within env "expired original Resume response" (fun () ->
                  Eio.Promise.await_exn response)
                |> protocol_ok
              in
              (match result with
               | Resume_result.Resume_failed { receipt; error } ->
                 Staging.assert_receipt request stopped receipt;
                 [%test_eq: P.Error.code] Permission_denied error.code
               | Resumed _ | Retained _ | Rejected _ | Uncertain _ ->
                 failwith "expired actual source lost known Resume receipt");
              raise_failure failure));
       let root, closed =
         within env "actual Resume no-writer factual close" (fun () ->
           let rec await () =
             let branch = Direct.branch parent scope reference in
             match GD.Branch.pending branch, GD.Branch.last branch with
             | None, Some (No_writer_closed closed) -> branch, closed
             | (Some _ | None), (None | Some (Included _ | No_writer_closed _)) ->
               Eio.Fiber.yield ();
               await ()
           in
           await ())
       in
       [%test_eq: int64]
         (GD.Branch.confirmed_prefix before_branch)
         (GD.Branch.confirmed_prefix root);
       [%test_eq: int64]
         (Int64.succ (GD.Branch.last_nonce before_branch))
         (GD.Branch.last_nonce root);
       assert (
         A.Goal_descendant_occurrence.equal
           (Option.value_exn barrier.occurrence)
           (A.Goal_descendant_pending.occurrence closed));
       let before = Option.value_exn !observed_child in
       let after = checkpoint child in
       assert (
         List.equal
           P.History.equal_entry
           before.conversation.canonical_history
           after.conversation.canonical_history);
       assert (A.Goal_accounting.equal (Goal.accounting before) (Goal.accounting after));
       assert (Option.is_none after.active_operation);
       let _, previous_run = Held.run before admission in
       let _, current_run = Held.run after admission in
       assert (P.Run_source.equal previous_run.source current_run.source);
       assert (
         List.equal
           P.Run_work.equal
           (Runtime.operation_work previous_run)
           (Runtime.operation_work current_run));
       let marks = Direct.marks child in
       [%test_eq: int64] 1L marks.turn;
       [%test_eq: int64] 1L marks.attempt;
       [%test_eq: int] 3 !(counters.providers);
       ignore daemon);
  print_endline
    "known Resume receipt survives original retirement; exact ancestor reservation \
     closes without child Turn";
  [%expect
    {| known Resume receipt survives original retirement; exact ancestor reservation closes without child Turn |}]
;;
