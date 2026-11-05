open! Core
open Agent_server_test_support
open Retry_support
module Service = Agent_server.Goal_run_service
module G = P.Goal
module Catalog = A.Goal_catalog
module Accounting = A.Goal_accounting
module Ledger = A.Inference_ledger
module Ordinary = Agent_server.Run_start_service

let ordinary daemon entry request =
  let service =
    Ordinary.create
      ~server_id:(Agent_store.Session_store.server_id (Daemon.store daemon))
      ~authorize:(fun _ -> authorize)
  in
  Ordinary.start_headless_command
    service
    ~actor:original
    ~entry
    ~request
    ~command_audit:None
;;

let goal_id = P.Id.Goal.of_string "goal_public_episode" |> protocol_ok

let goal current =
  Catalog.find (Option.value_exn current.A.Session_state.goal_catalog) goal_id
  |> Option.value_exn
;;

let accounting current =
  A.Goal_accounts.find (Option.value_exn current.A.Session_state.goal_accounts) goal_id
  |> Option.value_exn
  |> fun document ->
  A.Goal_accounting_document.resolve
    document
    ~goal:(goal current)
    ~ledger:current.inference_ledger
  |> protocol_ok
;;

let create_goal daemon entry =
  let before = state entry in
  let catalog = Option.value before.goal_catalog ~default:Catalog.empty in
  let _, source = A.Goal_catalog_source.capture before |> protocol_ok in
  let request =
    A.Goal_command.create
      ~session_id:before.identity.session_id
      ~generation:before.identity.generation
      ~source:(Some source)
      ~expected_catalog_revision:(Catalog.revision catalog)
      ~expected_goal_revision:None
      ~key:(P.Idempotency_key.of_string "goal-public-create" |> protocol_ok)
      ~mutation:
        (Create
           { id = goal_id
           ; objective =
               G.Objective.of_string "Qualify the actual public episode" |> protocol_ok
           ; budget = G.Budget.unlimited
           })
    |> protocol_ok
  in
  let session =
    P.Session_ref.create
      ~server_id:(Agent_store.Session_store.server_id (Daemon.store daemon))
      ~session_id:before.identity.session_id
  in
  let receipt =
    Actor.goal_command entry.Registry.actor ~actor:original ~authorize ~session ~request
    |> protocol_ok
  in
  let current = state entry in
  let created = goal current in
  assert (G.State.equal (Open (Held Source_review)) (G.state created));
  assert (Option.is_none (G.episode created));
  assert (P.Run_source.equal source (G.source created));
  assert (P.Id.Goal.equal goal_id receipt.goal_id);
  [%test_eq: int64] (G.revision created) receipt.goal_revision;
  [%test_eq: int64]
    (Catalog.revision (Option.value_exn current.goal_catalog))
    receipt.catalog_revision;
  let account = accounting current in
  [%test_eq: int64] 0L (Accounting.admitted_attempts account);
  [%test_eq: int64] 0L (Accounting.admitted_turns account);
  assert (List.is_empty (Accounting.bindings account));
  session, current
;;

let activated = function
  | Service.Outcome.Retained _ -> failwith "fresh Goal episode returned retained DATA"
  | Attempted attempt ->
    (match Service.Attempt.result attempt |> protocol_ok with
     | Activated admission ->
       assert (Option.is_none (Service.Attempt.no_effects attempt));
       admission
     | Admission_uncertain error | Activation_failed { error; _ } ->
       raise_s [%sexp "Goal activation failed", (error : P.Error.t)]
     | Retained _ -> failwith "fresh Goal episode returned retained DATA")
;;

let await_input env entry counters =
  let rec await () =
    let current =
      Actor.with_moderator_checkpoint entry.Registry.actor (fun () ->
        Actor.state entry.actor)
      |> protocol_ok
    in
    let summary = Ledger.summary current.inference_ledger in
    let turns = P.Inference_query.Summary.turns summary in
    if Int64.(turns.failed > 0L || turns.cancelled > 0L || turns.interrupted > 0L)
    then
      raise_s
        [%sexp
          "Goal input host turn ended adversely", (summary : P.Inference_query.Summary.t)];
    List.iter (Ledger.rows current.inference_ledger) ~f:(fun row ->
      let record = Ledger.Row.record row in
      match Inference.Observation.Attempt_record.state record with
      | Prepared | Running -> ()
      | Interrupted _ ->
        raise_s
          [%sexp
            "Goal inference interrupted"
          , (Inference.Observation.Attempt_record.to_json record : Jsonaf.t)]
      | Terminal terminal ->
        (match Inference.Event.Terminal.outcome terminal with
         | Completed -> ()
         | Refused | Incomplete _ | Failed _ ->
           raise_s
             [%sexp
               "Goal inference ended adversely", (terminal : Inference.Event.Terminal.t)]));
    List.iter
      (Option.value_map current.run_state ~default:[] ~f:A.Run_state.runs)
      ~f:(fun run ->
        match run.P.Run.lifecycle with
        | Admitted | Active | Waiting _ -> ()
        | Terminal _ ->
          raise_s
            [%sexp
              "Goal Workflow ended before the expected live episode", (run : P.Run.t)]);
    let callbacks_complete =
      List.for_all current.moderator_executions ~f:(fun receipt ->
        match receipt.P.Moderator_execution.status with
        | Completed _ -> true
        | Running -> false
        | Failed _ | Interrupted _ ->
          raise_s
            [%sexp
              "Goal moderator callback ended adversely"
            , (receipt : P.Moderator_execution.t)])
    in
    if !(counters.providers) > 1 then failwith "Goal input dispatched more than once";
    if
      !(counters.providers) = 1
      && Option.is_none current.active_operation
      && Int64.equal turns.completed 1L
      && callbacks_complete
    then current
    else (
      Eio.Fiber.yield ();
      await ())
  in
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. await
;;

let%expect_test "actual Goal create starts one public episode and retains its association"
  =
  with_fixture (fun env _sw daemon entry runtime _ counters ->
    let session, before = create_goal daemon entry in
    let created = goal before in
    let ports = !(counters.inference_ports) in
    let startup = before.moderator_executions in
    let request =
      A.Run_admission_request.Headless.create
        ~session_id:before.identity.session_id
        ~generation:before.identity.generation
        ~expected_revision:before.counters.revision
        ~mode:Workflow
        ~input:
          (User_submission
             { kind = Plain_text
             ; text = "542 actual Goal episode input"
             ; attachments = []
             })
        ~key:(P.Idempotency_key.of_string "goal-public-episode" |> protocol_ok)
      |> protocol_ok
    in
    let activation ~goal_id ~expected_goal_revision =
      A.Goal_activation_request.create
        ~goal_id
        ~expected_goal_revision
        ~expected_catalog_revision:
          (Catalog.revision (Option.value_exn before.goal_catalog))
        ~request
      |> protocol_ok
    in
    let requested = activation ~goal_id ~expected_goal_revision:(G.revision created) in
    let public =
      Service.create ~factory:(Daemon.factory daemon) ~authorize:(fun _ -> authorize)
    in
    let call request =
      Service.start_episode public ~entry ~actor:original ~session ~request
    in
    let admission = call requested |> protocol_ok |> activated in
    let final = await_input env entry counters in
    let index = Option.value_exn final.run_state in
    let run = A.Run_state.find index admission.run_id |> Option.value_exn in
    let active_goal = goal final in
    let episode = G.episode active_goal |> Option.value_exn in
    let link = A.Run_state.goal_link index run.id |> Option.value_exn in
    assert (G.State.equal (Open Enabled) (G.state active_goal));
    assert (P.Run.Mode.equal Workflow run.mode);
    assert (P.Run_source.equal (G.source created) run.source);
    assert (P.Goal_episode.matches_run episode run);
    assert (P.Run_receipt.equal admission (P.Goal_episode.admission episode));
    assert (P.Id.Goal.equal goal_id (A.Run_goal_link.goal_id link));
    assert (P.Goal_episode.equal episode (A.Run_goal_link.episode link));
    [%test_eq: string]
      (A.Goal_episode_admission.digest (A.Goal_activation_request.intent requested))
      (A.Run_goal_link.intent_sha256 link);
    [%test_eq: int]
      1
      (List.count (A.Run_state.receipts index) ~f:(fun receipt ->
         P.Id.Run.equal receipt.P.Run_receipt.run_id run.id
         && P.Run_receipt.Kind.equal Admission receipt.kind));
    let operation =
      List.filter_map run.owned_work ~f:(fun work ->
        match work.P.Run_work.key with
        | Operation id -> Some id
        | Retained _ -> None)
      |> function
      | [ operation ] -> operation
      | _ -> failwith "Goal input did not retain exactly one actual root operation"
    in
    let account = accounting final in
    let turns = P.Inference_query.Summary.turns (Ledger.summary final.inference_ledger) in
    [%test_eq: int64] 1L turns.completed;
    [%test_eq: int64] 0L turns.pending;
    [%test_eq: int64] 0L turns.failed;
    [%test_eq: int64] 0L turns.cancelled;
    [%test_eq: int64] 0L turns.interrupted;
    [%test_eq: int64] 1L (Accounting.admitted_turns account);
    [%test_eq: int64] 1L (Accounting.admitted_attempts account);
    [%test_eq: int64] 0L (Accounting.untracked_turns account);
    [%test_eq: int64] 0L (Accounting.untracked_attempts account);
    let binding = List.hd_exn (Accounting.bindings account) in
    [%test_eq: int] 1 (List.length (Accounting.bindings account));
    assert (P.Id.Run.equal run.id (Accounting.Binding.run_id binding));
    assert (P.Run_source.equal run.source (Accounting.Binding.source binding));
    let handle = Accounting.Binding.handle binding in
    [%test_eq: P.Id.Operation.t option]
      (Some operation)
      (Ledger.Handle.operation_id handle);
    let row =
      Ledger.find final.inference_ledger ~ordinal:(Ledger.Handle.ordinal handle)
      |> Option.value_exn
    in
    assert (Ledger.Handle.equal handle (Ledger.Row.handle row));
    (match Inference.Observation.Attempt_record.state (Ledger.Row.record row) with
     | Terminal _ -> ()
     | Prepared | Running | Interrupted _ -> failwith "actual inference did not complete");
    [%test_eq: int64]
      1L
      (P.Inference_query.Summary.retained_attempts
         (Ledger.summary final.inference_ledger));
    [%test_eq: int] 1 !(counters.providers);
    [%test_eq: int] (ports + 1) !(counters.inference_ports);
    assert (
      String.is_substring
        (List.hd_exn !(counters.transcripts))
        ~substring:"542 actual Goal episode input");
    List.iter startup ~f:(fun previous ->
      assert (
        List.exists final.moderator_executions ~f:(P.Moderator_execution.equal previous)));
    Owner.with_loaded_runtime entry.runtime (fun actual ->
      assert (phys_equal runtime actual);
      Ok ())
    |> protocol_ok;
    let frames = Existing.frames env entry in
    let counts = !(counters.providers), !(counters.inference_ports) in
    (match call requested |> protocol_ok with
     | Retained retained -> assert (P.Run_receipt.equal admission retained)
     | Attempted _ -> failwith "Goal replay acquired another source attempt");
    Existing.assert_same final entry;
    [%test_eq: string list] frames (Existing.frames env entry);
    [%test_eq: int * int] counts (!(counters.providers), !(counters.inference_ports));
    (match ordinary daemon entry request with
     | Rejected error ->
       [%test_eq: P.Error.code] Conflict error.code;
       assert (not error.retryable)
     | Admitted _ | Uncertain _ ->
       failwith "ordinary same-key command removed the original Goal association");
    Existing.assert_same final entry;
    [%test_eq: string list] frames (Existing.frames env entry);
    [%test_eq: int * int] counts (!(counters.providers), !(counters.inference_ports));
    List.iter
      [ activation ~goal_id ~expected_goal_revision:(Int64.succ (G.revision created))
      ; activation
          ~goal_id:(P.Id.Goal.of_string "goal_other_association" |> protocol_ok)
          ~expected_goal_revision:(G.revision created)
      ]
      ~f:(fun changed ->
        (match call changed with
         | Error error ->
           [%test_eq: P.Error.code] Conflict error.code;
           assert (not error.retryable)
         | Ok _ -> failwith "same Run key changed its original Goal association");
        Existing.assert_same final entry;
        [%test_eq: string list] frames (Existing.frames env entry);
        [%test_eq: int * int] counts (!(counters.providers), !(counters.inference_ports)));
    print_endline
      "actual catalog Create and public input handoff bind one immutable Goal episode";
    print_endline
      "one canonical inference and host turn; retained replay and changed association \
       add nothing");
  [%expect
    {|
    actual catalog Create and public input handoff bind one immutable Goal episode
    one canonical inference and host turn; retained replay and changed association add nothing
    |}]
;;

let%expect_test "ordinary same-key admission cannot acquire a Goal association" =
  with_fixture (fun env _sw daemon entry _runtime _revision counters ->
    let session, before = create_goal daemon entry in
    let request =
      A.Run_admission_request.Headless.create
        ~session_id:before.identity.session_id
        ~generation:before.identity.generation
        ~expected_revision:before.counters.revision
        ~mode:Workflow
        ~input:
          (User_submission
             { kind = Plain_text; text = "542 ordinary original input"; attachments = [] })
        ~key:(P.Idempotency_key.of_string "goal-must-not-be-added" |> protocol_ok)
      |> protocol_ok
    in
    let goal_request =
      A.Goal_activation_request.create
        ~goal_id
        ~expected_goal_revision:(G.revision (goal before))
        ~expected_catalog_revision:
          (Catalog.revision (Option.value_exn before.goal_catalog))
        ~request
      |> protocol_ok
    in
    let admission =
      match ordinary daemon entry request with
      | Admitted admission -> admission
      | Rejected error | Uncertain error -> raise_s [%sexp (error : P.Error.t)]
    in
    let final = await_input env entry counters in
    let index = Option.value_exn final.run_state in
    assert (Option.is_none (A.Run_state.goal_link index admission.run_id));
    assert (Option.is_none (G.episode (goal final)));
    let frames = Existing.frames env entry in
    let counts = !(counters.providers), !(counters.inference_ports) in
    (match ordinary daemon entry request with
     | Admitted retained -> assert (P.Run_receipt.equal admission retained)
     | Rejected error | Uncertain error -> raise_s [%sexp (error : P.Error.t)]);
    Existing.assert_same final entry;
    [%test_eq: string list] frames (Existing.frames env entry);
    [%test_eq: int * int] counts (!(counters.providers), !(counters.inference_ports));
    let service =
      Service.create ~factory:(Daemon.factory daemon) ~authorize:(fun _ -> authorize)
    in
    (match
       Service.start_episode service ~entry ~actor:original ~session ~request:goal_request
     with
     | Error error ->
       [%test_eq: P.Error.code] Conflict error.code;
       assert (not error.retryable)
     | Ok _ -> failwith "same ordinary Run key acquired a new Goal association");
    Existing.assert_same final entry;
    [%test_eq: string list] frames (Existing.frames env entry);
    [%test_eq: int * int] counts (!(counters.providers), !(counters.inference_ports));
    print_endline
      "ordinary retry retains its original receipt; adding Goal intent conflicts without \
       writes");
  [%expect
    {| ordinary retry retains its original receipt; adding Goal intent conflicts without writes |}]
;;

let%expect_test "a fresh public Goal episode preserves its actual terminal predecessor" =
  let entered, entered_resolver = Eio.Promise.create () in
  let release, release_resolver = Eio.Promise.create () in
  let provider_calls = ref 0 in
  let release_provider () =
    if not (Eio.Promise.is_resolved release) then Eio.Promise.resolve release_resolver ()
  in
  with_fixture
    ~post_stream:(fun _ ->
      Int.incr provider_calls;
      if !provider_calls = 1
      then (
        Eio.Promise.resolve entered_resolver ();
        Eio.Promise.await release);
      completed_response "542 second episode complete")
    (fun env _sw daemon entry _runtime _revision counters ->
       protect_cleanup ~finally:release_provider (fun () ->
         let session, _ = create_goal daemon entry in
         let writer, _ =
           Actor.attach entry.actor ~mode:Read_write ~subscribe:false |> protocol_ok
         in
         let public =
           Service.create ~factory:(Daemon.factory daemon) ~authorize:(fun _ -> authorize)
         in
         let request current key =
           let input =
             A.Run_admission_request.Headless.create
               ~session_id:current.A.Session_state.identity.session_id
               ~generation:current.identity.generation
               ~expected_revision:current.counters.revision
               ~mode:Workflow
               ~input:
                 (User_submission { kind = Plain_text; text = key; attachments = [] })
               ~key:(P.Idempotency_key.of_string key |> protocol_ok)
             |> protocol_ok
           in
           A.Goal_activation_request.create
             ~goal_id
             ~expected_goal_revision:(G.revision (goal current))
             ~expected_catalog_revision:
               (Catalog.revision (Option.value_exn current.goal_catalog))
             ~request:input
           |> protocol_ok
         in
         let start request =
           Service.start_episode public ~entry ~actor:original ~session ~request
         in
         let snapshot () =
           Actor.with_moderator_checkpoint entry.actor (fun () -> Actor.state entry.actor)
           |> protocol_ok
         in
         let await_state ready =
           let rec await () =
             let current = snapshot () in
             if ready current
             then current
             else (
               Eio.Fiber.yield ();
               await ())
           in
           Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. await
         in
         let find_run current (admission : P.Run_receipt.t) =
           A.Run_state.find
             (Option.value_exn current.A.Session_state.run_state)
             admission.run_id
           |> Option.value_exn
         in
         let root_operation (run : P.Run.t) =
           List.filter_map run.owned_work ~f:(fun work ->
             match work.P.Run_work.key with
             | Operation id -> Some id
             | Retained _ -> None)
           |> function
           | [ operation ] -> operation
           | _ -> failwith "Goal episode did not own exactly one actual root operation"
         in
         let first_request = request (state entry) "goal-public-first-episode" in
         let first = start first_request |> protocol_ok |> activated in
         Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
           Eio.Promise.await entered);
         let running = snapshot () in
         let first_operation = root_operation (find_run running first) in
         let frames = Existing.frames env entry in
         let counts = !(counters.providers), !(counters.inference_ports) in
         (match start (request running "goal-public-live-predecessor") with
          | Error error ->
            [%test_eq: P.Error.code] Conflict error.code;
            assert (not error.retryable)
          | Ok _ -> failwith "fresh Goal episode replaced its actual live predecessor");
         Existing.assert_same running entry;
         [%test_eq: string list] frames (Existing.frames env entry);
         [%test_eq: int * int] counts (!(counters.providers), !(counters.inference_ports));
         Actor.cancel_operation
           entry.actor
           ~attachment_id:writer.id
           ~operation_id:first_operation
         |> protocol_ok
         |> ignore;
         release_provider ();
         let cancelled =
           await_state (fun current ->
             let run = find_run current first in
             let ended =
               List.for_all (Ledger.rows current.inference_ledger) ~f:(fun row ->
                 match
                   Inference.Observation.Attempt_record.state (Ledger.Row.record row)
                 with
                 | Prepared | Running -> false
                 | Interrupted _ | Terminal _ -> true)
             in
             match run.lifecycle with
             | Terminal Cancelled -> Option.is_none current.active_operation && ended
             | Terminal _ ->
               raise_s [%sexp "unexpected first episode outcome", (run : P.Run.t)]
             | Admitted | Active | Waiting _ -> false)
         in
         let old_index = Option.value_exn cancelled.run_state in
         let old_run = find_run cancelled first in
         let old_link =
           A.Run_state.goal_link old_index first.run_id |> Option.value_exn
         in
         let old_episode = G.episode (goal cancelled) |> Option.value_exn in
         let terminal =
           List.filter (A.Run_state.receipts old_index) ~f:(fun receipt ->
             P.Id.Run.equal receipt.P.Run_receipt.run_id first.run_id
             && P.Run_receipt.Kind.equal Terminal receipt.kind
             && Int64.equal receipt.run_revision old_run.revision)
           |> function
           | [ receipt ] -> receipt
           | _ -> failwith "cancelled episode lacks its exact unique Terminal receipt"
         in
         let predecessor =
           P.Goal_episode.Settlement.create old_episode ~run:old_run ~receipt:terminal
           |> protocol_ok
         in
         let old_account = accounting cancelled in
         [%test_eq: int64] 1L (Accounting.admitted_attempts old_account);
         [%test_eq: int64] 1L (Accounting.admitted_turns old_account);
         let old_binding =
           match Accounting.bindings old_account with
           | [ binding ] -> binding
           | _ -> failwith "first episode lacks its exact unique inference binding"
         in
         let old_row =
           Ledger.find
             cancelled.inference_ledger
             ~ordinal:(Ledger.Handle.ordinal (Accounting.Binding.handle old_binding))
           |> Option.value_exn
         in
         let next_request = request cancelled "goal-public-second-episode" in
         let second = start next_request |> protocol_ok |> activated in
         assert (not (P.Id.Run.equal first.run_id second.run_id));
         let completed =
           await_state (fun current ->
             let run = find_run current second in
             let operation = root_operation run in
             let succeeded =
               List.exists run.terminal_work ~f:(fun work ->
                 P.Run_work.Key.equal
                   (Operation operation)
                   work.P.Run_work.Terminal.work.key
                 && P.Run_work.Terminal.equal_outcome Succeeded work.outcome)
             in
             let account = accounting current in
             let completed_row =
               List.exists (Accounting.bindings account) ~f:(fun binding ->
                 P.Id.Run.equal second.run_id (Accounting.Binding.run_id binding)
                 &&
                 match
                   Ledger.find
                     current.inference_ledger
                     ~ordinal:(Ledger.Handle.ordinal (Accounting.Binding.handle binding))
                 with
                 | None -> false
                 | Some row ->
                   (match
                      Inference.Observation.Attempt_record.state (Ledger.Row.record row)
                    with
                    | Terminal terminal ->
                      Inference.Event.Terminal.equal_outcome
                        Completed
                        (Inference.Event.Terminal.outcome terminal)
                    | Prepared | Running | Interrupted _ -> false))
             in
             Option.is_none current.active_operation && succeeded && completed_row)
         in
         let index = Option.value_exn completed.run_state in
         let current_goal = goal completed in
         let next_run = find_run completed second in
         (match next_run.lifecycle with
          | Admitted | Active -> ()
          | Waiting _ | Terminal _ ->
            raise_s [%sexp "second Workflow did not remain live", (next_run : P.Run.t)]);
         assert (P.Run.equal old_run (find_run completed first));
         assert (
           A.Run_goal_link.equal
             old_link
             (A.Run_state.goal_link index first.run_id |> Option.value_exn));
         List.iter [ first; terminal; second ] ~f:(fun receipt ->
           assert (
             List.exists (A.Run_state.receipts index) ~f:(P.Run_receipt.equal receipt)));
         assert (
           P.Goal_episode.Settlement.equal
             predecessor
             (G.previous_episode_settlement current_goal |> Option.value_exn));
         assert (
           P.Run_receipt.equal
             second
             (G.episode current_goal |> Option.value_exn |> P.Goal_episode.admission));
         assert (P.Run_source.equal (G.source (goal cancelled)) (G.source current_goal));
         [%test_eq: int64]
           (Int64.succ (G.revision (goal cancelled)))
           (G.revision current_goal);
         let account = accounting completed in
         [%test_eq: int64] 2L (Accounting.admitted_attempts account);
         [%test_eq: int64] 2L (Accounting.admitted_turns account);
         [%test_eq: int] 2 (List.length (Accounting.bindings account));
         assert (
           List.exists
             (Accounting.bindings account)
             ~f:(Accounting.Binding.equal old_binding));
         let retained =
           Ledger.find
             completed.inference_ledger
             ~ordinal:(Ledger.Handle.ordinal (Ledger.Row.handle old_row))
           |> Option.value_exn
         in
         assert (
           Ledger.Handle.equal (Ledger.Row.handle old_row) (Ledger.Row.handle retained));
         assert (
           Jsonaf.exactly_equal
             (Inference.Observation.Attempt_record.to_json (Ledger.Row.record old_row))
             (Inference.Observation.Attempt_record.to_json (Ledger.Row.record retained)));
         [%test_eq: int] 2 !(counters.providers);
         print_endline
           "live predecessor rejected; actual owner cancellation retains its terminal \
            proof";
         print_endline
           "fresh episode keeps both Run links and lifetime charges without resetting \
            prior inference"));
  [%expect
    {|
    live predecessor rejected; actual owner cancellation retains its terminal proof
    fresh episode keeps both Run links and lifetime charges without resetting prior inference
    |}]
;;
