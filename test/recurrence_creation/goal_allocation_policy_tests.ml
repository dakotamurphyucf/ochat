open! Core
open Creation_support
module Goal = Goal_worker_support
module Actor = A.Session_actor
module Accounting = A.Goal_accounting
module Ledger = A.Inference_ledger
module Usage = A.Goal_usage
module G = P.Goal
module Graph = Agent_server.Graph_tracking
module Runtime = Inference_runtime
module Client = Inference_client
module Observation = Inference.Observation

let budget ?observed_tokens ?deadline () =
  G.Budget.create ~admitted_turns:None ~admitted_attempts:None ~observed_tokens ~deadline
  |> protocol_ok
;;

let preparation_ok result =
  Result.map_error result ~f:(fun error ->
    Sexp.to_string_hum (Runtime.Preparation_error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let backend dispatches =
  (* EOF is the existing offline backend's explicit Completed response, with
     Not_reported usage for every component; it invents no provider token count. *)
  let fixture =
    Inference_fixture.create
      ~namespace:"goal-allocation-policy"
      ~default_model:"fixture"
      ~post_stream:(fun ~sw:_ ~inputs:_ ->
        Int.incr dispatches;
        Seq.empty)
  in
  let target =
    Inference_fixture.capture_config fixture Chat_response.Config.default
    |> preparation_ok
  in
  let context = Inference_fixture.resolve fixture target |> preparation_ok in
  let request =
    Inference.Request.create
      ~target
      ~history:[]
      ~tools:[]
      ~assets:[]
      ~limits:Transcript.Admission.default
    |> Result.map_error ~f:(fun error ->
      Sexp.to_string_hum (Inference.Request.Error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  context, request
;;

let with_graph actor f =
  let next = ref 0 in
  let upstream : Graph.Upstream.t =
    { new_preparation_id =
        (fun () ->
          Int.incr next;
          "goal-allocation-" ^ Int.to_string !next)
    ; on_admitted = (fun ~scope:_ ~accounting_id:_ -> ())
    ; on_attempt = ignore
    ; on_observation = ignore
    ; on_completion = ignore
    }
  in
  let graph =
    Graph.create
      actor
      ~source:
        (Transcript.Source_id.of_string "goal-allocation-operation"
         |> Result.ok_or_failwith)
      ~upstream
    |> protocol_ok
  in
  Handoff_tests.protect_cleanup
    ~finally:(fun () ->
      Eio.Cancel.protect (fun () ->
        Graph.seal graph |> protocol_ok;
        Graph.finish graph |> protocol_ok))
    (fun () -> f graph)
;;

let dispatch ?submission_guard context request ~sw ~graph ~identity =
  let receipt =
    Client.run
      ?submission_guard
      context
      ~sw
      ~identity
      ~relation:Root
      ~request
      ~before_dispatch:ignore
      ~on_attempt:(Graph.on_attempt graph)
      ~on_observation:(Graph.on_observation graph)
      ~on_completion:(Graph.on_completion graph)
      ~on_event:ignore
    |> Result.map_error ~f:(fun error ->
      Sexp.to_string_hum (Client.Error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  assert (
    Inference.Event.Terminal.equal_outcome
      Completed
      (Runtime.Receipt.terminal receipt |> Inference.Event.Terminal.outcome))
;;

let charged current (input : A.Operation_worker.Input.t) expected =
  let account = Goal.accounting current in
  [%test_eq: int64] expected (Accounting.admitted_attempts account);
  let rows = Ledger.rows current.A.Session_state.inference_ledger in
  [%test_eq: int] (Int64.to_int_exn expected) (List.length rows);
  [%test_eq: int] (List.length rows) (List.length (Accounting.bindings account));
  let episode = G.episode (Goal.goal current) |> Option.value_exn in
  let run_id = (P.Goal_episode.admission episode).run_id in
  let run =
    A.Run_state.find (Option.value_exn current.run_state) run_id |> Option.value_exn
  in
  assert (P.Goal_episode.matches_run episode run);
  List.iter rows ~f:(fun row ->
    let handle = Ledger.Row.handle row in
    [%test_eq: P.Id.Operation.t option]
      (Some input.operation.id)
      (Ledger.Handle.operation_id handle);
    let binding =
      List.find_exn (Accounting.bindings account) ~f:(fun binding ->
        Ledger.Handle.equal handle (Accounting.Binding.handle binding))
    in
    assert (P.Id.Run.equal run_id (Accounting.Binding.run_id binding));
    assert (P.Run_source.equal run.source (Accounting.Binding.source binding)));
  account
;;

let ledger_json ledger =
  Ledger.to_document ledger
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (Ledger.Error.sexp_of_t error))
  |> Result.ok_or_failwith
  |> Document_schema.Document.json
;;

let completed_rows current =
  List.iter (Ledger.rows current.A.Session_state.inference_ledger) ~f:(fun row ->
    match Ledger.Row.record row |> Observation.Attempt_record.state with
    | Terminal terminal ->
      assert (
        Inference.Event.Terminal.equal_outcome
          Completed
          (Inference.Event.Terminal.outcome terminal))
    | Prepared | Running | Interrupted _ ->
      failwith "offline backend did not retain its actual completed inference")
;;

let input_unknown current expected =
  let usage =
    Accounting.usage
      (Goal.accounting current)
      ~ledger:current.A.Session_state.inference_ledger
    |> Result.map_error ~f:(fun error ->
      Sexp.to_string_hum (Accounting.Error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  let metric = Usage.component usage Input in
  let equal_sum = P.Inference_query.Metric.equal_sum in
  assert (equal_sum (Tokens 0L) (Usage.Metric.actual metric));
  assert (equal_sum (Tokens 0L) (Usage.Metric.estimated metric));
  [%test_eq: int64]
    expected
    (Usage.Unknown.count (Usage.Metric.unknown metric) Not_reported);
  [%test_eq: int64] 0L (Usage.Metric.untracked_attempts metric)
;;

let%expect_test "recurrence Goal capture uses the actual admitted local Workflow" =
  let module Capture = A.Recurrence_goal_capture in
  let module Binding = A.Recurrence_definition.Goal_binding.Capture in
  let module Request = A.Recurrence_command.Goal_owner_request in
  let deadline = ref timestamp in
  Goal.with_worker
    ~budget:(fun fixture ->
      deadline := P.Timestamp.add_ms !(fixture.wall) 1000 |> protocol_ok;
      budget ~deadline:!deadline ())
    ~run:(fun fixture actor ~sw:_ ~input:_ _caps ->
      let before = state actor in
      let goal = Goal.goal before in
      let episode = G.episode goal |> Option.value_exn in
      let source = G.source goal in
      let goal_id = G.id goal in
      let revision = G.revision goal in
      let now = !(fixture.wall) in
      let request expected_goal_revision =
        Request.create ~goal_id ~expected_goal_revision |> protocol_ok
      in
      let capture state request =
        Capture.capture state ~owner:session ~source ~selection:(Operator request) ~now
      in
      let binding = capture before (request revision) |> protocol_ok in
      assert (P.Session_ref.equal (Binding.owner binding) session);
      assert (P.Id.Goal.equal (Binding.goal_id binding) goal_id);
      assert (P.Goal_episode.equal (Binding.creator_episode binding) episode);
      assert (Int64.equal (Binding.goal_revision_at_capture binding) revision);
      let assert_denied code = function
        | Error (actual : P.Error.t) -> assert (P.Error.equal_code actual.code code)
        | Ok _ -> failwith "unexpected recurrence Goal eligibility"
      in
      capture before (request (Int64.succ revision)) |> assert_denied Conflict;
      let run =
        A.Run_state.find
          (Option.value_exn before.run_state)
          (P.Goal_episode.admission episode).run_id
        |> Option.value_exn
      in
      let attribution =
        A.Goal_work_attribution.of_run before run |> protocol_ok |> Option.value_exn
      in
      let native =
        Capture.capture before ~owner:session ~source ~selection:(Native attribution) ~now
        |> protocol_ok
      in
      assert (Binding.equal native binding);
      let stale_source =
        P.Run_source.create
          ~observer:source.observer
          ~generation:source.generation
          ~installation_epoch:(Int64.succ source.installation_epoch)
        |> protocol_ok
      in
      Capture.check_current binding before ~owner:session ~source:stale_source ~now
      |> assert_denied Conflict;
      Capture.check_current binding before ~owner:session ~source ~now:!deadline
      |> assert_denied Permission_denied;
      assert (equal_state before (state actor));
      let schedule = interval ~first_due:(P.Timestamp.to_time_ns now) () in
      let recurrence_id = id "actual-goal-capture" in
      let mutation =
        A.Recurrence_command.Mutation.Create
          ( recurrence_id
          , { schedule
            ; target = fresh_target before.spec.workspace_instance
            ; policy = A.Recurrence_policy.default
            } )
      in
      let command =
        Creation_support.request actor "actual-goal-recurrence-create" mutation
      in
      let bind revision =
        A.Recurrence_command.with_goal_owner command (request revision) |> protocol_ok
      in
      let publish request =
        Actor.recurrence_command actor ~actor:proof ~authorize ~request
      in
      publish (bind (Int64.succ revision)) |> assert_denied Conflict;
      assert (equal_state before (state actor));
      let original = bind revision in
      let receipt = publish original |> protocol_ok in
      let created_state = state actor in
      let created =
        C.find (Option.value_exn created_state.recurrence_catalog) recurrence_id
        |> Option.value_exn
      in
      assert (
        F.Goal_binding.equal
          (F.goal_binding (E.current created))
          (Controller_goal binding));
      let ordinary_id = id "ordinary-with-enabled-goal" in
      mutate
        actor
        "ordinary-with-enabled-goal"
        (Create
           ( ordinary_id
           , { schedule
             ; target = fresh_target before.spec.workspace_instance
             ; policy = A.Recurrence_policy.default
             } ))
      |> ignore;
      let ordinary =
        C.find (Option.value_exn (state actor).recurrence_catalog) ordinary_id
        |> Option.value_exn
      in
      assert (F.Goal_binding.equal (F.goal_binding (E.current ordinary)) Unbound);
      Goal.command
        actor
        ~key:"recurrence-goal-progress"
        (Record_progress
           (goal_id, G.Progress.of_string "preserve the birth association" |> protocol_ok))
      |> ignore;
      let advanced = state actor in
      let advanced_revision = G.revision (Goal.goal advanced) in
      assert (Int64.(advanced_revision > revision));
      Capture.retain
        binding
        advanced
        ~owner:session
        ~source
        ~request:(request revision)
        ~now
      |> assert_denied Conflict;
      let retained =
        Capture.retain
          binding
          advanced
          ~owner:session
          ~source
          ~request:(request advanced_revision)
          ~now
        |> protocol_ok
      in
      assert (Binding.equal binding retained);
      Capture.check_current binding advanced ~owner:session ~source ~now |> protocol_ok;
      Goal.command actor ~key:"recurrence-goal-hold" (Hold (goal_id, Operator_pause))
      |> ignore;
      let held = state actor in
      Capture.check_current binding held ~owner:session ~source ~now
      |> assert_denied Permission_denied;
      let replay = publish original |> protocol_ok in
      assert (A.Recurrence_command_receipt.equal receipt replay);
      assert (equal_state held (state actor));
      publish (bind advanced_revision) |> assert_denied Conflict);
  print_endline
    "actual Goal episode captured once; edit preserves it and held work cannot start";
  [%expect
    {| actual Goal episode captured once; edit preserves it and held work cannot start |}]
;;

let%expect_test
    "actual Goal allocation save rejection has no dispatch or charge and same owner \
     retries"
  =
  let dispatches = ref 0 in
  let context, request = backend dispatches in
  Goal.with_worker
    ~budget:(fun _ -> budget ())
    ~run:(fun fixture actor ~sw ~input caps ->
      with_graph actor (fun graph ->
        let identity = Graph.bind_identity graph ~allocate:caps.allocate_inference in
        let before = state actor in
        let before_account = Goal.accounting before in
        let frames = Held_tests.frames fixture in
        fixture.reject := true;
        let rejection =
          Exn.protect
            ~finally:(fun () -> fixture.reject := false)
            ~f:(fun () ->
              try
                dispatch context request ~sw ~graph ~identity;
                failwith "failed Goal allocation save reached the backend"
              with
              | Graph.Rejected error -> error)
        in
        [%test_eq: P.Error.code] Persistence_error rejection.code;
        [%test_eq: bool] true rejection.retryable;
        assert (
          Document_schema.Json.equal
            (P.Error.create
               Persistence_error
               ~message:"definite no-write runtime fixture"
               ~retryable:true
               ()
             |> P.Error.to_json)
            (P.Error.to_json rejection));
        [%test_eq: int] 0 !dispatches;
        let unchanged = state actor in
        assert (equal_state before unchanged);
        [%test_eq: string list] frames (Held_tests.frames fixture);
        assert (Accounting.equal before_account (Goal.accounting unchanged));
        dispatch context request ~sw ~graph ~identity;
        [%test_eq: int] 1 !dispatches;
        let after = state actor in
        let account = charged after input 1L in
        [%test_eq: int64]
          (Accounting.admitted_turns before_account)
          (Accounting.admitted_turns account);
        assert (
          Option.equal
            P.Goal_episode.equal
            (G.episode (Goal.goal before))
            (G.episode (Goal.goal after)));
        completed_rows after));
  print_endline "real allocation rejects atomically; same live Goal owner dispatches once";
  [%expect {| real allocation rejects atomically; same live Goal owner dispatches once |}]
;;

let%expect_test "actual Goal submission samples deadline after all composed refreshes" =
  let dispatches = ref 0 in
  let context, request = backend dispatches in
  let deadline = ref timestamp in
  Goal.with_worker
    ~budget:(fun fixture ->
      deadline := P.Timestamp.add_ms !(fixture.wall) 1000 |> protocol_ok;
      budget ~deadline:!deadline ())
    ~run:(fun fixture actor ~sw ~input caps ->
      with_graph actor (fun graph ->
        let identity = Graph.bind_identity graph ~allocate:caps.allocate_inference in
        let before = state actor in
        let refreshed = ref false in
        let at_final_check = ref false in
        let submission_guard =
          Runtime.Submission_guard.create ~refresh:(fun () ->
            refreshed := true;
            (* Client composes this guard before the actual allocation guard.
               Every refresh completes first. This nonyielding closure advances
               only the fixture services clock, immediately before the canonical
               final check must resample it. This is not a realtime deadline. *)
            fun () ->
              assert !refreshed;
              at_final_check := true;
              fixture.wall := !deadline)
        in
        let rejection =
          try
            dispatch ~submission_guard context request ~sw ~graph ~identity;
            failwith "expired acknowledged Goal allocation reached the backend"
          with
          | A.Inference_allocation.Rejected error -> error
        in
        [%test_eq: P.Error.code] Permission_denied rejection.code;
        [%test_eq: bool] false rejection.retryable;
        assert !at_final_check;
        assert (Operator_authorization.is_current proof);
        authorize (state actor) |> protocol_ok;
        [%test_eq: int] 0 !dispatches;
        let after = state actor in
        let account = charged after input 1L in
        [%test_eq: int64]
          (Accounting.admitted_turns (Goal.accounting before))
          (Accounting.admitted_turns account);
        assert (G.Budget.equal (G.budget (Goal.goal before)) (G.budget (Goal.goal after)));
        assert (G.State.equal (G.state (Goal.goal before)) (G.state (Goal.goal after)));
        assert (
          Option.equal
            P.Goal_episode.equal
            (G.episode (Goal.goal before))
            (G.episode (Goal.goal after)));
        (* The allocation ACK remains factual evidence. The final submission
           denial is followed by factual interruption, not a fabricated terminal. *)
        match Ledger.rows after.inference_ledger with
        | [ row ] ->
          (match Ledger.Row.record row |> Observation.Attempt_record.state with
           | Interrupted { reason = Host_interrupted; _ } -> ()
           | Prepared | Running | Terminal _ | Interrupted { reason = Cancelled; _ } ->
             failwith "denied submission lost its actual interruption evidence")
        | _ -> failwith "deadline case did not retain one actual allocation"));
  print_endline "one ACKed Goal charge remains; final deadline rejects before backend";
  [%expect {| one ACKed Goal charge remains; final deadline rejects before backend |}]
;;

let%expect_test
    "actual unknown usage is reported without tokens and gates only selected Input"
  =
  List.iter [ false; true ] ~f:(fun select_input ->
    let dispatches = ref 0 in
    let context, request = backend dispatches in
    let observed_tokens =
      if select_input
      then Some (G.Budget.Token_stop.create ~component:Input ~ceiling:5L |> protocol_ok)
      else None
    in
    Goal.with_worker_on_initial_state
      ~initialize_state:(fun initial ->
        (* Start from new-session inventory so only provider usage becomes unknown. *)
        A.Child_accounting.initialize_new
          initial
          ~max_children:4
          ~max_encoded_bytes:1048576
        |> protocol_ok)
      ~budget:(fun _ -> budget ?observed_tokens ())
      ~run:(fun _fixture actor ~sw ~input caps ->
        with_graph actor (fun graph ->
          let identity = Graph.bind_identity graph ~allocate:caps.allocate_inference in
          dispatch context request ~sw ~graph ~identity;
          [%test_eq: int] 1 !dispatches;
          let first = state actor in
          charged first input 1L |> ignore;
          completed_rows first;
          input_unknown first 1L;
          if select_input
          then (
            let rejection =
              try
                dispatch context request ~sw ~graph ~identity;
                failwith "unknown selected Input usage admitted a second backend call"
              with
              | Graph.Rejected error -> error
            in
            [%test_eq: P.Error.code] Permission_denied rejection.code;
            [%test_eq: bool] false rejection.retryable;
            [%test_eq: int] 1 !dispatches;
            let held = state actor in
            assert (
              G.State.equal
                (Open (Held (Usage_uncertain Input)))
                (G.state (Goal.goal held)));
            charged held input 1L |> ignore;
            completed_rows held;
            input_unknown held 1L;
            assert (
              Document_schema.Json.equal
                (ledger_json first.inference_ledger)
                (ledger_json held.inference_ledger)))
          else (
            dispatch context request ~sw ~graph ~identity;
            [%test_eq: int] 2 !dispatches;
            let allowed = state actor in
            assert (G.State.equal (Open Enabled) (G.state (Goal.goal allowed)));
            charged allowed input 2L |> ignore;
            completed_rows allowed;
            input_unknown allowed 2L))));
  print_endline
    "no token policy reports unknown and dispatches twice; selected Input holds after one";
  [%expect
    {| no token policy reports unknown and dispatches twice; selected Input holds after one |}]
;;
