open! Core
open Agent_server_test_support
open Retry_support
open Goal_native_test_support
module Intent = P.Goal_native_resume_intent
module Native = Actor.Goal_native_resume
module Driver = Agent_server.Goal_native_resume_dispatcher

let native_intent entry invocation =
  List.find_exn (state entry).invocations ~f:(fun actual ->
    P.Id.Invocation.equal actual.context.id invocation.P.Invocation.context.id)
  |> fun actual ->
  match actual.P.Invocation.native_goal_action with
  | Some (P.Goal_native_action_intent.Resume intent) -> intent
  | None | Some (Start _) -> failwith "expected actual native Resume intent"
;;

let goal_run current admission =
  A.Run_state.find
    (Option.value_exn current.A.Session_state.run_state)
    admission.P.Run_receipt.run_id
  |> Option.value_exn
;;

let no_accepted entry =
  List.iter (state entry).invocations ~f:(fun invocation ->
    Option.iter invocation.P.Invocation.native_goal_action ~f:(function
      | P.Goal_native_action_intent.Start _ ->
        failwith "unexpected Start in Resume fixture"
      | Resume intent ->
        (match Intent.phase intent with
         | Staged | Discarded _ -> ()
         | Accepted _ | Dispatch_claimed _ | Rejected _ ->
           failwith "rolled-back callback published an accepted command")))
;;

let%expect_test
    "native Goal edits commit only selected callback receipts; rollback retires Resume \
     loans"
  =
  List.iter
    [ true, "progress", true
    ; true, "progress-rollback", false
    ; true, "multiple", false
    ; false, "resume-catch", true
    ]
    ~f:(fun (v1, mode, succeeds) ->
      with_callback
        ~v1
        (fun
            _env
             _daemon
             entry
             runtime
             counters
             _session
             admission
             _old_live
             held
             request
             operation
             ~release_ordinary
             ~resumed_entered
             ~release_resumed:_
             ~reopen:_
           ->
           let before = state entry in
           let result, invocations =
             callback
               entry
               runtime
               ~operation
               ~mode
               ~run:
                 (if String.is_prefix mode ~prefix:"progress"
                  then progress entry
                  else
                    fun admission ->
                      Actor.stage_native_goal_resume
                        entry.actor
                        ~admission
                        ~authorize
                        ~request)
           in
           if succeeds
           then assert (result |> protocol_ok)
           else assert (Result.is_error result);
           [%test_eq: int]
             (if String.equal mode "multiple" then 2 else 1)
             (List.length invocations);
           let current = state entry in
           assert (P.Run.equal (goal_run held admission) (goal_run current admission));
           assert (
             A.Goal_accounting.equal (Goal.accounting before) (Goal.accounting current));
           if String.equal mode "progress"
           then (
             [%test_eq: int64]
               Int64.(G.revision (Goal.goal before) + 1L)
               (G.revision (Goal.goal current));
             [%test_eq: string]
               "actual local native progress"
               (G.Progress.to_string (G.progress (Goal.goal current))))
           else assert (G.equal (Goal.goal before) (Goal.goal current));
           no_accepted entry;
           release_ordinary ();
           ignore (await_quiet entry : A.Session_state.t);
           assert (
             Option.is_none (Actor.select_native_goal_resume entry.actor |> protocol_ok));
           assert (Option.is_none (Eio.Promise.peek resumed_entered));
           [%test_eq: int] 2 !(counters.providers)));
  print_endline
    "selected edit ACK; parent and multiple-Resume rollback preserve Goal/account and \
     retire dispatch custody";
  [%expect
    {| selected edit ACK; parent and multiple-Resume rollback preserve Goal/account and retire dispatch custody |}]
;;

let status entry runtime operation invocation_id =
  let observed = ref None in
  let result, _ =
    callback entry runtime ~operation ~mode:"status" ~run:(fun admission ->
      Actor.native_goal_resume_status
        entry.Registry.actor
        ~admission
        ~authorize
        ~invocation_id
      |> Result.map ~f:(fun value -> observed := Some value))
  in
  assert (result |> protocol_ok);
  Option.value_exn !observed
;;

let%expect_test
    "legacy and v1 native Resume survives callback close and returns before provider \
     completion"
  =
  List.iter [ false; true ] ~f:(fun v1 ->
    with_callback
      ~v1
      (fun
          _env
           daemon
           entry
           runtime
           counters
           _session
           admission
           _old_live
           held
           request
           operation
           ~release_ordinary
           ~resumed_entered
           ~release_resumed
           ~reopen:_
         ->
         let callback_closed = ref false
         and later_policy_checks = ref 0 in
         let current_host_policy actor current =
           let open Result.Let_syntax in
           let%map () = authorize actor current in
           if !callback_closed then incr later_policy_checks
         in
         let result, invocations =
           callback entry runtime ~operation ~mode:"resume" ~run:(fun admission ->
             Actor.stage_native_goal_resume
               entry.actor
               ~admission
               ~authorize:current_host_policy
               ~request)
         in
         assert (result |> protocol_ok);
         callback_closed := true;
         let invocation = List.hd_exn invocations in
         (match Intent.phase (native_intent entry invocation) with
          | Accepted _ -> ()
          | _ -> assert false);
         assert (P.Run.equal (goal_run held admission) (goal_run (state entry) admission));
         assert (
           A.Goal_accounting.equal (Goal.accounting held) (Goal.accounting (state entry)));
         (match status entry runtime operation invocation.context.id with
          | Native.Status.Accepted _ -> ()
          | _ -> failwith "accepted command lost its lexical-close custody");
         assert (
           Option.is_none (Actor.select_native_goal_resume entry.actor |> protocol_ok));
         release_ordinary ();
         let rec ordinary_joined () =
           match (state entry).active_operation with
           | Some current when P.Id.Operation.equal current.id operation.id ->
             Eio.Fiber.yield ();
             ordinary_joined ()
           | Some _ | None -> ()
         in
         ordinary_joined ();
         let driver = Driver.create ~factory:(Daemon.factory daemon) in
         Exn.protect
           ~finally:(fun () -> Driver.close driver)
           ~f:(fun () ->
             (* No direct dispatcher call before this barrier. Only the genuine
                Daemon schedule host can consume the accepted command after the
                ordinary callback and runtime leases have joined. *)
             Eio.Promise.await resumed_entered;
             (* The actual provider remains blocked: neither callback nor dispatcher
           synchronously waits for arbitrary workflow completion. *)
             [%test_eq: int] 3 !(counters.providers);
             assert (!later_policy_checks > 0);
             let current = state entry in
             let resumed_operation = Option.value_exn current.active_operation in
             [%test_eq: P.Operation.kind] (Turn Administrative) resumed_operation.kind;
             let run = goal_run current admission in
             assert (
               List.exists run.owned_work ~f:(fun work ->
                 P.Run_work.Key.equal work.key (Operation resumed_operation.id)));
             [%test_eq: int]
               (if v1 then 2 else 1)
               (List.length (A.Run_state.runs (Option.value_exn current.run_state)));
             let resume_intent = native_intent entry invocation in
             (match Intent.phase resume_intent with
              | Dispatch_claimed _ -> ()
              | _ -> assert false);
             let before_status =
               P.Invocation.to_json
                 (List.find_exn current.invocations ~f:(fun row ->
                    P.Id.Invocation.equal row.context.id invocation.context.id))
             in
             (match status entry runtime resumed_operation invocation.context.id with
              | Native.Status.Acknowledged
                  { receipt; detail = Actor_outcome (Started actual); failure = None; _ }
                ->
                assert (P.Id.Operation.equal actual.id resumed_operation.id);
                assert (
                  P.Idempotency_key.equal receipt.key (A.Goal_resume_request.key request))
              | _ -> failwith "status did not derive the same actual preparation handoff");
             let after_status =
               List.find_exn (state entry).invocations ~f:(fun row ->
                 P.Id.Invocation.equal row.context.id invocation.context.id)
               |> P.Invocation.to_json
             in
             assert (Jsonaf.exactly_equal before_status after_status);
             Driver.process_entry driver entry |> protocol_ok;
             [%test_eq: int] 3 !(counters.providers);
             if v1
             then (
               let before = state entry in
               let finish_intent =
                 P.Goal_achievement.Intent.create
                   ~expected_goal_revision:Int64.(G.revision (Goal.goal before) + 1L)
                   ~evidence:
                     (G.Reason.of_string "native progress and real scoped Finish"
                      |> protocol_ok)
                   ~action:(Finish { terminal = Completed None; relinquish = [] })
                 |> protocol_ok
                 |> P.Goal_achievement.Intent.to_json
               in
               let result, _ =
                 callback
                   ~finish_intent
                   entry
                   runtime
                   ~operation:resumed_operation
                   ~mode:"progress-finish"
                   ~run:(progress entry)
               in
               assert (result |> protocol_ok);
               assert (Option.is_some (G.achievement_request (Goal.goal (state entry)))));
             release_resumed ();
             let settled = await_quiet entry in
             if v1
             then (
               match
                 (goal_run settled admission).lifecycle, G.state (Goal.goal settled)
               with
               | Terminal (Completed None), Closed (Achieved achievement) ->
                 let terminal =
                   Goal_finish_tests.terminal_receipt
                     (Option.value_exn settled.run_state)
                     admission
                 in
                 assert (
                   P.Run_receipt.equal terminal (P.Goal_achievement.receipt achievement));
                 [%test_eq: string]
                   "actual local native progress"
                   (G.Progress.to_string (G.progress (Goal.goal settled)))
               | _ ->
                 failwith
                   "staged progress and Goal.finish did not share actual callback \
                    settlement");
             assert (
               Option.is_none (Actor.select_native_goal_resume entry.actor |> protocol_ok)))));
  print_endline
    "legacy/v1 actual callback ACK -> same loan -> finite shared source dispatch -> \
     factual Started; no replay";
  [%expect
    {| legacy/v1 actual callback ACK -> same loan -> finite shared source dispatch -> factual Started; no replay |}]
;;

let%expect_test
    "post-callback host policy revocation cannot dispatch its retained command loan"
  =
  with_callback
    ~v1:true
    (fun
        _env
         daemon
         entry
         runtime
         counters
         _session
         admission
         _old_live
         held
         request
         operation
         ~release_ordinary
         ~resumed_entered
         ~release_resumed
         ~reopen:_
       ->
       let enabled = ref true
       and denied = ref false in
       let current_host_policy actor current =
         let open Result.Let_syntax in
         let%bind () = authorize actor current in
         if !enabled
         then Ok ()
         else (
           denied := true;
           Error
             (P.Error.create
                Permission_denied
                ~message:"host revoked native Goal command policy"
                ~retryable:false
                ()))
       in
       let result, invocations =
         callback entry runtime ~operation ~mode:"resume" ~run:(fun admission ->
           Actor.stage_native_goal_resume
             entry.actor
             ~admission
             ~authorize:current_host_policy
             ~request)
       in
       assert (result |> protocol_ok);
       let invocation = List.hd_exn invocations in
       enabled := false;
       release_ordinary ();
       ignore (await_quiet entry : A.Session_state.t);
       let rec selected () =
         if not !denied
         then (
           Eio.Fiber.yield ();
           selected ())
       in
       selected ();
       ignore (await_quiet entry : A.Session_state.t);
       assert (Option.is_none (Eio.Promise.peek resumed_entered));
       [%test_eq: int] 2 !(counters.providers);
       assert (G.equal (Goal.goal held) (Goal.goal (state entry)));
       assert (P.Run.equal (goal_run held admission) (goal_run (state entry) admission));
       assert (Option.is_none (Actor.select_native_goal_resume entry.actor |> protocol_ok));
       let _reader = ordinary ~key:"native-goal-revoked-status" daemon entry in
       Eio.Promise.await resumed_entered;
       let reader = Option.value_exn (state entry).active_operation in
       (match status entry runtime reader invocation.context.id with
        | Native.Status.Interrupted_before_dispatch _ -> ()
        | _ -> failwith "policy-revoked accepted DATA retained execution custody");
       release_resumed ();
       ignore (await_quiet entry : A.Session_state.t));
  print_endline
    "native borrow expires; transferred loan still obeys current host policy; accepted \
     DATA does not reissue it";
  [%expect
    {| native borrow expires; transferred loan still obeys current host policy; accepted DATA does not reissue it |}]
;;

let selected_prompt =
  {|<developer>Stage an explicitly requested existing Goal episode continuation.</developer>
<tool name="goal_get"/>
<tool name="goal_resume"/>
<script id="selected_resume" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let prefix = "ochat-test-native-goal:"
let field value key =
  match Json.get_field(value, key) with | `Some(value) -> value | `None -> `Null
let current_request ctx =
  match Context.last_user_item(ctx) with
  | `None -> `None
  | `Some(item) ->
    (match Item.text(item) with
     | `Some(text) ->
       let size = String.length(prefix) in
       if String.starts_with(text, prefix)
       then `Some(Json.parse(String.slice(text, size, String.length(text) - size)))
       else `None
     | `None -> `None)
let resume_request view goal run key revision =
  `Object([
    {key = "kind"; value = `String("goal_resume_episode_v1")},
    {key = "session_id"; value = field(field(view, "session"), "session_id")},
    {key = "generation"; value = field(view, "generation")},
    {key = "goal_id"; value = field(goal, "id")},
    {key = "run_id"; value = field(run, "run_id")},
    {key = "source"; value = field(view, "source")},
    {key = "expected_goal_revision"; value = field(goal, "revision")},
    {key = "expected_catalog_revision"; value = field(view, "catalog_revision")},
    {key = "expected_run_revision"; value = revision},
    {key = "key"; value = `String(key)}
  ])
let selected_row goal_id =
  let* response = Tool.call("goal_get", `Object([{key = "goal_id"; value = goal_id}])) in
  match response with
  | `Error(message) -> Task.fail(message)
  | `Ok(view) -> (match field(view, "goals") with
    | `Array(rows) -> if Array.length(rows) == 1 then Task.pure((|view, rows[0]|))
      else Task.fail("Expected one actual Goal")
    | _ -> Task.fail("Expected current Goal rows"))
let expect_denied request =
  let* result = Tool.call("goal_resume", request) in
  match result with
  | `Error(_) -> Task.pure(())
  | `Ok(_) -> Task.fail("Expected native Goal Resume denial")
let on_event ctx state event =
  match event with
  | `Turn_end ->
    if state >= 2 then Task.pure(state)
    else (match current_request(ctx) with
      | `None -> Task.pure(state)
      | `Some(envelope) ->
        let* observed = selected_row(field(envelope, "goal_id")) in
        let (|view, row|) = observed in
        let goal = field(row, "goal") in
        let run = field(row, "current_run") in
        let current = field(run, "revision") in
        let* result = match field(envelope, "allow") with
          | `Bool(true) ->
            let admission = field(field(goal, "episode"), "admission") in
            let historical = field(admission, "run_revision") in
            let stale = resume_request(view, goal, run, "selected-resume-historical", historical) in
            let* denied = expect_denied(stale) in
            let request = resume_request(view, goal, run, "selected-resume-current", current) in
            let* result = Tool.call("goal_resume", request) in
            (match result with
            | `Error(message) -> Task.fail(message)
            | `Ok(value) ->
              (match (|field(value, "staged"), field(value, "effect")|) with
               | (|`Bool(true), `String("goal_resume")|) -> Task.pure(())
               | _ -> Task.fail("Expected the local staged result")))
          | _ -> expect_denied(resume_request(view, goal, run, "selected-resume-paused", current)) in
        Task.pure(state + 1))
  | _ -> Task.pure(state)
</script>|}
;;

let selected_submit daemon entry ~key envelope =
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
           ; text = "ochat-test-native-goal:" ^ Jsonaf.to_string envelope
           ; attachments = []
           })
      ~key:(P.Idempotency_key.of_string key |> protocol_ok)
    |> protocol_ok
  in
  match Goal.ordinary daemon entry request with
  | Admitted _ -> ()
  | Rejected error | Uncertain error -> raise_s [%sexp (error : P.Error.t)]
;;

let selected_invocations current =
  List.filter current.A.Session_state.invocations ~f:(fun invocation ->
    String.equal invocation.P.Invocation.context.tool_name "goal_resume")
;;

let selected_denied invocation code =
  let open P.Invocation in
  assert (Option.is_none invocation.native_goal_action);
  match invocation.status with
  | Resolved (Fail failure) | Published (Fail failure) ->
    [%test_eq: string] (P.Error.code_to_string code) failure.code
  | status -> raise_s [%sexp "expected actual selected denial", (status : status)]
;;

let%expect_test
    "selected authored Goal Resume carries the actual callback intent to dispatch"
  =
  let ordinal = ref 0 in
  let entered, entered_u = Eio.Promise.create () in
  let release, release_u = Eio.Promise.create () in
  let post_stream _ =
    Int.incr ordinal;
    if Int.equal !ordinal 4
    then (
      Eio.Promise.resolve entered_u ();
      Eio.Promise.await release);
    completed_response "actual selected Goal Resume provider"
  in
  Staging.with_episode
    ~prompt_text:selected_prompt
    ~post_stream
    (fun _env daemon entry _runtime counters session admission _old_live paused ->
       Exn.protect
         ~finally:(fun () ->
           if Option.is_none (Eio.Promise.peek release)
           then Eio.Promise.resolve release_u ())
         ~f:(fun () ->
           let module R = P.Goal_resume_request in
           let input allow =
             `Object
               [ ("allow", if allow then `True else `False)
               ; "goal_id", P.Id.Goal.to_json Goal.goal_id
               ]
           in
           let paused_request =
             Staging.exact_request paused ~key:"selected-resume-paused"
           in
           selected_submit daemon entry ~key:"selected-resume-pause-probe" (input false);
           let after_denial = await_quiet entry in
           let denied =
             Goal_resume_runtime_tests.one_actual
               "native operator pause denial"
               (selected_invocations after_denial)
           in
           assert (
             Document_schema.Json.equal denied.context.input (R.to_json paused_request));
           selected_denied denied Permission_denied;
           assert (
             P.Run.equal (goal_run paused admission) (goal_run after_denial admission));
           assert (
             Option.is_none
               (Staging.historical entry session ~actor:original paused_request));
           [%test_eq: int] 2 !(counters.providers);
           let held = set_passive daemon entry in
           let run = goal_run held admission in
           let request = Staging.exact_request held ~key:"selected-resume-current" in
           assert (not (Int64.equal run.revision admission.run_revision));
           let historical =
             Staging.request
               held
               ~key:"selected-resume-historical"
               ~goal_revision:(G.revision (Goal.goal held))
               ~run_revision:admission.run_revision
               ~source:run.source
           in
           selected_submit daemon entry ~key:"selected-resume-probe" (input true);
           Eio.Promise.await entered;
           let current = state entry in
           let invocation, intent =
             List.filter_map (selected_invocations current) ~f:(fun invocation ->
               match invocation.P.Invocation.native_goal_action with
               | Some (Resume intent) -> Some (invocation, intent)
               | Some (Start _) | None -> None)
             |> Goal_resume_runtime_tests.one_actual "accepted public Resume"
           in
           [%test_eq: int] 3 (List.length (selected_invocations current));
           List.find_exn (selected_invocations current) ~f:(fun invocation ->
             Document_schema.Json.equal invocation.context.input (R.to_json historical))
           |> fun stale ->
           selected_denied stale Conflict;
           assert (Document_schema.Json.equal invocation.context.input (R.to_json request));
           assert (R.equal request (Intent.request intent));
           (match Intent.phase intent with
            | Dispatch_claimed _ -> ()
            | _ -> failwith "actual schedule host did not claim the accepted Resume");
           (match invocation.status with
            | Resolved (Complete result) | Published (Complete result) ->
              assert (
                Document_schema.Json.equal
                  result
                  (`Object [ "staged", `True; "effect", `String "goal_resume" ]))
            | _ -> failwith "selected Resume lost its exact local staged outcome");
           assert (
             Option.is_some (Staging.historical entry session ~actor:original request));
           assert (
             Option.is_none (Staging.historical entry session ~actor:original historical));
           let current_run = goal_run current admission in
           assert (P.Id.Run.equal run.id current_run.id);
           assert (G.State.equal (Open Enabled) (G.state (Goal.goal current)));
           let operation = Option.value_exn current.active_operation in
           assert (
             List.exists current_run.owned_work ~f:(fun work ->
               P.Run_work.Key.equal work.key (Operation operation.id)));
           Eio.Promise.resolve release_u ();
           ignore (await_quiet entry : A.Session_state.t);
           [%test_eq: int] 4 !(counters.providers);
           [%test_eq: int] 3 (List.length (selected_invocations (state entry)))));
  print_endline
    "selected local stage, operator pause and historical revision denials, one \
     continuation";
  [%expect
    {| selected local stage, operator pause and historical revision denials, one continuation |}]
;;
