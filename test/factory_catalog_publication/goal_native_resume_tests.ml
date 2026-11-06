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
