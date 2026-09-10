open Core
open Fixtures
open Job_fixtures
module P = Agent_protocol
module S = P.Schedule
module Setup = Subscription_transaction_tests

let event =
  Chat_response.Moderation.Event.Pre_tool_call
    { id = "schedule"
    ; name = "fixture"
    ; args = `Null
    ; kind = Function
    ; payload_text = "null"
    ; meta = `Null
    }
;;

let with_event ?(snapshot = Setup.before) actor parent f =
  A.with_job_execution
    actor
    ~job_id:parent.J.id
    ~generation:0
    ~attempt:parent.attempt
    ~deadline:(Some deadline)
    (fun services ->
       services.claim_event
         ~event
         ~snapshot:(fun () -> Ok snapshot)
         (fun ~executing ~event:_ ~execute:_ ~commit ->
            f (J.Moderator_event executing.context.id) commit))
;;

let save commit =
  commit
    ~snapshot:Setup.after
    ~requests:I.{ request_turn = false; request_compaction = false; end_session = None }
;;

let create actor owner =
  A.create_script_schedule
    actor
    ~owner
    ~source:Setup.source
    ~delay_ms:0
    ~payload:(`String "tick")
    ~misfire:Deliver_once_immediately
;;

let%expect_test
    "owned schedule encoding cannot silently downgrade into legacy timer authority"
  =
  let legacy = Subscription_expiry_tests.schedule () in
  let owned =
    { legacy with
      ownership =
        Some
          { source = Setup.source
          ; creator = Invocation (P.Id.Invocation.of_string "inv_timer" |> protocol_ok)
          ; subscription = None
          }
    }
  in
  List.iter [ legacy; owned ] ~f:(fun timer ->
    let restored = S.of_json (S.to_json timer) |> protocol_ok in
    assert (Jsonaf.exactly_equal (S.to_json timer) (S.to_json restored));
    assert (
      Jsonaf.exactly_equal (S.to_json timer) (S.to_json (S.t_of_sexp (S.sexp_of_t timer)))));
  let envelope = S.to_json owned in
  let replace json name value =
    match json with
    | `Object fields -> `Object (List.Assoc.add fields ~equal:String.equal name value)
    | _ -> failwith "expected schedule object"
  in
  let remove json name =
    match json with
    | `Object fields -> `Object (List.Assoc.remove fields ~equal:String.equal name)
    | _ -> failwith "expected schedule object"
  in
  (* The previous reader requires a top-level id, so it rejects this envelope. *)
  reject
    "old reader"
    (P.Json_codec.required_as
       (P.Json_codec.fields envelope |> protocol_ok)
       "id"
       P.Id.Schedule.of_json);
  reject "missing ownership" (S.of_json (remove envelope "ownership"));
  reject "missing version" (S.of_json (remove envelope "schema_version"));
  reject "unknown version" (S.of_json (replace envelope "schema_version" (`Number "3")));
  reject "legacy authority upgrade" (S.validate_transition ~previous:(Some legacy) owned);
  reject "ownership removed" (S.validate_transition ~previous:(Some owned) legacy);
  reject
    "due time rewritten"
    (S.validate_transition
       ~previous:(Some owned)
       { owned with next_due_at = Subscription_expiry_tests.at 1 });
  let ownership = Option.value_exn owned.ownership in
  let bound =
    { owned with
      ownership =
        Some
          { ownership with
            subscription = Some (P.Id.Subscription.of_string "sub_timer" |> protocol_ok, 1)
          }
    }
  in
  S.validate_transition ~previous:(Some owned) bound |> protocol_ok;
  reject
    "epoch rebound"
    (S.validate_transition
       ~previous:(Some bound)
       { owned with
         ownership =
           Some
             { ownership with
               subscription =
                 Some (P.Id.Subscription.of_string "sub_timer" |> protocol_ok, 2)
             }
       });
  [%expect
    {|
    ("old reader" Invalid_request)
    ("missing ownership" Invalid_request)
    ("missing version" Invalid_request)
    ("unknown version" Incompatible_protocol)
    ("legacy authority upgrade" Conflict)
    ("ownership removed" Conflict)
    ("due time rewritten" Conflict)
    ("epoch rebound" Conflict)
    |}]
;;

let%expect_test
    "schedule reservations obey source, rollback, quotas and checkpoint persistence"
  =
  List.iter [ `Accepted; `Cancelled; `Rejected; `Unselected; `Abandoned ] ~f:(fun mode ->
    let reject_save = ref false in
    with_actor
      ~reject_save:(fun _ -> !reject_save)
      ~schedule_limits:
        { Agent_session.Staged_schedules.default_limits with max_per_source = 1 }
      (fun _env _sw actor _writer backend ->
         A.change_moderator actor (Some (Setup.encode Setup.before))
         |> protocol_ok
         |> ignore;
         let parent = add_claimed_job actor in
         let admitted = ref None in
         let result =
           with_event actor parent (fun owner commit ->
             let discarded, _ = create actor owner |> protocol_ok in
             assert (Result.is_error (create actor owner));
             A.abort_schedule_mutation actor ~owner ~receipt:discarded |> protocol_ok;
             let creation, timer = create actor owner |> protocol_ok in
             admitted := Some timer;
             assert (List.is_empty (Agent_session.Memory_backend.state backend).schedules);
             assert (
               Result.is_error
                 (A.claim_schedule actor ~schedule_id:timer.id ~generation:0));
             assert (
               Result.is_error
                 (A.read_script_schedule
                    actor
                    ~owner
                    ~source:{ Setup.source with source_sha256 = String.make 64 'b' }
                    ~id:timer.id));
             let receipts =
               match mode with
               | `Cancelled ->
                 let cancelled = { timer with status = S.Cancelled } in
                 let receipt =
                   A.stage_schedule_mutation
                     actor
                     ~owner
                     ~source:Setup.source
                     ~previous:(Some timer)
                     ~next:cancelled
                   |> protocol_ok
                 in
                 assert (
                   Result.is_error
                     (A.abort_schedule_mutation actor ~owner ~receipt:creation));
                 [ creation; receipt ]
               | `Unselected -> []
               | _ -> [ creation ]
             in
             A.select_schedule_mutations actor ~owner ~source:Setup.source ~receipts
             |> protocol_ok;
             match mode with
             | `Abandoned -> Error (handoff_error "handler abandoned timer")
             | _ ->
               (match mode with
                | `Rejected -> reject_save := true
                | _ -> ());
               let result = save commit in
               reject_save := false;
               result)
         in
         reject_save := false;
         (match mode, result with
          | (`Rejected | `Abandoned), Error _
          | (`Accepted | `Cancelled | `Unselected), Ok _ -> ()
          | _ -> failwith "unexpected timer checkpoint result");
         let state = Agent_session.Memory_backend.state backend in
         let timer = Option.value_exn !admitted in
         (match mode with
          | `Accepted ->
            let claimed =
              A.claim_schedule actor ~schedule_id:timer.id ~generation:0
              |> protocol_ok
              |> Option.value_exn
            in
            assert (P.Id.Schedule.equal claimed.id timer.id)
          | `Cancelled ->
            assert (
              Option.is_none
                (A.claim_schedule actor ~schedule_id:timer.id ~generation:0 |> protocol_ok))
          | _ -> assert (List.is_empty state.schedules));
         assert (
           Option.is_some (A.with_quiescent_state actor ~f:(fun _ -> Ok ()) |> protocol_ok));
         let restored =
           Agent_session.Session_persistence.restore_snapshot
             (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t state))
           |> store_ok
         in
         Agent_session.Session_state.validate restored |> protocol_ok;
         print_s
           [%sexp
             (mode : [ `Accepted | `Cancelled | `Rejected | `Unselected | `Abandoned ])
           , (List.map state.schedules ~f:(fun schedule -> schedule.S.status)
              : S.status list)]));
  [%expect
    {|
    (Accepted (Scheduled))
    (Cancelled (Cancelled))
    (Rejected ())
    (Unselected ())
    (Abandoned ())
    |}]
;;

let%expect_test
    "timer and subscription epoch binding commit together and reject forged linkage"
  =
  let now = ref timestamp in
  with_actor
    ~now:(fun () -> !now)
    (fun _env _sw actor _writer backend ->
       A.change_moderator actor (Some (Setup.encode Setup.before))
       |> protocol_ok
       |> ignore;
       let subscription =
         Subscription_expiry_tests.admit
           actor
           ~deadline:(Subscription_expiry_tests.at 1)
           ~timer:None
           ~job:None
           ~completed:false
       in
       let parent = add_claimed_job actor in
       with_event actor parent (fun owner commit ->
         let creation, timer = create actor owner |> protocol_ok in
         let armed =
           P.Subscription.arm
             subscription
             ~expected_epoch:subscription.epoch
             ~timer_id:(Some timer.id)
             ~job_id:None
           |> protocol_ok
         in
         let receipt =
           A.stage_subscription_mutation
             actor
             ~owner
             ~source:Setup.source
             ~previous:(Some subscription)
             ~next:armed
           |> protocol_ok
         in
         let ownership = Option.value_exn timer.ownership in
         let bound =
           { timer with
             ownership =
               Some
                 { ownership with
                   subscription = Some (subscription.context.id, armed.epoch)
                 }
           }
         in
         let linkage =
           A.stage_schedule_mutation
             actor
             ~owner
             ~source:Setup.source
             ~previous:(Some timer)
             ~next:bound
           |> protocol_ok
         in
         A.select_subscription_mutations
           actor
           ~owner
           ~source:Setup.source
           ~receipts:[ receipt ]
         |> protocol_ok;
         A.select_schedule_mutations
           actor
           ~owner
           ~source:Setup.source
           ~receipts:[ creation; linkage ]
         |> protocol_ok;
         save commit)
       |> protocol_ok
       |> ignore;
       let state = Agent_session.Memory_backend.state backend in
       let timer = List.hd_exn state.schedules in
       let ownership = Option.value_exn timer.ownership in
       let corrupt label ownership =
         let corrupted =
           { state with schedules = [ { timer with ownership = Some ownership } ] }
         in
         assert (Result.is_error (Agent_session.Session_state.validate corrupted));
         print_endline label
       in
       corrupt
         "wrong source"
         { ownership with
           source = { Setup.source with source_sha256 = String.make 64 'c' }
         };
       corrupt
         "future epoch"
         { ownership with subscription = Some (subscription.context.id, 3) };
       corrupt
         "obsolete epoch"
         { ownership with subscription = Some (subscription.context.id, 1) };
       corrupt
         "missing creator"
         { ownership with creator = Invocation (P.Id.Invocation.create ()) };
       now := Subscription_expiry_tests.at 1;
       [%test_eq: int] 1 (A.expire_subscriptions actor |> protocol_ok);
       let expired = Agent_session.Memory_backend.state backend in
       print_s
         [%sexp
           ((List.hd_exn expired.schedules).status : S.status)
         , ((List.hd_exn expired.subscriptions).result : P.Completion.t option)]);
  [%expect
    {|
    wrong source
    future epoch
    obsolete epoch
    missing creator
    (Cancelled (Expired))
    |}]
;;

let%expect_test "a claimed timer invalidates an uncommitted cancellation" =
  with_actor (fun _env _sw actor _writer backend ->
    A.change_moderator actor (Some (Setup.encode Setup.before)) |> protocol_ok |> ignore;
    let parent = add_claimed_job actor in
    with_event actor parent (fun owner commit ->
      let receipt, _ = create actor owner |> protocol_ok in
      A.select_schedule_mutations actor ~owner ~source:Setup.source ~receipts:[ receipt ]
      |> protocol_ok;
      save commit)
    |> protocol_ok
    |> ignore;
    let timer = List.hd_exn (Agent_session.Memory_backend.state backend).schedules in
    let result =
      with_event ~snapshot:Setup.after actor parent (fun owner commit ->
        let receipt =
          A.stage_schedule_mutation
            actor
            ~owner
            ~source:Setup.source
            ~previous:(Some timer)
            ~next:{ timer with status = S.Cancelled }
          |> protocol_ok
        in
        A.select_schedule_mutations
          actor
          ~owner
          ~source:Setup.source
          ~receipts:[ receipt ]
        |> protocol_ok;
        A.claim_schedule actor ~schedule_id:timer.id ~generation:0
        |> protocol_ok
        |> Option.value_exn
        |> ignore;
        save commit)
    in
    reject "stale cancellation" result;
    let state = Agent_session.Memory_backend.state backend in
    print_s [%sexp ((List.hd_exn state.schedules).status : S.status)]);
  [%expect
    {|
    ("stale cancellation" Conflict)
    Delivering
    |}]
;;
