open Core
open Fixtures
open Job_fixtures
module P = Agent_protocol
module S = P.Subscription

let source : I.observer = { script_id = "handoff"; source_sha256 = String.make 64 'a' }

let before =
  { (handoff_snapshot 0) with
    script_id = source.script_id
  ; script_source_hash = source.source_sha256
  }
;;

let after = { before with current_state = Session.Snapshot.Int 1 }
let encode = Agent_session.Runtime_builder.encode_moderator_snapshot

let make_subscription ?parent_job invocation =
  S.create
    { id = P.Id.Subscription.create ()
    ; session_id
    ; generation = 0
    ; invocation_id = invocation.I.context.id
    ; source = Some source
    ; parent_job
    ; kind = "fixture"
    ; created_at = timestamp
    ; deadline =
        (P.Timestamp.to_time_ns timestamp
         |> fun time ->
         Time_ns.add time (Time_ns.Span.of_int_sec 3600) |> P.Timestamp.of_time_ns)
    ; completion_schema = Some (`Object [ "type", `String "string" ])
    ; wake = Next_turn
    ; ingress_capability = None
    }
  |> protocol_ok
;;

let finish value completion =
  S.finish value ~expected_epoch:value.S.epoch ~now:timestamp completion
  |> protocol_ok
  |> fst
;;

let%expect_test
    "subscription reservations and terminal updates commit with their moderator owner"
  =
  List.iter
    [ `Accepted; `Rejected_save; `Cancelled; `Unselected; `Stopped ]
    ~f:(fun mode ->
      let reject_save = ref false in
      with_actor
        ~reject_save:(fun _ -> !reject_save)
        ~subscription_limits:
          { Agent_session.Staged_subscriptions.default_limits with max_active = 1 }
        (fun _env _sw actor writer backend ->
           A.change_moderator actor (Some (encode before)) |> protocol_ok |> ignore;
           let parent = add_claimed_job actor in
           let retained = ref None in
           let result =
             try
               A.with_job_execution
                 actor
                 ~job_id:parent.id
                 ~generation:0
                 ~attempt:parent.attempt
                 ~deadline:(Some deadline)
                 (fun services ->
                    services.execute ~invocation:(root parent) (fun ~dispatched:root ->
                      let attempted = make_subscription root in
                      assert (
                        Result.is_error
                          (A.stage_subscription_mutation
                             actor
                             ~owner:(J.Invocation root.context.id)
                             ~source
                             ~previous:None
                             ~next:attempted));
                      let invocation =
                        I.create
                          { root.context with
                            id = P.Id.Invocation.create ()
                          ; parent_job = None
                          ; parent_invocation = Some root.context.id
                          ; tool_name = "counter"
                          }
                        |> protocol_ok
                      in
                      let result =
                        services.moderator_execute ~invocation (fun ~dispatched ~commit ->
                          assert (Option.is_none dispatched.observation);
                          let owner = J.Invocation dispatched.context.id in
                          let value =
                            make_subscription
                              ~parent_job:(parent.id, parent.attempt)
                              dispatched
                          in
                          retained := Some (owner, value);
                          let stage previous next =
                            A.stage_subscription_mutation
                              actor
                              ~owner
                              ~source
                              ~previous
                              ~next
                          in
                          let first = stage None value |> protocol_ok in
                          assert (
                            Result.is_error
                              (stage
                                 None
                                 (make_subscription
                                    ~parent_job:(parent.id, parent.attempt)
                                    dispatched)));
                          A.abort_subscription_mutation actor ~owner ~receipt:first
                          |> protocol_ok;
                          let creation = stage None value |> protocol_ok in
                          let wrong = finish value (Succeeded `Null) in
                          assert (Result.is_error (stage (Some value) wrong));
                          let completed = finish value (Succeeded (`String "ready")) in
                          let completion = stage (Some value) completed |> protocol_ok in
                          let repeated = finish completed (Cancelled "too late") in
                          let repeat = stage (Some completed) repeated |> protocol_ok in
                          assert (
                            Result.is_error
                              (A.abort_subscription_mutation
                                 actor
                                 ~owner
                                 ~receipt:completion));
                          A.abort_subscription_mutation actor ~owner ~receipt:repeat
                          |> protocol_ok;
                          let current =
                            A.read_script_subscription
                              actor
                              ~owner
                              ~source
                              ~id:value.context.id
                            |> protocol_ok
                          in
                          assert (S.equal current completed);
                          assert (
                            Result.is_error
                              (A.read_script_subscription
                                 actor
                                 ~owner
                                 ~source:
                                   { source with source_sha256 = String.make 64 'b' }
                                 ~id:value.context.id));
                          assert (
                            List.is_empty
                              (Agent_session.Memory_backend.state backend).subscriptions);
                          assert (
                            Result.is_error
                              (A.select_subscription_mutations
                                 actor
                                 ~owner
                                 ~source
                                 ~receipts:[ completion ]));
                          assert (
                            Result.is_error
                              (A.select_subscription_mutations
                                 actor
                                 ~owner
                                 ~source
                                 ~receipts:[ creation; completion; completion ]));
                          let receipts =
                            match mode with
                            | `Unselected -> []
                            | _ -> [ creation; completion ]
                          in
                          A.select_subscription_mutations actor ~owner ~source ~receipts
                          |> protocol_ok;
                          let outcome =
                            match mode with
                            | `Cancelled -> I.Cancelled "handler cancelled"
                            | _ ->
                              I.Pending (Subscription value.context.id, `String "accepted")
                          in
                          let resolved =
                            I.resolve dispatched ~session_id ~generation:0 outcome
                            |> protocol_ok
                          in
                          (match mode with
                           | `Rejected_save -> reject_save := true
                           | `Stopped ->
                             A.stop actor ~attachment_id:writer.id ~mode:Cancel
                             |> protocol_ok
                             |> ignore
                           | _ -> ());
                          let result = commit ~resolved ~snapshot:after in
                          reject_save := false;
                          result)
                      in
                      Result.map result ~f:(fun () -> I.Complete `Null)))
             with
             | Eio.Cancel.Cancelled _ as exn ->
               (match mode with
                | `Stopped -> Error (handoff_error "session stopped")
                | _ -> raise exn)
           in
           reject_save := false;
           let saved = Agent_session.Memory_backend.state backend in
           (match mode, result with
            | (`Accepted | `Cancelled), Ok _ -> ()
            | (`Rejected_save | `Unselected | `Stopped), Error _ -> ()
            | _ -> failwith "unexpected owner commit result");
           let committed =
             match mode with
             | `Accepted -> true
             | _ -> false
           in
           [%test_eq: int] (if committed then 1 else 0) (List.length saved.subscriptions);
           let owner, original = Option.value_exn !retained in
           assert (
             Result.is_error
               (A.read_script_subscription actor ~owner ~source ~id:original.context.id));
           (match saved.subscriptions with
            | [ value ] ->
              assert (
                Option.equal
                  P.Completion.equal
                  (Some (Succeeded (`String "ready")))
                  value.result);
              assert (S.equal value (S.of_json (S.to_json value) |> protocol_ok));
              assert (
                Option.exists saved.moderator ~f:(Jsonaf.exactly_equal (encode after)))
            | [] -> ()
            | _ -> assert false);
           assert (
             Option.is_some
               (A.with_quiescent_state actor ~f:(fun _ -> Ok ()) |> protocol_ok));
           print_s
             [%sexp
               (mode
                : [ `Accepted | `Rejected_save | `Cancelled | `Unselected | `Stopped ])
             , (committed : bool)]));
  [%expect
    {|
    (Accepted true)
    (Rejected_save false)
    (Cancelled false)
    (Unselected false)
    (Stopped false)
    |}]
;;

let%expect_test
    "event subscription saves recheck a terminal winner committed after selection"
  =
  with_actor (fun _env _sw actor _writer backend ->
    A.change_moderator actor (Some (encode before)) |> protocol_ok |> ignore;
    let commit changes =
      let state = A.state actor |> protocol_ok in
      A.commit_extensions
        actor
        ~generation:0
        ~expected_revision:state.counters.revision
        changes
      |> protocol_ok
      |> ignore
    in
    let admitted = invocation_fixture () in
    let dispatched = I.dispatch admitted |> protocol_ok in
    let value = make_subscription dispatched in
    let resolved =
      I.resolve
        dispatched
        ~session_id
        ~generation:0
        (Pending (Subscription value.context.id, `String "accepted"))
      |> protocol_ok
    in
    commit
      [ Invocation admitted
      ; Invocation dispatched
      ; Subscription value
      ; Invocation resolved
      ];
    let parent = add_claimed_job actor in
    let event =
      Chat_response.Moderation.Event.Pre_tool_call
        { id = "watch"
        ; name = "fixture"
        ; args = `Null
        ; kind = Function
        ; payload_text = "null"
        ; meta = `Null
        }
    in
    let result =
      A.with_job_execution
        actor
        ~job_id:parent.id
        ~generation:0
        ~attempt:parent.attempt
        ~deadline:(Some deadline)
        (fun services ->
           services.claim_event
             ~event
             ~snapshot:(fun () -> Ok before)
             (fun ~executing ~event:_ ~execute:_ ~commit:save ->
                let owner = J.Moderator_event executing.context.id in
                let current =
                  A.read_script_subscription actor ~owner ~source ~id:value.context.id
                  |> protocol_ok
                in
                assert (S.equal value current);
                let future, _ =
                  S.finish value ~expected_epoch:0 ~now:value.context.deadline Expired
                  |> protocol_ok
                in
                assert (
                  Result.is_error
                    (A.stage_subscription_mutation
                       actor
                       ~owner
                       ~source
                       ~previous:(Some value)
                       ~next:future));
                let cancellation = finish value (Cancelled "watch cancelled") in
                let receipt =
                  A.stage_subscription_mutation
                    actor
                    ~owner
                    ~source
                    ~previous:(Some value)
                    ~next:cancellation
                  |> protocol_ok
                in
                A.select_subscription_mutations actor ~owner ~source ~receipts:[ receipt ]
                |> protocol_ok;
                (* A privileged completion adapter commits its result while this
             moderator's proposed checkpoint is still uncommitted. *)
                commit
                  [ Subscription (finish value (Succeeded (`String "first winner"))) ];
                save
                  ~snapshot:after
                  ~requests:
                    { request_turn = false
                    ; request_compaction = false
                    ; end_session = None
                    }))
    in
    (match result with
     | Error error -> print_s [%sexp (error.code : P.Error.code)]
     | Ok _ -> failwith "stale subscription transaction committed");
    let saved = Agent_session.Memory_backend.state backend in
    let winner = List.hd_exn saved.subscriptions in
    assert (Option.exists saved.moderator ~f:(Jsonaf.exactly_equal (encode before)));
    assert (
      Option.is_some (A.with_quiescent_state actor ~f:(fun _ -> Ok ()) |> protocol_ok));
    print_s [%sexp (winner.result : P.Completion.t option)]);
  [%expect
    {|
    Conflict
    ((Succeeded (String "first winner")))
    |}]
;;
