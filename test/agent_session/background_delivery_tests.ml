open Core
open Fixtures
module P = Agent_protocol
module A = Agent_session.Session_actor
module B = Agent_session.Background_job_event
module Frame = Chat_response.Background_delivery
module Q = Agent_session.Queued_moderator_event
module S = Session.Moderator_state.Identity_snapshot

let%expect_test
    "generic completion binds source and result, saves atomically and cannot replay a \
     forged attempt"
  =
  let before = { (handoff_snapshot 0) with script_source_hash = String.make 64 'a' } in
  let observer : P.Invocation.observer =
    { script_id = before.script_id; source_sha256 = before.script_source_hash }
  in
  let parent = invocation_fixture () in
  let root =
    P.Invocation.create
      ~observer
      { parent.context with
        id = P.Id.Invocation.create ()
      ; parent_invocation = Some parent.context.id
      }
    |> protocol_ok
    |> P.Invocation.dispatch
    |> protocol_ok
    |> fun root ->
    P.Invocation.resolve root ~session_id ~generation:0 (Complete `Null) |> protocol_ok
  in
  let job : P.Job.t =
    { id = P.Id.Job.create ()
    ; session_id
    ; generation = 0
    ; kind = Async_tool
    ; payload = `Object []
    ; status = Succeeded
    ; retry_policy = Never
    ; attempt = 1
    ; created_at = timestamp
    ; started_at = Some timestamp
    ; next_run_at = None
    ; completed_at = Some timestamp
    ; result =
        Some
          (P.Completion.to_json
             (Succeeded (`Object [ "exact", `Number "9007199254740993" ])))
    ; delivery = Pending
    ; launch =
        Some
          { owner = Invocation root.context.id
          ; parent_job = None
          ; nested_depth = 0
          ; moderator_source = Some observer
          }
    ; progress = None
    }
  in
  let encode = Agent_session.Runtime_builder.encode_moderator_snapshot in
  let reject_save = ref false in
  Job_fixtures.with_actor
    ~reject_save:(fun _ -> !reject_save)
    ~prepare_state:(fun state ->
      { state with
        invocations = [ parent; root ]
      ; jobs = [ job ]
      ; moderator = Some (encode before)
      })
    (fun _ _ actor _ backend ->
       let current () = A.state actor |> protocol_ok in
       let initial = current () in
       let frame = B.frame ~state:initial ~observer job |> protocol_ok in
       let captured =
         Frame.capture frame |> Session.Snapshot.of_value |> Result.ok_or_failwith
       in
       let append =
         { before with
           queued_internal_events = before.queued_internal_events @ [ captured ]
         }
       in
       let deliver after =
         A.deliver_job
           ~expected:before
           ~expected_job:job
           actor
           ~job_id:job.id
           ~generation:0
           ~moderator_snapshot:(Some (encode after))
       in
       Job_fixtures.reject
         "forged checkpoint"
         (deliver { append with current_state = Session.Snapshot.Int 99 });
       reject_save := true;
       assert (Result.is_error (deliver append));
       reject_save := false;
       assert_same_session_snapshot initial (current ());
       assert_same_session_snapshot initial (Agent_session.Memory_backend.state backend);
       let delivered = deliver append |> protocol_ok in
       let saved = current () in
       let restored =
         Agent_session.Session_persistence.restore_snapshot
           (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t saved))
         |> store_ok
       in
       assert_same_session_snapshot saved restored;
       let checked = B.frame ~state:restored ~observer delivered |> protocol_ok in
       assert (Frame.equal frame checked);
       let source =
         Option.value_exn (Option.value_exn delivered.launch).moderator_source
       in
       assert (P.Invocation.equal_observer source observer);
       let reason state event =
         Q.delivery_retirement_reason
           ~state
           ~observer
           ~event
           ~subscription_expired:(fun _ -> Ok false)
         |> protocol_ok
       in
       [%test_eq: string option] None (reason restored captured);
       let forged =
         Frame.create ~source:observer { delivered with attempt = 2 }
         |> Result.ok_or_failwith
         |> Frame.capture
         |> Session.Snapshot.of_value
         |> Result.ok_or_failwith
       in
       [%test_eq: string option]
         (Some "background.stale_or_forged_delivery")
         (reason restored forged);
       let foreign = { observer with source_sha256 = String.make 64 'b' } in
       Job_fixtures.reject
         "foreign source"
         (B.frame ~state:restored ~observer:foreign delivered);
       let changed_pin =
         { delivered with
           launch =
             Some
               { (Option.value_exn delivered.launch) with
                 moderator_source = Some foreign
               }
         }
       in
       assert (
         Result.is_error
           (Agent_session.Job_launch.validate
              ~invocations:restored.invocations
              ~events:restored.moderator_executions
              ~jobs:restored.jobs
              changed_pin));
       assert (
         Result.is_error
           (Agent_session.Session_delta.apply restored (Job_changed changed_pin)));
       let legacy_job =
         { delivered with
           launch =
             Some { (Option.value_exn delivered.launch) with moderator_source = None }
         }
       in
       let legacy_root = P.Invocation.create root.context |> protocol_ok in
       assert (
         Option.is_none
           (B.source
              ~state:
                { restored with
                  invocations = [ parent; legacy_root ]
                ; jobs = [ legacy_job ]
                }
              legacy_job
            |> protocol_ok));
       let downgraded =
         match P.Job.to_json delivered with
         | `Object fields ->
           `Object
             (List.map fields ~f:(function
                | "launch", `Object launch ->
                  ( "launch"
                  , `Object
                      (List.Assoc.add
                         launch
                         ~equal:String.equal
                         "schema_version"
                         (`Number "1")) )
                | field -> field))
         | _ -> assert false
       in
       assert (Result.is_error (P.Job.of_json downgraded));
       let forged_claim =
         P.Moderator_execution.create
           { id = P.Id.Moderator_execution.create ()
           ; session_id
           ; generation = 0
           ; source = observer
           ; operation_id = None
           ; job = None
           ; phase = Internal_event
           ; event =
               `Object
                 [ ( "snapshot_sexp"
                   , `String (Sexp.to_string_mach (Session.Snapshot.sexp_of_t forged)) )
                 ]
           ; checkpoint_sha256 = String.make 64 'c'
           ; created_at = timestamp
           }
         |> protocol_ok
       in
       [%test_eq: string option]
         None
         (reason { restored with moderator_executions = [ forged_claim ] } captured);
       A.with_current_idle_queued_moderator_event_tools
         actor
         ~snapshot:(fun () -> Ok append)
         (fun ~executing:_ ~retirement_reason ~event:_ ~execute:_ ~commit ->
            [%test_eq: string option] None retirement_reason;
            commit
              ~snapshot:before
              ~requests:
                { request_turn = false; request_compaction = false; end_session = None })
       |> protocol_ok
       |> ignore;
       [%test_eq: string option]
         (Some "background.duplicate_delivery")
         (reason (current ()) captured);
       print_endline
         "failed save changed nothing; exact source/result survived restore; forged \
          claim did not consume real delivery");
  [%expect
    {|
    ("forged checkpoint" Conflict)
    ("foreign source" Permission_denied)
    failed save changed nothing; exact source/result survived restore; forged claim did not consume real delivery
    |}]
;;
