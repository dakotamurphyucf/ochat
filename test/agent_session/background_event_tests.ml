open Core
open Fixtures
open Job_fixtures
module E = Agent_protocol.Moderator_execution

let%expect_test
    "job event commits checkpoint and intent atomically and never replays failed effects"
  =
  List.iter [ false; true ] ~f:(fun fail_commit ->
    let rejected = ref false in
    with_actor
      ~reject_save:(fun state ->
        match
          fail_commit
          && (not !rejected)
          && List.exists state.state.moderator_executions ~f:(fun event ->
            match event.status with
            | Completed _ -> true
            | _ -> false)
        with
        | false -> false
        | true ->
          rejected := true;
          true)
      (fun _env _sw actor _writer backend ->
         let before =
           { (handoff_snapshot 0) with script_source_hash = String.make 64 'a' }
         in
         let after = { before with current_state = Session.Snapshot.Int 1 } in
         A.change_moderator
           actor
           (Some (Agent_session.Runtime_builder.encode_moderator_snapshot before))
         |> protocol_ok
         |> ignore;
         let job = add_claimed_job actor in
         let calls = ref 0 in
         let escaped = ref None in
         let event =
           Chat_response.Moderation.Event.Pre_tool_call
             { id = "fixture-call"
             ; name = "read_file"
             ; args = `Object []
             ; kind = Function
             ; payload_text = "{}"
             ; meta = `Null
             }
         in
         let result =
           A.with_job_execution
             actor
             ~job_id:job.id
             ~generation:job.generation
             ~attempt:job.attempt
             ~deadline:(Some deadline)
             (fun services ->
                let run () =
                  services.claim_event
                    ~event
                    ~snapshot:(fun () -> Ok before)
                    (fun ~executing ~event:_ ~execute ~commit ->
                       let observer = executing.context.source in
                       let invocation =
                         I.create
                           ~parent_event:executing.context.id
                           ~observer
                           { (invocation_fixture ()).context with
                             id = Agent_protocol.Id.Invocation.create ()
                           ; origin = Moderator
                           ; parent_job = None
                           ; parent_invocation = None
                           ; provider_call_id = None
                           ; call_entry_id = None
                           ; deadline = Some deadline
                           }
                         |> protocol_ok
                       in
                       let open Result.Let_syntax in
                       let%bind _ =
                         execute ~invocation (fun ~dispatched:_ ->
                           incr calls;
                           Ok (I.Complete (`String "effect happened")))
                       in
                       commit
                         ~snapshot:after
                         ~requests:
                           { request_turn = true
                           ; request_compaction = false
                           ; end_session = None
                           })
                in
                escaped := Some run;
                let result = run () in
                (match fail_commit with
                 | true -> reject "replay after failed commit" (run ())
                 | false -> ());
                result)
         in
         (match result with
          | Ok true -> print_endline "committed"
          | Ok false -> failwith "job event was skipped"
          | Error error -> print_s [%sexp (error.code : Agent_protocol.Error.code)]);
         reject "escaped event claim" ((Option.value_exn !escaped) ());
         [%test_eq: int] 1 !calls;
         let state = Agent_session.Memory_backend.state backend in
         let expected =
           match fail_commit with
           | true -> before
           | false -> after
         in
         assert (
           Option.exists
             state.moderator
             ~f:
               (Jsonaf.exactly_equal
                  (Agent_session.Runtime_builder.encode_moderator_snapshot expected)));
         let receipt = List.hd_exn state.moderator_executions in
         let restored = E.of_json (E.to_json receipt) |> protocol_ok in
         assert (E.equal receipt restored);
         print_s [%sexp (receipt.intent : E.intent option)];
         let corrupt_reference =
           { (Option.value_exn receipt.context.job) with
             job_id = Agent_protocol.Id.Job.create ()
           }
         in
         let corrupt =
           E.create { receipt.context with job = Some corrupt_reference } |> protocol_ok
         in
         assert (
           Result.is_error
             (Agent_session.Session_state.validate
                { state with moderator_executions = [ corrupt ] }));
         let downgraded =
           match E.to_json receipt with
           | `Object fields ->
             `Object
               (List.Assoc.add fields ~equal:String.equal "schema_version" (`Number "2"))
           | _ -> assert false
         in
         assert (Result.is_error (E.of_json downgraded))));
  [%expect
    {|
    committed
    ("escaped event claim" Conflict)
    (Pending)
    ("replay after failed commit" Conflict)
    Internal_error
    ("escaped event claim" Conflict)
    ()
    |}]
;;
