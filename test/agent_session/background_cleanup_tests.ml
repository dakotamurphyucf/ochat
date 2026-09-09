open Core
open Fixtures
open Job_fixtures

let%expect_test
    "completion recovery retires finished moderator owners without replaying their \
     callbacks"
  =
  List.iter [ `Handler; `Event ] ~f:(fun mode ->
    let reject_settlement = ref false in
    with_actor
      ~reject_save:(fun _ -> !reject_settlement)
      (fun _env _sw actor _writer backend ->
         let before =
           { (handoff_snapshot 0) with script_source_hash = String.make 64 'a' }
         in
         let after = { before with current_state = Session.Snapshot.Int 1 } in
         let encoded = Agent_session.Runtime_builder.encode_moderator_snapshot before in
         A.change_moderator actor (Some encoded) |> protocol_ok |> ignore;
         let job = add_claimed_job actor in
         let effects = ref 0
         and escaped = ref (fun () -> Ok ()) in
         let result =
           A.with_job_execution
             actor
             ~job_id:job.id
             ~generation:job.generation
             ~attempt:job.attempt
             ~deadline:(Some deadline)
             (fun services ->
                match mode with
                | `Handler ->
                  services.execute ~invocation:(root job) (fun ~dispatched:parent ->
                    let observer : I.observer =
                      { script_id = before.script_id
                      ; source_sha256 = before.script_source_hash
                      }
                    in
                    let invocation =
                      I.create
                        ~observer
                        { parent.context with
                          id = Agent_protocol.Id.Invocation.create ()
                        ; parent_job = None
                        ; parent_invocation = Some parent.context.id
                        ; tool_name = "counter"
                        }
                      |> protocol_ok
                    in
                    let result =
                      services.moderator_execute ~invocation (fun ~dispatched ~commit ->
                        incr effects;
                        let resolved =
                          I.resolve
                            dispatched
                            ~session_id
                            ~generation:job.generation
                            (Complete `Null)
                          |> protocol_ok
                        in
                        let save () = commit ~resolved ~snapshot:after in
                        escaped := save;
                        reject_settlement := true;
                        save ())
                    in
                    reject_settlement := false;
                    assert (Result.is_error (!escaped ()));
                    reject_settlement := true;
                    let open Result.Let_syntax in
                    let%map () = result in
                    I.Complete `Null)
                  |> Result.map ~f:ignore
                | `Event ->
                  let event =
                    Chat_response.Moderation.Event.Pre_tool_call
                      { id = "cleanup"
                      ; name = "read_file"
                      ; args = `Object []
                      ; kind = Function
                      ; payload_text = "{}"
                      ; meta = `Null
                      }
                  in
                  services.claim_event
                    ~event
                    ~snapshot:(fun () -> Ok before)
                    (fun ~executing:_ ~event:_ ~execute:_ ~commit ->
                       incr effects;
                       let save () =
                         commit
                           ~snapshot:after
                           ~requests:
                             { request_turn = true
                             ; request_compaction = false
                             ; end_session = None
                             }
                       in
                       escaped := save;
                       reject_settlement := true;
                       save ())
                  |> Result.map ~f:ignore)
         in
         assert (Result.is_error result);
         reject_settlement := false;
         A.complete_background_job
           actor
           ~job_id:job.id
           ~generation:job.generation
           ~attempt:job.attempt
           (Failed
              { code = "fixture.cleanup_failed"
              ; message = "checkpoint failed"
              ; retryable = false
              ; details = `Null
              })
         |> protocol_ok
         |> ignore;
         [%test_eq: int] 1 !effects;
         assert (Result.is_error (!escaped ()));
         let state = Agent_session.Memory_backend.state backend in
         assert (Option.exists state.moderator ~f:(Jsonaf.exactly_equal encoded));
         assert (
           List.for_all state.invocations ~f:(fun invocation ->
             match invocation.status with
             | Resolved _ | Published _ -> true
             | _ -> false));
         assert (
           List.for_all state.moderator_executions ~f:(fun event ->
             match event.status with
             | Interrupted _ | Failed _ -> true
             | _ -> false));
         (* Taking a new moderator owner proves cleanup released the old reservation. *)
         let next = add_claimed_job actor in
         A.with_job_execution
           actor
           ~job_id:next.id
           ~generation:next.generation
           ~attempt:next.attempt
           ~deadline:(Some deadline)
           (fun services ->
              services.claim_event
                ~event:
                  (Chat_response.Moderation.Event.Pre_tool_call
                     { id = "cleanup_probe"
                     ; name = "read_file"
                     ; args = `Object []
                     ; kind = Function
                     ; payload_text = "{}"
                     ; meta = `Null
                     })
                ~snapshot:(fun () -> Ok before)
                (fun ~executing:_ ~event:_ ~execute:_ ~commit ->
                   commit
                     ~snapshot:before
                     ~requests:
                       { request_turn = false
                       ; request_compaction = false
                       ; end_session = None
                       }))
         |> protocol_ok
         |> ignore;
         print_s [%sexp (mode : [ `Handler | `Event ])];
         print_endline
           "failed checkpoint retired; effect once; escaped commit rejected; next owner \
            admitted"));
  [%expect
    {|
    Handler
    failed checkpoint retired; effect once; escaped commit rejected; next owner admitted
    Event
    failed checkpoint retired; effect once; escaped commit rejected; next owner admitted
    |}]
;;
