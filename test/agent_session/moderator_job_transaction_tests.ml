open Core
open Fixtures
open Job_fixtures
module M = Chat_response.Moderator_manager
module Jobs = Agent_session.Script_job_service
module Capacity = Agent_server.Job_capacity

let%expect_test "moderator saves select jobs before persistence and never reject after it"
  =
  List.iter [ `Accepted; `Rejected_save; `Revoke_before; `Revoke_after ] ~f:(fun mode ->
    let reject_save = ref false in
    with_actor
      ~reject_save:(fun _ -> !reject_save)
      (fun env _sw actor _writer backend ->
         let calls = ref 0 in
         let selected = native_registry calls ~raises:false in
         let registry = ref selected in
         let revoke () =
           registry
           := C.select selected ~names:[]
              |> Result.map_error ~f:(fun error -> error.C.message)
              |> Result.ok_or_failwith
         in
         let manager, _, _ =
           handoff_definition
             env
             ~capability_registry:selected
             ~declare_tool:false
             ~events:
               {| | `Pre_tool_call(p) ->
            Task.bind(Job.start_tool("read_file", `Object([])), fun id ->
              let ignored = state[0] <- state[0] + 1 in Task.pure(state))
            | _ -> Task.pure(state)|}
         in
         let before = M.identity_snapshot manager |> Result.ok_or_failwith in
         let encode = Agent_session.Runtime_builder.encode_moderator_snapshot in
         A.change_moderator actor (Some (encode before)) |> protocol_ok |> ignore;
         let capacity =
           Capacity.create
             ~limits:
               { daemon_total = 1
               ; per_principal = 1
               ; per_prompt = 1
               ; per_workspace = 1
               ; per_session = 1
               ; per_kind = 1
               ; max_nested_depth = 2
               }
         in
         let staged = ref None in
         let trace = ref [] in
         let record step = trace := step :: !trace in
         let host : Jobs.host =
           { stage =
               (fun owner request ->
                 let job =
                   Background_transaction_tests.stage actor backend capacity request owner
                 in
                 staged := Some job;
                 record "staged";
                 Ok job)
           ; select =
               (fun owner ids ->
                 record "selected";
                 A.select_background_jobs actor ~owner ~ids)
           ; abort =
               (fun owner id -> A.abort_background_job actor ~owner ~id |> protocol_ok)
           ; get = (fun owner id -> A.read_script_job actor ~owner ~id)
           ; materialize =
               (fun owner expected -> A.read_script_job_result actor ~owner ~expected)
           ; cancel = (fun owner id -> A.cancel_script_job actor ~owner ~id)
           }
         in
         let jobs =
           Jobs.create
             ~env
             ~policy:Chat_response.One_off_request.default_policy
             ~current_capabilities:(fun () -> !registry)
             ~host
         in
         let parent = add_claimed_job actor in
         let event =
           Chat_response.Moderation.Event.Pre_tool_call
             { id = "fixture"
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
             ~job_id:parent.id
             ~generation:parent.generation
             ~attempt:parent.attempt
             ~deadline:(Some deadline)
             (fun services ->
                services.claim_event
                  ~event
                  ~snapshot:(fun () -> Ok before)
                  (fun ~executing ~event:_ ~execute:_ ~commit ->
                     Jobs.with_scope
                       jobs
                       ~owner:(Moderator_event executing.context.id)
                       ~selected
                       ~error:handoff_error
                       (fun scope ->
                          M.handle_event_entries_transactional
                            ~jobs:(Jobs.moderator_transaction scope)
                            manager
                            ~session_id:(Agent_protocol.Id.Session.to_string session_id)
                            ~now_ms:0
                            ~history:[]
                            ~available_tools:[]
                            ~session_meta:`Null
                            ~event
                            ~authorize:(fun () -> Ok ())
                            ~on_tool_call:(fun ~name:_ ~args:_ ->
                              failwith "unexpected native call")
                            ~prepare_event:(fun ~outcome:_ ~snapshot ->
                              record "validated";
                              (match mode with
                               | `Revoke_before -> revoke ()
                               | _ -> ());
                              Ok
                                { M.persist =
                                    (fun () ->
                                      record "persist";
                                      (reject_save
                                       := match mode with
                                          | `Rejected_save -> true
                                          | _ -> false);
                                      let result =
                                        commit
                                          ~snapshot
                                          ~requests:
                                            { request_turn = false
                                            ; request_compaction = false
                                            ; end_session = None
                                            }
                                      in
                                      reject_save := false;
                                      (match mode, result with
                                       | `Revoke_after, Ok () -> revoke ()
                                       | _ -> ());
                                      Result.map_error result ~f:(fun error ->
                                        error.Agent_protocol.Error.message))
                                ; install = (fun () -> record "installed")
                                })
                          |> Result.map ~f:ignore
                          |> Result.map_error ~f:handoff_error)))
         in
         let committed =
           match mode with
           | `Accepted | `Revoke_after -> true
           | _ -> false
         in
         (match committed with
          | true -> ignore (protocol_ok result : bool)
          | false -> assert (Result.is_error result));
         let after = M.identity_snapshot manager |> Result.ok_or_failwith in
         (match committed, after.current_state with
          | true, Array [ Int 1 ] | false, Array [ Int 0 ] -> ()
          | _ -> failwith "manager state disagrees with durable commit");
         let saved = Agent_session.Memory_backend.state backend in
         assert (Option.exists saved.moderator ~f:(Jsonaf.exactly_equal (encode after)));
         let job = Option.value_exn !staged in
         [%test_eq: bool]
           committed
           (List.exists saved.jobs ~f:(fun saved ->
              Agent_protocol.Id.Job.equal saved.id job.id));
         let lease =
           Capacity.try_acquire capacity (Background_admission_tests.key actor job)
           |> protocol_ok
         in
         [%test_eq: bool] (not committed) (Option.is_some lease);
         Option.iter lease ~f:Capacity.release;
         [%test_eq: int] 0 !calls;
         print_s
           [%sexp
             (mode : [ `Accepted | `Rejected_save | `Revoke_before | `Revoke_after ])
           , (List.rev !trace : string list)
           , (committed : bool)]));
  [%expect
    {|
    (Accepted (staged validated selected persist installed) true)
    (Rejected_save (staged validated selected persist) false)
    (Revoke_before (staged validated) false)
    (Revoke_after (staged validated selected persist installed) true)
    |}]
;;
