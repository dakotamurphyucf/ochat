open Core
open Fixtures
open Job_fixtures
module P = Agent_protocol
module Jobs = Agent_session.Script_job_service
module Notices = Agent_session.Script_notification_service
module Setup = Subscription_transaction_tests
module Timers = Schedule_transaction_tests
module Background = Background_execution_tests

let notices actor =
  Notices.create
    ~host:
      { create =
          (fun owner source ~correlation ~completion ~wake ~disclosure_pins ->
            A.create_script_notification
              ~disclosure_pins
              actor
              ~owner
              ~source
              ~correlation
              ~completion
              ~wake)
      ; get = (fun owner source id -> A.read_script_notification actor ~owner ~source ~id)
      ; select =
          (fun owner source receipts ->
            A.select_notification_mutations actor ~owner ~source ~receipts)
      ; abort =
          (fun owner receipt ->
            A.abort_notification_mutation actor ~owner ~receipt |> protocol_ok)
      }
;;

let jobs env actor current =
  Jobs.create
    ~env
    ~policy:Background.policy
    ~current_capabilities:(fun () -> !current)
    ~host:
      { stage = (fun _ _ -> Error (handoff_error "unused start"))
      ; select = (fun owner ids -> A.select_background_jobs actor ~owner ~ids)
      ; abort = (fun _ _ -> ())
      ; get = (fun owner id -> A.read_script_job actor ~owner ~id)
      ; materialize =
          (fun owner expected -> A.read_script_job_result actor ~owner ~expected)
      ; cancel = (fun owner id -> A.cancel_script_job actor ~owner ~id)
      }
;;

let%expect_test
    "notification job results retain capability checks during preparation and scoped \
     reads"
  =
  List.iter [ `Allowed; `Narrowed; `Revoked_at_prepare ] ~f:(fun mode ->
    with_actor (fun env _sw actor _writer backend ->
      A.change_moderator actor (Some (Setup.encode Setup.before)) |> protocol_ok |> ignore;
      let calls = ref 0 in
      let registry = native_registry calls ~raises:false in
      let narrow = C.select registry ~names:[] |> Background.cap in
      let current = ref registry in
      let selected =
        match mode with
        | `Narrowed -> narrow
        | _ -> registry
      in
      let request = Background.capture_tool registry Background.policy in
      let target =
        add_claimed_job actor ~payload:(Chat_response.Background_request.to_json request)
      in
      let completion = P.Completion.Succeeded (`String "PRIVATE-JOB-RESULT") in
      A.complete_background_job
        actor
        ~job_id:target.id
        ~generation:0
        ~attempt:target.attempt
        completion
      |> protocol_ok
      |> ignore;
      let parent = add_claimed_job actor in
      let retained = ref None in
      let notice_service = notices actor in
      let job_service = jobs env actor current in
      let result =
        Timers.with_event actor parent (fun owner commit ->
          Jobs.with_scope
            job_service
            ~owner
            ~selected
            ~error:P.Error.invalid_request
            (fun job_scope ->
               Notices.with_scope
                 notice_service
                 ~owner
                 ~source:Setup.source
                 ~selected
                 ~jobs:(Some job_scope)
                 ~error:P.Error.invalid_request
                 (fun scope ->
                    let open Result.Let_syntax in
                    let transaction = Notices.moderator_transaction scope in
                    let%bind receipt, value =
                      transaction.handlers.publish
                        ~correlation:
                          { key = "job"
                          ; invocation_id = None
                          ; work = Some (Job target.id)
                          }
                        ~completion
                        ~wake:No_wake
                      |> Result.map_error ~f:P.Error.invalid_request
                    in
                    retained := Some value.context.id;
                    let job_transaction = Jobs.moderator_transaction job_scope in
                    let%bind acknowledge_jobs =
                      job_transaction.prepare []
                      |> Result.map_error ~f:P.Error.invalid_request
                    in
                    assert (Result.is_error (job_transaction.handlers.get target.id));
                    (match mode with
                     | `Revoked_at_prepare -> current := narrow
                     | _ -> ());
                    let%bind acknowledge =
                      transaction.prepare [ receipt ]
                      |> Result.map_error ~f:P.Error.invalid_request
                    in
                    let%map () = Timers.save commit in
                    acknowledge_jobs ();
                    acknowledge ())))
      in
      (match mode, result with
       | `Allowed, Ok _ -> ()
       | (`Narrowed | `Revoked_at_prepare), Error _ -> ()
       | _ -> failwith "notification bypassed selected job capabilities");
      let state = A.state actor |> protocol_ok in
      assert_same_session_snapshot state (Agent_session.Memory_backend.state backend);
      [%test_eq: int] 0 !calls;
      let expected =
        match mode with
        | `Allowed -> 1
        | _ -> 0
      in
      [%test_eq: int] expected (List.length state.deliveries);
      (match mode with
       | `Allowed ->
         let read_checked = ref false in
         let denied =
           Timers.with_event ~snapshot:Setup.after actor parent (fun owner _commit ->
             Jobs.with_scope
               job_service
               ~owner
               ~selected:narrow
               ~error:P.Error.invalid_request
               (fun job_scope ->
                  Notices.with_scope
                    notice_service
                    ~owner
                    ~source:Setup.source
                    ~selected:narrow
                    ~jobs:(Some job_scope)
                    ~error:P.Error.invalid_request
                    (fun scope ->
                       let result =
                         (Notices.moderator_transaction scope).handlers.get
                           (Option.value_exn !retained)
                       in
                       match result with
                       | Error _ ->
                         read_checked := true;
                         Error (handoff_error "read denied as expected")
                       | Ok _ ->
                         failwith "narrowed scope exposed private notification content")))
         in
         assert (Result.is_error denied && !read_checked)
       | _ -> ());
      print_s
        [%sexp (mode : [ `Allowed | `Narrowed | `Revoked_at_prepare ]), (expected : int)]));
  [%expect
    {|
    (Allowed 1)
    (Narrowed 0)
    (Revoked_at_prepare 0)
    |}]
;;
