open! Core
open Fixtures
module A = Agent_session.Session_actor
module I = Agent_protocol.Invocation
module N = Agent_session.Native_tool_invocation
module C = Chat_response.Tool_capability
module P = Agent_protocol

let authentication live =
  let principal =
    P.Principal.create
      ~id:principal_id
      ~authentication_kind:"local"
      ~scopes:(P.Scope.Set.of_list [ Send_messages ])
      ~attributes:[]
    |> protocol_ok
  in
  Operator_authorization.guarded ~principal ~is_current:(fun () -> Atomic.get live)
;;

let require_actor = function
  | Ok (Some actor) -> actor
  | Ok None -> failwith "genuine admitted execution lost its authentication"
  | Error failure -> raise_s [%sexp (failure : P.Error.t)]
;;

let current actor = assert (Operator_authorization.is_current actor)
let expired actor = assert (not (Operator_authorization.is_current actor))

(* Surface actual worker failures rather than hiding a failed callback assertion
   behind the fixture's outer deadline. This reports no history or proof data. *)
let await_worker_checkpoint actor checkpoint =
  Eio.Fiber.first
    (fun () -> Eio.Promise.await checkpoint)
    (fun () ->
       let rec watch () =
         let state = A.state actor |> protocol_ok in
         match state.active_operation, state.failure with
         | _, Some error ->
           raise_s [%sexp "worker failed before checkpoint", (error : P.Error.t)]
         | None, None -> failwith "worker ended before checkpoint"
         | Some _, None ->
           Eio.Fiber.yield ();
           watch ()
       in
       watch ())
;;

let%expect_test "original authentication follows real native owners and cannot escape" =
  List.iter [ `Success; `Failed_save ] ~f:(fun mode ->
    let live = Atomic.make true in
    let original = authentication live in
    let same_principal_other_proof = authentication (Atomic.make true) in
    let reject = ref false in
    let worker_actor = ref None
    and returned_actor = ref None in
    let entered, entered_u = Eio.Promise.create () in
    let release, release_u = Eio.Promise.create () in
    let calls = ref 0 in
    let on_call = ref (fun () -> ()) in
    let registry = native_registry calls ~raises:false ~on_call:(fun () -> !on_call ()) in
    Job_fixtures.with_actor
      ~reject_save:(fun _ -> !reject)
      (fun _env _sw actor writer _backend ->
         let worker =
           Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input:_ caps ->
             let operation_actor = caps.original_agent_actor () |> require_actor in
             worker_actor := Some operation_actor;
             current operation_actor;
             let exact_borrow = ref None in
             let execute ~invocation f =
               caps.with_invocation ~invocation (fun ~dispatched ~capabilities ->
                 exact_borrow := Some capabilities.borrow_agent_actor;
                 f ~dispatched ~capabilities)
             in
             (on_call
              := fun () ->
                   let scope = N.borrow () |> protocol_ok in
                   let parent = N.borrowed_invocation scope in
                   let parent_actor = N.borrowed_agent_actor scope |> require_actor in
                   returned_actor := Some parent_actor;
                   current parent_actor;
                   N.require_native_admission
                     scope
                     ~binding:
                       (C.find registry ~name:"read_file"
                        |> Result.map_error ~f:(fun error -> error.C.message)
                        |> Result.ok_or_failwith)
                   |> protocol_ok;
                   let child =
                     I.create
                       { parent.context with
                         id = P.Id.Invocation.create ()
                       ; origin = Script
                       ; provider_call_id = None
                       ; call_entry_id = None
                       ; parent_invocation = Some parent.context.id
                       }
                     |> protocol_ok
                   in
                   let parent_borrow = Option.value_exn !exact_borrow in
                   let child_actor = ref None in
                   N.execute_borrowed
                     scope
                     ~invocation:child
                     (fun ~dispatched ~capabilities ->
                        (* Both physical invocations are live. Equal principal and
                       concurrent lifetime do not authorize cross-association. *)
                        assert (Result.is_error (parent_borrow ~invocation:dispatched));
                        assert (
                          Result.is_error
                            (capabilities.borrow_agent_actor ~invocation:parent));
                        let proof =
                          capabilities.borrow_agent_actor ~invocation:dispatched
                          |> require_actor
                        in
                        child_actor := Some proof;
                        current proof;
                        Ok (I.Complete `Null))
                   |> protocol_ok
                   |> ignore;
                   expired (Option.value_exn !child_actor);
                   current parent_actor;
                   if [%equal: [ `Success | `Failed_save ]] mode `Failed_save
                   then reject := true);
             let call, invocation = publication_call caps () in
             let reference, invocation = native_context registry invocation in
             caps.commit_invocation_call ~invocation call |> protocol_ok;
             let result =
               N.run_scoped
                 ~execute
                 ~registry:(fun () -> registry)
                 ~reference
                 ~invocation
                 ~is_halted:(fun () -> false)
                 ~authorize:(fun _ _ -> Ok ())
                 ~prepare_output:(fun _ -> Ok (`String "disclosed"))
             in
             reject := false;
             (match mode, result with
              | `Success, Ok resolved ->
                caps.publish_invocation_output
                  ~invocation_id:resolved.context.id
                  (publication_output
                     caps
                     ~text:{|{"type":"complete","value":"disclosed"}|}
                     ())
                |> protocol_ok
              | `Failed_save, Error _ -> ()
              | `Success, Error _ | `Failed_save, Ok _ -> failwith "wrong save outcome");
             expired (Option.value_exn !returned_actor);
             current operation_actor;
             Eio.Promise.resolve entered_u ();
             Eio.Promise.await release;
             expired operation_actor;
             let state = A.state actor |> protocol_ok in
             Agent_session.Operation_worker.Completed
               { final_history =
                   Agent_session.History_codec.all_of_protocol
                     state.conversation.canonical_history
                   |> protocol_ok
               ; moderator_snapshot = None
               ; runtime_requests = []
               })
         in
         A.set_operation_worker actor (Some worker) |> protocol_ok;
         let connection =
           Agent_server.Connection_context.create_authenticated
             ~actor:original
             ~connection_id:"authority-test"
             ~transport:In_memory
             ~publish_notification:ignore
             ~max_attachments:1
         in
         let admitted =
           Agent_server.Connection_context.request_actor connection None |> protocol_ok
         in
         A.submit_message_authenticated
           actor
           ~actor:admitted
           ~attachment_id:writer.id
           actor_entry
         |> protocol_ok
         |> ignore;
         await_worker_checkpoint actor entered;
         Atomic.set live false;
         expired (Option.value_exn !worker_actor);
         current same_principal_other_proof;
         Eio.Promise.resolve release_u ();
         await_idle actor |> ignore;
         [%test_eq: int] 1 !calls));
  print_endline
    "real dispatcher; exact live invocation; failed save and retained proof expire; same \
     principal cannot renew";
  [%expect
    {| real dispatcher; exact live invocation; failed save and retained proof expire; same principal cannot renew |}]
;;

let%expect_test
    "connection provenance preserves legacy unbound and expired authentic admission"
  =
  let live = Atomic.make false in
  let proof = authentication live in
  let principal = Operator_authorization.principal proof in
  let connection authenticated =
    if authenticated
    then
      Agent_server.Connection_context.create_authenticated
        ~actor:proof
        ~connection_id:"expired-authenticated"
        ~transport:In_memory
        ~publish_notification:ignore
        ~max_attachments:1
    else
      Agent_server.Connection_context.create
        ~principal
        ~connection_id:"legacy-principal"
        ~transport:In_memory
        ~publish_notification:ignore
        ~max_attachments:1
  in
  let legacy = connection false
  and authentic = connection true in
  assert (Option.is_none (Agent_server.Connection_context.original_actor legacy));
  let retained =
    Agent_server.Connection_context.original_actor authentic |> Option.value_exn
  in
  assert (phys_equal retained proof);
  expired retained;
  Job_fixtures.with_actor (fun _env _sw actor writer _backend ->
    let calls = ref 0 in
    let worker =
      Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
        Int.incr calls;
        assert (Option.is_none (caps.original_agent_actor () |> protocol_ok));
        Agent_session.Operation_worker.Completed
          { final_history = input.history
          ; moderator_snapshot = None
          ; runtime_requests = []
          })
    in
    A.set_operation_worker actor (Some worker) |> protocol_ok;
    let expired_request =
      Agent_server.Connection_context.request_actor authentic None |> protocol_ok
    in
    assert (
      Result.is_error
        (A.submit_message_authenticated
           actor
           ~actor:expired_request
           ~attachment_id:writer.id
           actor_entry));
    [%test_eq: int] 0 !calls;
    A.submit_message
      actor
      ~submitting_principal:principal.id
      ~attachment_id:writer.id
      actor_entry
    |> protocol_ok
    |> ignore;
    await_idle actor |> ignore;
    [%test_eq: int] 1 !calls);
  print_endline "None remains unbound; expired Some is retained and rejects before worker";
  [%expect {| None remains unbound; expired Some is retained and rejects before worker |}]
;;

let%expect_test
    "acknowledged jobs retain original custody across native and background moderator \
     owners"
  =
  List.iter [ true; false ] ~f:(fun acknowledge ->
    let original_live = Atomic.make true in
    let proof = authentication original_live in
    let on_call = ref (fun () -> ()) in
    let registry =
      native_registry (ref 0) ~raises:false ~on_call:(fun () -> !on_call ())
    in
    let request =
      Background_execution_tests.capture_tool
        registry
        Chat_response.One_off_request.default_policy
    in
    let selected_job = ref None
    and parent_actor = ref None in
    let published = ref false
    and aborted = ref false in
    let ready, ready_u = Eio.Promise.create () in
    let release, release_u = Eio.Promise.create () in
    Job_fixtures.with_actor (fun env _sw actor writer _backend ->
      let manager, _, _ = handoff_definition ~capability_registry:registry env in
      let before =
        Chat_response.Moderator_manager.identity_snapshot manager |> Result.ok_or_failwith
      in
      A.change_moderator actor (Some (Agent_session.Moderator_checkpoint.encode before))
      |> protocol_ok
      |> ignore;
      let worker =
        Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input:_ caps ->
          (on_call
           := fun () ->
                let scope = N.borrow () |> protocol_ok in
                parent_actor := Some (N.borrowed_agent_actor scope |> require_actor);
                let owner = P.Job.Invocation (N.borrowed_invocation scope).context.id in
                let job =
                  A.prepare_background_job_launch actor ~owner request |> protocol_ok
                in
                selected_job := Some job;
                A.stage_background_job
                  actor
                  ~job
                  ~capacity:
                    { publish = (fun () -> published := true)
                    ; abort = (fun () -> aborted := true)
                    }
                |> protocol_ok;
                assert (not !published);
                A.select_background_jobs
                  actor
                  ~owner
                  ~ids:(if acknowledge then [ job.id ] else [])
                |> protocol_ok);
          let call, invocation = publication_call caps () in
          let reference, invocation = native_context registry invocation in
          caps.commit_invocation_call ~invocation call |> protocol_ok;
          let resolved =
            N.run_scoped
              ~execute:caps.with_invocation
              ~registry:(fun () -> registry)
              ~reference
              ~invocation
              ~is_halted:(fun () -> false)
              ~authorize:(fun _ _ -> Ok ())
              ~prepare_output:(fun _ -> Ok (`String "disclosed"))
            |> protocol_ok
          in
          caps.publish_invocation_output
            ~invocation_id:resolved.context.id
            (publication_output caps ~text:{|{"type":"complete","value":"disclosed"}|} ())
          |> protocol_ok;
          expired (Option.value_exn !parent_actor);
          Eio.Promise.resolve ready_u ();
          Eio.Promise.await release;
          let state = A.state actor |> protocol_ok in
          Agent_session.Operation_worker.Completed
            { final_history =
                Agent_session.History_codec.all_of_protocol
                  state.conversation.canonical_history
                |> protocol_ok
            ; moderator_snapshot = Some (Agent_session.Moderator_checkpoint.encode before)
            ; runtime_requests = []
            })
      in
      A.set_operation_worker actor (Some worker) |> protocol_ok;
      A.submit_message_authenticated
        actor
        ~actor:proof
        ~attachment_id:writer.id
        actor_entry
      |> protocol_ok
      |> ignore;
      await_worker_checkpoint actor ready;
      let job = Option.value_exn !selected_job in
      if not acknowledge
      then (
        assert ((not !published) && !aborted);
        assert (
          Result.is_error (A.claim_job actor ~job_id:job.id ~generation:job.generation)))
      else (
        assert (!published && not !aborted);
        let job =
          A.claim_job actor ~job_id:job.id ~generation:job.generation
          |> protocol_ok
          |> Option.value_exn
        in
        let saved = ref None in
        A.with_job_execution
          actor
          ~job_id:job.id
          ~generation:job.generation
          ~attempt:job.attempt
          ~deadline:(Some Job_fixtures.deadline)
          (fun services ->
             services.execute
               ~invocation:(Job_fixtures.root job)
               (fun ~dispatched:parent ~capabilities ->
                  let actor_proof =
                    capabilities.borrow_agent_actor ~invocation:parent |> require_actor
                  in
                  saved := Some actor_proof;
                  current actor_proof;
                  let observer : I.observer =
                    { script_id = before.script_id
                    ; source_sha256 = before.script_source_hash
                    }
                  in
                  let moderator =
                    I.create
                      ~observer
                      { parent.context with
                        id = P.Id.Invocation.create ()
                      ; parent_job = None
                      ; parent_invocation = Some parent.context.id
                      ; tool_name = "counter"
                      ; capability_fingerprint = C.fingerprint registry
                      }
                    |> protocol_ok
                  in
                  services.moderator_execute
                    ~invocation:moderator
                    (fun ~dispatched ~capabilities ~commit ->
                       let moderator_actor =
                         capabilities.borrow_agent_actor ~invocation:dispatched
                         |> require_actor
                       in
                       current moderator_actor;
                       N.with_dispatched_scope
                         ~execute:services.execute
                         ~moderator_execute:services.moderator_execute
                         ~capabilities
                         ~selected:registry
                         ~invocation:dispatched
                         (fun () ->
                            let child =
                              I.create
                                { dispatched.context with
                                  id = P.Id.Invocation.create ()
                                ; origin = Moderator
                                ; parent_invocation = Some dispatched.context.id
                                ; parent_job = None
                                ; tool_name = "read_file"
                                }
                              |> protocol_ok
                            in
                            let reference, child = native_context registry child in
                            let child = I.create ~observer child.context |> protocol_ok in
                            let observed = ref false in
                            (on_call
                             := fun () ->
                                  let borrowed = N.borrow () |> protocol_ok in
                                  current
                                    (N.borrowed_agent_actor borrowed |> require_actor);
                                  observed := true);
                            N.run_scoped
                              ~execute:services.execute
                              ~registry:(fun () -> registry)
                              ~reference
                              ~invocation:child
                              ~is_halted:(fun () -> false)
                              ~authorize:(fun _ _ -> Ok ())
                              ~prepare_output:(fun _ -> Ok `Null)
                            |> protocol_ok
                            |> ignore;
                            assert !observed;
                            Ok ())
                       |> protocol_ok;
                       let resolved =
                         I.resolve
                           dispatched
                           ~session_id
                           ~generation:job.generation
                           (Complete `Null)
                         |> protocol_ok
                       in
                       commit ~resolved ~snapshot:before)
                  |> protocol_ok;
                  current actor_proof;
                  Ok (I.Complete `Null))
             |> Result.map ~f:ignore)
        |> protocol_ok;
        expired (Option.value_exn !saved);
        Job_fixtures.complete actor job |> protocol_ok |> ignore);
      Eio.Promise.resolve release_u ();
      await_idle actor |> ignore));
  print_endline
    "only durable selected launch publishes; original survives parent callback; \
     background moderator native child borrows exact job owner";
  [%expect
    {| only durable selected launch publishes; original survives parent callback; background moderator native child borrows exact job owner |}]
;;

let%expect_test
    "authenticated initialization owns an explicit expiring proof and acknowledged job \
     transfer"
  =
  let live = Atomic.make true in
  let original = authentication live in
  Job_fixtures.with_actor
    ~start:false
    ~prepare_state:(fun state ->
      { state with runtime_initialization = Pending { fresh_history = true } })
    (fun _env _sw actor writer _backend ->
       let retained = ref None
       and transferred_job = ref None in
       let base = Runtime_lease_tests.runtime ~close:ignore () in
       let owner =
         Agent_server.Runtime_owner.create_with_authenticated_run_unload
           ~before_unload:(fun ~closing:_ -> Ok ())
           ~actor
           ~initial:None
           ~build:(fun ~original_actor preparation ->
             assert (Option.is_none preparation);
             let original_actor = Option.value_exn original_actor in
             assert (phys_equal original_actor original);
             let expected = A.state actor |> protocol_ok in
             let scope =
               A.begin_initialization_authenticated actor ~actor:original_actor ~expected
               |> protocol_ok
             in
             Exn.protect
               ~finally:(fun () -> A.end_initialization actor ~scope |> protocol_ok)
               ~f:(fun () ->
                 let proof = A.initialization_agent_actor actor ~scope |> require_actor in
                 retained := Some proof;
                 current proof;
                 let job : P.Job.t =
                   { id = P.Id.Job.create ()
                   ; session_id
                   ; generation = expected.identity.generation
                   ; kind = Model_call
                   ; payload = `Object []
                   ; status = Queued
                   ; retry_policy = Never
                   ; attempt = 0
                   ; created_at = timestamp
                   ; started_at = None
                   ; next_run_at = None
                   ; completed_at = None
                   ; result = None
                   ; delivery = Pending
                   ; launch = None
                   ; inference_origin = None
                   ; progress = None
                   }
                 in
                 let job =
                   A.add_initialization_model_job actor ~scope job |> protocol_ok
                 in
                 transferred_job := Some job;
                 A.complete_initialization actor ~scope ~candidate:expected
                 |> protocol_ok
                 |> ignore;
                 Ok base))
       in
       Exn.protect
         ~finally:(fun () -> Agent_server.Runtime_owner.close_and_wait owner)
         ~f:(fun () ->
           Agent_server.Runtime_owner.ensure_loaded_authenticated owner ~actor:original
           |> protocol_ok;
           expired (Option.value_exn !retained);
           current original;
           A.start actor ~actor:original ~attachment_id:writer.id |> protocol_ok |> ignore;
           let queued = Option.value_exn !transferred_job in
           let job =
             A.claim_job actor ~job_id:queued.id ~generation:queued.generation
             |> protocol_ok
             |> Option.value_exn
           in
           Job_fixtures.with_job actor job (fun ~job:_ ~execute ->
             execute ~invocation:(Job_fixtures.root job) (fun ~dispatched ~capabilities ->
               let transferred =
                 capabilities.borrow_agent_actor ~invocation:dispatched |> require_actor
               in
               current transferred;
               Atomic.set live false;
               expired transferred;
               assert (
                 Result.is_error (capabilities.borrow_agent_actor ~invocation:dispatched));
               Ok (I.Complete `Null)))
           |> protocol_ok
           |> ignore;
           Job_fixtures.complete actor job |> protocol_ok |> ignore));
  print_endline
    "actual authenticated build and actor initialization; retained proof expires; \
     acknowledged job has independent original custody";
  [%expect
    {| actual authenticated build and actor initialization; retained proof expires; acknowledged job has independent original custody |}]
;;

let%expect_test
    "revocation during a real authenticated constructor denies initialization publication"
  =
  let live = Atomic.make true in
  let original = authentication live in
  Job_fixtures.with_actor
    ~start:false
    ~prepare_state:(fun state ->
      { state with runtime_initialization = Pending { fresh_history = true } })
    (fun _env sw actor writer _backend ->
       let entered, entered_u = Eio.Promise.create () in
       let proceed, proceed_u = Eio.Promise.create () in
       let finished, finished_u = Eio.Promise.create () in
       let closed = ref 0
       and completion_rejected = ref false in
       let base = Runtime_lease_tests.runtime ~close:(fun () -> Int.incr closed) () in
       let owner =
         Agent_server.Runtime_owner.create_with_authenticated_run_unload
           ~actor
           ~initial:None
           ~before_unload:(fun ~closing:_ -> Ok ())
           ~build:(fun ~original_actor preparation ->
             assert (Option.is_none preparation);
             let proof = Option.value_exn original_actor in
             assert (phys_equal proof original);
             let expected = A.state actor |> protocol_ok in
             let scope =
               A.begin_initialization_authenticated actor ~actor:proof ~expected
               |> protocol_ok
             in
             Exn.protect
               ~finally:(fun () -> A.end_initialization actor ~scope |> protocol_ok)
               ~f:(fun () ->
                 let borrowed =
                   A.initialization_agent_actor actor ~scope |> require_actor
                 in
                 Eio.Promise.resolve entered_u ();
                 Eio.Promise.await proceed;
                 expired borrowed;
                 (match A.complete_initialization actor ~scope ~candidate:expected with
                  | Error error ->
                    [%test_eq: P.Error.code] Permission_denied error.code;
                    completion_rejected := true
                  | Ok _ -> failwith "expired initializer published Ready");
                 Ok base))
       in
       Exn.protect
         ~finally:(fun () -> Agent_server.Runtime_owner.close_and_wait owner)
         ~f:(fun () ->
           Eio.Fiber.fork ~sw (fun () ->
             Eio.Promise.resolve
               finished_u
               (Agent_server.Runtime_owner.ensure_loaded_authenticated
                  owner
                  ~actor:original));
           Eio.Promise.await entered;
           Atomic.set live false;
           Eio.Promise.resolve proceed_u ();
           (match Eio.Promise.await finished with
            | Error error -> [%test_eq: P.Error.code] Permission_denied error.code
            | Ok () -> failwith "expired constructor installed runtime");
           assert !completion_rejected;
           [%test_eq: int] 1 !closed;
           assert (not (Agent_server.Runtime_owner.is_loaded owner));
           let state = A.state actor |> protocol_ok in
           (match state.runtime_initialization with
            | Pending _ -> ()
            | Ready -> assert false);
           match A.start actor ~actor:original ~attachment_id:writer.id with
           | Error error -> [%test_eq: P.Error.code] Permission_denied error.code
           | Ok _ -> failwith "expired queued Start was admitted"));
  print_endline
    "expired original denies Ready, closes returned runtime before installation, and \
     rejects actual actor Start";
  [%expect
    {| expired original denies Ready, closes returned runtime before installation, and rejects actual actor Start |}]
;;

let%expect_test "worker return revokes custody even when terminal persistence fails" =
  let original = authentication (Atomic.make true) in
  let retained = ref None
  and borrower = ref None in
  let reject = ref false in
  let failed, failed_u = Eio.Promise.create () in
  Job_fixtures.with_actor
    ~reject_save:(fun _ ->
      if !reject && Option.is_none (Eio.Promise.peek failed)
      then Eio.Promise.resolve failed_u ();
      !reject)
    (fun _env _sw actor writer _backend ->
       let worker =
         Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
           retained := Some (caps.original_agent_actor () |> require_actor);
           borrower := Some caps.original_agent_actor;
           current (Option.value_exn !retained);
           reject := true;
           Agent_session.Operation_worker.Completed
             { final_history = input.history
             ; moderator_snapshot = None
             ; runtime_requests = []
             })
       in
       A.set_operation_worker actor (Some worker) |> protocol_ok;
       A.submit_message_authenticated
         actor
         ~actor:original
         ~attachment_id:writer.id
         actor_entry
       |> protocol_ok
       |> ignore;
       Eio.Promise.await failed;
       expired (Option.value_exn !retained);
       assert (Result.is_error ((Option.value_exn !borrower) ()));
       current original;
       let state = A.state actor |> protocol_ok in
       assert (Option.is_some state.active_operation);
       reject := false);
  print_endline
    "failed terminal save retains durable operation but cannot retain or reissue \
     physical worker authority";
  [%expect
    {| failed terminal save retains durable operation but cannot retain or reissue physical worker authority |}]
;;

let%expect_test
    "runtime retirement cancels the actual worker and revokes a retained proof"
  =
  let original = authentication (Atomic.make true) in
  let retained = ref None in
  let entered, entered_u = Eio.Promise.create () in
  let never, _ = Eio.Promise.create () in
  Job_fixtures.with_actor (fun _env _sw actor writer _backend ->
    let worker =
      Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input:_ caps ->
        retained := Some (caps.original_agent_actor () |> require_actor);
        Eio.Promise.resolve entered_u ();
        Eio.Promise.await never)
    in
    A.set_operation_worker actor (Some worker) |> protocol_ok;
    A.submit_message_authenticated
      actor
      ~actor:original
      ~attachment_id:writer.id
      actor_entry
    |> protocol_ok
    |> ignore;
    Eio.Promise.await entered;
    current (Option.value_exn !retained);
    let retirement = A.retire_runtime_worker actor ~closing:false |> protocol_ok in
    expired (Option.value_exn !retained);
    Agent_session.Session_actor.Runtime_retirement.await retirement |> protocol_ok;
    expired (Option.value_exn !retained);
    current original);
  print_endline
    "retirement revokes before joined cancellation; original authentication remains \
     current";
  [%expect
    {| retirement revokes before joined cancellation; original authentication remains current |}]
;;

let%expect_test
    "queued authentication is neither arbitrarily selected nor downgraded after expiry"
  =
  List.iter [ `Mixed; `Expired ] ~f:(fun case ->
    Job_fixtures.with_actor (fun _env _sw actor writer _backend ->
      let before =
        { Subscription_transaction_tests.before with
          queued_internal_events =
            [ Session.Snapshot.Variant
                ("Internal_event", [ Variant ("String", [ String "tick" ]) ])
            ]
        }
      in
      let after = { before with queued_internal_events = [] } in
      let encode = Agent_session.Runtime_builder.encode_moderator_snapshot in
      A.change_moderator actor (Some (encode before)) |> protocol_ok |> ignore;
      let count = ref 0 in
      let worker =
        Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
          Int.incr count;
          assert (Option.is_none (caps.original_agent_actor () |> protocol_ok));
          Agent_session.Operation_worker.Completed
            { final_history = input.history
            ; moderator_snapshot = Some (encode after)
            ; runtime_requests = []
            })
      in
      A.set_operation_worker actor (Some worker) |> protocol_ok;
      let entries =
        match case with
        | `Mixed -> 3
        | `Expired -> 1
      in
      let reservation = A.reserve_history_block actor ~count:entries |> protocol_ok in
      let first = Int64.to_int_exn reservation.first_sequence in
      let lives = List.init entries ~f:(fun _ -> Atomic.make true) in
      let proofs = List.map lives ~f:authentication in
      assert (
        A.with_idle_queued_moderator_event actor ~snapshot:before (fun ~event:_ ~commit ->
          List.iteri proofs ~f:(fun i proof ->
            let entry = Pending_input_tests.occurrence (first + i) (Int.to_string i) in
            let submitted =
              A.submit_message_authenticated
                actor
                ~actor:proof
                ~attachment_id:writer.id
                entry
              |> protocol_ok
            in
            match submitted.disposition with
            | Deferred -> ()
            | Started -> failwith "borrowed event did not defer input");
          commit
            ~snapshot:after
            ~requests:
              { request_turn = false; request_compaction = false; end_session = None })
        |> protocol_ok);
      let queued = A.state actor |> protocol_ok in
      [%test_eq: int] entries (List.length queued.conversation.deferred_user_entries);
      List.iter proofs ~f:current;
      match case with
      | `Mixed ->
        assert (A.apply_moderator_follow_up actor |> protocol_ok);
        await_idle actor |> ignore;
        [%test_eq: int] 1 !count
      | `Expired ->
        List.iter lives ~f:(fun live -> Atomic.set live false);
        (match A.apply_moderator_follow_up actor with
         | Error error -> [%test_eq: P.Error.code] Permission_denied error.code
         | Ok _ -> failwith "expired pending authentication became legacy unbound");
        let unchanged = A.state actor |> protocol_ok in
        [%test_eq: int64] queued.counters.revision unchanged.counters.revision;
        [%test_eq: int] 1 (List.length unchanged.conversation.deferred_user_entries);
        [%test_eq: int] 0 !count));
  print_endline
    "ambiguous same-principal batch is unbound; expired single admission rejects before \
     persistence";
  [%expect
    {| ambiguous same-principal batch is unbound; expired single admission rejects before persistence |}]
;;

let%expect_test
    "physical journal reopen retains inputs and creator but restores no operator \
     authority"
  =
  let module Store = Agent_store in
  let module Persistence = Agent_session.Session_persistence in
  let original = authentication (Atomic.make true) in
  let retained = ref None in
  with_actor_workspace (fun env workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let directory =
      Filename.concat workspace_instance.canonical_root.native_path "authority-journal"
    in
    let snapshots =
      Filename.concat workspace_instance.canonical_root.native_path "authority-snapshots"
    in
    Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / snapshots);
    let attachment =
      Eio.Switch.run (fun sw ->
        let journal =
          Store.Journal.create
            ~env
            ~directory
            ~max_payload_length:1048576
            ~max_segment_bytes:4194304L
            ~max_segment_frames:32
          |> store_ok
        in
        let writer =
          Store.Commit_writer.create
            ~sw
            ~journal
            ~session_id
            ~next_transaction_sequence:1L
            ~previous_transaction_hash:None
            ~queue_capacity:8
          |> store_ok
        in
        let durable =
          Run_persistence_tests.create_persistence
            writer
            (Persistence.Restored.authored initial)
            ~previous_transaction_hash:None
        in
        let actor =
          Run_persistence_tests.create_actor
            env
            sw
            initial
            (Persistence.actor_persistence durable)
        in
        Exn.protect
          ~finally:(fun () ->
            A.shutdown actor;
            Store.Commit_writer.close writer)
          ~f:(fun () ->
            let worker =
              Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
                retained := Some (caps.original_agent_actor () |> require_actor);
                Agent_session.Operation_worker.Completed
                  { final_history = input.history
                  ; moderator_snapshot = None
                  ; runtime_requests = []
                  })
            in
            A.set_operation_worker actor (Some worker) |> protocol_ok;
            let writer, _ =
              A.attach actor ~mode:Read_write ~subscribe:false |> protocol_ok
            in
            A.start actor ~attachment_id:writer.id |> protocol_ok |> ignore;
            A.submit_message_authenticated
              actor
              ~actor:original
              ~attachment_id:writer.id
              actor_entry
            |> protocol_ok
            |> ignore;
            await_idle actor |> ignore;
            expired (Option.value_exn !retained);
            writer.id))
    in
    Eio.Switch.run (fun sw ->
      let journal =
        Store.Journal.open_existing
          ~env
          ~directory
          ~max_payload_length:1048576
          ~max_segment_bytes:4194304L
          ~max_segment_frames:32
        |> store_ok
      in
      let recovered =
        Store.Recovery.load
          ~env
          ~journal
          ~snapshot_directory:snapshots
          ~max_snapshot_payload_length:1048576
          ~session_id
          ~initial:(Persistence.Restored.authored initial)
          ~restore_snapshot:(Persistence.restore_snapshot ~limits:document_limits)
          ~apply:(Persistence.apply_transaction ~limits:document_limits)
          ~validate_transaction:(Persistence.validate_transaction ~limits:document_limits)
          ~validate:Persistence.validate
        |> store_ok
      in
      let state = Persistence.Restored.state recovered.state in
      assert (
        Option.equal
          P.Id.Principal.equal
          state.identity.creating_principal
          (Some principal_id));
      assert (
        List.mem
          state.conversation.canonical_history
          actor_entry
          ~equal:P.History.equal_entry);
      let writer =
        Store.Commit_writer.create
          ~sw
          ~journal
          ~session_id
          ~next_transaction_sequence:(Int64.succ recovered.latest_transaction_sequence)
          ~previous_transaction_hash:recovered.latest_transaction_hash
          ~queue_capacity:8
        |> store_ok
      in
      let durable =
        Run_persistence_tests.create_persistence
          writer
          recovered.state
          ~previous_transaction_hash:recovered.latest_transaction_hash
      in
      let actor =
        Run_persistence_tests.create_actor
          env
          sw
          state
          (Persistence.actor_persistence durable)
      in
      Exn.protect
        ~finally:(fun () ->
          A.shutdown actor;
          Store.Commit_writer.close writer)
        ~f:(fun () ->
          let calls = ref 0 in
          let worker =
            Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
              Int.incr calls;
              assert (Option.is_none (caps.original_agent_actor () |> protocol_ok));
              Agent_session.Operation_worker.Completed
                { final_history = input.history
                ; moderator_snapshot = None
                ; runtime_requests = []
                })
          in
          A.set_operation_worker actor (Some worker) |> protocol_ok;
          let reservation = A.reserve_history_block actor ~count:1 |> protocol_ok in
          let entry =
            Pending_input_tests.occurrence
              (Int64.to_int_exn reservation.first_sequence)
              "after physical reopen"
          in
          A.submit_message
            actor
            ~submitting_principal:principal_id
            ~attachment_id:attachment
            entry
          |> protocol_ok
          |> ignore;
          await_idle actor |> ignore;
          [%test_eq: int] 1 !calls;
          expired (Option.value_exn !retained);
          current original)));
  print_endline
    "distinct writer/actor replay exact input and creating principal; original and \
     legacy metadata cannot resurrect execution custody";
  [%expect
    {| distinct writer/actor replay exact input and creating principal; original and legacy metadata cannot resurrect execution custody |}]
;;

let%expect_test
    "authenticated Start lends only its exact lifecycle event and ACKed no-input intent"
  =
  List.iter [ `Live; `Expired_original; `Legacy; `Rejected_intent ] ~f:(fun mode ->
    let live = Atomic.make true in
    let original = authentication live in
    let duplicate = authentication (Atomic.make true) in
    let reject = ref false in
    Job_fixtures.with_actor
      ~start:false
      ~reject_save:(fun _ ->
        let reject_now = !reject in
        reject := false;
        reject_now)
      (fun _env _sw actor writer _backend ->
         let snapshot = Subscription_transaction_tests.before in
         let encode = Agent_session.Runtime_builder.encode_moderator_snapshot in
         A.change_moderator actor (Some (encode snapshot)) |> protocol_ok |> ignore;
         (match mode with
          | `Legacy -> A.start actor ~attachment_id:writer.id
          | `Live | `Expired_original | `Rejected_intent ->
            A.start ~actor:original actor ~attachment_id:writer.id)
         |> protocol_ok
         |> ignore;
         (* A duplicate Start must neither upgrade legacy admission nor replace
           an original whose authentication later expires. *)
         A.start ~actor:duplicate actor ~attachment_id:writer.id |> protocol_ok |> ignore;
         if
           [%equal: [ `Live | `Expired_original | `Legacy | `Rejected_intent ]]
             mode
             `Expired_original
         then Atomic.set live false;
         let runs = ref 0 in
         let worker =
           Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
             Int.incr runs;
             current (caps.original_agent_actor () |> require_actor);
             Agent_session.Operation_worker.Completed
               { final_history = input.history
               ; moderator_snapshot = Some (encode snapshot)
               ; runtime_requests = []
               })
         in
         A.set_operation_worker actor (Some worker) |> protocol_ok;
         let borrowed = ref None in
         let called = ref false in
         let result =
           A.with_current_moderator_event
             actor
             ~operation_id:None
             ~event:Session_start
             ~snapshot:(fun () -> Ok snapshot)
             (fun ~executing
               ~model_jobs:_
               ~allocate_inference:_
               ~retirement_reason:_
               ~event:_
               ~execute
               ~commit ->
                let invocation =
                  I.create
                    ~observer:Subscription_transaction_tests.source
                    ~parent_event:executing.context.id
                    { (invocation_fixture ()).context with
                      id = P.Id.Invocation.create ()
                    ; origin = Moderator
                    }
                  |> protocol_ok
                in
                let result =
                  execute ~invocation (fun ~dispatched ~capabilities ->
                    called := true;
                    let actor = capabilities.borrow_agent_actor ~invocation:dispatched in
                    (match mode, actor with
                     | `Legacy, Ok None -> ()
                     | `Expired_original, Error error ->
                       [%test_eq: P.Error.code] Permission_denied error.code
                     | (`Live | `Rejected_intent), Ok (Some actor) ->
                       current actor;
                       borrowed := Some actor
                     | _ -> failwith "wrong lifecycle authority");
                    Ok (I.Complete `Null))
                in
                (match result with
                 | Ok _ -> ()
                 | Error error -> raise_s [%sexp (error : P.Error.t)]);
                if
                  [%equal: [ `Live | `Expired_original | `Legacy | `Rejected_intent ]]
                    mode
                    `Rejected_intent
                then reject := true;
                commit
                  ~snapshot
                  ~requests:
                    { request_turn = true
                    ; request_compaction = false
                    ; end_session = None
                    })
         in
         reject := false;
         assert !called;
         Option.iter !borrowed ~f:expired;
         match mode, result with
         | `Rejected_intent, Error _ -> [%test_eq: int] 0 !runs
         | `Rejected_intent, Ok _ -> failwith "rejected intent was acknowledged"
         | (`Live | `Legacy | `Expired_original), Ok true ->
           (match mode with
            | `Live ->
              assert (A.apply_moderator_follow_up actor |> protocol_ok);
              await_idle actor |> ignore;
              [%test_eq: int] 1 !runs
            | `Expired_original ->
              (match A.apply_moderator_follow_up actor with
               | Error error -> [%test_eq: P.Error.code] Permission_denied error.code
               | Ok _ -> failwith "expired intent gained fresh Start authority")
            | `Legacy -> ()
            | `Rejected_intent -> assert false)
         | _, Error error -> raise_s [%sexp (error : P.Error.t)]
         | _, Ok false -> failwith "lifecycle event was not claimed"));
  print_endline
    "Start is single-use; duplicate cannot replace or upgrade; only ACKed exact no-input \
     intent transfers";
  [%expect
    {| Start is single-use; duplicate cannot replace or upgrade; only ACKed exact no-input intent transfers |}]
;;

let%expect_test
    "completion-only compaction handoff preserves known expiry and terminal-save failure"
  =
  List.iter [ `Live; `Expired; `Failed_save ] ~f:(fun mode ->
    let live = Atomic.make true in
    let original = authentication live in
    let returned = ref false in
    let worker_proof = ref None in
    let compactor_proof = ref None in
    let compactor_calls = ref 0 in
    let observed, observed_u = Eio.Promise.create () in
    let signal () = ignore (Eio.Promise.try_resolve observed_u ()) in
    let terminal_candidate (transition : Agent_session.Session_transition.t) =
      !returned && Option.is_none transition.state.active_operation
    in
    Job_fixtures.with_actor
      ~reject_save:(fun transition ->
        let reject =
          [%equal: [ `Live | `Expired | `Failed_save ]] mode `Failed_save
          && terminal_candidate transition
        in
        if reject then signal ();
        reject)
      ~state_committed:(fun _state events ->
        List.iter events ~f:(fun (event : P.Event.Durable.t) ->
          let payload =
            P.Event.Durable.Payload.of_json ~kind:event.kind event.payload |> protocol_ok
          in
          match mode, payload with
          | `Live, (Operation_failed operation | Operation_completed operation)
            when P.Operation.equal_kind operation.kind Compaction -> signal ()
          | `Expired, Operation_completed { kind = Turn _; _ } -> signal ()
          | _ -> ()))
      (fun _env _sw actor writer _backend ->
         let port : A.Compaction_inference.t =
           { with_execution =
               (fun ~operation:_ ~selection:_ ~borrow_agent_actor _ ->
                 Int.incr compactor_calls;
                 let proof = borrow_agent_actor () |> require_actor in
                 current proof;
                 expired (Option.value_exn !worker_proof);
                 compactor_proof := Some proof;
                 Error
                   (P.Error.create
                      Configuration_invalid
                      ~message:"intentional auxiliary fixture stop"
                      ~retryable:false
                      ()))
           }
         in
         A.set_compaction_inference actor (Some port) |> protocol_ok;
         let worker =
           Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
             let proof = caps.original_agent_actor () |> require_actor in
             worker_proof := Some proof;
             current proof;
             if [%equal: [ `Live | `Expired | `Failed_save ]] mode `Expired
             then Atomic.set live false;
             returned := true;
             Agent_session.Operation_worker.Completed
               { final_history = input.history
               ; moderator_snapshot = None
               ; runtime_requests = [ Request_compaction ]
               })
         in
         A.set_operation_worker actor (Some worker) |> protocol_ok;
         A.submit_message_authenticated
           actor
           ~actor:original
           ~attachment_id:writer.id
           actor_entry
         |> protocol_ok
         |> ignore;
         Eio.Promise.await observed;
         (* The live path waits for the exact Compaction terminal event; expired
            completion waits for its turn ACK; failed-save waits for rejection.
            The harness timeout bounds a missing terminal regression. *)
         let state = A.state actor |> protocol_ok in
         expired (Option.value_exn !worker_proof);
         Option.iter !compactor_proof ~f:expired;
         match mode with
         | `Live ->
           [%test_eq: int] 1 !compactor_calls;
           assert (Option.is_some !compactor_proof)
         | `Expired ->
           [%test_eq: int] 0 !compactor_calls;
           assert (Option.is_none state.active_operation);
           (* The acknowledged turn completion is truthful; denied automatic
              continuation does not rewrite it to a failed turn. *)
           assert (Option.is_none state.failure)
         | `Failed_save ->
           [%test_eq: int] 0 !compactor_calls;
           assert (Option.is_some state.active_operation)));
  print_endline
    "worker proof retires before terminal save; live completion lends once; known expiry \
     and rejected terminal save launch no compactor";
  [%expect
    {| worker proof retires before terminal save; live completion lends once; known expiry and rejected terminal save launch no compactor |}]
;;

let%expect_test
    "genuine constructor Job loans retain exact custody through claim and both retry \
     owners"
  =
  List.iter [ `Initialization; `Run_constructor ] ~f:(fun owner ->
    List.iter [ false; true ] ~f:(fun authenticated ->
      let live = Atomic.make true in
      let original = if authenticated then Some (authentication live) else None in
      let reject = ref false in
      Job_fixtures.with_actor
        ~start:false
        ~prepare_state:(fun state ->
          match owner with
          | `Initialization ->
            { state with runtime_initialization = Pending { fresh_history = true } }
          | `Run_constructor -> state)
        ~reject_save:(fun _ -> !reject)
        (fun _env _sw actor writer backend ->
           let expected = A.state actor |> protocol_ok in
           let add, finish =
             match owner with
             | `Initialization ->
               let scope =
                 (match original with
                  | None -> A.begin_initialization actor ~expected
                  | Some original ->
                    A.begin_initialization_authenticated actor ~actor:original ~expected)
                 |> protocol_ok
               in
               ( (fun job -> A.add_initialization_model_job actor ~scope job)
               , fun () ->
                   A.complete_initialization actor ~scope ~candidate:expected
                   |> protocol_ok
                   |> ignore;
                   A.end_initialization actor ~scope |> protocol_ok )
             | `Run_constructor ->
               let request =
                 P.Run_start.create
                   ~session_id
                   ~attachment_id:writer.id
                   ~generation:expected.identity.generation
                   ~expected_revision:expected.counters.revision
                   ~mode:Workflow
                   ~input:Authored_start
                   ~key:
                     (P.Idempotency_key.of_string "constructor-job-custody" |> protocol_ok)
                 |> protocol_ok
               in
               let request_sha256 =
                 P.Run_start.to_json request
                 |> Jsonaf.to_string
                 |> Digestif.SHA256.digest_string
                 |> Digestif.SHA256.to_hex
               in
               let preparation =
                 (match original with
                  | None ->
                    A.begin_run_preparation
                      actor
                      ~authorize:(fun _ -> Ok ())
                      ~principal_id
                      ~request
                      ~request_sha256
                  | Some original ->
                    A.begin_run_preparation_authenticated
                      actor
                      ~actor:original
                      ~authorize:(fun _ -> Ok ())
                      ~request
                      ~request_sha256)
                 |> protocol_ok
                 |> function
                 | Agent_session.Run_preparation.Decision.Prepare preparation ->
                   preparation
                 | Retained _ -> failwith "fresh constructor unexpectedly replayed"
               in
               ( (fun job -> A.add_run_constructor_job actor ~preparation job)
               , fun () -> A.end_run_preparation actor preparation |> protocol_ok )
           in
           let job : P.Job.t =
             { id = P.Id.Job.create ()
             ; session_id
             ; generation = expected.identity.generation
             ; kind =
                 (match owner with
                  | `Initialization -> Model_call
                  | `Run_constructor -> Async_tool)
             ; payload = `Object []
             ; status = Queued
             ; retry_policy = Safe_retry { max_attempts = 3; backoff_ms = 0 }
             ; attempt = 0
             ; created_at = timestamp
             ; started_at = None
             ; next_run_at = None
             ; completed_at = None
             ; result = None
             ; delivery = Pending
             ; launch = None
             ; inference_origin = None
             ; progress = None
             }
           in
           reject := true;
           assert (Result.is_error (add job));
           reject := false;
           assert (List.is_empty (Agent_session.Memory_backend.state backend).jobs);
           let job = add job |> protocol_ok in
           finish ();
           A.start ?actor:original actor ~attachment_id:writer.id |> protocol_ok |> ignore;
           let claim () = A.claim_job actor ~job_id:job.id ~generation:job.generation in
           let before = Agent_session.Memory_backend.state backend in
           reject := true;
           assert (Result.is_error (claim ()));
           reject := false;
           [%test_eq: int64]
             before.counters.revision
             (Agent_session.Memory_backend.state backend).counters.revision;
           let check_callback job =
             let observed = ref None in
             let resolved =
               Job_fixtures.with_job actor job (fun ~job:_ ~execute ->
                 execute
                   ~invocation:(Job_fixtures.root job)
                   (fun ~dispatched ~capabilities ->
                      let borrow_agent_actor =
                        capabilities
                          .Agent_session.Invocation_capabilities.borrow_agent_actor
                      in
                      let borrowed = borrow_agent_actor ~invocation:dispatched in
                      observed
                      := Some
                           ( borrowed
                           , match borrowed with
                             | Ok (Some proof) -> Operator_authorization.is_current proof
                             | Ok None | Error _ -> false );
                      Ok (I.Complete `Null)))
               |> protocol_ok
             in
             (match resolved.I.status with
              | Resolved (Complete `Null) -> ()
              | _ -> failwith "Job callback failed before completing its oracle");
             match authenticated, Option.value_exn !observed with
             | false, (Ok None, false) -> ()
             | true, (Ok (Some proof), true) -> expired proof
             | _, (Error failure, _) -> raise_s [%sexp (failure : P.Error.t)]
             | false, (Ok (Some _), _)
             | true, (Ok None, _)
             | false, (Ok None, true)
             | true, (Ok (Some _), false) ->
               failwith "Job callback changed its original admission"
           in
           let running = claim () |> protocol_ok |> Option.value_exn in
           [%test_eq: int] 1 running.attempt;
           check_callback running;
           let retry () =
             match owner with
             | `Initialization ->
               A.complete_job
                 actor
                 ~job_id:running.id
                 ~generation:running.generation
                 ~attempt:running.attempt
                 (Model_failed "retry this admitted attempt")
             | `Run_constructor ->
               A.complete_background_job
                 actor
                 ~job_id:running.id
                 ~generation:running.generation
                 ~attempt:running.attempt
                 (Failed
                    { code = "fixture.retry"
                    ; message = "retry this admitted attempt"
                    ; retryable = true
                    ; details = `Null
                    })
           in
           reject := true;
           assert (Result.is_error (retry ()));
           reject := false;
           check_callback running;
           let queued = retry () |> protocol_ok in
           (match queued.status with
            | Queued -> ()
            | _ -> failwith "retry did not acknowledge its queued successor");
           let running = claim () |> protocol_ok |> Option.value_exn in
           [%test_eq: int] 2 running.attempt;
           check_callback running;
           if authenticated
           then (
             Atomic.set live false;
             let denied = ref None in
             Job_fixtures.with_job actor running (fun ~job:_ ~execute ->
               execute
                 ~invocation:(Job_fixtures.root running)
                 (fun ~dispatched ~capabilities ->
                    let borrow_agent_actor =
                      capabilities
                        .Agent_session.Invocation_capabilities.borrow_agent_actor
                    in
                    denied := Some (borrow_agent_actor ~invocation:dispatched);
                    Ok (I.Complete `Null)))
             |> protocol_ok
             |> ignore;
             match Option.value_exn !denied with
             | Error { code = Permission_denied; _ } -> ()
             | Error failure -> raise_s [%sexp (failure : P.Error.t)]
             | Ok _ -> failwith "expired Job original became legacy None");
           let terminal =
             (match owner with
              | `Initialization ->
                A.complete_job
                  actor
                  ~job_id:running.id
                  ~generation:running.generation
                  ~attempt:running.attempt
                  (Model_succeeded `Null)
              | `Run_constructor ->
                A.complete_background_job
                  actor
                  ~job_id:running.id
                  ~generation:running.generation
                  ~attempt:running.attempt
                  (Succeeded `Null))
             |> protocol_ok
           in
           match terminal.status with
           | Succeeded -> ()
           | _ -> failwith "known original expiry suppressed truthful terminal fact")));
  print_endline
    "genuine initialization/Run-constructor admissions; failed ACK retries; exact \
     claims; Model and Async retries preserve original custody";
  [%expect
    {| genuine initialization/Run-constructor admissions; failed ACK retries; exact claims; Model and Async retries preserve original custody |}]
;;
