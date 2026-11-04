open Core
open Fixtures
open Job_fixtures
module Owner = Agent_server.Runtime_owner
module Builder = Agent_session.Runtime_builder

type source_stage_fixture =
  { actor : Agent_session.Session_actor.t
  ; writer_id : Agent_protocol.Id.Attachment.t
  ; environment : Eio_unix.Stdenv.base * Agent_session.Workspace_instance.t
  ; source : Builder.Source.t
  ; proof : Operator_authorization.t
  ; guards : int ref
  ; policies : int ref
  ; guard : (unit -> bool) ref
  ; reject : bool ref
  ; backend : Agent_session.Memory_backend.t
  }

let with_source_stage_actor ?environment ?(source_name = "staged-source") f =
  let with_workspace f =
    match environment with
    | None -> with_actor_workspace f
    | Some (env, workspace) -> f env workspace
  in
  with_workspace (fun env workspace ->
    let root = Eio.Path.(Eio.Stdenv.fs env / workspace.canonical_root.native_path) in
    let source_root = Eio.Path.(root / source_name) in
    Eio.Path.mkdir ~perm:0o700 source_root;
    let root_file = Eio.Path.(source_root / "root.chatmd") in
    Eio.Path.save
      ~create:(`Exclusive 0o600)
      root_file
      {|<script id="staged" language="chatml" kind="moderator">let initial_state = fail("source-stage initializer ran")
let on_event = fun event state -> Task.pure(state)</script>|};
    let definition =
      Agent_session.Prompt_definition.create
        ~root_invocation:Ui
        ~id:(Agent_protocol.Id.Prompt_definition.of_string "prd_staged" |> protocol_ok)
        ~config_name:"staged"
        ~root_file:(Eio.Path.native_exn root_file)
        ~allowed_workspaces:[]
        ~permission_profile:"interactive"
        ~runtime_policy:None
        ~enabled:true
        ~description:None
      |> store_ok
    in
    let artifacts =
      Agent_store.Prompt_artifact_store.create
        ~env
        ~root:(Eio.Path.native_exn Eio.Path.(root / "artifacts"))
      |> store_ok
    in
    let revision =
      match
        Agent_session.Prompt_revision_builder.build
          ~env
          ~artifact_store:artifacts
          ~transaction_id
          ~created_at:timestamp
          definition
      with
      | Ok revision -> revision
      | Error diagnostics ->
        raise_s
          [%sexp (diagnostics : Agent_session.Prompt_revision_builder.Diagnostic.t list)]
    in
    let source =
      Builder.Source.prepare
        revision
        ~script_preparation:(Some (script_preparation ()))
        ~invocation:Ui
        ~env
        ~checkpoint:Eio.Fiber.check
      |> protocol_ok
    in
    Eio.Path.unlink root_file;
    let reject = ref false in
    with_actor
      ~environment:(env, workspace)
      ~start:false
      ~reject_save:(fun _ -> !reject)
      ~prepare_state:(fun state ->
        { state with
          spec =
            { state.spec with
              prompt_definition_id = Some definition.id
            ; prompt_revision_id = Agent_session.Prompt_revision.id revision
            }
        })
      (fun _env _sw actor writer backend ->
         let guards = ref 0
         and policies = ref 0
         and guard = ref (fun () -> true) in
         let principal =
           Agent_protocol.Principal.create
             ~id:principal_id
             ~authentication_kind:"local"
             ~scopes:(Agent_protocol.Scope.Set.of_list [ Send_messages ])
             ~attributes:[]
           |> protocol_ok
         in
         let proof =
           Operator_authorization.guarded ~principal ~is_current:(fun () ->
             Int.incr guards;
             !guard ())
         in
         f
           { actor
           ; writer_id = writer.id
           ; environment = env, workspace
           ; source
           ; proof
           ; guards
           ; policies
           ; guard
           ; reject
           ; backend
           }))
;;

let source_stage_preparation fixture key =
  let module A = Agent_session.Session_actor in
  let state = A.state fixture.actor |> protocol_ok in
  let request =
    Agent_session.Run_admission_request.Headless.create
      ~session_id:state.identity.session_id
      ~generation:state.identity.generation
      ~expected_revision:state.counters.revision
      ~mode:Single_turn
      ~input:Authored_start
      ~key:(Agent_protocol.Idempotency_key.of_string key |> protocol_ok)
    |> protocol_ok
  in
  match
    A.begin_headless_run_preparation
      fixture.actor
      ~actor:fixture.proof
      ~authorize:(fun _ ->
        Int.incr fixture.policies;
        Ok ())
      ~request
    |> protocol_ok
  with
  | Prepare preparation -> preparation
  | Retained _ -> failwith "source-stage fixture unexpectedly retained an admission"
;;

let runtime ?script_tools ?check_execution ?activity ~close () : Builder.t =
  let inference = Inference_ports.create ~config:Chat_response.Config.default () in
  { worker =
      Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input:_ _ ->
        failwith "unexpected model operation")
  ; inference_execution = Inference_ports.execution inference
  ; now = Agent_protocol.Timestamp.now
  ; parse_user_content = (fun ~id:_ _ -> failwith "unexpected input")
  ; initial_history = []
  ; initial_prompt_entry_count = 0
  ; reserved_history_through = 0
  ; moderator_snapshot = (fun () -> None)
  ; moderator_manager = None
  ; moderator_tools = []
  ; moderator_script_tools = script_tools
  ; standalone_completion = None
  ; background_executor = None
  ; idle_notifications = None
  ; automatic_turn_policy = None
  ; check_execution
  ; ancestor_capabilities = None
  ; source_lease = Agent_session.Prompt_source_lease.legacy
  ; activity
  ; native_runtime = None
  ; moderator_activation = None
  ; start_moderator = (fun () -> Ok None)
  ; enqueue_internal_event = (fun ?prepare:_ _ -> failwith "unexpected event")
  ; drain_internal_events = (fun _ -> failwith "unexpected event drain")
  ; execute_model_job =
      (fun ~inference_context:_ ~capture_recipe_target:_ ~recipe:_ ~payload:_ ->
        failwith "unexpected model job")
  ; enqueue_model_job_completion = (fun ?prepare:_ _ -> failwith "unexpected completion")
  ; close
  }
;;

let%expect_test
    "source stages retain exact actor custody without loading or calling policy from \
     basis"
  =
  let module A = Agent_session.Session_actor in
  let module Binding = A.Source_binding in
  with_source_stage_actor (fun fixture ->
    let builds = ref 0 in
    let owner =
      Owner.create ~actor:fixture.actor ~initial:None ~build:(fun () ->
        Int.incr builds;
        failwith "source stage invoked the runtime builder")
    in
    let conflict (result : (_, Agent_protocol.Error.t) result) =
      match result with
      | Error error -> [%test_eq: Agent_protocol.Error.code] Conflict error.code
      | Ok _ -> failwith "source stage unexpectedly retained authority"
    in
    let reset_calls () =
      fixture.guards := 0;
      fixture.policies := 0
    in
    let no_calls () =
      [%test_eq: int] 0 !(fixture.guards);
      [%test_eq: int] 0 !(fixture.policies)
    in
    let with_stage key f =
      let preparation = source_stage_preparation fixture key in
      Exn.protect
        ~finally:(fun () ->
          A.end_run_preparation fixture.actor preparation |> protocol_ok)
        ~f:(fun () ->
          Owner.with_prepared_source
            owner
            ~preparation
            ~source:fixture.source
            (f preparation)
          |> protocol_ok)
    in
    Exn.protect
      ~finally:(fun () -> Owner.close_and_wait owner)
      ~f:(fun () ->
        let escaped = ref None in
        with_stage "stage-own-commit" (fun preparation prepared ->
          let binding = Owner.Prepared.binding prepared in
          reset_calls ();
          Binding.check_basis binding |> protocol_ok;
          Owner.Prepared.check_basis prepared |> protocol_ok;
          no_calls ();
          Binding.check_current binding |> protocol_ok;
          [%test_eq: int] 1 !(fixture.policies);
          assert (!(fixture.guards) > 0);
          conflict (Owner.ensure_loaded owner);
          conflict (Owner.with_loaded_runtime owner (fun _ -> failwith "borrow entered"));
          conflict
            (Owner.with_prepared_source
               owner
               ~preparation
               ~source:fixture.source
               (fun _ -> failwith "competing source stage entered"));
          [%test_eq: int] 0 !builds;
          A.reserve_run_history_block fixture.actor ~preparation ~count:1
          |> protocol_ok
          |> ignore;
          reset_calls ();
          Binding.check_basis binding |> protocol_ok;
          Owner.Prepared.check_basis prepared |> protocol_ok;
          no_calls ();
          assert (phys_equal (Binding.source binding) fixture.source);
          escaped := Some prepared;
          Ok ());
        reset_calls ();
        let ended = Option.value_exn !escaped in
        conflict (Binding.check_basis (Owner.Prepared.binding ended));
        conflict (Owner.Prepared.check_basis ended);
        no_calls ();
        print_endline
          "basis: zero callbacks; current: one policy; own ACK retained; eager loads \
           excluded";
        with_stage "stage-rejected-write" (fun _ prepared ->
          let binding = Owner.Prepared.binding prepared in
          let json state = state_document state |> Document_schema.Document.json in
          let before = A.state fixture.actor |> protocol_ok |> json in
          let saved = Agent_session.Memory_backend.state fixture.backend |> json in
          fixture.reject := true;
          let result =
            Exn.protect
              ~finally:(fun () -> fixture.reject := false)
              ~f:(fun () -> A.reserve_history_block fixture.actor ~count:1)
          in
          (match result with
           | Error error ->
             [%test_eq: Agent_protocol.Error.code] Internal_error error.code;
             [%test_eq: string] "injected job transition failure" error.message;
             [%test_eq: bool] false error.retryable
           | Ok _ -> failwith "injected write unexpectedly committed");
          assert (
            Jsonaf.exactly_equal before (A.state fixture.actor |> protocol_ok |> json));
          assert (
            Jsonaf.exactly_equal
              saved
              (Agent_session.Memory_backend.state fixture.backend |> json));
          reset_calls ();
          conflict (Binding.check_current binding);
          conflict (Owner.Prepared.check_basis prepared);
          no_calls ();
          Ok ());
        print_endline
          "rejected ordinary write: exact state retained; source custody irreversibly \
           closed";
        with_stage "stage-guard-close" (fun _ prepared ->
          let binding = Owner.Prepared.binding prepared in
          (fixture.guard
           := fun () ->
                Binding.close binding;
                true);
          Exn.protect
            ~finally:(fun () -> fixture.guard := fun () -> true)
            ~f:(fun () ->
              reset_calls ();
              conflict (Binding.check_current binding);
              [%test_eq: int] 1 !(fixture.policies);
              assert (!(fixture.guards) > 0);
              reset_calls ();
              conflict (Binding.check_basis binding);
              conflict (Owner.Prepared.check_basis prepared);
              no_calls ());
          Ok ());
        [%test_eq: int] 0 !builds;
        print_endline
          "guard closure: final basis rejects; callback exit closes lifetime; builder \
           never entered"));
  [%expect
    {|
    basis: zero callbacks; current: one policy; own ACK retained; eager loads excluded
    rejected ordinary write: exact state retained; source custody irreversibly closed
    guard closure: final basis rejects; callback exit closes lifetime; builder never entered |}]
;;

let%expect_test
    "source stage stop and retirement close custody before joining; foreign owners never \
     enter"
  =
  let module A = Agent_session.Session_actor in
  let module Binding = A.Source_binding in
  let module Failure = struct
    exception Primary
  end
  in
  with_source_stage_actor (fun fixture ->
    let builds = ref 0 in
    let barrier, barrier_u = Eio.Promise.create () in
    let release, release_u = Eio.Promise.create () in
    let release_barrier () =
      if Option.is_none (Eio.Promise.peek release) then Eio.Promise.resolve release_u ()
    in
    let owner =
      Owner.create_with_unload
        ~actor:fixture.actor
        ~initial:None
        ~build:(fun () ->
          Int.incr builds;
          failwith "source-stage retirement invoked builder")
        ~before_unload:(fun ~closing:_ ->
          if Option.is_none (Eio.Promise.peek barrier)
          then Eio.Promise.resolve barrier_u ();
          Eio.Promise.await release;
          Ok ())
    in
    let conflict (result : (_, Agent_protocol.Error.t) result) =
      match result with
      | Error error -> [%test_eq: Agent_protocol.Error.code] Conflict error.code
      | Ok _ -> failwith "closed or foreign source stage was admitted"
    in
    let with_preparation key f =
      let preparation = source_stage_preparation fixture key in
      Exn.protect
        ~finally:(fun () ->
          A.end_run_preparation fixture.actor preparation |> protocol_ok)
        ~f:(fun () -> f preparation)
    in
    Exn.protect
      ~finally:(fun () ->
        release_barrier ();
        Owner.close_and_wait owner)
      ~f:(fun () ->
        with_source_stage_actor
          ~environment:fixture.environment
          ~source_name:"foreign-source"
          (fun foreign ->
             with_preparation "stage-foreign-owner" (fun preparation ->
               let other = source_stage_preparation foreign "other-source-owner" in
               Exn.protect
                 ~finally:(fun () ->
                   A.end_run_preparation foreign.actor other |> protocol_ok)
                 ~f:(fun () ->
                   assert (
                     not
                       (Agent_protocol.Id.Prompt_revision.equal
                          (Builder.Source.artifact fixture.source).revision_id
                          (Builder.Source.artifact foreign.source).revision_id));
                   fixture.guards := 0;
                   fixture.policies := 0;
                   foreign.guards := 0;
                   foreign.policies := 0;
                   conflict
                     (Owner.with_prepared_source
                        owner
                        ~preparation:other
                        ~source:foreign.source
                        (fun _ -> failwith "foreign actor stage entered"));
                   conflict
                     (Owner.with_prepared_source
                        owner
                        ~preparation
                        ~source:foreign.source
                        (fun _ -> failwith "foreign captured source entered"));
                   List.iter [ fixture; foreign ] ~f:(fun fixture ->
                     [%test_eq: int] 0 !(fixture.guards);
                     [%test_eq: int] 0 !(fixture.policies)))));
        print_endline
          "foreign actor and captured source: rejected before original/policy callbacks";
        with_preparation "stage-primary-failure" (fun preparation ->
          let escaped = ref None in
          let primary = Failure.Primary in
          let observed =
            Result.try_with (fun () ->
              Owner.with_prepared_source
                owner
                ~preparation
                ~source:fixture.source
                (fun prepared ->
                   escaped := Some prepared;
                   raise primary))
          in
          (match observed with
           | Error observed -> assert (phys_equal observed primary)
           | Ok _ -> failwith "stage primary exception disappeared");
          let ended = Option.value_exn !escaped in
          conflict (Binding.check_basis (Owner.Prepared.binding ended));
          conflict (Owner.Prepared.check_basis ended));
        A.start fixture.actor ~attachment_id:fixture.writer_id |> protocol_ok |> ignore;
        with_preparation "stage-public-stop" (fun preparation ->
          Owner.with_prepared_source
            owner
            ~preparation
            ~source:fixture.source
            (fun prepared ->
               A.stop fixture.actor ~attachment_id:fixture.writer_id ~mode:Cancel
               |> protocol_ok
               |> ignore;
               let stopped = A.state fixture.actor |> protocol_ok in
               (match stopped.lifecycle.observed with
                | Stopped -> ()
                | observed ->
                  raise_s
                    [%sexp
                      "public Stop did not stop source-stage actor"
                    , (observed : Agent_protocol.Session.observed_state)]);
               let binding = Owner.Prepared.binding prepared in
               conflict (Binding.check_basis binding);
               conflict (Owner.Prepared.check_basis prepared);
               assert (phys_equal (Binding.source binding) fixture.source);
               Ok ())
          |> protocol_ok);
        print_endline
          "primary exception preserved; actual public Stop closes exact source custody";
        with_preparation "stage-retirement-cancel" (fun preparation ->
          Eio.Switch.run (fun sw ->
            Exn.protect ~finally:release_barrier ~f:(fun () ->
              let entered, entered_u = Eio.Promise.create () in
              let leaving, leaving_u = Eio.Promise.create () in
              let never, _ = Eio.Promise.create () in
              let caller =
                Eio.Fiber.fork_promise ~sw (fun () ->
                  Owner.with_prepared_source
                    owner
                    ~preparation
                    ~source:fixture.source
                    (fun prepared ->
                       Eio.Promise.resolve entered_u prepared;
                       Exn.protect
                         ~finally:(fun () -> Eio.Promise.resolve leaving_u ())
                         ~f:(fun () ->
                           Eio.Promise.await never;
                           Ok ())))
              in
              let prepared = Eio.Promise.await entered in
              let stopping =
                Eio.Fiber.fork_promise ~sw (fun () -> Owner.unload_and_wait owner)
              in
              Eio.Promise.await barrier;
              fixture.guards := 0;
              fixture.policies := 0;
              conflict (Binding.check_current (Owner.Prepared.binding prepared));
              conflict (Owner.Prepared.check_basis prepared);
              [%test_eq: int] 0 !(fixture.guards);
              [%test_eq: int] 0 !(fixture.policies);
              assert (Option.is_none (Eio.Promise.peek leaving));
              assert (Option.is_none (Eio.Promise.peek stopping));
              release_barrier ();
              Eio.Promise.await_exn stopping |> protocol_ok;
              (match Eio.Promise.await caller with
               | Error (Eio.Cancel.Cancelled _) -> ()
               | _ -> failwith "source-stage retirement lost cancellation");
              assert (Option.is_some (Eio.Promise.peek leaving));
              conflict (Owner.Prepared.check_basis prepared))));
        [%test_eq: int] 0 !builds;
        assert (not (Owner.is_loaded owner));
        print_endline
          "retirement: custody closed before barrier/cancellation; callback joined; no \
           runtime built"));
  [%expect
    {|
    foreign actor and captured source: rejected before original/policy callbacks
    primary exception preserved; actual public Stop closes exact source custody
    retirement: custody closed before barrier/cancellation; callback joined; no runtime built |}]
;;

let%expect_test
    "concurrent retirement callers share dependency failures and preserve resources for \
     retry"
  =
  List.iter
    (List.cartesian_product [ false; true ] [ false; true ])
    ~f:(fun (raises, closing) ->
      with_actor (fun _env sw actor _writer _backend ->
        let entered, entered_u = Eio.Promise.create () in
        let release, release_u = Eio.Promise.create () in
        let fail = ref true
        and calls = ref 0
        and closes = ref 0 in
        let failure =
          Agent_protocol.Error.create
            Persistence_error
            ~message:"descendant stop could not be saved"
            ~retryable:true
            ()
        in
        let owner =
          Owner.create_with_unload
            ~actor
            ~initial:(Some (runtime ~close:(fun () -> Int.incr closes) ()))
            ~build:(fun () -> failwith "unexpected rebuild")
            ~before_unload:(fun ~closing:_ ->
              Int.incr calls;
              match !fail with
              | false -> Ok ()
              | true ->
                if !calls = 1 then Eio.Promise.resolve entered_u ();
                Eio.Promise.await release;
                if raises then failwith "descendant cleanup exception" else Error failure)
        in
        let stop ~closing () =
          try
            Ok
              (if closing
               then (
                 Owner.close_and_wait owner;
                 Ok ())
               else Owner.unload_and_wait owner)
          with
          | Owner.Cleanup_failed error -> Ok (Error error)
          | exn -> Error exn
        in
        let first = Eio.Fiber.fork_promise ~sw (stop ~closing:false) in
        Eio.Promise.await entered;
        let second = Eio.Fiber.fork_promise ~sw (stop ~closing) in
        Eio.Fiber.yield ();
        assert (Owner.is_loaded owner);
        [%test_eq: int] 0 !closes;
        (match closing, Owner.ensure_loaded owner with
         | false, Error { code = Conflict; _ }
         | true, Error { code = Server_shutting_down; _ } -> ()
         | _ -> failwith "dependency barrier admitted new runtime work");
        Eio.Promise.resolve release_u ();
        List.iter [ first; second ] ~f:(fun pending ->
          match raises, Eio.Promise.await_exn pending with
          | false, Ok (Error actual) ->
            [%test_eq: Sexp.t]
              (Agent_protocol.Error.sexp_of_t failure)
              (Agent_protocol.Error.sexp_of_t actual)
          | true, Error (Failure message) ->
            [%test_eq: string] "descendant cleanup exception" message
          | _ -> failwith "retirement failure changed between waiters");
        [%test_eq: int] 1 !calls;
        [%test_eq: int] 0 !closes;
        assert (Owner.is_loaded owner);
        fail := false;
        (match closing with
         | false -> Owner.unload_and_wait owner |> protocol_ok
         | true -> Owner.close_and_wait owner);
        [%test_eq: int] 2 !calls;
        [%test_eq: int] 1 !closes;
        assert (not (Owner.is_loaded owner));
        Owner.close_and_wait owner;
        print_s
          [%sexp
            { exception_path = (raises : bool)
            ; closing : bool
            ; shared_failure = true
            ; retry_closed_once = true
            }]));
  [%expect
    {|
    ((exception_path false) (closing false) (shared_failure true)
     (retry_closed_once true))
    ((exception_path false) (closing true) (shared_failure true)
     (retry_closed_once true))
    ((exception_path true) (closing false) (shared_failure true)
     (retry_closed_once true))
    ((exception_path true) (closing true) (shared_failure true)
     (retry_closed_once true))
    |}]
;;

let%expect_test "external close joins an accepted stop before retiring its dependencies" =
  List.iter [ false; true ] ~f:(fun background ->
    with_actor (fun _env sw actor _writer _backend ->
      let entered, entered_u = Eio.Promise.create () in
      let release, release_u = Eio.Promise.create () in
      let worker_entered, worker_entered_u = Eio.Promise.create () in
      let never, _ = Eio.Promise.create () in
      let released = ref false in
      let release_cleanup () =
        if not !released
        then (
          released := true;
          Eio.Promise.resolve release_u ())
      in
      let closes = ref 0 in
      let owner =
        Owner.create_with_unload
          ~actor
          ~initial:(Some (runtime ~close:(fun () -> Int.incr closes) ()))
          ~build:(fun () -> failwith "unexpected rebuild")
          ~before_unload:(fun ~closing:_ ->
            Eio.Promise.resolve entered_u ();
            Eio.Promise.await release;
            Ok ())
      in
      Exn.protect ~finally:release_cleanup ~f:(fun () ->
        let worker =
          match background with
          | false -> None
          | true ->
            Some
              (Eio.Fiber.fork_promise ~sw (fun () ->
                 Result.try_with (fun () ->
                   Owner.with_background_runtime owner (fun _ ->
                     Eio.Promise.resolve worker_entered_u ();
                     Eio.Promise.await never;
                     Ok ()))))
        in
        if background then Eio.Promise.await worker_entered;
        let stopping =
          Eio.Fiber.fork_promise ~sw (fun () -> Owner.unload_and_wait owner)
        in
        Eio.Promise.await entered;
        let closing = Eio.Fiber.fork_promise ~sw (fun () -> Owner.close_and_wait owner) in
        Eio.Fiber.yield ();
        Eio.Fiber.yield ();
        [%test_eq: int] 0 !closes;
        assert (Owner.is_loaded owner);
        assert (Option.is_none (Eio.Promise.peek closing));
        release_cleanup ();
        Eio.Promise.await_exn stopping |> protocol_ok;
        Eio.Promise.await_exn closing;
        Option.iter worker ~f:(fun worker ->
          match Eio.Promise.await_exn worker with
          | Error (Eio.Cancel.Cancelled _) -> ()
          | _ -> failwith "background lease was not cancelled");
        [%test_eq: int] 1 !closes;
        assert (not (Owner.is_loaded owner));
        print_s [%sexp (background : bool), "stop barrier joined before close"])));
  [%expect
    {|
    (false "stop barrier joined before close")
    (true "stop barrier joined before close") |}]
;;

let%expect_test "failed authority lookup preserves owner cleanup and rechecks every lease"
  =
  with_actor (fun _env _sw actor _writer _backend ->
    let unavailable = ref true in
    let checks = ref 0 in
    let closes = ref 0 in
    let initial =
      runtime
        ~check_execution:(fun () ->
          Int.incr checks;
          Eio.Fiber.yield ();
          match !unavailable with
          | true -> failwith "injected authority lookup failure"
          | false -> Ok ())
        ~close:(fun () -> Int.incr closes)
        ()
    in
    let owner =
      Owner.create ~actor ~initial:(Some initial) ~build:(fun () -> assert false)
    in
    let expect_lookup_failure f =
      match f () with
      | _ -> failwith "missing authority failure"
      | exception Failure message ->
        [%test_eq: string] "injected authority lookup failure" message
    in
    expect_lookup_failure (fun () -> Owner.ensure_loaded owner);
    expect_lookup_failure (fun () ->
      Owner.with_background_runtime owner (fun _ -> failwith "unauthorized lease"));
    assert (Owner.is_loaded owner);
    unavailable := false;
    Owner.with_background_runtime owner (fun _ -> Ok ()) |> protocol_ok;
    unavailable := true;
    expect_lookup_failure (fun () -> Owner.ensure_loaded owner);
    Owner.unload_and_wait owner |> protocol_ok;
    assert (not (Owner.is_loaded owner));
    [%test_eq: int] 4 !checks;
    [%test_eq: int] 1 !closes;
    Owner.close owner;
    print_endline
      "failed reads deny entry without poisoning retirement; leases revalidate");
  [%expect {| failed reads deny entry without poisoning retirement; leases revalidate |}]
;;

let%expect_test
    "committed stop joins cleanup, excludes admission and retires even when close fails"
  =
  List.iter [ false; true ] ~f:(fun fail_close ->
    with_actor (fun _env sw actor _writer _backend ->
      let entered, enter = Eio.Promise.create () in
      let cleaning, cleaning_u = Eio.Promise.create () in
      let release, release_u = Eio.Promise.create () in
      let never, _ = Eio.Promise.create () in
      let cleaned = ref false
      and closes = ref 0
      and fail_once = ref fail_close in
      let build () =
        Ok
          (runtime
             ~close:(fun () ->
               assert !cleaned;
               incr closes;
               match !fail_once with
               | false -> ()
               | true ->
                 fail_once := false;
                 failwith "injected runtime close failure")
             ())
      in
      let owner = Owner.create ~actor ~initial:None ~build in
      let worker =
        Eio.Fiber.fork_promise ~sw (fun () ->
          Result.try_with (fun () ->
            Owner.with_background_runtime owner (fun _ ->
              Eio.Promise.resolve enter ();
              Exn.protect
                ~finally:(fun () ->
                  Eio.Cancel.protect (fun () ->
                    Eio.Promise.resolve cleaning_u ();
                    Eio.Promise.await release;
                    cleaned := true))
                ~f:(fun () ->
                  Eio.Promise.await never;
                  Ok ()))))
      in
      Eio.Promise.await entered;
      let stopped =
        Eio.Fiber.fork_promise ~sw (fun () ->
          Result.try_with (fun () -> Owner.unload_and_wait owner))
      in
      Eio.Promise.await cleaning;
      let joined =
        List.init 2 ~f:(fun _ ->
          Eio.Fiber.fork_promise ~sw (fun () ->
            Result.try_with (fun () -> Owner.unload_and_wait owner)))
      in
      Eio.Fiber.yield ();
      [%test_eq: int] 0 !closes;
      (match Owner.ensure_loaded owner with
       | Error { code = Conflict; retryable = true; _ } -> ()
       | _ -> failwith "new runtime admission escaped pending cleanup");
      Eio.Promise.resolve release_u ();
      (match Eio.Promise.await_exn worker with
       | Error (Eio.Cancel.Cancelled _) -> ()
       | _ -> failwith "worker cancellation was lost");
      List.iter (stopped :: joined) ~f:(fun waiter ->
        match fail_close, Eio.Promise.await_exn waiter with
        | false, Ok (Ok ()) -> ()
        | true, Error (Failure message)
          when String.equal message "injected runtime close failure" -> ()
        | _ -> failwith "stop produced an unexpected cleanup outcome");
      assert (not (Owner.is_loaded owner));
      [%test_eq: int] 1 !closes;
      Owner.ensure_loaded owner |> protocol_ok;
      Owner.unload_and_wait owner |> protocol_ok;
      [%test_eq: int] 2 !closes;
      print_s
        [%sexp
          (fail_close : bool)
        , "cleanup joined before close; admission excluded; owner reusable"]));
  [%expect
    {|
    (false "cleanup joined before close; admission excluded; owner reusable")
    (true "cleanup joined before close; admission excluded; owner reusable")
    |}]
;;

let%expect_test
    "retention excludes job scopes and keeps runtime ownership until a cancelled \
     caller's checkpoint finishes"
  =
  with_actor (fun _env sw actor _writer _backend ->
    let job = add_claimed_job actor in
    with_job actor job (fun ~job:_ ~execute:_ ->
      assert (
        Option.is_none
          (A.with_quiescent_state actor ~f:(fun _ ->
             failwith "live job scope entered retention")
           |> protocol_ok));
      Ok ())
    |> protocol_ok;
    let owner =
      Owner.create ~actor ~initial:None ~build:(fun () ->
        failwith "maintenance must not load a runtime")
    in
    let entered, enter = Eio.Promise.create () in
    let release, release_u = Eio.Promise.create () in
    let cancellation, cancellation_u = Eio.Promise.create () in
    let caller_done, caller_done_u = Eio.Promise.create () in
    Eio.Fiber.fork ~sw (fun () ->
      Exn.protect
        ~finally:(fun () -> Eio.Promise.resolve caller_done_u ())
        ~f:(fun () ->
          try
            Eio.Cancel.sub (fun context ->
              Eio.Promise.resolve cancellation_u context;
              Owner.with_unloaded owner (fun () ->
                A.with_quiescent_state actor ~f:(fun _ ->
                  Eio.Promise.resolve enter ();
                  Eio.Promise.await release;
                  Ok ()))
              |> protocol_ok
              |> ignore)
          with
          | Eio.Cancel.Cancelled _ -> ()));
    Eio.Promise.await entered;
    Eio.Cancel.cancel (Eio.Promise.await cancellation) Exit;
    let competitor_entered = ref false in
    let competitor_done, competitor_done_u = Eio.Promise.create () in
    Eio.Fiber.fork ~sw (fun () ->
      Owner.with_unloaded owner (fun () ->
        competitor_entered := true;
        Ok ())
      |> protocol_ok
      |> ignore;
      Eio.Promise.resolve competitor_done_u ());
    Eio.Fiber.yield ();
    assert (not !competitor_entered);
    Eio.Promise.resolve release_u ();
    Eio.Promise.await caller_done;
    Eio.Promise.await competitor_done;
    assert !competitor_entered;
    assert (
      Result.is_error
        (Result.try_with (fun () ->
           Owner.with_unloaded owner (fun () -> failwith "injected inspection exception"))));
    assert (Option.is_some (Owner.with_unloaded owner (fun () -> Ok ()) |> protocol_ok));
    print_endline
      "live job scope deferred; cancellation did not release an outstanding actor \
       checkpoint";
    print_endline
      "competing maintenance resumed after completion; callback exception did not poison \
       owner");
  [%expect
    {|
    live job scope deferred; cancellation did not release an outstanding actor checkpoint
    competing maintenance resumed after completion; callback exception did not poison owner
    |}]
;;

let%expect_test
    "independent native jobs retain one runtime without serializing each other"
  =
  with_actor (fun env sw actor _writer backend ->
    let first_entered, first_entered_u = Eio.Promise.create () in
    let second_entered, second_entered_u = Eio.Promise.create () in
    let first_done, first_done_u = Eio.Promise.create () in
    let calls = ref 0
    and running = ref 0
    and maximum = ref 0 in
    let registry =
      native_registry calls ~raises:false ~on_call:(fun () ->
        incr running;
        maximum := Int.max !maximum !running;
        Exn.protect
          ~finally:(fun () -> decr running)
          ~f:(fun () ->
            match !calls with
            | 1 ->
              Eio.Promise.resolve first_entered_u ();
              Eio.Promise.await second_entered
            | _ -> Eio.Promise.resolve second_entered_u ()))
    in
    let script_tools =
      Agent_session.Script_tool_calls.create
        ~registry:(fun () -> registry)
        ~moderator_names:String.Set.empty
        ~now:(fun () -> timestamp)
        ~is_halted:(fun () -> false)
        ~requires_active_moderator:(fun _ -> false)
        ~authorize:(fun _ _ -> Ok ())
        ~prepare_output:(fun _ -> Ok (`String "disclosed"))
        ~defer_observation:(fun _ -> Ok ())
    in
    let builds = ref 0
    and closes = ref 0 in
    let owner =
      Owner.create ~actor ~initial:None ~build:(fun () ->
        incr builds;
        Ok
          (runtime
             ~script_tools
             ~close:(fun () ->
               [%test_eq: int] 0 !running;
               incr closes)
             ()))
    in
    let request =
      Chat_response.Background_request.tool
        ~capabilities:registry
        ~reference:(List.hd_exn (C.references registry))
        ~input:(`Object [])
        ~policy:Chat_response.One_off_request.default_policy
      |> protocol_ok
    in
    let run_job () =
      let job =
        add_claimed_job ~payload:(Chat_response.Background_request.to_json request) actor
      in
      Owner.with_background_runtime owner (fun runtime ->
        with_job actor job (fun ~job ~execute ->
          Agent_session.Background_execution.run
            ~env
            ~job
            ~deadline
            ~execute
            ~request
            ~policy:Chat_response.One_off_request.default_policy
            ~script_tools:(Option.value_exn runtime.moderator_script_tools)
            ~now:(fun () -> timestamp)
            ~moderate_tool:(fun _ _ -> Ok None)
            ~prepare_outcome:(fun _ -> Ok ())
            ()))
      |> protocol_ok
      |> ignore
    in
    Eio.Fiber.fork ~sw (fun () ->
      run_job ();
      Eio.Promise.resolve first_done_u ());
    Eio.Promise.await first_entered;
    let administrative_calls = ref 0 in
    reject "unload while owned" (Owner.unload owner);
    reject
      "administration while owned"
      (Owner.with_administration owner (fun () ->
         incr administrative_calls;
         Ok ()));
    [%test_eq: int] 0 !administrative_calls;
    [%test_eq: int] 0 !closes;
    run_job ();
    Eio.Promise.await first_done;
    Owner.unload owner |> protocol_ok;
    Owner.with_background_runtime owner (fun _ -> Ok ()) |> protocol_ok;
    Owner.close owner;
    let state = Agent_session.Memory_backend.state backend in
    print_s
      [%sexp
        { builds = (!builds : int)
        ; closes = (!closes : int)
        ; concurrent = (!maximum : int)
        ; invocations = (List.length state.invocations : int)
        ; history = (List.length state.conversation.canonical_history : int)
        }]);
  [%expect
    {|
    ("unload while owned" Conflict)
    ("administration while owned" Conflict)
    ((builds 2) (closes 2) (concurrent 2) (invocations 4) (history 0))
    |}]
;;

let%expect_test "close cancels callbacks and retires only after their cleanup finishes" =
  with_actor (fun _env sw actor _writer _backend ->
    let entered, entered_u = Eio.Promise.create () in
    let cleaning, cleaning_u = Eio.Promise.create () in
    let release, release_u = Eio.Promise.create () in
    let done_, done_u = Eio.Promise.create () in
    let joined, joined_u = Eio.Promise.create () in
    let never, _ = Eio.Promise.create () in
    let closes = ref 0 in
    let owner =
      Owner.create ~actor ~initial:None ~build:(fun () ->
        Ok (runtime ~close:(fun () -> incr closes) ()))
    in
    Eio.Fiber.fork ~sw (fun () ->
      let cancelled =
        try
          Owner.with_background_runtime owner (fun _ ->
            Exn.protect
              ~f:(fun () ->
                Eio.Promise.resolve entered_u ();
                Eio.Promise.await never;
                Ok ())
              ~finally:(fun () ->
                Eio.Cancel.protect (fun () ->
                  Eio.Promise.resolve cleaning_u ();
                  Eio.Promise.await release)))
          |> protocol_ok;
          false
        with
        | Eio.Cancel.Cancelled _ -> true
      in
      Eio.Promise.resolve done_u cancelled);
    Eio.Promise.await entered;
    Eio.Fiber.fork ~sw (fun () ->
      Owner.close_and_wait owner;
      Eio.Promise.resolve joined_u ());
    Eio.Promise.await cleaning;
    assert (Option.is_none (Eio.Promise.peek joined));
    [%test_eq: int] 0 !closes;
    [%test_eq: bool] true (Owner.is_loaded owner);
    reject "reload after close" (Owner.ensure_loaded owner);
    reject
      "new callback after close"
      (Owner.with_background_runtime owner (fun _ ->
         failwith "closed owner admitted work"));
    Eio.Promise.resolve release_u ();
    [%test_eq: bool] true (Eio.Promise.await done_);
    Eio.Promise.await joined;
    Owner.close owner;
    print_s [%sexp (!closes : int), (Owner.is_loaded owner : bool)]);
  [%expect
    {|
    ("reload after close" Server_shutting_down)
    ("new callback after close" Server_shutting_down)
    (1 false)
    |}]
;;

let%expect_test
    "callback errors release ownership and self-close cleanup cannot poison the mutex"
  =
  with_actor (fun _env _sw actor _writer _backend ->
    let closes = ref 0 in
    let owner =
      Owner.create ~actor ~initial:None ~build:(fun () ->
        Ok
          (runtime
             ~close:(fun () ->
               incr closes;
               failwith "fixture close failure")
             ()))
    in
    let callback_failed =
      try
        Owner.with_background_runtime owner (fun _ -> failwith "fixture callback failure")
        |> ignore;
        false
      with
      | Failure message -> String.equal message "fixture callback failure"
    in
    [%test_eq: bool] true callback_failed;
    let reused = ref false in
    Owner.with_background_runtime owner (fun _ ->
      reused := true;
      Ok ())
    |> protocol_ok;
    let close_failed =
      Result.try_with (fun () ->
        Owner.with_background_runtime owner (fun _ ->
          Owner.close owner;
          Ok ()))
      |> Result.is_error
    in
    [%test_eq: bool] true close_failed;
    reject "closed after cleanup error" (Owner.ensure_loaded owner);
    Owner.close owner;
    print_s [%sexp (!reused : bool), (!closes : int), (Owner.is_loaded owner : bool)]);
  [%expect
    {|
    ("closed after cleanup error" Server_shutting_down)
    (true 1 false)
    |}]
;;

let%expect_test
    "cancelled callback releases its lease even when cleanup waits for the owner mutex"
  =
  with_actor (fun _env sw actor _writer _backend ->
    let entered, entered_u = Eio.Promise.create () in
    let leaving, leaving_u = Eio.Promise.create () in
    let finished, finished_u = Eio.Promise.create () in
    let never, _ = Eio.Promise.create () in
    let closes = ref 0 in
    let owner =
      Owner.create ~actor ~initial:None ~build:(fun () ->
        Ok (runtime ~close:(fun () -> incr closes) ()))
    in
    Eio.Fiber.fork ~sw (fun () ->
      let was_cancelled =
        try
          Eio.Cancel.sub (fun context ->
            Owner.with_background_runtime owner (fun _ ->
              Eio.Promise.resolve entered_u (fun () -> Eio.Cancel.cancel context Exit);
              Exn.protect
                ~finally:(fun () -> Eio.Promise.resolve leaving_u ())
                ~f:(fun () ->
                  Eio.Promise.await never;
                  Ok ())))
          |> protocol_ok;
          false
        with
        | Eio.Cancel.Cancelled _ -> true
      in
      Eio.Promise.resolve finished_u was_cancelled);
    let cancel = Eio.Promise.await entered in
    Owner.For_testing.with_loaded_runtime owner (fun () ->
      cancel ();
      Eio.Promise.await leaving;
      Eio.Fiber.yield ();
      assert (Option.is_none (Eio.Promise.peek finished));
      Ok ())
    |> protocol_ok;
    [%test_eq: bool] true (Eio.Promise.await finished);
    Owner.close_and_wait owner;
    print_s [%sexp (!closes : int), (Owner.is_loaded owner : bool)]);
  [%expect {| (1 false) |}]
;;

let%expect_test "administration initialization fences committed stopped cleanup" =
  List.iter [ false; true ] ~f:(fun fenced ->
    let on_commit = ref (fun (_ : Agent_session.Session_state.t) -> ()) in
    with_actor
      ~state_committed:(fun state _ -> !on_commit state)
      (fun _env sw actor writer _backend ->
         let module State = Agent_session.Session_state in
         A.stop actor ~attachment_id:writer.id ~mode:Graceful |> protocol_ok |> ignore;
         let before = A.state actor |> protocol_ok in
         let queued, queue = Eio.Promise.create () in
         let entered, enter = Eio.Promise.create () in
         let release, finish = Eio.Promise.create () in
         let cleanup = ref None in
         let initialized = ref 0 in
         let initialization_revision = ref None in
         let closed = ref 0 in
         let owner =
           Owner.create_with_unload
             ~actor
             ~initial:(Some (runtime ~close:(fun () -> incr closed) ()))
             ~before_unload:(fun ~closing:_ ->
               if Option.is_none (Eio.Promise.peek entered)
               then Eio.Promise.resolve enter ();
               Eio.Promise.await release;
               Ok ())
             ~build:(fun () ->
               incr initialized;
               let expected = A.state actor |> protocol_ok in
               (match expected.runtime_initialization with
                | Pending _ -> ()
                | Ready -> failwith "initialization did not read the committed selection");
               let scope = A.begin_initialization actor ~expected |> protocol_ok in
               Exn.protect
                 ~f:(fun () ->
                   A.complete_initialization actor ~scope ~candidate:expected
                   |> protocol_ok
                   |> ignore)
                 ~finally:(fun () ->
                   Eio.Cancel.protect (fun () ->
                     A.end_initialization actor ~scope |> protocol_ok));
               initialization_revision
               := Some (A.state actor |> protocol_ok).counters.revision;
               Ok (runtime ~close:(fun () -> incr closed) ()))
         in
         Exn.protect
           ~finally:(fun () ->
             if Option.is_none (Eio.Promise.peek release)
             then Eio.Promise.resolve finish ())
           ~f:(fun () ->
             let candidate =
               Agent_session.Administration.plan_rebuild
                 before
                 (Agent_protocol.Id.Prompt_revision.create ())
               |> protocol_ok
             in
             let candidate =
               { candidate with
                 runtime_initialization = Pending { fresh_history = true }
               }
             in
             (on_commit
              := fun state ->
                   match state.runtime_initialization, !cleanup with
                   | Pending _, None ->
                     cleanup
                     := Some
                          (Eio.Fiber.fork_promise ~sw (fun () ->
                             Eio.Promise.resolve queue ();
                             Owner.unload_and_wait owner))
                   | Ready, _ | Pending _, Some _ -> ());
             let commit () =
               A.commit_reconciled_administration
                 actor
                 ~command_audit:None
                 ~attachment_id:writer.id
                 ~expected:before
                 ~kind:Rebuild
                 candidate
               |> Result.map ~f:(fun _ -> ())
             in
             let before_initialize () =
               (* This is the actual stop-cleanup task queued by State_committed. The
             split control lets it acquire ownership; the fenced route retains
             ownership through the same yield and all initialization admission. *)
               Eio.Promise.await queued;
               match fenced with
               | false -> Eio.Promise.await entered
               | true ->
                 Eio.Fiber.yield ();
                 assert (Option.is_none (Eio.Promise.peek entered))
             in
             let result =
               match fenced with
               | true ->
                 Owner.reinitialize_administration
                   owner
                   ~validate:(fun () -> Ok ())
                   ~commit
                   ~before_initialize
               | false ->
                 let open Result.Let_syntax in
                 let%bind () = Owner.unload owner in
                 let%bind () = commit () in
                 before_initialize ();
                 let%bind () = Owner.ensure_loaded owner in
                 let%map state = A.state actor in
                 State.summary state
             in
             (match fenced, result with
              | false, Error { code = Conflict; retryable = true; message; _ } ->
                [%test_eq: string]
                  "session runtime is waiting for background cleanup"
                  message;
                [%test_eq: int] 0 !initialized;
                let committed = A.state actor |> protocol_ok in
                [%test_eq: int]
                  (before.identity.generation + 1)
                  committed.identity.generation;
                [%test_eq: int64]
                  (Int64.succ before.counters.revision)
                  committed.counters.revision;
                (match committed.runtime_initialization with
                 | Pending _ -> ()
                 | Ready -> failwith "split route unexpectedly initialized")
              | true, Ok summary ->
                let ready = A.state actor |> protocol_ok in
                [%test_eq: int] 1 !initialized;
                [%test_eq: int] (before.identity.generation + 1) ready.identity.generation;
                [%test_eq: int64]
                  Int64.(before.counters.revision + 2L)
                  (Option.value_exn !initialization_revision);
                (* Runtime installation separately records its moderator snapshot. *)
                [%test_eq: int64]
                  Int64.(before.counters.revision + 3L)
                  ready.counters.revision;
                [%test_eq: int64] ready.counters.revision summary.revision;
                (match ready.runtime_initialization with
                 | Ready -> ()
                 | Pending _ -> failwith "fenced command returned Pending");
                assert (not (Owner.is_loaded owner))
              | _ -> failwith "administration fence changed the expected outcome");
             Eio.Promise.resolve finish ();
             Eio.Promise.await_exn (Option.value_exn !cleanup) |> protocol_ok;
             if not fenced
             then (
               Owner.ensure_loaded owner |> protocol_ok;
               Owner.unload owner |> protocol_ok);
             [%test_eq: int] 1 !initialized;
             [%test_eq: int] 2 !closed;
             Owner.close_and_wait owner;
             print_s [%sexp (fenced : bool), "one committed selection; cleanup joined"])));
  [%expect
    {|
    (false "one committed selection; cleanup joined")
    (true "one committed selection; cleanup joined")
    |}]
;;

exception Lease_primary
exception Lease_cleanup

let%expect_test "lease cleanup preserves primary failures and reports standalone cleanup" =
  List.iter [ `Typed; `Raised; `Success ] ~f:(fun outcome ->
    with_actor (fun _env _sw actor _writer _ ->
      let closes = ref 0 in
      let owner =
        Owner.create
          ~actor
          ~initial:
            (Some
               (runtime
                  ~close:(fun () ->
                    incr closes;
                    raise Lease_cleanup)
                  ()))
          ~build:(fun () -> failwith "unexpected lease reload")
      in
      let primary = Agent_protocol.Error.invalid_request "primary lease failure" in
      let observed =
        try
          match
            Owner.with_delegation_resources owner (fun _ ->
              Owner.unload_and_wait owner |> protocol_ok;
              match outcome with
              | `Typed -> Error primary
              | `Raised -> raise Lease_primary
              | `Success -> Ok ())
          with
          | Error error ->
            [%test_eq: Sexp.t]
              (Agent_protocol.Error.sexp_of_t error)
              (Agent_protocol.Error.sexp_of_t primary);
            `Typed
          | Ok () -> failwith "cleanup failure was lost"
        with
        | Lease_primary -> `Raised
        | Lease_cleanup -> `Success
      in
      assert (
        match outcome, observed with
        | `Typed, `Typed | `Raised, `Raised | `Success, `Success -> true
        | _ -> false);
      [%test_eq: int] 1 !closes;
      assert (not (Owner.is_loaded owner));
      Owner.close_and_wait owner));
  print_endline
    "typed error and exception retained; cleanup raised only after success; closed once";
  [%expect
    {| typed error and exception retained; cleanup raised only after success; closed once |}]
;;

let%expect_test
    "loaded-only inspection never builds and releases failed or cancelled borrows"
  =
  with_actor (fun _env _sw actor _writer _backend ->
    let builds = ref 0 in
    let owner =
      Owner.create ~actor ~initial:None ~build:(fun () ->
        Int.incr builds;
        failwith "cold inspection invoked the builder")
    in
    Exn.protect
      ~finally:(fun () -> Owner.close_and_wait owner)
      ~f:(fun () ->
        assert (Result.is_error (Owner.with_loaded_runtime owner (fun _ -> Ok ())));
        [%test_eq: int] 0 !builds));
  with_actor (fun env sw actor _writer _backend ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
      let builds = ref 0 in
      let build () =
        Int.incr builds;
        failwith "loaded-only inspection invoked the builder"
      in
      let closes = ref 0 in
      let entered, entered_u = Eio.Promise.create () in
      let finish, finish_u = Eio.Promise.create () in
      let owner =
        Owner.create_with_unload
          ~actor
          ~initial:(Some (runtime ~close:(fun () -> Int.incr closes) ()))
          ~build
          ~before_unload:(fun ~closing:_ ->
            if Option.is_none (Eio.Promise.peek entered)
            then Eio.Promise.resolve entered_u ();
            Eio.Promise.await finish;
            Ok ())
      in
      let release () =
        if Option.is_none (Eio.Promise.peek finish) then Eio.Promise.resolve finish_u ()
      in
      Exn.protect
        ~finally:(fun () ->
          release ();
          Owner.close_and_wait owner)
        ~f:(fun () ->
          let failure =
            Agent_protocol.Error.create
              Permission_denied
              ~message:"fixture rejection"
              ~retryable:false
              ()
          in
          (match Owner.with_loaded_runtime owner (fun _ -> Error failure) with
           | Error actual ->
             [%test_eq: Agent_protocol.Error.code] failure.code actual.code;
             [%test_eq: string] failure.message actual.message;
             [%test_eq: bool] failure.retryable actual.retryable;
             assert (Document_schema.Json.equal failure.data actual.data)
           | Ok _ -> assert false);
          let borrowed, borrowed_u = Eio.Promise.create () in
          let never, _ = Eio.Promise.create () in
          let caller =
            Eio.Fiber.fork_promise ~sw (fun () ->
              Eio.Cancel.sub (fun context ->
                Owner.with_loaded_runtime owner (fun _ ->
                  Eio.Promise.resolve borrowed_u context;
                  Eio.Promise.await never;
                  Ok ())))
          in
          Eio.Cancel.cancel (Eio.Promise.await borrowed) Exit;
          (match Eio.Promise.await caller with
           | Error (Eio.Cancel.Cancelled _) -> ()
           | _ -> failwith "borrow lost caller cancellation");
          let retired =
            Eio.Fiber.fork_promise ~sw (fun () -> Owner.unload_and_wait owner)
          in
          Eio.Promise.await entered;
          assert (Result.is_error (Owner.with_loaded_runtime owner (fun _ -> Ok ())));
          [%test_eq: int] 0 !builds;
          [%test_eq: int] 0 !closes;
          release ();
          Eio.Promise.await_exn retired |> protocol_ok;
          [%test_eq: int] 1 !closes;
          assert (Result.is_error (Owner.with_loaded_runtime owner (fun _ -> Ok ())));
          [%test_eq: int] 0 !builds;
          print_endline
            "unloaded/retiring deny; no builder; Error/cancellation release; unload joins")));
  [%expect
    {| unloaded/retiring deny; no builder; Error/cancellation release; unload joins |}]
;;
