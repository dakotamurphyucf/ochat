open! Core
open Agent_server_test_support
module A = Agent_session
module P = Agent_protocol
module Actor = A.Session_actor
module Daemon = Agent_server.Daemon
module Factory = Agent_server.Session_factory
module Registry = Agent_server.Session_registry
module Store = Agent_store.Session_store
module Attempt = Factory.Deferred_run_attempt
module Support = Deferred_source_support
module Existing = Source_preparation_tests

let attempted = function
  | Attempt.Outcome.Attempted attempt -> attempt
  | Retained receipt ->
    raise_s [%sexp "unexpected retained source request", (receipt : P.Run_receipt.t)]
;;

let rejected = function
  | Error error -> error
  | Ok _ -> failwith "expired source observation remained current"
;;

let assert_error expected actual =
  assert (Jsonaf.exactly_equal (P.Error.to_json expected) (P.Error.to_json actual))
;;

let with_fixture f =
  let constructions = ref 0 in
  let providers = ref 0 in
  let base =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
      Int.incr providers;
      failwith "source recovery dispatched inference")
  in
  let policy =
    { base with
      runtime_inference_ports =
        (fun actor ->
          Int.incr constructions;
          base.runtime_inference_ports actor)
    }
  in
  Creation_tests.with_daemon
    ~qualify_chatml_extensions:true
    ~prompt_text:Support.prompt_text
    ~policy
    (fun env sw root daemon ->
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
         Support.with_loaded_entry daemon (fun ~entry ~runtime:_ ~revision:_ ->
           f env sw root daemon policy constructions providers entry)))
;;

let%expect_test "omitting Run preparation from genuine initialization excludes absence" =
  with_fixture (fun env _sw _root daemon _policy constructions providers entry ->
    let before = Existing.state entry in
    let frames = Existing.frames env entry in
    let counts = !constructions, !providers in
    let original = Operator_authorization.trusted_local (principal ()) in
    let primary =
      P.Error.create
        Invalid_request
        ~message:"selected failure after actual initialization"
        ~retryable:false
        ~data:(`Object [ "retained", `Number "538" ])
        ()
    in
    let borrowed = ref None in
    let callbacks = ref 0 in
    let attempt =
      Factory.with_effect_deferred_run_source
        (Daemon.factory daemon)
        entry
        ~actor:original
        ~authorize:(fun _ -> Ok ())
        ~request:(Existing.headless_request entry "deferred-init-without-preparation")
        (fun _deferred _prepared ->
           Int.incr callbacks;
           let scope =
             Actor.begin_initialization_authenticated
               ~purpose:Runtime_construction
               entry.actor
               ~actor:original
               ~expected:(Existing.state entry)
             |> protocol_ok
           in
           Support.protect_cleanup
             ~finally:(fun () ->
               Actor.end_initialization entry.actor ~scope |> protocol_ok)
             (fun () ->
                let proof =
                  Actor.initialization_agent_actor entry.actor ~scope
                  |> protocol_ok
                  |> Option.value_exn
                in
                assert (Operator_authorization.is_current proof);
                borrowed := Some proof);
           Error primary)
      |> protocol_ok
      |> attempted
    in
    assert_error primary (Attempt.result attempt |> rejected);
    assert (Option.is_none (Attempt.no_effects attempt));
    assert (not (Operator_authorization.is_current (Option.value_exn !borrowed)));
    assert (Operator_authorization.is_current original);
    [%test_eq: int] 1 !callbacks;
    Existing.assert_same before entry;
    [%test_eq: string list] frames (Existing.frames env entry);
    [%test_eq: int * int] counts (!constructions, !providers);
    [%test_eq: int] 0 !providers;
    print_endline
      "actual initialization closes; exact primary remains; absence unavailable");
  [%expect {| actual initialization closes; exact primary remains; absence unavailable |}]
;;

let%expect_test "closed source custody does not recover with its durable session" =
  with_fixture (fun env sw root daemon policy constructions providers entry ->
    let original = Operator_authorization.trusted_local (principal ()) in
    let before = Existing.state entry in
    let session_id = before.identity.session_id in
    let old_request = Existing.headless_request entry "old-entry-new-factory" in
    let attempt =
      Factory.with_effect_deferred_run_source
        (Daemon.factory daemon)
        entry
        ~actor:original
        ~authorize:(fun _ -> Ok ())
        ~request:(Existing.headless_request entry "source-before-real-shutdown")
        (fun _ _ -> Ok ())
      |> protocol_ok
      |> attempted
    in
    Attempt.result attempt |> protocol_ok;
    let observation = Attempt.no_effects attempt |> Option.value_exn in
    Actor.No_effects_observation.check_current observation |> protocol_ok;
    Actor.No_effects_observation.check_basis observation |> protocol_ok;
    let old_store = Daemon.store daemon in
    Daemon.shutdown daemon |> protocol_ok;
    assert (Store.is_closed old_store);
    ignore
      (Actor.No_effects_observation.check_current observation |> rejected : P.Error.t);
    ignore (Actor.No_effects_observation.check_basis observation |> rejected : P.Error.t);
    let reopened =
      Daemon.start
        ~sw
        ~env:(env :> Eio_unix.Stdenv.base)
        ~config:(config root root (Filename.concat root "parent.chatmd"))
        ~tool_dir:root
        ~home:root
        ~process_start_identity:None
        ~options:
          { Daemon.default_options with
            inference_policy = policy
          ; qualify_chatml_extensions = true
          }
        ()
      |> protocol_ok
    in
    Support.protect_cleanup
      ~finally:(fun () -> Daemon.shutdown reopened |> protocol_ok)
      (fun () ->
         assert (not (phys_equal old_store (Daemon.store reopened)));
         assert (not (Store.is_closed (Daemon.store reopened)));
         let recovered =
           Registry.load (Daemon.registry reopened) session_id |> protocol_ok
         in
         assert (not (phys_equal entry.actor recovered.actor));
         [%test_eq: P.Id.Session.t]
           session_id
           (Existing.state recovered).identity.session_id;
         let original_startup =
           List.find_exn before.moderator_executions ~f:(fun receipt ->
             match receipt.P.Moderator_execution.context.phase, receipt.status with
             | Session_start, Completed _ -> true
             | _ -> false)
         in
         let new_resume (receipt : P.Moderator_execution.t) =
           P.Moderator_execution.equal_phase receipt.context.phase Session_resume
           && P.Invocation.equal_observer
                receipt.context.source
                original_startup.context.source
           && not
                (List.exists before.moderator_executions ~f:(fun previous ->
                   P.Id.Moderator_execution.equal previous.context.id receipt.context.id))
         in
         (* Recovery installs a fresh Lifecycle with a pending Session_resume.
            Old completed receipts and Idle alone do not join that activation. *)
         let rec await_settled () =
           let current =
             Actor.with_moderator_checkpoint recovered.actor (fun () ->
               Actor.state recovered.actor)
             |> protocol_ok
           in
           List.iter current.moderator_executions ~f:(fun receipt ->
             if new_resume receipt
             then (
               match receipt.status with
               | Failed _ | Interrupted _ ->
                 raise_s
                   [%sexp
                     "recovered lifecycle failed", (receipt : P.Moderator_execution.t)]
               | Running | Completed _ -> ()));
           if
             (match
                ( current.lifecycle.desired
                , current.lifecycle.observed
                , current.runtime_initialization )
              with
              | Running, Idle, Ready -> true
              | _ -> false)
             && Option.is_none current.active_operation
             && List.exists current.moderator_executions ~f:(fun receipt ->
               new_resume receipt
               && Int.equal receipt.context.generation current.identity.generation
               &&
               match receipt.status with
               | Completed _ -> true
               | Running | Failed _ | Interrupted _ -> false)
             && List.for_all current.moderator_executions ~f:(fun receipt ->
               match receipt.P.Moderator_execution.status with
               | Running -> false
               | Completed _ | Failed _ | Interrupted _ -> true)
           then current
           else (
             Eio.Fiber.yield ();
             await_settled ())
         in
         let confirmed = await_settled () in
         let frames = Existing.frames env recovered in
         let counts = !constructions, !providers in
         let callbacks = ref 0 in
         let error =
           Factory.with_effect_deferred_run_source
             (Daemon.factory reopened)
             entry
             ~actor:original
             ~authorize:(fun _ -> Ok ())
             ~request:old_request
             (fun _ _ ->
                Int.incr callbacks;
                Ok ())
           |> rejected
         in
         [%test_eq: P.Error.code] Permission_denied error.code;
         [%test_eq: int] 0 !callbacks;
         Existing.assert_same confirmed recovered;
         [%test_eq: string list] frames (Existing.frames env recovered);
         [%test_eq: int * int] counts (!constructions, !providers);
         [%test_eq: int] 0 !providers;
         ignore
           (Actor.No_effects_observation.check_current observation |> rejected
            : P.Error.t);
         ignore
           (Actor.No_effects_observation.check_basis observation |> rejected : P.Error.t);
         print_endline "same durable ID recovers under a new Actor and Store";
         print_endline "old entry is denied before callback; old absence stays closed"));
  [%expect
    {|
    same durable ID recovers under a new Actor and Store
    old entry is denied before callback; old absence stays closed
    |}]
;;
