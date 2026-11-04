open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module Request = A.Run_admission_request
module Service = Agent_server.Run_start_service

type fixture =
  { actor : A.Session_actor.t
  ; proof : Operator_authorization.t
  ; principal : P.Principal.t
  ; scope : A.Run_admission.Scope.t
  ; reference : P.Session_ref.t
  ; policy : bool ref
  ; auth_now : P.Timestamp.t ref
  ; expires_at : P.Timestamp.t
  ; parse_hook : (unit -> unit) ref
  ; constructions : int ref
  ; parses : int ref
  ; workers : int ref
  ; reject_commit : bool ref
  ; request :
      ?mode:P.Run.Mode.t -> ?input:P.Run_start.Input.t -> string -> Request.Headless.t
  ; start :
      ?proof:Operator_authorization.t -> Request.Headless.t -> A.Run_admission_outcome.t
  ; start_writer : Request.Headless.t -> A.Run_admission_outcome.t
  ; activate : unit -> (bool, P.Error.t) result
  }

let with_service
      ?make_persistence
      ?prepare
      ?(keep_writer = false)
      ?events
      ?worker
      ?(authorize_hook = fun () -> ())
      f
  =
  let events =
    Option.value
      events
      ~default:
        {| | `Session_start ->
       let ignored = state[0] <- state[0] + 1 in Task.pure(state)
       | _ -> Task.pure(state) |}
  in
  Run_admission_tests.with_startup_actor
    ?make_persistence
    ?prepare
    ~events
    (fun actor _ reject_commit _ _ manager _ reference attachment ->
       if not keep_writer
       then (
         A.Session_actor.detach actor attachment.id |> protocol_ok;
         assert (List.is_empty (A.Session_actor.state actor |> protocol_ok).attachments));
       let constructions = ref 0
       and parses = ref 0
       and workers = ref 0 in
       let policy = ref true in
       let auth_now = ref timestamp in
       let expires_at = P.Timestamp.add_ms timestamp 1 |> protocol_ok in
       let principal =
         P.Principal.create
           ~id:principal_id
           ~authentication_kind:"local"
           ~scopes:(P.Scope.Set.of_list [ Send_messages ])
           ~attributes:[]
         |> protocol_ok
       in
       let proof =
         Operator_authorization.bounded ~principal ~now:(fun () -> !auth_now) ~expires_at
         |> protocol_ok
       in
       let lifecycle = A.Moderator_event.Lifecycle.create ~manager ~resume:false in
       let snapshot () =
         Chat_response.Moderator_manager.identity_snapshot manager
         |> Result.ok_or_failwith
         |> A.Moderator_checkpoint.encode
         |> Option.some
       in
       let activate () =
         A.Moderator_event.Lifecycle.run
           lifecycle
           ~claim:(fun ~event ->
             A.Session_actor.with_current_moderator_event actor ~operation_id:None ~event)
           ~run_actions:(A.Session_actor.run_actions actor)
           ~history:(fun () -> [])
           ~available_tools:[]
           ~session_meta:`Null
           ~now:(fun () -> timestamp)
           ()
         |> Result.map ~f:(function
           | Activated _ -> true
           | Unavailable | Already_active -> false)
       in
       let parse_hook = ref (fun () -> ()) in
       let worker =
         Option.value
           worker
           ~default:
             (A.Operation_worker.create ~run:(fun ~sw:_ ~input capabilities ->
                let authority =
                  capabilities.original_agent_actor () |> protocol_ok |> Option.value_exn
                in
                assert (Operator_authorization.is_current authority);
                Int.incr workers;
                A.Operation_worker.Completed
                  { final_history = input.history
                  ; moderator_snapshot = snapshot ()
                  ; runtime_requests = []
                  }))
       in
       let base = Runtime_lease_tests.runtime ~close:(fun () -> ()) () in
       let runtime =
         { base with
           worker
         ; moderator_manager = Some manager
         ; moderator_snapshot = snapshot
         ; moderator_activation =
             Some
               { pending = (fun () -> A.Moderator_event.Lifecycle.pending lifecycle)
               ; startup_pending =
                   (fun () -> A.Moderator_event.Lifecycle.startup_pending lifecycle)
               ; run = activate
               }
         ; parse_user_content =
             (fun ~id content ->
               Int.incr parses;
               !parse_hook ();
               Ok (A.History_codec.user_text ~id content.text))
         }
       in
       let owner =
         Agent_server.Runtime_owner.create_with_authenticated_run_unload
           ~actor
           ~initial:None
           ~before_unload:(fun ~closing:_ -> Ok ())
           ~build:(fun ~original_actor _preparation ->
             let original_actor = Option.value_exn original_actor in
             assert (phys_equal original_actor proof);
             assert (Operator_authorization.is_current original_actor);
             Int.incr constructions;
             Ok runtime)
       in
       let history_ids =
         A.History_id_source.create
           ~namespace:"headless"
           ~block_size:1
           ~reserve:(fun ~count -> A.Session_actor.reserve_history_block actor ~count)
         |> protocol_ok
       in
       let entry : Agent_server.Session_registry.entry =
         { actor
         ; history_ids
         ; runtime = owner
         ; durable_events = A.Durable_event_log.create ~capacity:64 [] |> protocol_ok
         ; capacity = None
         ; store_handle = None
         ; expire_permissions = (fun ~now:_ -> ())
         ; collect_results = (fun () -> Ok None)
         ; close = (fun () -> ())
         }
       in
       let service =
         Service.create
           ~server_id:(P.Session_ref.server_id reference)
           ~authorize:(fun proof state ->
             authorize_hook ();
             if
               !policy
               && Option.equal
                    P.Id.Principal.equal
                    (Some (Operator_authorization.principal proof).id)
                    state.identity.creating_principal
             then Ok ()
             else
               Error
                 (P.Error.create
                    Permission_denied
                    ~message:"host policy"
                    ~retryable:false
                    ()))
       in
       let scope =
         A.Run_admission.Scope.create
           ~principal_id
           ~observer:
             (Chat_response.Moderator_manager.invocation_observer manager
              |> Option.value_exn)
           ~startup_pending:(fun () ->
             A.Moderator_event.Lifecycle.startup_pending lifecycle)
           ~authorize:
             (A.Run_command_authorization.check proof ~authorize:(fun _ ->
                if !policy
                then Ok ()
                else
                  Error
                    (P.Error.create
                       Permission_denied
                       ~message:"host policy"
                       ~retryable:false
                       ())))
         |> protocol_ok
       in
       let request
             ?(mode = P.Run.Mode.Workflow)
             ?(input = P.Run_start.Input.Authored_start)
             key
         =
         let state = A.Session_actor.state actor |> protocol_ok in
         Request.Headless.create
           ~session_id
           ~generation:state.identity.generation
           ~expected_revision:state.counters.revision
           ~mode
           ~input
           ~key:(P.Idempotency_key.of_string key |> protocol_ok)
         |> protocol_ok
       in
       let start ?(proof = proof) request =
         Service.start_headless_command
           service
           ~actor:proof
           ~entry
           ~request
           ~command_audit:None
       in
       let start_writer (request : Request.Headless.t) =
         let request =
           P.Run_start.create
             ~session_id:request.session_id
             ~attachment_id:attachment.id
             ~generation:request.generation
             ~expected_revision:request.expected_revision
             ~mode:request.mode
             ~input:request.input
             ~key:request.key
           |> protocol_ok
         in
         Service.start service ~actor:proof ~entry ~request ~command_audit:None
       in
       Exn.protect
         ~finally:(fun () -> Agent_server.Runtime_owner.close_and_wait owner)
         ~f:(fun () ->
           f
             { actor
             ; proof
             ; principal
             ; scope
             ; reference
             ; policy
             ; auth_now
             ; expires_at
             ; parse_hook
             ; constructions
             ; parses
             ; workers
             ; reject_commit
             ; request
             ; start
             ; start_writer
             ; activate
             }))
;;

let admitted = function
  | A.Run_admission_outcome.Admitted receipt -> receipt
  | Rejected error | Uncertain error -> raise_s [%sexp (error : P.Error.t)]
;;

let rejected = function
  | A.Run_admission_outcome.Rejected _ -> true
  | Admitted _ | Uncertain _ -> false
;;

let input text =
  P.Run_start.Input.User_submission
    { P.Session.Message_content.kind = Plain_text; text; attachments = [] }
;;

let%expect_test "real headless command admits once without a writer or startup replay" =
  with_service (fun t ->
    let request = t.request "headless-start" in
    let receipt = t.start request |> admitted in
    assert (Int.equal !(t.constructions) 1);
    assert (Int.equal !(t.parses) 0);
    assert (t.activate () |> protocol_ok);
    let before = A.Session_actor.state t.actor |> protocol_ok in
    let replay = t.start request |> admitted in
    let after = A.Session_actor.state t.actor |> protocol_ok in
    assert (P.Run_receipt.equal receipt replay);
    assert (Int64.equal before.counters.revision after.counters.revision);
    assert (not (t.activate () |> protocol_ok));
    assert (rejected (t.start (t.request "consumed-startup")));
    assert (Int.equal !(t.constructions) 1);
    assert (List.is_empty after.attachments);
    t.policy := false;
    assert (rejected (t.start request));
    t.policy := true;
    t.auth_now := t.expires_at;
    assert (rejected (t.start request));
    print_endline
      "one construction and actual startup; retained receipt grants no authority");
  [%expect
    {| one construction and actual startup; retained receipt grants no authority |}]
;;

let%expect_test "headless single-turn input owns one parsed history and one worker" =
  with_service (fun t ->
    t.start (t.request "input-start") |> admitted |> ignore;
    assert (t.activate () |> protocol_ok);
    let request = t.request ~mode:Single_turn ~input:(input "hello") "headless-input" in
    let receipt = t.start request |> admitted in
    let after = await_idle t.actor in
    let next_sequence = after.conversation.next_history_sequence in
    let replay = t.start request |> admitted in
    let replayed = A.Session_actor.state t.actor |> protocol_ok in
    assert (P.Run_receipt.equal receipt replay);
    assert (Int64.equal next_sequence replayed.conversation.next_history_sequence);
    assert (Int.equal !(t.parses) 1);
    assert (Int.equal !(t.workers) 1);
    assert (List.length replayed.conversation.canonical_history = 1);
    assert (List.is_empty replayed.attachments);
    print_endline "retained input receipt precedes parser, history allocator and worker");
  [%expect {| retained input receipt precedes parser, history allocator and worker |}]
;;

let%expect_test "current proof and host policy are checked again after yielding parse" =
  List.iter [ `Expiry; `Policy ] ~f:(fun revoke ->
    with_service (fun t ->
      t.start (t.request "revocation-start") |> admitted |> ignore;
      assert (t.activate () |> protocol_ok);
      let request =
        t.request ~mode:Single_turn ~input:(input "blocked") "revoked-input"
      in
      (t.parse_hook
       := fun () ->
            Eio.Fiber.yield ();
            match revoke with
            | `Expiry -> t.auth_now := t.expires_at
            | `Policy -> t.policy := false);
      assert (rejected (t.start request));
      let after = A.Session_actor.state t.actor |> protocol_ok in
      assert (List.is_empty after.conversation.canonical_history);
      assert (Int.equal !(t.workers) 0);
      assert (Int.equal !(t.parses) 1);
      assert (List.length (A.Run_state.runs (Option.value_exn after.run_state)) = 1)));
  print_endline "expiry and policy revocation after preparation prevent input admission";
  [%expect {| expiry and policy revocation after preparation prevent input admission |}]
;;

let%expect_test "intent, scope and digest cannot bypass actual headless command authority"
  =
  with_service (fun t ->
    let request = t.request "bad-scope" in
    let no_scope = { t.principal with scopes = P.Scope.Set.empty } in
    let proof = Operator_authorization.trusted_local no_scope in
    assert (rejected (t.start ~proof request));
    assert (Int.equal !(t.constructions) 0);
    let foreign =
      { t.principal with id = P.Id.Principal.of_string "pri_other" |> protocol_ok }
      |> Operator_authorization.trusted_local
    in
    assert (rejected (t.start ~proof:foreign request));
    assert (Int.equal !(t.constructions) 0);
    (* Host policy can revoke the same original proof before returning success.
       That callback cannot authorize the subsequent admission. *)
    let state = A.Session_actor.state t.actor |> protocol_ok in
    let authorize _ =
      t.auth_now := t.expires_at;
      Ok ()
    in
    (match A.Run_command_authorization.check t.proof ~authorize state with
     | Error error -> [%test_eq: P.Error.code] Permission_denied error.code
     | Ok () -> failwith "host callback renewed expired original proof");
    assert (Int.equal !(t.constructions) 0);
    let writer =
      P.Run_start.create
        ~session_id:request.session_id
        ~attachment_id:(P.Id.Attachment.of_string "att_unused" |> protocol_ok)
        ~generation:request.generation
        ~expected_revision:request.expected_revision
        ~mode:request.mode
        ~input:request.input
        ~key:request.key
      |> protocol_ok
    in
    let old_digest =
      P.Json_codec.canonical_string (P.Run_start.to_json writer)
      |> protocol_ok
      |> Digestif.SHA256.digest_string
      |> Digestif.SHA256.to_hex
    in
    let writer_digest =
      Request.request_sha256 (Request.of_writer writer) |> protocol_ok
    in
    let headless_digest =
      Request.request_sha256 (Request.of_headless request) |> protocol_ok
    in
    assert (String.equal writer_digest old_digest);
    assert (not (String.equal writer_digest headless_digest));
    print_endline
      "proof and current policy required; wire digest unchanged and callers disjoint");
  [%expect
    {| proof and current policy required; wire digest unchanged and callers disjoint |}]
;;

let prepare t request =
  A.Session_actor.begin_headless_run_preparation
    t.actor
    ~actor:t.proof
    ~authorize:(fun _ -> Ok ())
    ~request
  |> protocol_ok
  |> function
  | A.Run_preparation.Decision.Prepare permit -> permit
  | Retained _ -> failwith "fresh headless request unexpectedly retained"
;;

let finish t preparation =
  A.Session_actor.admit_prepared_run
    t.actor
    ~command_audit:None
    ~preparation
    ~scope:t.scope
    ~session:t.reference
    ~entry:None
  |> protocol_ok
;;

let%expect_test "headless permit is exact, exclusive and invalidated by ordinary state" =
  with_service (fun t ->
    let request = t.request "permit" in
    let actual = prepare t request in
    let state = A.Session_actor.state t.actor |> protocol_ok in
    let normalized = Request.of_headless request in
    let foreign =
      A.Run_preparation.create_request
        ~owner:(A.Run_preparation.Owner.create ())
        ~state
        ~principal_id
        ~request:normalized
        ~request_sha256:(Request.request_sha256 normalized |> protocol_ok)
        ~authorize:(fun _ -> Ok ())
      |> protocol_ok
    in
    assert (rejected (finish t foreign));
    assert (
      Result.is_error
        (A.Session_actor.begin_headless_run_preparation
           t.actor
           ~actor:t.proof
           ~authorize:(fun _ -> Ok ())
           ~request));
    A.Session_actor.end_run_preparation t.actor actual |> protocol_ok;
    assert (rejected (finish t actual));
    let stale = t.request "stale-cas" in
    A.Session_actor.reserve_history_block t.actor ~count:1 |> protocol_ok |> ignore;
    assert (rejected (t.start stale));
    assert (Int.equal !(t.constructions) 0);
    let displaced = prepare t (t.request "source-removed") in
    A.Session_actor.change_moderator t.actor None |> protocol_ok |> ignore;
    assert (rejected (finish t displaced));
    let after = A.Session_actor.state t.actor |> protocol_ok in
    assert (Option.is_none after.run_state);
    assert (Int.equal !(t.workers) 0));
  print_endline "foreign, closed, stale and source-displaced permits cannot admit a Run";
  [%expect {| foreign, closed, stale and source-displaced permits cannot admit a Run |}]
;;

let%expect_test
    "headless failed input save cannot dispatch; committed input retains its receipt"
  =
  with_service (fun t ->
    t.start (t.request "save-startup") |> admitted |> ignore;
    assert (t.activate () |> protocol_ok);
    let failed_request =
      t.request ~mode:Single_turn ~input:(input "not committed") "failed-input-save"
    in
    (* Reserve history successfully, then fail the actual input/Run checkpoint.
       This is a definite fixture no-write failure, not simulated lost-write ACK. *)
    (t.parse_hook := fun () -> t.reject_commit := true);
    let failed = t.start failed_request in
    assert (
      match failed with
      | A.Run_admission_outcome.Uncertain _ -> true
      | Rejected _ | Admitted _ -> false);
    assert (Int.equal !(t.workers) 0);
    let failed_state = A.Session_actor.state t.actor |> protocol_ok in
    assert (List.is_empty failed_state.conversation.canonical_history);
    assert (List.length (A.Run_state.runs (Option.value_exn failed_state.run_state)) = 1);
    t.reject_commit := false;
    (t.parse_hook := fun () -> ());
    (* A separate authorized command after inspecting the failed no-write fixture;
       no refreshed CAS or new key is presented as automatic uncertainty retry. *)
    let request =
      t.request ~mode:Single_turn ~input:(input "committed") "accepted-after-save"
    in
    let receipt = t.start request |> admitted in
    let after = await_idle t.actor in
    let replay = t.start request |> admitted in
    let reconciled = A.Session_actor.state t.actor |> protocol_ok in
    assert (P.Run_receipt.equal receipt replay);
    assert (Int64.equal after.counters.revision reconciled.counters.revision);
    assert (Int.equal !(t.parses) 2);
    assert (Int.equal !(t.workers) 1));
  print_endline
    "failed input checkpoint dispatches nothing; committed input retry repeats nothing";
  [%expect
    {| failed input checkpoint dispatches nothing; committed input retry repeats nothing |}]
;;

let%expect_test "headless Workflow preserves the one unfinished workflow invariant" =
  with_service (fun t ->
    t.start (t.request "workflow-one") |> admitted |> ignore;
    assert (t.activate () |> protocol_ok);
    let request = t.request ~input:(input "second workflow") "workflow-two" in
    assert (rejected (t.start request));
    let after = A.Session_actor.state t.actor |> protocol_ok in
    assert (List.is_empty after.conversation.canonical_history);
    assert (Int.equal !(t.workers) 0);
    assert (List.length (A.Run_state.runs (Option.value_exn after.run_state)) = 1));
  print_endline "headless admission cannot create a second source-bound Workflow";
  [%expect {| headless admission cannot create a second source-bound Workflow |}]
;;

let%expect_test "cancelled queued writer and headless acquisition release actor custody" =
  List.iter [ `Writer; `Headless ] ~f:(fun caller ->
    let gate = ref false in
    let entered, entered_u = Eio.Promise.create () in
    let release, release_u = Eio.Promise.create () in
    let make_persistence _env _sw _initial : A.Session_actor.persistence =
      { archive_reference = Fixtures.archive_reference
      ; commit =
          (fun ~command_audit:_ ~previous:_ _transition ->
            if !gate
            then (
              gate := false;
              Eio.Promise.resolve entered_u ();
              Eio.Promise.await release);
            Ok ())
      }
    in
    let keep_writer =
      match caller with
      | `Writer -> true
      | `Headless -> false
    in
    with_service ~make_persistence ~keep_writer (fun t ->
      Eio.Switch.run (fun sw ->
        let before = A.Session_actor.state t.actor |> protocol_ok in
        (* The blocking reservation commits exactly one revision before the
           queued begin can run, so its original CAS is valid when processed. *)
        let request =
          Request.Headless.create
            ~session_id
            ~generation:before.identity.generation
            ~expected_revision:(Int64.succ before.counters.revision)
            ~mode:Workflow
            ~input:Authored_start
            ~key:(P.Idempotency_key.of_string "cancelled-queued-start" |> protocol_ok)
          |> protocol_ok
        in
        gate := true;
        let blocking =
          Eio.Fiber.fork_promise ~sw (fun () ->
            A.Session_actor.reserve_history_block t.actor ~count:1)
        in
        Eio.Promise.await entered;
        let context, context_u = Eio.Promise.create () in
        let pending =
          Eio.Fiber.fork_promise ~sw (fun () ->
            Eio.Cancel.sub (fun cancellation ->
              Eio.Promise.resolve context_u cancellation;
              match caller with
              | `Writer -> t.start_writer request
              | `Headless -> t.start request))
        in
        let cancellation = Eio.Promise.await context in
        (* After the signal the synchronous service reaches actor enqueue/await
           without another yielding operation. Give it its scheduler turn while
           the actor is deterministically held at the persistence gate. *)
        Eio.Fiber.yield ();
        Eio.Cancel.cancel cancellation Exit;
        Eio.Promise.resolve release_u ();
        Eio.Promise.await_exn blocking |> protocol_ok |> ignore;
        let cancelled =
          match Eio.Promise.await pending with
          | Error (Eio.Cancel.Cancelled _) -> true
          | Error exn -> Exn.reraise exn "unexpected queued-start failure"
          | Ok _ -> false
        in
        assert cancelled;
        assert (Int.equal !(t.constructions) 0);
        assert (Int.equal !(t.parses) 0);
        assert (Int.equal !(t.workers) 0);
        let after = A.Session_actor.state t.actor |> protocol_ok in
        assert (Option.is_none after.run_state);
        let fresh = t.request "after-cancelled-acquisition" in
        (match caller with
         | `Writer -> t.start_writer fresh
         | `Headless -> t.start fresh)
        |> admitted
        |> ignore;
        assert (Int.equal !(t.constructions) 1);
        assert (Int.equal !(t.workers) 0))));
  print_endline
    "both queued callers cancel before construction; fresh admission obtains custody";
  [%expect
    {| both queued callers cancel before construction; fresh admission obtains custody |}]
;;

let%expect_test
    "authored-start continuation carries only its exact admitted original proof"
  =
  let events =
    {| | `Session_start -> Task.bind(Run.continue(), fun ignored -> Task.pure(state))
       | _ -> Task.pure(state) |}
  in
  with_service ~events (fun t ->
    t.start (t.request "authority-authored-continuation") |> admitted |> ignore;
    assert (t.activate () |> protocol_ok);
    assert (A.Session_actor.apply_moderator_follow_up t.actor |> protocol_ok);
    await_idle t.actor |> ignore;
    (* with_service's real worker requires Some/current from the original Run
       port; its actor-owned Runtime_owner build also checks physical input. *)
    [%test_eq: int] 1 !(t.workers);
    assert (Operator_authorization.is_current t.proof));
  print_endline
    "actual compiled startup Continue -> owned operation -> original Run authority";
  [%expect
    {| actual compiled startup Continue -> owned operation -> original Run authority |}]
;;
