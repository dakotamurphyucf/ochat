open! Core
open Agent_server_test_support
module A = Agent_session
module Actor = A.Session_actor
module Owner = Agent_server.Runtime_owner
module Factory = Agent_server.Session_factory
module Daemon = Agent_server.Daemon
module P = Agent_protocol
module Source = Deferred_source_support
module Child = Child_usage_fault_tests
module H = Agent_client.Session_handle

(* This is the real inference-port construction boundary, selected by the SAME
   actual child Actor. It creates no permission or resource and is never called
   by production solely for this test. The source/parent loan already spans it. *)
module Gate = struct
  type t =
    { mutable actor : Actor.t option
    ; mutable protected : bool
    ; entered : unit Eio.Promise.t
    ; enter : unit Eio.Promise.u
    ; released : unit Eio.Promise.t
    ; release : unit Eio.Promise.u
    }

  let create () =
    let entered, enter = Eio.Promise.create () in
    let released, release = Eio.Promise.create () in
    { actor = None; protected = true; entered; enter; released; release }
  ;;

  let pass t actor =
    if Option.exists t.actor ~f:(phys_equal actor)
    then (
      t.actor <- None;
      Eio.Promise.resolve t.enter ();
      if t.protected
      then Eio.Cancel.protect (fun () -> Eio.Promise.await t.released)
      else Eio.Promise.await t.released)
  ;;

  let release t =
    if Option.is_none (Eio.Promise.peek t.released) then Eio.Promise.resolve t.release ()
  ;;
end

let definition env root runtime =
  let native = Option.value_exn runtime.A.Runtime_builder.native_runtime in
  let capabilities =
    Lazy.force native.capabilities
    |> Result.map_error ~f:(fun error -> error.Chat_response.Tool_capability.message)
    |> Result.ok_or_failwith
  in
  let bundle =
    Chatmd_source_bundle.create
      ~root_file:"child.chatmd"
      ~sources:
        [ ( "child.chatmd"
          , {|<config model="fixture-model"/><developer>Actual delegated load custody.</developer>
<script id="load" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = match event with
| `Session_resume -> Task.pure(state + 1)
| _ -> Task.pure(state)
</script>|}
          )
        ]
      ()
    |> Result.ok_or_failwith
  in
  A.Generated_definition.prepare
    ~env
    ~dir:Eio.Path.(Eio.Stdenv.fs env / root)
    ~revision_id:(P.Id.Prompt_revision.create ())
    ~created_at:(P.Timestamp.now ())
    ~current_capabilities:(fun () -> capabilities)
    ~references:[]
    bundle
  |> Result.map_error ~f:(fun diagnostics ->
    P.Error.invalid_request
      (List.map diagnostics ~f:Chatmd_shell_spec.Diagnostic.to_string
       |> String.concat ~sep:"\n"))
  |> protocol_ok
;;

let with_child
      ?(wrap_env = Fn.id)
      ?configure
      ?(on_inference_ports = ignore)
      ?(on_post_stream = ignore)
      f
  =
  let gate = Gate.create () in
  let providers = ref 0 in
  let policy =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
      incr providers;
      on_post_stream ();
      Retry_support.completed_response "unexpected delegated load provider")
  in
  let policy =
    { policy with
      runtime_inference_ports =
        (fun actor ->
          on_inference_ports actor;
          Gate.pass gate actor;
          policy.runtime_inference_ports actor)
    }
  in
  Creation_tests.with_daemon
    ~wrap_env
    ?configure
    ~prompt_text:Source.prompt_text
    ~qualify_chatml_extensions:true
    ~policy
    (fun env sw root daemon ->
       Eio.Time.with_timeout_exn env#clock 30.0 (fun () ->
         Source.with_loaded_entry daemon (fun ~entry:parent ~runtime ~revision:_ ->
           let definition = definition env root runtime in
           let key = P.Idempotency_key.of_string "561-actual-child" |> protocol_ok in
           let child =
             Child.create_after_idle
               (Daemon.factory daemon)
               (Actor.state parent.actor |> protocol_ok).identity.session_id
               key
               definition
             |> protocol_ok
           in
           let current = Actor.state child.actor |> protocol_ok in
           assert (Owner.is_loaded child.runtime);
           (match current.runtime_initialization with
            | Ready -> ()
            | Pending _ -> failwith "actual child startup did not initialize");
           assert (not (Owner.has_activation_handoff child.runtime));
           [%test_eq: int] 0 !providers;
           Creation_tests.protect_cleanup
             ~finally:(fun () -> Gate.release gate)
             (fun () -> f env sw daemon parent child gate providers))))
;;

let expect_background_busy = function
  | Error { P.Error.code = Conflict; retryable = true; _ } -> ()
  | Error error -> raise_s [%sexp (error : P.Error.t)]
  | Ok () -> failwith "actual parent source loan was not retained"
;;

let capture_load f =
  try Owner.Scope_outcome.Returned (f ()) with
  | exn -> Owner.Scope_outcome.Raised (exn, Stdlib.Printexc.get_raw_backtrace ())
;;

let expect_cancelled_load = function
  | Owner.Scope_outcome.Returned (Error { code = Conflict | Interrupted; _ }) -> ()
  | Raised (Eio.Cancel.Cancelled _, _) -> ()
  | Returned (Error error) -> raise_s [%sexp (error : P.Error.t)]
  | Returned (Ok _) -> failwith "cancelled child load reported success"
  | Raised (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;

let%expect_test "delegated cold source wait permits Owner status and actual Stop joins" =
  with_child (fun _env sw _daemon parent child gate providers ->
    Owner.unload_and_wait child.runtime |> protocol_ok;
    assert (not (Owner.is_loaded child.runtime));
    gate.actor <- Some child.actor;
    let loaded, loaded_u = Eio.Promise.create () in
    Eio.Fiber.fork ~sw (fun () ->
      Eio.Promise.resolve
        loaded_u
        (capture_load (fun () -> Owner.ensure_loaded child.runtime)));
    Eio.Promise.await gate.entered;
    (* This positive observation takes the actual Owner mutex while construction
       remains blocked. A timeout alone would not prove lock availability. *)
    assert (not (Owner.is_loaded child.runtime));
    assert (Option.is_none (Eio.Promise.peek loaded));
    Owner.unload parent.runtime |> expect_background_busy;
    let attachment, _ =
      Actor.attach child.actor ~mode:Read_write ~subscribe:false |> protocol_ok
    in
    Creation_tests.protect_cleanup
      ~finally:(fun () ->
        Gate.release gate;
        Actor.detach child.actor attachment.id |> protocol_ok)
      (fun () ->
         Actor.stop child.actor ~attachment_id:attachment.id ~mode:Cancel
         |> protocol_ok
         |> ignore;
         let stopped, stopped_u = Eio.Promise.create () in
         Eio.Fiber.fork ~sw (fun () ->
           Eio.Promise.resolve stopped_u (Owner.unload_and_wait child.runtime));
         (* Stop has entered, but its real source lease cannot join while the
            protected actual construction boundary is still waiting. *)
         let current = Actor.state child.actor |> protocol_ok in
         (match current.lifecycle.desired with
          | Stopped -> ()
          | Running -> failwith "actual Stop did not enter during source wait");
         assert (Option.is_none (Eio.Promise.peek loaded));
         assert (Option.is_none (Eio.Promise.peek stopped));
         Gate.release gate;
         Eio.Promise.await loaded |> expect_cancelled_load;
         Eio.Promise.await stopped |> protocol_ok;
         assert (not (Owner.is_loaded child.runtime));
         assert (Owner.source_lane_idle child.runtime);
         assert (not (Owner.has_activation_handoff child.runtime));
         [%test_eq: int] 0 !providers;
         (* This succeeds only after the child's actual parent capability loan
            has left its joined source scope. *)
         Owner.unload parent.runtime |> protocol_ok));
  print_endline "real source wait: Owner status and Stop enter; source/resources join";
  [%expect {| real source wait: Owner status and Stop enter; source/resources join |}]
;;

module Public_retirement = struct
  type t =
    | Unload
    | Close
end

module Administrative_schedulers = struct
  type wall_waiter =
    { deadline : float
    ; mutable parked : bool
    }

  type t =
    { mutable pause : unit -> unit
    ; mutable resume : unit -> unit
    ; mutable wall_waiting : float -> bool
    ; mutable mono_now : unit -> Mtime.t
    }

  let wall_clock real_clock =
    let paused = ref false in
    let changed = ref (Eio.Promise.create ()) in
    let waiters = ref [] in
    let notify_change () =
      let _, resolve = !changed in
      changed := Eio.Promise.create ();
      Eio.Promise.resolve resolve ()
    in
    let module Clock = struct
      type t = unit
      type time = float

      (* Persistence timestamps always follow the real clock, even while an
         administrative test owns the scheduler's sleep continuation. *)
      let now () = Eio.Time.now real_clock

      let sleep_until () deadline =
        let waiter = { deadline; parked = false } in
        waiters := waiter :: !waiters;
        Exn.protect
          ~finally:(fun () ->
            waiters
            := List.filter !waiters ~f:(fun current -> not (phys_equal current waiter)))
          ~f:(fun () ->
            let rec wait () =
              let change, _ = !changed in
              if !paused
              then (
                waiter.parked <- true;
                Eio.Promise.await change;
                waiter.parked <- false;
                wait ())
              else if Float.(deadline <= now ())
              then Eio.Fiber.yield ()
              else (
                waiter.parked <- false;
                Eio.Fiber.first
                  (fun () -> Eio.Time.sleep_until real_clock deadline)
                  (fun () -> Eio.Promise.await change);
                wait ())
            in
            wait ())
      ;;
    end
    in
    let pause () =
      paused := true;
      notify_change ()
    in
    let resume () =
      paused := false;
      notify_change ()
    in
    let waiting deadline =
      List.exists !waiters ~f:(fun waiter ->
        waiter.parked && Float.equal waiter.deadline deadline)
    in
    Eio.Resource.T ((), Eio.Time.Pi.clock (module Clock)), pause, resume, waiting
  ;;

  let create () =
    { pause = ignore
    ; resume = ignore
    ; wall_waiting = (fun _ -> failwith "administrative wall sleeper is uninitialized")
    ; mono_now = (fun () -> failwith "administrative monotonic clock is uninitialized")
    }
  ;;

  let wrap t (env : Eio_unix.Stdenv.base) =
    let clock, pause_wall, resume_wall, wall_waiting = wall_clock env#clock in
    let mono_clock, pause_mono, resume_mono, _ =
      controlled_monotonic_clock env#mono_clock
    in
    t.pause
    <- (fun () ->
         pause_wall ();
         pause_mono ());
    t.resume
    <- (fun () ->
         resume_mono ();
         resume_wall ());
    t.wall_waiting <- wall_waiting;
    t.mono_now <- (fun () -> Eio.Time.Mono.now mono_clock);
    (object
       method clock = clock
       method mono_clock = mono_clock
       method domain_mgr = env#domain_mgr
       method stdin = env#stdin
       method stdout = env#stdout
       method stderr = env#stderr
       method net = env#net
       method process_mgr = env#process_mgr
       method fs = env#fs
       method cwd = env#cwd
       method secure_random = env#secure_random
       method debug = env#debug
       method backend_id = env#backend_id
     end
     : Eio_unix.Stdenv.base)
  ;;

  let park t (env : Eio_unix.Stdenv.base) daemon =
    t.pause ();
    (* with_child passes its original environment to the callback; only the
       daemon has wrapped clocks. This phase bound therefore remains real. *)
    Eio.Time.with_timeout_exn env#clock 10. (fun () ->
      let rec waiting () =
        match
          ( Daemon.For_testing.job_scheduler_wait_deadline daemon
          , Daemon.For_testing.schedule_scheduler_wait_deadline daemon )
        with
        | Some wall, Some mono
          when t.wall_waiting wall && Mtime.compare mono (t.mono_now ()) > 0 -> ()
        | Some _, Some _ | Some _, None | None, Some _ | None, None ->
          Eio.Fiber.yield ();
          waiting ()
      in
      waiting ();
      Daemon.For_testing.await_schedule_scheduler_idle daemon)
  ;;

  let with_child f =
    let t = create () in
    with_child ~wrap_env:(wrap t) (fun env sw daemon parent child gate providers ->
      Creation_tests.protect_cleanup
        ~finally:(fun () -> t.resume ())
        (fun () -> f t env sw daemon parent child gate providers))
  ;;
end

let restore = function
  | Owner.Scope_outcome.Returned result -> protocol_ok result
  | Raised (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;

let%expect_test
    "delegated admin cleanup waits for Init while explicit close cancels source"
  =
  let with_child = Administrative_schedulers.with_child in
  List.iter [ Public_retirement.Unload; Close ] ~f:(fun retirement ->
    with_child (fun schedulers env sw daemon _parent child gate providers ->
      (* v1 construction prepares/checkpoints a Manager. Its lifecycle event
         runs separately through the installed Owner callback port. Settle the
         child's actual initial activation before selecting a Ready reload. *)
      Owner.drain_idle_moderator child.runtime |> protocol_ok |> ignore;
      let settled () =
        Actor.with_moderator_checkpoint child.actor (fun () -> Actor.state child.actor)
        |> protocol_ok
      in
      let before_resume = settled () in
      let completed state phase =
        List.filter state.A.Session_state.moderator_executions ~f:(fun receipt ->
          P.Moderator_execution.equal_phase receipt.context.phase phase)
        |> List.map ~f:(fun receipt ->
          match receipt.P.Moderator_execution.status with
          | Completed _ -> receipt
          | Running | Failed _ | Interrupted _ ->
            raise_s [%sexp (receipt : P.Moderator_execution.t)])
      in
      [%test_eq: int] 1 (List.length (completed before_resume Session_start));
      (* This fixture owns each subsequent lifecycle drain and its global idle
         assertions. Join independent sweepers after real startup, before the
         first Ready reload; a finite drain joins only its own Owner lease. *)
      Administrative_schedulers.park schedulers env daemon;
      let runtime () =
        Owner.with_loaded_runtime child.runtime (fun runtime -> Ok runtime) |> protocol_ok
      in
      let initial_runtime = runtime () in
      let assert_prepared previous =
        let current = settled () in
        assert (
          List.equal
            P.Moderator_execution.equal
            previous.A.Session_state.moderator_executions
            current.moderator_executions);
        (match current.runtime_initialization with
         | Ready -> ()
         | Pending _ -> failwith "delegated Ready load did not complete its checkpoint");
        assert (Owner.is_loaded child.runtime);
        assert (Owner.source_lane_idle child.runtime);
        assert (not (Owner.has_activation_handoff child.runtime));
        [%test_eq: int] 0 !providers;
        current
      in
      let drain_resume previous =
        (* Source/Init is already joined. This actual installed callback owns
           the v1 Session_resume; physical construction does not run it. *)
        assert (Owner.source_lane_idle child.runtime);
        Owner.drain_idle_moderator child.runtime |> protocol_ok |> ignore;
        let current = settled () in
        [%test_eq: int]
          (List.length (completed previous Session_resume) + 1)
          (List.length (completed current Session_resume));
        assert (Owner.source_lane_idle child.runtime);
        assert (not (Owner.has_activation_handoff child.runtime));
        [%test_eq: int] 0 !providers;
        current
      in
      (* No unresolved Run redirects this authenticated Ready load to the
         installed-callback alternative. The current host original selects
         genuine Init custody for this child, never its saved creator. *)
      assert (
        Option.for_all before_resume.run_state ~f:(fun index ->
          List.for_all (A.Run_state.runs index) ~f:(fun run ->
            match run.P.Run.lifecycle with
            | Terminal _ -> true
            | Admitted | Active | Waiting _ -> false)));
      Owner.unload_and_wait child.runtime |> protocol_ok;
      assert (not (Owner.is_loaded child.runtime));
      let original = Operator_authorization.trusted_local (principal ()) in
      Owner.ensure_loaded_authenticated child.runtime ~actor:original |> protocol_ok;
      let prepared = assert_prepared before_resume in
      let authenticated_runtime = runtime () in
      assert (not (phys_equal initial_runtime authenticated_runtime));
      let authenticated = drain_resume prepared in
      (* The distinct Ready/no-original load prepares another real v1 Manager
         without fabricating Init or running its separately owned callback. *)
      Owner.unload_and_wait child.runtime |> protocol_ok;
      assert (not (Owner.is_loaded child.runtime));
      Owner.ensure_loaded child.runtime |> protocol_ok;
      let prepared = assert_prepared authenticated in
      assert (not (phys_equal authenticated_runtime (runtime ())));
      let resumed = drain_resume prepared in
      [%test_eq: int]
        (List.length (completed before_resume Session_resume) + 2)
        (List.length (completed resumed Session_resume));
      let connection = connection daemon (principal ()) in
      Creation_tests.protect_cleanup
        ~finally:(fun () -> Agent_client.Connection.close connection)
        (fun () ->
           initialize connection;
           let handle =
             H.attach
               ~sw
               ~clock:env#clock
               ~connection
               ~session_id:(Actor.state child.actor |> protocol_ok).identity.session_id
               ~mode:Read_write
               ~subscribe:false
               ()
             |> protocol_ok
           in
           Creation_tests.protect_cleanup
             ~finally:(fun () ->
               Gate.release gate;
               H.close handle)
             (fun () ->
                H.stop handle ~mode:Graceful |> protocol_ok |> ignore;
                Owner.unload_and_wait child.runtime |> protocol_ok;
                let before = Actor.state child.actor |> protocol_ok in
                gate.actor <- Some child.actor;
                gate.protected
                <- (match retirement with
                    | Public_retirement.Unload -> true
                    | Close -> false);
                let rebuilt, rebuilt_u = Eio.Promise.create () in
                Eio.Fiber.fork ~sw (fun () ->
                  Eio.Promise.resolve
                    rebuilt_u
                    (capture_load (fun () ->
                       H.rebuild
                         handle
                         ~expected_revision:before.counters.revision
                         ~prompt_choice:Pinned)));
                Eio.Promise.await gate.entered;
                assert (not (Owner.is_loaded child.runtime));
                let committed = Actor.state child.actor |> protocol_ok in
                [%test_eq: int]
                  (before.identity.generation + 1)
                  committed.identity.generation;
                (match committed.runtime_initialization with
                 | Pending _ -> ()
                 | Ready -> failwith "construction gate did not hold actual pending Init");
                (* Queue the real public retirement after the command's ACK. It
                 must join this same administrative source suffix, not cancel
                 its Init or publish a promise the suffix would itself join. *)
                let entered, enter = Eio.Promise.create () in
                let retired, retired_u = Eio.Promise.create () in
                Eio.Fiber.fork ~sw (fun () ->
                  Eio.Promise.resolve enter ();
                  Eio.Promise.resolve
                    retired_u
                    (capture_load (fun () ->
                       match retirement with
                       | Public_retirement.Unload -> Owner.unload_and_wait child.runtime
                       | Close ->
                         Owner.close_and_wait child.runtime;
                         Ok ())));
                Eio.Promise.await entered;
                let after =
                  match retirement with
                  | Public_retirement.Unload ->
                    Eio.Fiber.yield ();
                    assert (Option.is_none (Eio.Promise.peek retired));
                    assert (Option.is_none (Eio.Promise.peek rebuilt));
                    Gate.release gate;
                    let response = Eio.Promise.await rebuilt |> restore in
                    Eio.Promise.await retired |> restore;
                    let after = Actor.state child.actor |> protocol_ok in
                    (match response.desired_state, after.lifecycle.desired with
                     | Stopped, Stopped -> ()
                     | _ -> failwith "stopped rebuild changed its actual desired state");
                    (match after.runtime_initialization with
                     | Ready -> ()
                     | Pending _ -> failwith "queued cleanup cancelled the Init suffix");
                    after
                  | Close ->
                    (* A cancellable real source wait must be interrupted by
                       explicit close, without requiring the test to release it.
                       The committed command generation remains factual. *)
                    Eio.Promise.await retired |> restore;
                    assert (Option.is_none (Eio.Promise.peek gate.released));
                    Eio.Promise.await rebuilt |> expect_cancelled_load;
                    let after = Actor.state child.actor |> protocol_ok in
                    (match after.runtime_initialization with
                     | Pending _ -> ()
                     | Ready -> failwith "closed source unexpectedly completed Init");
                    (* The actual journal ACK is known, while cancellation left
                       the command's final reply unresolved. Reconciliation reads
                       that retained original without replay or a new command. *)
                    let pending =
                      Agent_client.Connection.pending_commands connection
                      |> List.filter ~f:(fun pending ->
                        String.equal
                          (Agent_client.Connection.pending_identity pending).method_name
                          "session.rebuild")
                      |> function
                      | [ pending ] -> pending
                      | pending ->
                        raise_s
                          [%sexp
                            "expected one unresolved rebuild", (List.length pending : int)]
                    in
                    (match
                       Agent_client.Connection.reconcile connection pending |> protocol_ok
                     with
                     | P.Command_receipt.Pending { accepted_sequence = Some sequence; _ }
                       ->
                       (* Rebuild is the first writer after the stopped,
                          unloaded baseline. Init may reserve history before
                          reaching the construction gate, without changing the
                          command's original accepted transaction. *)
                       [%test_eq: int64]
                         (Int64.succ before.counters.transaction_sequence)
                         sequence
                     | receipt -> raise_s [%sexp (receipt : P.Command_receipt.t)]);
                    assert (
                      List.exists
                        (Agent_client.Connection.pending_commands connection)
                        ~f:(phys_equal pending));
                    [%test_eq: int]
                      committed.identity.generation
                      (Actor.state child.actor |> protocol_ok).identity.generation;
                    after
                in
                [%test_eq: int] (before.identity.generation + 1) after.identity.generation;
                assert (Option.is_none after.active_operation);
                assert (not (Owner.is_loaded child.runtime));
                (match retirement with
                 | Public_retirement.Unload ->
                   assert (Owner.source_lane_idle child.runtime)
                 | Close -> ());
                assert (not (Owner.has_activation_handoff child.runtime));
                [%test_eq: int] 0 !providers;
                (* A new-key mutation must still enter the shared receipt owner
                   after cancellation; cleanup's H.close then observes detached. *)
                H.detach handle |> protocol_ok))));
  print_endline
    "real admin suffix: queued cleanup waits; explicit close cancels and joins";
  [%expect
    {| real admin suffix: queued cleanup waits; explicit close cancels and joins |}]
;;

let%expect_test "source-bound administration joins actual constructor facts before detach"
  =
  let module Marks = A.Canonical_usage_fold.Marks in
  let selected = ref None in
  let builds = ref 0 in
  let cache = ref None in
  let cache_closes = ref 0 in
  let on_inference_ports actor =
    if Option.exists !selected ~f:(phys_equal actor) then Int.incr builds
  in
  let wrap_env env =
    Crash_fault_io.wrap
      env
      ~boundary:After_rename
      ~matches:(fun path ->
        Option.exists !cache ~f:(fun target ->
          String.equal (Filename.dirname path) (Filename.dirname target)
          && String.is_prefix (Filename.basename path) ~prefix:".cache.bin."
          && String.is_suffix path ~suffix:".tmp"))
      ~reached:(fun destination ->
        (* Observe only a completed real cache publication; never inject a
           failure that could be mistaken for successful physical closure. *)
        if Option.exists !cache ~f:(String.equal destination) then Int.incr cache_closes)
  in
  let marks state =
    Marks.of_ledger state.A.Session_state.inference_ledger
    |> Result.map_error ~f:(fun error ->
      Sexp.to_string_hum (A.Canonical_usage_fold.Error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  let equal_state left right =
    let encoded state =
      A.Session_state_document.encode
        (A.Session_state_document.authored state)
        ~limits:Document_schema.Limits.default
      |> Result.map_error ~f:(fun error ->
        Sexp.to_string_hum (Document_schema.Error.sexp_of_t error))
      |> Result.ok_or_failwith
      |> Document_schema.Document.to_string
    in
    String.equal (encoded left) (encoded right)
  in
  with_child
    ~wrap_env
    ~on_inference_ports
    (fun env sw daemon parent entry _gate providers ->
       Owner.drain_idle_moderator entry.runtime |> protocol_ok |> ignore;
       Actor.with_moderator_checkpoint entry.actor (fun () -> Actor.state entry.actor)
       |> protocol_ok
       |> ignore;
       let principal = principal () in
       let original = Operator_authorization.trusted_local principal in
       let connection = connection daemon principal in
       Creation_tests.protect_cleanup
         ~finally:(fun () -> Agent_client.Connection.close connection)
         (fun () ->
            initialize connection;
            let session_id =
              (Actor.state entry.actor |> protocol_ok).identity.session_id
            in
            let handle =
              H.attach
                ~sw
                ~clock:env#clock
                ~connection
                ~session_id
                ~mode:Read_write
                ~subscribe:false
                ()
              |> protocol_ok
            in
            Creation_tests.protect_cleanup
              ~finally:(fun () -> H.close handle)
              (fun () ->
                 H.stop handle ~mode:Graceful |> protocol_ok |> ignore;
                 Owner.unload_and_wait entry.runtime |> protocol_ok;
                 assert (not (Owner.is_loaded entry.runtime));
                 selected := Some entry.actor;
                 Owner.ensure_loaded_authenticated entry.runtime ~actor:original
                 |> protocol_ok;
                 assert (Owner.is_loaded entry.runtime);
                 assert (Owner.source_lane_idle entry.runtime);
                 let loaded = Actor.state entry.actor |> protocol_ok in
                 let reference = Option.value_exn loaded.spec.delegation in
                 let parent_state = Actor.state parent.actor |> protocol_ok in
                 assert (
                   P.Id.Session.equal
                     reference.key.parent_session_id
                     parent_state.identity.session_id);
                 (match
                    parent_state.lifecycle.desired, parent_state.lifecycle.observed
                  with
                  | Running, Idle -> ()
                  | _ -> failwith "genuine parent source is unavailable before admin");
                 (match
                    ( loaded.runtime_initialization
                    , loaded.lifecycle.desired
                    , loaded.lifecycle.observed )
                  with
                  | Ready, Stopped, Stopped -> ()
                  | _ -> failwith "source-bound child did not load while stopped");
                 assert (Option.is_none loaded.active_operation);
                 assert (not (Actor.constructor_model_retirement_pending entry.actor));
                 let attachment = H.attachment handle in
                 let initial_builds = !builds in
                 assert (initial_builds > 0);
                 [%test_eq: int] 0 !providers;
                 let initial_marks = marks loaded in
                 cache
                 := Some
                      (Filename.concat
                         (Agent_store.Session_store.Handle.cache_directory
                            (Option.value_exn entry.store_handle))
                         "cache.bin");
                 let purpose = Actor.Initialization_scope.Purpose.Runtime_construction in
                 let scope =
                   Actor.begin_initialization_authenticated
                     ~purpose
                     entry.actor
                     ~actor:original
                     ~expected:loaded
                   |> protocol_ok
                 in
                 Exn.protect
                   ~finally:(fun () ->
                     Eio.Cancel.protect (fun () ->
                       ignore
                         (Actor.end_initialization entry.actor ~scope
                          : (unit, P.Error.t) result);
                       ignore
                         (Actor.drain_constructor_model_retirement entry.actor
                          : (unit, P.Error.t) result)))
                   ~f:(fun () ->
                     let primary =
                       P.Error.invalid_request "constructor exited before claim"
                     in
                     let queued = ref None in
                     let result =
                       Actor.with_initialization_model_producer
                         entry.actor
                         ~scope
                         ~f:(fun producer ->
                           let job : P.Job.t =
                             { id = P.Id.Job.create ()
                             ; session_id = loaded.identity.session_id
                             ; generation = loaded.identity.generation
                             ; kind = Model_call
                             ; payload =
                                 `Object
                                   [ "recipe", `String "agent_prompt.v1"
                                   ; "payload", `Null
                                   ]
                             ; status = Queued
                             ; retry_policy = Never
                             ; attempt = 0
                             ; created_at = loaded.identity.updated_at
                             ; started_at = None
                             ; next_run_at = None
                             ; completed_at = None
                             ; result = None
                             ; delivery = Not_required
                             ; launch = None
                             ; inference_origin = None
                             ; progress = None
                             }
                           in
                           let admitted =
                             A.Model_job_admission.admit
                               (Actor.initialization_model_producer_admission producer)
                               job
                             |> protocol_ok
                           in
                           queued := Some admitted;
                           Error primary)
                     in
                     (match result with
                      | Error actual -> assert (phys_equal primary actual)
                      | Ok () -> failwith "producer failure was lost");
                     let queued = Option.value_exn !queued in
                     assert (
                       Document_schema.Json.equal
                         (P.Job.status_to_json queued.status)
                         (P.Job.status_to_json Queued));
                     [%test_eq: int] 0 queued.attempt;
                     assert (Option.is_none queued.started_at);
                     assert (Option.is_none queued.completed_at);
                     Actor.end_initialization entry.actor ~scope |> protocol_ok;
                     assert (Actor.constructor_model_retirement_pending entry.actor);
                     let before = Actor.state entry.actor |> protocol_ok in
                     Actor.validate_administration_basis
                       entry.actor
                       ~attachment_id:attachment.id
                       ~expected:before
                     |> protocol_ok;
                     let rebuild state =
                       H.rebuild
                         handle
                         ~expected_revision:state.A.Session_state.counters.revision
                         ~prompt_choice:Pinned
                     in
                     (match rebuild before with
                      | Error error ->
                        assert (P.Error.equal_code error.code Conflict);
                        assert error.retryable
                      | Ok _ ->
                        failwith
                          "administration detached a runtime with pending constructor \
                           facts");
                     let unchanged = Actor.state entry.actor |> protocol_ok in
                     assert (equal_state before unchanged);
                     assert (Owner.is_loaded entry.runtime);
                     [%test_eq: int] initial_builds !builds;
                     [%test_eq: int] 0 !providers;
                     [%test_eq: int] 0 !cache_closes;
                     assert (Marks.equal initial_marks (marks unchanged));
                     assert (Actor.constructor_model_retirement_pending entry.actor);
                     let actual_job state =
                       List.find_exn state.A.Session_state.jobs ~f:(fun job ->
                         P.Id.Job.equal queued.id job.id)
                     in
                     assert (
                       Document_schema.Json.equal
                         (P.Job.to_json queued)
                         (P.Job.to_json (actual_job unchanged)));
                     Actor.drain_constructor_model_retirement entry.actor |> protocol_ok;
                     let drained = Actor.state entry.actor |> protocol_ok in
                     let cancelled = actual_job drained in
                     assert (
                       Document_schema.Json.equal
                         (P.Job.status_to_json cancelled.status)
                         (P.Job.status_to_json Cancelled));
                     [%test_eq: int] 0 cancelled.attempt;
                     assert (Option.is_none cancelled.started_at);
                     assert (Option.is_none cancelled.result);
                     assert (Option.is_some cancelled.completed_at);
                     assert (
                       Document_schema.Json.equal
                         (P.Job.to_json cancelled)
                         (P.Job.to_json
                            { queued with
                              status = Cancelled
                            ; completed_at = cancelled.completed_at
                            }));
                     assert (not (Actor.constructor_model_retirement_pending entry.actor));
                     assert (Marks.equal initial_marks (marks drained));
                     assert (Owner.is_loaded entry.runtime);
                     [%test_eq: int] initial_builds !builds;
                     [%test_eq: int] 0 !providers;
                     [%test_eq: int] 0 !cache_closes;
                     Actor.drain_constructor_model_retirement entry.actor |> protocol_ok;
                     assert (equal_state drained (Actor.state entry.actor |> protocol_ok));
                     ignore (rebuild drained |> protocol_ok : P.Session.t);
                     let rebuilt = Actor.state entry.actor |> protocol_ok in
                     assert (
                       P.Id.Session.equal
                         loaded.identity.session_id
                         rebuilt.identity.session_id);
                     [%test_eq: int]
                       (loaded.identity.generation + 1)
                       rebuilt.identity.generation;
                     (match
                        ( rebuilt.runtime_initialization
                        , rebuilt.lifecycle.desired
                        , rebuilt.lifecycle.observed )
                      with
                      | Ready, Stopped, Stopped -> ()
                      | _ -> failwith "successful administration did not finish stopped");
                     assert (Option.is_none rebuilt.active_operation);
                     assert (not (Owner.is_loaded entry.runtime));
                     [%test_eq: int] (initial_builds + 1) !builds;
                     [%test_eq: int] 0 !providers;
                     (* Rebuild retires the old loaded resource and the new stopped
                        initializer. Both physical cache publications happened once. *)
                     [%test_eq: int] 2 !cache_closes;
                     Owner.close_and_wait entry.runtime;
                     Owner.close_and_wait entry.runtime;
                     [%test_eq: int] 2 !cache_closes))));
  [%test_eq: int] 2 !cache_closes;
  print_endline
    "source-bound admin preserves loaded resources until factual attempt-zero \
     cancellation";
  [%expect
    {| source-bound admin preserves loaded resources until factual attempt-zero cancellation |}]
;;
