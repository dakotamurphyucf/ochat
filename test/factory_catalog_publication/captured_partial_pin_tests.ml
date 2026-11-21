open! Core
open Agent_server_test_support
open Retry_support
module Existing = Delegated_load_tests
module Warm = Delegated_warm_resource_tests
module Native = Native_resource_failure_tests
module Integration = Native_resource_integration_tests

(* Only scheduler sleep continuations are controlled. Persistence and source
   timestamps must sample the same real clocks as generated child creation. *)
module Sleep_gate = struct
  type 'time waiter =
    { deadline : 'time
    ; mutable parked : bool
    }

  type 'time t =
    { equal : 'time -> 'time -> bool
    ; real_sleep_until : 'time -> unit
    ; mutable paused : bool
    ; mutable change : unit Eio.Promise.t * unit Eio.Promise.u
    ; mutable waiters : 'time waiter list
    }

  let create ~equal ~sleep_until =
    { equal
    ; real_sleep_until = sleep_until
    ; paused = false
    ; change = Eio.Promise.create ()
    ; waiters = []
    }
  ;;

  let set_paused t paused =
    if not (Bool.equal t.paused paused)
    then (
      t.paused <- paused;
      let _, resolve = t.change in
      t.change <- Eio.Promise.create ();
      Eio.Promise.resolve resolve ())
  ;;

  let pause t = set_paused t true
  let resume t = set_paused t false

  let parked t ~deadline =
    List.exists t.waiters ~f:(fun waiter ->
      waiter.parked && t.equal waiter.deadline deadline)
  ;;

  let sleep_until t deadline =
    let waiter = { deadline; parked = false } in
    t.waiters <- waiter :: t.waiters;
    Exn.protect
      ~finally:(fun () ->
        t.waiters
        <- List.filter t.waiters ~f:(fun current -> not (phys_equal current waiter)))
      ~f:(fun () ->
        let rec wait () =
          (* Capture before any yield so a concurrent transition cannot be lost. *)
          let change, _ = t.change in
          if t.paused
          then (
            waiter.parked <- true;
            Eio.Promise.await change;
            waiter.parked <- false;
            wait ())
          else (
            waiter.parked <- false;
            let completed =
              Eio.Fiber.first
                (fun () ->
                   t.real_sleep_until deadline;
                   true)
                (fun () ->
                   Eio.Promise.await change;
                   false)
            in
            if completed && (not t.paused) && phys_equal change (fst t.change)
            then ()
            else wait ())
        in
        wait ())
  ;;
end

module Scheduler_waits = struct
  type t =
    { mutable pause : unit -> unit
    ; mutable resume : unit -> unit
    ; mutable wall_parked : deadline:float -> bool
    ; mutable mono_parked : deadline:Mtime.t -> bool
    }

  let create () =
    { pause = ignore
    ; resume = ignore
    ; wall_parked = (fun ~deadline:_ -> false)
    ; mono_parked = (fun ~deadline:_ -> false)
    }
  ;;

  let resume t = t.resume ()

  let wrap t (env : Eio_unix.Stdenv.base) =
    let wall =
      Sleep_gate.create ~equal:Float.equal ~sleep_until:(Eio.Time.sleep_until env#clock)
    in
    let mono =
      Sleep_gate.create
        ~equal:Mtime.equal
        ~sleep_until:(Eio.Time.Mono.sleep_until env#mono_clock)
    in
    let module Wall_clock = struct
      type t = unit
      type time = float

      let now () = Eio.Time.now env#clock
      let sleep_until () deadline = Sleep_gate.sleep_until wall deadline
    end
    in
    let module Mono_clock = struct
      type t = unit
      type time = Mtime.t

      let now () = Eio.Time.Mono.now env#mono_clock
      let sleep_until () deadline = Sleep_gate.sleep_until mono deadline
    end
    in
    t.pause
    <- (fun () ->
         Sleep_gate.pause wall;
         Sleep_gate.pause mono);
    t.resume
    <- (fun () ->
         Sleep_gate.resume mono;
         Sleep_gate.resume wall);
    t.wall_parked <- Sleep_gate.parked wall;
    t.mono_parked <- Sleep_gate.parked mono;
    let clock = Eio.Resource.T ((), Eio.Time.Pi.clock (module Wall_clock)) in
    let mono_clock = Eio.Resource.T ((), Eio.Time.Pi.clock (module Mono_clock)) in
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

  let park t daemon =
    t.pause ();
    let rec waiting () =
      match
        ( Daemon.For_testing.job_scheduler_wait_deadline daemon
        , Daemon.For_testing.schedule_scheduler_wait_deadline daemon )
      with
      | Some wall, Some mono
        when t.wall_parked ~deadline:wall && t.mono_parked ~deadline:mono -> ()
      | Some _, Some _ | Some _, None | None, Some _ | None, None ->
        Eio.Fiber.yield ();
        waiting ()
    in
    waiting ();
    Daemon.For_testing.await_schedule_scheduler_idle daemon
  ;;
end

module Store = Agent_store.Session_store
module Target = A.Recurrence_target
module Artifact = Agent_store.Prompt_artifact_store
module Ledger = Agent_store.Delegation_store
module Held = Actor.Held_run
module Activation = Actor.Recurrence_activation
module Service = Agent_server.Recurrence_run_service

(* The same public reciprocal occurrence sequence as recurrence_child's
   with_cold_occurrence/with_cold_held, narrowed to Artifact and parameterized
   keys. Keep it here with the actual standalone-policy Caller fixture; do not
   link another inline-test library to obtain its private helpers. *)
module Captured = struct
  type t =
    { child : Registry.entry
    ; intent : Intent.t
    ; attempt : Attempt.t
    }

  let recurrence parent id =
    Catalog.find (Option.value_exn (state parent).recurrence_catalog) id
    |> Option.value_exn
  ;;

  let command parent id key mutation =
    let current = state parent in
    let catalog = Option.value current.recurrence_catalog ~default:Catalog.empty in
    let _, source = A.Goal_catalog_source.capture current |> protocol_ok in
    let needs_source = A.Recurrence_command.Mutation.needs_source mutation in
    let request =
      A.Recurrence_command.create
        ~session_id:current.identity.session_id
        ~generation:current.identity.generation
        ~source:(Option.some_if needs_source source)
        ~expected_lifecycle_revision:
          (Option.some_if needs_source P.Session_lifecycle.Revision.zero)
        ~expected_catalog_revision:(Catalog.revision catalog)
        ~expected_control_revision:
          (Option.map (Catalog.find catalog id) ~f:Entry.control_revision)
        ~key:(P.Idempotency_key.of_string key |> protocol_ok)
        ~mutation
      |> protocol_ok
    in
    Actor.recurrence_command parent.actor ~actor:original ~authorize ~request
    |> protocol_ok
    |> ignore
  ;;

  let target env daemon parent ~root_file =
    let current = state parent in
    let definition =
      A.Prompt_definition.create
        ~id:(Option.value_exn current.spec.prompt_definition_id)
        ~config_name:"648-captured-partial-native-pin"
        ~root_file
        ~root_invocation:current.spec.root_invocation
        ~allowed_workspaces:(Option.to_list current.spec.workspace_instance.definition_id)
        ~permission_profile:current.spec.permission_profile
        ~runtime_policy:current.spec.runtime_policy
        ~enabled:true
        ~description:None
      |> Source_preparation_tests.store_ok
    in
    let artifacts =
      Artifact.open_existing
        ~env
        ~root:
          (Agent_store.Data_root.prompt_artifacts_path
             (Store.data_root (Daemon.store daemon)))
      |> Source_preparation_tests.store_ok
    in
    let revision =
      match
        A.Prompt_revision_builder.build
          ~env
          ~artifact_store:artifacts
          ~transaction_id:(P.Id.Transaction.create ())
          ~created_at:(P.Timestamp.now ())
          definition
      with
      | Ok revision -> revision
      | Error diagnostics ->
        raise_s [%sexp (diagnostics : A.Prompt_revision_builder.Diagnostic.t list)]
    in
    let selected =
      A.Runtime_builder.Source.prepare
        revision
        ~script_preparation:Daemon.default_options.script_preparation
        ~invocation:Ui
        ~env
        ~checkpoint:Eio.Fiber.check
      |> protocol_ok
    in
    let observer =
      match A.Runtime_builder.Source.moderator selected with
      | Legacy observer | Extension observer -> observer
      | No_moderator -> failwith "648 captured source has no actual moderator"
    in
    let parent =
      Target.Parent.create
        ~workspace:current.spec.workspace_instance
        ~permission_profile:current.spec.permission_profile
        ~permission_profile_digest:current.spec.permission_profile_digest
        ~runtime_policy:current.spec.runtime_policy
        ~configuration_revision:current.spec.configuration_revision
        ~inference_selection:current.spec.inference_target
        ~capability_pins:[]
      |> protocol_ok
    in
    let target =
      Target.create_fresh
        ~mode:Single_turn
        ~artifact:
          (A.Prompt_revision.artifact revision
           |> Agent_store.Captured_prompt_artifact.create
           |> document_ok)
        ~definition
        ~parent
      |> protocol_ok
    in
    (match Target.view target with
     | Fresh_child { source_kind = Artifact; _ } -> ()
     | Fresh_child _ | Existing_session _ ->
       failwith "648 expected an actual Artifact target");
    target, observer
  ;;

  let create env daemon parent ~(polling : Scheduler_waits.t) ~key ~target ~observer =
    (* The explicit planning Host supplies authority; the real scheduler sweeps
       the frontier. Let it produce Ready, then join/park its Registry reservations
       before entering the separately owned reciprocal child creation. *)
    Scheduler_waits.resume polling;
    let id = P.Id.Recurrence.of_string ("rec_648_" ^ key) |> protocol_ok in
    let period =
      Recurrence_rules.Rule.Period.create_ms 600_000L
      |> Result.map_error ~f:(fun error ->
        Sexp.to_string_hum [%sexp (error : Recurrence_rules.Timing_error.t)])
      |> Result.ok_or_failwith
    in
    let schedule =
      A.Recurrence_definition.Schedule.Interval
        { rule = Recurrence_rules.Rule.Interval.create ~mode:Fixed_rate ~period
        ; first_due = P.Timestamp.to_time_ns (P.Timestamp.now ())
        }
    in
    command
      parent
      id
      (key ^ "-create")
      (Create (id, { schedule; target; policy = A.Recurrence_policy.default }));
    command parent id (key ^ "-enable") (Enable id);
    let nominal =
      Eio.Time.with_timeout_exn env#clock 10. (fun () ->
        let rec await () =
          let entry = recurrence parent id in
          let definition = A.Recurrence_definition.key (Entry.current entry) in
          match
            Frontier.ready (Entry.frontier entry |> protocol_ok)
            |> List.find ~f:(fun nominal ->
              A.Recurrence_identity.Definition.equal
                (A.Recurrence_identity.Occurrence.definition
                   (A.Recurrence_nominal.key nominal))
                definition)
          with
          | Some nominal -> nominal
          | None ->
            Eio.Fiber.yield ();
            await ()
        in
        await ())
    in
    Eio.Time.with_timeout_exn env#clock 10. (fun () ->
      Scheduler_waits.park polling daemon;
      (* A lifecycle reservation that outlives the scheduler's work must
         release before the separate factory create. Join it with a real
         read of this same controller, then release our read admission. *)
      let registry = Daemon.registry daemon in
      Registry.with_loaded_read
        registry
        (state parent).identity.session_id
        (fun current ->
           assert (phys_equal current.actor parent.actor);
           assert (phys_equal current.runtime parent.runtime);
           assert (Option.equal phys_equal current.store_handle parent.store_handle);
           Ok ())
      |> protocol_ok);
    let permit =
      Actor.reserve_recurrence_child
        parent.Registry.actor
        ~actor:original
        ~authorize:(fun current definition ->
          let open Result.Let_syntax in
          let%bind () = authorize current in
          match Target.view (A.Recurrence_definition.target definition) with
          | Fresh_child { definition; _ }
            when definition.enabled && P.Principal.has_scope principal Create_sessions ->
            Ok ()
          | Fresh_child _ | Existing_session _ ->
            Error (P.Error.invalid_request "648 caller cannot create this child"))
        ~occurrence:(A.Recurrence_nominal.key nominal)
        ~expected_control_revision:(Entry.control_revision (recurrence parent id))
      |> protocol_ok
    in
    let reservation = Actor.Recurrence_creation.reservation permit in
    let linked =
      Factory.create_recurrence_child (Daemon.factory daemon) ~parent ~permit
      |> protocol_ok
    in
    let session = Factory.Recurrence_child.session linked in
    let child_id = P.Session_ref.session_id session in
    let child = Registry.find (Daemon.registry daemon) child_id |> Option.value_exn in
    let reference = Factory.Recurrence_child.reference linked in
    let record =
      Ledger.resolve (Store.delegations (Daemon.store daemon)) reference
      |> Source_preparation_tests.store_ok
    in
    assert (Ledger.equal_stage record.stage Linked);
    (match record.admission.lifetime with
     | Owned -> ()
     | Invocation_owned _ | Independent _ ->
       failwith "648 captured child is not actual Owned");
    let origin = A.Recurrence_child_origin.of_record record |> protocol_ok in
    (match A.Recurrence_child_origin.source_kind origin with
     | Artifact -> ()
     | Authored_tool _ -> failwith "648 captured child selected authored-tool source");
    let current = state child in
    assert (not (Owner.is_loaded child.runtime));
    (match current.runtime_initialization with
     | Pending { fresh_history = true } -> ()
     | Pending _ | Ready -> failwith "648 captured child was not genuinely cold");
    let entry = recurrence parent id in
    let association =
      Entry.frontier entry
      |> protocol_ok
      |> Frontier.child_associations
      |> List.find_exn ~f:(fun association ->
        P.Id.Session.equal
          (A.Recurrence_child_association.reference association).child_session_id
          child_id)
    in
    assert (
      Ledger.Reference.equal
        (A.Recurrence_child_association.reference association)
        reference);
    let child_data = Intent.Child.create ~reservation ~association |> protocol_ok in
    let source =
      P.Run_source.create
        ~observer
        ~generation:current.identity.generation
        ~installation_epoch:1L
      |> protocol_ok
    in
    let intent =
      Intent.create
        ~definition:(Entry.current entry)
        ~control_revision:(Entry.control_revision entry)
        ~nominal_json:(A.Recurrence_child_reservation.nominal_json reservation)
        ~target:session
        ~source
        ~child:(Some child_data)
      |> protocol_ok
    in
    (match Intent.input intent with
     | Authored_start -> ()
     | User_submission _ -> failwith "648 captured occurrence changed input family");
    let attempt =
      Attempt.create
        ~intent
        ~number:0
        ~principal_id:principal.id
        ~expected_revision:current.counters.revision
        ~key:(P.Idempotency_key.of_string (key ^ "-initial-run") |> protocol_ok)
      |> protocol_ok
    in
    { child; intent; attempt }
  ;;

  let with_held daemon (parent : Registry.entry) t f =
    let preparation =
      match
        Actor.begin_headless_run_preparation
          t.child.actor
          ~actor:original
          ~authorize
          ~request:(Attempt.request t.attempt ~intent:t.intent |> protocol_ok)
        |> protocol_ok
      with
      | Prepare preparation -> preparation
      | Retained _ -> failwith "648 failed occurrence reused an actual Run"
    in
    protect_cleanup
      ~finally:(fun () ->
        Actor.end_run_preparation t.child.actor preparation |> protocol_ok)
      (fun () ->
         Factory.with_prepared_run_source
           (Daemon.factory daemon)
           t.child
           ~preparation
           (fun prepared ->
              let source =
                Actor.Source_binding.source (Owner.Prepared.binding prepared)
              in
              (match A.Runtime_builder.Source.selection source with
               | Captured retained ->
                 (match
                    A.Recurrence_child_origin.source_kind
                      (A.Captured_recurrence_source.origin retained)
                  with
                  | Artifact -> ()
                  | Authored_tool _ ->
                    failwith "648 selected an outer-parent authored source")
               | Authored _ | Authored_child _ | Generated _ | Generated_artifact _ ->
                 failwith "648 lost actual captured source");
              Actor.record_recurrence_execution
                parent.actor
                ~actor:original
                ~authorize:(fun current _ -> authorize current)
                ~intent:t.intent
                ~attempt:t.attempt
              |> protocol_ok
              |> ignore;
              let held =
                match
                  Actor.admit_held_recurrence_run
                    t.child.actor
                    ~binding:(Owner.Prepared.binding prepared)
                    ~intent:t.intent
                    ~attempt:t.attempt
                    ~session:(Intent.target t.intent)
                  |> protocol_ok
                with
                | Held held -> held
                | Retained _ -> failwith "648 failed occurrence lacked fresh Held owner"
                | Rejected error | Uncertain error -> raise_s [%sexp (error : P.Error.t)]
              in
              let observe () =
                Actor.observe_recurrence_run
                  t.child.actor
                  ~actor:original
                  ~authorize
                  ~intent:t.intent
                  ~attempt:t.attempt
                |> protocol_ok
                |> Option.value_exn
              in
              Actor.import_recurrence_execution
                parent.actor
                ~actor:original
                ~authorize:(fun current _ -> authorize current)
                ~automatic_policy:(fun _ _ -> Ok true)
                ~observation:(observe ())
              |> protocol_ok
              |> ignore;
              let observation = observe () in
              let permit =
                Actor.acquire_recurrence_activation
                  parent.actor
                  ~actor:original
                  ~authorize:(fun current _ -> authorize current)
                  ~observation
                |> protocol_ok
              in
              protect_cleanup
                ~finally:(fun () ->
                  Actor.release_recurrence_activation parent.actor permit |> protocol_ok)
                (fun () ->
                   let checked =
                     Actor.check_recurrence_activation parent.actor permit ~observation
                     |> protocol_ok
                   in
                   Owner.Prepared.check_basis prepared |> protocol_ok;
                   Held.check_basis held |> protocol_ok;
                   f prepared held checked))
         |> protocol_ok)
  ;;
end

let%expect_test
    "captured Artifact partial repairs its actual last native pin before a new occurrence"
  =
  let fault = Native.Cache_failure.create () in
  let polling = Scheduler_waits.create () in
  let child_cache = ref None in
  let armed = ref false in
  let reads = ref 0 in
  let entered, enter = Eio.Promise.create () in
  let released, release = Eio.Promise.create () in
  let release_build () =
    if Option.is_none (Eio.Promise.peek released) then Eio.Promise.resolve release ()
  in
  let build_failure = Failure "648 captured Artifact before runtime return" in
  let build_trace = ref None in
  let pin_failure = Failure "648 captured preparing last native pin cache failure" in
  let pin_trace = ref None in
  let fail_pin _path =
    try raise pin_failure with
    | exn ->
      pin_trace := Some (Stdlib.Printexc.get_raw_backtrace ());
      Exn.raise_with_original_backtrace exn (Option.value_exn !pin_trace)
  in
  let hold_build _path =
    if !armed
    then (
      armed := false;
      Int.incr reads;
      Eio.Promise.resolve enter ();
      Eio.Cancel.protect (fun () -> Eio.Promise.await released);
      try raise build_failure with
      | exn ->
        build_trace := Some (Stdlib.Printexc.get_raw_backtrace ());
        Exn.raise_with_original_backtrace exn (Option.value_exn !build_trace))
  in
  let recording = Stdlib.Printexc.backtrace_status () in
  Stdlib.Printexc.record_backtrace true;
  Exn.protect
    ~finally:(fun () -> Stdlib.Printexc.record_backtrace recording)
    ~f:(fun () ->
      Existing.with_child
        ~configure:(fun _root (config : Agent_server.Config.t) ->
          (* The fixture drives reservation and child creation separately. Keep
             the elapsed-time snapshot trigger from starting a retention lease
             between those calls. Event-count snapshots still work normally. *)
          let server = config.server in
          let durability = { server.durability with snapshot_every_ms = Int.max_value } in
          { config with server = { server with durability } })
        ~wrap_env:(fun env ->
          Crash_fault_io.wrap
            (Native.Cache_failure.wrap
               ~fail:fail_pin
               fault
               (Scheduler_waits.wrap polling env))
            ~boundary:Before_stat
            ~matches:(fun path -> Option.exists !child_cache ~f:(String.equal path))
            ~reached:hold_build)
        (fun env sw daemon parent original_child _gate providers ->
           protect_cleanup
             ~finally:(fun () -> Scheduler_waits.resume polling)
             (fun () ->
                let within phase f =
                  try Eio.Time.with_timeout_exn env#clock 15. f with
                  | Eio.Time.Timeout ->
                    failwith ("648 captured preparing last pin timed out: " ^ phase)
                in
                let root =
                  (state parent).spec.workspace_instance.canonical_root.native_path
                in
                let child_file = Filename.concat root "captured-partial-pin.chatmd" in
                Eio.Path.save
                  ~create:(`Exclusive 0o600)
                  Eio.Path.(env#fs / child_file)
                  {|<developer>648 genuine captured Artifact constructor.</developer>
<script id="captured" language="chatml" kind="moderator">
let initial_state = 0
let on_event ctx state event = Task.pure(state)
</script>|};
                let target, observer =
                  Captured.target env daemon parent ~root_file:child_file
                in
                Owner.drain_idle_moderator original_child.runtime |> protocol_ok |> ignore;
                fault.cache
                <- Some
                     (Filename.concat
                        (Store.Handle.cache_directory
                           (Option.value_exn parent.store_handle))
                        "cache.bin");
                within "unload original child Product reference" (fun () ->
                  Owner.unload_and_wait original_child.runtime |> protocol_ok);
                assert (Owner.is_loaded parent.runtime);
                with_planner env sw daemon parent (fun () ->
                  let first =
                    Captured.create
                      env
                      daemon
                      parent
                      ~polling
                      ~key:"partial-first"
                      ~target
                      ~observer
                  in
                  within "unload parent after genuine child reservation" (fun () ->
                    Owner.unload_and_wait parent.runtime |> protocol_ok);
                  [%test_eq: int] 1 fault.attempts;
                  [%test_eq: int] 1 fault.publications;
                  assert (not (Owner.is_loaded parent.runtime));
                  child_cache
                  := Some
                       (Filename.concat
                          (Store.Handle.cache_directory
                             (Option.value_exn first.child.store_handle))
                          "cache.bin");
                  assert (
                    not
                      (Eio.Path.is_file Eio.Path.(env#fs / Option.value_exn !child_cache)));
                  let attempted = ref None in
                  let escaped_prepared = ref None in
                  let escaped_held = ref None in
                  let first_admission = ref None in
                  let callback_calls = ref 0 in
                  let next_child = ref None in
                  protect_cleanup
                    ~finally:(fun () ->
                      armed := false;
                      fault.armed <- false;
                      release_build ();
                      Eio.Cancel.protect (fun () ->
                        within "join and close genuine retained children" (fun () ->
                          Option.iter !attempted ~f:(fun task ->
                            Eio.Promise.await_exn task |> ignore);
                          Option.iter !next_child ~f:(fun child ->
                            Owner.unload_and_wait child.Registry.runtime |> protocol_ok);
                          Owner.unload_and_wait first.child.runtime |> protocol_ok;
                          Owner.unload_and_wait parent.runtime |> protocol_ok)))
                    (fun () ->
                       let before = Warm.settled parent in
                       armed := true;
                       let attempted_promise =
                         Eio.Fiber.fork_promise ~sw (fun () ->
                           Existing.capture_load (fun () ->
                             Captured.with_held
                               daemon
                               parent
                               first
                               (fun prepared held checked ->
                                  escaped_prepared := Some prepared;
                                  escaped_held := Some held;
                                  first_admission := Some (Held.admission held);
                                  Owner.with_held_recurrence_runtime
                                    first.child.runtime
                                    ~prepared
                                    ~held
                                    ~checked
                                    (fun _runtime _continuation ->
                                       Int.incr callback_calls;
                                       Ok ()));
                             Ok ()))
                       in
                       attempted := Some attempted_promise;
                       (match
                          within "exact cold child cache or early result" (fun () ->
                            Eio.Fiber.first
                              (fun () ->
                                 Eio.Promise.await entered;
                                 `Entered)
                              (fun () ->
                                 `Returned (Eio.Promise.await_exn attempted_promise)))
                        with
                        | `Entered -> ()
                        | `Returned result ->
                          Existing.restore result;
                          failwith "648 captured constructor missed cache gate");
                       [%test_eq: int] 1 !reads;
                       [%test_eq: int] 0 !callback_calls;
                       assert (Option.is_none (Eio.Promise.peek attempted_promise));
                       assert (not (Owner.is_loaded first.child.runtime));
                       assert (not (Owner.is_loaded parent.runtime));
                       Integration.no_lifecycle_replay before (Warm.settled parent);
                       let phase = ref "648 actual captured pin baseline adoption" in
                       within "adopt same native baseline" (fun () ->
                         Cold_policy_resource_tests.run_standalone_policy
                           ~phase
                           env
                           daemon
                           parent
                           (Warm.settled parent)
                           ~original
                         |> ignore);
                       let after_policy = Warm.settled parent in
                       within
                         "release restored parent baseline while sole inner pin survives"
                         (fun () -> Owner.unload_and_wait parent.runtime |> protocol_ok);
                       [%test_eq: int] 1 fault.attempts;
                       [%test_eq: int] 1 fault.publications;
                       [%test_eq: int] 0 fault.failures;
                       assert (Option.is_none (Eio.Promise.peek attempted_promise));
                       fault.armed <- true;
                       release_build ();
                       (match
                          within "actual captured constructor failure" (fun () ->
                            Eio.Promise.await_exn attempted_promise)
                        with
                        | Owner.Scope_outcome.Raised (actual, trace) ->
                          assert (phys_equal actual build_failure);
                          let expected =
                            Stdlib.Printexc.raw_backtrace_to_string
                              (Option.value_exn !build_trace)
                          in
                          assert (not (String.is_empty expected));
                          assert (
                            String.is_prefix
                              (Stdlib.Printexc.raw_backtrace_to_string trace)
                              ~prefix:expected)
                        | Returned (Error error) ->
                          raise_s
                            [%sexp
                              "648 captured constructor changed F", (error : P.Error.t)]
                        | Returned (Ok ()) ->
                          failwith "648 captured failed constructor returned a runtime");
                       [%test_eq: int] 1 fault.failures;
                       [%test_eq: int] 2 fault.attempts;
                       [%test_eq: int] 1 fault.publications;
                       assert (
                         not
                           (String.is_empty
                              (Stdlib.Printexc.raw_backtrace_to_string
                                 (Option.value_exn !pin_trace))));
                       assert (
                         Result.is_error
                           (Owner.Prepared.check_basis
                              (Option.value_exn !escaped_prepared)));
                       assert (
                         Result.is_error
                           (Held.check_basis (Option.value_exn !escaped_held)));
                       [%test_eq: int] 0 !callback_calls;
                       [%test_eq: int] 0 !providers;
                       assert (not (Owner.is_loaded first.child.runtime));
                       let admission = Option.value_exn !first_admission in
                       let failed_state = state first.child in
                       let index = Option.value_exn failed_state.run_state in
                       let failed_run =
                         A.Run_state.find index admission.run_id |> Option.value_exn
                       in
                       (match failed_run.lifecycle with
                        | Terminal Interrupted -> ()
                        | _ ->
                          raise_s
                            [%sexp
                              "648 failed held constructor did not interrupt its own Run"
                            , (failed_run : P.Run.t)]);
                       within "repair SAME child-owned preparing pin" (fun () ->
                         Owner.unload_and_wait first.child.runtime |> protocol_ok);
                       [%test_eq: int] 1 fault.failures;
                       [%test_eq: int] 3 fault.attempts;
                       [%test_eq: int] 2 fault.publications;
                       within "repeat real cleanup without replay" (fun () ->
                         Owner.unload_and_wait first.child.runtime |> protocol_ok;
                         Owner.unload_and_wait parent.runtime |> protocol_ok);
                       [%test_eq: int] 3 fault.attempts;
                       [%test_eq: int] 2 fault.publications;
                       Integration.no_lifecycle_replay after_policy (Warm.settled parent);
                       let still_failed =
                         A.Run_state.find
                           (Option.value_exn (state first.child).run_state)
                           admission.run_id
                         |> Option.value_exn
                       in
                       assert (P.Run.equal failed_run still_failed);
                       (* Child creation checks the actual loaded parent capability
                     source. Restore it only after the exact retained-pin repair
                     and no-replay assertions, using normal authenticated admission. *)
                       within "restore actual parent for fresh child creation" (fun () ->
                         Owner.ensure_loaded_authenticated parent.runtime ~actor:original
                         |> protocol_ok);
                       assert (Owner.is_loaded parent.runtime);
                       (* An admitted failed Held cannot supply No_effects or be reused.
                     A genuinely new definition/occurrence/session supplies the next Run. *)
                       let second =
                         Captured.create
                           env
                           daemon
                           parent
                           ~polling
                           ~key:"partial-after-repair"
                           ~target
                           ~observer
                       in
                       next_child := Some second.child;
                       assert (
                         not
                           (P.Id.Session.equal
                              (state first.child).identity.session_id
                              (state second.child).identity.session_id));
                       let activated =
                         Service.activate
                           (Recurrence_service_tests.service daemon)
                           ~controller:parent
                           ~controller_actor:original
                           ~target:second.child
                           ~target_actor:original
                           ~intent:second.intent
                           ~attempt:second.attempt
                         |> protocol_ok
                         |> Recurrence_service_tests.activated
                       in
                       assert (not (P.Id.Run.equal activated.run_id admission.run_id));
                       let current = state second.child in
                       let selected =
                         A.Run_state.find
                           (Option.value_exn current.run_state)
                           activated.run_id
                         |> Option.value_exn
                       in
                       assert (P.Run_source.equal selected.source activated.source);
                       let origin =
                         A.Run_state.recurrence_origin
                           (Option.value_exn current.run_state)
                           selected.id
                         |> Option.value_exn
                       in
                       A.Recurrence_run_origin.validate
                         origin
                         ~intent:second.intent
                         ~attempt:second.attempt
                       |> protocol_ok;
                       A.Recurrence_run_origin.validate_run
                         origin
                         ~run:selected
                         ~admission:activated
                       |> protocol_ok;
                       [%test_eq: int] 1 !reads;
                       [%test_eq: int] 0 !providers;
                       within "join fresh real captured occurrence" (fun () ->
                         Owner.unload_and_wait second.child.runtime |> protocol_ok))))));
  print_endline
    "captured Artifact failed before runtime; its own last pin repaired once; new \
     occurrence activated";
  [%expect
    {| captured Artifact failed before runtime; its own last pin repaired once; new occurrence activated |}]
;;
