open! Core
open Agent_server_test_support
open Retry_support
module Host = Agent_server.Recurrence_execution_host
module Configuration = Agent_server.Recurrence_execution_configuration
module Driver = Agent_server.Recurrence_dispatcher

module Read_fault = struct
  type action =
    | Read
    | Missing

  let wrap_directory (Eio.Resource.T (directory, handler)) ~before_read =
    let module Original = (val Eio.Resource.get handler Eio.Fs.Pi.Dir) in
    let module Directory = struct
      include Original

      let open_in directory ~sw path =
        let path =
          match before_read path with
          | Read -> path
          | Missing -> path ^ ".485-unavailable"
        in
        Original.open_in directory ~sw path
      ;;
    end
    in
    let bindings =
      Eio.Resource.bindings handler
      |> List.filter ~f:(function
        | H (Eio.Fs.Pi.Dir, _) -> false
        | _ -> true)
    in
    Eio.Resource.T
      (directory, Eio.Resource.handler (H (Eio.Fs.Pi.Dir, (module Directory)) :: bindings))
  ;;

  let wrap env ~before_read =
    let directory, path = Eio.Stdenv.fs env in
    let fs = wrap_directory directory ~before_read, path in
    object
      method fs = fs
      method cwd = env#cwd
      method stdin = env#stdin
      method stdout = env#stdout
      method stderr = env#stderr
      method net = env#net
      method domain_mgr = env#domain_mgr
      method process_mgr = env#process_mgr
      method clock = env#clock
      method mono_clock = env#mono_clock
      method secure_random = env#secure_random
      method debug = env#debug
      method backend_id = env#backend_id
    end
  ;;
end

let manifest_path daemon revision =
  Filename.concat
    (Agent_store.Data_root.prompt_artifacts_path
       (Agent_store.Session_store.data_root (Daemon.store daemon)))
    (Filename.concat
       (P.Id.Prompt_revision.to_string (A.Prompt_revision.id revision))
       "manifest.sexp")
;;

let with_driver ?(other_targets = []) daemon sw controllers target f =
  let session_id entry = (state entry).identity.session_id in
  let grant controller =
    Configuration.Controller.create
      ~session_id:(session_id controller)
      ~existing_targets:(List.map (target :: other_targets) ~f:session_id)
      ~captured_children:false
    |> protocol_ok
  in
  let configuration =
    Configuration.create
      ~principal_id:(P.Id.Principal.of_string "pri_485_race_executor" |> protocol_ok)
      ~controllers:(List.map controllers ~f:grant)
      ~expires_at:None
    |> protocol_ok
  in
  let host =
    Host.create
      configuration
      ~factory:(Daemon.factory daemon)
      ~server_id:(Agent_store.Session_store.server_id (Daemon.store daemon))
      ~sw
      ~now:P.Timestamp.now
    |> protocol_ok
  in
  let driver =
    Driver.create
      ~factory:(Daemon.factory daemon)
      ~registry:(Daemon.registry daemon)
      ~host
  in
  protect_cleanup
    ~finally:(fun () ->
      Driver.close driver;
      Host.close host)
    (fun () -> f driver)
;;

let occurrence daemon env sw controller target =
  with_planner env sw daemon controller (fun () ->
    create_occurrence daemon controller target)
;;

let await env f =
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
    let rec loop () =
      match f () with
      | Some value -> value
      | None ->
        Eio.Fiber.yield ();
        loop ()
    in
    loop ())
;;

let await_terminal env driver controller occurrence =
  await env (fun () ->
    (* Reconciliation reads target lifecycle evidence across yielding IO. A
       concurrent factual completion may invalidate that read; the scheduler
       retries on its next pass rather than treating Conflict as completion. *)
    (match Driver.process_entry driver controller with
     | Ok () | Error { code = Conflict; _ } -> ()
     | Error error -> raise_s [%sexp (error : P.Error.t)]);
    let actual = execution controller occurrence.intent in
    let attempt = Execution.current_attempt actual in
    match Execution.Attempt.terminal attempt with
    | None -> None
    | Some terminal ->
      assert (not (Execution.is_pending actual));
      Some (actual, terminal))
;;

let%expect_test "target exclusion begins before the real source preview yields" =
  let before_read = ref (fun _ -> Read_fault.Read) in
  let entered, signal_entered = Eio.Promise.create () in
  let release, signal_release = Eio.Promise.create () in
  let provider, signal_provider = Eio.Promise.create () in
  let release_provider, signal_release_provider = Eio.Promise.create () in
  let release_once promise resolver =
    if Option.is_none (Eio.Promise.peek promise) then Eio.Promise.resolve resolver ()
  in
  with_fixture
    ~max_root_agents:3
    ~wrap_env:(fun env ->
      Read_fault.wrap env ~before_read:(fun path -> !before_read path))
    ~post_stream:(fun _ ->
      Eio.Promise.resolve signal_provider ();
      Eio.Promise.await release_provider;
      completed_response "485 serialized source preview")
    (fun env sw daemon controller _runtime revision counters ->
       Source.with_loaded_entry daemon (fun ~entry:other ~runtime:_ ~revision:_ ->
         Source.with_loaded_entry daemon (fun ~entry:target ~runtime:_ ~revision:_ ->
           let one = occurrence daemon env sw controller target in
           occurrence daemon env sw other target |> ignore;
           let manifest = manifest_path daemon revision in
           let armed = ref true in
           (before_read
            := fun path ->
                 if !armed && String.equal path manifest
                 then (
                   armed := false;
                   Eio.Promise.resolve signal_entered ();
                   Eio.Promise.await release);
                 Read_fault.Read);
           with_driver daemon sw [ controller; other ] target (fun driver ->
             protect_cleanup
               ~finally:(fun () ->
                 release_once release signal_release;
                 release_once release_provider signal_release_provider)
               (fun () ->
                  let finished, signal_finished = Eio.Promise.create () in
                  Eio.Fiber.fork ~sw (fun () ->
                    let result = Driver.process_entry driver controller in
                    Eio.Promise.resolve signal_finished result);
                  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
                    Eio.Promise.await entered);
                  let target_before = state target in
                  let other_before = state other in
                  let target_frames = Existing.frames env target in
                  let other_frames = Existing.frames env other in
                  let counts = !(counters.inference_ports), !(counters.providers) in
                  let denied = Driver.process_entry driver other |> rejected in
                  [%test_eq: P.Error.code] Conflict denied.code;
                  assert (not denied.retryable);
                  Existing.assert_same other_before other;
                  Existing.assert_same target_before target;
                  [%test_eq: string list] other_frames (Existing.frames env other);
                  [%test_eq: string list] target_frames (Existing.frames env target);
                  [%test_eq: int * int]
                    counts
                    (!(counters.inference_ports), !(counters.providers));
                  assert (List.is_empty (Frontier.executions (frontier other)));
                  assert (List.is_empty (Frontier.executions (frontier controller)));
                  release_once release signal_release;
                  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
                    Eio.Promise.await finished |> protocol_ok;
                    Eio.Promise.await provider);
                  let actual = execution controller one.intent in
                  let attempt = Execution.current_attempt actual in
                  let origin = Execution.Attempt.origin attempt |> Option.value_exn in
                  assert (
                    P.Session_ref.equal
                      (A.Recurrence_run_origin.session origin)
                      one.session);
                  [%test_eq: int] 1 !(counters.providers);
                  release_once release_provider signal_release_provider;
                  let actual, terminal = await_terminal env driver controller one in
                  assert (
                    A.Recurrence_run_reference.Outcome.equal
                      (A.Recurrence_run_reference.outcome terminal)
                      Completed);
                  [%test_eq: int] 1 (List.length (Execution.attempts actual));
                  [%test_eq: int] 1 !(counters.providers))))));
  print_endline
    "actual source-read yield retains target exclusion; competing controller has no \
     attempt, frame, constructor or provider; original occurrence settles once";
  [%expect
    {| actual source-read yield retains target exclusion; competing controller has no attempt, frame, constructor or provider; original occurrence settles once |}]
;;

type initial_follow_up =
  | Reenter
  | Attachment_changes_revision

let initial_source_failure follow_up =
  let before_read = ref (fun _ -> Read_fault.Read) in
  with_fixture
    ~max_root_agents:2
    ~wrap_env:(fun env ->
      Read_fault.wrap env ~before_read:(fun path -> !before_read path))
    (fun env sw daemon controller _runtime revision counters ->
       Source.with_loaded_entry daemon (fun ~entry:target ~runtime:_ ~revision:_ ->
         let one = occurrence daemon env sw controller target in
         let manifest = manifest_path daemon revision in
         let reads = ref 0 in
         (before_read
          := fun path ->
               if String.equal path manifest
               then (
                 Int.incr reads;
                 if Int.equal !reads 2 then Missing else Read)
               else Read);
         with_driver daemon sw [ controller ] target (fun driver ->
           let before = state controller in
           let target_before = state target in
           let frames = Existing.frames env controller in
           let target_frames = Existing.frames env target in
           let ports = !(counters.inference_ports) in
           let failure = Driver.process_entry driver controller |> rejected in
           [%test_eq: int] 2 !reads;
           [%test_eq: P.Error.code] Prompt_unavailable failure.code;
           Existing.assert_same before controller;
           Existing.assert_same target_before target;
           [%test_eq: string list] frames (Existing.frames env controller);
           [%test_eq: string list] target_frames (Existing.frames env target);
           [%test_eq: int] ports !(counters.inference_ports);
           [%test_eq: int] 0 !(counters.providers);
           assert (Owner.source_lane_idle target.runtime);
           assert (List.is_empty (Frontier.executions (frontier controller)));
           match follow_up with
           | Reenter ->
             Driver.process_entry driver controller |> protocol_ok;
             let actual, terminal = await_terminal env driver controller one in
             [%test_eq: int] 1 (List.length (Execution.attempts actual));
             let attempt =
               Execution.current_attempt actual |> Execution.Attempt.request
             in
             [%test_eq: int] 0 (Attempt.number attempt);
             [%test_eq: int64]
               target_before.counters.revision
               (Attempt.expected_revision attempt);
             assert (
               P.Id.Principal.equal
                 (Attempt.principal_id attempt)
                 (P.Id.Principal.of_string "pri_485_race_executor" |> protocol_ok));
             assert (
               A.Recurrence_run_reference.Outcome.equal
                 (A.Recurrence_run_reference.outcome terminal)
                 Completed);
             [%test_eq: int] 1 !(counters.providers)
           | Attachment_changes_revision ->
             let attachment, _ =
               Actor.attach target.actor ~mode:Read_only ~subscribe:false |> protocol_ok
             in
             let changed = state target in
             [%test_eq: int64]
               (Int64.succ target_before.counters.revision)
               changed.counters.revision;
             assert (
               List.exists changed.attachments ~f:(fun current ->
                 P.Id.Attachment.equal current.id attachment.id));
             let changed_frames = Existing.frames env target in
             let denied = Driver.process_entry driver controller |> rejected in
             [%test_eq: P.Error.code] Conflict denied.code;
             assert (not denied.retryable);
             Existing.assert_same changed target;
             Existing.assert_same before controller;
             [%test_eq: string list] changed_frames (Existing.frames env target);
             [%test_eq: string list] frames (Existing.frames env controller);
             assert (List.is_empty (Frontier.executions (frontier controller)));
             [%test_eq: int] ports !(counters.inference_ports);
             [%test_eq: int] 0 !(counters.providers))))
;;

let%expect_test "joined initial source failure re-enters ordinal zero before recording" =
  initial_source_failure Reenter;
  print_endline
    "real second source read fails before recording; actual joined absence re-enters \
     ordinal zero with original CAS and principal, then imports one terminal Run";
  [%expect
    {| real second source read fails before recording; actual joined absence re-enters ordinal zero with original CAS and principal, then imports one terminal Run |}]
;;

let%expect_test "pre-record re-entry never refreshes its frozen target CAS" =
  initial_source_failure Attachment_changes_revision;
  print_endline
    "actual read-only attachment advances target after joined source failure; retry \
     keeps frozen CAS and records no replacement attempt or effect";
  [%expect
    {| actual read-only attachment advances target after joined source failure; retry keeps frozen CAS and records no replacement attempt or effect |}]
;;

let%expect_test "missing initial source completion never creates a replacement request" =
  let before_read = ref (fun _ -> Read_fault.Read) in
  with_fixture
    ~max_root_agents:2
    ~wrap_env:(fun env ->
      Read_fault.wrap env ~before_read:(fun path -> !before_read path))
    (fun env sw daemon controller _runtime revision counters ->
       Source.with_loaded_entry daemon (fun ~entry:target ~runtime:_ ~revision:_ ->
         let _one = occurrence daemon env sw controller target in
         let manifest = manifest_path daemon revision in
         let reads = ref 0 in
         let attached = ref None in
         let detach () =
           match !attached with
           | None | Some (Error _) -> ()
           | Some (Ok (attachment, _)) ->
             Actor.detach target.actor attachment.P.Session.Attachment.id |> protocol_ok
         in
         protect_cleanup ~finally:detach (fun () ->
           (before_read
            := fun path ->
                 if String.equal path manifest
                 then (
                   Int.incr reads;
                   if Int.equal !reads 2
                   then (
                     (* The first read is the joined source preview. The second
                        belongs to the actual deferred activation. This real
                        mailbox write invalidates that exact preparation before
                        the failed source attempt can issue completion evidence.
                        Record its Result; assert outside the source adapter. *)
                     attached
                     := Some (Actor.attach target.actor ~mode:Read_only ~subscribe:false);
                     Missing)
                   else Read)
                 else Read);
           with_driver daemon sw [ controller ] target (fun driver ->
             let controller_before = state controller in
             let target_before = state target in
             let controller_frames = Existing.frames env controller in
             let ports = !(counters.inference_ports) in
             let first = Driver.process_entry driver controller |> rejected in
             [%test_eq: P.Error.code] Prompt_unavailable first.code;
             [%test_eq: int] 2 !reads;
             let attachment, _ = Option.value_exn !attached |> protocol_ok in
             let changed = state target in
             [%test_eq: int64]
               (Int64.succ target_before.counters.revision)
               changed.counters.revision;
             assert (
               List.exists changed.attachments ~f:(fun current ->
                 P.Id.Attachment.equal current.id attachment.id));
             assert (Option.is_none changed.active_operation);
             assert (
               List.equal
                 Jsonaf.exactly_equal
                 (List.map target_before.jobs ~f:P.Job.to_json)
                 (List.map changed.jobs ~f:P.Job.to_json));
             assert (
               List.equal P.Invocation.equal target_before.invocations changed.invocations);
             assert (
               List.equal
                 P.Moderator_execution.equal
                 target_before.moderator_executions
                 changed.moderator_executions);
             Existing.assert_same controller_before controller;
             [%test_eq: string list] controller_frames (Existing.frames env controller);
             assert (List.is_empty (Frontier.executions (frontier controller)));
             assert (Owner.source_lane_idle target.runtime);
             [%test_eq: int] ports !(counters.inference_ports);
             [%test_eq: int] 0 !(counters.providers);
             let target_frames = Existing.frames env target in
             let second = Driver.process_entry driver controller |> rejected in
             [%test_eq: P.Error.code] Conflict second.code;
             assert (not second.retryable);
             (* No new read means the failed attempt was retained as deny-only
                DATA, rather than replaced by another source/key/CAS request. *)
             [%test_eq: int] 2 !reads;
             Existing.assert_same changed target;
             Existing.assert_same controller_before controller;
             [%test_eq: string list] target_frames (Existing.frames env target);
             [%test_eq: string list] controller_frames (Existing.frames env controller);
             assert (List.is_empty (Frontier.executions (frontier controller)));
             [%test_eq: int] ports !(counters.inference_ports);
             [%test_eq: int] 0 !(counters.providers)))));
  print_endline
    "actual attachment during failed source preparation prevents completion; next pass \
     performs no read, replacement request, controller record or effect";
  [%expect
    {| actual attachment during failed source preparation prevents completion; next pass performs no read, replacement request, controller record or effect |}]
;;

let%expect_test "bounded cursor reaches an independent target past 32 busy candidates" =
  with_fixture
    ~max_root_agents:3
    (fun env sw daemon controller _runtime _revision counters ->
       Source.with_loaded_entry
         daemon
         (fun ~entry:busy ~runtime:busy_runtime ~revision:_ ->
            Source.with_loaded_entry
              daemon
              (fun ~entry:independent ~runtime:_ ~revision:_ ->
                 let busy_id =
                   P.Id.Recurrence.of_string "rec_485_a_busy" |> protocol_ok
                 in
                 let independent_id =
                   P.Id.Recurrence.of_string "rec_485_z_independent" |> protocol_ok
                 in
                 let frontier id =
                   Catalog.find
                     (Option.value_exn (state controller).recurrence_catalog)
                     id
                   |> Option.value_exn
                   |> Entry.frontier
                   |> protocol_ok
                 in
                 let keys id =
                   Frontier.ready (frontier id) |> List.map ~f:A.Recurrence_nominal.key
                 in
                 let command key mutation =
                   let current = state controller in
                   let catalog =
                     Option.value current.recurrence_catalog ~default:Catalog.empty
                   in
                   let _, source = A.Goal_catalog_source.capture current |> protocol_ok in
                   let request =
                     A.Recurrence_command.create
                       ~session_id:current.identity.session_id
                       ~generation:current.identity.generation
                       ~source:(Some source)
                       ~expected_lifecycle_revision:
                         (Some P.Session_lifecycle.Revision.zero)
                       ~expected_catalog_revision:(Catalog.revision catalog)
                       ~expected_control_revision:
                         (Option.map
                            (Catalog.find
                               catalog
                               (A.Recurrence_command.Mutation.id mutation))
                            ~f:Entry.control_revision)
                       ~key:(P.Idempotency_key.of_string key |> protocol_ok)
                       ~mutation
                     |> protocol_ok
                   in
                   Actor.recurrence_command
                     controller.actor
                     ~actor:original
                     ~authorize
                     ~request
                   |> protocol_ok
                   |> ignore
                 in
                 let create id target ~policy ~first_due =
                   let current = state target in
                   let session =
                     P.Session_ref.create
                       ~server_id:
                         (Agent_store.Session_store.server_id (Daemon.store daemon))
                       ~session_id:current.identity.session_id
                   in
                   let target =
                     A.Recurrence_target.create_existing
                       ~session
                       ~source:(A.Recurrence_source.installed current |> protocol_ok)
                       ~configuration_revision:current.spec.configuration_revision
                       ~input:
                         { kind = Plain_text
                         ; text = "485 independent cursor work"
                         ; attachments = []
                         }
                     |> protocol_ok
                   in
                   let period =
                     Recurrence_rules.Rule.Period.create_ms 1000L
                     |> Result.map_error ~f:(fun error ->
                       Sexp.to_string_hum (Recurrence_rules.Timing_error.sexp_of_t error))
                     |> Result.ok_or_failwith
                   in
                   let schedule =
                     A.Recurrence_definition.Schedule.Interval
                       { rule =
                           Recurrence_rules.Rule.Interval.create ~mode:Fixed_rate ~period
                       ; first_due
                       }
                   in
                   let name = P.Id.Recurrence.to_string id in
                   command (name ^ "-create") (Create (id, { schedule; target; policy }));
                   command (name ^ "-enable") (Enable id)
                 in
                 with_planner env sw daemon controller (fun () ->
                   let policy =
                     A.Recurrence_policy.create
                       ~misfire:(Catch_up 32)
                       ~grace_ms:600_000L
                       ~max_pending:32
                       ~max_history:64
                       ~max_calendar_steps:256
                     |> protocol_ok
                   in
                   let now = P.Timestamp.now () |> P.Timestamp.to_time_ns in
                   create
                     busy_id
                     busy
                     ~policy
                     ~first_due:(Time_ns.sub now (Time_ns.Span.of_int_sec 60));
                   create
                     independent_id
                     independent
                     ~policy:A.Recurrence_policy.default
                     ~first_due:now;
                   (* The real scheduler shares this planning owner. Wait for
                      its bounded result instead of racing a second manual sweep. *)
                   await env (fun () ->
                     if
                       List.length (keys busy_id) = 32
                       && List.length (keys independent_id) = 1
                     then Some ()
                     else None));
                 let busy_keys = keys busy_id in
                 let independent_key =
                   match keys independent_id with
                   | [ key ] -> key
                   | _ -> failwith "expected exactly one independent Ready occurrence"
                 in
                 [%test_eq: int] 32 (List.length busy_keys);
                 let ordered =
                   List.dedup_and_sort
                     (independent_key :: busy_keys)
                     ~compare:A.Recurrence_identity.Occurrence.compare
                 in
                 [%test_eq: int] 33 (List.length ordered);
                 assert (
                   List.equal
                     A.Recurrence_identity.Occurrence.equal
                     (List.take ordered 32)
                     busy_keys);
                 assert (
                   A.Recurrence_identity.Occurrence.equal
                     (List.last_exn ordered)
                     independent_key);
                 with_driver
                   ~other_targets:[ independent ]
                   daemon
                   sw
                   [ controller ]
                   busy
                   (fun driver ->
                      let borrowed = ref None in
                      let entered, signal_entered = Eio.Promise.create () in
                      let release, signal_release = Eio.Promise.create () in
                      let lease =
                        Eio.Fiber.fork_promise ~sw (fun () ->
                          Owner.with_background_runtime busy.runtime (fun actual ->
                            borrowed := Some actual;
                            Eio.Promise.resolve signal_entered ();
                            Eio.Promise.await release;
                            Ok ()))
                      in
                      let cleanup () =
                        if Option.is_none (Eio.Promise.peek release)
                        then Eio.Promise.resolve signal_release ();
                        Eio.Promise.await_exn lease |> protocol_ok
                      in
                      protect_cleanup ~finally:cleanup (fun () ->
                        Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
                          Eio.Promise.await entered;
                          (* Outside the adapter: this is the actual Factory resource. *)
                          assert (phys_equal (Option.value_exn !borrowed) busy_runtime);
                          let before = state controller in
                          let busy_before = state busy in
                          let independent_before = state independent in
                          let frames = Existing.frames env controller in
                          let busy_frames = Existing.frames env busy in
                          let independent_frames = Existing.frames env independent in
                          let ports = !(counters.inference_ports) in
                          let first =
                            Driver.process_entry driver controller |> rejected
                          in
                          [%test_eq: P.Error.code] Conflict first.code;
                          assert (not first.retryable);
                          Existing.assert_same before controller;
                          Existing.assert_same busy_before busy;
                          Existing.assert_same independent_before independent;
                          [%test_eq: string list] frames (Existing.frames env controller);
                          [%test_eq: string list] busy_frames (Existing.frames env busy);
                          [%test_eq: string list]
                            independent_frames
                            (Existing.frames env independent);
                          [%test_eq: int] ports !(counters.inference_ports);
                          [%test_eq: int] 0 !(counters.providers);
                          assert (Option.is_none (Eio.Promise.peek lease));
                          assert (
                            List.is_empty (Frontier.executions (frontier independent_id)));
                          (* Same driver/cursor; the busy resource stays genuinely borrowed. *)
                          Driver.process_entry driver controller |> protocol_ok;
                          let actual =
                            await env (fun () ->
                              (match Driver.process_entry driver controller with
                               | Ok () | Error { code = Conflict; _ } -> ()
                               | Error error -> raise_s [%sexp (error : P.Error.t)]);
                              match Frontier.executions (frontier independent_id) with
                              | [ execution ] ->
                                let attempt = Execution.current_attempt execution in
                                (match Execution.Attempt.terminal attempt with
                                 | Some terminal -> Some (execution, attempt, terminal)
                                 | None -> None)
                              | [] -> None
                              | _ ->
                                failwith
                                  "independent occurrence was admitted more than once")
                          in
                          let execution, attempt, terminal = actual in
                          assert (not (Execution.is_pending execution));
                          assert (
                            A.Recurrence_identity.Occurrence.equal
                              (Intent.occurrence (Execution.intent execution))
                              independent_key);
                          let origin =
                            Execution.Attempt.origin attempt |> Option.value_exn
                          in
                          let admission = A.Recurrence_run_origin.admission origin in
                          let current = state independent in
                          let run =
                            A.Run_state.find
                              (Option.value_exn current.run_state)
                              admission.run_id
                            |> Option.value_exn
                          in
                          A.Recurrence_run_origin.validate_run origin ~run ~admission
                          |> protocol_ok;
                          A.Recurrence_run_reference.validate
                            terminal
                            ~origin
                            ~run
                            ~receipt:(A.Recurrence_run_reference.receipt terminal)
                          |> protocol_ok;
                          assert (
                            A.Recurrence_run_reference.Outcome.equal
                              (A.Recurrence_run_reference.outcome terminal)
                              Completed);
                          assert (
                            P.Id.Principal.equal
                              run.principal_id
                              (P.Id.Principal.of_string "pri_485_race_executor"
                               |> protocol_ok));
                          [%test_eq: int] 1 (List.length (Execution.attempts execution));
                          [%test_eq: int] 1 !(counters.providers);
                          assert (Option.is_none (Eio.Promise.peek lease));
                          Existing.assert_same busy_before busy;
                          [%test_eq: string list] busy_frames (Existing.frames env busy);
                          assert (
                            List.equal
                              A.Recurrence_identity.Occurrence.equal
                              busy_keys
                              (keys busy_id));
                          assert (List.is_empty (Frontier.executions (frontier busy_id)))))))));
  print_endline
    "real planner retains 32 earlier busy candidates; first pass performs no writes or \
     effects; same cursor reaches and settles independent candidate 33 while busy lease \
     remains live";
  [%expect
    {| real planner retains 32 earlier busy candidates; first pass performs no writes or effects; same cursor reaches and settles independent candidate 33 while busy lease remains live |}]
;;

let%expect_test "daemon shutdown joins its blocked recurrence source before closing Store"
  =
  let before_read = ref (fun _ -> Read_fault.Read) in
  let wrap env = Read_fault.wrap env ~before_read:(fun path -> !before_read path) in
  with_recoverable_fixture
    ~wrap_env:wrap
    ~max_root_agents:2
    (fun env sw root old_daemon policy old_controller _runtime revision counters ->
       Source.with_loaded_entry
         old_daemon
         (fun ~entry:old_target ~runtime:_ ~revision:_ ->
            let _one = occurrence old_daemon env sw old_controller old_target in
            let controller_before = state old_controller in
            let controller_id = controller_before.identity.session_id in
            let target_id = (state old_target).identity.session_id in
            let manifest = manifest_path old_daemon revision in
            let planning =
              Agent_server.Recurrence_automation_configuration.create
                ~principal_id:
                  (P.Id.Principal.of_string "pri_485_shutdown_planner" |> protocol_ok)
                ~controllers:[ controller_id ]
                ~expires_at:None
                ~zone_directory:None
              |> protocol_ok
            in
            let grant =
              Configuration.Controller.create
                ~session_id:controller_id
                ~existing_targets:[ target_id ]
                ~captured_children:false
              |> protocol_ok
            in
            let execution =
              Configuration.create
                ~principal_id:
                  (P.Id.Principal.of_string "pri_485_shutdown_executor" |> protocol_ok)
                ~controllers:[ grant ]
                ~expires_at:None
              |> protocol_ok
            in
            let config =
              Creation_tests.with_max_root_agents
                (config root root (Filename.concat root "parent.chatmd"))
                ~max_root_agents:2
            in
            let config =
              { config with
                server =
                  { config.server with
                    recurrence_automation = Some planning
                  ; recurrence_execution = Some execution
                  }
              }
            in
            Daemon.shutdown old_daemon |> protocol_ok;
            let start startup_mode =
              Daemon.start
                ~sw
                ~env:(wrap (env :> Eio_unix.Stdenv.base))
                ~config
                ~tool_dir:root
                ~home:root
                ~process_start_identity:None
                ~options:
                  { Daemon.default_options with
                    startup_mode
                  ; inference_policy = policy
                  ; qualify_chatml_extensions = true
                  }
                ()
              |> protocol_ok
            in
            let daemon = start On_demand in
            let entered, signal_entered = Eio.Promise.create () in
            let left, signal_left = Eio.Promise.create () in
            let release, signal_release = Eio.Promise.create () in
            let captured_target = ref None in
            let cancelled = ref false in
            let closing = ref None in
            let cleanup () =
              (before_read := fun _ -> Read_fault.Read);
              if Option.is_none (Eio.Promise.peek release)
              then Eio.Promise.resolve signal_release ();
              match !closing with
              | None -> Daemon.shutdown daemon |> protocol_ok
              | Some promise -> Eio.Promise.await_exn promise |> protocol_ok
            in
            protect_cleanup ~finally:cleanup (fun () ->
              let registry = Daemon.registry daemon in
              [%test_eq: int] 0 (Registry.stats registry).loaded;
              (before_read
               := fun path ->
                    if String.equal path manifest && Option.is_none !captured_target
                    then (
                      match Registry.find registry target_id with
                      | Some target
                        when (not (Registry.execution_selected registry target))
                             && not (Owner.source_lane_idle target.runtime) ->
                        (* Actual metadata recovery is published; preview had no source
                   lane. This is the sole scheduler's real deferred source stage. *)
                        captured_target := Some target;
                        Eio.Promise.resolve signal_entered ();
                        Exn.protect
                          ~finally:(fun () -> Eio.Promise.resolve signal_left ())
                          ~f:(fun () ->
                            match Eio.Promise.await release with
                            | () -> ()
                            | exception exn ->
                              let backtrace = Stdlib.Printexc.get_raw_backtrace () in
                              (match exn with
                               | Eio.Cancel.Cancelled _ -> cancelled := true
                               | _ -> ());
                              Exn.raise_with_original_backtrace exn backtrace)
                      | None | Some _ -> ());
                    Read_fault.Read);
              Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
                let controller =
                  Recurrence_target_acquisition_tests.select_controller
                    env
                    daemon
                    controller_id
                    controller_before
                in
                Eio.Promise.await entered;
                let target = Option.value_exn !captured_target in
                let host = Option.value_exn (Daemon.recurrence_execution_host daemon) in
                let configured = Host.controller host controller |> protocol_ok in
                let original = Host.Controller.actor configured in
                assert (Operator_authorization.is_current original);
                assert (not (Registry.execution_selected registry target));
                assert (not (Owner.is_loaded target.runtime));
                assert (not (Owner.has_activation_handoff target.runtime));
                let controller_state = state controller in
                let target_state = state target in
                let controller_transactions =
                  Retry_tests.transaction_payloads env controller
                in
                let target_transactions = Retry_tests.transaction_payloads env target in
                let counts = !(counters.inference_ports), !(counters.providers) in
                [%test_eq: int] 0 !(counters.providers);
                let frontier =
                  Catalog.find
                    (Option.value_exn controller_state.recurrence_catalog)
                    recurrence_id
                  |> Option.value_exn
                  |> Entry.frontier
                  |> protocol_ok
                in
                [%test_eq: int] 1 (List.length (Frontier.ready frontier));
                assert (List.is_empty (Frontier.executions frontier));
                assert (Option.is_none target_state.active_operation);
                assert (
                  Option.value_map target_state.run_state ~default:true ~f:(fun index ->
                    List.is_empty (A.Run_state.runs index)));
                assert (
                  List.is_empty target_state.jobs
                  && List.is_empty target_state.invocations);
                let shutdown =
                  Eio.Fiber.fork_promise ~sw (fun () -> Daemon.shutdown daemon)
                in
                closing := Some shutdown;
                Eio.Promise.await_exn shutdown |> protocol_ok;
                (* Normal path never opens release: only actual scheduler cancellation
               can unwind the blocked source; shutdown has joined all its cleanup. *)
                assert (Option.is_none (Eio.Promise.peek release));
                assert !cancelled;
                assert (Option.is_some (Eio.Promise.peek left));
                assert (Agent_store.Session_store.is_closed (Daemon.store daemon));
                assert (not (Operator_authorization.is_current original));
                assert (not (Owner.is_loaded target.runtime));
                assert (not (Owner.has_activation_handoff target.runtime));
                let callbacks = ref 0 in
                let denied =
                  Owner.with_background_runtime target.runtime (fun _ ->
                    Int.incr callbacks;
                    Ok ())
                  |> rejected
                in
                [%test_eq: P.Error.code] Server_shutting_down denied.code;
                [%test_eq: int] 0 !callbacks;
                [%test_eq: int * int]
                  counts
                  (!(counters.inference_ports), !(counters.providers));
                (* Shutdown may prune journal prefixes already captured by a
                   snapshot. Every remaining transaction must be an exact suffix;
                   the physical reopen below also checks the complete canonical
                   state, including its transaction/revision counters. *)
                let check_retained_transactions entry before =
                  assert (
                    List.is_suffix
                      before
                      ~suffix:(Retry_tests.transaction_payloads env entry)
                      ~equal:String.equal)
                in
                check_retained_transactions controller controller_transactions;
                check_retained_transactions target target_transactions;
                (* The same physical Store is reopened only for immutable facts. No
               old envelope, source lease, authority or pending grant is restored. *)
                let reader = start Operator_only in
                protect_cleanup
                  ~finally:(fun () -> Daemon.shutdown reader |> protocol_ok)
                  (fun () ->
                     assert (Option.is_none (Daemon.recurrence_execution_host reader));
                     [%test_eq: int] 0 (Registry.stats (Daemon.registry reader)).loaded;
                     let read id =
                       let session =
                         P.Session_ref.create
                           ~server_id:
                             (Agent_store.Session_store.server_id (Daemon.store reader))
                           ~session_id:id
                       in
                       Factory.read_recurrence_state
                         (Daemon.factory reader)
                         ~session
                         ~actor:Retry_support.original
                         ~authorize:(fun actual ->
                           if P.Id.Session.equal actual.P.Session.id id
                           then Ok ()
                           else
                             Error
                               (P.Error.invalid_request
                                  "shutdown reader returned wrong session"))
                       |> protocol_ok
                     in
                     let recovered_controller = read controller_id in
                     let recovered_target = read target_id in
                     assert (
                       Jsonaf.exactly_equal
                         (state_document controller_state
                          |> Existing.store_ok
                          |> Document_schema.Document.json)
                         (state_document recovered_controller
                          |> Existing.store_ok
                          |> Document_schema.Document.json));
                     assert (
                       Jsonaf.exactly_equal
                         (state_document target_state
                          |> Existing.store_ok
                          |> Document_schema.Document.json)
                         (state_document recovered_target
                          |> Existing.store_ok
                          |> Document_schema.Document.json));
                     [%test_eq: int] 0 (Registry.stats (Daemon.registry reader)).loaded;
                     [%test_eq: int * int]
                       counts
                       (!(counters.inference_ports), !(counters.providers)))))));
  print_endline
    "actual sole scheduler source is cancelled and joined by Daemon shutdown; no \
     execution or handoff; Operator_only reopen preserves Ready and source facts without \
     replay";
  [%expect
    {| actual sole scheduler source is cancelled and joined by Daemon shutdown; no execution or handoff; Operator_only reopen preserves Ready and source facts without replay |}]
;;
