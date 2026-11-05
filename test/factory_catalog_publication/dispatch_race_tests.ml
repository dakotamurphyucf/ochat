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

let with_driver daemon sw controllers target f =
  let session_id entry = (state entry).identity.session_id in
  let grant controller =
    Configuration.Controller.create
      ~session_id:(session_id controller)
      ~existing_targets:[ session_id target ]
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
