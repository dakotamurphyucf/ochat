open! Core
open Agent_server_test_support
open Retry_support
module Result_attempt = Factory.Deferred_run_attempt

let with_distinct_target ?wrap_env ?post_stream f =
  with_recoverable_fixture
    ?wrap_env
    ?post_stream
    ~max_root_agents:2
    (fun env sw root daemon policy controller _ _ counters ->
       Source.with_loaded_entry daemon (fun ~entry:target ~runtime ~revision:_ ->
         assert (not (phys_equal controller.Registry.actor target.Registry.actor));
         assert (
           not
             (P.Id.Session.equal
                (state controller).identity.session_id
                (state target).identity.session_id));
         with_planner env sw daemon controller (fun () ->
           let occurrence = create_occurrence daemon controller target in
           let observation = initial_absence ~controller env daemon target occurrence in
           f
             env
             sw
             root
             daemon
             policy
             controller
             target
             runtime
             counters
             occurrence
             observation)))
;;

let assert_rotated occurrence next before row =
  assert (Intent.equal occurrence.intent (Execution.intent row));
  let resolution =
    match Execution.attempts row with
    | [ previous; current ] ->
      assert (Attempt.equal occurrence.previous (Execution.Attempt.request previous));
      assert (Option.is_none (Execution.Attempt.origin previous));
      assert (Option.is_none (Execution.Attempt.terminal previous));
      assert (Attempt.equal next (Execution.Attempt.request current));
      assert (Option.is_none (Execution.Attempt.retry_resolution current));
      assert (Option.is_none (Execution.Attempt.origin current));
      assert (Option.is_none (Execution.Attempt.terminal current));
      Option.value_exn (Execution.Attempt.retry_resolution previous)
    | _ -> failwith "controller rotation did not retain exactly two attempts"
  in
  let expected =
    Execution.resolve_retry before resolution
    |> protocol_ok
    |> fun resolved -> Execution.append_attempt resolved next |> protocol_ok
  in
  (* Compare the complete old prefix, including retained audit and future fields. *)
  assert (Execution.equal expected row)
;;

let assert_target_inert env target before frames counters ports =
  Existing.assert_same before target;
  [%test_eq: string list] frames (Existing.frames env target);
  [%test_eq: int] ports !(counters.inference_ports);
  [%test_eq: int] 0 !(counters.providers)
;;

let deny_pending target prepared occurrence next =
  let error =
    Actor.admit_held_recurrence_run
      target.Registry.actor
      ~binding:(Owner.Prepared.binding prepared)
      ~intent:occurrence.intent
      ~attempt:next
      ~session:occurrence.session
    |> rejected
  in
  [%test_eq: P.Error.code] Conflict error.code
;;

let begin_retry daemon target occurrence observation f =
  Factory.with_recurrence_retry_run_source
    (Daemon.factory daemon)
    target
    ~observation
    ~actor:original
    ~authorize
    ~intent:occurrence.intent
    ~previous:occurrence.previous
    ~request:(next_request target occurrence)
    f
  |> protocol_ok
;;

let%expect_test "distinct controller rotation ACK precedes actual target activation" =
  let entered, entered_u = Eio.Promise.create () in
  let release, release_u = Eio.Promise.create () in
  let release_provider () =
    if Option.is_none (Eio.Promise.peek release) then Eio.Promise.resolve release_u ()
  in
  with_distinct_target
    ~post_stream:(fun _ ->
      Eio.Promise.resolve entered_u ();
      Eio.Promise.await release;
      completed_response "539 distinct target complete")
    (fun env
      _sw
      _root
      daemon
      _policy
      controller
      target
      runtime
      counters
      occurrence
      observation ->
       protect_cleanup ~finally:release_provider (fun () ->
         let before = state target in
         let target_frames = Existing.frames env target in
         let ports = !(counters.inference_ports) in
         let controller_before = state controller in
         let controller_frames = Existing.frames env controller in
         let original_row = execution controller occurrence.intent in
         let selected = ref None in
         let attempted =
           begin_retry daemon target occurrence observation (fun retry prepared ->
             let next = Actor.Recurrence_retry.next_attempt retry in
             selected := Some next;
             [%test_eq: int] 1 (Attempt.number next);
             [%test_eq: int64] before.counters.revision (Attempt.expected_revision next);
             deny_pending target prepared occurrence next;
             assert_target_inert env target before target_frames counters ports;
             let row =
               Actor.rotate_prepared_recurrence_execution
                 controller.actor
                 ~retry
                 ~next:(Owner.Prepared.binding prepared)
                 ~actor:original
                 ~authorize:authorize_execution
               |> protocol_ok
             in
             assert_rotated occurrence next original_row row;
             [%test_eq: int64]
               (Int64.succ controller_before.counters.revision)
               (state controller).counters.revision;
             [%test_eq: int]
               (List.length controller_frames + 1)
               (List.length (Existing.frames env controller));
             assert_target_inert env target before target_frames counters ports;
             Owner.Prepared.check_basis prepared |> protocol_ok;
             let held =
               match
                 Actor.admit_held_recurrence_run
                   target.actor
                   ~binding:(Owner.Prepared.binding prepared)
                   ~intent:occurrence.intent
                   ~attempt:next
                   ~session:occurrence.session
                 |> protocol_ok
               with
               | Held held -> held
               | Retained _ | Rejected _ | Uncertain _ ->
                 failwith "remote acknowledged retry did not issue actual Held"
             in
             let returned =
               consume
                 daemon
                 controller
                 target
                 ~runtime
                 prepared
                 held
                 occurrence.intent
                 next
               |> protocol_ok
             in
             assert (P.Run_receipt.equal returned (Actor.Held_run.admission held));
             Ok returned)
         in
         let admission = Result_attempt.result attempted |> protocol_ok in
         assert (Option.is_none (Result_attempt.no_effects attempted));
         assert (Option.is_none (Result_attempt.retry attempted));
         let rec await_provider () =
           match Eio.Promise.peek entered with
           | Some () -> ()
           | None ->
             let index = Option.value_exn (state target).run_state in
             let run = A.Run_state.find index admission.run_id |> Option.value_exn in
             (match run.lifecycle with
              | Terminal _ ->
                raise_s [%sexp "target ended before provider", (run : P.Run.t)]
              | Admitted | Active | Waiting _ -> ());
             Eio.Fiber.yield ();
             await_provider ()
         in
         await_provider ();
         (* Both controller and target source CPS have exited, not just the writer. *)
         [%test_eq: int] 1 !(counters.providers);
         [%test_eq: int] (ports + 1) !(counters.inference_ports);
         assert (
           String.is_substring
             (List.hd_exn !(counters.transcripts))
             ~substring:"539 actual retry input");
         release_provider ();
         let rec await_terminal () =
           let current = state target in
           let index = Option.value_exn current.run_state in
           let run = A.Run_state.find index admission.run_id |> Option.value_exn in
           match run.lifecycle with
           | Terminal (Completed _) -> current, index, run
           | Terminal _ -> raise_s [%sexp "remote retry failed", (run : P.Run.t)]
           | Admitted | Active | Waiting _ ->
             Eio.Fiber.yield ();
             await_terminal ()
         in
         let final, index, run = await_terminal () in
         assert (Option.is_none final.active_operation);
         let next = Option.value_exn !selected in
         let origin = A.Run_state.recurrence_origin index run.id |> Option.value_exn in
         A.Recurrence_run_origin.validate origin ~intent:occurrence.intent ~attempt:next
         |> protocol_ok;
         A.Recurrence_run_origin.validate_run origin ~run ~admission |> protocol_ok;
         List.iter
           P.Run_receipt.Kind.[ Admission; Terminal ]
           ~f:(fun kind ->
             [%test_eq: int]
               1
               (List.count (A.Run_state.receipts index) ~f:(fun saved ->
                  P.Id.Run.equal saved.run_id run.id
                  && P.Run_receipt.Kind.equal saved.kind kind)));
         let observation =
           Actor.observe_recurrence_run
             target.actor
             ~actor:original
             ~authorize
             ~intent:occurrence.intent
             ~attempt:next
           |> protocol_ok
           |> Option.value_exn
         in
         Actor.import_recurrence_execution
           controller.actor
           ~actor:original
           ~authorize:authorize_execution
           ~automatic_policy:(fun _ _ -> Ok true)
           ~observation
         |> protocol_ok
         |> ignore;
         let retained = execution controller occurrence.intent in
         let current = Execution.current_attempt retained in
         assert (Attempt.equal next (Execution.Attempt.request current));
         assert (Option.is_some (Execution.Attempt.origin current));
         assert (Option.is_some (Execution.Attempt.terminal current));
         [%test_eq: int] 1 !(counters.providers);
         print_endline "distinct controller ACK writes once before any target admission";
         print_endline
           "same next Run completes after source exit and imports real terminal evidence"));
  [%expect
    {|
    distinct controller ACK writes once before any target admission
    same next Run completes after source exit and imports real terminal evidence
    |}]
;;

let rec atoms = function
  | A.Session_delta.Batch changes -> List.concat_map changes ~f:atoms
  | delta -> [ delta ]
;;

module Fault = struct
  type selected =
    { controller : P.Id.Session.t
    ; occurrence : occurrence
    ; next : Attempt.t
    ; original : Execution.t
    }

  type t =
    { mutable selected : selected option
    ; mutable candidate : Execution.t option
    ; mutable hits : int
    }

  let create () = { selected = None; candidate = None; hits = 0 }

  let reached t env path =
    match t.selected with
    | None -> ()
    | Some selected ->
      let scan =
        Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path)
        |> Agent_store.Journal_segment.scan_contents ~max_payload_length:(64 * 1024 * 1024)
        |> Existing.store_ok
      in
      (match List.last scan.entries with
       | Some frame when Int.equal (Agent_store.Frame.flags frame.frame) 0 ->
         let transaction =
           Agent_store.Transaction.decode (Agent_store.Frame.payload frame.frame)
           |> Existing.store_ok
         in
         if P.Id.Session.equal transaction.session_id selected.controller
         then (
           let delta =
             A.Session_delta_document.decode ~limits:D.Limits.default transaction.delta
             |> document_ok
             |> A.Session_delta_document.value
           in
           List.iter (atoms delta) ~f:(function
             | Recurrence_catalog_changed catalog ->
               let entry = Catalog.find catalog recurrence_id |> Option.value_exn in
               let frontier = Entry.frontier entry |> protocol_ok in
               let row =
                 Frontier.find_execution
                   frontier
                   (Intent.occurrence selected.occurrence.intent)
                 |> Option.value_exn
               in
               if
                 Attempt.equal
                   selected.next
                   (Execution.Attempt.request (Execution.current_attempt row))
               then (
                 assert_rotated selected.occurrence selected.next selected.original row;
                 t.selected <- None;
                 t.candidate <- Some row;
                 t.hits <- t.hits + 1;
                 raise (Core_unix.Unix_error (EIO, "539 rotation before sync", path)))
             | _ -> ()))
       | Some _ | None -> ())
  ;;
end

exception Observed_rotation_failure of P.Error.t

let await_recovered entry (previous : A.Session_state.t) =
  let startup =
    List.find_exn previous.moderator_executions ~f:(fun receipt ->
      match receipt.P.Moderator_execution.context.phase, receipt.status with
      | Session_start, Completed _ -> true
      | _ -> false)
  in
  let new_resume (receipt : P.Moderator_execution.t) =
    P.Moderator_execution.equal_phase receipt.context.phase Session_resume
    && P.Invocation.equal_observer receipt.context.source startup.context.source
    && not
         (List.exists previous.moderator_executions ~f:(fun old ->
            P.Id.Moderator_execution.equal old.context.id receipt.context.id))
  in
  let rec await () =
    let current =
      Actor.with_moderator_checkpoint entry.Registry.actor (fun () ->
        Actor.state entry.actor)
      |> protocol_ok
    in
    List.iter current.moderator_executions ~f:(fun receipt ->
      if new_resume receipt
      then (
        match receipt.status with
        | Failed _ | Interrupted _ ->
          raise_s
            [%sexp "recovered retry runtime failed", (receipt : P.Moderator_execution.t)]
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
      await ())
  in
  await ()
;;

let reconcile_reopened
      env
      sw
      root
      daemon
      policy
      controller
      target
      counters
      occurrence
      observation
      retry
      prepared
      candidate
  =
  let old_controller = state controller in
  let old_target = state target in
  let old_store = Daemon.store daemon in
  (* There is no acknowledged target Run to interrupt. Close actual owners and
     their poisoned writer; an unexpected shutdown error remains a test failure. *)
  Daemon.shutdown daemon |> protocol_ok;
  assert (Agent_store.Session_store.is_closed old_store);
  let reopened =
    Daemon.start
      ~sw
      ~env:(env :> Eio_unix.Stdenv.base)
      ~config:
        (Creation_tests.with_max_root_agents
           (config root root (Filename.concat root "parent.chatmd"))
           ~max_root_agents:2)
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
  protect_cleanup
    ~finally:(fun () -> Daemon.shutdown reopened |> protocol_ok)
    (fun () ->
       assert (not (phys_equal old_store (Daemon.store reopened)));
       let recovered_controller =
         Registry.load (Daemon.registry reopened) old_controller.identity.session_id
         |> protocol_ok
       in
       let recovered_target =
         Registry.load (Daemon.registry reopened) old_target.identity.session_id
         |> protocol_ok
       in
       assert (not (phys_equal controller.actor recovered_controller.actor));
       assert (not (phys_equal target.actor recovered_target.actor));
       let controller_before = await_recovered recovered_controller old_controller in
       let target_before = await_recovered recovered_target old_target in
       (* Recovery, rather than the failed writer's return or physical candidate
       alone, now establishes which complete original-next row survived. *)
       let recovered_row = execution recovered_controller occurrence.intent in
       assert (Execution.equal candidate recovered_row);
       let frames = Existing.frames env recovered_controller in
       let target_frames = Existing.frames env recovered_target in
       let counts = !(counters.inference_ports), !(counters.providers) in
       let reconciled =
         Actor.record_recurrence_execution
           recovered_controller.actor
           ~actor:original
           ~authorize:authorize_execution
           ~intent:occurrence.intent
           ~attempt:occurrence.previous
         |> protocol_ok
       in
       assert (Execution.equal candidate reconciled);
       Existing.assert_same controller_before recovered_controller;
       [%test_eq: string list] frames (Existing.frames env recovered_controller);
       assert (Result.is_error (Actor.Recurrence_retry.check_basis retry));
       assert (Result.is_error (Owner.Prepared.check_basis prepared));
       assert (Result.is_error (Actor.No_effects_observation.check_current observation));
       assert (Result.is_error (Actor.No_effects_observation.check_basis observation));
       let callbacks = ref 0 in
       ignore
         (Factory.resume_recurrence_retry_run_source
            (Daemon.factory reopened)
            recovered_target
            ~retry
            ~actor:original
            ~authorize
            (fun _ _ ->
               Int.incr callbacks;
               Ok ())
          |> rejected
          : P.Error.t);
       [%test_eq: int] 0 !callbacks;
       Existing.assert_same target_before recovered_target;
       [%test_eq: string list] target_frames (Existing.frames env recovered_target);
       [%test_eq: int * int] counts (!(counters.inference_ports), !(counters.providers));
       [%test_eq: int] 0 !(counters.providers);
       let index = Option.value_exn target_before.run_state in
       assert (List.is_empty (A.Run_state.runs index));
       List.iter (Execution.attempts recovered_row) ~f:(fun retained ->
         let request = Execution.Attempt.request retained in
         assert (Option.is_none (Execution.Attempt.origin retained));
         assert (Option.is_none (Execution.Attempt.terminal retained));
         assert (
           Option.is_none
             (A.Run_state.receipt
                index
                ~principal_id:(Attempt.principal_id request)
                ~key:(Attempt.key request)
                ~request_sha256:(Attempt.request_sha256 request)
              |> protocol_ok)));
       print_endline
         "actual recovery retains the exact original-next row; original record writes \
          nothing";
       print_endline
         "retained DATA restores no retry, source, absence or target Run authority")
;;

let%expect_test "uncertain controller rotation cannot release target execution" =
  let fault = Fault.create () in
  (match
     with_distinct_target
       ~wrap_env:(fun env ->
         Crash_fault_io.wrap
           env
           ~boundary:Before_sync
           ~matches:(fun path ->
             String.is_substring path ~substring:"/journal/"
             && String.is_suffix path ~suffix:".log")
           ~reached:(Fault.reached fault env))
       (fun env
         sw
         root
         daemon
         policy
         controller
         target
         _runtime
         counters
         occurrence
         observation ->
          let original_row = execution controller occurrence.intent in
          let before = state controller in
          let target_before = state target in
          let target_frames = Existing.frames env target in
          let ports = !(counters.inference_ports) in
          let escaped = ref None in
          let primary = ref None in
          let attempted =
            begin_retry daemon target occurrence observation (fun retry prepared ->
              escaped := Some (retry, prepared);
              let next = Actor.Recurrence_retry.next_attempt retry in
              deny_pending target prepared occurrence next;
              fault.selected
              <- Some
                   { controller = before.identity.session_id
                   ; occurrence
                   ; next
                   ; original = original_row
                   };
              let failure =
                Actor.rotate_prepared_recurrence_execution
                  controller.actor
                  ~retry
                  ~next:(Owner.Prepared.binding prepared)
                  ~actor:original
                  ~authorize:authorize_execution
                |> rejected
              in
              [%test_eq: P.Error.code] Persistence_error failure.code;
              primary := Some failure;
              [%test_eq: int] 1 fault.hits;
              assert_rotated
                occurrence
                next
                original_row
                (Option.value_exn fault.candidate);
              Existing.assert_same before controller;
              assert (
                Execution.equal original_row (execution controller occurrence.intent));
              assert_target_inert env target target_before target_frames counters ports;
              assert (Result.is_error (Actor.Recurrence_retry.check_basis retry));
              deny_pending target prepared occurrence next;
              let frames = Existing.frames env controller in
              Actor.rotate_prepared_recurrence_execution
                controller.actor
                ~retry
                ~next:(Owner.Prepared.binding prepared)
                ~actor:original
                ~authorize:authorize_execution
              |> rejected
              |> fun (error : P.Error.t) ->
              [%test_eq: P.Error.code] Conflict error.code;
              [%test_eq: string list] frames (Existing.frames env controller);
              Error failure)
          in
          let failure = Result_attempt.result attempted |> rejected in
          assert_error (Option.value_exn !primary) failure;
          [%test_eq: P.Error.code] Persistence_error failure.code;
          assert (Option.is_none (Result_attempt.no_effects attempted));
          assert (Option.is_none (Result_attempt.retry attempted));
          let callbacks = ref 0 in
          Factory.resume_recurrence_retry_run_source
            (Daemon.factory daemon)
            target
            ~retry:(fst (Option.value_exn !escaped))
            ~actor:original
            ~authorize
            (fun _ _ ->
               Int.incr callbacks;
               Ok ())
          |> rejected
          |> fun (error : P.Error.t) ->
          [%test_eq: P.Error.code] Conflict error.code;
          [%test_eq: int] 0 !callbacks;
          assert_target_inert env target target_before target_frames counters ports;
          assert (Result.is_error (Actor.No_effects_observation.check_basis observation));
          let retry, prepared = Option.value_exn !escaped in
          reconcile_reopened
            env
            sw
            root
            daemon
            policy
            controller
            target
            counters
            occurrence
            observation
            retry
            prepared
            (Option.value_exn fault.candidate);
          (* The original call still failed. Recovered DATA is not a retroactive
             successful controller ACK or a new effectful source capability. *)
          raise (Observed_rotation_failure failure))
   with
   | () -> failwith "rotation fault was not reached"
   | exception Observed_rotation_failure failure ->
     [%test_eq: P.Error.code] Persistence_error failure.code);
  [%test_eq: int] 1 fault.hits;
  print_endline "writer failure preserves the confirmed predecessor until actual recovery";
  print_endline "no target admission, provider, resumed source or replacement absence";
  [%expect
    {|
    actual recovery retains the exact original-next row; original record writes nothing
    retained DATA restores no retry, source, absence or target Run authority
    writer failure preserves the confirmed predecessor until actual recovery
    no target admission, provider, resumed source or replacement absence
    |}]
;;
