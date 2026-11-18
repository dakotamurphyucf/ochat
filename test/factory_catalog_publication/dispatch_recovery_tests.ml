open! Core
open Agent_server_test_support
open Retry_support
module Driver = Agent_server.Recurrence_dispatcher
module Host = Agent_server.Recurrence_execution_host
module Configuration = Agent_server.Recurrence_execution_configuration
module Store = Agent_store.Session_store
module Read_fault = Dispatch_race_tests.Read_fault
module Target_acquisition = Recurrence_target_acquisition_tests
module Origin = A.Recurrence_run_origin
module Terminal = A.Recurrence_run_reference

let await env f =
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
    let rec loop () =
      match f () with
      | Some result -> result
      | None ->
        Eio.Fiber.yield ();
        loop ()
    in
    loop ())
;;

let selected_failure daemon controller target occurrence =
  let primary =
    P.Error.create
      Invalid_request
      ~message:"485 recorded historical attempt before selected source failure"
      ~retryable:false
      ()
  in
  let before = state controller in
  let target_before = state target in
  let attempt =
    Factory.with_effect_deferred_run_source
      (Daemon.factory daemon)
      target
      ~actor:original
      ~authorize
      ~request:
        (Attempt.request occurrence.previous ~intent:occurrence.intent |> protocol_ok)
      (fun _ prepared ->
         Owner.Prepared.check_basis prepared |> protocol_ok;
         Actor.record_recurrence_execution
           controller.Registry.actor
           ~actor:original
           ~authorize:authorize_execution
           ~intent:occurrence.intent
           ~attempt:occurrence.previous
         |> protocol_ok
         |> ignore;
         Error primary)
    |> protocol_ok
    |> attempted
  in
  assert_error primary (Factory.Deferred_run_attempt.result attempt |> rejected);
  let no_effects = Factory.Deferred_run_attempt.no_effects attempt |> Option.value_exn in
  Actor.No_effects_observation.check_current no_effects |> protocol_ok;
  [%test_eq: int64]
    (Int64.succ before.counters.transaction_sequence)
    (state controller).counters.transaction_sequence;
  Existing.assert_same target_before target;
  let recorded = execution controller occurrence.intent in
  assert (Execution.is_pending recorded);
  assert (Option.is_none (Execution.Attempt.origin (Execution.current_attempt recorded)));
  no_effects
;;

let with_driver daemon sw configuration f =
  let host =
    Host.create
      configuration
      ~factory:(Daemon.factory daemon)
      ~server_id:(Store.server_id (Daemon.store daemon))
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
    (fun () -> f host driver)
;;

let execution_configuration historical_controller current_controller =
  let grant session_id =
    Configuration.Controller.create
      ~session_id
      ~existing_targets:[ session_id ]
      ~captured_children:false
    |> protocol_ok
  in
  Configuration.create
    ~principal_id:(P.Id.Principal.of_string "pri_485_recovered_executor" |> protocol_ok)
    ~controllers:[ grant historical_controller; grant current_controller ]
    ~expires_at:None
  |> protocol_ok
;;

let start env sw root ~policy =
  let config =
    Creation_tests.with_max_root_agents
      (config root root (Filename.concat root "parent.chatmd"))
      ~max_root_agents:3
  in
  Daemon.start
    ~sw
    ~env
    ~config
    ~tool_dir:root
    ~home:root
    ~process_start_identity:None
    ~options:
      { Daemon.default_options with
        startup_mode = On_demand
      ; inference_policy = policy
      ; qualify_chatml_extensions = true
      }
    ()
  |> protocol_ok
;;

let assert_indexed_only registry session_id =
  assert (Option.is_none (Registry.find registry session_id));
  assert (
    not
      (List.exists (Registry.entries registry) ~f:(fun entry ->
         P.Id.Session.equal (state entry).identity.session_id session_id)))
;;

let same_attempt expected execution =
  let requests = Execution.attempts execution |> List.map ~f:Execution.Attempt.request in
  assert (List.equal Attempt.equal [ expected ] requests)
;;

module Probe = struct
  type t =
    { providers : int ref
    ; ports : int ref
    ; entered : unit Eio.Promise.t
    ; release : unit -> unit
    }
end

module Saved = struct
  type t =
    { historical_id : P.Id.Session.t
    ; old_target_id : P.Id.Session.t
    ; historical_directory : string
    ; old_occurrence : occurrence
    ; current_before : A.Session_state.t
    ; current_occurrence : occurrence
    ; configuration : Configuration.t
    }
end

module Finished = struct
  type t =
    { receipt : P.Run_receipt.t
    ; origin : Origin.t
    ; attempt : Attempt.t
    ; state : A.Session_state.t
    }
end

let with_saved_sessions env sw daemon f =
  Source.with_loaded_entry daemon (fun ~entry:historical ~runtime:_ ~revision:_ ->
    Source.with_loaded_entry daemon (fun ~entry:old_target ~runtime:_ ~revision:_ ->
      let old_occurrence =
        with_planner env sw daemon historical (fun () ->
          create_occurrence daemon historical old_target)
      in
      let no_effects = selected_failure daemon historical old_target old_occurrence in
      Source.with_loaded_entry daemon (fun ~entry:current ~runtime:_ ~revision:_ ->
        let current_occurrence =
          with_planner env sw daemon current (fun () ->
            create_occurrence daemon current current)
        in
        let historical_id = (state historical).identity.session_id in
        let current_before = state current in
        let saved =
          { Saved.historical_id
          ; old_target_id = (state old_target).identity.session_id
          ; historical_directory =
              Store.Handle.directory (Option.value_exn historical.store_handle)
          ; old_occurrence
          ; current_before
          ; current_occurrence
          ; configuration =
              execution_configuration historical_id current_before.identity.session_id
          }
        in
        let old_store = Daemon.store daemon in
        Daemon.shutdown daemon |> protocol_ok;
        assert (Store.is_closed old_store);
        ignore
          (Actor.No_effects_observation.check_current no_effects |> rejected : P.Error.t);
        f saved)))
;;

let complete_without_import
      env
      registry
      driver
      current
      (saved : Saved.t)
      (probe : Probe.t)
  =
  Driver.process_entry driver current |> protocol_ok;
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
    Eio.Promise.await probe.entered);
  let active = state current in
  let current_execution = execution current saved.current_occurrence.intent in
  let attempt = Execution.current_attempt current_execution in
  let origin = Execution.Attempt.origin attempt |> Option.value_exn in
  let admission = Origin.admission origin in
  let run =
    A.Run_state.find (Option.value_exn active.run_state) admission.run_id
    |> Option.value_exn
  in
  Origin.validate_run origin ~run ~admission |> protocol_ok;
  assert (Option.is_none (Execution.Attempt.terminal attempt));
  let original_attempt = Execution.Attempt.request attempt in
  [%test_eq: int] 1 !(probe.providers);
  (* Lose the actual driver before target completion. Only durable pending
     occurrence DATA survives; no completed source envelope is copied. *)
  Driver.close driver;
  probe.release ();
  let ended, run =
    await env (fun () ->
      let current = state current in
      let run =
        A.Run_state.find (Option.value_exn current.run_state) admission.run_id
        |> Option.value_exn
      in
      match run.lifecycle with
      | Terminal (Completed _) -> Some (current, run)
      | Terminal _ -> raise_s [%sexp (run : P.Run.t)]
      | Admitted | Active | Waiting _ -> None)
  in
  let still_pending = execution current saved.current_occurrence.intent in
  same_attempt original_attempt still_pending;
  assert (Execution.is_pending still_pending);
  assert (
    Option.is_none (Execution.Attempt.terminal (Execution.current_attempt still_pending)));
  assert_indexed_only registry saved.historical_id;
  assert_indexed_only registry saved.old_target_id;
  let receipt =
    A.Run_state.receipts (Option.value_exn ended.run_state)
    |> List.find_exn ~f:(fun receipt ->
      P.Id.Run.equal receipt.P.Run_receipt.run_id run.id
      && P.Run_receipt.Kind.equal receipt.kind Terminal)
  in
  { Finished.receipt; origin; attempt = original_attempt; state = ended }
;;

let run_after_indexed_read_recovers env sw root policy (saved : Saved.t) (probe : Probe.t)
  =
  let unavailable = ref false in
  let failed_reads = ref 0 in
  let wrapped =
    Read_fault.wrap
      (env :> Eio_unix.Stdenv.base)
      ~before_read:(fun path ->
        if
          !unavailable
          && String.is_prefix path ~prefix:(saved.historical_directory ^ Filename.dir_sep)
        then (
          Int.incr failed_reads;
          Missing)
        else Read)
  in
  let reopened = start wrapped sw root ~policy in
  protect_cleanup
    ~finally:(fun () ->
      unavailable := false;
      probe.release ();
      Daemon.shutdown reopened |> protocol_ok)
    (fun () ->
       let registry = Daemon.registry reopened in
       [%test_eq: int] 0 (Registry.stats registry).loaded;
       let current =
         Target_acquisition.select_controller
           env
           reopened
           saved.current_before.identity.session_id
           saved.current_before
       in
       assert_indexed_only registry saved.historical_id;
       assert_indexed_only registry saved.old_target_id;
       [%test_eq: int] 1 (Registry.stats registry).loaded;
       with_planner env sw reopened current (fun () ->
         with_driver reopened sw saved.configuration (fun host driver ->
           let before = state current in
           let frames = Existing.frames env current in
           let index =
             Store.list_sessions_checked (Daemon.store reopened) |> Existing.store_ok
           in
           let counts = !(probe.ports), !(probe.providers) in
           unavailable := true;
           let inventory = Host.pending_occupancy host |> protocol_ok in
           [%test_eq: int] 1 (List.length inventory.unknown);
           let unknown = List.hd_exn inventory.unknown in
           assert (
             P.Session_ref.equal
               unknown.controller
               (P.Session_ref.create
                  ~server_id:(Store.server_id (Daemon.store reopened))
                  ~session_id:saved.historical_id));
           assert (List.is_empty inventory.known);
           let denied = Driver.process_entry driver current |> rejected in
           assert_error unknown.error denied;
           assert (!failed_reads > 0);
           Existing.assert_same before current;
           [%test_eq: string list] frames (Existing.frames env current);
           assert (
             List.equal
               Agent_store.Session_index.Entry.equal
               index
               (Store.list_sessions_checked (Daemon.store reopened) |> Existing.store_ok));
           [%test_eq: int * int] counts (!(probe.ports), !(probe.providers));
           assert_indexed_only registry saved.historical_id;
           assert_indexed_only registry saved.old_target_id;
           unavailable := false;
           let inventory = Host.pending_occupancy host |> protocol_ok in
           assert (List.is_empty inventory.unknown);
           [%test_eq: int] 1 (List.length inventory.known);
           let known = List.hd_exn inventory.known in
           assert (
             A.Recurrence_identity.Occurrence.equal
               known.occurrence
               (Intent.occurrence saved.old_occurrence.intent));
           assert (P.Session_ref.equal known.target saved.old_occurrence.session);
           assert (not (P.Session_ref.equal known.target saved.current_occurrence.session));
           complete_without_import env registry driver current saved probe)))
;;

let reconcile_after_reopen
      env
      sw
      root
      policy
      (saved : Saved.t)
      (finished : Finished.t)
      (probe : Probe.t)
  =
  let final = start (env :> Eio_unix.Stdenv.base) sw root ~policy in
  protect_cleanup
    ~finally:(fun () -> Daemon.shutdown final |> protocol_ok)
    (fun () ->
       let registry = Daemon.registry final in
       [%test_eq: int] 0 (Registry.stats registry).loaded;
       let current =
         Target_acquisition.select_controller
           env
           final
           saved.current_before.identity.session_id
           finished.state
       in
       assert_indexed_only registry saved.historical_id;
       assert_indexed_only registry saved.old_target_id;
       with_driver final sw saved.configuration (fun _host driver ->
         let counts = !(probe.ports), !(probe.providers) in
         Driver.process_entry driver current |> protocol_ok;
         let imported = execution current saved.current_occurrence.intent in
         same_attempt finished.attempt imported;
         let terminal =
           Execution.current_attempt imported
           |> Execution.Attempt.terminal
           |> Option.value_exn
         in
         assert (not (Execution.is_pending imported));
         assert (P.Run_receipt.equal finished.receipt (Terminal.receipt terminal));
         let ended = state current in
         let origin =
           Execution.current_attempt imported
           |> Execution.Attempt.origin
           |> Option.value_exn
         in
         assert (Origin.equal finished.origin origin);
         let run =
           A.Run_state.find (Option.value_exn ended.run_state) (Terminal.run_id terminal)
           |> Option.value_exn
         in
         Terminal.validate terminal ~origin ~run ~receipt:finished.receipt |> protocol_ok;
         assert (Terminal.Outcome.equal (Terminal.outcome terminal) Completed);
         [%test_eq: int * int] counts (!(probe.ports), !(probe.providers));
         List.iter saved.current_before.moderator_executions ~f:(fun previous ->
           let retained =
             List.find_exn ended.moderator_executions ~f:(fun receipt ->
               P.Id.Moderator_execution.equal
                 receipt.context.id
                 previous.P.Moderator_execution.context.id)
           in
           assert (P.Moderator_execution.equal previous retained));
         let frames = Existing.frames env current in
         Driver.process_entry driver current |> protocol_ok;
         Existing.assert_same ended current;
         [%test_eq: string list] frames (Existing.frames env current);
         [%test_eq: int] 1 !(probe.providers);
         [%test_eq: int] 1 (Registry.stats registry).loaded;
         assert_indexed_only registry saved.historical_id;
         assert_indexed_only registry saved.old_target_id))
;;

let%expect_test
    "indexed historical uncertainty blocks new work; restored reads and fresh driver \
     reconcile without replay"
  =
  let providers = ref 0 in
  let ports = ref 0 in
  let entered, signal_entered = Eio.Promise.create () in
  let release, signal_release = Eio.Promise.create () in
  let release_once () =
    if Option.is_none (Eio.Promise.peek release)
    then Eio.Promise.resolve signal_release ()
  in
  let probe = { Probe.providers; ports; entered; release = release_once } in
  let base =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
      Int.incr providers;
      if not (Int.equal !providers 1) then failwith "recovered driver replayed inference";
      Eio.Promise.resolve signal_entered ();
      Eio.Promise.await release;
      completed_response "485 actual target completes before import is recovered")
  in
  let policy =
    { base with
      runtime_inference_ports =
        (fun actor ->
          Int.incr ports;
          base.runtime_inference_ports actor)
    }
  in
  Creation_tests.with_daemon
    ~prompt_text:Source.prompt_text
    ~qualify_chatml_extensions:true
    ~max_root_agents:3
    ~policy
    (fun env sw root daemon ->
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 90. (fun () ->
         protect_cleanup ~finally:release_once (fun () ->
           with_saved_sessions env sw daemon (fun saved ->
             let finished =
               run_after_indexed_read_recovers env sw root policy saved probe
             in
             reconcile_after_reopen env sw root policy saved finished probe))));
  print_endline
    "indexed controller read error retains whole-local uncertainty and zero effects; \
     restored historic target facts permit independent work; reopened driver imports the \
     exact pending terminal once with no inference replay";
  [%expect
    {| indexed controller read error retains whole-local uncertainty and zero effects; restored historic target facts permit independent work; reopened driver imports the exact pending terminal once with no inference replay |}]
;;
