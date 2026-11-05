open! Core
open Agent_server_test_support
open Retry_support
module Configuration = Agent_server.Recurrence_execution_configuration
module Host = Agent_server.Recurrence_execution_host
module Service = Agent_server.Recurrence_run_service
module Public = Recurrence_service_tests
module Store = Agent_store.Session_store

let reference daemon session_id =
  P.Session_ref.create ~server_id:(Store.server_id (Daemon.store daemon)) ~session_id
;;

let configured controller target =
  Configuration.create
    ~principal_id:(P.Id.Principal.of_string "pri_544_metadata_execution" |> protocol_ok)
    ~controllers:
      (List.map [ controller; target ] ~f:(fun session_id ->
         Configuration.Controller.create
           ~session_id
           ~existing_targets:[ target ]
           ~captured_children:false
         |> protocol_ok))
    ~expires_at:None
  |> protocol_ok
;;

let select_entry daemon session_id =
  let indexed =
    Store.list_sessions_checked (Daemon.store daemon)
    |> Existing.store_ok
    |> List.find_exn ~f:(fun (row : Agent_store.Session_index.Entry.t) ->
      P.Id.Session.equal row.session.id session_id)
  in
  let expected =
    P.Session_lifecycle.Expected.create
      ~reference:(reference daemon session_id)
      ~generation:indexed.session.generation
      ~session_revision:indexed.session.revision
      ~lifecycle_revision:indexed.lifecycle_revision
    |> protocol_ok
  in
  Daemon.select_session daemon ~principal ~expected |> protocol_ok
;;

let select_controller env daemon session_id previous =
  let entry = select_entry daemon session_id in
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
    Retry_rotation_tests.await_recovered entry previous |> ignore);
  assert (Registry.execution_selected (Daemon.registry daemon) entry);
  entry
;;

let assert_unselected registry target =
  assert (not (Registry.execution_selected registry target));
  assert (not (Owner.has_activation_handoff target.Registry.runtime));
  assert (not (Owner.is_loaded target.runtime));
  assert (List.exists (Registry.entries registry) ~f:(phys_equal target));
  assert (not (List.exists (Registry.execution_entries registry) ~f:(phys_equal target)))
;;

let selected_absence env daemon controller target context selected occurrence =
  let failure =
    P.Error.create
      Invalid_request
      ~message:"544 joined selected source failure"
      ~retryable:false
      ()
  in
  let before = state target in
  let frames = Existing.frames env target in
  let before_controller = state controller in
  let controller_frames = Existing.frames env controller in
  let attempt =
    Factory.with_effect_deferred_run_source
      (Daemon.factory daemon)
      target
      ~actor:(Host.Target.actor selected)
      ~authorize:(Host.Target.authorize selected)
      ~request:
        (Attempt.request occurrence.previous ~intent:occurrence.intent |> protocol_ok)
      (fun _deferred prepared ->
         Owner.Prepared.check_basis prepared |> protocol_ok;
         let row =
           Actor.record_recurrence_execution
             controller.Registry.actor
             ~actor:(Host.Controller.actor context)
             ~authorize:(fun current _ -> Host.Controller.authorize context current)
             ~intent:occurrence.intent
             ~attempt:occurrence.previous
           |> protocol_ok
         in
         assert (Intent.equal occurrence.intent (Execution.intent row));
         assert (
           Attempt.equal
             occurrence.previous
             (Execution.Attempt.request (Execution.current_attempt row)));
         Error failure)
    |> protocol_ok
    |> attempted
  in
  assert_error failure (Factory.Deferred_run_attempt.result attempt |> rejected);
  assert (Option.is_none (Factory.Deferred_run_attempt.retry attempt));
  let observation = Factory.Deferred_run_attempt.no_effects attempt |> Option.value_exn in
  Actor.No_effects_observation.check_current observation |> protocol_ok;
  Actor.No_effects_observation.check_basis observation |> protocol_ok;
  Existing.assert_same before target;
  [%test_eq: string list] frames (Existing.frames env target);
  [%test_eq: int64]
    (Int64.succ before_controller.counters.revision)
    (state controller).counters.revision;
  [%test_eq: int]
    (List.length controller_frames + 1)
    (List.length (Existing.frames env controller));
  observation
;;

(* Every callback receives the SAME real recovered target index. The metadata
   acquisition and activation assertions stay in their individual scenario. *)
let with_indexed_targets ?wrap_env ?post_stream ?(check_operator_only = false) f =
  with_recoverable_fixture
    ?wrap_env
    ?post_stream
    ~max_root_agents:2
    (fun env sw root daemon policy old_controller _runtime _revision counters ->
       Source.with_loaded_entry daemon (fun ~entry:old_target ~runtime:_ ~revision:_ ->
         let controller_before = state old_controller in
         let target_before = state old_target in
         let controller_id = controller_before.identity.session_id in
         let target_id = target_before.identity.session_id in
         assert (not (P.Id.Session.equal controller_id target_id));
         let execution_configuration = configured controller_id target_id in
         let config =
           Creation_tests.with_max_root_agents
             (config root root (Filename.concat root "parent.chatmd"))
             ~max_root_agents:2
         in
         let config =
           { config with
             server =
               { config.server with recurrence_execution = Some execution_configuration }
           }
         in
         let old_store = Daemon.store daemon in
         Daemon.shutdown daemon |> protocol_ok;
         assert (Store.is_closed old_store);
         let start mode =
           Daemon.start
             ~sw
             ~env:
               (Option.value_map
                  wrap_env
                  ~default:(env :> Eio_unix.Stdenv.base)
                  ~f:(fun wrap -> wrap (env :> Eio_unix.Stdenv.base)))
             ~config
             ~tool_dir:root
             ~home:root
             ~process_start_identity:None
             ~options:
               { Daemon.default_options with
                 startup_mode = mode
               ; inference_policy = policy
               ; qualify_chatml_extensions = true
               }
             ()
           |> protocol_ok
         in
         if check_operator_only
         then (
           let operator = start Operator_only in
           protect_cleanup
             ~finally:(fun () -> Daemon.shutdown operator |> protocol_ok)
             (fun () ->
                let before =
                  Store.list_sessions_checked (Daemon.store operator) |> Existing.store_ok
                in
                let counts = !(counters.providers), !(counters.inference_ports) in
                assert (Option.is_none (Daemon.recurrence_execution_host operator));
                let row =
                  List.find_exn before ~f:(fun row ->
                    P.Id.Session.equal
                      row.Agent_store.Session_index.Entry.session.id
                      target_id)
                in
                let expected =
                  P.Session_lifecycle.Expected.create
                    ~reference:(reference operator target_id)
                    ~generation:row.session.generation
                    ~session_revision:row.session.revision
                    ~lifecycle_revision:row.lifecycle_revision
                  |> protocol_ok
                in
                let failure =
                  Daemon.select_session operator ~principal ~expected |> rejected
                in
                [%test_eq: P.Error.code] Invalid_state failure.code;
                [%test_eq: int] 0 (Registry.stats (Daemon.registry operator)).loaded;
                assert (
                  List.is_empty (Registry.execution_entries (Daemon.registry operator)));
                assert (
                  Option.is_none (Registry.find (Daemon.registry operator) target_id));
                assert (
                  List.equal
                    Agent_store.Session_index.Entry.equal
                    before
                    (Store.list_sessions_checked (Daemon.store operator)
                     |> Existing.store_ok));
                [%test_eq: int * int]
                  counts
                  (!(counters.providers), !(counters.inference_ports))));
         let reopened = start On_demand in
         protect_cleanup
           ~finally:(fun () -> Daemon.shutdown reopened |> protocol_ok)
           (fun () ->
              let registry = Daemon.registry reopened in
              [%test_eq: int] 0 (Registry.stats registry).loaded;
              let host = Option.value_exn (Daemon.recurrence_execution_host reopened) in
              let controller =
                select_controller env reopened controller_id controller_before
              in
              assert (Option.is_none (Registry.find registry target_id));
              [%test_eq: int] 1 (List.length (Registry.execution_entries registry));
              let context = Host.controller host controller |> protocol_ok in
              f env sw reopened old_target target_before controller context counters)))
;;

let%expect_test
    "On_demand target acquisition stays inert and preserves same-owner retry until real \
     handoff"
  =
  let provider_probe = ref (fun () -> ()) in
  let provider_selection = ref None in
  with_indexed_targets
    ~post_stream:(fun _ ->
      !provider_probe ();
      completed_response "544 acquired target complete")
    (fun env sw reopened old_target target_before controller context counters ->
       let registry = Daemon.registry reopened in
       let host = Option.value_exn (Daemon.recurrence_execution_host reopened) in
       let target_id = target_before.identity.session_id in
       let counts = !(counters.providers), !(counters.inference_ports) in
       let indexed_target () =
         Store.list_sessions_checked (Daemon.store reopened)
         |> Existing.store_ok
         |> List.find_exn ~f:(fun (row : Agent_store.Session_index.Entry.t) ->
           P.Id.Session.equal row.session.id target_id)
       in
       let indexed_before = indexed_target () in
       let recovery_rejection =
         P.Error.create
           Permission_denied
           ~message:"544 recovered target policy refusal"
           ~retryable:false
           ()
       in
       let error =
         Factory.acquire_recurrence_target
           (Daemon.factory reopened)
           ~session:(reference reopened target_id)
           ~actor:original
           ~authorize:(fun current ->
             let open Result.Let_syntax in
             let%bind () = authorize current in
             if Int64.(current.counters.revision > indexed_before.session.revision)
             then Error recovery_rejection
             else Ok ())
         |> rejected
       in
       assert_error recovery_rejection error;
       assert (Option.is_none (Registry.find registry target_id));
       [%test_eq: int] 1 (Registry.stats registry).loaded;
       [%test_eq: int] 1 (List.length (Registry.execution_entries registry));
       let indexed_after = indexed_target () in
       assert (Int64.(indexed_after.session.revision > indexed_before.session.revision));
       [%test_eq: P.Id.Session.t] target_id indexed_after.session.id;
       [%test_eq: int * int] counts (!(counters.providers), !(counters.inference_ports));
       let selected =
         Host.acquire_existing_target context ~session:(reference reopened target_id)
         |> protocol_ok
       in
       let target = Registry.find registry target_id |> Option.value_exn in
       assert (not (phys_equal target.actor old_target.actor));
       assert (not (phys_equal target.runtime old_target.runtime));
       assert_unselected registry target;
       [%test_eq: int] 2 (Registry.stats registry).loaded;
       [%test_eq: int] 1 (List.length (Registry.execution_entries registry));
       [%test_eq: int * int] counts (!(counters.providers), !(counters.inference_ports));
       let acquired = state target in
       assert (
         List.equal
           P.Moderator_execution.equal
           target_before.moderator_executions
           acquired.moderator_executions);
       assert (
         Option.equal Jsonaf.exactly_equal target_before.moderator acquired.moderator);
       assert (List.is_empty acquired.jobs);
       assert (List.is_empty acquired.invocations);
       assert (Option.is_none acquired.active_operation);
       let check_same_owner () =
         let again =
           Host.acquire_existing_target context ~session:(reference reopened target_id)
           |> protocol_ok
         in
         Host.Target.authorize again (state target) |> protocol_ok;
         let actual = Registry.find registry target_id |> Option.value_exn in
         assert (phys_equal target actual);
         assert (phys_equal target.actor actual.actor);
         assert (phys_equal target.runtime actual.runtime);
         assert (Option.equal phys_equal target.store_handle actual.store_handle)
       in
       let frames = Existing.frames env target in
       check_same_owner ();
       Existing.assert_same acquired target;
       [%test_eq: string list] frames (Existing.frames env target);
       let preview = Host.preview_target_source selected |> protocol_ok in
       let source = Factory.Recurrence_source_preview.source preview in
       assert (
         P.Run_source.equal source (A.Recurrence_source.installed acquired |> protocol_ok));
       [%test_eq: int64]
         acquired.counters.revision
         (Factory.Recurrence_source_preview.expected_revision preview);
       let second_preview = Host.preview_target_source selected |> protocol_ok in
       assert (
         P.Run_source.equal
           source
           (Factory.Recurrence_source_preview.source second_preview));
       Existing.assert_same acquired target;
       [%test_eq: string list] frames (Existing.frames env target);
       [%test_eq: int * int] counts (!(counters.providers), !(counters.inference_ports));
       (match Host.controller host target with
        | Error error ->
          [%test_eq: P.Error.code] Conflict error.code;
          assert (not error.retryable)
        | Ok _ -> failwith "configuration promoted an unselected metadata controller");
       let error =
         Owner.ensure_loaded_authenticated
           target.runtime
           ~actor:(Host.Target.actor selected)
         |> rejected
       in
       [%test_eq: P.Error.code] Conflict error.code;
       assert (not error.retryable);
       let graph_source =
         Transcript.Source_id.of_string
           ("graph/544-metadata-" ^ P.Id.Session.to_string target_id)
         |> Result.ok_or_failwith
       in
       let graph_error =
         Actor.open_inference_owner target.actor ~source:graph_source |> rejected
       in
       [%test_eq: P.Error.code] Conflict graph_error.code;
       assert (not graph_error.retryable);
       let idle_error = Actor.claim_idle_moderator target.actor |> rejected in
       [%test_eq: P.Error.code] Conflict idle_error.code;
       assert (not idle_error.retryable);
       Existing.assert_same acquired target;
       [%test_eq: string list] frames (Existing.frames env target);
       [%test_eq: int * int] counts (!(counters.providers), !(counters.inference_ports));
       List.iter
         P.Session.[ Read_only; Read_write ]
         ~f:(fun mode ->
           let attachment, _ =
             Actor.attach target.actor ~mode ~subscribe:false |> protocol_ok
           in
           assert (
             List.exists (state target).attachments ~f:(fun actual ->
               P.Id.Attachment.equal actual.id attachment.id));
           Actor.detach target.actor attachment.id |> protocol_ok;
           assert (
             not
               (List.exists (state target).attachments ~f:(fun actual ->
                  P.Id.Attachment.equal actual.id attachment.id))));
       assert_unselected registry target;
       let proposal = Host.preview_target_source selected |> protocol_ok in
       with_planner env sw reopened controller (fun () ->
         let occurrence = create_occurrence reopened controller target in
         assert (
           P.Run_source.equal
             (Intent.source occurrence.intent)
             (Factory.Recurrence_source_preview.source proposal));
         [%test_eq: int64]
           (state target).counters.revision
           (Factory.Recurrence_source_preview.expected_revision proposal);
         let previous =
           Attempt.create
             ~intent:occurrence.intent
             ~number:0
             ~principal_id:
               (Operator_authorization.principal (Host.Target.actor selected)).id
             ~expected_revision:
               (Factory.Recurrence_source_preview.expected_revision proposal)
             ~key:(Attempt.key occurrence.previous)
           |> protocol_ok
         in
         let occurrence = { occurrence with previous } in
         let before = state target in
         let frames = Existing.frames env target in
         let absence =
           selected_absence env reopened controller target context selected occurrence
         in
         assert_unselected registry target;
         check_same_owner ();
         Actor.No_effects_observation.check_current absence |> protocol_ok;
         Actor.No_effects_observation.check_basis absence |> protocol_ok;
         Existing.assert_same before target;
         [%test_eq: string list] frames (Existing.frames env target);
         [%test_eq: int * int] counts (!(counters.providers), !(counters.inference_ports));
         let selected_execution =
           Host.Target.execution selected ~intent:occurrence.intent |> protocol_ok
         in
         (provider_probe
          := fun () ->
               provider_selection
               := Some
                    ( Owner.has_activation_handoff target.runtime
                    , Registry.execution_selected registry target ));
         let attempted =
           Host.Execution.retry
             selected_execution
             ~observation:absence
             ~previous
             ~request:(next_request target occurrence)
           |> protocol_ok
         in
         let admission =
           match Service.Attempt.result attempted |> protocol_ok with
           | Activated admission -> admission
           | Retained _ | Admission_uncertain _ | Activation_failed _ ->
             failwith "same-owner actual retry failed to hand off"
         in
         assert (Option.is_none (Service.Attempt.no_effects attempted));
         assert (Option.is_none (Service.Attempt.retry attempted));
         let final, index, run = Public.await_completed target admission in
         assert (Owner.has_activation_handoff target.runtime);
         [%test_eq: (bool * bool) option] (Some (true, true)) !provider_selection;
         assert (Registry.execution_selected registry target);
         assert (List.exists (Registry.execution_entries registry) ~f:(phys_equal target));
         Host.controller host target |> protocol_ok |> ignore;
         let row = execution controller occurrence.intent in
         let next = Execution.current_attempt row |> Execution.Attempt.request in
         [%test_eq: int] 1 (Attempt.number next);
         let origin = A.Run_state.recurrence_origin index run.id |> Option.value_exn in
         A.Recurrence_run_origin.validate origin ~intent:occurrence.intent ~attempt:next
         |> protocol_ok;
         A.Recurrence_run_origin.validate_run origin ~run ~admission |> protocol_ok;
         List.iter target_before.moderator_executions ~f:(fun old ->
           assert (
             List.exists final.moderator_executions ~f:(P.Moderator_execution.equal old)));
         [%test_eq: int]
           1
           (List.count final.moderator_executions ~f:(fun receipt ->
              P.Moderator_execution.equal_phase receipt.context.phase Session_resume));
         assert (Result.is_error (Actor.No_effects_observation.check_basis absence));
         [%test_eq: int] 1 !(counters.providers);
         assert (
           String.is_substring
             (List.hd_exn !(counters.transcripts))
             ~substring:"539 actual retry input");
         Owner.with_background_runtime target.runtime (fun runtime ->
           assert (Option.is_some runtime.A.Runtime_builder.moderator_manager);
           Ok ())
         |> protocol_ok;
         print_endline
           "indexed target acquires one inert canonical owner; preview and ordinary \
            execution exclusion preserve effects";
         print_endline
           "real controls and joined absence preserve that owner; only actual retry \
            handoff selects it"));
  [%expect
    {|
    indexed target acquires one inert canonical owner; preview and ordinary execution exclusion preserve effects
    real controls and joined absence preserve that owner; only actual retry handoff selects it
    |}]
;;

let%expect_test "explicit selection promotes the same acquired legacy metadata owner" =
  let providers = ref 0 in
  let constructions = ref 0 in
  let base =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
      Int.incr providers;
      failwith "legacy metadata fixture submitted inference")
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
    ~max_root_agents:2
    ~qualify_chatml_extensions:true
    ~prompt_text:
      {|<developer>Explicit legacy metadata selection.</developer>
<script id="legacy" language="chatml" kind="moderator">
let initial_state = 0
let on_event ctx state event = Task.pure(state + 1)
</script>|}
    ~policy
    (fun env sw root daemon ->
       Source.with_loaded_entry
         daemon
         (fun ~entry:old_controller ~runtime:_ ~revision:_ ->
            Source.with_loaded_entry
              daemon
              (fun ~entry:old_target ~runtime:_ ~revision:_ ->
                 let controller_before = state old_controller in
                 let target_before = state old_target in
                 let controller_id = controller_before.identity.session_id in
                 let target_id = target_before.identity.session_id in
                 let config =
                   Creation_tests.with_max_root_agents
                     (config root root (Filename.concat root "parent.chatmd"))
                     ~max_root_agents:2
                 in
                 let config =
                   { config with
                     server =
                       { config.server with
                         recurrence_execution = Some (configured controller_id target_id)
                       }
                   }
                 in
                 Daemon.shutdown daemon |> protocol_ok;
                 let reopened =
                   Daemon.start
                     ~sw
                     ~env:(env :> Eio_unix.Stdenv.base)
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
                 in
                 protect_cleanup
                   ~finally:(fun () -> Daemon.shutdown reopened |> protocol_ok)
                   (fun () ->
                      let controller =
                        select_controller env reopened controller_id controller_before
                      in
                      let registry = Daemon.registry reopened in
                      assert (Option.is_none (Registry.find registry target_id));
                      let host =
                        Option.value_exn (Daemon.recurrence_execution_host reopened)
                      in
                      let context = Host.controller host controller |> protocol_ok in
                      let counts = !constructions, !providers in
                      let selected =
                        Host.acquire_existing_target
                          context
                          ~session:(reference reopened target_id)
                        |> protocol_ok
                      in
                      let target = Registry.find registry target_id |> Option.value_exn in
                      assert_unselected registry target;
                      [%test_eq: int * int] counts (!constructions, !providers);
                      let acquired = state target in
                      assert (
                        List.equal
                          P.Moderator_execution.equal
                          target_before.moderator_executions
                          acquired.moderator_executions);
                      Host.preview_target_source selected |> protocol_ok |> ignore;
                      Existing.assert_same acquired target;
                      let promoted = select_entry reopened target_id in
                      assert (phys_equal target promoted);
                      assert (phys_equal target.actor promoted.actor);
                      assert (phys_equal target.runtime promoted.runtime);
                      assert (
                        Option.equal phys_equal target.store_handle promoted.store_handle);
                      assert (Registry.execution_selected registry target);
                      assert (not (Owner.has_activation_handoff target.runtime));
                      Owner.ensure_loaded_authenticated
                        target.runtime
                        ~actor:(Host.Target.actor selected)
                      |> protocol_ok;
                      let final =
                        Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
                          Retry_rotation_tests.await_recovered target target_before)
                      in
                      assert (Owner.is_loaded target.runtime);
                      assert (not (Owner.has_activation_handoff target.runtime));
                      [%test_eq: int] (fst counts + 1) !constructions;
                      [%test_eq: int] 0 !providers;
                      [%test_eq: int]
                        1
                        (List.count final.moderator_executions ~f:(fun receipt ->
                           P.Moderator_execution.equal_phase
                             receipt.context.phase
                             Session_resume));
                      Host.controller host target |> protocol_ok |> ignore;
                      print_endline
                        "explicit selection retains the exact metadata owner and permits \
                         one real legacy resume";
                      print_endline
                        "ordinary selection is genuine policy, not an invented \
                         activation handoff"))));
  [%expect
    {|
    explicit selection retains the exact metadata owner and permits one real legacy resume
    ordinary selection is genuine policy, not an invented activation handoff
    |}]
;;

module Selection_cut = struct
  type t =
    | Root_before_sync
    | Stop_source
  [@@deriving sexp_of]
end

(* Read the actual appended frame through the original environment. Before_sync
   proves written candidate bytes, not durability or Actor installation. *)
let written_root env path session =
  let scan =
    Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path)
    |> Agent_store.Journal_segment.scan_contents ~max_payload_length:(64 * 1024 * 1024)
    |> Existing.store_ok
  in
  match List.last scan.entries with
  | Some entry when Int.equal (Agent_store.Frame.flags entry.frame) 0 ->
    let transaction =
      Agent_store.Transaction.decode (Agent_store.Frame.payload entry.frame)
      |> Existing.store_ok
    in
    if P.Id.Session.equal session transaction.session_id
    then
      A.Session_persistence.durable_events ~limits:D.Limits.default transaction
      |> Existing.store_ok
      |> List.find_map ~f:(fun event ->
        match
          P.Event.Durable.Payload.of_json ~kind:event.kind event.payload |> protocol_ok
        with
        | Operation_started operation ->
          (match operation.kind with
           | Turn User_submit -> Some (transaction, operation)
           | Turn _ | Compaction -> None)
        | _ -> None)
    else None
  | Some _ | None -> None
;;

exception Observed_handoff_failure of P.Error.t

let selection_cut cut =
  let fault_target : (P.Id.Session.t * Registry.t * Registry.entry) option ref =
    ref None
  in
  let written = ref None in
  let at_writer = ref None in
  let observed_failure = ref None in
  let hits = ref 0 in
  let wrap_env env =
    match cut with
    | Selection_cut.Stop_source -> env
    | Root_before_sync ->
      Crash_fault_io.wrap
        env
        ~boundary:Before_sync
        ~matches:(fun path ->
          String.is_substring path ~substring:"/journal/"
          && String.is_suffix path ~suffix:".log")
        ~reached:(fun path ->
          match !fault_target with
          | None -> ()
          | Some (session, registry, target) ->
            (match written_root env path session with
             | None -> ()
             | Some candidate ->
               fault_target := None;
               written := Some candidate;
               Int.incr hits;
               at_writer
               := Some
                    ( Owner.has_activation_handoff target.runtime
                    , Registry.execution_selected registry target );
               raise (Core_unix.Unix_error (EIO, "544 root before sync", path))))
  in
  try
    with_indexed_targets
      ~wrap_env
      ~check_operator_only:
        (match cut with
         | Root_before_sync -> true
         | Stop_source -> false)
      (fun env sw daemon _old_target target_before controller context counters ->
         let registry = Daemon.registry daemon in
         let target_id = target_before.identity.session_id in
         let selected =
           Host.acquire_existing_target context ~session:(reference daemon target_id)
           |> protocol_ok
         in
         let target = Registry.find registry target_id |> Option.value_exn in
         assert_unselected registry target;
         let attachment =
           match cut with
           | Root_before_sync -> None
           | Stop_source ->
             let attachment, _ =
               Actor.attach target.actor ~mode:Read_write ~subscribe:false |> protocol_ok
             in
             Some attachment
         in
         let cleanup_attachment () =
           Option.iter attachment ~f:(fun attachment ->
             Actor.detach target.actor attachment.P.Session.Attachment.id |> protocol_ok)
         in
         protect_cleanup ~finally:cleanup_attachment (fun () ->
           let proposal = Host.preview_target_source selected |> protocol_ok in
           with_planner env sw daemon controller (fun () ->
             let occurrence = create_occurrence daemon controller target in
             let previous =
               Attempt.create
                 ~intent:occurrence.intent
                 ~number:0
                 ~principal_id:
                   (Operator_authorization.principal (Host.Target.actor selected)).id
                 ~expected_revision:
                   (Factory.Recurrence_source_preview.expected_revision proposal)
                 ~key:(Attempt.key occurrence.previous)
               |> protocol_ok
             in
             let occurrence = { occurrence with previous } in
             match cut with
             | Root_before_sync ->
               let selected_execution =
                 Host.Target.execution selected ~intent:occurrence.intent |> protocol_ok
               in
               fault_target := Some (target_id, registry, target);
               let attempted =
                 match
                   Host.Execution.activate selected_execution ~attempt:previous
                   |> protocol_ok
                 with
                 | Service.Outcome.Attempted attempted -> attempted
                 | Retained _ ->
                   failwith "fresh metadata activation returned retained DATA"
               in
               let admission, failure =
                 match Service.Attempt.result attempted with
                 | Ok (Activation_failed { admission; error }) -> admission, error
                 | Error failure ->
                   raise_s
                     [%sexp
                       "root writer failed before known Admission", (failure : P.Error.t)]
                 | Ok (Activated _ | Retained _ | Admission_uncertain _) ->
                   failwith "unconfirmed root writer fabricated a handoff"
               in
               [%test_eq: P.Error.code] Persistence_error failure.code;
               [%test_eq: int] 1 !hits;
               [%test_eq: (bool * bool) option] (Some (false, false)) !at_writer;
               assert (Option.is_none (Service.Attempt.no_effects attempted));
               assert (Option.is_none (Service.Attempt.retry attempted));
               let candidate, operation = Option.value_exn !written in
               let confirmed = state target in
               [%test_eq: int64]
                 (Int64.succ confirmed.counters.revision)
                 candidate.session_revision;
               let index = Option.value_exn confirmed.run_state in
               let run = A.Run_state.find index admission.run_id |> Option.value_exn in
               let origin =
                 A.Run_state.recurrence_origin index run.id |> Option.value_exn
               in
               A.Recurrence_run_origin.validate
                 origin
                 ~intent:occurrence.intent
                 ~attempt:previous
               |> protocol_ok;
               A.Recurrence_run_origin.validate_run origin ~run ~admission |> protocol_ok;
               assert (
                 List.exists
                   (A.Run_state.receipts index)
                   ~f:(P.Run_receipt.equal admission));
               assert (Option.is_none confirmed.active_operation);
               assert (
                 not
                   (List.exists run.owned_work ~f:(fun work ->
                      match work.P.Run_work.key with
                      | Operation actual -> P.Id.Operation.equal actual operation.id
                      | Retained _ -> false)));
               (* The candidate exists physically, but the failed writer never
                  installed that root Operation or immutable handoff in the Actor. *)
               assert_unselected registry target;
               assert (
                 phys_equal target (Registry.find registry target_id |> Option.value_exn));
               [%test_eq: int] 0 !(counters.providers);
               observed_failure := Some failure;
               raise (Observed_handoff_failure failure)
             | Stop_source ->
               let entered, enter = Eio.Promise.create () in
               let release_gate, release_callback = Eio.Promise.create () in
               let callback_returned = ref false in
               let escaped = ref None in
               let release () =
                 if Option.is_none (Eio.Promise.peek release_gate)
                 then Eio.Promise.resolve release_callback ()
               in
               let pending =
                 Eio.Fiber.fork_promise ~sw (fun () ->
                   Factory.with_effect_deferred_run_source
                     (Daemon.factory daemon)
                     target
                     ~actor:(Host.Target.actor selected)
                     ~authorize:(Host.Target.authorize selected)
                     ~request:
                       (Attempt.request previous ~intent:occurrence.intent |> protocol_ok)
                     (fun _ prepared ->
                        escaped := Some prepared;
                        Owner.Prepared.check_basis prepared |> protocol_ok;
                        Eio.Promise.resolve enter ();
                        Eio.Promise.await release_gate;
                        callback_returned := true;
                        Error (P.Error.invalid_request "544 released source callback")))
               in
               protect_cleanup
                 ~finally:(fun () ->
                   release ();
                   ignore (Eio.Promise.await pending))
                 (fun () ->
                    Eio.Fiber.first
                      (fun () -> Eio.Promise.await entered)
                      (fun () ->
                         let result = Eio.Promise.await_exn pending in
                         raise_s
                           [%sexp
                             "source exited before control barrier"
                           , (Result.is_ok result : bool)]);
                    assert_unselected registry target;
                    Actor.stop
                      target.actor
                      ~attachment_id:(Option.value_exn attachment).id
                      ~mode:Cancel
                    |> protocol_ok
                    |> ignore;
                    Owner.unload_and_wait target.runtime |> protocol_ok;
                    (match Eio.Promise.await pending with
                     | Error (Eio.Cancel.Cancelled _) -> ()
                     | Error exn -> raise exn
                     | Ok (Error failure) ->
                       (match failure.P.Error.code with
                        | Conflict | Interrupted -> ()
                        | _ -> raise_s [%sexp (failure : P.Error.t)])
                     | Ok (Ok (Factory.Deferred_run_attempt.Outcome.Attempted attempted))
                       ->
                       assert (
                         Option.is_none
                           (Factory.Deferred_run_attempt.no_effects attempted));
                       let failure =
                         Factory.Deferred_run_attempt.result attempted |> rejected
                       in
                       (match failure.P.Error.code with
                        | Conflict | Interrupted -> ()
                        | _ -> raise_s [%sexp (failure : P.Error.t)])
                     | Ok (Ok (Retained _)) ->
                       failwith "Stop fabricated retained Admission");
                    assert (not !callback_returned);
                    assert (
                      Result.is_error
                        (Owner.Prepared.check_basis (Option.value_exn !escaped)));
                    assert_unselected registry target;
                    let current = state target in
                    [%test_eq: P.Session.desired_state] Stopped current.lifecycle.desired;
                    (match current.lifecycle.observed with
                     | Stopped -> ()
                     | observed -> raise_s [%sexp (observed : P.Session.observed_state)]);
                    assert (Option.is_none current.active_operation);
                    assert (List.is_empty current.jobs);
                    assert (List.is_empty current.invocations);
                    assert (
                      List.equal
                        P.Moderator_execution.equal
                        target_before.moderator_executions
                        current.moderator_executions);
                    assert (
                      phys_equal
                        target
                        (Registry.find registry target_id |> Option.value_exn));
                    [%test_eq: int] 0 !(counters.providers);
                    print_endline
                      "real Stop joins a live source callback without selecting its \
                       metadata owner"))))
  with
  | Observed_handoff_failure failure ->
    (match cut, !observed_failure with
     | Root_before_sync, Some actual -> assert_error actual failure
     | (Root_before_sync | Stop_source), _ -> raise (Observed_handoff_failure failure));
    print_endline
      "Operator_only cannot select; written unconfirmed root bytes never install a \
       handoff"
;;

let%expect_test "metadata execution selection requires ACK and remains controllable" =
  List.iter
    Selection_cut.[ Root_before_sync; Stop_source ]
    ~f:(fun cut ->
      try selection_cut cut with
      | exn ->
        Exn.reraise
          exn
          (Sexp.to_string_hum [%sexp "metadata selection cut", (cut : Selection_cut.t)]));
  [%expect
    {|
    Operator_only cannot select; written unconfirmed root bytes never install a handoff
    real Stop joins a live source callback without selecting its metadata owner
    |}]
;;
