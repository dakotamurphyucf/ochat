open! Core
open Agent_server_test_support
open Retry_support
module B = A.Runtime_builder
module Completion = Actor.Idle_moderator_completion
module Graph = Agent_server.Graph_tracking
module Manager = Chat_response.Moderator_manager

let within env phase f =
  try Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. f with
  | Eio.Time.Timeout -> failwith ("579 legacy completion timed out: " ^ phase)
;;

let source =
  {|<config model="fixture-model"/>
<developer>Actual standalone aggregate legacy callback.</developer>
<script language="chatml" kind="moderator">
type state = int
type event = [ `Session_start | `Session_resume | `Tick ]
let initial_state = 0
let on_event : context -> state -> event -> state task =
  fun ctx state event -> match event with
  | `Session_start -> Task.pure(state)
  | `Session_resume -> Task.pure(state)
  | `Tick -> Task.pure(state + 1)
</script>|}
;;

module Physical = struct
  module Store = Agent_store
  module Persistence = A.Session_persistence

  type t =
    { initial : A.Session_state.t
    ; handle : Store.Session_store.Handle.t
    ; journal : Store.Journal.t
    ; writer : Store.Commit_writer.t
    ; durable : Persistence.t
    ; actor : Actor.t
    }

  let state t = Actor.state t.actor |> protocol_ok
  let metadata = Session_projection.metadata
  let index_entry = Session_projection.index_entry

  let frames t =
    let scan = Store.Journal.scan t.journal |> Existing.store_ok in
    List.map scan.entries ~f:(fun entry -> Store.Frame.payload entry.Store.Journal.frame)
  ;;

  let with_actor env sw daemon ~revision ~definition ~permission ~inference_target f =
    let store = Daemon.store daemon in
    let session_id = P.Id.Session.create () in
    let request = create_request () in
    let now = P.Timestamp.now () in
    let workspace =
      A.Workspace_catalog.definitions (Daemon.workspaces daemon)
      |> List.filter ~f:(fun workspace ->
        A.Prompt_definition.allows_workspace
          definition
          workspace.A.Workspace_definition.id)
      |> function
      | [ workspace ] -> workspace
      | [] | _ :: _ :: _ ->
        failwith "legacy fixture requires one actual allowed workspace"
    in
    (match workspace.source with
     | Physical _ -> ()
     | Temporary _ ->
       failwith "legacy fixture requires its actual configured physical root");
    let initial = ref None in
    let handle =
      Store.Session_store.create_session_initialized
        store
        ~sw
        ~transaction_id:(P.Id.Transaction.create ())
        ~actor_lock_nonce:(P.Id.Transaction.to_string (P.Id.Transaction.create ()))
        ~initialize:(fun ~staging_directory ->
          let open Result.Let_syntax in
          let%bind workspace_instance =
            A.Workspace_resolver.resolve
              ~env
              ~instance_id:(P.Id.Workspace_instance.create ())
              ~session_directory:staging_directory
              workspace
          in
          let identity : A.Session_state.Identity.t =
            { session_id
            ; display_name = request.spec.display_name
            ; creating_principal = Some principal.P.Principal.id
            ; created_at = now
            ; updated_at = now
            ; labels = request.spec.labels
            ; generation = 0
            ; metadata_revision = 0L
            ; organization = P.Session_organization.Values.empty
            }
          in
          let spec : A.Session_state.Spec.t =
            { protocol = request.spec
            ; prompt_definition_id = Some definition.id
            ; delegation = None
            ; root_invocation = A.Prompt_definition.root_invocation definition
            ; prompt_revision_id = A.Prompt_revision.id revision
            ; configuration_revision = 0L
            ; inference_target
            ; workspace_instance
            ; permission_profile = permission.A.Permission_policy.id
            ; permission_profile_digest = permission.revision_digest
            ; runtime_policy = definition.runtime_policy
            ; quota_key = None
            }
          in
          let state = A.Session_state.create ~identity ~spec ~initial_history:[] in
          let%bind restored, _ =
            Persistence.install_restored_snapshot_at
              (Persistence.Restored.authored state)
              ~env
              ~directory:(Filename.concat staging_directory "snapshot")
              ~max_payload_length:1048576
              ~transaction_hash:None
              state
          in
          initial := Some restored;
          Store.Session_store.Initial_projection.create
            ~metadata:(metadata state)
            ~entry:(index_entry state))
      |> Existing.store_ok
    in
    protect_cleanup
      ~finally:(fun () ->
        Store.Session_store.close_session store handle |> Existing.store_ok)
      (fun () ->
         let restored = Option.value_exn !initial in
         let initial = Persistence.Restored.state restored in
         assert (Option.is_none initial.moderator && Option.is_none initial.run_state);
         assert (
           Option.is_none initial.goal_descendants && Option.is_none initial.goal_catalog);
         assert (Option.is_none (Registry.find (Daemon.registry daemon) session_id));
         let journal =
           Store.Journal.create
             ~env
             ~directory:(Store.Session_store.Handle.journal_directory handle)
             ~max_payload_length:1048576
             ~max_segment_bytes:4194304L
             ~max_segment_frames:32
           |> Existing.store_ok
         in
         let writer =
           Store.Commit_writer.create
             ~sw
             ~journal
             ~session_id
             ~next_transaction_sequence:1L
             ~previous_transaction_hash:None
             ~queue_capacity:8
           |> Existing.store_ok
         in
         protect_cleanup
           ~finally:(fun () -> Store.Commit_writer.close writer)
           (fun () ->
              let durable =
                Persistence.create
                  ~archive:
                    (A.Compaction_archive.write_document
                       ~env
                       ~handle
                       ~max_payload_length:1048576)
                  ~pending_archive:None
                  ~before_commit:
                    (Some
                       (fun state ->
                         Store.Session_store.prepare_canonical_projection
                           store
                           handle
                           ~metadata:(metadata state)
                           ~entry:(index_entry state)
                         |> Result.map_error ~f:Store.Store_error.to_protocol_error))
                  ~command_accepted:(fun _ _ -> ())
                  ~writer
                  ~durability:Flush
                  ~limits:Document_schema.Limits.default
                  ~archive_limits:Document_schema.Limits.default
                  ~retention_preflight:None
                  ~restored
                  ~previous_transaction_hash:None
              in
              let goal_descendant_profile =
                A.Goal_descendants.Profile.create
                  ~max_scopes:8
                  ~max_roots:16
                  ~max_branches:16
                  ~max_encoded_bytes:(256 * 1024)
                  ~child_max_encoded_bytes:(1024 * 1024)
                |> protocol_ok
              in
              let actor =
                Actor.create
                  ~sw
                  ~clock:(Eio.Stdenv.clock env)
                  ~mailbox_capacity:32
                  ~compaction_env:(Some env)
                  ~initial_state:initial
                  ~persistence:(Persistence.actor_persistence durable)
                  ~operation_worker:None
                  ~services:
                    { now = P.Timestamp.now
                    ; monotonic_now = (fun () -> Mtime.min_stamp)
                    ; create_attachment_id = P.Id.Attachment.create
                    ; create_reclaim_token = (fun () -> "legacy-physical-host")
                    ; state_committed =
                        (fun state _ ->
                          Store.Session_store.write_metadata
                            ~entry:(index_entry state)
                            store
                            handle
                            (metadata state)
                          |> Existing.store_ok)
                    ; job_results = None
                    ; goal_descendant_profile
                    ; subscription_limits = A.Staged_subscriptions.default_limits
                    ; schedule_limits = A.Staged_schedules.default_limits
                    ; notification_limits = A.Staged_notifications.default_limits
                    ; ingress_limits = A.Staged_ingress.default_limits
                    }
              in
              protect_cleanup
                ~finally:(fun () -> Actor.shutdown actor)
                (fun () ->
                   f { initial; handle; journal; writer; durable; actor };
                   assert (
                     Option.is_none (Registry.find (Daemon.registry daemon) session_id)))))
  ;;
end

module Fixture = struct
  type t =
    { env : Eio_unix.Stdenv.base
    ; sw : Eio.Switch.t
    ; root : string
    ; daemon : Daemon.t
    ; physical : Physical.t
    ; owner : Owner.t
    ; runtime : B.t
    ; providers : int ref
    }

  let preparation_ok result =
    Result.map_error result ~f:(fun error ->
      Sexp.to_string_hum (Inference_runtime.Preparation_error.sexp_of_t error))
    |> Result.ok_or_failwith
  ;;

  let with_runtime ?test_root ?(wrap_env = Fn.id) ?(before_provider = Fn.ignore) ~source f
    =
    let providers = ref 0 in
    let policy =
      inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
        Int.incr providers;
        before_provider ();
        completed_response "579 actual standalone legacy provider")
    in
    Creation_tests.with_daemon
      ?test_root
      ~wrap_env
      ~prompt_text:source
      ~qualify_chatml_extensions:true
      ~policy
      (fun env sw root daemon ->
         let env = wrap_env (env :> Eio_unix.Stdenv.base) in
         let selected = List.hd_exn (A.Prompt_catalog.entries (Daemon.prompts daemon)) in
         let revision =
           match selected.availability with
           | Ready revision -> revision
           | Disabled | Unavailable _ ->
             failwith "actual legacy source revision unavailable"
         in
         let permission =
           A.Permission_policy.create
             ~id:selected.definition.permission_profile
             ~tool_default:Allow
             ~approval_timeout_ms:None
             ~fallback:Fallback_deny
             ~manifest_authorization:Require_grant
             ~evaluator:None
             ~evaluator_revision:None
             ~reviewer:None
           |> protocol_ok
         in
         let target =
           policy.capture_inference_target
             ~prompt_revision_id:(A.Prompt_revision.id revision)
             ~config:
               (Chat_response.Config.of_elements (A.Prompt_revision.elements revision))
           |> preparation_ok
         in
         let inference_target =
           Inference.Selection.captured target ~limits:Document_schema.Limits.default
           |> Result.map_error ~f:(fun error ->
             Sexp.to_string_hum (Inference.Request.Error.sexp_of_t error))
           |> Result.ok_or_failwith
         in
         let context = policy.resolve_inference_context target |> preparation_ok in
         Physical.with_actor
           env
           sw
           daemon
           ~revision
           ~definition:selected.definition
           ~permission
           ~inference_target
           (fun physical ->
              let initial = physical.initial in
              let handle = physical.handle in
              let paths =
                A.Runtime_paths.create
                  ~env
                  ~tool_dir:root
                  ~workspace:initial.spec.workspace_instance.canonical_root.native_path
                  ~prompt_dir:
                    (Eio.Path.native_exn (A.Prompt_revision.materialized_tree revision))
                  ~session_dir:(Agent_store.Session_store.Handle.directory handle)
                  ~cache_dir:(Agent_store.Session_store.Handle.cache_directory handle)
                  ~home:root
                |> Existing.store_ok
              in
              let upstream =
                policy.runtime_inference_ports physical.actor |> preparation_ok
              in
              let graph_source =
                Transcript.Source_id.of_string
                  ("legacy-completion/"
                   ^ P.Id.Transaction.to_string (P.Id.Transaction.create ()))
                |> Result.ok_or_failwith
              in
              let graph =
                Graph.create physical.actor ~source:graph_source ~upstream |> protocol_ok
              in
              protect_cleanup
                ~finally:(fun () ->
                  Graph.seal graph |> protocol_ok;
                  Graph.finish graph |> protocol_ok)
                (fun () ->
                   let reservation =
                     Actor.reserve_history_block physical.actor ~count:100 |> protocol_ok
                   in
                   let jobs : A.Runtime_model_services.t =
                     { spawn_model =
                         (fun ~recipe:_ ~payload:_ ->
                           failwith "unexpected legacy model job")
                     ; call_model =
                         (fun ~recipe:_ ~payload:_ ~execute:_ ->
                           failwith "unexpected legacy model job")
                     }
                   in
                   let builds = ref 0 in
                   let built_runtime = ref None in
                   let build () =
                     Int.incr builds;
                     let runtime =
                       B.build
                         ~construction_checkpoint:None
                         ~legacy_construction:None
                         ~agent_source:None
                         ~startup:None
                         ~root_invocation:Ui
                         ~script_preparation:Daemon.default_options.script_preparation
                         ~sw
                         ~env
                         ~inference_context:context
                         ~inference_identity:(Graph.identity graph)
                         ~bind_inference_identity:(fun allocate ->
                           Graph.bind_identity graph ~allocate)
                         ~on_inference_attempt:(Graph.on_attempt graph)
                         ~on_inference_completion:(Graph.on_completion graph)
                         ~on_inference_observation:(Graph.on_observation graph)
                         ~paths
                         ~storage_paths:paths
                         ~revision
                         ~session_id:initial.identity.session_id
                         ~history_namespace:
                           (P.Id.Session.to_string initial.identity.session_id)
                         ~next_history_sequence:
                           (Int64.to_int_exn reservation.first_sequence)
                         ~existing_history:
                           (Some
                              (A.History_codec.all_of_protocol
                                 initial.conversation.canonical_history
                               |> protocol_ok))
                         ~existing_moderator_snapshot:None
                         ~moderator_reservation_size:100
                         ~manifest_authorizer:
                           Shell_runtime.Manifest_authorizer.assume_authorized
                         ~approval_provider:Shell_runtime.Approval_broker.None_available
                         ~approval_store:(Shell_access.Approval.create_store ())
                         ~permission_profile:permission
                         ~review_permission:(fun _ ->
                           failwith "unexpected legacy permission review")
                         ~schedule_services:
                           { after_ms =
                               (fun ~delay_ms:_ ~payload:_ ->
                                 failwith "unexpected legacy timer")
                           ; cancel = (fun ~id:_ -> failwith "unexpected legacy timer")
                           }
                         ~job_services:
                           { initialization = jobs; for_execution = (fun _ -> jobs) }
                       |> protocol_ok
                     in
                     let handed_off = ref false in
                     protect_cleanup
                       ~finally:(fun () -> if not !handed_off then runtime.close ())
                       (fun () ->
                          runtime.start_moderator () |> protocol_ok |> ignore;
                          built_runtime := Some runtime;
                          handed_off := true;
                          Ok runtime)
                   in
                   let owner = Owner.create ~actor:physical.actor ~initial:None ~build in
                   protect_cleanup
                     ~finally:(fun () -> Owner.close_and_wait owner)
                     (fun () ->
                        assert (not (Owner.is_loaded owner));
                        Owner.ensure_loaded owner |> protocol_ok;
                        let runtime = Option.value_exn !built_runtime in
                        [%test_eq: int] 1 !builds;
                        assert (Option.is_none runtime.legacy_events);
                        let manager = Option.value_exn runtime.moderator_manager in
                        assert (Option.is_none (Manager.extension_definition manager));
                        assert (
                          Option.is_none
                            (A.Moderator_checkpoint.queue
                               (Physical.state physical).moderator
                             |> protocol_ok));
                        Owner.with_loaded_runtime owner (fun selected ->
                          assert (phys_equal selected runtime);
                          Ok ())
                        |> protocol_ok;
                        let attachment, _ =
                          Actor.attach physical.actor ~mode:Read_write ~subscribe:false
                          |> protocol_ok
                        in
                        protect_cleanup
                          ~finally:(fun () ->
                            Actor.detach physical.actor attachment.id |> protocol_ok)
                          (fun () ->
                             Actor.start
                               ~actor:original
                               physical.actor
                               ~attachment_id:attachment.id
                             |> protocol_ok
                             |> ignore;
                             let current =
                               Actor.with_moderator_checkpoint physical.actor (fun () ->
                                 Actor.state physical.actor)
                               |> protocol_ok
                             in
                             assert (
                               Option.is_none current.active_operation
                               && Option.is_none current.failure);
                             (match current.lifecycle.observed with
                              | Idle -> ()
                              | _ -> failwith "real legacy root did not reach Idle");
                             assert (
                               Option.is_none
                                 (Registry.find
                                    (Daemon.registry daemon)
                                    initial.identity.session_id));
                             f
                               { env
                               ; sw
                               ; root
                               ; daemon
                               ; physical
                               ; owner
                               ; runtime
                               ; providers
                               };
                             [%test_eq: int] 1 !builds)))))
  ;;

  let enqueue t event =
    t.runtime.enqueue_internal_event
      (Session.Snapshot.to_jsonaf event)
      ~prepare:(fun ~before ~snapshot ->
        let actual =
          A.Moderator_checkpoint.decode (Physical.state t.physical).moderator
          |> protocol_ok
          |> Option.value_exn
        in
        assert (
          Jsonaf.exactly_equal
            (B.encode_moderator_snapshot before)
            (B.encode_moderator_snapshot actual));
        Actor.change_moderator
          t.physical.actor
          (Some (B.encode_moderator_snapshot snapshot))
        |> Result.map ~f:ignore)
    |> protocol_ok
    |> ignore
  ;;

  let with_completion t ~capture f =
    let completion = Completion.create t.physical.actor in
    let retire () =
      Completion.cancel
        completion
        ~failure:
          (P.Error.create
             Interrupted
             ~message:"test host left its actual legacy completion"
             ~retryable:false
             ());
      Completion.join_retirement completion |> protocol_ok
    in
    match
      let captured =
        Eio.Cancel.protect (fun () ->
          Exn.protect
            ~finally:(fun () -> Completion.leave_capture completion)
            ~f:(fun () ->
              let history =
                Completion.claim completion |> protocol_ok |> Option.value_exn
              in
              capture completion history))
      in
      f completion captured
    with
    | result ->
      retire ();
      result
    | exception exn ->
      let backtrace = Stdlib.Printexc.get_raw_backtrace () in
      (* Cancellation schedules the same Actor-owned retirement. If this caller
         is cancelled, its join can be interrupted; fixture source close still
         joins that retained owner. Never replace the original failure. *)
      (try retire () with
       | _ -> ());
      Exn.raise_with_original_backtrace exn backtrace
  ;;

  let count t =
    let snapshot =
      A.Moderator_checkpoint.decode (Physical.state t.physical).moderator
      |> protocol_ok
      |> Option.value_exn
    in
    assert (List.is_empty snapshot.queued_internal_events);
    match snapshot.current_state with
    | Session.Snapshot.Int count -> count
    | _ -> failwith "actual legacy checkpoint lost its integer state"
  ;;
end

let%expect_test "real standalone legacy Owner checkpoints one aggregate callback" =
  Fixture.with_runtime ~source (fun fixture ->
    [%test_eq: int] 0 (Fixture.count fixture);
    Fixture.enqueue fixture (Variant ("Tick", []));
    let before = Physical.state fixture.physical in
    assert (not (Owner.drain_idle_moderator fixture.owner |> protocol_ok));
    [%test_eq: int] 1 (Fixture.count fixture);
    assert (
      Int64.(
        (Physical.state fixture.physical).counters.revision > before.counters.revision));
    assert (Option.is_none (Physical.state fixture.physical).active_operation);
    [%test_eq: int] 0 !(fixture.providers));
  print_endline
    "actual legacy None-construction Owner, physical Actor checkpoint, no Turn/provider";
  [%expect
    {| actual legacy None-construction Owner, physical Actor checkpoint, no Turn/provider |}]
;;

let%expect_test "stale paired legacy completion cannot clear a newer actual claim" =
  Fixture.with_runtime ~source (fun fixture ->
    Fixture.enqueue fixture (Variant ("Tick", []));
    Fixture.with_completion
      fixture
      ~capture:(fun first history ->
        let drain =
          match fixture.runtime.drain_internal_events history with
          | Ok drain -> drain
          | Error failure ->
            ignore (Completion.fail_callback first ~failure : (unit, P.Error.t) result);
            protocol_ok (Error failure)
        in
        Completion.capture first drain |> protocol_ok;
        drain)
      (fun first drain ->
         Completion.finish first |> protocol_ok;
         [%test_eq: int] 1 (Fixture.count fixture);
         Fixture.with_completion
           fixture
           ~capture:(fun _ _ -> ())
           (fun next () ->
              let before = Physical.frames fixture.physical in
              let foreign_failure =
                P.Error.create
                  Interrupted
                  ~message:"foreign domain must not fail this actual claim"
                  ~retryable:false
                  ()
              in
              let rejected (result : (unit, P.Error.t) result) =
                match result with
                | Error error -> [%test_eq: P.Error.code] Conflict error.code
                | Ok () -> failwith "foreign domain consumed an actual legacy claim"
              in
              Eio.Domain_manager.run (Eio.Stdenv.domain_mgr fixture.env) (fun () ->
                rejected (Completion.capture next drain);
                rejected (Completion.fail_callback next ~failure:foreign_failure);
                rejected (Completion.join_retirement next));
              [%test_eq: string list] before (Physical.frames fixture.physical);
              assert (
                Option.is_none
                  (Actor.claim_idle_moderator fixture.physical.actor |> protocol_ok));
              (match Completion.capture first drain with
               | Error error -> [%test_eq: P.Error.code] Conflict error.code
               | Ok () -> failwith "old completion consumed replacement claim");
              Completion.cancel
                first
                ~failure:
                  (P.Error.create
                     Interrupted
                     ~message:"old completed caller"
                     ~retryable:false
                     ());
              Completion.join_retirement first |> protocol_ok;
              [%test_eq: string list] before (Physical.frames fixture.physical);
              assert (
                Option.is_none
                  (Actor.claim_idle_moderator fixture.physical.actor |> protocol_ok)));
         Fixture.with_completion fixture ~capture:(fun _ _ -> ()) (fun _fresh () -> ()));
    [%test_eq: int] 0 !(fixture.providers));
  print_endline "same captured callback once; old completion cannot release new claim";
  [%expect {| same captured callback once; old completion cannot release new claim |}]
;;

module Process_gate = struct
  type t =
    { mutable executable : string option
    ; mutable cache : string option
    ; mutable fingerprint_opens : int
    ; mutable cache_renames : int
    ; entered : unit Eio.Promise.t
    ; enter : unit Eio.Promise.u
    ; released : unit Eio.Promise.t
    ; release : unit Eio.Promise.u
    }

  let create () =
    let entered, enter = Eio.Promise.create () in
    let released, release = Eio.Promise.create () in
    { executable = None
    ; cache = None
    ; fingerprint_opens = 0
    ; cache_renames = 0
    ; entered
    ; enter
    ; released
    ; release
    }
  ;;

  let release t =
    match Eio.Promise.peek t.released with
    | None -> Eio.Promise.resolve t.release ()
    | Some () -> ()
  ;;

  let wrap t env =
    let env =
      Crash_fault_io.wrap
        env
        ~boundary:Before_open_in
        ~matches:(fun path -> Option.exists t.executable ~f:(String.equal path))
        ~reached:(fun _ ->
          t.fingerprint_opens <- t.fingerprint_opens + 1;
          (* Resolver.resolve fingerprints on every Process.run; verification
             can open again. Pause only its first actual postconstruction open. *)
          if Int.equal t.fingerprint_opens 1
          then (
            Eio.Promise.resolve t.enter ();
            Eio.Promise.await t.released))
    in
    Crash_fault_io.wrap
      env
      ~boundary:After_rename
      ~matches:(fun path ->
        Option.exists t.cache ~f:(fun cache ->
          String.equal (Filename.dirname cache) (Filename.dirname path)
          && String.is_prefix (Filename.basename path) ~prefix:".cache.bin."
          && String.is_suffix path ~suffix:".tmp"))
      ~reached:(fun path ->
        [%test_eq: string] (Option.value_exn t.cache) path;
        t.cache_renames <- t.cache_renames + 1)
  ;;
end

let process_source ~executable =
  sprintf
    {|<config model="fixture-model"/>
<developer>Actual ordinary legacy process callback.</developer>
<shell_access id="legacy-process" cwd="${workspace}">
  <capabilities sandbox="direct_unsafe" network="false"
      child_processes="false" arbitrary_code="false" privilege_change="false">
    <read path="${workspace}"/>
  </capabilities>
  <resolver allow_relative_search_path="false">
    <executable id="cat" path="%s" trusted="true"/>
  </resolver>
  <backends merge="replace"><direct when="macos"/><direct when="linux"/></backends>
  <policy default="deny">
    <rule id="actual-cat-only" action="allow"><resolved_path value="%s"/></rule>
  </policy>
  <approvals provider="none" unavailable="deny" scopes="once"/>
  <audit format="jsonl" path="${session_dir}/legacy-process.jsonl"
      content="full" failure="deny_start"/>
</shell_access>
<moderator_runtime shell_runtime="legacy-process"/>
<script language="chatml" kind="moderator">
type state = int
type event = [ `Session_start | `Session_resume | `Tick ]
let initial_state = 0
let on_event : context -> state -> event -> state task =
  fun ctx state event -> match event with
  | `Session_start -> Task.pure(state)
  | `Session_resume -> Task.pure(state)
  | `Tick -> Task.bind(Process.run("cat", ["legacy-capture-gate.txt"]),
      fun output -> if output == "actual retained native process"
        then Task.pure(state + 1)
        else Task.fail("actual process returned an unexpected marker"))
</script>|}
    executable
    executable
;;

let%expect_test "cancelled paired legacy process capture holds source close until exit" =
  let gate = Process_gate.create () in
  let executable = Eio_main.run (fun _ -> Eio_posix.Low_level.realpath "/bin/cat") in
  Fixture.with_runtime
    ~source:(process_source ~executable)
    ~wrap_env:(Process_gate.wrap gate)
    (fun fixture ->
       let read =
         Filename.concat
           (Physical.state fixture.physical).spec.workspace_instance.canonical_root
             .native_path
           "legacy-capture-gate.txt"
       in
       Eio.Path.save
         ~create:(`Exclusive 0o600)
         Eio.Path.(Eio.Stdenv.fs fixture.env / read)
         "actual retained native process";
       gate.executable <- Some executable;
       gate.cache
       <- Some
            (Filename.concat
               (Agent_store.Session_store.Handle.cache_directory fixture.physical.handle)
               "cache.bin");
       Fixture.enqueue fixture (Variant ("Tick", []));
       let before = Physical.state fixture.physical in
       let checkpoint = before.moderator in
       let sentinel = Failure "579 exact paired capture caller cancelled" in
       let negative =
         P.Error.create
           Interrupted
           ~message:"579 actual paired host cancelled before checkpoint"
           ~retryable:false
           ()
       in
       let caller, signal_caller = Eio.Promise.create () in
       let holder, signal_holder = Eio.Promise.create () in
       let callback = ref None in
       let capture = ref None in
       let task =
         Eio.Fiber.fork_promise ~sw:fixture.sw (fun () ->
           Result.try_with (fun () ->
             Eio.Cancel.sub (fun context ->
               Eio.Promise.resolve signal_caller context;
               Fixture.with_completion
                 fixture
                 ~capture:(fun completion history ->
                   Eio.Promise.resolve signal_holder completion;
                   let drain = fixture.runtime.drain_internal_events history in
                   callback := Some drain;
                   match drain with
                   | Error failure ->
                     ignore
                       (Completion.fail_callback completion ~failure
                        : (unit, P.Error.t) result);
                     protocol_ok (Error failure)
                   | Ok drain ->
                     let result = Completion.capture completion drain in
                     capture := Some result;
                     ())
                 (fun _ () ->
                    Eio.Fiber.check ();
                    failwith "cancelled actual host returned from its finite capture"))))
       in
       protect_cleanup
         ~finally:(fun () -> Process_gate.release gate)
         (fun () ->
            within fixture.env "actual process fingerprint entered" (fun () ->
              Eio.Fiber.first
                (fun () -> Eio.Promise.await gate.entered)
                (fun () ->
                   match Eio.Promise.await_exn task with
                   | Error exn -> raise exn
                   | Ok () ->
                     failwith "actual paired host completed before process fingerprint"));
            let completion = Eio.Promise.await holder in
            let foreign_failure =
              P.Error.create
                Conflict
                ~message:"foreign domain must not cancel this actual capture"
                ~retryable:false
                ()
            in
            (* Neither foreign unit API may install a negative or the exit latch.
               The existing blocked-close and exact negative assertions below
               observe both obligations through the real in-flight callback. *)
            Eio.Domain_manager.run (Eio.Stdenv.domain_mgr fixture.env) (fun () ->
              Completion.cancel completion ~failure:foreign_failure;
              Completion.leave_capture completion);
            Eio.Cancel.cancel (Eio.Promise.await caller) sentinel;
            (* The actual paired host sets its negative latch before releasing
               fingerprint IO. Worker retirement alone does not prove that order. *)
            Completion.cancel completion ~failure:negative;
            let retired =
              Eio.Fiber.fork_promise ~sw:fixture.sw (fun () ->
                Completion.join_retirement completion)
            in
            let closed =
              Eio.Fiber.fork_promise ~sw:fixture.sw (fun () ->
                Result.try_with (fun () -> Owner.close_and_wait fixture.owner))
            in
            within fixture.env "actual source admission closed" (fun () ->
              let rec await_closed () =
                match Owner.ensure_loaded fixture.owner with
                | Ok () ->
                  Eio.Fiber.yield ();
                  await_closed ()
                | Error error ->
                  [%test_eq: P.Error.code] Server_shutting_down error.code;
                  [%test_eq: string] "session runtime owner is closed" error.message
              in
              await_closed ());
            assert (Option.is_none (Eio.Promise.peek task));
            assert (Option.is_none (Eio.Promise.peek retired));
            assert (Option.is_none (Eio.Promise.peek closed));
            [%test_eq: int] 1 gate.fingerprint_opens;
            [%test_eq: int] 0 gate.cache_renames;
            assert (Option.is_none !callback && Option.is_none !capture);
            Process_gate.release gate;
            within fixture.env "actual capture exit and source close" (fun () ->
              (match Eio.Promise.await_exn task with
               | Error (Eio.Cancel.Cancelled cause) -> assert (phys_equal cause sentinel)
               | Error exn -> raise exn
               | Ok () -> failwith "original paired host cancellation was lost");
              Eio.Promise.await_exn retired |> protocol_ok;
              match Eio.Promise.await_exn closed with
              | Ok () -> ()
              | Error exn -> raise exn);
            let drain = Option.value_exn !callback |> protocol_ok in
            let completed =
              A.Moderator_checkpoint.decode drain.moderator_snapshot
              |> protocol_ok
              |> Option.value_exn
            in
            assert (List.is_empty completed.queued_internal_events);
            (match completed.current_state with
             | Session.Snapshot.Int 1 -> ()
             | _ -> failwith "actual process marker callback did not finish once");
            (match Option.value_exn !capture with
             | Error actual ->
               [%test_eq: P.Error.code] negative.code actual.code;
               [%test_eq: string] negative.message actual.message
             | Ok () -> failwith "cancelled paired checkpoint was acknowledged");
            let state = Physical.state fixture.physical in
            assert (Option.equal Jsonaf.exactly_equal checkpoint state.moderator);
            let marks ledger =
              A.Canonical_usage_fold.Marks.of_ledger ledger
              |> Result.map_error ~f:(fun error ->
                Sexp.to_string_hum (A.Canonical_usage_fold.Error.sexp_of_t error))
              |> Result.ok_or_failwith
            in
            assert (
              A.Canonical_usage_fold.Marks.equal
                (marks before.inference_ledger)
                (marks state.inference_ledger));
            let retained =
              A.Moderator_checkpoint.decode state.moderator
              |> protocol_ok
              |> Option.value_exn
            in
            (match retained.current_state with
             | Session.Snapshot.Int 0 -> ()
             | _ -> failwith "unacknowledged callback replaced persisted state");
            [%test_eq: int] 1 (List.length retained.queued_internal_events);
            assert (Option.is_none state.active_operation && Option.is_none state.failure);
            [%test_eq: int] 0 !(fixture.providers);
            [%test_eq: int] 1 gate.cache_renames;
            assert (not (Owner.is_loaded fixture.owner));
            let audit =
              Eio.Path.load
                Eio.Path.(
                  Eio.Stdenv.fs fixture.env
                  / Agent_store.Session_store.Handle.directory fixture.physical.handle
                  / "legacy-process.jsonl")
              |> String.split_lines
              |> List.filter ~f:(Fn.non String.is_empty)
              |> List.map ~f:Jsonaf.of_string
            in
            let event_name json =
              Jsonaf.member "event" json |> Option.value_exn |> Jsonaf.string_exn
            in
            let starts =
              List.filter audit ~f:(fun json -> String.equal (event_name json) "started")
            in
            let finished =
              List.filter audit ~f:(fun json -> String.equal (event_name json) "finished")
            in
            [%test_eq: int] 1 (List.length starts);
            let completed =
              match finished with
              | [ completed ] -> completed
              | [] | _ :: _ :: _ -> failwith "actual process did not have one completion"
            in
            [%test_eq: string]
              "exited"
              (Jsonaf.member "exit_kind" completed
               |> Option.value_exn
               |> Jsonaf.string_exn);
            [%test_eq: int]
              0
              (Jsonaf.member "exit_code" completed |> Option.value_exn |> Jsonaf.int_exn)));
  assert (gate.fingerprint_opens > 0);
  [%test_eq: int] 1 gate.cache_renames;
  print_endline
    "real process callback finishes once; negative checkpoint stays queued; source \
     closes once after capture exit";
  [%expect
    {| real process callback finishes once; negative checkpoint stays queued; source closes once after capture exit |}]
;;
