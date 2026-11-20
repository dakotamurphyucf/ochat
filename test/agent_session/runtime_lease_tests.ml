open Core
open Fixtures
open Job_fixtures
module Owner = Agent_server.Runtime_owner
module Builder = Agent_session.Runtime_builder

type source_stage_fixture =
  { actor : Agent_session.Session_actor.t
  ; writer_id : Agent_protocol.Id.Attachment.t
  ; environment : Eio_unix.Stdenv.base * Agent_session.Workspace_instance.t
  ; source : Builder.Source.t
  ; proof : Operator_authorization.t
  ; guards : int ref
  ; policies : int ref
  ; guard : (unit -> bool) ref
  ; reject : bool ref
  ; backend : Agent_session.Memory_backend.t
  }

let with_source_stage_actor ?environment ?(source_name = "staged-source") f =
  let with_workspace f =
    match environment with
    | None -> with_actor_workspace f
    | Some (env, workspace) -> f env workspace
  in
  with_workspace (fun env workspace ->
    let root = Eio.Path.(Eio.Stdenv.fs env / workspace.canonical_root.native_path) in
    let source_root = Eio.Path.(root / source_name) in
    Eio.Path.mkdir ~perm:0o700 source_root;
    let root_file = Eio.Path.(source_root / "root.chatmd") in
    Eio.Path.save
      ~create:(`Exclusive 0o600)
      root_file
      {|<script id="staged" language="chatml" kind="moderator">let initial_state = fail("source-stage initializer ran")
let on_event = fun event state -> Task.pure(state)</script>|};
    let definition =
      Agent_session.Prompt_definition.create
        ~root_invocation:Ui
        ~id:(Agent_protocol.Id.Prompt_definition.of_string "prd_staged" |> protocol_ok)
        ~config_name:"staged"
        ~root_file:(Eio.Path.native_exn root_file)
        ~allowed_workspaces:[]
        ~permission_profile:"interactive"
        ~runtime_policy:None
        ~enabled:true
        ~description:None
      |> store_ok
    in
    let artifacts =
      Agent_store.Prompt_artifact_store.create
        ~env
        ~root:(Eio.Path.native_exn Eio.Path.(root / "artifacts"))
      |> store_ok
    in
    let revision =
      match
        Agent_session.Prompt_revision_builder.build
          ~env
          ~artifact_store:artifacts
          ~transaction_id
          ~created_at:timestamp
          definition
      with
      | Ok revision -> revision
      | Error diagnostics ->
        raise_s
          [%sexp (diagnostics : Agent_session.Prompt_revision_builder.Diagnostic.t list)]
    in
    let source =
      Builder.Source.prepare
        revision
        ~script_preparation:(Some (script_preparation ()))
        ~invocation:Ui
        ~env
        ~checkpoint:Eio.Fiber.check
      |> protocol_ok
    in
    Eio.Path.unlink root_file;
    let reject = ref false in
    with_actor
      ~environment:(env, workspace)
      ~start:false
      ~reject_save:(fun _ -> !reject)
      ~prepare_state:(fun state ->
        { state with
          spec =
            { state.spec with
              prompt_definition_id = Some definition.id
            ; prompt_revision_id = Agent_session.Prompt_revision.id revision
            }
        })
      (fun _env _sw actor writer backend ->
         let guards = ref 0
         and policies = ref 0
         and guard = ref (fun () -> true) in
         let principal =
           Agent_protocol.Principal.create
             ~id:principal_id
             ~authentication_kind:"local"
             ~scopes:(Agent_protocol.Scope.Set.of_list [ Send_messages ])
             ~attributes:[]
           |> protocol_ok
         in
         let proof =
           Operator_authorization.guarded ~principal ~is_current:(fun () ->
             Int.incr guards;
             !guard ())
         in
         f
           { actor
           ; writer_id = writer.id
           ; environment = env, workspace
           ; source
           ; proof
           ; guards
           ; policies
           ; guard
           ; reject
           ; backend
           }))
;;

let source_stage_preparation fixture key =
  let module A = Agent_session.Session_actor in
  let state = A.state fixture.actor |> protocol_ok in
  let request =
    Agent_session.Run_admission_request.Headless.create
      ~session_id:state.identity.session_id
      ~generation:state.identity.generation
      ~expected_revision:state.counters.revision
      ~mode:Single_turn
      ~input:Authored_start
      ~key:(Agent_protocol.Idempotency_key.of_string key |> protocol_ok)
    |> protocol_ok
  in
  match
    A.begin_headless_run_preparation
      fixture.actor
      ~actor:fixture.proof
      ~authorize:(fun _ ->
        Int.incr fixture.policies;
        Ok ())
      ~request
    |> protocol_ok
  with
  | Prepare preparation -> preparation
  | Retained _ -> failwith "source-stage fixture unexpectedly retained an admission"
;;

let runtime ?script_tools ?check_execution ?activity ~close () : Builder.t =
  let inference = Inference_ports.create ~config:Chat_response.Config.default () in
  { worker =
      Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input:_ _ ->
        failwith "unexpected model operation")
  ; inference_execution = Inference_ports.execution inference
  ; now = Agent_protocol.Timestamp.now
  ; parse_prepared_input =
      (fun ~execution:_ ~tracking:_ ->
        Error
          (Agent_protocol.Error.create
             Invalid_state
             ~message:"fixture has no held input parser"
             ~retryable:false
             ()))
  ; parse_user_content = (fun ?asset_capture:_ ~id:_ _ -> failwith "unexpected input")
  ; initial_history = []
  ; initial_prompt_entry_count = 0
  ; reserved_history_through = 0
  ; moderator_snapshot = (fun () -> None)
  ; moderator_manager = None
  ; model_recipes = (fun _ -> Core.String.Map.empty)
  ; moderator_tools = []
  ; moderator_script_tools = script_tools
  ; standalone_completion = None
  ; background_executor = None
  ; idle_notifications = None
  ; automatic_turn_policy = None
  ; check_execution
  ; check_ancestry = None
  ; ancestor_capabilities = None
  ; retained_ancestor_sources = None
  ; source_lease = Agent_session.Prompt_source_lease.legacy
  ; activity
  ; product_callbacks = None
  ; unmoderated_source = None
  ; native_runtime = None
  ; moderator_activation = None
  ; authored_start =
      (fun () ->
        Error
          (Agent_protocol.Error.create
             Invalid_state
             ~message:"fixture has no authored startup"
             ~retryable:false
             ()))
  ; runtime_resume =
      (fun () ->
        Error
          (Agent_protocol.Error.create
             Invalid_state
             ~message:"fixture has no runtime resume"
             ~retryable:false
             ()))
  ; start_moderator = (fun () -> Ok None)
  ; enqueue_internal_event = (fun ?prepare:_ _ -> failwith "unexpected event")
  ; legacy_events = None
  ; drain_internal_events = (fun _ -> failwith "unexpected event drain")
  ; execute_model_job =
      (fun ~allocate_inference:_
        ~inference_context:_
        ~capture_recipe_target:_
        ~recipe:_
        ~payload:_ -> failwith "unexpected model job")
  ; enqueue_model_job_completion = (fun ?prepare:_ _ -> failwith "unexpected completion")
  ; close = Builder.Close.legacy close
  }
;;

module Canonical_delegated_source = struct
  module A = Agent_session
  module P = Agent_protocol
  module S = Agent_store.Session_store
  module D = Agent_store.Delegation_store
  module G = A.Generated_definition
  module C = Chat_response.Tool_capability

  type t =
    { env : Eio_unix.Stdenv.base
    ; sw : Eio.Switch.t
    ; actor : A.Session_actor.t
    ; source_owner : A.Runtime_source_owner.t
    ; source : Builder.Source.t
    }

  let capture f =
    match f () with
    | value -> Ok value
    | exception exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
  ;;

  let protect ~finally f =
    let result = capture f in
    let cleanup = Eio.Cancel.protect (fun () -> capture finally) in
    match result, cleanup with
    | Error (exn, backtrace), cleanup ->
      (match cleanup with
       | Ok () -> ()
       | Error (secondary, secondary_backtrace) ->
         (try
            eprintf
              "canonical delegated fixture cleanup: %s\n%s\n%!"
              (Exn.to_string secondary)
              (Stdlib.Printexc.raw_backtrace_to_string secondary_backtrace)
          with
          | _ -> ()));
      Exn.raise_with_original_backtrace exn backtrace
    | Ok _, Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
    | Ok value, Ok () -> value
  ;;

  let definition_ok = function
    | Ok value -> value
    | Error diagnostics ->
      failwith
        (String.concat
           ~sep:"; "
           (List.map diagnostics ~f:Chatmd_shell_spec.Diagnostic.to_string))
  ;;

  (* The full canonical projection is valid only for this deliberately empty
     jobs/schedules fixture, as in recurrence_runtime.Support. *)
  let index_entry (state : A.Session_state.t) =
    assert (List.is_empty state.jobs && List.is_empty state.schedules);
    Agent_store.Session_index.Entry.
      { session = A.Session_state.summary state
      ; runnable_job_count = 0
      ; deliverable_job_count = 0
      ; earliest_schedule_due = None
      ; owner_grace_deadline = None
      ; pending_initial_start = state.pending_initial_start
      ; archived = false
      ; lifecycle_revision = P.Session_lifecycle.Revision.zero
      ; admission = Automatic
      }
  ;;

  let metadata (state : A.Session_state.t) =
    S.Metadata.
      { schema_version = S.current_metadata_schema_version
      ; session = A.Session_state.summary state
      ; prompt_artifact = P.Id.Prompt_revision.to_string state.spec.prompt_revision_id
      ; workspace_identity = state.spec.workspace_instance.conflict_domain
      ; data_schema_version = A.Session_state.current_schema_version
      }
  ;;

  module Prepared_child = struct
    type t =
      { definition : G.t
      ; reservation : D.record
      ; initial : A.Session_state.t
      }
  end

  module Canonical_child = struct
    type t =
      { handle : S.Handle.t
      ; writer : Agent_store.Commit_writer.t
      ; durable : A.Session_persistence.t
      ; child : D.record
      }
  end

  let prepare_child store ~env ~workspace_instance =
    let root = workspace_instance.A.Workspace_instance.canonical_root.native_path in
    let artifact_store =
      Agent_store.Prompt_artifact_store.create
        ~env
        ~root:(Agent_store.Data_root.prompt_artifacts_path (S.data_root store))
      |> store_ok
    in
    let capabilities =
      C.create
        ~owner:"canonical-delegated-source"
        ~resource_fingerprint:
          (Chatmd_shell_spec.Source_ref.digest "canonical empty registry")
        []
      |> function
      | Ok capabilities -> capabilities
      | Error error -> raise_s [%sexp (error : C.error)]
    in
    let bundle =
      Chatmd_source_bundle.create
        ~root_file:"child.chatmd"
        ~sources:
          [ ( "child.chatmd"
            , {|<config model="fixture-model"/><developer>Canonical cleanup source.</developer>
<script id="cleanup" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = fail("canonical source fixture must not run Init")
let on_event ctx state event = Task.pure(state)
</script>|}
            )
          ]
        ()
      |> Result.ok_or_failwith
    in
    let definition =
      G.prepare
        ~env
        ~dir:Eio.Path.(Eio.Stdenv.fs env / root)
        ~revision_id:(P.Id.Prompt_revision.create ())
        ~created_at:timestamp
        ~current_capabilities:(fun () -> capabilities)
        ~references:(C.references capabilities)
        bundle
      |> definition_ok
    in
    let artifact = G.artifact definition in
    let ledger = S.delegations store in
    (* Real generated metadata, but this remains lower-trusted host DATA;
             it does not claim authenticated Factory birth or Linked admission. *)
    let reservation =
      D.reserve
        ledger
        ~key:
          { parent_session_id = second_session_id
          ; parent_generation = 0
          ; principal_id
          ; idempotency_key =
              P.Idempotency_key.of_string "canonical-cleanup-child" |> protocol_ok
          }
        ~request_sha256:
          (Chatmd_shell_spec.Source_ref.digest "canonical cleanup child DATA")
        ~admission:
          { child_session_id = session_id
          ; revision_id = artifact.revision_id
          ; transaction_id = P.Id.Transaction.create ()
          ; manifest_sha256 = artifact.manifest_sha256
          ; parent_revision_id = prompt_revision_id
          ; parent_stop_epoch = None
          ; authored_tool = None
          ; goal_origin = Some Agent_store.Delegation_goal_origin.unrelated
          ; captured_recurrence = None
          ; authority_sha256 = Chatmd_shell_spec.Source_ref.digest "canonical host DATA"
          ; capability_pins = G.capability_pins definition
          ; lifetime = Owned
          ; created_at = timestamp
          ; inference_target = Some (delegation_inference_target ())
          }
        ~max_records:8
        ~max_bytes:1048576
      |> store_ok
      |> function
      | D.New record -> record
      | Replay _ | Conflict _ -> failwith "expected a fresh canonical child"
    in
    let installed =
      G.install_reserved ~delegations:ledger ~reservation ~artifact_store definition
      |> definition_ok
    in
    let template =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let history, next = G.initial_history definition ~session_id |> protocol_ok in
    let target = Option.value_exn installed.admission.inference_target in
    let inference_target =
      match Inference.Selection.captured target ~limits:document_limits with
      | Ok selected -> selected
      | Error error -> raise_s [%sexp (error : Inference.Request.Error.t)]
    in
    let initial =
      A.Session_state.create
        ~identity:template.identity
        ~spec:
          { template.spec with
            protocol =
              { template.spec.protocol with prompt = Generated artifact.revision_id }
          ; delegation = Some (D.reference installed)
          ; prompt_revision_id = artifact.revision_id
          ; root_invocation = Ui
          ; inference_target
          }
        ~initial_history:(List.map history ~f:A.History_codec.to_protocol)
    in
    (* A trusted restored Ready state, not Factory birth/Init evidence. *)
    let initial =
      { initial with
        runtime_initialization = Ready
      ; lifecycle = { desired = Stopped; observed = Stopped }
      ; pending_initial_start = false
      ; parent_stop_epoch = Some 0L
      ; conversation =
          { initial.conversation with
            next_history_sequence = Int64.of_int next
          ; reserved_history_through = Int64.of_int next
          }
      }
      |> initialize_new_unrelated_child ~server_id:(S.server_id store)
    in
    A.Session_state.validate initial |> protocol_ok;
    { Prepared_child.definition; reservation = installed; initial }
  ;;

  let with_canonical_child store ~env ~sw (prepared : Prepared_child.t) f =
    let initial = prepared.initial in
    let installed = prepared.reservation in
    let ledger = S.delegations store in
    let handle =
      S.create_session_initialized
        store
        ~sw
        ~transaction_id:installed.admission.transaction_id
        ~actor_lock_nonce:"canonical-cleanup-child"
        ~initialize:(fun ~staging_directory:_ ->
          S.Initial_projection.create
            ~metadata:(metadata initial)
            ~entry:(index_entry initial))
      |> store_ok
    in
    protect
      ~finally:(fun () -> S.close_session store handle |> store_ok)
      (fun () ->
         let child = D.advance ledger installed Child_installed |> store_ok in
         assert (D.equal_stage child.stage Child_installed);
         let journal =
           Agent_store.Journal.create
             ~env
             ~directory:(Filename.concat (S.Handle.directory handle) "runtime-journal")
             ~max_payload_length:1048576
             ~max_segment_bytes:4194304L
             ~max_segment_frames:64
           |> store_ok
         in
         let writer =
           Agent_store.Commit_writer.create
             ~sw
             ~journal
             ~session_id
             ~next_transaction_sequence:1L
             ~previous_transaction_hash:None
             ~queue_capacity:8
           |> store_ok
         in
         protect
           ~finally:(fun () -> Agent_store.Commit_writer.close writer)
           (fun () ->
              let durable =
                A.Session_persistence.create
                  ~archive:(fun _ _ -> failwith "unexpected canonical child archive")
                  ~pending_archive:None
                  ~before_commit:
                    (Some
                       (fun state ->
                         S.prepare_canonical_projection
                           store
                           handle
                           ~metadata:(metadata state)
                           ~entry:(index_entry state)
                         |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error))
                  ~command_accepted:(fun _ _ -> ())
                  ~writer
                  ~durability:Flush
                  ~limits:document_limits
                  ~archive_limits:document_limits
                  ~retention_preflight:None
                  ~restored:(A.Session_persistence.Restored.authored initial)
                  ~previous_transaction_hash:None
              in
              f { Canonical_child.handle; writer; durable; child }))
  ;;

  let with_source_actor
        store
        ~env
        ~sw
        (prepared : Prepared_child.t)
        (canonical : Canonical_child.t)
        f
    =
    let initial = prepared.initial in
    let definition = prepared.definition in
    let artifact = G.artifact definition in
    let handle = canonical.handle in
    let writer = canonical.writer in
    let durable = canonical.durable in
    let child = canonical.child in
    let source_owner = A.Runtime_source_owner.create () in
    let actor =
      A.Session_actor.create_with_runtime_source_owner
        ~runtime_source_owner:(A.Runtime_source_owner.identity source_owner)
        ~ordinary_execution_admission:(fun () -> Ok ())
        ~schedule_permission_timeouts:false
        ~owner_lease_duration_ms:60_000
        ~max_attachments:4
        ~subscriber_capacity:8
        ~sw
        ~clock:(Eio.Stdenv.clock env)
        ~mailbox_capacity:32
        ~compaction_env:None
        ~initial_state:initial
        ~operation_worker:None
        ~persistence:
          { archive_reference
          ; commit =
              (fun ?observation ~command_audit ~previous transition ->
                A.Session_persistence.commit
                  durable
                  ?observation
                  ~command_audit
                  ~previous
                  transition)
          }
        ~services:
          { now = (fun () -> timestamp)
          ; monotonic_now = (fun () -> Mtime.min_stamp)
          ; create_attachment_id = P.Id.Attachment.create
          ; create_reclaim_token = (fun () -> "canonical-cleanup-child")
          ; state_committed =
              (fun state _ ->
                S.write_metadata store handle ~entry:(index_entry state) (metadata state)
                |> store_ok)
          ; job_results = None
          ; goal_descendant_profile
          ; subscription_limits = A.Staged_subscriptions.default_limits
          ; schedule_limits = A.Staged_schedules.default_limits
          ; notification_limits = A.Staged_notifications.default_limits
          ; ingress_limits = A.Staged_ingress.default_limits
          }
    in
    protect
      ~finally:(fun () -> A.Session_actor.shutdown actor)
      (fun () ->
         let controller =
           A.Recurrence_controller.create
             store
             ~handle
             ~writer
             ~entry_projection:index_entry
           |> protocol_ok
         in
         A.Session_actor.set_recurrence_controller actor controller |> protocol_ok;
         let state = A.Session_actor.state actor |> protocol_ok in
         let observed = A.Recurrence_controller.observe controller state |> protocol_ok in
         assert (A.Recurrence_controller.is_current observed);
         assert (
           A.Runtime_source_owner.matches
             source_owner
             (A.Session_actor.runtime_source_owner_identity actor |> Option.value_exn));
         assert (
           Option.exists state.spec.delegation ~f:(D.Reference.equal (D.reference child)));
         [%test_eq: P.Id.Prompt_revision.t]
           artifact.revision_id
           state.spec.prompt_revision_id;
         (match state.runtime_initialization, state.lifecycle.desired with
          | Ready, Stopped -> ()
          | (Ready | Pending _), (Running | Stopped) ->
            failwith "canonical child is not restored Ready/Stopped");
         assert (Option.is_none state.run_state && Option.is_none state.moderator);
         assert (List.is_empty state.jobs && List.is_empty state.schedules);
         A.Session_actor.check_runtime_admission actor |> protocol_ok;
         let source =
           Builder.Source.prepare_generated definition ~invocation:Ui |> protocol_ok
         in
         Builder.Source.validate_target source state |> protocol_ok;
         f { env; sw; actor; source_owner; source })
  ;;

  let with_fixture f =
    with_actor_workspace (fun env workspace_instance ->
      Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
        Eio.Switch.run (fun sw ->
          let store =
            S.create
              ~env
              ~sw
              ~root:
                (Filename.concat
                   workspace_instance.canonical_root.native_path
                   "canonical-delegated-store")
              ~server_id:(P.Id.Server.create ())
              ~process_start_identity:None
              ~lock_nonce:"canonical-delegated-source"
            |> store_ok
          in
          protect
            ~finally:(fun () -> S.close store |> store_ok)
            (fun () ->
               let prepared = prepare_child store ~env ~workspace_instance in
               with_canonical_child store ~env ~sw prepared (fun canonical ->
                 with_source_actor store ~env ~sw prepared canonical f)))))
  ;;
end

let%expect_test "failed installed source prefix retains its original delegated End" =
  let module A = Agent_session.Session_actor in
  let module Scope = Owner.Scope_outcome in
  let module R = Owner.Cleanup_recovery in
  let module CO = Agent_session.Close_observation in
  let module D = Agent_session.Cleanup_diagnostic in
  let run observed =
    Canonical_delegated_source.with_fixture (fun fixture ->
      let close_failure = Failure "actual installed source prefix close" in
      let reject_close = ref true in
      let closes = ref 0
      and builds = ref 0 in
      let raw =
        runtime
          ~close:(fun () ->
            Int.incr closes;
            if !reject_close then raise close_failure)
          ()
      in
      let captured = ref None in
      let owner =
        Owner.create_with_deferred_run_unload
          ~source_owner:fixture.source_owner
          ~before_unload:(fun ~closing:_ -> Ok ())
          ~policy_resource_cleanup:
            (Owner.Policy_resource_cleanup.create
               ~pending:(fun () -> false)
               ~close_pending:(fun () -> Ok ()))
          ~policy_resource_access:None
          ~native_resource_access:None
          ~actor:fixture.actor
          ~initial:None
          ~build:(fun ~initialization_cleanup:_ ~original_actor:_ _ ->
            failwith "ordinary source build ran")
          ~build_deferred:
            (fun
              ~initialization_cleanup:_
              ~source_consumer
              ~source
              ~expected_product
              ~construction ->
            Int.incr builds;
            Owner.Source_consumer.check source_consumer |> protocol_ok;
            assert (
              phys_equal
                (Owner.Source_consumer.cleanup source_consumer)
                (Option.value_exn !captured));
            assert (phys_equal source fixture.source);
            assert (Option.is_none expected_product);
            (match A.Runtime_construction.initialization construction with
             | Installed_resume -> ()
             | Scoped -> failwith "restored Ready source selected Init");
            A.Runtime_construction.check_basis construction |> protocol_ok;
            Ok (Owner.Construction_result.Built (Builder.Attachment.cold raw)))
          ~ordinary_warm:None
          ~delegated_load:None
      in
      let admitted_probe = ref None in
      let end_probe () =
        Option.iter !admitted_probe ~f:(fun preparation ->
          A.end_delegated_load_preparation fixture.actor ~preparation |> protocol_ok;
          admitted_probe := None)
      in
      let close_owner () =
        (* A failed unload can deliver its original failure before a later explicit
         close retries the exact resource. Never swallow a different exception. *)
        match Owner.close_and_wait owner with
        | () -> ()
        | exception exn when phys_equal exn close_failure -> Owner.close_and_wait owner
      in
      let repair_cleanup () =
        Option.iter !captured ~f:(fun cleanup ->
          match R.snapshot cleanup |> protocol_ok with
          | None -> ()
          | Some snapshot ->
            (match R.Snapshot.progress snapshot with
             | Joined -> ()
             | Retryable | Blocked | Terminal ->
               let repaired = R.retry cleanup ~owner ~after:snapshot |> protocol_ok in
               assert (R.Snapshot.equal_progress (R.Snapshot.progress repaired) Joined)))
      in
      Canonical_delegated_source.protect
        ~finally:(fun () ->
          reject_close := false;
          (* Release an unexpectedly admitted probe before exact original repair.
           Actor shutdown is not a receipt for the original deferred End. *)
          Canonical_delegated_source.protect ~finally:close_owner (fun () ->
            Canonical_delegated_source.protect ~finally:repair_cleanup end_probe))
        (fun () ->
           let selected_summary = ref None in
           let consumer = ref None in
           let callback source =
             let selected = Owner.Delegated_load_source.consumer source in
             consumer := Some selected;
             let cleanup = Owner.Source_consumer.cleanup selected in
             assert (Option.is_none !captured);
             captured := Some cleanup;
             assert (Option.is_none (R.original cleanup |> protocol_ok));
             assert (Option.is_none (R.snapshot cleanup |> protocol_ok));
             let prepared =
               Owner.Delegated_load_source.prepare source ~source:fixture.source
               |> protocol_ok
             in
             let loaded =
               if observed
               then
                 Owner.load_delegated_runtime_observed
                   owner
                   ~prepared
                   ~retention:Release_if_stopped
               else
                 CO.create
                   (Returned
                      (Owner.load_delegated_runtime
                         owner
                         ~prepared
                         ~retention:Release_if_stopped))
                   ~diagnostics:[]
                   ()
             in
             (match CO.exit loaded with
              | Returned (Ok summary) ->
                (* Actual ACK returned this non-unit value before source cleanup. *)
                assert (Owner.is_loaded owner);
                [%test_eq: int] 1 !builds;
                [%test_eq: int] 0 !closes;
                [%test_eq: Agent_protocol.Session.desired_state]
                  Stopped
                  summary.desired_state;
                selected_summary := Some summary
              | Returned (Error error) -> raise_s [%sexp (error : Agent_protocol.Error.t)]
              | Raised (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace);
             loaded
           in
           let report, observation =
             if observed
             then
               ( None
               , Some
                   (Owner.with_delegated_load_source_observed
                      owner
                      ~source_owner:fixture.source_owner
                      ~custody:(Host None)
                      callback) )
             else
               ( Some
                   (Owner.with_delegated_load_source_reported
                      owner
                      ~source_owner:fixture.source_owner
                      ~custody:(Host None)
                      (fun source -> CO.resume (callback source)))
               , None )
           in
           (match report with
            | Some report ->
              (match Scope.primary report with
               | Returned (Ok summary) ->
                 assert (phys_equal summary (Option.value_exn !selected_summary))
               | Returned (Error error) ->
                 raise_s [%sexp (error : Agent_protocol.Error.t)]
               | Raised (exn, backtrace) ->
                 Exn.raise_with_original_backtrace exn backtrace)
            | None -> assert (Option.is_some !selected_summary));
           let report_cleanup =
             match report with
             | Some report -> Scope.cleanup report
             | None ->
               R.original (Option.value_exn !captured) |> protocol_ok |> Option.value_exn
           in
           let original_backtrace =
             match Scope.Cleanup.status report_cleanup with
             | Failed (Raised (exn, backtrace), []) when phys_equal exn close_failure ->
               assert (
                 not (String.is_empty (Stdlib.Printexc.raw_backtrace_to_string backtrace)));
               backtrace
             | Failed (Raised (exn, backtrace), _) ->
               Exn.raise_with_original_backtrace exn backtrace
             | Failed (Protocol error, _) ->
               raise_s [%sexp (error : Agent_protocol.Error.t)]
             | Never_acquired | Joined ->
               failwith "actual installed-prefix failure was lost"
           in
           let assert_original cleanup =
             match Scope.Cleanup.status cleanup with
             | Failed (Raised (exn, backtrace), []) ->
               assert (phys_equal exn close_failure);
               assert (phys_equal backtrace original_backtrace)
             | Failed _ | Never_acquired | Joined ->
               failwith "original source cleanup changed"
           in
           let cleanup = Option.value_exn !captured in
           let original = R.original cleanup |> protocol_ok |> Option.value_exn in
           let snapshot = R.snapshot cleanup |> protocol_ok |> Option.value_exn in
           let original_view =
             R.original_view cleanup |> protocol_ok |> Option.value_exn
           in
           let original_group =
             match Agent_server.Cleanup_original_view.diagnostics original_view with
             | [ group ] -> group
             | _ ->
               failwith "source original view lost or duplicated actual prefix origin"
           in
           let assert_observation () =
             match observation with
             | None -> ()
             | Some actual ->
               (match CO.exit actual with
                | Raised (exn, backtrace) ->
                  assert (phys_equal exn close_failure);
                  (* The observed raw exit is the actual Scope.resume rethrow,
                     not the earlier flat cleanup capture. It forwards the same
                     producer and may add its own projection frames. *)
                  assert (
                    String.is_prefix
                      (Stdlib.Printexc.raw_backtrace_to_string backtrace)
                      ~prefix:(Stdlib.Printexc.raw_backtrace_to_string original_backtrace))
                | Returned _ -> failwith "observed source lost actual cleanup failure");
               assert (Option.equal D.equal (CO.primary actual) (Some original_group));
               assert (List.equal D.equal (CO.diagnostics actual) [ original_group ]);
               assert (
                 List.equal
                   D.equal
                   (Agent_server.Cleanup_original_view.diagnostics original_view)
                   [ original_group ])
           in
           assert_observation ();
           let assert_failed_snapshot snapshot =
             assert (R.Snapshot.equal_progress (R.Snapshot.progress snapshot) Retryable);
             match R.Snapshot.failures snapshot with
             | [ Raised (exn, backtrace) ] ->
               assert (phys_equal exn close_failure);
               assert (
                 not (String.is_empty (Stdlib.Printexc.raw_backtrace_to_string backtrace)))
             | _ -> failwith "source snapshot lost the actual prefix close failure"
           in
           assert_failed_snapshot snapshot;
           let snapshot_backtrace =
             match R.Snapshot.failures snapshot with
             | [ Raised (_, backtrace) ] -> backtrace
             | _ -> failwith "source snapshot lost its original raw trace"
           in
           (* The canonical snapshot retains the closer's producer trace. The flat
            source report captures its later rethrow through retirement/source
            projection, preserving that origin with additional frames. Freeze
            each immutable outcome's OWN trace rather than equating both values. *)
           let producer_trace =
             Stdlib.Printexc.raw_backtrace_to_string snapshot_backtrace
           in
           assert (not (String.is_empty producer_trace));
           assert (
             String.is_prefix
               (Stdlib.Printexc.raw_backtrace_to_string original_backtrace)
               ~prefix:producer_trace);
           let unchanged_original () =
             assert_observation ();
             assert (
               phys_equal original (R.original cleanup |> protocol_ok |> Option.value_exn));
             assert_original original;
             assert_original report_cleanup;
             assert_failed_snapshot snapshot;
             (match R.Snapshot.failures snapshot with
              | [ Raised (_, backtrace) ] ->
                assert (phys_equal backtrace snapshot_backtrace)
              | _ -> failwith "source snapshot lost its original raw trace");
             let joined_original =
               Owner.Source_consumer.join (Option.value_exn !consumer) |> protocol_ok
             in
             assert_original joined_original
           in
           let assert_end_owed () =
             (* Detach already ACKed worker=None/inference=None. The surviving
              preparation guard, not a blanket Actor admission lock, rejects. *)
             A.check_runtime_admission fixture.actor |> protocol_ok;
             match
               A.begin_delegated_load_preparation
                 fixture.actor
                 ~source_owner:fixture.source_owner
                 ~custody:(Host None)
             with
             | Error { code = Conflict; _ } -> ()
             | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
             | Ok preparation ->
               admitted_probe := Some preparation;
               Canonical_delegated_source.protect ~finally:end_probe (fun () ->
                 failwith "failed prefix discarded its unattempted original End")
           in
           let source_callbacks = ref 0 in
           let probe_source () =
             Owner.with_delegated_load_source_reported
               owner
               ~source_owner:fixture.source_owner
               ~custody:(Host None)
               (fun _source ->
                  Int.incr source_callbacks;
                  Ok ())
           in
           let assert_source_fenced () =
             let denied = probe_source () in
             (match Scope.primary denied with
              | Returned (Error { code = Conflict; retryable = true; _ }) -> ()
              | Returned (Error error) -> raise_s [%sexp (error : Agent_protocol.Error.t)]
              | Raised (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
              | Returned (Ok ()) ->
                failwith "replacement source bypassed original cleanup");
             (match Scope.Cleanup.status (Scope.cleanup denied) with
              | Never_acquired -> ()
              | Joined | Failed _ -> failwith "rejected source acquired cleanup custody");
             [%test_eq: int] 0 !source_callbacks
           in
           [%test_eq: int] 1 !builds;
           [%test_eq: int] 1 !closes;
           assert (not (Owner.is_loaded owner));
           assert_end_owed ();
           assert_source_fenced ();
           assert (
             phys_equal snapshot (R.snapshot cleanup |> protocol_ok |> Option.value_exn));
           unchanged_original ();
           (* No Begin/build/install occurs inside either exact retry. The failed
            repeat proves the prefix still precedes its same deferred End. *)
           let still_failed = R.retry cleanup ~owner ~after:snapshot |> protocol_ok in
           assert_failed_snapshot still_failed;
           let repeated_backtrace =
             match R.Snapshot.failures still_failed with
             | [ Raised (_, backtrace) ] -> backtrace
             | _ -> failwith "repeated prefix failure lost its raw trace"
           in
           [%test_eq: int] 2 !closes;
           [%test_eq: int] 1 !builds;
           assert_end_owed ();
           assert_source_fenced ();
           assert (
             phys_equal
               still_failed
               (R.snapshot cleanup |> protocol_ok |> Option.value_exn));
           unchanged_original ();
           reject_close := false;
           (* A legacy ordinary unload may close the SAME retained resource, but it
            does not own the already captured original delegated End. Physical
            success alone must not release this source's admission fence. *)
           Owner.unload_and_wait owner |> protocol_ok;
           [%test_eq: int] 3 !closes;
           [%test_eq: int] 1 !builds;
           assert (not (Owner.is_loaded owner));
           assert_end_owed ();
           assert_source_fenced ();
           assert (
             phys_equal
               still_failed
               (R.snapshot cleanup |> protocol_ok |> Option.value_exn));
           unchanged_original ();
           (* The exact recovery now finishes its deferred End without replaying
            the successful legacy close, selecting a source, or rebuilding. *)
           let joined = R.retry cleanup ~owner ~after:still_failed |> protocol_ok in
           assert (R.Snapshot.equal_progress (R.Snapshot.progress joined) Joined);
           assert (List.is_empty (R.Snapshot.failures joined));
           (match R.Snapshot.cleanup joined with
            | Some cleanup ->
              (match Scope.Cleanup.status cleanup with
               | Joined -> ()
               | Never_acquired | Failed _ ->
                 failwith "exact prefix and deferred End lack positive cleanup")
            | None -> failwith "exact prefix and deferred End have unfinished cleanup");
           assert (
             phys_equal joined (R.snapshot cleanup |> protocol_ok |> Option.value_exn));
           assert_failed_snapshot still_failed;
           (match R.Snapshot.failures still_failed with
            | [ Raised (_, backtrace) ] ->
              assert (phys_equal backtrace repeated_backtrace)
            | _ -> failwith "positive retry changed the earlier failed snapshot");
           unchanged_original ();
           [%test_eq: int] 3 !closes;
           [%test_eq: int] 1 !builds;
           let preparation =
             A.begin_delegated_load_preparation
               fixture.actor
               ~source_owner:fixture.source_owner
               ~custody:(Host None)
             |> protocol_ok
           in
           admitted_probe := Some preparation;
           end_probe ();
           if observed
           then (
             let expected_summary =
               A.state fixture.actor |> protocol_ok |> Agent_session.Session_state.summary
             in
             let positive =
               Owner.with_delegated_load_source_observed
                 owner
                 ~source_owner:fixture.source_owner
                 ~custody:(Host None)
                 (fun _source ->
                    Int.incr source_callbacks;
                    CO.create (Returned (Ok expected_summary)) ~diagnostics:[] ())
             in
             (match CO.exit positive with
              | Returned (Ok actual) -> assert (phys_equal actual expected_summary)
              | Returned (Error error) -> raise_s [%sexp (error : Agent_protocol.Error.t)]
              | Raised (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace);
             assert (List.is_empty (CO.diagnostics positive));
             assert (Option.is_none (CO.primary positive)))
           else Scope.resume (probe_source ()) |> protocol_ok;
           [%test_eq: int] 1 !source_callbacks;
           [%test_eq: int] 3 !closes;
           [%test_eq: int] 1 !builds;
           ()))
  in
  List.iter [ false; true ] ~f:run;
  print_endline
    "actual install; Actor admission open; source fenced while prefix fails; exact \
     prefix then original delegated End repaired; immutable original retained";
  [%expect
    {| actual install; Actor admission open; source fenced while prefix fails; exact prefix then original delegated End repaired; immutable original retained |}]
;;

let%expect_test "a no-work idle drain does not exclude a queued source stage" =
  let module A = Agent_session.Session_actor in
  with_source_stage_actor (fun fixture ->
    let builds = ref 0 in
    let owner =
      Owner.create ~actor:fixture.actor ~initial:None ~build:(fun () ->
        Int.incr builds;
        failwith "no-work drain or source stage loaded a runtime")
    in
    let preparation = source_stage_preparation fixture "stage-after-no-work" in
    let capture f =
      match f () with
      | value -> Ok value
      | exception exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
    in
    let result =
      capture (fun () ->
        let before = A.state fixture.actor |> protocol_ok in
        let saved = Agent_session.Memory_backend.state fixture.backend in
        let entered_stage = ref 0 in
        Eio.Switch.run (fun sw ->
          let blocked, blocked_u = Eio.Promise.create () in
          let release, release_u = Eio.Promise.create () in
          let release_checkpoint () =
            if Option.is_none (Eio.Promise.peek release)
            then Eio.Promise.resolve release_u ()
          in
          Exn.protect ~finally:release_checkpoint ~f:(fun () ->
            let checkpoint =
              Eio.Fiber.fork_promise ~sw (fun () ->
                A.checkpoint fixture.actor ~persist:(fun _ ->
                  Eio.Promise.resolve blocked_u ();
                  Eio.Promise.await release;
                  Ok ()))
            in
            Eio.Promise.await blocked;
            let draining, draining_u = Eio.Promise.create () in
            let drain =
              Eio.Fiber.fork_promise ~sw (fun () ->
                Eio.Promise.resolve draining_u ();
                Owner.drain_idle_moderator owner)
            in
            Eio.Promise.await draining;
            (* An uncontended Eio mutex acquisition does not switch fibers.
               The drain now owns the owner mutex and its State request waits
               behind the serialized checkpoint. The next caller queues on that
               mutex before the checkpoint is released. Unlock hands ownership
               to that waiter before the drain can reacquire it for cleanup. *)
            let staging, staging_u = Eio.Promise.create () in
            let stage =
              Eio.Fiber.fork_promise ~sw (fun () ->
                Eio.Promise.resolve staging_u ();
                Owner.with_prepared_source
                  owner
                  ~preparation
                  ~source:fixture.source
                  (fun prepared ->
                     let open Result.Let_syntax in
                     let%bind () = Owner.Prepared.check_basis prepared in
                     let%map () =
                       A.Source_binding.check_basis (Owner.Prepared.binding prepared)
                     in
                     Int.incr entered_stage;
                     true))
            in
            Eio.Promise.await staging;
            assert (Option.is_none (Eio.Promise.peek drain));
            assert (Option.is_none (Eio.Promise.peek stage));
            release_checkpoint ();
            Eio.Promise.await_exn checkpoint |> protocol_ok;
            let did_work = Eio.Promise.await_exn drain |> protocol_ok in
            let staged = Eio.Promise.await_exn stage |> protocol_ok in
            assert (not did_work);
            assert staged));
        [%test_eq: int] 1 !entered_stage;
        [%test_eq: int] 0 !builds;
        assert (not (Owner.is_loaded owner));
        [%test_eq: Sexp.t]
          (Agent_session.Session_state.sexp_of_t before)
          (Agent_session.Session_state.sexp_of_t (A.state fixture.actor |> protocol_ok));
        [%test_eq: Sexp.t]
          (Agent_session.Session_state.sexp_of_t saved)
          (Agent_session.Session_state.sexp_of_t
             (Agent_session.Memory_backend.state fixture.backend)))
    in
    let cleanup =
      capture (fun () ->
        Eio.Cancel.protect (fun () ->
          Exn.protect
            ~finally:(fun () -> Owner.close_and_wait owner)
            ~f:(fun () -> A.end_run_preparation fixture.actor preparation |> protocol_ok)))
    in
    (match result, cleanup with
     | Error (exn, backtrace), _ | Ok (), Error (exn, backtrace) ->
       Exn.raise_with_original_backtrace exn backtrace
     | Ok (), Ok () -> ());
    print_endline
      "no-work drain and queued source stage both finish; one staged owner; zero builds \
       or writes");
  [%expect
    {| no-work drain and queued source stage both finish; one staged owner; zero builds or writes |}]
;;

let%expect_test
    "source stages retain exact actor custody without loading or calling policy from \
     basis"
  =
  let module A = Agent_session.Session_actor in
  let module Binding = A.Source_binding in
  with_source_stage_actor (fun fixture ->
    let builds = ref 0 in
    let owner =
      Owner.create ~actor:fixture.actor ~initial:None ~build:(fun () ->
        Int.incr builds;
        failwith "source stage invoked the runtime builder")
    in
    let conflict (result : (_, Agent_protocol.Error.t) result) =
      match result with
      | Error error -> [%test_eq: Agent_protocol.Error.code] Conflict error.code
      | Ok _ -> failwith "source stage unexpectedly retained authority"
    in
    let reset_calls () =
      fixture.guards := 0;
      fixture.policies := 0
    in
    let no_calls () =
      [%test_eq: int] 0 !(fixture.guards);
      [%test_eq: int] 0 !(fixture.policies)
    in
    let with_stage key f =
      let preparation = source_stage_preparation fixture key in
      Exn.protect
        ~finally:(fun () ->
          A.end_run_preparation fixture.actor preparation |> protocol_ok)
        ~f:(fun () ->
          Owner.with_prepared_source
            owner
            ~preparation
            ~source:fixture.source
            (f preparation)
          |> protocol_ok)
    in
    Exn.protect
      ~finally:(fun () -> Owner.close_and_wait owner)
      ~f:(fun () ->
        let escaped = ref None in
        with_stage "stage-own-commit" (fun preparation prepared ->
          let binding = Owner.Prepared.binding prepared in
          reset_calls ();
          Binding.check_basis binding |> protocol_ok;
          Owner.Prepared.check_basis prepared |> protocol_ok;
          no_calls ();
          Binding.check_current binding |> protocol_ok;
          [%test_eq: int] 1 !(fixture.policies);
          assert (!(fixture.guards) > 0);
          conflict (Owner.ensure_loaded owner);
          conflict (Owner.with_loaded_runtime owner (fun _ -> failwith "borrow entered"));
          conflict
            (Owner.with_prepared_source
               owner
               ~preparation
               ~source:fixture.source
               (fun _ -> failwith "competing source stage entered"));
          [%test_eq: int] 0 !builds;
          A.reserve_run_history_block fixture.actor ~preparation ~count:1
          |> protocol_ok
          |> ignore;
          reset_calls ();
          Binding.check_basis binding |> protocol_ok;
          Owner.Prepared.check_basis prepared |> protocol_ok;
          no_calls ();
          assert (phys_equal (Binding.source binding) fixture.source);
          escaped := Some prepared;
          Ok ());
        reset_calls ();
        let ended = Option.value_exn !escaped in
        conflict (Binding.check_basis (Owner.Prepared.binding ended));
        conflict (Owner.Prepared.check_basis ended);
        no_calls ();
        print_endline
          "basis: zero callbacks; current: one policy; own ACK retained; eager loads \
           excluded";
        with_stage "stage-rejected-write" (fun _ prepared ->
          let binding = Owner.Prepared.binding prepared in
          let json state = state_document state |> Document_schema.Document.json in
          let before = A.state fixture.actor |> protocol_ok |> json in
          let saved = Agent_session.Memory_backend.state fixture.backend |> json in
          fixture.reject := true;
          let result =
            Exn.protect
              ~finally:(fun () -> fixture.reject := false)
              ~f:(fun () -> A.reserve_history_block fixture.actor ~count:1)
          in
          (match result with
           | Error error ->
             [%test_eq: Agent_protocol.Error.code] Internal_error error.code;
             [%test_eq: string] "injected job transition failure" error.message;
             [%test_eq: bool] false error.retryable
           | Ok _ -> failwith "injected write unexpectedly committed");
          assert (
            Jsonaf.exactly_equal before (A.state fixture.actor |> protocol_ok |> json));
          assert (
            Jsonaf.exactly_equal
              saved
              (Agent_session.Memory_backend.state fixture.backend |> json));
          reset_calls ();
          conflict (Binding.check_current binding);
          conflict (Owner.Prepared.check_basis prepared);
          no_calls ();
          Ok ());
        print_endline
          "rejected ordinary write: exact state retained; source custody irreversibly \
           closed";
        with_stage "stage-guard-close" (fun _ prepared ->
          let binding = Owner.Prepared.binding prepared in
          (fixture.guard
           := fun () ->
                Binding.close binding;
                true);
          Exn.protect
            ~finally:(fun () -> fixture.guard := fun () -> true)
            ~f:(fun () ->
              reset_calls ();
              conflict (Binding.check_current binding);
              [%test_eq: int] 1 !(fixture.policies);
              assert (!(fixture.guards) > 0);
              reset_calls ();
              conflict (Binding.check_basis binding);
              conflict (Owner.Prepared.check_basis prepared);
              no_calls ());
          Ok ());
        [%test_eq: int] 0 !builds;
        print_endline
          "guard closure: final basis rejects; callback exit closes lifetime; builder \
           never entered"));
  [%expect
    {|
    basis: zero callbacks; current: one policy; own ACK retained; eager loads excluded
    rejected ordinary write: exact state retained; source custody irreversibly closed
    guard closure: final basis rejects; callback exit closes lifetime; builder never entered |}]
;;

let%expect_test
    "source stage stop and retirement close custody before joining; foreign owners never \
     enter"
  =
  let module A = Agent_session.Session_actor in
  let module Binding = A.Source_binding in
  let module Failure = struct
    exception Primary
  end
  in
  with_source_stage_actor (fun fixture ->
    let builds = ref 0 in
    let barrier, barrier_u = Eio.Promise.create () in
    let release, release_u = Eio.Promise.create () in
    let release_barrier () =
      if Option.is_none (Eio.Promise.peek release) then Eio.Promise.resolve release_u ()
    in
    let owner =
      Owner.create_with_unload
        ~actor:fixture.actor
        ~initial:None
        ~build:(fun () ->
          Int.incr builds;
          failwith "source-stage retirement invoked builder")
        ~before_unload:(fun ~closing:_ ->
          if Option.is_none (Eio.Promise.peek barrier)
          then Eio.Promise.resolve barrier_u ();
          Eio.Promise.await release;
          Ok ())
    in
    let conflict (result : (_, Agent_protocol.Error.t) result) =
      match result with
      | Error error -> [%test_eq: Agent_protocol.Error.code] Conflict error.code
      | Ok _ -> failwith "closed or foreign source stage was admitted"
    in
    let with_preparation key f =
      let preparation = source_stage_preparation fixture key in
      Exn.protect
        ~finally:(fun () ->
          A.end_run_preparation fixture.actor preparation |> protocol_ok)
        ~f:(fun () -> f preparation)
    in
    Exn.protect
      ~finally:(fun () ->
        release_barrier ();
        Owner.close_and_wait owner)
      ~f:(fun () ->
        with_source_stage_actor
          ~environment:fixture.environment
          ~source_name:"foreign-source"
          (fun foreign ->
             with_preparation "stage-foreign-owner" (fun preparation ->
               let other = source_stage_preparation foreign "other-source-owner" in
               Exn.protect
                 ~finally:(fun () ->
                   A.end_run_preparation foreign.actor other |> protocol_ok)
                 ~f:(fun () ->
                   assert (
                     not
                       (Agent_protocol.Id.Prompt_revision.equal
                          (Builder.Source.artifact fixture.source).revision_id
                          (Builder.Source.artifact foreign.source).revision_id));
                   fixture.guards := 0;
                   fixture.policies := 0;
                   foreign.guards := 0;
                   foreign.policies := 0;
                   conflict
                     (Owner.with_prepared_source
                        owner
                        ~preparation:other
                        ~source:foreign.source
                        (fun _ -> failwith "foreign actor stage entered"));
                   conflict
                     (Owner.with_prepared_source
                        owner
                        ~preparation
                        ~source:foreign.source
                        (fun _ -> failwith "foreign captured source entered"));
                   List.iter [ fixture; foreign ] ~f:(fun fixture ->
                     [%test_eq: int] 0 !(fixture.guards);
                     [%test_eq: int] 0 !(fixture.policies)))));
        print_endline
          "foreign actor and captured source: rejected before original/policy callbacks";
        with_preparation "stage-primary-failure" (fun preparation ->
          let escaped = ref None in
          let primary = Failure.Primary in
          let observed =
            Result.try_with (fun () ->
              Owner.with_prepared_source
                owner
                ~preparation
                ~source:fixture.source
                (fun prepared ->
                   escaped := Some prepared;
                   raise primary))
          in
          (match observed with
           | Error observed -> assert (phys_equal observed primary)
           | Ok _ -> failwith "stage primary exception disappeared");
          let ended = Option.value_exn !escaped in
          conflict (Binding.check_basis (Owner.Prepared.binding ended));
          conflict (Owner.Prepared.check_basis ended));
        A.start fixture.actor ~attachment_id:fixture.writer_id |> protocol_ok |> ignore;
        with_preparation "stage-public-stop" (fun preparation ->
          Owner.with_prepared_source
            owner
            ~preparation
            ~source:fixture.source
            (fun prepared ->
               A.stop fixture.actor ~attachment_id:fixture.writer_id ~mode:Cancel
               |> protocol_ok
               |> ignore;
               let stopped = A.state fixture.actor |> protocol_ok in
               (match stopped.lifecycle.observed with
                | Stopped -> ()
                | observed ->
                  raise_s
                    [%sexp
                      "public Stop did not stop source-stage actor"
                    , (observed : Agent_protocol.Session.observed_state)]);
               let binding = Owner.Prepared.binding prepared in
               conflict (Binding.check_basis binding);
               conflict (Owner.Prepared.check_basis prepared);
               assert (phys_equal (Binding.source binding) fixture.source);
               Ok ())
          |> protocol_ok);
        print_endline
          "primary exception preserved; actual public Stop closes exact source custody";
        with_preparation "stage-retirement-cancel" (fun preparation ->
          Eio.Switch.run (fun sw ->
            Exn.protect ~finally:release_barrier ~f:(fun () ->
              let entered, entered_u = Eio.Promise.create () in
              let leaving, leaving_u = Eio.Promise.create () in
              let never, _ = Eio.Promise.create () in
              let caller =
                Eio.Fiber.fork_promise ~sw (fun () ->
                  Owner.with_prepared_source
                    owner
                    ~preparation
                    ~source:fixture.source
                    (fun prepared ->
                       Eio.Promise.resolve entered_u prepared;
                       Exn.protect
                         ~finally:(fun () -> Eio.Promise.resolve leaving_u ())
                         ~f:(fun () ->
                           Eio.Promise.await never;
                           Ok ())))
              in
              let prepared = Eio.Promise.await entered in
              let stopping =
                Eio.Fiber.fork_promise ~sw (fun () -> Owner.unload_and_wait owner)
              in
              Eio.Promise.await barrier;
              fixture.guards := 0;
              fixture.policies := 0;
              conflict (Binding.check_current (Owner.Prepared.binding prepared));
              conflict (Owner.Prepared.check_basis prepared);
              [%test_eq: int] 0 !(fixture.guards);
              [%test_eq: int] 0 !(fixture.policies);
              assert (Option.is_none (Eio.Promise.peek leaving));
              assert (Option.is_none (Eio.Promise.peek stopping));
              release_barrier ();
              Eio.Promise.await_exn stopping |> protocol_ok;
              (match Eio.Promise.await caller with
               | Error (Eio.Cancel.Cancelled _) -> ()
               | _ -> failwith "source-stage retirement lost cancellation");
              assert (Option.is_some (Eio.Promise.peek leaving));
              conflict (Owner.Prepared.check_basis prepared))));
        [%test_eq: int] 0 !builds;
        assert (not (Owner.is_loaded owner));
        print_endline
          "retirement: custody closed before barrier/cancellation; callback joined; no \
           runtime built"));
  [%expect
    {|
    foreign actor and captured source: rejected before original/policy callbacks
    primary exception preserved; actual public Stop closes exact source custody
    retirement: custody closed before barrier/cancellation; callback joined; no runtime built |}]
;;

let%expect_test
    "concurrent retirement callers share dependency failures and preserve resources for \
     retry"
  =
  List.iter
    (List.cartesian_product [ false; true ] [ false; true ])
    ~f:(fun (raises, closing) ->
      with_actor (fun _env sw actor _writer _backend ->
        let entered, entered_u = Eio.Promise.create () in
        let release, release_u = Eio.Promise.create () in
        let fail = ref true
        and calls = ref 0
        and closes = ref 0 in
        let failure =
          Agent_protocol.Error.create
            Persistence_error
            ~message:"descendant stop could not be saved"
            ~retryable:true
            ()
        in
        let owner =
          Owner.create_with_unload
            ~actor
            ~initial:(Some (runtime ~close:(fun () -> Int.incr closes) ()))
            ~build:(fun () -> failwith "unexpected rebuild")
            ~before_unload:(fun ~closing:_ ->
              Int.incr calls;
              match !fail with
              | false -> Ok ()
              | true ->
                if !calls = 1 then Eio.Promise.resolve entered_u ();
                Eio.Promise.await release;
                if raises then failwith "descendant cleanup exception" else Error failure)
        in
        let stop ~closing () =
          try
            Ok
              (if closing
               then (
                 Owner.close_and_wait owner;
                 Ok ())
               else Owner.unload_and_wait owner)
          with
          | Owner.Cleanup_failed error -> Ok (Error error)
          | exn -> Error exn
        in
        let first = Eio.Fiber.fork_promise ~sw (stop ~closing:false) in
        Eio.Promise.await entered;
        let second = Eio.Fiber.fork_promise ~sw (stop ~closing) in
        Eio.Fiber.yield ();
        assert (Owner.is_loaded owner);
        [%test_eq: int] 0 !closes;
        (match closing, Owner.ensure_loaded owner with
         | false, Error { code = Conflict; _ }
         | true, Error { code = Server_shutting_down; _ } -> ()
         | _ -> failwith "dependency barrier admitted new runtime work");
        Eio.Promise.resolve release_u ();
        List.iter [ first; second ] ~f:(fun pending ->
          match raises, Eio.Promise.await_exn pending with
          | false, Ok (Error actual) ->
            [%test_eq: Sexp.t]
              (Agent_protocol.Error.sexp_of_t failure)
              (Agent_protocol.Error.sexp_of_t actual)
          | true, Error (Failure message) ->
            [%test_eq: string] "descendant cleanup exception" message
          | _ -> failwith "retirement failure changed between waiters");
        [%test_eq: int] 1 !calls;
        [%test_eq: int] 0 !closes;
        assert (Owner.is_loaded owner);
        fail := false;
        (match closing with
         | false -> Owner.unload_and_wait owner |> protocol_ok
         | true -> Owner.close_and_wait owner);
        [%test_eq: int] 2 !calls;
        [%test_eq: int] 1 !closes;
        assert (not (Owner.is_loaded owner));
        Owner.close_and_wait owner;
        print_s
          [%sexp
            { exception_path = (raises : bool)
            ; closing : bool
            ; shared_failure = true
            ; retry_closed_once = true
            }]));
  [%expect
    {|
    ((exception_path false) (closing false) (shared_failure true)
     (retry_closed_once true))
    ((exception_path false) (closing true) (shared_failure true)
     (retry_closed_once true))
    ((exception_path true) (closing false) (shared_failure true)
     (retry_closed_once true))
    ((exception_path true) (closing true) (shared_failure true)
     (retry_closed_once true))
    |}]
;;

let%expect_test "retirement seals before a selected loan's late physical cleanup failure" =
  let module Recovery = Owner.Cleanup_recovery in
  let module Retirement = Owner.Resource_retirement in
  let module S = Owner.Scope_outcome in
  let previous_backtraces = Stdlib.Printexc.backtrace_status () in
  Exn.protect
    ~finally:(fun () -> Stdlib.Printexc.record_backtrace previous_backtraces)
    ~f:(fun () ->
      Stdlib.Printexc.record_backtrace true;
      with_actor (fun env sw actor _writer _backend ->
        Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
          let dependency_failure = Failure "original retirement dependency"
          and late_failure = Failure "selected loan late physical close" in
          let dependency_fails = ref false
          and close_fails = ref true in
          let dependency_trace = ref None
          and close_trace = ref None in
          let dependency_closing = ref [] in
          let closes = ref 0
          and builds = ref 0 in
          let raise_recorded trace exn =
            try raise exn with
            | actual ->
              let backtrace = Stdlib.Printexc.get_raw_backtrace () in
              trace := Some backtrace;
              Exn.raise_with_original_backtrace actual backtrace
          in
          let retained =
            runtime
              ~close:(fun () ->
                Int.incr closes;
                if !close_fails then raise_recorded close_trace late_failure)
              ()
          in
          let owner =
            Owner.create_with_unload
              ~actor
              ~initial:(Some retained)
              ~build:(fun () ->
                Int.incr builds;
                failwith "late cleanup rebuilt a runtime")
              ~before_unload:(fun ~closing ->
                dependency_closing := closing :: !dependency_closing;
                if !dependency_fails
                then raise_recorded dependency_trace dependency_failure;
                Ok ())
          in
          let entered, enter = Eio.Promise.create () in
          let release, release_u = Eio.Promise.create () in
          let loan = ref None
          and retirement_work = ref None
          and retirement_task = ref None
          and captured = ref None in
          let check_after_cleanup = ref (fun () -> ()) in
          let check_trace actual expected =
            let expected = Stdlib.Printexc.raw_backtrace_to_string expected in
            assert (not (String.is_empty expected));
            assert (
              String.is_prefix
                (Stdlib.Printexc.raw_backtrace_to_string actual)
                ~prefix:expected)
          in
          let run_claim = function
            | Retirement.Run work ->
              Retirement.run work;
              Retirement.completion work
            | Join attempt -> attempt
          in
          let acknowledge_dependency attempt =
            match Retirement.await attempt with
            | _ -> failwith "original dependency failure became a result"
            | exception actual -> assert (phys_equal actual dependency_failure)
          in
          Exn.protect
            ~finally:(fun () ->
              Eio.Cancel.protect (fun () ->
                dependency_fails := false;
                close_fails := false;
                ignore (Eio.Promise.try_resolve release_u ());
                Exn.protect
                  ~f:(fun () ->
                    Option.iter !loan ~f:(fun task -> ignore (Eio.Promise.await_exn task)))
                  ~finally:(fun () ->
                    Exn.protect
                      ~f:(fun () -> Option.iter !retirement_task ~f:Eio.Promise.await_exn)
                      ~finally:(fun () ->
                        (* The failed All_resources attempt still owns exclusion.
                           Use its actual public retry, not a fresh ordinary unload. *)
                        Option.iter !retirement_work ~f:(fun work ->
                          Retirement.run work;
                          let original = Retirement.completion work in
                          match Retirement.peek original with
                          | Some (Raised (actual, _))
                            when phys_equal actual dependency_failure ->
                            acknowledge_dependency original;
                            Owner.retry_resource_retirement owner ~failed:original
                            |> protocol_ok
                            |> run_claim
                            |> Retirement.await
                            |> protocol_ok
                          | Some (Returned (Ok ())) -> ()
                          | Some (Returned (Error error)) ->
                            raise (Owner.Cleanup_failed error)
                          | Some (Raised (exn, backtrace)) ->
                            Exn.raise_with_original_backtrace exn backtrace
                          | None ->
                            failwith "retirement remained running during finalization");
                        Owner.close_and_wait owner))))
            ~f:(fun () ->
              let task =
                Eio.Fiber.fork_promise ~sw (fun () ->
                  Owner.with_delegation_resources_outcome
                    owner
                    ~capture:(fun recovery ->
                      assert (Option.is_none !captured);
                      captured := Some recovery)
                    (fun actual ->
                       assert (phys_equal actual retained);
                       Eio.Cancel.protect (fun () ->
                         Eio.Promise.resolve enter ();
                         Eio.Promise.await release;
                         Ok ())))
              in
              loan := Some task;
              (match
                 Eio.Fiber.first
                   (fun () ->
                      Eio.Promise.await entered;
                      `Entered)
                   (fun () -> `Finished (Eio.Promise.await_exn task))
               with
               | `Entered -> ()
               | `Finished report ->
                 S.resume (Owner.Lease_outcome.outcome report) |> protocol_ok;
                 failwith "loan ended before its actual callback");
              let cleanup = Option.value_exn !captured in
              Owner.unload_and_wait owner |> protocol_ok;
              assert (not (Owner.is_loaded owner));
              [%test_eq: int] 0 !closes;
              assert (Option.is_none (Recovery.original cleanup |> protocol_ok));
              assert (Option.is_none (Recovery.snapshot cleanup |> protocol_ok));
              dependency_fails := true;
              Owner.close owner;
              let work =
                match Owner.claim_resource_retirement owner |> protocol_ok with
                | Run work -> work
                | Join _ ->
                  failwith "permanent retirement did not create its actual producer"
              in
              retirement_work := Some work;
              let original = Retirement.completion work in
              assert (Option.is_none (Retirement.peek original));
              let producer = Eio.Fiber.fork_promise ~sw (fun () -> Retirement.run work) in
              retirement_task := Some producer;
              Eio.Promise.await_exn producer;
              let original_trace =
                match Retirement.peek original with
                | Some (Raised (actual, backtrace)) ->
                  assert (phys_equal actual dependency_failure);
                  check_trace backtrace (Option.value_exn !dependency_trace);
                  backtrace
                | None | Some (Returned _) ->
                  failwith "retirement did not seal its actual dependency failure"
              in
              acknowledge_dependency original;
              [%test_eq: bool list] [ true; false ] !dependency_closing;
              assert (Option.is_none (Eio.Promise.peek task));
              assert (Option.is_none (Recovery.original cleanup |> protocol_ok));
              [%test_eq: int] 0 !closes;
              Eio.Promise.resolve release_u ();
              let report = Eio.Promise.await_exn task in
              (match S.primary (Owner.Lease_outcome.outcome report) with
               | Returned (Ok ()) -> ()
               | Returned (Error _) | Raised _ ->
                 failwith "selected loan's body primary changed during physical cleanup");
              assert (phys_equal cleanup (Owner.Lease_outcome.cleanup report));
              let late_original =
                Recovery.original cleanup |> protocol_ok |> Option.value_exn
              in
              let late_trace =
                match S.Cleanup.status late_original with
                | Failed (Raised (actual, backtrace), []) ->
                  assert (phys_equal actual late_failure);
                  check_trace backtrace (Option.value_exn !close_trace);
                  backtrace
                | Failed _ | Never_acquired | Joined ->
                  failwith "actual late physical close was not the sole cleanup failure"
              in
              let before = Recovery.snapshot cleanup |> protocol_ok |> Option.value_exn in
              (match Recovery.Snapshot.failures before with
               | [ Raised (actual, backtrace) ] ->
                 assert (phys_equal actual late_failure);
                 assert (phys_equal backtrace late_trace)
               | _ -> failwith "late physical cleanup snapshot lost its original failure");
              [%test_eq: int] 1 !closes;
              [%test_eq: int] 0 !builds;
              assert (not (Owner.is_loaded owner));
              let check_frozen () =
                assert (
                  phys_equal
                    late_original
                    (Recovery.original cleanup |> protocol_ok |> Option.value_exn));
                assert (
                  phys_equal
                    before
                    (Recovery.snapshot cleanup |> protocol_ok |> Option.value_exn));
                (* A completed parent does not become the later child's outcome.
                   Reported close origin transport is a separate integration boundary. *)
                match Retirement.peek original with
                | Some (Raised (actual, backtrace)) ->
                  assert (phys_equal actual dependency_failure);
                  assert (phys_equal backtrace original_trace)
                | None | Some (Returned _) ->
                  failwith "late child cleanup rewrote its original parent outcome"
              in
              check_frozen ();
              check_after_cleanup := check_frozen;
              acknowledge_dependency original;
              (* This actual joined close names the failed predecessor BEFORE
                 replacing it. Its fresh original view includes the SAME loan's
                 later failure; no earlier fault-on report adds another attempt. *)
              dependency_fails := false;
              close_fails := false;
              let disposed = Owner.close_and_wait_reported owner in
              (match Owner.Disposal_report.disposition disposed with
               | Joined ->
                 (* The real positive receipt makes the test-local explicit
                    retirement retry unnecessary. On every earlier failure the
                    existing release-before-join fallback remains installed. *)
                 retirement_work := None
               | Retained -> failwith "repaired late-child retirement stayed retained");
              let failures = Owner.Disposal_report.failures disposed in
              let repeated = Owner.close_and_wait_reported owner in
              (* Report reads do not consume presentation. Deliver the cached
                 diagnostic before further assertions, so teardown stays safe if
                 an assertion below detects lost/duplicated report data. *)
              (match Owner.close_and_wait owner with
               | () -> failwith "reported reads consumed the original dependency"
               | exception actual ->
                 assert (phys_equal actual dependency_failure);
                 check_trace (Stdlib.Printexc.get_raw_backtrace ()) original_trace);
              Owner.close_and_wait owner;
              assert (phys_equal disposed repeated);
              (match failures with
               | [ Raised (dependency, dependency_backtrace)
                 ; Raised (late, late_backtrace)
                 ] ->
                 assert (phys_equal dependency dependency_failure);
                 assert (phys_equal dependency_backtrace original_trace);
                 assert (phys_equal late late_failure);
                 assert (phys_equal late_backtrace late_trace)
               | _ ->
                 failwith "final disposal lost or duplicated parent/late-child originals");
              check_frozen ();
              (* New accepted cleanup supersedes the completed cache, without
                 reminting its already delivered parent/child diagnostic groups. *)
              let queued =
                match Owner.signal_resource_retirement owner |> protocol_ok with
                | Retirement.Run work -> Retirement.completion work
                | Join _ -> failwith "completed late-child Owner retained old work"
              in
              assert (Option.is_none (Retirement.peek queued));
              let refreshed = Owner.close_and_wait_reported owner in
              (match Owner.Disposal_report.disposition refreshed with
               | Joined -> ()
               | Retained -> failwith "newly accepted cleanup stayed retained");
              assert (not (phys_equal disposed refreshed));
              (match Retirement.peek queued with
               | Some (Returned (Ok ())) -> ()
               | Some (Returned (Error error)) -> raise (Owner.Cleanup_failed error)
               | Some (Raised (exn, trace)) -> Exn.raise_with_original_backtrace exn trace
               | None -> failwith "reported cache skipped newly accepted cleanup");
              let check_originals = function
                | [ S.Cleanup.Failure.Raised (dependency, dependency_backtrace)
                  ; Raised (late, late_backtrace)
                  ] ->
                  assert (phys_equal dependency dependency_failure);
                  assert (phys_equal dependency_backtrace original_trace);
                  assert (phys_equal late late_failure);
                  assert (phys_equal late_backtrace late_trace)
                | _ -> failwith "superseded cache lost or reminted original diagnostics"
              in
              check_originals (Owner.Disposal_report.failures disposed);
              check_originals (Owner.Disposal_report.failures refreshed);
              assert (phys_equal refreshed (Owner.close_and_wait_reported owner));
              Owner.close_and_wait owner;
              check_frozen ();
              [%test_eq: int] 2 !closes;
              [%test_eq: int] 0 !builds);
          !check_after_cleanup ();
          [%test_eq: int] 2 !closes;
          [%test_eq: int] 0 !builds)));
  print_endline
    "original retirement sealed; final disposal retains parent and late child once; no \
     rebuild";
  [%expect
    {| original retirement sealed; final disposal retains parent and late child once; no rebuild |}]
;;

let%expect_test "joined close joins genuine retirement accepted in its completed suffix" =
  let module Retirement = Owner.Resource_retirement in
  let run fail_late =
    Canonical_delegated_source.with_fixture (fun fixture ->
      let suffix_entered, suffix_entered_u = Eio.Promise.create () in
      let suffix_release, suffix_release_u = Eio.Promise.create () in
      let original = ref None in
      let suffix_gated = ref false in
      let dependency_calls = ref 0 in
      let late_admitted = ref false in
      let reject_late = ref fail_late in
      let dependency_failure =
        Agent_protocol.Error.create
          Conflict
          ~message:"actual late retirement dependency failure"
          ~retryable:true
          ()
      in
      let check_error (actual : Agent_protocol.Error.t) =
        assert (phys_equal actual dependency_failure);
        [%test_eq: Sexp.t]
          (Agent_protocol.Error.sexp_of_t dependency_failure)
          (Agent_protocol.Error.sexp_of_t actual)
      in
      let builds = ref 0 in
      let owner =
        Owner.create_with_deferred_run_unload
          ~source_owner:fixture.source_owner
          ~before_unload:(fun ~closing:_ ->
            Int.incr dependency_calls;
            if !late_admitted && !reject_late then Error dependency_failure else Ok ())
          ~policy_resource_cleanup:
            (Owner.Policy_resource_cleanup.create
               ~pending:(fun () -> false)
               ~close_pending:(fun () ->
                 (* Before A's actual completion these are A's own cleanup
                  callbacks. Gate only the later joined-close suffix. *)
                 (match Option.bind !original ~f:Retirement.peek with
                  | Some (Returned (Ok ())) when not !suffix_gated ->
                    suffix_gated := true;
                    Eio.Promise.resolve suffix_entered_u ();
                    Eio.Promise.await suffix_release
                  | None | Some (Returned _ | Raised _) -> ());
                 Ok ()))
          ~policy_resource_access:None
          ~native_resource_access:None
          ~actor:fixture.actor
          ~initial:None
          ~build:(fun ~initialization_cleanup:_ ~original_actor:_ _ ->
            Int.incr builds;
            failwith "late retirement built an ordinary runtime")
          ~build_deferred:
            (fun
              ~initialization_cleanup:_
              ~source_consumer:_
              ~source:_
              ~expected_product:_
              ~construction:_ ->
            Int.incr builds;
            failwith "late retirement built a deferred runtime")
          ~ordinary_warm:None
          ~delegated_load:None
      in
      let original_work = ref None in
      let late_work = ref None in
      let close_task = ref None in
      let release_suffix () =
        if Option.is_none (Eio.Promise.peek suffix_release)
        then Eio.Promise.resolve suffix_release_u ()
      in
      let completed attempt =
        match Retirement.peek attempt with
        | Some (Returned (Ok ())) -> ()
        | Some (Returned (Error error)) -> raise (Owner.Cleanup_failed error)
        | Some (Raised (exn, trace)) -> Exn.raise_with_original_backtrace exn trace
        | None -> failwith "joined close left its actual retirement unfinished"
      in
      Canonical_delegated_source.protect
        ~finally:(fun () ->
          (* Release before joining, including on the unfixed red path. Run only
           actual retained work; duplicate producers cannot repeat cleanup. *)
          release_suffix ();
          reject_late := false;
          Canonical_delegated_source.protect
            ~finally:(fun () ->
              let repaired = Owner.close_and_wait_reported owner in
              (match Owner.Disposal_report.disposition repaired with
               | Joined -> ()
               | Retained -> failwith "late retirement fixture cleanup stayed retained");
              (* Only this actual producer's known Protocol may be delivered by
               the repaired report. A different cleanup failure is never waived. *)
              match Owner.close_and_wait owner with
              | () -> ()
              | exception Owner.Cleanup_failed error ->
                assert fail_late;
                check_error error;
                Owner.close_and_wait owner)
            (fun () ->
               Option.iter !original_work ~f:Retirement.run;
               Option.iter !late_work ~f:Retirement.run;
               Option.iter !close_task ~f:(fun task ->
                 ignore (Eio.Promise.await_exn task : Owner.Disposal_report.t))))
        (fun () ->
           let first_work =
             match Owner.claim_resource_retirement owner |> protocol_ok with
             | Run work -> work
             | Join _ -> failwith "fresh Owner unexpectedly joined prior retirement"
           in
           original_work := Some first_work;
           let first = Retirement.completion first_work in
           original := Some first;
           assert (Option.is_none (Retirement.peek first));
           let task =
             Eio.Fiber.fork_promise ~sw:fixture.sw (fun () ->
               Owner.close_and_wait_reported owner)
           in
           close_task := Some task;
           Eio.Promise.await suffix_entered;
           completed first;
           [%test_eq: int] 1 !dependency_calls;
           let second_work =
             match Owner.claim_resource_retirement owner |> protocol_ok with
             | Run work -> work
             | Join _ -> failwith "completed A was mistaken for newly accepted B"
           in
           late_work := Some second_work;
           late_admitted := true;
           let second = Retirement.completion second_work in
           assert (not (phys_equal first second));
           assert (Option.is_none (Retirement.peek second));
           assert (Option.is_none (Eio.Promise.peek task));
           release_suffix ();
           let report = Eio.Promise.await_exn task in
           if fail_late
           then (
             (match Owner.Disposal_report.disposition report with
              | Retained -> ()
              | Joined -> failwith "failed late B incorrectly certified joined disposal");
             let check_original_failure () =
               (match Retirement.peek second with
                | Some (Returned (Error error)) -> check_error error
                | None | Some (Returned (Ok ()) | Raised _) ->
                  failwith "late B lost its actual returned Protocol failure");
               match Owner.Disposal_report.failures report with
               | [ Protocol error ] -> check_error error
               | _ -> failwith "late close lost or duplicated actual Protocol B"
             in
             check_original_failure ();
             (match Retirement.await second with
              | Error error -> check_error error
              | Ok () -> failwith "observing old B changed its actual failure");
             check_original_failure ();
             completed first;
             [%test_eq: int] 2 !dependency_calls;
             [%test_eq: int] 0 !builds)
           else (
             (match Owner.Disposal_report.disposition report with
              | Joined -> ()
              | Retained -> failwith "joined close skipped actual late retirement B");
             completed second;
             completed first;
             assert (List.is_empty (Owner.Disposal_report.failures report));
             [%test_eq: int] 2 !dependency_calls;
             [%test_eq: int] 0 !builds;
             assert (phys_equal report (Owner.close_and_wait_reported owner));
             completed first;
             completed second;
             [%test_eq: int] 2 !dependency_calls)))
  in
  run false;
  print_endline "late accepted retirement joined; original completion frozen; no rebuild";
  run true;
  print_endline
    "late Protocol failure retained once; no implicit retry; original A frozen";
  [%expect
    {|
    late accepted retirement joined; original completion frozen; no rebuild
    late Protocol failure retained once; no implicit retry; original A frozen
    |}]
;;

let%expect_test "external close joins an accepted stop before retiring its dependencies" =
  List.iter [ false; true ] ~f:(fun background ->
    with_actor (fun _env sw actor _writer _backend ->
      let entered, entered_u = Eio.Promise.create () in
      let release, release_u = Eio.Promise.create () in
      let worker_entered, worker_entered_u = Eio.Promise.create () in
      let never, _ = Eio.Promise.create () in
      let released = ref false in
      let release_cleanup () =
        if not !released
        then (
          released := true;
          Eio.Promise.resolve release_u ())
      in
      let closes = ref 0 in
      let owner =
        Owner.create_with_unload
          ~actor
          ~initial:(Some (runtime ~close:(fun () -> Int.incr closes) ()))
          ~build:(fun () -> failwith "unexpected rebuild")
          ~before_unload:(fun ~closing:_ ->
            Eio.Promise.resolve entered_u ();
            Eio.Promise.await release;
            Ok ())
      in
      Exn.protect ~finally:release_cleanup ~f:(fun () ->
        let worker =
          match background with
          | false -> None
          | true ->
            Some
              (Eio.Fiber.fork_promise ~sw (fun () ->
                 Result.try_with (fun () ->
                   Owner.with_background_runtime owner (fun _ ->
                     Eio.Promise.resolve worker_entered_u ();
                     Eio.Promise.await never;
                     Ok ()))))
        in
        if background then Eio.Promise.await worker_entered;
        let stopping =
          Eio.Fiber.fork_promise ~sw (fun () -> Owner.unload_and_wait owner)
        in
        Eio.Promise.await entered;
        let closing = Eio.Fiber.fork_promise ~sw (fun () -> Owner.close_and_wait owner) in
        Eio.Fiber.yield ();
        Eio.Fiber.yield ();
        [%test_eq: int] 0 !closes;
        assert (Owner.is_loaded owner);
        assert (Option.is_none (Eio.Promise.peek closing));
        release_cleanup ();
        Eio.Promise.await_exn stopping |> protocol_ok;
        Eio.Promise.await_exn closing;
        Option.iter worker ~f:(fun worker ->
          match Eio.Promise.await_exn worker with
          | Error (Eio.Cancel.Cancelled _) -> ()
          | _ -> failwith "background lease was not cancelled");
        [%test_eq: int] 1 !closes;
        assert (not (Owner.is_loaded owner));
        print_s [%sexp (background : bool), "stop barrier joined before close"])));
  [%expect
    {|
    (false "stop barrier joined before close")
    (true "stop barrier joined before close") |}]
;;

let%expect_test "failed authority lookup preserves owner cleanup and rechecks every lease"
  =
  with_actor (fun _env _sw actor _writer _backend ->
    let unavailable = ref true in
    let checks = ref 0 in
    let closes = ref 0 in
    let activations = ref 0 in
    let initial =
      runtime
        ~check_execution:(fun () ->
          Int.incr checks;
          Eio.Fiber.yield ();
          match !unavailable with
          | true -> failwith "injected authority lookup failure"
          | false -> Ok ())
        ~close:(fun () -> Int.incr closes)
        ()
    in
    let initial =
      { initial with
        moderator_activation =
          Some
            { pending = (fun () -> true)
            ; startup_pending = (fun () -> false)
            ; run =
                (fun ?capture_cleanup:_ () ->
                  Int.incr activations;
                  Ok true)
            }
      }
    in
    let owner =
      Owner.create ~actor ~initial:(Some initial) ~build:(fun () -> assert false)
    in
    let expect_lookup_failure f =
      match f () with
      | _ -> failwith "missing authority failure"
      | exception Failure message ->
        [%test_eq: string] "injected authority lookup failure" message
    in
    expect_lookup_failure (fun () -> Owner.ensure_loaded owner);
    expect_lookup_failure (fun () ->
      Owner.with_background_runtime owner (fun _ -> failwith "unauthorized lease"));
    assert (Owner.is_loaded owner);
    unavailable := false;
    let saved_activation = ref None in
    Owner.with_background_runtime_activation owner (fun _ ~activate ->
      let activate = Option.value_exn activate in
      saved_activation := Some activate;
      assert (activate () |> protocol_ok);
      Ok ())
    |> protocol_ok;
    [%test_eq: int] 1 !activations;
    (* The custom activation ignores capture_cleanup. Only the lexical wrapper's
       own lifetime check can reject before this underlying effect runs. *)
    (match (Option.value_exn !saved_activation) () with
     | Error error -> [%test_eq: Agent_protocol.Error.code] Conflict error.code
     | Ok _ -> failwith "sealed activation closure reached its underlying effect");
    [%test_eq: int] 1 !activations;
    unavailable := true;
    expect_lookup_failure (fun () -> Owner.ensure_loaded owner);
    Owner.unload_and_wait owner |> protocol_ok;
    assert (not (Owner.is_loaded owner));
    [%test_eq: int] 4 !checks;
    [%test_eq: int] 1 !closes;
    Owner.close owner;
    print_endline
      "failed reads deny entry without poisoning retirement; leases revalidate");
  [%expect {| failed reads deny entry without poisoning retirement; leases revalidate |}]
;;

let%expect_test
    "committed stop joins cleanup, excludes admission and retires even when close fails"
  =
  List.iter [ false; true ] ~f:(fun fail_close ->
    with_actor (fun _env sw actor _writer _backend ->
      let entered, enter = Eio.Promise.create () in
      let cleaning, cleaning_u = Eio.Promise.create () in
      let release, release_u = Eio.Promise.create () in
      let never, _ = Eio.Promise.create () in
      let cleaned = ref false
      and closes = ref 0
      and fail_once = ref fail_close in
      let build () =
        Ok
          (runtime
             ~close:(fun () ->
               assert !cleaned;
               incr closes;
               match !fail_once with
               | false -> ()
               | true ->
                 fail_once := false;
                 failwith "injected runtime close failure")
             ())
      in
      let owner = Owner.create ~actor ~initial:None ~build in
      let worker =
        Eio.Fiber.fork_promise ~sw (fun () ->
          Result.try_with (fun () ->
            Owner.with_background_runtime owner (fun _ ->
              Eio.Promise.resolve enter ();
              Exn.protect
                ~finally:(fun () ->
                  Eio.Cancel.protect (fun () ->
                    Eio.Promise.resolve cleaning_u ();
                    Eio.Promise.await release;
                    cleaned := true))
                ~f:(fun () ->
                  Eio.Promise.await never;
                  Ok ()))))
      in
      Eio.Promise.await entered;
      let stopped =
        Eio.Fiber.fork_promise ~sw (fun () ->
          Result.try_with (fun () -> Owner.unload_and_wait owner))
      in
      Eio.Promise.await cleaning;
      let joined =
        List.init 2 ~f:(fun _ ->
          Eio.Fiber.fork_promise ~sw (fun () ->
            Result.try_with (fun () -> Owner.unload_and_wait owner)))
      in
      Eio.Fiber.yield ();
      [%test_eq: int] 0 !closes;
      (match Owner.ensure_loaded owner with
       | Error { code = Conflict; retryable = true; _ } -> ()
       | _ -> failwith "new runtime admission escaped pending cleanup");
      Eio.Promise.resolve release_u ();
      (match Eio.Promise.await_exn worker with
       | Error (Eio.Cancel.Cancelled _) -> ()
       | _ -> failwith "worker cancellation was lost");
      List.iter (stopped :: joined) ~f:(fun waiter ->
        match fail_close, Eio.Promise.await_exn waiter with
        | false, Ok (Ok ()) -> ()
        | true, Error (Failure message)
          when String.equal message "injected runtime close failure" -> ()
        | _ -> failwith "stop produced an unexpected cleanup outcome");
      assert (not (Owner.is_loaded owner));
      [%test_eq: int] 1 !closes;
      if fail_close
      then (
        assert (not (Owner.source_lane_idle owner));
        (match Owner.ensure_loaded owner with
         | Error { code = Conflict; retryable = true; _ } -> ()
         | Ok () | Error _ -> failwith "failed retirement lost its exact runtime");
        Owner.unload_and_wait owner |> protocol_ok;
        [%test_eq: int] 2 !closes);
      Owner.ensure_loaded owner |> protocol_ok;
      Owner.unload_and_wait owner |> protocol_ok;
      [%test_eq: int] (if fail_close then 3 else 2) !closes;
      print_s
        [%sexp
          (fail_close : bool)
        , "cleanup joined before close; admission excluded; owner reusable"]));
  [%expect
    {|
    (false "cleanup joined before close; admission excluded; owner reusable")
    (true "cleanup joined before close; admission excluded; owner reusable")
    |}]
;;

let%expect_test
    "retention excludes job scopes and keeps runtime ownership until a cancelled \
     caller's checkpoint finishes"
  =
  with_actor (fun _env sw actor _writer _backend ->
    let job = add_claimed_job actor in
    with_job actor job (fun ~job:_ ~execute:_ ->
      assert (
        Option.is_none
          (A.with_quiescent_state actor ~f:(fun _ ->
             failwith "live job scope entered retention")
           |> protocol_ok));
      Ok ())
    |> protocol_ok;
    let owner =
      Owner.create ~actor ~initial:None ~build:(fun () ->
        failwith "maintenance must not load a runtime")
    in
    let entered, enter = Eio.Promise.create () in
    let release, release_u = Eio.Promise.create () in
    let cancellation, cancellation_u = Eio.Promise.create () in
    let caller_done, caller_done_u = Eio.Promise.create () in
    Eio.Fiber.fork ~sw (fun () ->
      Exn.protect
        ~finally:(fun () -> Eio.Promise.resolve caller_done_u ())
        ~f:(fun () ->
          try
            Eio.Cancel.sub (fun context ->
              Eio.Promise.resolve cancellation_u context;
              Owner.with_unloaded owner (fun () ->
                A.with_quiescent_state actor ~f:(fun _ ->
                  Eio.Promise.resolve enter ();
                  Eio.Promise.await release;
                  Ok ()))
              |> protocol_ok
              |> ignore)
          with
          | Eio.Cancel.Cancelled _ -> ()));
    Eio.Promise.await entered;
    Eio.Cancel.cancel (Eio.Promise.await cancellation) Exit;
    let competitor_entered = ref false in
    let competitor_done, competitor_done_u = Eio.Promise.create () in
    Eio.Fiber.fork ~sw (fun () ->
      Owner.with_unloaded owner (fun () ->
        competitor_entered := true;
        Ok ())
      |> protocol_ok
      |> ignore;
      Eio.Promise.resolve competitor_done_u ());
    Eio.Fiber.yield ();
    assert (not !competitor_entered);
    Eio.Promise.resolve release_u ();
    Eio.Promise.await caller_done;
    Eio.Promise.await competitor_done;
    assert !competitor_entered;
    assert (
      Result.is_error
        (Result.try_with (fun () ->
           Owner.with_unloaded owner (fun () -> failwith "injected inspection exception"))));
    assert (Option.is_some (Owner.with_unloaded owner (fun () -> Ok ()) |> protocol_ok));
    print_endline
      "live job scope deferred; cancellation did not release an outstanding actor \
       checkpoint";
    print_endline
      "competing maintenance resumed after completion; callback exception did not poison \
       owner");
  [%expect
    {|
    live job scope deferred; cancellation did not release an outstanding actor checkpoint
    competing maintenance resumed after completion; callback exception did not poison owner
    |}]
;;

let%expect_test
    "independent native jobs retain one runtime without serializing each other"
  =
  with_actor (fun env sw actor _writer backend ->
    let first_entered, first_entered_u = Eio.Promise.create () in
    let second_entered, second_entered_u = Eio.Promise.create () in
    let first_done, first_done_u = Eio.Promise.create () in
    let calls = ref 0
    and running = ref 0
    and maximum = ref 0 in
    let registry =
      native_registry calls ~raises:false ~on_call:(fun () ->
        incr running;
        maximum := Int.max !maximum !running;
        Exn.protect
          ~finally:(fun () -> decr running)
          ~f:(fun () ->
            match !calls with
            | 1 ->
              Eio.Promise.resolve first_entered_u ();
              Eio.Promise.await second_entered
            | _ -> Eio.Promise.resolve second_entered_u ()))
    in
    let script_tools =
      Agent_session.Script_tool_calls.create
        ~registry:(fun () -> registry)
        ~moderator_names:String.Set.empty
        ~now:(fun () -> timestamp)
        ~is_halted:(fun () -> false)
        ~requires_active_moderator:(fun _ -> false)
        ~authorize:(fun _ _ -> Ok ())
        ~prepare_output:(fun _ -> Ok (`String "disclosed"))
        ~defer_observation:(fun _ -> Ok ())
    in
    let builds = ref 0
    and closes = ref 0 in
    let owner =
      Owner.create ~actor ~initial:None ~build:(fun () ->
        incr builds;
        Ok
          (runtime
             ~script_tools
             ~close:(fun () ->
               [%test_eq: int] 0 !running;
               incr closes)
             ()))
    in
    let request =
      Chat_response.Background_request.tool
        ~capabilities:registry
        ~reference:(List.hd_exn (C.references registry))
        ~input:(`Object [])
        ~policy:Chat_response.One_off_request.default_policy
      |> protocol_ok
    in
    let run_job () =
      let job =
        add_claimed_job ~payload:(Chat_response.Background_request.to_json request) actor
      in
      Owner.with_background_runtime owner (fun runtime ->
        with_job actor job (fun ~job ~execute ->
          Agent_session.Background_execution.run
            ~env
            ~job
            ~deadline
            ~execute
            ~request
            ~policy:Chat_response.One_off_request.default_policy
            ~script_tools:(Option.value_exn runtime.moderator_script_tools)
            ~now:(fun () -> timestamp)
            ~moderate_tool:(fun _ _ -> Ok None)
            ~prepare_outcome:(fun _ -> Ok ())
            ()))
      |> protocol_ok
      |> ignore
    in
    Eio.Fiber.fork ~sw (fun () ->
      run_job ();
      Eio.Promise.resolve first_done_u ());
    Eio.Promise.await first_entered;
    let administrative_calls = ref 0 in
    reject "unload while owned" (Owner.unload owner);
    reject
      "administration while owned"
      (Owner.with_administration owner (fun () ->
         incr administrative_calls;
         Ok ()));
    [%test_eq: int] 0 !administrative_calls;
    [%test_eq: int] 0 !closes;
    run_job ();
    Eio.Promise.await first_done;
    Owner.unload owner |> protocol_ok;
    Owner.with_background_runtime owner (fun _ -> Ok ()) |> protocol_ok;
    Owner.close owner;
    let state = Agent_session.Memory_backend.state backend in
    print_s
      [%sexp
        { builds = (!builds : int)
        ; closes = (!closes : int)
        ; concurrent = (!maximum : int)
        ; invocations = (List.length state.invocations : int)
        ; history = (List.length state.conversation.canonical_history : int)
        }]);
  [%expect
    {|
    ("unload while owned" Conflict)
    ("administration while owned" Conflict)
    ((builds 2) (closes 2) (concurrent 2) (invocations 4) (history 0))
    |}]
;;

let%expect_test "close cancels callbacks and retires only after their cleanup finishes" =
  with_actor (fun _env sw actor _writer _backend ->
    let entered, entered_u = Eio.Promise.create () in
    let cleaning, cleaning_u = Eio.Promise.create () in
    let release, release_u = Eio.Promise.create () in
    let done_, done_u = Eio.Promise.create () in
    let joined, joined_u = Eio.Promise.create () in
    let never, _ = Eio.Promise.create () in
    let closes = ref 0 in
    let owner =
      Owner.create ~actor ~initial:None ~build:(fun () ->
        Ok (runtime ~close:(fun () -> incr closes) ()))
    in
    Eio.Fiber.fork ~sw (fun () ->
      let cancelled =
        try
          Owner.with_background_runtime owner (fun _ ->
            Exn.protect
              ~f:(fun () ->
                Eio.Promise.resolve entered_u ();
                Eio.Promise.await never;
                Ok ())
              ~finally:(fun () ->
                Eio.Cancel.protect (fun () ->
                  Eio.Promise.resolve cleaning_u ();
                  Eio.Promise.await release)))
          |> protocol_ok;
          false
        with
        | Eio.Cancel.Cancelled _ -> true
      in
      Eio.Promise.resolve done_u cancelled);
    Eio.Promise.await entered;
    Eio.Fiber.fork ~sw (fun () ->
      Owner.close_and_wait owner;
      Eio.Promise.resolve joined_u ());
    Eio.Promise.await cleaning;
    assert (Option.is_none (Eio.Promise.peek joined));
    [%test_eq: int] 0 !closes;
    [%test_eq: bool] true (Owner.is_loaded owner);
    reject "reload after close" (Owner.ensure_loaded owner);
    reject
      "new callback after close"
      (Owner.with_background_runtime owner (fun _ ->
         failwith "closed owner admitted work"));
    Eio.Promise.resolve release_u ();
    [%test_eq: bool] true (Eio.Promise.await done_);
    Eio.Promise.await joined;
    Owner.close owner;
    print_s [%sexp (!closes : int), (Owner.is_loaded owner : bool)]);
  [%expect
    {|
    ("reload after close" Server_shutting_down)
    ("new callback after close" Server_shutting_down)
    (1 false)
    |}]
;;

let%expect_test "callback errors retain failed close ownership until exact retry" =
  with_actor (fun _env _sw actor _writer _backend ->
    let closes = ref 0 in
    let close_succeeds = ref false in
    let owner =
      Owner.create ~actor ~initial:None ~build:(fun () ->
        Ok
          (runtime
             ~close:(fun () ->
               incr closes;
               if not !close_succeeds then failwith "fixture close failure")
             ()))
    in
    let callback_failed =
      try
        Owner.with_background_runtime owner (fun _ -> failwith "fixture callback failure")
        |> ignore;
        false
      with
      | Failure message -> String.equal message "fixture callback failure"
    in
    [%test_eq: bool] true callback_failed;
    let reused = ref false in
    Owner.with_background_runtime owner (fun _ ->
      reused := true;
      Ok ())
    |> protocol_ok;
    let close_failed =
      Result.try_with (fun () ->
        Owner.with_background_runtime owner (fun _ ->
          Owner.close owner;
          Ok ()))
      |> Result.is_error
    in
    [%test_eq: bool] true close_failed;
    reject "closed after cleanup error" (Owner.ensure_loaded owner);
    let retry_failed =
      try
        Owner.close owner;
        false
      with
      | Failure message -> String.equal message "fixture close failure"
    in
    assert retry_failed;
    [%test_eq: int] 2 !closes;
    assert (not (Owner.is_loaded owner));
    close_succeeds := true;
    Owner.close owner;
    Owner.close owner;
    print_s [%sexp (!reused : bool), (!closes : int), (Owner.is_loaded owner : bool)]);
  [%expect
    {|
    ("closed after cleanup error" Server_shutting_down)
    (true 3 false)
    |}]
;;

let%expect_test
    "cancelled callback releases its lease even when cleanup waits for the owner mutex"
  =
  with_actor (fun _env sw actor _writer _backend ->
    let entered, entered_u = Eio.Promise.create () in
    let leaving, leaving_u = Eio.Promise.create () in
    let finished, finished_u = Eio.Promise.create () in
    let never, _ = Eio.Promise.create () in
    let closes = ref 0 in
    let owner =
      Owner.create ~actor ~initial:None ~build:(fun () ->
        Ok (runtime ~close:(fun () -> incr closes) ()))
    in
    Eio.Fiber.fork ~sw (fun () ->
      let was_cancelled =
        try
          Eio.Cancel.sub (fun context ->
            Owner.with_background_runtime owner (fun _ ->
              Eio.Promise.resolve entered_u (fun () -> Eio.Cancel.cancel context Exit);
              Exn.protect
                ~finally:(fun () -> Eio.Promise.resolve leaving_u ())
                ~f:(fun () ->
                  Eio.Promise.await never;
                  Ok ())))
          |> protocol_ok;
          false
        with
        | Eio.Cancel.Cancelled _ -> true
      in
      Eio.Promise.resolve finished_u was_cancelled);
    let cancel = Eio.Promise.await entered in
    Owner.For_testing.with_loaded_runtime owner (fun () ->
      cancel ();
      Eio.Promise.await leaving;
      Eio.Fiber.yield ();
      assert (Option.is_none (Eio.Promise.peek finished));
      Ok ())
    |> protocol_ok;
    [%test_eq: bool] true (Eio.Promise.await finished);
    Owner.close_and_wait owner;
    print_s [%sexp (!closes : int), (Owner.is_loaded owner : bool)]);
  [%expect {| (1 false) |}]
;;

let%expect_test "administration initialization fences committed stopped cleanup" =
  List.iter [ false; true ] ~f:(fun fenced ->
    let on_commit = ref (fun (_ : Agent_session.Session_state.t) -> ()) in
    with_actor
      ~state_committed:(fun state _ -> !on_commit state)
      (fun _env sw actor writer _backend ->
         let module State = Agent_session.Session_state in
         A.stop actor ~attachment_id:writer.id ~mode:Graceful |> protocol_ok |> ignore;
         let before = A.state actor |> protocol_ok in
         let queued, queue = Eio.Promise.create () in
         let entered, enter = Eio.Promise.create () in
         let release, finish = Eio.Promise.create () in
         let cleanup = ref None in
         let initialized = ref 0 in
         let initialization_revision = ref None in
         let closed = ref 0 in
         let owner =
           Owner.create_with_unload
             ~actor
             ~initial:(Some (runtime ~close:(fun () -> incr closed) ()))
             ~before_unload:(fun ~closing:_ ->
               if Option.is_none (Eio.Promise.peek entered)
               then Eio.Promise.resolve enter ();
               Eio.Promise.await release;
               Ok ())
             ~build:(fun () ->
               incr initialized;
               let expected = A.state actor |> protocol_ok in
               (match expected.runtime_initialization with
                | Pending _ -> ()
                | Ready -> failwith "initialization did not read the committed selection");
               let scope = A.begin_initialization actor ~expected |> protocol_ok in
               Exn.protect
                 ~f:(fun () ->
                   A.complete_initialization actor ~scope ~candidate:expected
                   |> protocol_ok
                   |> ignore)
                 ~finally:(fun () ->
                   Eio.Cancel.protect (fun () ->
                     A.end_initialization actor ~scope |> protocol_ok));
               initialization_revision
               := Some (A.state actor |> protocol_ok).counters.revision;
               Ok (runtime ~close:(fun () -> incr closed) ()))
         in
         Exn.protect
           ~finally:(fun () ->
             if Option.is_none (Eio.Promise.peek release)
             then Eio.Promise.resolve finish ())
           ~f:(fun () ->
             let candidate =
               Agent_session.Administration.plan_rebuild
                 before
                 (Agent_protocol.Id.Prompt_revision.create ())
               |> protocol_ok
             in
             let candidate =
               { candidate with
                 runtime_initialization = Pending { fresh_history = true }
               }
             in
             (on_commit
              := fun state ->
                   match state.runtime_initialization, !cleanup with
                   | Pending _, None ->
                     cleanup
                     := Some
                          (Eio.Fiber.fork_promise ~sw (fun () ->
                             Eio.Promise.resolve queue ();
                             Owner.unload_and_wait owner))
                   | Ready, _ | Pending _, Some _ -> ());
             let commit () =
               A.commit_reconciled_administration
                 actor
                 ~command_audit:None
                 ~attachment_id:writer.id
                 ~expected:before
                 ~kind:Rebuild
                 candidate
               |> Result.map ~f:(fun _ -> ())
             in
             let before_initialize () =
               (* This is the actual stop-cleanup task queued by State_committed. The
             split control lets it acquire ownership; the fenced route retains
             ownership through the same yield and all initialization admission. *)
               Eio.Promise.await queued;
               match fenced with
               | false -> Eio.Promise.await entered
               | true ->
                 Eio.Fiber.yield ();
                 assert (Option.is_none (Eio.Promise.peek entered))
             in
             let result =
               match fenced with
               | true ->
                 Owner.reinitialize_administration
                   owner
                   ~validate:(fun () -> Ok ())
                   ~commit
                   ~before_initialize
               | false ->
                 let open Result.Let_syntax in
                 let%bind () = Owner.unload owner in
                 let%bind () = commit () in
                 before_initialize ();
                 let%bind () = Owner.ensure_loaded owner in
                 let%map state = A.state actor in
                 State.summary state
             in
             (match fenced, result with
              | false, Error { code = Conflict; retryable = true; message; _ } ->
                [%test_eq: string]
                  "session runtime is waiting for background cleanup"
                  message;
                [%test_eq: int] 0 !initialized;
                let committed = A.state actor |> protocol_ok in
                [%test_eq: int]
                  (before.identity.generation + 1)
                  committed.identity.generation;
                [%test_eq: int64]
                  (Int64.succ before.counters.revision)
                  committed.counters.revision;
                (match committed.runtime_initialization with
                 | Pending _ -> ()
                 | Ready -> failwith "split route unexpectedly initialized")
              | true, Ok summary ->
                let ready = A.state actor |> protocol_ok in
                [%test_eq: int] 1 !initialized;
                [%test_eq: int] (before.identity.generation + 1) ready.identity.generation;
                [%test_eq: int64]
                  Int64.(before.counters.revision + 2L)
                  (Option.value_exn !initialization_revision);
                (* Runtime installation separately records its moderator snapshot. *)
                [%test_eq: int64]
                  Int64.(before.counters.revision + 3L)
                  ready.counters.revision;
                [%test_eq: int64] ready.counters.revision summary.revision;
                (match ready.runtime_initialization with
                 | Ready -> ()
                 | Pending _ -> failwith "fenced command returned Pending");
                assert (not (Owner.is_loaded owner))
              | _ -> failwith "administration fence changed the expected outcome");
             Eio.Promise.resolve finish ();
             Eio.Promise.await_exn (Option.value_exn !cleanup) |> protocol_ok;
             if not fenced
             then (
               Owner.ensure_loaded owner |> protocol_ok;
               Owner.unload owner |> protocol_ok);
             [%test_eq: int] 1 !initialized;
             [%test_eq: int] 2 !closed;
             Owner.close_and_wait owner;
             print_s [%sexp (fenced : bool), "one committed selection; cleanup joined"])));
  [%expect
    {|
    (false "one committed selection; cleanup joined")
    (true "one committed selection; cleanup joined")
    |}]
;;

exception Lease_primary
exception Lease_cleanup

let%expect_test "lease cleanup preserves primary failures and reports standalone cleanup" =
  let module CO = Agent_session.Close_observation in
  let module D = Agent_session.Cleanup_diagnostic in
  let module R = Owner.Cleanup_recovery in
  List.iter [ false; true ] ~f:(fun reported ->
    List.iter [ `Typed; `Raised; `Success ] ~f:(fun outcome ->
      with_actor (fun _env _sw actor _writer _ ->
        let closes = ref 0 in
        let owner =
          Owner.create
            ~actor
            ~initial:
              (Some
                 (runtime
                    ~close:(fun () ->
                      incr closes;
                      if Int.equal !closes 1 then raise Lease_cleanup)
                    ()))
            ~build:(fun () -> failwith "unexpected lease reload")
        in
        let primary = Agent_protocol.Error.invalid_request "primary lease failure" in
        let callback_group = ref None in
        let captured = ref None in
        let body_calls = ref 0 in
        let callback () =
          Int.incr body_calls;
          Owner.unload_and_wait owner |> protocol_ok;
          match outcome with
          | `Typed -> Error primary
          | `Raised -> raise Lease_primary
          | `Success -> Ok ()
        in
        let observed =
          try
            let result =
              if not reported
              then Owner.with_delegation_resources owner (fun _ -> callback ())
              else (
                let report =
                  let recording = Stdlib.Printexc.backtrace_status () in
                  Exn.protect
                    ~finally:(fun () -> Stdlib.Printexc.record_backtrace recording)
                    ~f:(fun () ->
                      Stdlib.Printexc.record_backtrace true;
                      Owner.with_delegation_resources_observed
                        owner
                        ~capture:(fun cleanup ->
                          assert (Option.is_none !captured);
                          captured := Some cleanup)
                        (fun _ ->
                           let exit, failures =
                             match callback () with
                             | Ok () -> CO.Returned (Ok ()), []
                             | Error error ->
                               ( CO.Returned (Error error)
                               , [ Agent_session.Callback_cleanup.Failure.Protocol error ]
                               )
                             | exception exn ->
                               let trace = Stdlib.Printexc.get_raw_backtrace () in
                               CO.Raised (exn, trace), [ Raised (exn, trace) ]
                           in
                           let diagnostics =
                             if List.is_empty failures
                             then []
                             else (
                               let group = D.create failures in
                               callback_group := Some group;
                               [ group ])
                           in
                           CO.create exit ~diagnostics ?primary:!callback_group ()))
                in
                assert (
                  Option.equal
                    Owner.Lease_outcome.Primary_stage.equal
                    (Owner.Lease_outcome.primary_stage report)
                    (Some Callback));
                let cleanup = Option.value_exn !captured in
                assert (phys_equal cleanup (Owner.Lease_outcome.cleanup report));
                let original_view =
                  R.original_view cleanup |> protocol_ok |> Option.value_exn
                in
                let cleanup_group =
                  match Agent_server.Cleanup_original_view.diagnostics original_view with
                  | [ group ] ->
                    (match D.failures group with
                     | [ Raised (actual, _) ] when phys_equal actual Lease_cleanup -> ()
                     | _ -> failwith "lease original view lost actual closer failure");
                    group
                  | _ -> failwith "lease original view duplicated actual closer failure"
                in
                let paired = Owner.Lease_outcome.observation report in
                let expected = Option.to_list !callback_group @ [ cleanup_group ] in
                assert (List.equal D.equal (CO.diagnostics paired) expected);
                let selected = Option.value !callback_group ~default:cleanup_group in
                assert (Option.equal D.equal (CO.primary paired) (Some selected));
                let actual_raised expected exn trace =
                  assert (phys_equal exn expected);
                  let producer_trace =
                    match D.failures selected with
                    | [ Raised (actual, producer_trace) ] ->
                      assert (phys_equal actual expected);
                      Stdlib.Printexc.raw_backtrace_to_string producer_trace
                    | _ -> failwith "selected lease origin lost its actual raw failure"
                  in
                  assert (not (String.is_empty producer_trace));
                  assert (
                    String.is_prefix
                      (Stdlib.Printexc.raw_backtrace_to_string trace)
                      ~prefix:producer_trace)
                in
                (match outcome, CO.exit paired with
                 | `Typed, Returned (Error error) -> assert (phys_equal error primary)
                 | `Raised, Raised (exn, trace) -> actual_raised Lease_primary exn trace
                 | `Success, Raised (exn, trace) -> actual_raised Lease_cleanup exn trace
                 | _ -> failwith "observed lease changed the actual selected raw exit");
                (* Resume the SAME actual reported lease through its old Scope
                 projection. New/duplicate wrapper groups fail above. *)
                Owner.Scope_outcome.resume (Owner.Lease_outcome.outcome report))
            in
            match result with
            | Error error ->
              [%test_eq: Sexp.t]
                (Agent_protocol.Error.sexp_of_t error)
                (Agent_protocol.Error.sexp_of_t primary);
              `Typed
            | Ok () -> failwith "cleanup failure was lost"
          with
          | Lease_primary -> `Raised
          | Lease_cleanup -> `Success
        in
        assert (
          match outcome, observed with
          | `Typed, `Typed | `Raised, `Raised | `Success, `Success -> true
          | _ -> false);
        [%test_eq: int] 1 !closes;
        [%test_eq: int] 1 !body_calls;
        assert (not (Owner.is_loaded owner));
        Owner.close_and_wait owner;
        Owner.close_and_wait owner;
        [%test_eq: int] 2 !closes)));
  print_endline
    "typed error and exception retained; cleanup raised only after success; failed close \
     retried exactly";
  [%expect
    {| typed error and exception retained; cleanup raised only after success; failed close retried exactly |}]
;;

let%expect_test "source report joins delayed cleanup and retains its hidden failure" =
  let module A = Agent_session.Session_actor in
  let module Scope = Owner.Scope_outcome in
  with_source_stage_actor (fun fixture ->
    let env, _workspace = fixture.environment in
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
      Eio.Switch.run (fun sw ->
        let closing, closing_u = Eio.Promise.create () in
        let release, release_u = Eio.Promise.create () in
        let release_close () =
          if Option.is_none (Eio.Promise.peek release)
          then Eio.Promise.resolve release_u ()
        in
        let closes = ref 0 in
        let owner =
          Owner.create
            ~actor:fixture.actor
            ~initial:
              (Some
                 (runtime
                    ~close:(fun () ->
                      Int.incr closes;
                      if Int.equal !closes 1
                      then (
                        Eio.Promise.resolve closing_u ();
                        Eio.Promise.await release;
                        raise Lease_cleanup))
                    ()))
            ~build:(fun () -> failwith "reported cleanup reloaded a runtime")
        in
        let preparation = source_stage_preparation fixture "reported-close-failure" in
        let primary =
          Agent_protocol.Error.create
            Invalid_request
            ~message:"actual source callback failure"
            ~retryable:false
            ~data:(`Object [ "retained", `Number "538" ])
            ()
        in
        let escaped = ref None in
        let report =
          Eio.Fiber.fork_promise ~sw (fun () ->
            Owner.with_prepared_source_reported
              owner
              ~preparation
              ~source:fixture.source
              (fun prepared ->
                 Owner.Prepared.check_basis prepared |> protocol_ok;
                 escaped := Some prepared;
                 (* Actual negative close cancels this lease. Preserve the
                   callback's selected Error while real protected finalizers
                   retire it; no effectful work is shielded from cancellation. *)
                 Eio.Cancel.protect (fun () ->
                   Owner.close owner;
                   Error primary)))
        in
        let capture f =
          match f () with
          | value -> Ok value
          | exception exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
        in
        let result =
          capture (fun () ->
            Eio.Fiber.first
              (fun () -> Eio.Promise.await closing)
              (fun () ->
                 let premature = Eio.Promise.await_exn report in
                 match Scope.primary premature with
                 | Raised (exn, backtrace) ->
                   Exn.raise_with_original_backtrace exn backtrace
                 | Returned (Error error) ->
                   raise_s
                     [%sexp
                       "source returned before actual close"
                     , (error : Agent_protocol.Error.t)]
                 | Returned (Ok _) -> failwith "source succeeded before actual close");
            [%test_eq: int] 1 !closes;
            assert (Option.is_none (Eio.Promise.peek report));
            (* The close holds the Owner mutex: observe only its already closed
               Prepared and the report Promise, never reenter the Owner here. *)
            assert (
              Result.is_error (Owner.Prepared.check_basis (Option.value_exn !escaped)));
            release_close ();
            let outcome = Eio.Promise.await_exn report in
            let exact_primary error =
              assert (
                Jsonaf.exactly_equal
                  (Agent_protocol.Error.to_json primary)
                  (Agent_protocol.Error.to_json error))
            in
            (match Scope.primary outcome with
             | Returned (Error error) -> exact_primary error
             | Returned (Ok _) -> failwith "source callback Error was lost"
             | Raised (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace);
            (match Scope.Cleanup.status (Scope.cleanup outcome) with
             | Failed (Raised (Lease_cleanup, _), []) -> ()
             | Failed _ -> failwith "report lost the exact inner cleanup failure"
             | Never_acquired | Joined ->
               failwith "successful outer source clear concealed failed inner close");
            (match Scope.resume outcome with
             | Error error -> exact_primary error
             | Ok _ -> failwith "reported cleanup replaced its primary Error");
            [%test_eq: int] 1 !closes;
            assert (not (Owner.is_loaded owner)))
        in
        let cleanup =
          capture (fun () ->
            Eio.Cancel.protect (fun () ->
              release_close ();
              ignore (Eio.Promise.await_exn report);
              Exn.protect
                ~finally:(fun () -> Owner.close_and_wait owner)
                ~f:(fun () ->
                  A.end_run_preparation fixture.actor preparation |> protocol_ok)))
        in
        match result, cleanup with
        | Error (exn, backtrace), _ | Ok (), Error (exn, backtrace) ->
          Exn.raise_with_original_backtrace exn backtrace
        | Ok (), Ok () -> [%test_eq: int] 2 !closes)));
  print_endline
    "no report before join; failed inner close survives outer clear; exact primary \
     retained";
  [%expect
    {| no report before join; failed inner close survives outer clear; exact primary retained |}]
;;

let%expect_test
    "loaded-only inspection never builds and releases failed or cancelled borrows"
  =
  with_actor (fun _env _sw actor _writer _backend ->
    let builds = ref 0 in
    let owner =
      Owner.create ~actor ~initial:None ~build:(fun () ->
        Int.incr builds;
        failwith "cold inspection invoked the builder")
    in
    Exn.protect
      ~finally:(fun () -> Owner.close_and_wait owner)
      ~f:(fun () ->
        assert (Result.is_error (Owner.with_loaded_runtime owner (fun _ -> Ok ())));
        [%test_eq: int] 0 !builds));
  with_actor (fun env sw actor _writer _backend ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
      let builds = ref 0 in
      let build () =
        Int.incr builds;
        failwith "loaded-only inspection invoked the builder"
      in
      let closes = ref 0 in
      let entered, entered_u = Eio.Promise.create () in
      let finish, finish_u = Eio.Promise.create () in
      let owner =
        Owner.create_with_unload
          ~actor
          ~initial:(Some (runtime ~close:(fun () -> Int.incr closes) ()))
          ~build
          ~before_unload:(fun ~closing:_ ->
            if Option.is_none (Eio.Promise.peek entered)
            then Eio.Promise.resolve entered_u ();
            Eio.Promise.await finish;
            Ok ())
      in
      let release () =
        if Option.is_none (Eio.Promise.peek finish) then Eio.Promise.resolve finish_u ()
      in
      Exn.protect
        ~finally:(fun () ->
          release ();
          Owner.close_and_wait owner)
        ~f:(fun () ->
          let failure =
            Agent_protocol.Error.create
              Permission_denied
              ~message:"fixture rejection"
              ~retryable:false
              ()
          in
          (match Owner.with_loaded_runtime owner (fun _ -> Error failure) with
           | Error actual ->
             [%test_eq: Agent_protocol.Error.code] failure.code actual.code;
             [%test_eq: string] failure.message actual.message;
             [%test_eq: bool] failure.retryable actual.retryable;
             assert (Document_schema.Json.equal failure.data actual.data)
           | Ok _ -> assert false);
          let borrowed, borrowed_u = Eio.Promise.create () in
          let never, _ = Eio.Promise.create () in
          let caller =
            Eio.Fiber.fork_promise ~sw (fun () ->
              Eio.Cancel.sub (fun context ->
                Owner.with_loaded_runtime owner (fun _ ->
                  Eio.Promise.resolve borrowed_u context;
                  Eio.Promise.await never;
                  Ok ())))
          in
          Eio.Cancel.cancel (Eio.Promise.await borrowed) Exit;
          (match Eio.Promise.await caller with
           | Error (Eio.Cancel.Cancelled _) -> ()
           | _ -> failwith "borrow lost caller cancellation");
          let retired =
            Eio.Fiber.fork_promise ~sw (fun () -> Owner.unload_and_wait owner)
          in
          Eio.Promise.await entered;
          assert (Result.is_error (Owner.with_loaded_runtime owner (fun _ -> Ok ())));
          [%test_eq: int] 0 !builds;
          [%test_eq: int] 0 !closes;
          release ();
          Eio.Promise.await_exn retired |> protocol_ok;
          [%test_eq: int] 1 !closes;
          assert (Result.is_error (Owner.with_loaded_runtime owner (fun _ -> Ok ())));
          [%test_eq: int] 0 !builds;
          print_endline
            "unloaded/retiring deny; no builder; Error/cancellation release; unload joins")));
  [%expect
    {| unloaded/retiring deny; no builder; Error/cancellation release; unload joins |}]
;;

let%expect_test "retired cleanup attempts every resource and preserves the first failure" =
  let module R = Owner.Cleanup_recovery in
  let module S = Owner.Scope_outcome in
  let module A = Agent_session.Session_actor in
  let module CO = Agent_session.Close_observation in
  let module D = Agent_session.Cleanup_diagnostic in
  let previous_backtraces = Stdlib.Printexc.backtrace_status () in
  Exn.protect
    ~finally:(fun () -> Stdlib.Printexc.record_backtrace previous_backtraces)
    ~f:(fun () ->
      Stdlib.Printexc.record_backtrace true;
      with_actor (fun env sw actor _writer _backend ->
        Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
          let first_closes = ref 0
          and second_closes = ref 0 in
          let first_succeeds = ref false
          and second_succeeds = ref false in
          let first_trace = ref None
          and second_trace = ref None in
          let sentinel = Failure "two actual retained resources" in
          let resource closes succeeds trace =
            runtime
              ~close:(fun () ->
                Int.incr closes;
                if not !succeeds
                then (
                  try raise sentinel with
                  | exn ->
                    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
                    trace := Some backtrace;
                    Exn.raise_with_original_backtrace exn backtrace))
              ()
          in
          let first = resource first_closes first_succeeds first_trace in
          let second = resource second_closes second_succeeds second_trace in
          let builds = ref 0 in
          let owner =
            Owner.create ~actor ~initial:(Some first) ~build:(fun () ->
              Int.incr builds;
              [%test_eq: int] 1 !builds;
              Ok second)
          in
          let releases = ref []
          and tasks = ref [] in
          let borrow expected ~capture =
            let entered, enter = Eio.Promise.create () in
            let release, release_u = Eio.Promise.create () in
            releases := release_u :: !releases;
            let finished =
              Eio.Fiber.fork_promise ~sw (fun () ->
                Owner.with_delegation_resources_outcome owner ~capture (fun actual ->
                  assert (phys_equal expected actual);
                  Eio.Cancel.protect (fun () ->
                    Eio.Promise.resolve enter ();
                    Eio.Promise.await release;
                    Ok ())))
            in
            tasks := finished :: !tasks;
            (match
               Eio.Fiber.first
                 (fun () ->
                    Eio.Promise.await entered;
                    `Entered)
                 (fun () -> `Finished (Eio.Promise.await_exn finished))
             with
             | `Entered -> ()
             | `Finished report ->
               S.resume (Owner.Lease_outcome.outcome report) |> protocol_ok;
               failwith "resource lease ended before its actual callback");
            release_u, finished
          in
          Exn.protect
            ~finally:(fun () ->
              Eio.Cancel.protect (fun () ->
                first_succeeds := true;
                second_succeeds := true;
                List.iter !releases ~f:(fun release ->
                  ignore (Eio.Promise.try_resolve release ()));
                let rec join_all = function
                  | [] -> ()
                  | task :: rest ->
                    Exn.protect
                      ~f:(fun () -> ignore (Eio.Promise.await_exn task))
                      ~finally:(fun () -> join_all rest)
                in
                Exn.protect
                  ~f:(fun () -> join_all !tasks)
                  ~finally:(fun () -> Owner.close_and_wait owner)))
            ~f:(fun () ->
              let first_release, first_finished = borrow first ~capture:ignore in
              Owner.unload_and_wait owner |> protocol_ok;
              let captured = ref None
              and captures = ref 0 in
              let second_release, second_finished =
                borrow second ~capture:(fun cleanup ->
                  Int.incr captures;
                  assert (Option.is_none !captured);
                  captured := Some cleanup)
              in
              Owner.unload_and_wait owner |> protocol_ok;
              [%test_eq: int] 0 !first_closes;
              [%test_eq: int] 0 !second_closes;
              Eio.Promise.resolve first_release ();
              let first_report = Eio.Promise.await_exn first_finished in
              (match
                 Result.try_with (fun () ->
                   S.resume (Owner.Lease_outcome.outcome first_report) |> protocol_ok)
               with
               | Error actual -> assert (phys_equal actual sentinel)
               | Ok () -> failwith "first actual close failure was forgotten");
              [%test_eq: int] 1 !first_closes;
              Owner.close owner;
              (* Close selected the Actor retirement. While B is still held, no
                 detach can clear it: duplicate retirement returns that SAME
                 foreground_finished promise, never a fresh completed receipt. *)
              let foreground =
                A.retire_runtime_worker actor ~closing:true |> protocol_ok
              in
              A.Runtime_retirement.await foreground |> protocol_ok;
              assert (A.Runtime_retirement.is_finished foreground);
              assert (Option.is_none (Eio.Promise.peek second_finished));
              [%test_eq: int] 0 !second_closes;
              Eio.Promise.resolve second_release ();
              let second_report = Eio.Promise.await_exn second_finished in
              (match S.primary (Owner.Lease_outcome.outcome second_report) with
               | Raised (Eio.Cancel.Cancelled _, backtrace) ->
                 assert (
                   not
                     (String.is_empty (Stdlib.Printexc.raw_backtrace_to_string backtrace)))
               | Returned _ | Raised _ ->
                 failwith "close cancellation primary was replaced");
              [%test_eq: int] 1 !captures;
              let cleanup = Option.value_exn !captured in
              assert (phys_equal cleanup (Owner.Lease_outcome.cleanup second_report));
              let original = R.original cleanup |> protocol_ok |> Option.value_exn in
              let snapshot = R.snapshot cleanup |> protocol_ok |> Option.value_exn in
              let original_view =
                R.original_view cleanup |> protocol_ok |> Option.value_exn
              in
              let original_diagnostics =
                Agent_server.Cleanup_original_view.diagnostics original_view
              in
              let observed snapshot = R.Snapshot.observation snapshot |> protocol_ok in
              let negative_observation snapshot =
                let observation = observed snapshot in
                let groups =
                  List.map (R.Snapshot.failures snapshot) ~f:(function
                    | Raised (actual, trace) ->
                      assert (phys_equal actual sentinel);
                      let matches =
                        List.filter (CO.diagnostics observation) ~f:(fun diagnostic ->
                          List.exists (D.failures diagnostic) ~f:(function
                            | Raised (actual, actual_trace) ->
                              phys_equal actual sentinel && phys_equal trace actual_trace
                            | Protocol _ -> false))
                      in
                      (match matches with
                       | [ diagnostic ] -> diagnostic
                       | _ ->
                         failwith "current failure lost or duplicated its actual producer")
                    | Protocol _ -> failwith "physical snapshot became protocol error")
                in
                (match R.Snapshot.failures snapshot, groups, CO.exit observation with
                 | ( Raised (actual, trace) :: _
                   , primary :: _
                   , CO.Raised (selected, selected_trace) ) ->
                   assert (phys_equal actual selected);
                   assert (phys_equal trace selected_trace);
                   assert (D.equal primary (Option.value_exn (CO.primary observation)))
                 | _ -> failwith "current observation did not select actual first failure");
                observation, groups
              in
              let initial_observation, initial_groups = negative_observation snapshot in
              (match initial_groups with
               | [ first; second ] -> assert (not (D.equal first second))
               | _ ->
                 failwith "distinct physical producers sharing an exception coalesced");
              let check_old_observation () =
                assert (phys_equal initial_observation (observed snapshot));
                assert (
                  List.equal
                    D.equal
                    original_diagnostics
                    (Agent_server.Cleanup_original_view.diagnostics original_view));
                assert (
                  List.equal
                    D.equal
                    original_diagnostics
                    (R.original_view cleanup
                     |> protocol_ok
                     |> Option.value_exn
                     |> Agent_server.Cleanup_original_view.diagnostics))
              in
              let initial_traces =
                [ Option.value_exn !second_trace; Option.value_exn !first_trace ]
              in
              let check_failures failures =
                List.iter2_exn
                  failures
                  initial_traces
                  ~f:(fun (failure : S.Cleanup.Failure.t) expected ->
                    match failure with
                    | Raised (actual, backtrace) ->
                      assert (phys_equal actual sentinel);
                      let expected = Stdlib.Printexc.raw_backtrace_to_string expected in
                      assert (not (String.is_empty expected));
                      assert (
                        String.is_prefix
                          (Stdlib.Printexc.raw_backtrace_to_string backtrace)
                          ~prefix:expected)
                    | Protocol _ ->
                      failwith "physical close failure became protocol error")
              in
              let original_failures =
                match S.Cleanup.status original with
                | Failed (first, rest) -> first :: rest
                | Never_acquired | Joined ->
                  failwith "actual original cleanup became positive"
              in
              let check_original () =
                assert (
                  phys_equal
                    original
                    (R.original cleanup |> protocol_ok |> Option.value_exn));
                check_failures original_failures
              in
              let check_snapshot () =
                assert (R.Snapshot.equal_progress (R.Snapshot.progress snapshot) Retryable);
                check_failures (R.Snapshot.failures snapshot);
                List.iter2_exn
                  original_failures
                  (R.Snapshot.failures snapshot)
                  ~f:(fun original current ->
                    match original, current with
                    | Raised (_, original_trace), Raised (_, current_trace) ->
                      assert (phys_equal original_trace current_trace)
                    | _ -> failwith "canonical cleanup changed original raw failure order")
              in
              check_original ();
              check_snapshot ();
              check_old_observation ();
              assert (
                phys_equal snapshot (R.snapshot cleanup |> protocol_ok |> Option.value_exn));
              [%test_eq: int] 2 !first_closes;
              [%test_eq: int] 1 !second_closes;
              [%test_eq: int] 1 !builds;
              assert (not (Owner.is_loaded owner));
              first_succeeds := true;
              let partial = R.retry cleanup ~owner ~after:snapshot |> protocol_ok in
              assert (R.Snapshot.equal_progress (R.Snapshot.progress partial) Retryable);
              (match R.Snapshot.failures partial with
               | [ Raised (actual, backtrace) ] ->
                 assert (phys_equal actual sentinel);
                 let expected =
                   Stdlib.Printexc.raw_backtrace_to_string
                     (Option.value_exn !second_trace)
                 in
                 assert (not (String.is_empty expected));
                 assert (
                   String.is_prefix
                     (Stdlib.Printexc.raw_backtrace_to_string backtrace)
                     ~prefix:expected)
               | _ -> failwith "partial retry lost B failure or retained successful A");
              let partial_observation, partial_groups = negative_observation partial in
              (match partial_groups with
               | [ current ] ->
                 List.iter initial_groups ~f:(fun original ->
                   assert (not (D.equal current original)))
               | _ -> failwith "partial retry did not retain its new failed producer");
              assert (
                phys_equal partial (R.snapshot cleanup |> protocol_ok |> Option.value_exn));
              check_old_observation ();
              check_original ();
              check_snapshot ();
              [%test_eq: int] 3 !first_closes;
              [%test_eq: int] 2 !second_closes;
              [%test_eq: int] 1 !builds;
              second_succeeds := true;
              let joined = R.retry cleanup ~owner ~after:partial |> protocol_ok in
              assert (R.Snapshot.equal_progress (R.Snapshot.progress joined) Joined);
              assert (List.is_empty (R.Snapshot.failures joined));
              let joined_observation = observed joined in
              (match CO.exit joined_observation with
               | Returned (Ok ()) -> ()
               | Returned (Error _) | Raised _ ->
                 failwith "joined actual cleanup retained a negative current exit");
              assert (Option.is_none (CO.primary joined_observation));
              assert (phys_equal joined_observation (observed joined));
              assert (phys_equal partial_observation (observed partial));
              ignore (negative_observation partial);
              check_old_observation ();
              (match Option.map (R.Snapshot.cleanup joined) ~f:S.Cleanup.status with
               | Some Joined -> ()
               | Some (Never_acquired | Failed _) | None ->
                 failwith "joined exact targets lack positive cleanup evidence");
              assert (
                phys_equal joined (R.snapshot cleanup |> protocol_ok |> Option.value_exn));
              check_original ();
              check_snapshot ();
              [%test_eq: int] 3 !first_closes;
              [%test_eq: int] 3 !second_closes;
              [%test_eq: int] 1 !builds;
              Owner.close_and_wait owner;
              [%test_eq: int] 3 !first_closes;
              [%test_eq: int] 3 !second_closes))));
  print_endline
    "a failed head does not starve later cleanup; successful resources never close again";
  [%expect
    {| a failed head does not starve later cleanup; successful resources never close again |}]
;;

let%expect_test
    "cleanup capture failure skips admission without inventing cleanup failure"
  =
  let module R = Owner.Cleanup_recovery in
  let module S = Owner.Scope_outcome in
  let module A = Agent_session.Session_actor in
  let previous_backtraces = Stdlib.Printexc.backtrace_status () in
  Exn.protect
    ~finally:(fun () -> Stdlib.Printexc.record_backtrace previous_backtraces)
    ~f:(fun () ->
      Stdlib.Printexc.record_backtrace true;
      with_actor (fun _env _sw actor _writer _backend ->
        let builds = ref 0
        and bodies = ref 0
        and closes = ref 0 in
        let captures = ref 0
        and captured = ref None
        and raised_trace = ref None in
        let sentinel = Failure "actual cleanup capture failed" in
        let owner =
          Owner.create ~actor ~initial:None ~build:(fun () ->
            Int.incr builds;
            Ok (runtime ~close:(fun () -> Int.incr closes) ()))
        in
        Exn.protect
          ~finally:(fun () -> Owner.close_and_wait owner)
          ~f:(fun () ->
            let before = A.state actor |> protocol_ok in
            let report =
              Owner.with_delegation_resources_outcome
                owner
                ~capture:(fun cleanup ->
                  Int.incr captures;
                  captured := Some cleanup;
                  try raise sentinel with
                  | exn ->
                    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
                    raised_trace := Some backtrace;
                    Exn.raise_with_original_backtrace exn backtrace)
                (fun _ ->
                   Int.incr bodies;
                   Ok ())
            in
            assert (
              Option.equal
                Owner.Lease_outcome.Primary_stage.equal
                (Owner.Lease_outcome.primary_stage report)
                (Some Admission));
            (match S.primary (Owner.Lease_outcome.outcome report) with
             | Raised (actual, backtrace) ->
               assert (phys_equal actual sentinel);
               let expected =
                 Stdlib.Printexc.raw_backtrace_to_string (Option.value_exn !raised_trace)
               in
               assert (not (String.is_empty expected));
               assert (
                 String.is_prefix
                   (Stdlib.Printexc.raw_backtrace_to_string backtrace)
                   ~prefix:expected)
             | Returned _ -> failwith "actual capture exception was not preserved");
            [%test_eq: int] 1 !captures;
            [%test_eq: int] 0 !builds;
            [%test_eq: int] 0 !bodies;
            [%test_eq: int] 0 !closes;
            assert (not (Owner.is_loaded owner));
            assert_same_session_snapshot before (A.state actor |> protocol_ok);
            let cleanup = Option.value_exn !captured in
            assert (phys_equal cleanup (Owner.Lease_outcome.cleanup report));
            let original = R.original cleanup |> protocol_ok |> Option.value_exn in
            assert (phys_equal original (S.cleanup (Owner.Lease_outcome.outcome report)));
            (match S.Cleanup.status original with
             | Never_acquired -> ()
             | Joined | Failed _ ->
               failwith "capture failure manufactured an acquired cleanup");
            let snapshot = R.snapshot cleanup |> protocol_ok |> Option.value_exn in
            assert (R.Snapshot.equal_progress (R.Snapshot.progress snapshot) Joined);
            assert (List.is_empty (R.Snapshot.failures snapshot));
            assert (
              phys_equal original (R.original cleanup |> protocol_ok |> Option.value_exn));
            assert (
              phys_equal snapshot (R.snapshot cleanup |> protocol_ok |> Option.value_exn)))));
  print_endline
    "capture raw primary retained; admission and body skipped; no cleanup invented";
  [%expect
    {| capture raw primary retained; admission and body skipped; no cleanup invented |}]
;;

let%expect_test "cleanup retry reserves exact ownership and defers observer cancellation" =
  let module R = Owner.Cleanup_recovery in
  let module S = Owner.Scope_outcome in
  with_actor (fun env sw actor _writer _backend ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
      let entered, enter = Eio.Promise.create () in
      let release_body, release_body_u = Eio.Promise.create () in
      let retry_entered, retry_enter = Eio.Promise.create () in
      let release_close, release_close_u = Eio.Promise.create () in
      let context, context_u = Eio.Promise.create () in
      let closes = ref 0
      and builds = ref 0
      and wrong_builds = ref 0 in
      let allow_close = ref false
      and captured = ref None
      and captures = ref 0 in
      let borrowed = ref None
      and retrying = ref None
      and returned = ref None in
      let sentinel = Failure "actual retained close failed" in
      let selected =
        runtime
          ~close:(fun () ->
            Int.incr closes;
            if Int.equal !closes 2
            then (
              Eio.Promise.resolve retry_enter ();
              Eio.Promise.await release_close);
            if not !allow_close then raise sentinel)
          ()
      in
      let owner =
        Owner.create ~actor ~initial:(Some selected) ~build:(fun () ->
          Int.incr builds;
          failwith "cleanup retry rebuilt the runtime")
      in
      (* A genuine different Owner tag on the SAME Actor isolates affinity
         refusal; this empty Owner owns no resources or callback duties. *)
      let wrong_owner =
        Owner.create ~actor ~initial:None ~build:(fun () ->
          Int.incr wrong_builds;
          failwith "foreign cleanup built a runtime")
      in
      let release promise = ignore (Eio.Promise.try_resolve promise ()) in
      let join task =
        Option.iter task ~f:(fun task -> ignore (Eio.Promise.await_exn task))
      in
      Exn.protect
        ~finally:(fun () ->
          Eio.Cancel.protect (fun () ->
            allow_close := true;
            release release_body_u;
            release release_close_u;
            Exn.protect
              ~f:(fun () -> join !borrowed)
              ~finally:(fun () ->
                Exn.protect
                  ~f:(fun () -> join !retrying)
                  ~finally:(fun () -> Owner.close_and_wait owner))))
        ~f:(fun () ->
          let task =
            Eio.Fiber.fork_promise ~sw (fun () ->
              Owner.with_delegation_resources_outcome
                owner
                ~capture:(fun cleanup ->
                  Int.incr captures;
                  captured := Some cleanup)
                (fun actual ->
                   assert (phys_equal actual selected);
                   Eio.Promise.resolve enter ();
                   Eio.Promise.await release_body;
                   Ok ()))
          in
          borrowed := Some task;
          (match
             Eio.Fiber.first
               (fun () ->
                  Eio.Promise.await entered;
                  `Entered)
               (fun () -> `Finished (Eio.Promise.await_exn task))
           with
           | `Entered -> ()
           | `Finished report ->
             S.resume (Owner.Lease_outcome.outcome report) |> protocol_ok;
             failwith "resource borrow ended before its actual body gate");
          [%test_eq: int] 1 !captures;
          let cleanup = Option.value_exn !captured in
          assert (Option.is_none (R.original cleanup |> protocol_ok));
          assert (Option.is_none (R.snapshot cleanup |> protocol_ok));
          Owner.unload_and_wait owner |> protocol_ok;
          assert (Option.is_none (R.original cleanup |> protocol_ok));
          assert (Option.is_none (R.snapshot cleanup |> protocol_ok));
          release release_body_u;
          let report = Eio.Promise.await_exn task in
          assert (phys_equal cleanup (Owner.Lease_outcome.cleanup report));
          (match S.primary (Owner.Lease_outcome.outcome report) with
           | Returned (Ok ()) -> ()
           | Returned (Error _) | Raised _ ->
             failwith "cleanup replaced actual body result");
          let original = R.original cleanup |> protocol_ok |> Option.value_exn in
          assert (phys_equal original (S.cleanup (Owner.Lease_outcome.outcome report)));
          (match S.Cleanup.status original with
           | Failed (Raised (actual, _), []) -> assert (phys_equal actual sentinel)
           | Never_acquired | Joined | Failed _ ->
             failwith "original physical cleanup failure was not retained");
          let initial = R.snapshot cleanup |> protocol_ok |> Option.value_exn in
          let negative snapshot =
            assert (R.Snapshot.equal_progress (R.Snapshot.progress snapshot) Retryable);
            match R.Snapshot.failures snapshot with
            | [ Raised (actual, _) ] -> assert (phys_equal actual sentinel)
            | _ -> failwith "observer cancellation became a physical cleanup failure"
          in
          negative initial;
          let initial_observation = R.Snapshot.observation initial |> protocol_ok in
          [%test_eq: int] 1 !closes;
          let refused = function
            | Error _ -> ()
            | Ok _ -> failwith "invalid cleanup observation acquired work"
          in
          let cached snapshot =
            assert (
              phys_equal original (R.original cleanup |> protocol_ok |> Option.value_exn));
            assert (
              phys_equal snapshot (R.snapshot cleanup |> protocol_ok |> Option.value_exn));
            assert (
              phys_equal
                initial_observation
                (R.Snapshot.observation initial |> protocol_ok));
            [%test_eq: int] 2 !closes;
            [%test_eq: int] 0 !builds;
            [%test_eq: int] 0 !wrong_builds
          in
          let retry =
            Eio.Fiber.fork_promise ~sw (fun () ->
              try
                Eio.Cancel.sub (fun caller ->
                  Eio.Promise.resolve context_u caller;
                  let snapshot = R.retry cleanup ~owner ~after:initial |> protocol_ok in
                  returned := Some snapshot;
                  (* Establish an explicit cancellation observation boundary rather
                 than prescribing where a protected API must deliver it. *)
                  Eio.Fiber.check ();
                  false)
              with
              | Eio.Cancel.Cancelled _ -> true)
          in
          retrying := Some retry;
          (match
             Eio.Fiber.first
               (fun () ->
                  Eio.Promise.await retry_entered;
                  `Entered)
               (fun () -> `Finished (Eio.Promise.await_exn retry))
           with
           | `Entered -> ()
           | `Finished _ -> failwith "retry ended before actual close gate");
          cached initial;
          refused (R.retry cleanup ~owner ~after:initial);
          cached initial;
          Eio.Cancel.cancel (Eio.Promise.await context) Exit;
          Eio.Fiber.yield ();
          assert (Option.is_none (Eio.Promise.peek retry));
          cached initial;
          release release_close_u;
          assert (Eio.Promise.await_exn retry);
          let latest = R.snapshot cleanup |> protocol_ok |> Option.value_exn in
          assert (not (phys_equal initial latest));
          Option.iter !returned ~f:(fun returned -> assert (phys_equal latest returned));
          negative latest;
          negative initial;
          cached latest;
          (* These refusals run after the reservation ended, so the busy guard
             cannot mask stale or wrong-Owner validation. *)
          refused (R.retry cleanup ~owner ~after:initial);
          cached latest;
          refused (R.retry cleanup ~owner:wrong_owner ~after:latest);
          cached latest;
          Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () ->
            refused (R.original cleanup);
            refused (R.snapshot cleanup);
            refused (R.retry cleanup ~owner ~after:latest));
          cached latest;
          allow_close := true;
          let joined = R.retry cleanup ~owner ~after:latest |> protocol_ok in
          assert (R.Snapshot.equal_progress (R.Snapshot.progress joined) Joined);
          assert (List.is_empty (R.Snapshot.failures joined));
          negative initial;
          negative latest;
          assert (
            phys_equal original (R.original cleanup |> protocol_ok |> Option.value_exn));
          [%test_eq: int] 3 !closes;
          [%test_eq: int] 0 !builds;
          [%test_eq: int] 0 !wrong_builds;
          Owner.close_and_wait owner;
          [%test_eq: int] 3 !closes)));
  print_endline
    "cached evidence survives refusals; cancelled retry finishes actual cleanup; same \
     target joins";
  [%expect
    {| cached evidence survives refusals; cancelled retry finishes actual cleanup; same target joins |}]
;;

let%expect_test
    "retained close joins outside the Owner mutex and retries the same resource"
  =
  with_actor (fun env sw actor _writer _backend ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
      let entered, enter = Eio.Promise.create () in
      let released, release = Eio.Promise.create () in
      let attempts = ref 0
      and builds = ref 0 in
      let failure = Failure "selected retained close" in
      let selected =
        runtime
          ~close:(fun () ->
            Int.incr attempts;
            if Int.equal !attempts 1
            then (
              Eio.Promise.resolve enter ();
              Eio.Promise.await released;
              raise failure))
          ()
      in
      let owner =
        Owner.create ~actor ~initial:(Some selected) ~build:(fun () ->
          Int.incr builds;
          failwith "pending cleanup permitted a replacement runtime")
      in
      let release_close () =
        if Option.is_none (Eio.Promise.peek released) then Eio.Promise.resolve release ()
      in
      Exn.protect
        ~finally:(fun () ->
          release_close ();
          Owner.close_and_wait owner)
        ~f:(fun () ->
          let first =
            Eio.Fiber.fork_promise ~sw (fun () ->
              Result.try_with (fun () -> Owner.unload owner |> protocol_ok))
          in
          Eio.Promise.await entered;
          let second_entered, second_enter = Eio.Promise.create () in
          let second =
            Eio.Fiber.fork_promise ~sw (fun () ->
              Eio.Promise.resolve second_enter ();
              Result.try_with (fun () -> Owner.unload owner |> protocol_ok))
          in
          Eio.Promise.await second_entered;
          let observed = Eio.Fiber.fork_promise ~sw (fun () -> Owner.is_loaded owner) in
          (* A failed old implementation still releases its protected close:
             time out only this promise wait, never the Owner observation fiber. *)
          let progress =
            try
              Some
                (Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 1. (fun () ->
                   Eio.Promise.await_exn observed))
            with
            | Eio.Time.Timeout -> None
          in
          (match progress with
           | None -> ()
           | Some loaded ->
             assert (not loaded);
             [%test_eq: int] 1 !attempts;
             assert (Option.is_none (Eio.Promise.peek first));
             assert (Option.is_none (Eio.Promise.peek second));
             assert (not (Owner.source_lane_idle owner));
             assert (not (Owner.reserve_inactive_close owner));
             [%test_eq: bool option]
               None
               (Owner.with_unloaded owner (fun () -> Ok true) |> protocol_ok);
             (match Owner.ensure_loaded owner with
              | Error { code = Conflict; _ } -> ()
              | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
              | Ok () -> failwith "fresh runtime bypassed retained cleanup"));
          release_close ();
          let assert_failure result =
            match result with
            | Error actual -> assert (phys_equal failure actual)
            | Ok () -> failwith "joined close lost the original failure"
          in
          Eio.Promise.await_exn first |> assert_failure;
          Eio.Promise.await_exn second |> assert_failure;
          [%test_eq: bool option] (Some false) progress;
          [%test_eq: int] 1 !attempts;
          [%test_eq: int] 0 !builds;
          Owner.unload owner |> protocol_ok;
          [%test_eq: int] 2 !attempts;
          [%test_eq: int] 0 !builds;
          assert (Owner.source_lane_idle owner))));
  print_endline "reads progress; concurrent closers share failure; exact retained retry";
  [%expect {| reads progress; concurrent closers share failure; exact retained retry |}]
;;

let%expect_test
    "accepted retirement invalidates an administration suspended in resource close"
  =
  with_actor (fun env sw actor _writer _backend ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
      let closing, close_enter = Eio.Promise.create () in
      let closed, close_release = Eio.Promise.create () in
      let retiring, retire_enter = Eio.Promise.create () in
      let retired, retire_release = Eio.Promise.create () in
      let commits = ref 0
      and initializes = ref 0
      and builds = ref 0
      and closes = ref 0 in
      let owner =
        Owner.create_with_unload
          ~actor
          ~initial:
            (Some
               (runtime
                  ~close:(fun () ->
                    Int.incr closes;
                    Eio.Promise.resolve close_enter ();
                    Eio.Promise.await closed)
                  ()))
          ~before_unload:(fun ~closing:_ ->
            if Option.is_none (Eio.Promise.peek retiring)
            then Eio.Promise.resolve retire_enter ();
            Eio.Promise.await retired;
            Ok ())
          ~build:(fun () ->
            Int.incr builds;
            failwith "retired administration built a runtime")
      in
      let release promise resolver =
        if Option.is_none (Eio.Promise.peek promise) then Eio.Promise.resolve resolver ()
      in
      Exn.protect
        ~finally:(fun () ->
          release closed close_release;
          release retired retire_release;
          Owner.close_and_wait owner)
        ~f:(fun () ->
          let admin =
            Eio.Fiber.fork_promise ~sw (fun () ->
              Owner.reinitialize_administration
                owner
                ~validate:(fun () -> Ok ())
                ~commit:(fun () ->
                  Int.incr commits;
                  Ok ())
                ~before_initialize:(fun () -> Int.incr initializes))
          in
          Eio.Promise.await closing;
          let stop = Eio.Fiber.fork_promise ~sw (fun () -> Owner.unload_and_wait owner) in
          (* This is the existing actual retirement callback, reached only after
             accepted Owner retirement and foreground cancellation, not a fake
             marker before a possibly rejected request. *)
          Eio.Promise.await retiring;
          release closed close_release;
          (match Eio.Promise.await_exn admin with
           | Error { code = Conflict; _ } -> ()
           | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
           | Ok _ -> failwith "intervening retirement revived selected administration");
          [%test_eq: int] 0 !commits;
          [%test_eq: int] 0 !initializes;
          [%test_eq: int] 0 !builds;
          release retired retire_release;
          Eio.Promise.await_exn stop |> protocol_ok;
          [%test_eq: int] 1 !closes;
          assert (not (Owner.is_loaded owner)))));
  print_endline
    "accepted retirement wins; no selected commit, initialization, or replacement";
  [%expect
    {| accepted retirement wins; no selected commit, initialization, or replacement |}]
;;

let%test_unit "quiescent retention defers to a lifecycle holder waiting for its Actor" =
  with_actor (fun env sw actor _writer _backend ->
    let module Registry = Agent_server.Session_registry in
    let module Retention = Agent_server.Session_retention in
    let registry = Registry.create () in
    let retention = Retention.create () in
    let released, release = Eio.Promise.create () in
    let helpers = ref [] in
    let holder_context, holder_control = Eio.Promise.create () in
    let cleanup ~cancel_holder () =
      if cancel_holder
      then
        Option.iter (Eio.Promise.peek holder_context) ~f:(fun context ->
          Eio.Cancel.cancel context Exit);
      if not (Eio.Promise.is_resolved released) then Eio.Promise.resolve release ();
      let failures = ref [] in
      let join f =
        match f () with
        | () -> ()
        | exception exn ->
          failures := (exn, Stdlib.Printexc.get_raw_backtrace ()) :: !failures
      in
      join (fun () -> Retention.close_and_wait retention);
      List.iter !helpers ~f:(fun helper ->
        join (fun () -> Eio.Promise.await_exn helper |> protocol_ok));
      match List.last !failures with
      | None -> ()
      | Some (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
    in
    let body () =
      Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
        let session_id = (A.state actor |> protocol_ok).identity.session_id in
        let occupied, occupy = Eio.Promise.create () in
        let checkpoint =
          Eio.Fiber.fork_promise ~sw (fun () ->
            A.checkpoint actor ~persist:(fun _ ->
              Eio.Promise.resolve occupy ();
              Eio.Promise.await released;
              Ok ()))
        in
        helpers := checkpoint :: !helpers;
        Eio.Promise.await occupied;
        let reserved, reserve = Eio.Promise.create () in
        let deferred, defer = Eio.Promise.create () in
        let holder =
          Eio.Fiber.fork_promise ~sw (fun () ->
            Eio.Cancel.sub (fun context ->
              Eio.Promise.resolve holder_control context;
              Registry.with_lifecycle registry session_id (fun _ ->
                Eio.Promise.resolve reserve ();
                match A.state actor with
                | Error error -> Error error
                | Ok _ ->
                  (* Keep the actual reservation even if this Actor read wins
                     scheduling: retention must witness this same held owner. *)
                  Eio.Promise.await deferred;
                  Ok ())))
        in
        helpers := holder :: !helpers;
        Eio.Promise.await reserved;
        let entered, enter = Eio.Promise.create () in
        let completed, complete = Eio.Promise.create () in
        Retention.bind_exn
          retention
          ~sw
          ~clock:(Eio.Stdenv.mono_clock env)
          ~attempt:(fun () ->
            if not (Eio.Promise.is_resolved entered) then Eio.Promise.resolve enter ();
            Eio.Cancel.protect (fun () ->
              A.with_quiescent_state actor ~f:(fun _ ->
                (* Lock-order oracle only: no installed entry or Store Handle,
                 so this test neither constructs nor exercises deletion authority. *)
                match Registry.with_lifecycle registry session_id (fun _ -> Ok ()) with
                | Error { code = Conflict; _ } ->
                  Eio.Promise.resolve defer ();
                  Ok Retention.Deferred
                | Error error -> Error error
                | Ok () ->
                  Eio.Promise.resolve complete ();
                  Ok Retention.Completed)
              |> Result.map ~f:(function
                | None -> Retention.Deferred
                | Some result -> result)));
        Retention.request retention;
        Eio.Promise.await entered;
        Eio.Promise.resolve release ();
        Eio.Promise.await deferred;
        Eio.Promise.await_exn holder |> protocol_ok;
        Eio.Promise.await_exn checkpoint |> protocol_ok;
        Eio.Promise.await completed)
    in
    match body () with
    | () -> Eio.Cancel.protect (cleanup ~cancel_holder:false)
    | exception exn ->
      let backtrace = Stdlib.Printexc.get_raw_backtrace () in
      (try Eio.Cancel.protect (cleanup ~cancel_holder:true) with
       | _ -> ());
      Exn.raise_with_original_backtrace exn backtrace)
;;

let%expect_test "timer deferral is separate from admitted errors and lease interruption" =
  let module P = Agent_protocol in
  let module A = Agent_session.Session_actor in
  with_actor (fun _env sw actor _writer _backend ->
    let initial = A.state actor |> protocol_ok in
    let schedule : P.Schedule.t =
      { id = P.Id.Schedule.create ()
      ; session_id = initial.identity.session_id
      ; generation = initial.identity.generation
      ; payload = `Null
      ; created_at = timestamp
      ; next_due_at = timestamp
      ; misfire = Deliver_once_immediately
      ; status = Scheduled
      ; delivery_count = 0
      ; last_delivery_at = None
      ; ownership = None
      ; inference_provenance = None
      ; delivery_cancellation = None
      }
    in
    A.add_schedule actor schedule |> protocol_ok |> ignore;
    let claimed =
      A.claim_schedule actor ~schedule_id:schedule.id ~generation:schedule.generation
      |> protocol_ok
      |> Option.value_exn
    in
    let before = A.state actor |> protocol_ok in
    let stopping_entered, stopping_entered_u = Eio.Promise.create () in
    let release_stop, release_stop_u = Eio.Promise.create () in
    let enqueue_entered, enqueue_entered_u = Eio.Promise.create () in
    let never, _ = Eio.Promise.create () in
    let stop_gate_armed = ref true in
    let mode = ref `Reject in
    let enqueues = ref 0 in
    let closes = ref 0 in
    let transient =
      P.Error.create
        Conflict
        ~message:"actual admitted timer callback refuses its work"
        ~retryable:true
        ~data:(`String "must remain the same error")
        ()
    in
    let make_runtime () =
      { (runtime ~close:(fun () -> Int.incr closes) ()) with
        enqueue_internal_event =
          (fun ?prepare:_ _ ->
            Int.incr enqueues;
            match !mode with
            | `Reject -> Error transient
            | `Wait ->
              (* This callback is reached only after the real Owner lease and
                 Actor checkpoint gate have both been acquired. It never calls
                 prepare, so this cancellation cut has no queue/Store ACK. *)
              Eio.Promise.resolve enqueue_entered_u ();
              Eio.Promise.await never;
              Error transient)
      }
    in
    let owner =
      Owner.create_with_unload
        ~actor
        ~initial:(Some (make_runtime ()))
        ~build:(fun () -> Ok (make_runtime ()))
        ~before_unload:(fun ~closing:_ ->
          if !stop_gate_armed
          then (
            Eio.Promise.resolve stopping_entered_u ();
            Eio.Promise.await release_stop;
            stop_gate_armed := false);
          Ok ())
    in
    let stopping = ref None in
    let delivery = ref None in
    let delivery_cancel = ref None in
    let release_stop () =
      if Option.is_none (Eio.Promise.peek release_stop)
      then Eio.Promise.resolve release_stop_u ()
    in
    let capture f =
      match f () with
      | result -> Ok result
      | exception exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
    in
    let assert_no_delivery (before : Agent_session.Session_state.t) =
      let after = A.state actor |> protocol_ok in
      [%test_eq: int64] before.counters.revision after.counters.revision;
      assert (Option.equal Jsonaf.exactly_equal before.moderator after.moderator);
      match after.schedules with
      | [ { status = Delivering; delivery_count = 0; last_delivery_at = None; _ } ] -> ()
      | _ -> failwith "an uncommitted timer changed its durable delivery"
    in
    let primary =
      capture (fun () ->
        let stopping_work =
          Eio.Fiber.fork_promise ~sw (fun () -> Owner.unload_and_wait owner)
        in
        stopping := Some stopping_work;
        Eio.Promise.await stopping_entered;
        let deferred =
          match Owner.deliver_schedule_attempt owner claimed |> protocol_ok with
          | Deferred error -> error
          | Delivered -> failwith "retirement admitted a new timer delivery"
        in
        [%test_eq: P.Error.code] Conflict deferred.code;
        assert deferred.retryable;
        (match Owner.deliver_schedule owner claimed with
         | Error error ->
           [%test_eq: Sexp.t] (P.Error.sexp_of_t deferred) (P.Error.sexp_of_t error)
         | Ok () -> failwith "the legacy delivery API lost its original error");
        [%test_eq: int] 0 !enqueues;
        assert_no_delivery before;
        release_stop ();
        Eio.Promise.await_exn stopping_work |> protocol_ok;
        stopping := None;
        Owner.ensure_loaded owner |> protocol_ok;
        let after_reload = A.state actor |> protocol_ok in
        (match Owner.deliver_schedule_attempt owner claimed with
         | Error error ->
           [%test_eq: Sexp.t] (P.Error.sexp_of_t transient) (P.Error.sexp_of_t error)
         | Ok (Delivered | Deferred _) ->
           failwith "an admitted callback error was classified as deferral");
        [%test_eq: int] 1 !enqueues;
        assert_no_delivery after_reload;
        mode := `Wait;
        let actual_delivery =
          Eio.Fiber.fork_promise ~sw (fun () ->
            capture (fun () ->
              Eio.Cancel.sub (fun context ->
                delivery_cancel := Some context;
                Exn.protect
                  ~finally:(fun () -> delivery_cancel := None)
                  ~f:(fun () -> Owner.deliver_schedule owner claimed))))
        in
        delivery := Some actual_delivery;
        Eio.Promise.await enqueue_entered;
        (* This is positive entry evidence, rather than a started-before-call
           promise or a scheduler yield. Read-only Owner queries still progress. *)
        Owner.ensure_loaded owner |> protocol_ok;
        assert (Owner.is_loaded owner);
        Owner.unload_and_wait owner |> protocol_ok;
        (match Eio.Promise.await_exn actual_delivery with
         | Ok (Error { code = Interrupted; _ }) -> ()
         | Ok (Ok ()) | Ok (Error _) | Error _ ->
           failwith "the admitted timer lease did not return Interrupted");
        delivery := None;
        [%test_eq: int] 2 !enqueues;
        [%test_eq: int] 2 !closes;
        assert_no_delivery after_reload;
        print_endline
          "clean unacquired refusal deferred; entered error preserved; actual timer \
           lease interrupted and joined without queue commit")
    in
    let cleanup =
      Eio.Cancel.protect (fun () ->
        List.map
          [ release_stop
          ; (fun () ->
              Option.iter !delivery_cancel ~f:(fun context ->
                Eio.Cancel.cancel context Exit))
          ; (fun () -> Owner.close_and_wait owner)
          ; (fun () ->
              Option.iter !stopping ~f:(fun pending ->
                Eio.Promise.await_exn pending |> protocol_ok))
          ; (fun () ->
              Option.iter !delivery ~f:(fun pending ->
                match Eio.Promise.await_exn pending with
                | Ok _ | Error (Eio.Cancel.Cancelled _, _) -> ()
                | Error (exn, backtrace) ->
                  Exn.raise_with_original_backtrace exn backtrace))
          ]
          ~f:capture)
    in
    let failures = List.filter_map cleanup ~f:Result.error in
    match primary, failures with
    | Ok (), [] -> ()
    | Ok (), (exn, backtrace) :: _ -> Exn.raise_with_original_backtrace exn backtrace
    | Error (exn, backtrace), failures ->
      List.iter failures ~f:(fun (cleanup, cleanup_backtrace) ->
        try
          Eio.traceln
            "timer fixture cleanup: %s\n%s"
            (Exn.to_string cleanup)
            (Stdlib.Printexc.raw_backtrace_to_string cleanup_backtrace)
        with
        | _ -> ());
      Exn.raise_with_original_backtrace exn backtrace);
  [%expect
    {| clean unacquired refusal deferred; entered error preserved; actual timer lease interrupted and joined without queue commit |}]
;;

let%expect_test "ordinary warm timer preparation defers only at its pure busy guard" =
  let module P = Agent_protocol in
  let module A = Agent_session.Session_actor in
  let module Source_owner = Agent_session.Runtime_source_owner in
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let source_owner = Source_owner.create () in
      let initial =
        actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
      in
      let backend =
        Agent_session.Memory_backend.create ~event_capacity:128 ~initial_state:initial
      in
      let actor =
        A.create_with_runtime_source_owner
          ~runtime_source_owner:(Source_owner.identity source_owner)
          ~ordinary_execution_admission:(fun () -> Ok ())
          ~schedule_permission_timeouts:false
          ~sw
          ~clock:(Eio.Stdenv.clock env)
          ~mailbox_capacity:32
          ~owner_lease_duration_ms:60_000
          ~max_attachments:4
          ~subscriber_capacity:8
          ~compaction_env:None
          ~initial_state:initial
          ~persistence:(Agent_session.Memory_backend.persistence backend)
          ~operation_worker:None
          ~services:
            { now = (fun () -> timestamp)
            ; monotonic_now = (fun () -> Mtime.min_stamp)
            ; create_attachment_id = P.Id.Attachment.create
            ; create_reclaim_token = (fun () -> "warm-timer-fixture")
            ; job_results = None
            ; schedule_limits = Agent_session.Staged_schedules.default_limits
            ; notification_limits = Agent_session.Staged_notifications.default_limits
            ; ingress_limits = Agent_session.Staged_ingress.default_limits
            ; goal_descendant_profile
            ; subscription_limits = Agent_session.Staged_subscriptions.default_limits
            ; state_committed = (fun _ _ -> ())
            }
      in
      Exn.protect
        ~finally:(fun () -> A.shutdown actor)
        ~f:(fun () ->
          assert (
            Source_owner.matches
              source_owner
              (A.runtime_source_owner_identity actor |> Option.value_exn));
          let stopping_entered, stopping_entered_u = Eio.Promise.create () in
          let release_stop, release_stop_u = Eio.Promise.create () in
          let stop_gate_armed = ref true in
          let warm_calls = ref 0 in
          let build_calls = ref 0 in
          let raise_warm = ref false in
          let warm_failure = Failure "actual warm preflight raised" in
          let original_warm_backtrace = ref None in
          let later_refusal =
            P.Error.create
              Conflict
              ~message:"actual warm preparation rejected after entering its source stage"
              ~retryable:true
              ~data:(`String "later preparation errors are not deferral evidence")
              ()
          in
          let owner =
            Owner.create_with_deferred_run_unload
              ~native_resource_access:None
              ~source_owner
              ~before_unload:(fun ~closing:_ ->
                if !stop_gate_armed
                then (
                  Eio.Promise.resolve stopping_entered_u ();
                  Eio.Promise.await release_stop;
                  stop_gate_armed := false);
                Ok ())
              ~policy_resource_cleanup:
                (Owner.Policy_resource_cleanup.create
                   ~pending:(fun () -> false)
                   ~close_pending:(fun () -> Ok ()))
              ~policy_resource_access:None
              ~actor
              ~initial:None
              ~build:(fun ~initialization_cleanup:_ ~original_actor:_ _ ->
                Int.incr build_calls;
                failwith "pure warm refusal reached the ordinary constructor")
              ~build_deferred:
                (fun
                  ~initialization_cleanup:_
                  ~source_consumer:_
                  ~source:_
                  ~expected_product:_
                  ~construction:_ ->
                Int.incr build_calls;
                failwith "pure warm refusal reached the deferred constructor")
              ~ordinary_warm:
                (Some
                   (fun ~expected_product:_ ~original_actor:_ ~run_preparation:_ ->
                     Int.incr warm_calls;
                     if !raise_warm
                     then (
                       try raise warm_failure with
                       | exn ->
                         let backtrace = Stdlib.Printexc.get_raw_backtrace () in
                         original_warm_backtrace := Some backtrace;
                         Exn.raise_with_original_backtrace exn backtrace)
                     else Error later_refusal))
              ~delegated_load:None
          in
          let stopping = ref None in
          let release_stop () =
            if not (Eio.Promise.is_resolved release_stop)
            then Eio.Promise.resolve release_stop_u ()
          in
          let capture f =
            match f () with
            | result -> Ok result
            | exception exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
          in
          let primary =
            capture (fun () ->
              Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
                let schedule : P.Schedule.t =
                  { id = P.Id.Schedule.create ()
                  ; session_id = initial.identity.session_id
                  ; generation = initial.identity.generation
                  ; payload = `Null
                  ; created_at = timestamp
                  ; next_due_at = timestamp
                  ; misfire = Deliver_once_immediately
                  ; status = Scheduled
                  ; delivery_count = 0
                  ; last_delivery_at = None
                  ; ownership = None
                  ; inference_provenance = None
                  ; delivery_cancellation = None
                  }
                in
                A.add_schedule actor schedule |> protocol_ok |> ignore;
                let claimed =
                  A.claim_schedule
                    actor
                    ~schedule_id:schedule.id
                    ~generation:schedule.generation
                  |> protocol_ok
                  |> Option.value_exn
                in
                let actual_stop =
                  Eio.Fiber.fork_promise ~sw (fun () -> Owner.unload_and_wait owner)
                in
                stopping := Some actual_stop;
                Eio.Promise.await stopping_entered;
                let before = A.state actor |> protocol_ok in
                let assert_unchanged () =
                  let after = A.state actor |> protocol_ok in
                  [%test_eq: int64] before.counters.revision after.counters.revision;
                  assert (
                    Option.equal Jsonaf.exactly_equal before.moderator after.moderator);
                  [%test_eq: Sexp.t]
                    ([%sexp_of: P.Schedule.t list] before.schedules)
                    ([%sexp_of: P.Schedule.t list] after.schedules)
                in
                let deferred =
                  match Owner.deliver_schedule_attempt owner claimed |> protocol_ok with
                  | Deferred error -> error
                  | Delivered -> failwith "busy warm preparation admitted a timer"
                in
                [%test_eq: P.Error.code] Conflict deferred.code;
                assert deferred.retryable;
                (match Owner.deliver_schedule owner claimed with
                 | Error error ->
                   [%test_eq: Sexp.t]
                     (P.Error.sexp_of_t deferred)
                     (P.Error.sexp_of_t error)
                 | Ok () -> failwith "legacy timer API lost the pure guard error");
                (match Owner.ensure_loaded owner with
                 | Error error ->
                   [%test_eq: Sexp.t]
                     (P.Error.sexp_of_t deferred)
                     (P.Error.sexp_of_t error)
                 | Ok () -> failwith "ordinary load ignored its original pure guard");
                [%test_eq: int] 0 !warm_calls;
                [%test_eq: int] 0 !build_calls;
                assert_unchanged ();
                release_stop ();
                Eio.Promise.await_exn actual_stop |> protocol_ok;
                stopping := None;
                (* This same real warm path now enters its source stage. Its
                   retryable Error must not be relabelled as a pure refusal. *)
                (match Owner.deliver_schedule_attempt owner claimed with
                 | Error error ->
                   [%test_eq: Sexp.t]
                     (P.Error.sexp_of_t later_refusal)
                     (P.Error.sexp_of_t error)
                 | Ok (Delivered | Deferred _) ->
                   failwith "later warm preparation was classified as deferral");
                [%test_eq: int] 1 !warm_calls;
                [%test_eq: int] 0 !build_calls;
                assert_unchanged ();
                let outer_bodies = ref 0 in
                let previous_backtraces = Stdlib.Printexc.backtrace_status () in
                Exn.protect
                  ~finally:(fun () ->
                    Stdlib.Printexc.record_backtrace previous_backtraces)
                  ~f:(fun () ->
                    Stdlib.Printexc.record_backtrace true;
                    raise_warm := true;
                    match
                      capture (fun () ->
                        Owner.with_delegation_resources owner (fun _ ->
                          Int.incr outer_bodies;
                          Ok ()))
                    with
                    | Error (actual, backtrace) ->
                      assert (phys_equal actual warm_failure);
                      let original =
                        Option.value_exn !original_warm_backtrace
                        |> Stdlib.Printexc.raw_backtrace_to_string
                      in
                      assert (not (String.is_empty original));
                      assert (
                        String.is_prefix
                          (Stdlib.Printexc.raw_backtrace_to_string backtrace)
                          ~prefix:original)
                    | Ok (Error error) -> raise_s [%sexp (error : P.Error.t)]
                    | Ok (Ok ()) -> failwith "warm preflight exception was lost");
                [%test_eq: int] 2 !warm_calls;
                [%test_eq: int] 0 !build_calls;
                [%test_eq: int] 0 !outer_bodies;
                assert (not (Owner.is_loaded owner));
                assert_unchanged ();
                print_endline
                  "pure warm guard deferred before preparation; legacy errors unchanged; \
                   entered warm failure preserved without delivery"))
          in
          let failures =
            Eio.Cancel.protect (fun () ->
              List.filter_map
                [ release_stop
                ; (fun () -> Owner.close_and_wait owner)
                ; (fun () ->
                    Option.iter !stopping ~f:(fun pending ->
                      Eio.Promise.await_exn pending |> protocol_ok))
                ]
                ~f:(fun cleanup -> capture cleanup |> Result.error))
          in
          match primary, failures with
          | Ok (), [] -> ()
          | Ok (), (exn, backtrace) :: _ ->
            Exn.raise_with_original_backtrace exn backtrace
          | Error (exn, backtrace), failures ->
            List.iter failures ~f:(fun (cleanup, cleanup_backtrace) ->
              try
                Eio.traceln
                  "warm timer fixture cleanup: %s\n%s"
                  (Exn.to_string cleanup)
                  (Stdlib.Printexc.raw_backtrace_to_string cleanup_backtrace)
              with
              | _ -> ());
            Exn.raise_with_original_backtrace exn backtrace)));
  [%expect
    {| pure warm guard deferred before preparation; legacy errors unchanged; entered warm failure preserved without delivery |}]
;;

let%expect_test
    "administration capture failures finalize without committing or rebuilding"
  =
  let module Recovery = Owner.Cleanup_recovery in
  let module Entry = struct
    type t =
      | Commit
      | Retire
      | Reinitialize
  end
  in
  let recording = Stdlib.Printexc.backtrace_status () in
  Exn.protect
    ~finally:(fun () -> Stdlib.Printexc.record_backtrace recording)
    ~f:(fun () ->
      Stdlib.Printexc.record_backtrace true;
      List.iter [ Entry.Commit; Retire; Reinitialize ] ~f:(fun entry ->
        with_actor (fun _env _sw actor _writer _backend ->
          let builds = ref 0
          and commits = ref 0
          and initializes = ref 0 in
          let owner =
            Owner.create ~actor ~initial:None ~build:(fun () ->
              Int.incr builds;
              failwith "capture failure unexpectedly built a runtime")
          in
          Exn.protect
            ~finally:(fun () -> Owner.close_and_wait owner)
            ~f:(fun () ->
              let failure = Failure "actual administration capture" in
              let captured = ref None
              and original_trace = ref None
              and captures = ref 0 in
              let capture_cleanup cleanup =
                Int.incr captures;
                assert (Option.is_none !captured);
                captured := Some cleanup;
                assert (Option.is_none (Recovery.original cleanup |> protocol_ok));
                assert (Option.is_none (Recovery.snapshot cleanup |> protocol_ok));
                assert (Int.equal !commits 0);
                assert (Int.equal !initializes 0);
                try raise failure with
                | exn ->
                  let backtrace = Stdlib.Printexc.get_raw_backtrace () in
                  original_trace
                  := Some (Stdlib.Printexc.raw_backtrace_to_string backtrace);
                  Exn.raise_with_original_backtrace exn backtrace
              in
              let commit () =
                Int.incr commits;
                Ok ()
              in
              let invoke capture_cleanup =
                match entry with
                | Commit -> Owner.with_administration ~capture_cleanup owner commit
                | Retire ->
                  Owner.retire_administration
                    ~capture_cleanup
                    owner
                    ~validate:(fun () -> Ok ())
                    ~commit
                | Reinitialize ->
                  Owner.reinitialize_administration
                    ~capture_cleanup
                    owner
                    ~validate:(fun () -> Ok ())
                    ~commit
                    ~before_initialize:(fun () -> Int.incr initializes)
                  |> Result.map ~f:(fun _ -> ())
              in
              (match invoke capture_cleanup with
               | Ok () | Error _ -> failwith "administration lost its capture exception"
               | exception exn ->
                 let trace =
                   Stdlib.Printexc.get_raw_backtrace ()
                   |> Stdlib.Printexc.raw_backtrace_to_string
                 in
                 assert (phys_equal exn failure);
                 let original = Option.value_exn !original_trace in
                 assert (not (String.is_empty original));
                 assert (String.is_substring trace ~substring:original));
              [%test_eq: int * int * int * int]
                (1, 0, 0, 0)
                (!captures, !commits, !initializes, !builds);
              let cleanup = Option.value_exn !captured in
              let original =
                Recovery.original cleanup |> protocol_ok |> Option.value_exn
              in
              let snapshot =
                Recovery.snapshot cleanup |> protocol_ok |> Option.value_exn
              in
              assert (List.is_empty (Recovery.Snapshot.failures snapshot));
              assert (
                Recovery.Snapshot.equal_progress
                  (Recovery.Snapshot.progress snapshot)
                  Joined);
              (match Recovery.Snapshot.cleanup snapshot with
               | Some cleanup ->
                 (match Owner.Scope_outcome.Cleanup.status cleanup with
                  | Joined -> ()
                  | Never_acquired | Failed _ ->
                    failwith "administration cleanup did not join")
               | None -> failwith "administration cleanup remained unfinished");
              (match Owner.Scope_outcome.Cleanup.status original with
               | Never_acquired | Joined -> ()
               | Failed _ ->
                 failwith "capture primary was reclassified as cleanup failure");
              (* A primary callback exception must not fabricate a cleanup failure
               or strand its acquired administration transition. *)
              Owner.with_administration owner (fun () -> Ok ()) |> protocol_ok;
              assert (
                phys_equal
                  original
                  (Recovery.original cleanup |> protocol_ok |> Option.value_exn));
              assert (
                phys_equal
                  snapshot
                  (Recovery.snapshot cleanup |> protocol_ok |> Option.value_exn));
              Owner.close_and_wait owner;
              (match invoke (fun _ -> Int.incr captures) with
               | Error _ -> ()
               | Ok () -> failwith "closed owner admitted administration");
              [%test_eq: int * int * int * int]
                (1, 0, 0, 0)
                (!captures, !commits, !initializes, !builds)))));
  print_endline
    "all administration entry points preserve capture failure, finalize, and skip \
     rejected capture";
  [%expect
    {| all administration entry points preserve capture failure, finalize, and skip rejected capture |}]
;;

module Pending_administration_cut = struct
  type t =
    | Before_reservation
    | During_commit
end

exception Pending_administration_cleanup

let%expect_test
    "pending retirement retains actual administration report before delayed launch"
  =
  let module R = Owner.Resource_retirement in
  List.iter
    [ Pending_administration_cut.Before_reservation; During_commit ]
    ~f:(fun cut ->
      with_actor (fun env sw actor _writer _backend ->
        Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
          let entered, enter = Eio.Promise.create () in
          let released, release = Eio.Promise.create () in
          let closes = ref 0
          and builds = ref 0
          and commits = ref 0
          and resource_joined = ref false in
          let owner =
            Owner.create
              ~actor
              ~initial:
                (Some
                   (runtime
                      ~close:(fun () ->
                        Int.incr closes;
                        if Int.equal !closes 1
                        then raise Pending_administration_cleanup
                        else resource_joined := true)
                      ()))
              ~build:(fun () ->
                Int.incr builds;
                failwith "pending administration unexpectedly reconstructed resources")
          in
          let capture f =
            match f () with
            | result -> Ok result
            | exception exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
          in
          let wait () =
            Eio.Promise.resolve enter ();
            Eio.Promise.await released;
            Ok ()
          in
          let admin =
            Eio.Fiber.fork_promise ~sw (fun () ->
              capture (fun () ->
                match cut with
                | Before_reservation ->
                  Owner.retire_administration owner ~validate:wait ~commit:(fun () ->
                    Int.incr commits;
                    Ok ())
                | During_commit ->
                  Owner.with_administration owner (fun () ->
                    let open Result.Let_syntax in
                    let%map () = wait () in
                    Int.incr commits)))
          in
          let release_admin () =
            if Option.is_none (Eio.Promise.peek released)
            then Eio.Promise.resolve release ()
          in
          Exn.protect
            ~finally:(fun () ->
              release_admin ();
              Eio.Cancel.protect (fun () ->
                ignore (Eio.Promise.await_exn admin);
                (* A failed assertion must still leave no held stage/fiber. The
                   first close may deliver the unobserved pending failure; the
                   second explicit close is the permitted same-holder retry. *)
                try Owner.close_and_wait owner with
                | Pending_administration_cleanup -> Owner.close_and_wait owner))
            ~f:(fun () ->
              Eio.Promise.await entered;
              (* The actual administration owns the mutex while waiting above.
                 A locking signal would time out here before we could release it. *)
              let work =
                match Owner.signal_resource_retirement owner |> protocol_ok with
                | R.Run work -> work
                | Join _ -> failwith "pending signal did not elect its first producer"
              in
              let completion = R.completion work in
              let duplicate =
                match Owner.signal_resource_retirement owner |> protocol_ok with
                | R.Join same -> same
                | Run _ -> failwith "pending duplicate elected another producer"
              in
              assert (phys_equal completion duplicate);
              assert (Option.is_none (R.peek completion));
              release_admin ();
              (* Let the real extent close and CLEAR before the external worker
                 starts. Only its actual association can preserve the failure. *)
              (match cut, Eio.Promise.await_exn admin with
               | During_commit, Ok (Ok ()) -> [%test_eq: int] 1 !commits
               | Before_reservation, Error (Pending_administration_cleanup, _) ->
                 [%test_eq: int] 0 !commits
               | _, Error (exn, backtrace) ->
                 Exn.raise_with_original_backtrace exn backtrace
               | _, Ok (Error error) -> raise_s [%sexp (error : Agent_protocol.Error.t)]
               | Before_reservation, Ok (Ok ()) ->
                 failwith "failed old-resource disposal unexpectedly committed");
              let stage_attempts = !closes in
              let already_joined = !resource_joined in
              assert (stage_attempts > 0);
              (* Public Stop must select pending, deliver its first failure,
                 and never elect an ordinary physical retry ahead of it. *)
              (match Owner.unload_and_wait owner with
               | Ok () | Error _ -> failwith "public Stop bypassed pending extent failure"
               | exception Pending_administration_cleanup -> ());
              let failure, original_backtrace =
                match R.peek completion with
                | Some (Raised ((Pending_administration_cleanup as exn), backtrace)) ->
                  exn, backtrace
                | Some (Raised (exn, backtrace)) ->
                  Exn.raise_with_original_backtrace exn backtrace
                | Some (Returned _) | None ->
                  failwith "pending worker lost the completed actual extent failure"
              in
              let observe_original () =
                (match R.peek completion with
                 | Some (Raised (exn, backtrace)) ->
                   assert (phys_equal exn failure);
                   assert (phys_equal backtrace original_backtrace)
                 | Some (Returned _) | None -> failwith "old pending outcome changed");
                match R.await completion with
                | Ok () | Error _ -> failwith "raw extent failure changed projection"
                | exception Pending_administration_cleanup -> ()
              in
              observe_original ();
              (match R.await duplicate with
               | Ok () | Error _ -> failwith "duplicate lost actual raw failure"
               | exception Pending_administration_cleanup -> ());
              [%test_eq: int] stage_attempts !closes;
              [%test_eq: int] 0 !builds;
              let retry =
                match
                  Owner.retry_signalled_resource_retirement owner ~failed:completion
                  |> protocol_ok
                with
                | R.Run work -> work
                | Join _ -> failwith "exact explicit pending retry did not claim work"
              in
              R.run retry;
              R.await (R.completion retry) |> protocol_ok;
              let expected_attempts =
                if already_joined then stage_attempts else stage_attempts + 1
              in
              [%test_eq: int] expected_attempts !closes;
              assert !resource_joined;
              [%test_eq: int] 0 !builds;
              observe_original ();
              R.run work;
              [%test_eq: int] expected_attempts !closes;
              Owner.close_and_wait owner;
              let queued =
                match Owner.signal_resource_retirement owner |> protocol_ok with
                | R.Run work -> R.completion work
                | Join _ -> failwith "fully closed Owner unexpectedly retained work"
              in
              assert (Option.is_none (R.peek queued));
              Owner.close_and_wait owner;
              (match R.peek queued with
               | Some (Returned (Ok ())) -> ()
               | Some (Returned (Error error)) ->
                 raise_s [%sexp (error : Agent_protocol.Error.t)]
               | Some (Raised (exn, backtrace)) ->
                 Exn.raise_with_original_backtrace exn backtrace
               | None -> failwith "close_finished skipped actual newly queued work");
              let disposed = Owner.close_and_wait_reported owner in
              (match Owner.Disposal_report.disposition disposed with
               | Joined -> ()
               | Retained -> failwith "newly accepted cleanup stayed retained");
              assert (phys_equal disposed (Owner.close_and_wait_reported owner));
              let queued =
                match Owner.signal_resource_retirement owner |> protocol_ok with
                | R.Run work -> R.completion work
                | Join _ -> failwith "reported cache retained completed old work"
              in
              assert (Option.is_none (R.peek queued));
              let refreshed = Owner.close_and_wait_reported owner in
              (match Owner.Disposal_report.disposition refreshed with
               | Joined -> ()
               | Retained -> failwith "newly accepted cleanup stayed retained");
              assert (not (phys_equal disposed refreshed));
              (match R.peek queued with
               | Some (Returned (Ok ())) -> ()
               | Some (Returned (Error error)) ->
                 raise_s [%sexp (error : Agent_protocol.Error.t)]
               | Some (Raised (exn, backtrace)) ->
                 Exn.raise_with_original_backtrace exn backtrace
               | None -> failwith "reported cache skipped actual newly queued work");
              assert (phys_equal refreshed (Owner.close_and_wait_reported owner));
              Owner.close_and_wait owner;
              observe_original ();
              [%test_eq: int] expected_attempts !closes))));
  print_endline
    "signal never locked; both real extents retained first cleanup failure; explicit \
     same-holder retry";
  [%expect
    {| signal never locked; both real extents retained first cleanup failure; explicit same-holder retry |}]
;;

exception Pending_physical_cleanup
exception Pending_retry_launch

let%expect_test
    "pending retry keeps actual failed physical predecessor across launch failure"
  =
  with_actor (fun env _sw actor _writer _backend ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
      let module R = Owner.Resource_retirement in
      let attempts = ref 0
      and closes = ref 0
      and builds = ref 0 in
      let owner =
        Owner.create_with_unload
          ~actor
          ~initial:(Some (runtime ~close:(fun () -> Int.incr closes) ()))
          ~build:(fun () ->
            Int.incr builds;
            failwith "pending retry rebuilt resources")
          ~before_unload:(fun ~closing:_ ->
            Int.incr attempts;
            if Int.equal !attempts 1 then raise Pending_physical_cleanup;
            Ok ())
      in
      let work = function
        | R.Run work -> work
        | Join _ -> failwith "explicit pending election unexpectedly joined"
      in
      Exn.protect
        ~finally:(fun () -> Owner.close_and_wait owner)
        ~f:(fun () ->
          let a = Owner.signal_resource_retirement owner |> protocol_ok |> work in
          R.run a;
          let failed_a = R.completion a in
          (match R.await failed_a with
           | Ok () | Error _ -> failwith "physical failure was lost"
           | exception Pending_physical_cleanup -> ());
          [%test_eq: int] 1 !attempts;
          [%test_eq: int] 0 !closes;
          let b =
            Owner.retry_signalled_resource_retirement owner ~failed:failed_a
            |> protocol_ok
            |> work
          in
          let launch_failure =
            try raise Pending_retry_launch with
            | exn ->
              Owner.Scope_outcome.Cleanup.Failure.Raised
                (exn, Stdlib.Printexc.get_raw_backtrace ())
          in
          R.fail_to_start b launch_failure;
          let failed_b = R.completion b in
          (match R.await failed_b with
           | Ok () | Error _ -> failwith "launch failure was lost"
           | exception Pending_retry_launch -> ());
          [%test_eq: int] 1 !attempts;
          [%test_eq: int] 0 !closes;
          let c =
            Owner.retry_signalled_resource_retirement owner ~failed:failed_b
            |> protocol_ok
            |> work
          in
          R.run c;
          R.await (R.completion c) |> protocol_ok;
          [%test_eq: int] 2 !attempts;
          [%test_eq: int] 1 !closes;
          [%test_eq: int] 0 !builds;
          (match R.peek failed_a, R.peek failed_b with
           | ( Some (Raised (Pending_physical_cleanup, _))
             , Some (Raised (Pending_retry_launch, _)) ) -> ()
           | _ -> failwith "explicit retry changed an old immutable failure");
          R.run a;
          R.run b;
          [%test_eq: int] 2 !attempts;
          [%test_eq: int] 1 !closes)));
  print_endline "physical A failed; queued B failed; exact C retried actual A once";
  [%expect {| physical A failed; queued B failed; exact C retried actual A once |}]
;;
