open! Core
module A = Agent_session
module P = Agent_protocol
module D = Agent_store.Delegation_store

let protocol_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp "invalid native authority DATA", (error : P.Error.t)]
;;

let store_ok = function
  | Ok value -> value
  | Error error ->
    raise_s [%sexp "invalid native authority ledger", (error : Agent_store.Store_error.t)]
;;

let%expect_test "native authority DATA permits factual progress but fences owner epochs" =
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run (fun env ->
    let path =
      Filename.concat
        "/tmp"
        ("ochat-native-basis-" ^ P.Id.Transaction.(create () |> to_string))
    in
    let root = Eio.Path.(Eio.Stdenv.fs env / path) in
    Eio.Path.mkdir ~perm:0o700 root;
    Exn.protect
      ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true root)
      ~f:(fun () ->
        let now = P.Timestamp.of_string "2026-11-15T12:00:00Z" |> protocol_ok in
        let principal = P.Id.Principal.create () in
        let digest = Chatmd_shell_spec.Source_ref.digest in
        let workspace =
          A.Workspace_resolver.resolve_current
            ~env
            ~instance_id:(P.Id.Workspace_instance.create ())
            ~path
            ~access:Shared_write
            ~created_at:now
          |> store_ok
        in
        let protocol =
          P.Session.Spec.create
            ~execution_host:Daemon
            ~prompt:(Local_path "/data.chatmd")
            ~workspace:Current
            ~liveness:Detached
            ~persistence:Durable
            ~start_immediately:false
            ~labels:[]
            ()
          |> protocol_ok
        in
        let identity : A.Session_state.Identity.t =
          { session_id = P.Id.Session.create ()
          ; display_name = None
          ; creating_principal = Some principal
          ; created_at = now
          ; updated_at = now
          ; labels = []
          ; generation = 0
          ; metadata_revision = 0L
          ; organization = P.Session_organization.Values.empty
          }
        in
        let target =
          Inference.Request.Target.create
            ~adapter:"fixture.responses"
            ~profile:"selected"
            ~profile_revision:None
            ~account:None
            ~endpoint:"fixture://responses"
            ~model:"fixture-model"
            ~settings:[]
            ~limits:Agent_store.Delegation_document.limits
          |> Result.map_error ~f:(fun error ->
            Sexp.to_string_hum [%sexp (error : Inference.Request.Error.t)])
          |> Result.ok_or_failwith
        in
        let spec : A.Session_state.Spec.t =
          { protocol
          ; prompt_definition_id = None
          ; root_invocation = Ui
          ; prompt_revision_id = P.Id.Prompt_revision.create ()
          ; configuration_revision = 0L
          ; inference_target =
              Inference.Selection.captured target ~limits:Document_schema.Limits.default
              |> Result.map_error ~f:(fun error ->
                Sexp.to_string_hum [%sexp (error : Inference.Selection.Error.t)])
              |> Result.ok_or_failwith
          ; delegation = None
          ; workspace_instance = workspace
          ; permission_profile = "data"
          ; permission_profile_digest = digest "data profile"
          ; runtime_policy = None
          ; quota_key = None
          }
        in
        let initial = A.Session_state.create ~identity ~spec ~initial_history:[] in
        let advance state ~now delta =
          let next =
            A.Session_transition.apply ~now state ~delta ~payloads:[] |> protocol_ok
          in
          A.Session_state.validate next.state |> protocol_ok;
          next.state
        in
        (* These are validated DATA transitions, never an Actor installation,
           Init ACK, Caller, runtime owner or permission to perform an effect. *)
        A.Session_state.validate initial |> protocol_ok;
        let before =
          advance initial ~now (Lifecycle_changed { desired = Running; observed = Idle })
        in
        let data_root =
          Agent_store.Data_root.create ~env ~path:(Filename.concat path "ledger")
          |> store_ok
        in
        let ledger = D.create ~env ~data_root in
        let admission : D.Admission.t =
          { child_session_id = P.Id.Session.create ()
          ; revision_id = P.Id.Prompt_revision.create ()
          ; transaction_id = P.Id.Transaction.create ()
          ; manifest_sha256 = digest "data child"
          ; parent_revision_id = before.spec.prompt_revision_id
          ; parent_stop_epoch = Some before.stop_epoch
          ; authority_sha256 = Native_resource_authority.fingerprint before |> protocol_ok
          ; authored_tool = None
          ; goal_origin = None
          ; captured_recurrence = None
          ; capability_pins = []
          ; lifetime = Owned
          ; created_at = now
          ; inference_target = Some target
          }
        in
        let record =
          D.reserve
            ledger
            ~key:
              { parent_session_id = before.identity.session_id
              ; parent_generation = before.identity.generation
              ; principal_id = principal
              ; idempotency_key =
                  P.Idempotency_key.of_string "basis-data-law" |> protocol_ok
              }
            ~request_sha256:(digest "data request")
            ~admission
            ~max_records:1
            ~max_bytes:65536
          |> store_ok
          |> function
          | D.New record -> record
          | Replay _ | Conflict _ -> failwith "unexpected prior DATA-law record"
        in
        let record =
          List.fold
            [ D.Artifact_installed; Child_installed; Linked ]
            ~init:record
            ~f:(fun record stage -> D.advance ledger record stage |> store_ok)
        in
        let basis =
          Native_resource_authority.Basis.capture ~record before |> protocol_ok
        in
        (* Actual immutable Source preparations for a DATA-only comparison law.
           No Cell, Owner, Manager, admission or installation is constructed. *)
        let source_path = Eio.Path.(root / "data.chatmd") in
        Eio.Path.save ~create:(`Exclusive 0o600) source_path "";
        let definition =
          A.Prompt_definition.create
            ~id:(P.Id.Prompt_definition.create ())
            ~config_name:"native-source-proof-data"
            ~root_file:(Eio.Path.native_exn source_path)
            ~root_invocation:Ui
            ~allowed_workspaces:[]
            ~permission_profile:"data"
            ~runtime_policy:None
            ~enabled:true
            ~description:None
          |> store_ok
        in
        let artifact =
          Agent_store.Prompt_artifact_store.Artifact.create
            ~revision_id:before.spec.prompt_revision_id
            ~root_relative_path:"data.chatmd"
            ~root_chatmd:""
            ~sources:[]
            ~parser_schema_version:5
            ~runtime_schema_version:1
            ~created_at:now
            ()
          |> store_ok
        in
        let revision =
          A.Prompt_revision.create
            ~definition
            ~artifact
            ~materialized_tree:root
            ~elements:[]
        in
        let prepare_source () =
          A.Runtime_builder.Source.prepare
            revision
            ~script_preparation:None
            ~invocation:Ui
            ~env
            ~checkpoint:Eio.Fiber.check
          |> protocol_ok
        in
        let source = prepare_source () in
        let other_source = prepare_source () in
        assert (not (phys_equal source other_source));
        let selected_basis =
          Native_resource_authority.Basis.capture ~installed_source:source ~record before
          |> protocol_ok
        in
        let proof_mismatch (outcome : (unit, P.Error.t) Result.t) =
          match outcome with
          | Error { code = Conflict; retryable = false; _ } -> ()
          | Error error -> raise_s [%sexp (error : P.Error.t)]
          | Ok () -> failwith "668 native DATA basis accepted a substituted proof"
        in
        Native_resource_authority.Basis.check
          ~installed_source:source
          selected_basis
          ~expected:before
        |> protocol_ok;
        Native_resource_authority.Basis.check selected_basis ~expected:before
        |> proof_mismatch;
        Native_resource_authority.Basis.check
          ~installed_source:source
          basis
          ~expected:before
        |> proof_mismatch;
        Native_resource_authority.Basis.check
          ~installed_source:other_source
          selected_basis
          ~expected:before
        |> proof_mismatch;
        let window =
          Native_resource_authority.Source_window.capture ~source before |> protocol_ok
        in
        let window_mismatch expected =
          Native_resource_authority.Source_window.check window ~expected |> proof_mismatch
        in
        window_mismatch { before with stop_epoch = Int64.succ before.stop_epoch };
        window_mismatch { before with parent_stop_epoch = Some 1L };
        window_mismatch
          { before with lifecycle = { desired = Stopped; observed = Stopped } };
        window_mismatch
          { before with lifecycle = { desired = Running; observed = Stopping } };
        window_mismatch { before with halted = true };
        window_mismatch
          { before with failure = Some (P.Error.invalid_request "DATA-only failure") };
        (* Complete actual Run-source DATA, not an Actor installation/receipt.
           Even with no checkpoint the full None/Some source identity must bind. *)
        let installation =
          A.Run_source_installation.apply
            A.Run_source_installation.initial
            ~change:
              (Replace
                 { script_id = "source-window-data"; source_sha256 = digest "source" })
          |> protocol_ok
        in
        let index =
          A.Run_state.replace_installation
            A.Run_state.empty
            ~installation
            ~retired_runs:[]
          |> protocol_ok
        in
        window_mismatch { before with run_state = Some index };
        let stopped =
          { before with lifecycle = { desired = Stopped; observed = Stopped } }
        in
        let stopped_window =
          Native_resource_authority.Source_window.capture ~source stopped |> protocol_ok
        in
        Native_resource_authority.Source_window.check stopped_window ~expected:stopped
        |> protocol_ok;
        Native_resource_authority.Source_window.check stopped_window ~expected:before
        |> proof_mismatch;
        let foreign =
          { before with
            spec =
              { before.spec with prompt_revision_id = P.Id.Prompt_revision.create () }
          }
        in
        Native_resource_authority.Basis.check
          ~installed_source:source
          selected_basis
          ~expected:foreign
        |> proof_mismatch;
        (* Prepare a genuine modern Extension Source without installing a
           Manager, Cell, lifecycle receipt or execution permission. These are
           immutable identity DATA laws; the parent-policy integration test
           owns the actual loaded-Owner compatibility oracle. *)
        Eio.Path.save
          ~create:(`Or_truncate 0o600)
          source_path
          {|<script id="policy" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event = fun ctx state event -> Task.pure(state)
</script>|};
        let artifact_store =
          Agent_store.Prompt_artifact_store.create
            ~env
            ~root:(Eio.Path.native_exn Eio.Path.(root / "modern-artifacts"))
          |> store_ok
        in
        let modern_revision =
          A.Prompt_revision_builder.build
            ~env
            ~artifact_store
            ~transaction_id:(P.Id.Transaction.create ())
            ~created_at:now
            definition
          |> Result.map_error ~f:(fun errors ->
            Sexp.to_string_hum
              [%sexp (errors : A.Prompt_revision_builder.Diagnostic.t list)])
          |> Result.ok_or_failwith
        in
        let modern_source =
          A.Runtime_builder.Source.prepare
            modern_revision
            ~script_preparation:Daemon.default_options.script_preparation
            ~invocation:Ui
            ~env
            ~checkpoint:Eio.Fiber.check
          |> protocol_ok
        in
        let observer =
          match A.Runtime_builder.Source.moderator modern_source with
          | Extension observer -> observer
          | No_moderator | Legacy _ -> failwith "668 modern DATA source is not Extension"
        in
        let snapshot : Session.Moderator_state.Identity_snapshot.t =
          { script_id = observer.script_id
          ; script_source_hash = observer.source_sha256
          ; current_state = Session.Snapshot.Int 0
          ; queued_internal_events = []
          ; halted = false
          ; revision = 0
          ; next_change_id = 0
          ; prepended_items = []
          ; appended_items = []
          ; replacements = []
          ; tombstones = []
          ; halted_reason = None
          }
        in
        let modern =
          { before with
            spec =
              { before.spec with
                prompt_definition_id = Some definition.id
              ; prompt_revision_id = A.Prompt_revision.id modern_revision
              }
          ; moderator = Some (A.Moderator_checkpoint.encode snapshot)
          }
        in
        let modern_installation =
          A.Run_source_installation.apply
            A.Run_source_installation.initial
            ~change:(Replace observer)
          |> protocol_ok
        in
        let modern_index =
          A.Run_state.replace_installation
            A.Run_state.empty
            ~installation:modern_installation
            ~retired_runs:[]
          |> protocol_ok
        in
        let installed = { modern with run_state = Some modern_index } in
        assert (Option.is_none (A.Run_state.moderator_lifecycle modern_index));
        assert (not (Native_resource_authority.requires_installed_source before));
        List.iter
          [ modern; { modern with run_state = Some A.Run_state.empty }; installed ]
          ~f:(fun expected ->
            assert (Native_resource_authority.requires_installed_source expected);
            assert (Result.is_error (Native_resource_authority.moderation_source expected));
            let actual =
              Native_resource_authority.moderation_source
                ~installed_source:modern_source
                expected
              |> protocol_ok
            in
            assert (Option.equal P.Invocation.equal_observer actual (Some observer)));
        assert (
          Result.is_error
            (Native_resource_authority.moderation_source
               ~installed_source:other_source
               installed));
        let installed_window =
          Native_resource_authority.Source_window.capture ~source:modern_source installed
          |> protocol_ok
        in
        Native_resource_authority.Source_window.check installed_window ~expected:installed
        |> protocol_ok;
        let next_installation =
          A.Run_source_installation.apply modern_installation ~change:(Replace observer)
          |> protocol_ok
        in
        let next_index =
          A.Run_state.replace_installation
            modern_index
            ~installation:next_installation
            ~retired_runs:[]
          |> protocol_ok
        in
        Native_resource_authority.Source_window.check
          installed_window
          ~expected:{ installed with run_state = Some next_index }
        |> proof_mismatch;
        List.iter
          [ { installed with
              moderator =
                Some (A.Moderator_checkpoint.encode { snapshot with halted = true })
            }
          ; { installed with
              moderator =
                Some
                  (A.Moderator_checkpoint.encode
                     { snapshot with script_source_hash = digest "foreign checkpoint" })
            }
          ; { installed with run_state = Some index }
          ]
          ~f:(fun expected ->
            assert (
              Result.is_error
                (Native_resource_authority.moderation_source
                   ~installed_source:modern_source
                   expected)));
        let current_source =
          A.Run_source_installation.captured modern_installation ~generation:0
          |> protocol_ok
        in
        let unmatched_pointer =
          A.Moderator_lifecycle.create
            ~source:current_source
            ~execution_id:(P.Id.Moderator_execution.create ())
          |> protocol_ok
        in
        let unmatched_index =
          A.Run_state.capture_moderator_lifecycle modern_index unmatched_pointer
          |> protocol_ok
        in
        let unmatched = { installed with run_state = Some unmatched_index } in
        (* A real validated pointer DATA value deliberately has no matching
           receipt here. It is not an issued lifecycle claim. Its presence must
           choose State and preserve strict validation, never Source fallback. *)
        assert (not (Native_resource_authority.requires_installed_source unmatched));
        (match
           Native_resource_authority.moderation_source
             ~installed_source:modern_source
             unmatched
         with
         | Error { code = Invalid_request; retryable = false; _ } -> ()
         | Error error -> raise_s [%sexp (error : P.Error.t)]
         | Ok _ -> failwith "668 actual Source replaced invalid present lifecycle DATA");
        let later = P.Timestamp.add_ms now 1 |> protocol_ok in
        let factual = advance before ~now:later (Batch []) in
        assert (
          not (P.Timestamp.equal before.identity.updated_at factual.identity.updated_at));
        assert (Int64.(factual.counters.revision > before.counters.revision));
        assert (
          Int64.(
            factual.counters.transaction_sequence > before.counters.transaction_sequence));
        Native_resource_authority.Basis.check basis ~expected:factual |> protocol_ok;
        Native_resource_authority.Source_window.check window ~expected:factual
        |> protocol_ok;
        (* Shared-checker callback transport on the SAME real ledger record and
           validated State DATA; no live Source/Caller/Actor is manufactured. *)
        let capabilities =
          Chat_response.Tool_capability.create
            ~owner:"native-frame-data"
            ~resource_fingerprint:(digest "empty resources")
            []
          |> Result.map_error ~f:(fun error ->
            error.Chat_response.Tool_capability.message)
          |> Result.ok_or_failwith
        in
        let legacy_calls = ref 0 in
        let current_calls = ref 0 in
        let resolved = ref None in
        let make_authority ?moderation_current () =
          let state_reads = ref 0 in
          A.Delegation_authority.create
            ~moderation:(fun id ->
              assert (P.Id.Session.equal id before.identity.session_id);
              Int.incr legacy_calls;
              Ok None)
            ?moderation_current
            ~host:
              { state =
                  (fun id ->
                    assert (P.Id.Session.equal id before.identity.session_id);
                    Int.incr state_reads;
                    Ok (if Int.equal !state_reads 1 then before else factual))
              ; resolve =
                  (fun reference ->
                    D.resolve ledger reference
                    |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
                    |> Result.map ~f:(fun actual ->
                      resolved := Some actual;
                      actual))
              ; capabilities = (fun _ -> Ok capabilities)
              }
            ~reference:(D.reference record)
            ~capabilities
            ()
        in
        let check_frame actual ~expected =
          Int.incr current_calls;
          assert (phys_equal actual (Option.value_exn !resolved));
          assert (D.equal_record actual record);
          assert (
            phys_equal expected (if Int.equal !current_calls 1 then before else factual))
        in
        let supplied =
          make_authority
            ~moderation_current:(fun actual ~expected ->
              check_frame actual ~expected;
              Ok None)
            ()
        in
        A.Delegation_authority.check_execution supplied |> protocol_ok;
        [%test_eq: int] 2 !current_calls;
        [%test_eq: int] 0 !legacy_calls;
        current_calls := 0;
        let selected_error = P.Error.invalid_request "actual frame callback failure" in
        let rejected =
          make_authority
            ~moderation_current:(fun actual ~expected ->
              check_frame actual ~expected;
              if Int.equal !current_calls 2 then Error selected_error else Ok None)
            ()
        in
        (match A.Delegation_authority.check_execution rejected with
         | Error actual -> assert (phys_equal actual selected_error)
         | Ok () -> failwith "668 frame callback error fell back to legacy moderation");
        [%test_eq: int] 2 !current_calls;
        [%test_eq: int] 0 !legacy_calls;
        A.Delegation_authority.check_execution (make_authority ()) |> protocol_ok;
        [%test_eq: int] 2 !legacy_calls;
        (* Historical execution DTO completion is comparison DATA, not an Actor
           ACK, installation or claim. It must not pin the whole receipt list. *)
        let execution =
          P.Moderator_execution.create
            { id = P.Id.Moderator_execution.create ()
            ; session_id = before.identity.session_id
            ; generation = before.identity.generation
            ; source =
                { script_id = "source-window-data"; source_sha256 = digest "source" }
            ; operation_id = None
            ; job = None
            ; phase = Turn_start
            ; event = `Object []
            ; checkpoint_sha256 = digest "historical checkpoint"
            ; created_at = now
            }
          |> protocol_ok
        in
        let completed =
          P.Moderator_execution.complete
            execution
            ~checkpoint_sha256:(digest "completed checkpoint")
            ~requests:
              { request_turn = false; request_compaction = false; end_session = None }
          |> protocol_ok
        in
        Native_resource_authority.Source_window.check
          window
          ~expected:{ factual with moderator_executions = [ execution ] }
        |> protocol_ok;
        Native_resource_authority.Source_window.check
          window
          ~expected:{ factual with moderator_executions = [ completed ] }
        |> protocol_ok;
        let rotated =
          advance
            factual
            ~now:later
            (Owner_lease_generation_changed
               (Int64.succ factual.counters.owner_lease_generation))
        in
        (match Native_resource_authority.Basis.check basis ~expected:rotated with
         | Error { code = Conflict; message; retryable = false; _ } ->
           [%test_eq: string]
             "native resource authority changed during validation"
             message
         | Error _ | Ok () -> failwith "changed owner epoch reused the DATA basis");
        print_endline "DATA only: factual progress accepted; owner epoch rotation denied"));
  [%expect {| DATA only: factual progress accepted; owner epoch rotation denied |}]
;;
