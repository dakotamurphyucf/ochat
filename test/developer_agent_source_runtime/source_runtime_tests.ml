open! Core
module P = Agent_protocol
module Store = Agent_store.Developer_run_store
module Session = Agent_store.Session_store
module Root = Agent_server.Developer_root_owner
module Source_runtime = Developer_agent_source_runtime
module Provider = Provider_runtime_host_tests.Runtime_host_tests
module Native_fixture = Developer_root_owner_test_suite.Native_executor_tests
module Root_fixture = Developer_root_owner_test_suite.Root_owner_tests
module Jobs = Agent_server.Developer_script_job_service

let ok = Root_fixture.ok

let%expect_test "deferred saved Agent binds one physical Root/runtime" =
  let requests = ref 0 in
  Provider.with_fixture ~model_requests:requests (fun env sw anchor create _ ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 45. (fun () ->
      let provider = create () in
      ignore (Provider.setup provider : P.Method_result.t);
      let reads = ref 0 in
      ignore
        (Provider.enroll provider ~sw "one" "agent-key" reads
         : P.Provider_operator.Configuration_result.t);
      let target = Provider.capture provider in
      let selected_model =
        Agent_server.Provider_operator_port.select_model
          (Provider_runtime.operator_port provider)
          ~actor:Provider.actor
          ~target
        |> ok
      in
      let session =
        Session.create
          ~env
          ~sw
          ~root:(Eio.Path.native_exn Eio.Path.(anchor / "agent-runs"))
          ~server_id:Provider.server_id
          ~process_start_identity:None
          ~lock_nonce:"deferred-agent-source-test"
        |> ok
      in
      let store =
        Store.open_or_create
          session
          ~sw
          ~limits:
            (Root_fixture.store_limits
               ~max_job_payload_bytes:4096
               ~max_jobs_per_run:5
               ~max_document_bytes:131072
               ~max_inference_bytes_per_run:65536
               ~max_work_steps:4096
               ~max_document_fields:8192
               ~max_document_nodes:16384
               ())
          ~checkpoint:ignore
        |> ok
      in
      let source =
        Native_fixture.installed_agent_source_with_contents
          ~agent_names:[ "child_before"; "child_bound"; "child_revoked" ]
          ~child_contents:"<developer>Original child instructions</developer>"
          ~env
          ~data_root:(Agent_store.Data_root.path (Session.data_root session))
          ()
      in
      let binding =
        Agent_session.Captured_agent_runner.Source_binding.create
          ~source:source.source
          ~tool_contract:Chatml_compilation.Tool_contract.Standard
          ~runtime_policy_revision:
            (Chatml_source.Digest.of_contents
               "deferred-source-runtime"
               ~checkpoint:ignore)
          ~content_limits:None
          ~artifact_store:source.artifacts
          ~source_lease:Agent_session.Prompt_source_lease.legacy
      in
      let prepare () =
        Source_runtime.prepare
          ~selected_target:Literal_only
          ~source:binding
          ~script_preparation:source.policy
          ~manifest_authorizer:Shell_runtime.Manifest_authorizer.assume_authorized
          ~approval_provider:Shell_runtime.Approval_broker.None_available
          ~response_dir:source.response_root
          ~env
          ~revision:source.parent
          ~sw
          ~limits:
            (Source_runtime.Limits.create ~max_agents:3 ~max_work_steps:100_000
             |> Or_error.ok_exn)
          ~checkpoint:ignore
        |> ok
      in
      let prepared = prepare () in
      let make_runtime registrations =
        let elements = Agent_session.Prompt_revision.elements source.parent in
        let host =
          Chat_response.Agent_runtime.host
            ~env
            ~workspace:source.source_root
            ~tool_dir:source.response_root
            ~prompt_dir:source.source_root
            ~session_dir:source.response_root
            ~cache_dir:source.response_root
            ~home:source.response_root
            ~session_id:"deferred-source"
            ~prompt_elements:elements
          |> ok
        in
        (Chat_response.Agent_runtime.prepare_extensions_without_inference
           ~native_registrations:registrations
           ~approval_store:(Shell_access.Approval.create_store ())
           ~sw
           ~host
           ~platform:(Chat_response.Agent_runtime.platform ())
           ~prompt_elements:elements
           ~manifest_authorizer:Shell_runtime.Manifest_authorizer.assume_authorized
           ~approval_provider:Shell_runtime.Approval_broker.None_available
           ()
         |> Result.map_error ~f:(fun diagnostics ->
           Sexp.to_string_hum
             ([%sexp_of: Chat_response.Agent_runtime.diagnostic list] diagnostics))
         |> Result.ok_or_failwith)
          .native
      in
      let runtime = make_runtime (Source_runtime.registrations prepared) in
      let selected = Lazy.force runtime.capabilities |> ok in
      let captured = Root_fixture.capture () in
      let admit_handle run_id =
        let owner =
          match
            P.Execution_owner.developer
              ~host_id:Provider.server_id
              ~run_id:(P.Id.Developer_run.of_string run_id |> ok)
              ~epoch:0
              ~principal_id:Provider.principal.id
            |> ok
          with
          | Developer owner -> owner
          | Conversation _ -> failwith "expected developer owner"
        in
        Store.admit_epoch
          store
          ~owner
          ~capture_fingerprint:(Chatml_developer_capture.fingerprint captured)
          ~previous:None
        |> ok
        |> fst
      in
      let registry =
        Root.Registry.create
          ~sw
          ~store
          ~capacity:(Root_fixture.capacity ())
          ~now:Root_fixture.now
          ~limits:
            (Root.Limits.create
               ~max_active_runs:2
               ~max_staged_jobs:2
               ~max_native_occupancy:2
               ~max_active_workers:2
               ~max_quarantined_workers:2
               ~max_active_inference_attempts:4
               ~max_admitted_call_bindings:8
             |> Or_error.ok_exn)
      in
      let admit run_id =
        let handle = admit_handle run_id in
        let root =
          Root.Registry.admit
            registry
            ~actor:Provider.actor
            ~capture:captured
            ~registry:selected
            ~selected
            ~selected_model:(Some selected_model)
            ~job_policy:Chat_response.One_off_request.default_policy
            ~handle
            ~workspace_conflict_domain:"deferred-source"
          |> ok
        in
        root, handle
      in
      let root, handle = admit "dvr_source_root" in
      let foreign_root, _ = admit "dvr_foreign_source_root" in
      let blobs =
        let data_root = Session.data_root session in
        Agent_store.Blob_store.create
          ~env
          ~temporary_directory:(Agent_store.Data_root.temporary_blobs_path data_root)
          ~durable_directory:(Agent_store.Data_root.durable_blobs_path data_root)
          ~max_upload_bytes:65536L
        |> ok
      in
      Agent_store.Blob_store.bind_developer_runs blobs ~session_store:session store |> ok;
      let results =
        Root.Job_result.bind
          root
          ~blobs
          ~limits:
            (Agent_store.Developer_result_publisher.Limits.create
               ~inline_bytes:1024
               ~max_result_bytes:65536
               ~max_pending:2
             |> ok)
          ~storage:Automatic
        |> ok
      in
      let models =
        Agent_server.Developer_model_job_runner.create
          ~root
          ~results
          ~sources:
            (Agent_server.Developer_model_job_runner.Sources.create
               ~env
               ~prompt_dir:source.source_root
               ~tool_dir:source.response_root
               ~response_dir:source.response_root
               ~cache:(Chat_response.Cache.create ~max_size:1 ())
               ~manifest_authorizer:Shell_runtime.Manifest_authorizer.assume_authorized
               ~approval_provider:Shell_runtime.Approval_broker.None_available)
        |> ok
      in
      let disclosure =
        Agent_server.Developer_result_disclosure.create
          ~runs:store
          ~blobs
          ~policy:
            (Agent_server.Developer_result_disclosure.Policy.create ~authorize:(fun _ _ ->
               true))
          ~limits:
            (Agent_server.Developer_result_disclosure.Limits.create
               ~max_result_bytes:65536
               ~json:Document_schema.Limits.default
             |> ok)
        |> ok
      in
      let jobs =
        Jobs.create
          ~root
          ~queued_issuer:None
          ~store
          ~disclosure
          ~env
          ~current_actor:(fun () -> Provider.actor)
          ~prepare_source:(fun ~parent ~binding ~request ~checkpoint ->
            let open Result.Let_syntax in
            let%bind () = Jobs.Source_parent.check parent ~binding in
            let%bind launch = Jobs.Source_parent.launch parent in
            let%bind prepared =
              Chat_response.Agent_runtime.prepare_agent_source
                runtime
                ~binding
                ~launch
                ~request
                ~max_registrations:16
                ~checkpoint
            in
            let%map () = Jobs.Source_parent.check parent ~binding in
            prepared)
        |> ok
      in
      let runner =
        Agent_server.Developer_background_job_runner.create
          ~root
          ~results
          ~env
          ~managed_definition:None
          ~agent_runtime:(Some runtime)
          ~jobs
          ~authorize:(fun _ _ -> Ok ())
          ~prepare_output:(function
            | Openai.Responses.Tool_output.Output.Text value -> Ok (`String value)
            | _ -> Error (P.Error.invalid_request "unexpected Agent output"))
        |> ok
      in
      let run_one name =
        let id = Native_fixture.stage_registered_agent_tool ~name jobs in
        let candidate =
          Root.Job_candidate.read_current root ~checkpoint:ignore
          |> ok
          |> List.find_exn ~f:(fun candidate ->
            P.Id.Job.equal id (P.Developer_job.id (Root.Job_candidate.job candidate)))
        in
        let completed =
          Agent_server.Developer_background_job_runner.run_candidate runner candidate
          |> ok
          |> Option.value_exn
        in
        assert (P.Id.Job.equal id (P.Developer_job.id completed));
        (P.Developer_job.state completed).status
      in
      let failed = function
        | P.Job.Failed error ->
          assert (P.Error.equal_code error.code P.Error.Invalid_state)
        | _ -> failwith "saved Agent did not persist a failed claim"
      in
      assert (
        Result.is_error (Source_runtime.bound_to prepared ~root ~agent_runtime:runtime));
      failed (run_one "child_before");
      assert (Int.equal !requests 0);
      assert (
        Result.is_error
          (Source_runtime.bind
             prepared
             ~root:foreign_root
             ~models
             ~agent_runtime:runtime
             ~run_selected_target:None
             ~checkpoint:ignore));
      let foreign_runtime = make_runtime (Source_runtime.registrations (prepare ())) in
      assert (
        Result.is_error
          (Source_runtime.bind
             prepared
             ~root
             ~models
             ~agent_runtime:foreign_runtime
             ~run_selected_target:None
             ~checkpoint:ignore));
      Source_runtime.bind
        prepared
        ~root
        ~models
        ~agent_runtime:runtime
        ~run_selected_target:None
        ~checkpoint:ignore
      |> ok;
      Source_runtime.bound_to prepared ~root ~agent_runtime:runtime |> ok;
      assert (
        Result.is_error
          (Source_runtime.bound_to prepared ~root ~agent_runtime:foreign_runtime));
      assert (
        Result.is_error
          (Source_runtime.bind
             prepared
             ~root
             ~models
             ~agent_runtime:runtime
             ~run_selected_target:None
             ~checkpoint:ignore));
      (match run_one "child_bound" with
       | P.Job.Succeeded -> ()
       | _ -> failwith "bound saved Agent did not complete");
      assert (Int.equal !requests 1);
      Source_runtime.revoke prepared;
      assert (
        Result.is_error (Source_runtime.bound_to prepared ~root ~agent_runtime:runtime));
      failed (run_one "child_revoked");
      assert (Int.equal !requests 1);
      Root.close_and_wait foreign_root |> ok;
      Root.close_and_wait root |> ok;
      Root.Registry.close_and_wait registry |> ok;
      print_endline "prebind denied; foreign bind denied; bound completed; revoke denied"));
  [%expect {| prebind denied; foreign bind denied; bound completed; revoke denied |}]
;;
