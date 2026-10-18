let%test_module "retained Developer Host policy" =
  (module struct
    open! Core
    module P = Agent_protocol
    module A = Agent_session
    module Store = Agent_store
    module Bridge = Inference_host.Credential_bridge
    module Provider = Inference_host.Provider_configuration
    module Credentials = Credential_registry
    module Identity = Credential_registry_model
    module Responses = Openai.Responses_driver

    let required name = function
      | Ok value -> value
      | Error _ -> failwith (name ^ " failed")
    ;;

    let credential_id value = Identity.Id.create value |> required "credential ID"

    let local_model_endpoint env sw requests respond =
      let socket =
        Eio.Net.listen
          ~sw
          ~reuse_addr:true
          ~backlog:4
          (Eio.Stdenv.net env)
          (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
      in
      let port =
        match Eio.Net.listening_addr socket with
        | `Tcp (_, port) -> port
        | _ -> failwith "model fixture did not bind TCP"
      in
      Eio.Fiber.fork_daemon ~sw (fun () ->
        while true do
          let flow, _ = Eio.Net.accept ~sw socket in
          Eio.Fiber.fork_daemon ~sw (fun () ->
            Exn.protect
              ~finally:(fun () -> Eio.Flow.close flow)
              ~f:(fun () ->
                let reader = Eio.Buf_read.of_flow flow ~max_size:2_000_000 in
                match Eio.Buf_read.line reader with
                | exception End_of_file -> ()
                | _ ->
                  let rec headers length =
                    let line = Eio.Buf_read.line reader in
                    if String.is_empty line
                    then length
                    else (
                      let key, value = String.lsplit2 line ~on:':' |> Option.value_exn in
                      headers
                        (if String.Caseless.equal key "content-length"
                         then Int.of_string (String.strip value)
                         else length))
                  in
                  let body = Eio.Buf_read.take (headers 0) reader in
                  Int.incr requests;
                  let response = respond !requests body in
                  Eio.Flow.copy_string
                    (sprintf
                       "HTTP/1.1 200 OK\r\n\
                        Content-Type: text/event-stream\r\n\
                        Content-Length: %d\r\n\
                        Connection: close\r\n\
                        \r\n\
                        %s"
                       (String.length response)
                       response)
                    flow);
            `Stop_daemon)
        done);
      sprintf "http://127.0.0.1:%d/v1/responses" port
    ;;

    (* The actual scheduler alone consumes this injected clock. A frozen future
       wait is evidence that the old Host cannot offer a newly staged row. *)
    let controlled_wall_clock real_clock ~initial =
      let logical_now = ref initial in
      let paused = ref false in
      let waiters = ref [] in
      let changed = ref (Eio.Promise.create ()) in
      let notify_change () =
        let _, resolver = !changed in
        changed := Eio.Promise.create ();
        Eio.Promise.resolve resolver ()
      in
      let advance_to deadline =
        logical_now := Float.max !logical_now deadline;
        notify_change ()
      in
      let module Clock = struct
        type t = unit
        type time = float

        let now () = !logical_now

        let rec wait deadline =
          if Float.(deadline <= !logical_now)
          then Eio.Fiber.yield ()
          else (
            let change, _ = !changed in
            (match !paused with
             | true -> Eio.Promise.await change
             | false ->
               let completed =
                 Eio.Fiber.first
                   (fun () ->
                      Eio.Time.sleep real_clock (deadline -. !logical_now);
                      true)
                   (fun () ->
                      Eio.Promise.await change;
                      false)
               in
               if completed && (not !paused) && phys_equal change (fst !changed)
               then (
                 let earliest =
                   List.fold !waiters ~init:deadline ~f:(fun earliest (_, candidate) ->
                     if Float.(candidate > !logical_now)
                     then Float.min earliest candidate
                     else earliest)
                 in
                 advance_to earliest));
            wait deadline)
        ;;

        let sleep_until () deadline =
          if Float.(deadline <= !logical_now)
          then Eio.Fiber.yield ()
          else (
            let token = ref () in
            waiters := (token, deadline) :: !waiters;
            Exn.protect
              ~finally:(fun () ->
                waiters
                := List.filter !waiters ~f:(fun (other, _) ->
                     not (phys_equal token other)))
              ~f:(fun () -> wait deadline))
        ;;
      end
      in
      let pause () =
        paused := true;
        notify_change ()
      in
      let resume () =
        paused := false;
        notify_change ()
      in
      ( Eio.Resource.T ((), Eio.Time.Pi.clock (module Clock))
      , pause
      , resume
      , fun () -> !logical_now )
    ;;

    let model_provider ~env ~root ~endpoint ~sw ~server_id:_ =
      let host = credential_id "retained_host" in
      let binding = credential_id "retained_binding" in
      let identity =
        Identity.Identity.api_key
          ~host
          ~provider:"openai"
          ~billing:"api"
          ~account:None
          ~key_reference:binding
        |> required "provider identity"
      in
      let profile =
        Responses.Profile.create
          ~id:"retained"
          ~account:None
          ~endpoint
          ~capabilities:
            (Responses.Capability.create
               ~baseline:
                 [ Text_input, Supported
                 ; Function_tools, Supported
                 ; Opaque_replay, Supported
                 ]
               ~models:[]
             |> required "model capabilities")
          ~defaults:[]
        |> required "model profile"
      in
      let mapping =
        Bridge.Mapping.create profile ~revision:"fixture-config" ~binding ~identity
        |> required "provider mapping"
      in
      let environment =
        Bridge.Environment.Entry.create
          ~binding
          ~identity
          ~name:"RETAINED"
          ~configuration_revision:None
          ~status:(fun () -> Available)
          ~resolve:(fun ~sw:_ ->
            Ok
              (Credentials.Environment.resolved
                 ~access:
                   (Provider_secret_store.Secret.of_bytes
                      (Bytes.of_string "synthetic-key")
                    |> required "synthetic key")
                 ~configuration_revision:None
                 ~check_current:(fun () -> Ok ())))
        |> required "provider environment entry"
        |> List.return
        |> Bridge.Environment.create
        |> required "provider environment"
      in
      let configuration =
        Provider.create
          ~anchor:Eio.Path.(Eio.Stdenv.fs env / root)
          ~components:[ Private_storage.Name.create "retained" |> required "component" ]
          ~host
          ~secret_namespace:
            (Provider_secret_store.Namespace.create "retained"
             |> required "secret namespace")
          ~mode:(Initialize (credential_id "retained_port"))
        |> required "provider configuration"
      in
      let serial = ref 0 in
      let opened =
        Provider.open_host
          configuration
          ~sw
          ~env
          ~driver:
            (Responses.create ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env) ()
             |> required "responses driver")
          ~new_operation:(fun () ->
            Int.incr serial;
            credential_id (sprintf "operation_%d" !serial))
          ~environment:(Some environment)
          ~oauth:None
          ~mappings:[ mapping ]
          ~authorize:(fun ~principal ~profile:_ ~operation:_ ->
            String.equal principal "pri_operator")
          ~maximum_wait:(Time_ns.Span.of_sec 0.05)
          ~transport_policy:Http_sse
          ~limits:Inference_runtime.Limits.default
        |> required "provider host"
      in
      let bridge = Provider.Opened.bridge opened in
      Bridge.configure_environment
        bridge
        ~principal:"pri_operator"
        ~profile:"retained"
        ~operation:(credential_id "first_enrollment")
        ~name:"RETAINED"
        ~configuration_revision:None
      |> required "provider enrollment";
      let target =
        Bridge.capture
          bridge
          ~principal:"pri_operator"
          ~default_profile:"retained"
          ~current:None
          ~model:"model"
          ~settings:[]
        |> required "model target"
      in
      let port =
        Provider_operator_port.create
          ~dispatch:(fun ~actor:_ _ -> Error P.Provider_operator.Error.Unsupported)
          ~receipt:(fun ~actor:_ _ -> Error P.Provider_operator.Error.Unsupported)
          ~current_model_bridge:(fun () -> Ok bridge)
          ~close:(fun () -> ())
      in
      Some port, Some target
    ;;

    let timestamp env =
      Eio.Time.now (Eio.Stdenv.clock env)
      |> Time_ns.Span.of_sec
      |> Time_ns.of_span_since_epoch
      |> P.Timestamp.of_time_ns
    ;;

    type shared =
      { env : Eio_unix.Stdenv.base
      ; sw : Eio.Switch.t
      ; root : string
      ; actor : Operator_authorization.t
      ; store : Store.Session_store.t
      ; blobs : Store.Blob_store.t
      ; authoring : Authoring_installation.t
      ; connection : Connection_context.t
      ; expected : P.Authoring.Context.Expected.t
      ; provider : Provider_operator_port.t option
      ; model_target : Inference.Request.Target.t option
      ; authorize_native :
          P.Host_invocation.t
          -> Chat_response.Tool_capability.binding
          -> (unit, P.Error.t) result
      ; prepare_output :
          Openai.Responses.Tool_output.Output.t -> (Jsonaf.t, P.Error.t) result
      ; prompts : A.Prompt_catalog.t
      ; project_catalog : Prompt_catalog_host.t
      ; project_sources : Project_source_host.t
      ; factory : Session_factory.t
      ; catalogs : Session_factory.Developer_catalog.t
      ; registry : Session_registry.t
      ; capacity : Job_capacity.t
      ; workspace_leases : A.Workspace_lease.t
      ; child_sources : Session_factory.Child_source_policy.t option
      ; active_phase : (unit -> unit) option ref
      }

    type phase =
      { shared : shared
      ; host : Developer_host.t
      ; scheduler : Job_scheduler.t
      ; original : Developer_command_original.t
      ; closed : bool ref
      }

    let selected_contract target =
      let contracts =
        Authoring_standard_contracts.create
          ~targets:[ target ]
          ~max_identity_bytes:(16 * 1024 * 1024)
          ~max_work_steps:1_000_000
          ~checkpoint:Eio.Fiber.check
        |> required "standard contracts"
      in
      let entry =
        Authoring_standard_contracts.find contracts target |> Option.value_exn
      in
      (match target with
       | Chatml.Chatml_compiler_contract.Target.One_off_v1 ->
         Authoring_context_service.Developer.Selected_contract.of_standard_entry entry
       | Developer_tool_v1 ->
         Authoring_context_service.Developer.Selected_contract
         .of_standard_developer_tool_entry
           entry
       | Tool_v1 | Moderator_v1 | Delegated_moderator_v1 ->
         failwith "unsupported Host pair selected fixture")
      |> required "selected contract"
    ;;

    let capture_policy target =
      let selected = selected_contract target in
      Authoring_context_service.Developer.Target_policy.create
        ~local_project:(fun _ ~root:_ ~project:_ ~selection:_ -> None)
        ~local_source:(fun _ ~root:_ ~member:_ -> None)
        ~authoring_project:(fun _ ~origin:_ ~context:_ ~current:_ -> None)
        ~document_project:
          (fun
            _
            ~origin:_
            ~context:_
            ~current:_
            ~parsed:_
            ~declaration:_
            ~build:_
            ~metadata:_ -> None)
        ~document_source:
          (fun
            _ ~origin:_ ~context:_ ~current:_ ~parsed:_ ~declaration:_ ~script:_ ->
          Some selected)
    ;;

    let capture_limits () =
      Authoring_context_service.Developer.Limits.create
        ~max_prepared_input_bytes:4096
        ~max_document_origin_bytes:16384
        ~max_editor_versions:8
        ~build:
          (Chatml_project_build.Limits.create
             ~max_work_steps:100_000
             ~max_input_bytes:(Some 4096)
             ~document:Document_schema.Limits.default
           |> Or_error.ok_exn)
      |> Or_error.ok_exn
    ;;

    let config ?(manifest_grants = []) ?data_dir root prompt_file =
      let profile =
        Config.Permission_profile.
          { id = "retained.permission"
          ; tool_default = Allow
          ; approval_timeout_ms = None
          ; approval_fallback = Deny
          ; manifest_authorization = Assume_authorized
          }
      in
      Config.
        { version = current_version
        ; source_file = Filename.concat root "server.sexp"
        ; server =
            { data_dir = Option.value data_dir ~default:(Filename.concat root "data")
            ; session_helpers = []
            ; authoring_packages = []
            ; authoring_budget = None
            ; developer = None
            ; project_sources = None
            ; authoring_project_access = None
            ; authoring_upload_access = None
            ; unix_socket = Filename.concat root "agent.sock"
            ; http =
                { enabled = false
                ; address = "127.0.0.1"
                ; port = 8787
                ; require_auth = true
                ; static_tokens_file = None
                ; oauth_validator = None
                ; reverse_proxy = None
                ; max_connections = 16
                ; idle_connection_timeout_ms = 300_000
                }
            ; shutdown_grace_ms = 5_000
            ; max_attachments_per_session = 16
            ; subscriber_queue_capacity = 16
            ; event_retention =
                { completed_stream_ms = 3_600_000
                ; response_artifact_ms = 3_600_000
                ; max_events_per_session = 1024
                }
            ; durability =
                { journal_flush = Each
                ; journal_flush_ms = 1
                ; snapshot_every_events = 100
                ; snapshot_every_ms = 5_000
                }
            ; job_limits =
                { daemon_total = 8
                ; per_principal = 8
                ; per_prompt = 8
                ; per_workspace = 8
                ; per_session = 4
                ; per_kind = 8
                ; max_nested_depth = 8
                }
            ; unsafe_allow_unauthenticated_remote_http = false
            }
        ; workspaces =
            [ { id = "restart.workspace"
              ; source = Physical root
              ; access = Shared_write
              ; conflict_domain = None
              ; prompt_limits =
                  [ { prompt = "restart.prompt"; max_root_agents = 1; overflow = Reject }
                  ]
              }
            ]
        ; prompts =
            [ { id = "restart.prompt"
              ; path = prompt_file
              ; description = None
              ; allowed_workspaces = [ "restart.workspace" ]
              ; permission_profile = profile.id
              ; runtime_policy = None
              ; tool_contract = Chatml_compilation.Tool_contract.Standard
              ; enabled = true
              }
            ]
        ; permission_profiles = [ profile ]
        ; manifest_grants
        }
    ;;

    let semantic_authoring_policy env =
      let module Service = Authoring_context_service in
      let module Policy = Authoring_installation.Host_policy in
      let base =
        Policy.basic_supplied ~initialize_limits:Daemon.default_options.protocol_limits
        |> required "authoring policy"
      in
      let type_display =
        Chatml_type_display.Limits.create
          ~max_nodes:512
          ~max_edges:1024
          ~max_depth:16
          ~max_work_steps:100_000
          ~max_text_bytes:1024
          ~max_output_bytes:4096
        |> Or_error.ok_exn
      in
      let query =
        Chatml_authoring_query.Limits.create
          ~max_items:8
          ~max_member_depth:4
          ~max_member_edges:128
          ~max_label_bytes:4096
          ~max_work_steps:100_000
          ~type_display
        |> Or_error.ok_exn
      in
      Policy.with_semantic
        base
        ~semantic:
          (Policy.Semantic.create
             ~script_selection:None
             ~project_metadata:None
             ~project_build_limits:
               (Chatml_project_build.Limits.create
                  ~max_work_steps:100_000
                  ~max_input_bytes:(Some 4096)
                  ~document:Document_schema.Limits.default
                |> Or_error.ok_exn)
             ~limits:
               (Service.Semantic.Limits.create
                  ~max_document_bundle_files:4
                  ~max_document_bundle_bytes:2048
                  ~max_parser_work_steps:100_000
                  ~max_diagnostic_work_steps:100_000
                  ~max_selection_rows:4
                  ~max_retained_selection_source_bytes:(4 * 1024 * 1024)
                  ~max_retained_selection_input_bytes:(4 * 1024 * 1024)
                  ~max_selection_work_steps:1_000_000
                |> Or_error.ok_exn)
             ~prefix_limits:
               (Service.Semantic.Prefix_limits.create
                  ~max_document_bundle_files:4
                  ~max_document_bundle_bytes:2048
                  ~max_parser_work_steps:100_000
                  ~max_host_work_steps:100_000
                |> Or_error.ok_exn)
             ~query_limits:
               (Service.Semantic.Query_limits.create
                  ~max_host_work_steps:100_000
                  ~projection:query
                |> Or_error.ok_exn)
             ~document_dir:(Eio.Stdenv.cwd env))
      |> required "semantic authoring policy"
    ;;

    let store_limits () =
      let document =
        Document_schema.Limits.create
          ~max_bytes:(1024 * 1024)
          ~max_depth:16
          ~max_fields:1024
          ~max_nodes:2048
        |> required "store document limits"
      in
      Agent_store.Developer_run_store.Limits.create
        ~max_runs:6
        ~max_epochs_per_run:3
        ~max_jobs_per_run:8
        ~max_invocations_per_run:8
        ~max_inference_attempts_per_run:256
        ~commands:(Agent_store.Developer_command_limits.default ~document)
        ~max_inference_bytes_per_run:65536
        ~max_selected_per_commit:3
        ~max_job_payload_bytes:8192
        ~max_document_bytes:(1024 * 1024)
        ~max_recovery_entries:16
        ~max_recovery_bytes:(1024 * 1024)
        ~max_work_steps:100_000
        ~document
      |> required "run store limits"
    ;;

    let with_shared
          ~manifest_grants
          ~data_dir
          ~env
          ~sw
          ~root
          ~prompt_file
          ~actor
          ~script_source
          ~provider_factory
          ~authorize_native
          ~prepare_output
          ~f
      =
      let config = config ~manifest_grants ~data_dir root prompt_file in
      let options = Daemon.default_options in
      let authoring_validation_host =
        A.Authoring_runtime.configure_host
          ?host:options.authoring_validation_host
          ~policy:Chat_response.One_off_request.default_policy
          ()
        |> required "authoring validation host"
      in
      let data_dir = config.server.data_dir in
      let store =
        Store.Session_store.create
          ~env
          ~sw
          ~root:data_dir
          ~server_id:(P.Id.Server.create ())
          ~process_start_identity:None
          ~lock_nonce:P.Id.Transaction.(create () |> to_string)
        |> required "session store"
      in
      let provider, model_target =
        provider_factory ~sw ~server_id:(Store.Session_store.server_id store)
      in
      let data_root = Store.Session_store.data_root store in
      let blobs =
        Store.Blob_store.create
          ~env
          ~temporary_directory:(Store.Data_root.temporary_blobs_path data_root)
          ~durable_directory:(Store.Data_root.durable_blobs_path data_root)
          ~max_upload_bytes:(Int64.of_int options.protocol_limits.max_request_bytes)
        |> required "blob store"
      in
      let idempotency_store =
        Store.Idempotency_store.open_or_create
          ~env
          ~path:
            (Filename.concat (Store.Data_root.indexes_path data_root) "idempotency.sexp")
        |> required "idempotency store"
      in
      let built = Catalog_builder.build config |> required "catalog build" in
      let project_sources =
        Project_source_host.open_owned
          Project_source_configuration.local_only
          ~sw
          ~fs:(Eio.Stdenv.fs env)
          ~data_root
          ~clock:(Eio.Stdenv.mono_clock env)
          ~checkpoint:Eio.Fiber.check
          ()
        |> required "project source"
      in
      let project_catalog =
        Prompt_catalog_host.create
          ~sw
          ~env
          ~store
          ~origin:Created
          ~sources:project_sources
          ~content_limits:options.content_limits
          ~checkpoint:Eio.Fiber.check
        |> required "prompt catalog"
      in
      let prepared =
        Prompt_catalog_host.prepare
          project_catalog
          ~mode:Build
          ~created_at:(timestamp env)
          ~checkpoint:Eio.Fiber.check
          built.prompts
        |> required "prompt preparation"
      in
      Prompt_catalog_host.publish prepared |> required "prompt publication";
      Prompt_catalog_host.commit prepared |> required "prompt commit";
      Prompt_catalog_host.retire prepared;
      let prompts = Prompt_catalog_host.catalog project_catalog in
      let registry = Session_registry.create () in
      let workspace_leases = A.Workspace_lease.create () in
      let quota_manager =
        A.Quota_manager.create ~limits:options.quota_limits ~workspace_leases
        |> required "quota manager"
      in
      let capacity = Job_capacity.create ~limits:config.server.job_limits in
      let child_sources =
        match options.script_preparation with
        | None -> None
        | Some _ ->
          let module Runner = A.Captured_agent_runner in
          let module Sources = A.Prompt_script_preparation in
          let identity tag =
            [%sexp
              (tag : string)
            , (options.implementation_name : string)
            , (options.implementation_version : string)
            , (config.server.project_sources : Project_source_configuration.t option)]
            |> Sexp.to_string_mach
            |> fun contents ->
            Chatml_source.Digest.of_contents contents ~checkpoint:Eio.Fiber.check
          in
          let source =
            Project_source_host.with_host project_sources ~f:(fun policy _ ->
              Runner.Source.create
                ~policy_revision:(identity "ochat.daemon.child-source.v1")
                ~compilation:(Sources.Policy.compilation policy)
                ~parser_limits:(A.Prompt_revision_builder.capture_limits ())
                ~max_work_steps:(Sources.Policy.max_work_steps policy)
                ~preprocess:Meta_prompting.Preprocessor.preprocess
                ~prepare_programs:(fun startup ~tool_contract ~invocation scope capture ->
                  Prompt_catalog_host.prepare_child_programs
                    project_catalog
                    ~startup
                    ~tool_contract
                    ~invocation
                    ~scope
                    ~capture
                    ~checkpoint:(fun () ->
                      Chat_response.Startup_compilation.checkpoint startup)))
            |> required "source policy"
            |> required "child source"
          in
          Some
            (Session_factory.Child_source_policy.create
               ~source
               ~runtime_policy_revision:(identity "ochat.daemon.child-runtime.v1"))
      in
      let factory =
        Session_factory.create
          ~sw
          ~env
          ~store
          ~registry
          ~idempotency_store
          ~blob_store:blobs
          ~prompts
          ~workspaces:built.workspaces
          ~permission_profiles:built.permission_profiles
          ~manifest_grants:built.manifest_grants
          ~quota_manager
          ~job_capacity:capacity
          ~tool_dir:root
          ~home:(Some root)
          ~inference_policy:options.inference_policy
          ~qualify_chatml_extensions:options.qualify_chatml_extensions
          ~session_helpers:options.session_helpers
          ~independent_lifetime_policy:options.independent_lifetime_policy
          ~chatml_runtime_policy:options.chatml_runtime_policy
          ~authoring_validation_host:(Some authoring_validation_host)
          ~script_preparation:options.script_preparation
          ~content_limits:options.content_limits
          ~child_sources
          ~durability:Flush
          ~limits:options.factory_limits
      in
      let catalogs = Session_factory.developer_catalog factory in
      let authoring =
        Authoring_installation.create
          ~env
          ~sw
          ~clock:(Eio.Stdenv.mono_clock env)
          ~project_sources
          ~store
          ~policy:(semantic_authoring_policy env)
          ~initialize_limits:options.protocol_limits
          ~checkpoint:Eio.Fiber.check
        |> required "authoring installation"
      in
      let connection =
        Connection_context.create_authenticated
          ~actor
          ~connection_id:P.Id.Transaction.(create () |> to_string)
          ~transport:In_memory
          ~publish_notification:(fun _ -> ())
          ~max_attachments:8
      in
      let principal = Operator_authorization.principal actor in
      let response =
        P.Initialize.Response.create
          ~protocol_name:"ochat.agent"
          ~selected_version:P.Version.current
          ~implementation:
            (P.Initialize.Implementation.create ~name:"retained-host" ~version:"1"
             |> required "implementation")
          ~server_id:(Store.Session_store.server_id store)
          ~enabled_features:[ P.Authoring.Features.core; P.Authoring.Features.semantic ]
          ~extensions:None
          ~principal
          ~limits:options.protocol_limits
          ~event_retention:
            { minimum_age_ms = 0; maximum_events = 1; oldest_replayable_sequence = None }
          ~timing:options.timing
          ~server_time:(timestamp env)
        |> required "initialize response"
      in
      Authoring_installation.publish_initialize_and_wait authoring ~connection ~response
      |> required "authoring initialize";
      let call request =
        Authoring_installation.handle authoring ~actor ~connection request
        |> required "authoring request"
      in
      let opened =
        match
          P.Authoring.Context.Open_request.create_supplied_document
            ~member:"draft.chatmd"
            ~default_environment:None
            ~contents:
              ("<script id=\"s\" language=\"chatml\" kind=\"tool\">"
               ^ script_source
               ^ "</script>")
          |> required "document request"
          |> fun request -> call (Context_open request)
        with
        | Context_open view -> view
        | _ -> failwith "unexpected authoring response"
      in
      let expected = P.Authoring.Context.Expected.of_view opened in
      let analyze =
        P.Authoring.Analysis.Analyze.Request.create
          ~context:expected
          ~target:(P.Authoring.Analysis.Target.selected_script "s" |> required "selector")
        |> required "analysis request"
      in
      (match call (Analyze analyze) with
       | Analyze _ -> ()
       | _ -> failwith "unexpected analysis response");
      let shared =
        { env
        ; sw
        ; root
        ; actor
        ; store
        ; blobs
        ; authoring
        ; connection
        ; expected
        ; provider
        ; model_target
        ; authorize_native
        ; prepare_output
        ; prompts
        ; project_catalog
        ; project_sources
        ; factory
        ; catalogs
        ; registry
        ; capacity
        ; workspace_leases
        ; child_sources
        ; active_phase = ref None
        }
      in
      let cleanup () =
        let first_error = ref None in
        let perform close =
          try close () with
          | exn ->
            let backtrace = Backtrace.Exn.most_recent () in
            if Option.is_none !first_error then first_error := Some (exn, backtrace)
        in
        Option.iter !(shared.active_phase) ~f:(fun close -> perform close);
        perform (fun () ->
          Authoring_installation.close_connection_and_wait authoring connection
          |> required "authoring connection close");
        perform (fun () ->
          Authoring_installation.close_and_wait authoring |> required "authoring close");
        perform (fun () -> Option.iter provider ~f:Provider_operator_port.close);
        perform (fun () -> Project_source_host.close_and_wait project_sources);
        perform (fun () ->
          Store.Session_store.close store |> required "session store close");
        Option.iter !first_error ~f:(fun (exn, backtrace) ->
          Exn.raise_with_original_backtrace exn backtrace)
      in
      match f shared with
      | value ->
        cleanup ();
        value
      | exception exn ->
        let backtrace = Backtrace.Exn.most_recent () in
        (try cleanup () with
         | _ -> ());
        Exn.raise_with_original_backtrace exn backtrace
    ;;

    let repl_configuration () =
      let parse =
        Chatml.Chatml_parse.Submission.Limits.create
          ~max_source_bytes:4096
          ~max_tokens:4096
          ~max_work_steps:100_000
          ~sites:
            (Chatml.Chatml_syntax_sites.Limits.create
               ~max_sites:4096
               ~max_name_bytes:16384
             |> Or_error.ok_exn)
        |> Or_error.ok_exn
      in
      let display =
        Chatml_typechecker.Type_snapshot.Limits.create
          ~max_nodes:4096
          ~max_edges:4096
          ~max_label_bytes:16384
        |> Or_error.ok_exn
      in
      let root_copy =
        Chatml_typechecker.Repl_type_roots.Limits.create
          ~max_bindings:128
          ~max_type_roots:256
          ~max_type_nodes:4096
          ~max_variable_cells:1024
          ~max_type_edges:4096
          ~max_copy_work:100_000
          ~max_copy_depth:128
          ~max_name_work:100_000
          ~max_name_bytes:16384
        |> Or_error.ok_exn
      in
      let typing_limits =
        Chatml_repl_typing.Limits.create
          ~parse
          ~display
          ~max_display_name_bytes:4096
          ~max_statements:16
          ~root_copy
        |> Or_error.ok_exn
      in
      let evaluator_limits =
        Chatml_repl_execution.Limits.create
          ~max_preview_bytes:4096
          ~max_preview_nodes:1024
          ~max_preview_depth:16
          ~max_diagnostic_bytes:1024
          ~max_retained_tasks:8
        |> Or_error.ok_exn
      in
      let limits =
        Developer_repl_epoch.Limits.create
          ~max_instances:2
          ~max_keys_per_instance:8
          ~max_submissions_per_epoch:8
          ~max_source_bytes:4096
          ~max_receipt_bytes:(512 * 1024)
          ~max_total_receipt_bytes:(1024 * 1024)
          ~max_total_type_detail_bytes:(512 * 1024)
          ~max_task_origins:8
          ~max_reference_page_bytes:4096
        |> Or_error.ok_exn
      in
      Developer_repl_epoch.Configuration.create
        ~scope_limits:
          (Chatml_configured_lookup.Repl_scope.Limits.create
             ~max_snippets:16
             ~max_identity_bytes:4096
           |> Or_error.ok_exn)
        ~root_limits:
          (Chatml_dependency_session.Roots.Limits.create
             ~max_checked_modules:16
             ~max_edges:64
             ~max_dependency_depth:16
           |> Or_error.ok_exn)
        ~typing_limits
        ~evaluator_limits
        ~execution_policy:(Bounded Chatml_execution.default_limits)
        ~type_display_limits:
          (Chatml_type_display.Limits.create
             ~max_nodes:4096
             ~max_edges:8192
             ~max_depth:32
             ~max_work_steps:131072
             ~max_text_bytes:4096
             ~max_output_bytes:32768
           |> Or_error.ok_exn)
        ~limits
      |> Or_error.ok_exn
    ;;

    let trace_configuration ~read_allowed =
      let recorder =
        Chatml_trace.Recorder.Limits.create
          ~max_events:128
          ~max_normal_bytes:(64 * 1024)
          ~max_terminal_bytes:128
        |> Or_error.ok_exn
      in
      let limits =
        Developer_trace_retention.Limits.create
          ~max_traces:3
          ~max_retained_bytes:(512 * 1024)
          ~max_source_refs_per_trace:32
          ~max_provenance_bytes_per_trace:8192
          ~max_expired_tombstones:4
          ~max_age_seconds:3600.
          ~max_header_bytes:8192
          ~recorder
        |> Or_error.ok_exn
      in
      Developer_trace_retention.Configuration.create
        ~mode:Chatml_trace.Recorder.Mode.Full
        ~limits
        ~read_policy:
          (Developer_trace_retention.Read_policy.create ~authorize:(fun actor _ ->
             !read_allowed && Operator_authorization.is_current actor))
    ;;

    let start_phase
          ?(repl = false)
          ?(selected_target = Chatml.Chatml_compiler_contract.Target.One_off_v1)
          ?approval
          ?trace
          shared
          ~clock
          ~max_active_agent_calls
      =
      if Option.is_some !(shared.active_phase)
      then failwith "prior Host phase is still open";
      let roots =
        Developer_root_owner.Limits.create
          ~max_active_runs:6
          ~max_staged_jobs:4
          ~max_native_occupancy:2
          ~max_active_workers:1
          ~max_quarantined_workers:2
          ~max_active_inference_attempts:8
          ~max_admitted_call_bindings:32
        |> Or_error.ok_exn
      in
      let store_limits = store_limits () in
      let document = Store.Developer_run_store.Limits.document store_limits in
      let disclosure =
        Developer_result_disclosure.Limits.create ~max_result_bytes:65536 ~json:document
        |> required "disclosure limits"
      in
      let limits =
        Developer_host.Limits.create ~roots ~max_active_commands:4 ~disclosure
        |> Or_error.ok_exn
      in
      let budget_configuration =
        Developer_command_budgets.Configuration.create
          ~compilation:(Bounded Chatml_compilation.default_limits)
          ~max_dependency_depth:32
          ~execution:Chatml_execution.default_limits
          ~max_output_bytes:65536
          ~receipt:document
        |> required "budget configuration"
      in
      let agent_source_limits =
        Developer_agent_source_runtime.Limits.create ~max_agents:6 ~max_work_steps:100_000
        |> Or_error.ok_exn
      in
      let active_agent_calls =
        A.Selected_agent_depth.Limits.create ~max_active_calls:max_active_agent_calls
        |> Or_error.ok_exn
      in
      let workspace_cleanup =
        A.Workspace_cleanup.Limits.create
          ~max_entries:128
          ~max_bytes:65536
          ~max_work_steps:100_000
        |> Or_error.ok_exn
      in
      let capture_policy = capture_policy selected_target in
      let capture_limits = capture_limits () in
      let publisher_limits =
        Store.Developer_result_publisher.Limits.create
          ~inline_bytes:1024
          ~max_result_bytes:65536
          ~max_pending:2
        |> required "publisher limits"
      in
      let physical_roots =
        Session_factory.Developer_catalog.workspace_definitions shared.catalogs
        |> List.filter_map ~f:(fun definition ->
          match definition.A.Workspace_definition.source with
          | Physical { configured_root } -> Some configured_root
          | Temporary _ -> None)
      in
      let host =
        Developer_host.create
          ~sw:shared.sw
          ~env:shared.env
          ~session_store:shared.store
          ~blobs:shared.blobs
          ~capacity:shared.capacity
          ~authoring:shared.authoring
          ~provider:shared.provider
          ~model_target:shared.model_target
          ~prompts:shared.prompts
          ~catalogs:shared.catalogs
          ~workspace_leases:shared.workspace_leases
          ~physical_roots
          ~workspace_cleanup
          ~script_preparation:Daemon.default_options.script_preparation
          ~tool_dir:Eio.Path.(Eio.Stdenv.fs shared.env / shared.root)
          ~home:Eio.Path.(Eio.Stdenv.fs shared.env / shared.root)
          ~cache:(Chat_response.Cache.create ~max_size:8 ())
          ~approval
          ~max_native_bindings:8
          ~child_sources:shared.child_sources
          ~content_limits:Daemon.default_options.content_limits
          ~remote_content:None
          ~agent_source_limits
          ~active_agent_calls
          ~job_policy:Chat_response.One_off_request.default_policy
          ~runtime_policy:Daemon.default_options.chatml_runtime_policy
          ~native_permission:
            (Developer_native_permission.Mode.Explicit_callback shared.authorize_native)
          ~prepare_output:shared.prepare_output
          ~capture_policy
          ~capture_limits
          ~command_disclosure:
            (Developer_host.Command.Disclosure_policy.create ~authorize:(fun _ _ -> true))
          ~command_control:
            (Developer_host.Command.Control_policy.create ~authorize:(fun _ _ -> true))
          ~result_disclosure:
            (Developer_result_disclosure.Policy.create ~authorize:(fun _ _ -> true))
          ~publisher_limits
          ~publisher_storage:Store.Developer_result_publisher.Storage.Automatic
          ~store_limits
          ~limits
          ~budget_configuration
          ~repl:(if repl then Some (repl_configuration ()) else None)
          ~trace
          ~now:(fun () -> timestamp shared.env)
          ~checkpoint:Eio.Fiber.check
          ()
        |> Result.map_error ~f:(fun error ->
          Sexp.to_string_hum [%sexp "Developer Host", (error : P.Error.t)])
        |> Result.ok_or_failwith
      in
      let scheduler =
        Job_scheduler.start_controlled
          ~enabled:true
          ~sw:shared.sw
          ~clock
          ~registry:shared.registry
          ~developer_registry:(Some (Developer_host.registry host))
          ~capacity:shared.capacity
          ~model_job_inference:(Session_factory.model_job_inference shared.factory)
      in
      Developer_host.install_scheduler host scheduler |> required "scheduler install";
      let route =
        Developer_command_original.Route.Document_script
          { expected = shared.expected
          ; selector =
              P.Authoring.Analysis.Target.selected_script "s" |> required "selector"
          }
      in
      let bounds =
        Chatml_developer_capture.Json_bounds.create
          ~max_bytes:4096
          ~max_depth:16
          ~max_nodes:128
        |> Or_error.ok_exn
      in
      let original =
        Developer_command_original.create
          ~route
          ~kind:Live_run
          ~target:Chatml.Chatml_compiler_contract.Target.One_off_v1
          ~prompt:(Some (Catalog_identity.prompt_definition "restart.prompt"))
          ~workspace:(Some (Catalog_identity.workspace_definition "restart.workspace"))
          ~entrypoint:"main"
          ~input:(`Object [ "input", `String "go" ])
          ~fixtures:None
          ~requested_execution:None
          ~requested_max_output_bytes:None
          ~bounds
          ~checkpoint:Eio.Fiber.check
        |> required "original request"
      in
      let phase = { shared; host; scheduler; original; closed = ref false } in
      shared.active_phase
      := Some
           (fun () ->
             Exn.protect
               ~f:(fun () -> Developer_host.close_and_wait host |> required "Host close")
               ~finally:(fun () ->
                 Job_scheduler.close_and_wait scheduler |> required "scheduler close");
             phase.closed := true;
             shared.active_phase := None);
      phase
    ;;

    let submit_live phase ~run_id ~key =
      if !(phase.closed) then failwith "Host phase is closed";
      Developer_host.Command.submit
        phase.host
        ~actor:phase.shared.actor
        ~run_id
        ~key
        ~original:phase.original
        ~fresh_source:
          (Some
             (Developer_command_original.Fresh_source.Document_script
                { connection = phase.shared.connection }))
    ;;

    let await_terminal phase ~run_id ~key =
      Developer_host.Command.await_terminal
        phase.host
        ~actor:phase.shared.actor
        ~run_id
        ~key
    ;;

    let loop_wait_deadline phase =
      Job_scheduler.For_testing.loop_wait_deadline phase.scheduler
    ;;

    let close_phase_and_wait phase =
      if not !(phase.closed) then Option.value_exn !(phase.shared.active_phase) ()
    ;;

    let persisted_jobs shared ~run_id =
      let data_root =
        Store.Session_store.data_root shared.store |> Store.Data_root.path
      in
      let path =
        Eio.Path.(
          Eio.Stdenv.fs shared.env
          / data_root
          / "developer-runs"
          / ("run_" ^ P.Id.Developer_run.to_string run_id ^ ".json"))
      in
      let payload =
        match Eio.Path.load path |> Jsonaf.parse |> Or_error.ok_exn with
        | `Object fields ->
          (match List.Assoc.find_exn fields ~equal:String.equal "payload" with
           | `Object fields -> fields
           | _ -> failwith "invalid run payload")
        | _ -> failwith "invalid run envelope"
      in
      let jobs =
        match List.Assoc.find_exn payload ~equal:String.equal "jobs" with
        | `Array rows -> rows
        | _ -> failwith "invalid run jobs"
      in
      List.map jobs ~f:(fun json ->
        P.Developer_job.of_json json
        |> Result.map_error ~f:(fun error ->
          Sexp.to_string_hum [%sexp (error : P.Protocol_error.t)])
        |> Result.ok_or_failwith)
    ;;

    let fixture_actor ?(repl = false) () =
      let scopes =
        P.Scope.Set.of_list
          [ List_prompts
          ; List_workspaces
          ; Create_sessions
          ; View_session_transcript
          ; Send_messages
          ; Own_sessions
          ; Answer_approvals
          ; View_security_state
          ; Manage_grants
          ; Read_audit
          ; Stop_sessions
          ; Delete_sessions
          ; Administer_configuration
          ; Diagnostics
          ; Submit_ingress
          ; Provider_view
          ; Provider_manage
          ; Provider_select
          ; Developer_live_run
          ]
      in
      let scopes = if repl then Set.add scopes Developer_repl else scopes in
      let principal =
        P.Principal.create
          ~id:(P.Id.Principal.of_string "pri_operator" |> required "principal ID")
          ~authentication_kind:"test"
          ~scopes
          ~attributes:[]
        |> required "principal"
      in
      Operator_authorization.trusted_local principal
    ;;

    let model_response output =
      `Object
        [ "type", `String "response.completed"
        ; "sequence_number", `Number "1"
        ; ( "response"
          , `Object
              [ "object", `String "response"
              ; "id", `String "retained-model-response"
              ; "status", `String "completed"
              ; "output", `Array output
              ] )
        ]
      |> Jsonaf.to_string
      |> fun json -> "data: " ^ json ^ "\n\n"
    ;;

    let model_text =
      model_response
        [ `Object
            [ "type", `String "message"
            ; "id", `String "retained-model-message"
            ; "role", `String "assistant"
            ; "status", `String "completed"
            ; ( "content"
              , `Array
                  [ `Object
                      [ "type", `String "output_text"
                      ; "text", `String "retained grandchild answer"
                      ; "annotations", `Array []
                      ]
                  ] )
            ]
        ]
    ;;

    let model_queued_producer =
      model_response
        [ `Object
            [ "type", `String "function_call"
            ; "id", `String "retained-queued-item"
            ; "call_id", `String "retained-queued-call"
            ; "name", `String "queued_producer"
            ; "arguments", `String {|{"input":"go"}|}
            ; "status", `String "completed"
            ]
        ]
    ;;

    let%expect_test
        "replaced physical Host applies current depth to the same queued Agent row"
      =
      let run_case second_depth =
        Mirage_crypto_rng_unix.use_default ();
        Eio_main.run (fun env ->
          let real_clock = Eio.Stdenv.clock env in
          Eio.Time.with_timeout_exn real_clock 30. (fun () ->
            let root =
              Filename.concat
                "/tmp"
                ("ochat-host-pair-agent-" ^ P.Id.Transaction.(create () |> to_string))
            in
            let root_path = Eio.Path.(Eio.Stdenv.fs env / root) in
            Eio.Path.mkdir ~perm:0o700 root_path;
            Exn.protect
              ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true root_path)
              ~f:(fun () ->
                let write member contents =
                  Eio.Path.save
                    ~create:(`Exclusive 0o600)
                    Eio.Path.(Eio.Stdenv.fs env / root / member)
                    contents
                in
                write
                  "prompt.chatmd"
                  {|<tool name="child" agent="child.chatmd" local source_mode="file"/>|};
                write
                  "child.chatmd"
                  {|<developer>Retained child marker</developer>
<tool name="grand" agent="grand.chatmd" local source_mode="captured"/>
<script id="queue_grand" language="chatml" kind="tool">
let run ctx input =
  Task.bind(Job.start_tool("grand", input), fun _ -> Task.pure(`Complete(input)))
</script>
<tool name="queued_producer" type="chatml" script="queue_grand" entrypoint="run" input_schema="input.json" output_schema="output.json"><uses tool="grand"/></tool>|};
                write "grand.chatmd" "<developer>Retained grand marker</developer>";
                write "input.json" {|{"type":"object"}|};
                write "output.json" {|{"type":"object"}|};
                Eio.Switch.run (fun sw ->
                  let requests = ref 0 in
                  let grand_requests = ref 0 in
                  let grand_native_attempts = ref 0 in
                  let authorized_names = ref [] in
                  let sent_producer = ref false in
                  let parked = ref false in
                  let wall_clock, pause, resume, frozen_now =
                    controlled_wall_clock real_clock ~initial:(Eio.Time.now real_clock)
                  in
                  let active_phase = ref None in
                  let respond _ body =
                    if String.is_substring body ~substring:"Retained grand marker"
                    then incr grand_requests;
                    if
                      (not !sent_producer)
                      && String.is_substring body ~substring:"Retained child marker"
                    then (
                      sent_producer := true;
                      pause ();
                      let phase = Option.value_exn !active_phase in
                      let rec wait_for_park () =
                        match loop_wait_deadline phase with
                        | Some deadline when Float.(deadline > frozen_now ()) ->
                          parked := true
                        | _ ->
                          Eio.Time.sleep real_clock 0.002;
                          wait_for_park ()
                      in
                      Eio.Time.with_timeout_exn real_clock 5. wait_for_park;
                      model_queued_producer)
                    else model_text
                  in
                  let endpoint = local_model_endpoint env sw requests respond in
                  let actor = fixture_actor () in
                  with_shared
                    ~manifest_grants:[]
                    ~data_dir:(Filename.concat root "data")
                    ~env
                    ~sw
                    ~root
                    ~prompt_file:(Filename.concat root "prompt.chatmd")
                    ~actor
                    ~script_source:
                      {|let main input = Task.bind(Job.start_tool("child", input), fun id -> Task.pure(`String(id)))|}
                    ~provider_factory:(model_provider ~env ~root ~endpoint)
                    ~authorize_native:(fun _ binding ->
                      let name = (Chat_response.Tool_capability.reference binding).name in
                      authorized_names := name :: !authorized_names;
                      if String.equal name "grand" then incr grand_native_attempts;
                      if
                        List.mem
                          [ "child"; "grand"; "queued_producer" ]
                          name
                          ~equal:String.equal
                      then Ok ()
                      else Error (P.Error.invalid_request "unexpected native binding"))
                    ~prepare_output:(function
                      | Openai.Responses.Tool_output.Output.Text value ->
                        Ok (`String value)
                      | _ -> Error (P.Error.invalid_request "unexpected native output"))
                    ~f:(fun shared ->
                      let phase =
                        start_phase shared ~clock:wall_clock ~max_active_agent_calls:2
                      in
                      active_phase := Some phase;
                      let run_id = P.Id.Developer_run.create () in
                      let key =
                        P.Idempotency_key.of_string "selected-host-a"
                        |> required "idempotency key"
                      in
                      let fresh_source () =
                        Developer_command_original.Fresh_source.Document_script
                          { connection = phase.shared.connection }
                      in
                      let request_live = ref true in
                      let denied_key =
                        P.Idempotency_key.of_string "stale-before-reserve"
                        |> required "stale request key"
                      in
                      let denied_resolutions = ref 0 in
                      let stale =
                        Developer_host.Command.submit_resolving
                          phase.host
                          ~actor:phase.shared.actor
                          ~run_id
                          ~key:denied_key
                          ~original:phase.original
                          ~request_current:(fun () -> !request_live)
                          ~validate_capture:(fun _ -> Ok ())
                          ~resolve_fresh:(fun () ->
                            incr denied_resolutions;
                            request_live := false;
                            Ok (fresh_source ()))
                      in
                      (match stale with
                       | Error error ->
                         [%test_eq: P.Error.code] error.code Permission_denied
                       | Ok _ -> failwith "stale request reserved a command");
                      [%test_eq: int] !denied_resolutions 1;
                      assert (
                        Option.is_none
                          (Developer_host.Command.observe
                             phase.host
                             ~actor:phase.shared.actor
                             ~run_id
                             ~key:denied_key
                           |> required "stale request observation"));
                      request_live := true;
                      let incompatible_key =
                        P.Idempotency_key.of_string "source-not-wire-compatible"
                        |> required "incompatible source key"
                      in
                      let native_before = !grand_native_attempts in
                      (match
                         Developer_host.Command.submit_resolving
                           phase.host
                           ~actor:phase.shared.actor
                           ~run_id
                           ~key:incompatible_key
                           ~original:phase.original
                           ~request_current:(fun () -> !request_live)
                           ~validate_capture:(fun _ ->
                             Error
                               (P.Error.invalid_request
                                  "captured source ID exceeds the portable wire bound"))
                           ~resolve_fresh:(fun () -> Ok (fresh_source ()))
                       with
                       | Error error ->
                         [%test_eq: P.Error.code] error.code Invalid_request
                       | Ok _ -> failwith "incompatible capture reserved a command");
                      assert (
                        Option.is_none
                          (Developer_host.Command.observe
                             phase.host
                             ~actor:phase.shared.actor
                             ~run_id
                             ~key:incompatible_key
                           |> required "incompatible capture observation"));
                      [%test_eq: int] !grand_native_attempts native_before;
                      let resolutions = ref 0 in
                      let initial =
                        Developer_host.Command.submit_resolving
                          phase.host
                          ~actor:phase.shared.actor
                          ~run_id
                          ~key
                          ~original:phase.original
                          ~request_current:(fun () -> !request_live)
                          ~validate_capture:(fun capture ->
                            Developer_command_wire_receipt.validate_prepared
                              (Chatml_developer_capture.prepared capture))
                          ~resolve_fresh:(fun () ->
                            incr resolutions;
                            Ok (fresh_source ()))
                        |> required "live submit"
                      in
                      request_live := false;
                      let view =
                        await_terminal phase ~run_id ~key |> required "live terminal"
                      in
                      request_live := true;
                      let replay =
                        Developer_host.Command.submit_resolving
                          phase.host
                          ~actor:phase.shared.actor
                          ~run_id
                          ~key
                          ~original:phase.original
                          ~request_current:(fun () -> !request_live)
                          ~validate_capture:(fun _ ->
                            failwith "historical key revalidated its capture")
                          ~resolve_fresh:(fun () ->
                            failwith "historical key recaptured source")
                        |> required "live replay"
                      in
                      [%test_eq: int] !resolutions 1;
                      assert (
                        Store.Developer_command_row.same_identity
                          (Developer_host.Command.row initial)
                          (Developer_host.Command.row replay));
                      let parent_job =
                        match
                          Option.map
                            (Developer_host.Command.receipt view)
                            ~f:Store.Developer_command_receipt.value
                        with
                        | Some (Live (Succeeded (`String id))) ->
                          P.Id.Job.of_string id |> required "parent Job ID"
                        | result ->
                          raise_s
                            [%sexp
                              "expected selected parent Job"
                            , (result : Store.Developer_command_receipt.value option)
                            , (!requests : int)
                            , (!sent_producer : bool)
                            , (!parked : bool)
                            , (!authorized_names : string list)]
                      in
                      let rec wait_for_queued () =
                        let jobs = persisted_jobs shared ~run_id in
                        match
                          List.find jobs ~f:(fun job ->
                            not (P.Id.Job.equal (P.Developer_job.id job) parent_job))
                        with
                        | Some job -> job
                        | None ->
                          Eio.Time.sleep real_clock 0.002;
                          wait_for_queued ()
                      in
                      let job =
                        Eio.Time.with_timeout_exn real_clock 10. wait_for_queued
                      in
                      let state = P.Developer_job.state job in
                      [%test_eq: int] state.attempt 0;
                      (match state.status with
                       | Queued -> ()
                       | _ -> failwith "selected grandchild was claimed before park");
                      let request =
                        Chat_response.Background_request.of_json
                          ~policy:Chat_response.One_off_request.default_policy
                          (P.Developer_job.payload job)
                        |> required "queued request"
                      in
                      let issuer =
                        Chat_response.Background_request.issuer_source request
                        |> Option.value_exn
                      in
                      let target =
                        Chat_response.Background_request.agent_artifact request
                        |> Option.value_exn
                      in
                      [%test_eq: string]
                        (P.Developer_agent_artifact.root_relative_path
                           (Chat_response.Background_request.Issuer_source.artifact
                              issuer)
                         |> Source.Portable_path.to_string)
                        "child.chatmd";
                      [%test_eq: string]
                        (P.Developer_agent_artifact.root_relative_path target
                         |> Source.Portable_path.to_string)
                        "grand.chatmd";
                      assert !sent_producer;
                      assert !parked;
                      Exn.protect
                        ~f:(fun () ->
                          let done_, done_u = Eio.Promise.create () in
                          Eio.Fiber.fork ~sw (fun () ->
                            Eio.Promise.resolve
                              done_u
                              (Or_error.try_with (fun () -> close_phase_and_wait phase)));
                          let rec wait_for_close () =
                            match loop_wait_deadline phase with
                            | None -> ()
                            | Some _ ->
                              Eio.Time.sleep real_clock 0.002;
                              wait_for_close ()
                          in
                          Eio.Time.with_timeout_exn real_clock 5. wait_for_close;
                          resume ();
                          Eio.Promise.await done_ |> Or_error.ok_exn)
                        ~finally:resume;
                      [%test_eq: int] !requests 1;
                      pause ();
                      Exn.protect ~finally:resume ~f:(fun () ->
                        let phase_b =
                          start_phase
                            shared
                            ~clock:wall_clock
                            ~max_active_agent_calls:second_depth
                        in
                        let rec wait_for_park () =
                          match loop_wait_deadline phase_b with
                          | Some deadline when Float.(deadline > frozen_now ()) -> ()
                          | None | Some _ ->
                            Eio.Time.sleep real_clock 0.002;
                            wait_for_park ()
                        in
                        Eio.Time.with_timeout_exn real_clock 5. wait_for_park;
                        let second_key =
                          P.Idempotency_key.of_string "selected-host-b"
                          |> required "second idempotency key"
                        in
                        ignore
                          (submit_live phase_b ~run_id ~key:second_key
                           |> required "second live submit");
                        let second =
                          await_terminal phase_b ~run_id ~key:second_key
                          |> required "second live terminal"
                        in
                        assert (
                          Chatml_source.Digest.equal
                            (Developer_host.Command.capture_fingerprint view)
                            (Developer_host.Command.capture_fingerprint second));
                        assert (
                          Int.equal
                            (Developer_host.Command.owner view).epoch
                            (Developer_host.Command.owner second).epoch);
                        let old_job () =
                          persisted_jobs shared ~run_id
                          |> List.find_exn ~f:(fun candidate ->
                            P.Id.Job.equal
                              (P.Developer_job.id candidate)
                              (P.Developer_job.id job))
                        in
                        let before = P.Developer_job.state (old_job ()) in
                        [%test_eq: int] before.attempt 0;
                        assert (
                          String.equal
                            (Jsonaf.to_string (P.Developer_job.payload (old_job ())))
                            (Jsonaf.to_string (P.Developer_job.payload job)));
                        assert (
                          P.Execution_owner.equal_developer
                            (P.Developer_job.owner (old_job ()))
                            (Developer_host.Command.owner second));
                        (match before.status with
                         | Queued -> ()
                         | _ ->
                           failwith "old selected row changed before second scheduler ran");
                        resume ();
                        let rec await_old () =
                          let latest = old_job () in
                          match (P.Developer_job.state latest).status with
                          | Queued | Running | Waiting_permission _ | Waiting_completion _
                            ->
                            Eio.Time.sleep real_clock 0.002;
                            await_old ()
                          | Succeeded | Failed _ | Cancelled | Interrupted _ -> latest
                        in
                        let terminal =
                          Eio.Time.with_timeout_exn real_clock 10. await_old
                        in
                        (match second_depth, (P.Developer_job.state terminal).status with
                         | 2, Succeeded -> [%test_eq: int] !grand_requests 1
                         | 1, Failed error ->
                           [%test_eq: int] !grand_requests 0;
                           [%test_eq: int] !grand_native_attempts 0;
                           [%test_eq: int] (P.Developer_job.state terminal).attempt 1;
                           [%test_eq: P.Error.code] error.code Invalid_request;
                           [%test_eq: string]
                             error.message
                             "claimed child execution policy differs from the Live source"
                         | _, status ->
                           raise_s
                             [%sexp
                               "unexpected old row outcome"
                             , (second_depth : int)
                             , (status : P.Job.status)
                             , (!grand_requests : int)]);
                        close_phase_and_wait phase_b;
                        if Int.equal second_depth 2
                        then print_endline "same old selected Agent row completed"
                        else print_endline "changed depth denied the old selected row"))))))
      in
      run_case 2;
      run_case 1;
      [%expect
        {|
        same old selected Agent row completed
        changed depth denied the old selected row
        |}]
    ;;

    let%expect_test "retained REPL Task stages and completes Job and Model work" =
      Mirage_crypto_rng_unix.use_default ();
      Eio_main.run (fun env ->
        let real_clock = Eio.Stdenv.clock env in
        Eio.Time.with_timeout_exn real_clock 30. (fun () ->
          let root =
            Filename.concat
              "/tmp"
              ("ochat-host-repl-jobs-" ^ P.Id.Transaction.(create () |> to_string))
          in
          let root_path = Eio.Path.(Eio.Stdenv.fs env / root) in
          Eio.Path.mkdir ~perm:0o700 root_path;
          Exn.protect
            ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true root_path)
            ~f:(fun () ->
              let write member contents =
                Eio.Path.save
                  ~create:(`Exclusive 0o600)
                  Eio.Path.(Eio.Stdenv.fs env / root / member)
                  contents
              in
              write
                "prompt.chatmd"
                {|<developer>Retained REPL Task root</developer>
<tool name="child" agent="child.chatmd" local source_mode="file"/>
<shell_access id="approval" cwd="${workspace}">
  <capabilities sandbox="direct_unsafe" network="false"
      child_processes="false" arbitrary_code="false" privilege_change="false">
    <write path="${workspace}"/>
  </capabilities>
  <backends merge="replace"><direct when="macos"/><direct when="linux"/></backends>
  <policy default="ask"/>
  <approvals provider="ui" unavailable="deny" scopes="once" durable="false"/>
  <audit format="none"/>
</shell_access>
<tool name="approval_touch" type="shell" mode="fixed" runtime="approval"
      result="stdout">
  <command program="/usr/bin/touch">
    <path_arg base="workspace" path="task-approval-marker"/>
  </command>
  <arguments mode="none"/>
</tool>|};
              write "child.chatmd" "<developer>Retained Task child</developer>";
              write "model.chatmd" "<developer>Retained Task Model worker</developer>";
              let approval_marker =
                Eio.Path.(Eio.Stdenv.fs env / root / "task-approval-marker")
              in
              let actor = fixture_actor ~repl:true () in
              let prompt_file = Filename.concat root "prompt.chatmd" in
              let source_sha256, manifest_sha256 =
                Eio.Switch.run (fun bootstrap_sw ->
                  with_shared
                    ~manifest_grants:[]
                    ~data_dir:(Filename.concat root "bootstrap-data")
                    ~env
                    ~sw:bootstrap_sw
                    ~root
                    ~prompt_file
                    ~actor
                    ~script_source:"let main input = Task.pure(input)"
                    ~provider_factory:(fun ~sw:_ ~server_id:_ -> None, None)
                    ~authorize_native:(fun _ _ -> Ok ())
                    ~prepare_output:(fun _ -> Ok `Null)
                    ~f:(fun shared ->
                      let entry =
                        A.Prompt_catalog.find_by_name shared.prompts "restart.prompt"
                        |> Option.value_exn
                      in
                      match entry.availability with
                      | Ready revision ->
                        let artifact = A.Prompt_revision.artifact revision in
                        ( artifact.root_sha256
                        , Option.value_exn artifact.shell_manifest_sha256 )
                      | Disabled | Unavailable _ ->
                        failwith "Task approval bootstrap prompt was unavailable"))
              in
              let grant =
                Config.Manifest_grant.
                  { id = "retained-task-approval"
                  ; prompt = "restart.prompt"
                  ; workspaces = [ "restart.workspace" ]
                  ; manifest_sha256
                  ; source_sha256
                  ; principals =
                      [ P.Id.Principal.to_string
                          (Operator_authorization.principal actor).id
                      ]
                  }
              in
              Eio.Switch.run (fun sw ->
                let requests = ref 0 in
                let child_requests = ref 0 in
                let model_requests = ref 0 in
                let wall_clock, pause, resume, frozen_now =
                  controlled_wall_clock real_clock ~initial:(Eio.Time.now real_clock)
                in
                pause ();
                let respond _ body =
                  if String.is_substring body ~substring:"Retained Task child"
                  then incr child_requests;
                  if String.is_substring body ~substring:"Retained Task Model worker"
                  then incr model_requests;
                  model_response
                    [ `Object
                        [ "type", `String "message"
                        ; "id", `String "task-job-message"
                        ; "role", `String "assistant"
                        ; "status", `String "completed"
                        ; ( "content"
                          , `Array
                              [ `Object
                                  [ "type", `String "output_text"
                                  ; "text", `String "task job answer"
                                  ; "annotations", `Array []
                                  ]
                              ] )
                        ]
                    ]
                in
                let endpoint = local_model_endpoint env sw requests respond in
                let approval_attempts = ref 0 in
                with_shared
                  ~manifest_grants:[ grant ]
                  ~data_dir:(Filename.concat root "data")
                  ~env
                  ~sw
                  ~root
                  ~prompt_file
                  ~actor
                  ~script_source:"let run ctx input = Task.pure(`Complete(input))"
                  ~provider_factory:(model_provider ~env ~root ~endpoint)
                  ~authorize_native:(fun _ binding ->
                    if
                      List.mem
                        [ "child"; "approval_touch" ]
                        (Chat_response.Tool_capability.reference binding).name
                        ~equal:String.equal
                    then (
                      if
                        String.equal
                          (Chat_response.Tool_capability.reference binding).name
                          "approval_touch"
                      then Int.incr approval_attempts;
                      Ok ())
                    else Error (P.Error.invalid_request "unexpected Task native call"))
                  ~prepare_output:(function
                    | Openai.Responses.Tool_output.Output.Text value -> Ok (`String value)
                    | _ -> Error (P.Error.invalid_request "unexpected Task native output"))
                  ~f:(fun shared ->
                    let approval =
                      Developer_approval_owner.Configuration.create
                        ~limits:
                          (Developer_approval_owner.Limits.create
                             ~max_pending:2
                             ~max_request_bytes:(32 * 1024)
                             ~max_display_bytes:512
                             ~max_decision_reason_bytes:128
                             ~wait_seconds:10.
                           |> required "Task approval limits")
                        ~command_read:(fun _ _ -> true)
                        ~command_respond:(fun _ _ -> true)
                        ~job_read:(fun _ _ -> false)
                        ~job_respond:(fun _ _ -> false)
                    in
                    let trace_read_allowed = ref true in
                    let phase =
                      start_phase
                        ~repl:true
                        ~selected_target:
                          Chatml.Chatml_compiler_contract.Target.Developer_tool_v1
                        ~approval
                        ~trace:(trace_configuration ~read_allowed:trace_read_allowed)
                        shared
                        ~clock:wall_clock
                        ~max_active_agent_calls:2
                    in
                    let rec wait_for_park () =
                      match loop_wait_deadline phase with
                      | Some deadline when Float.(deadline > frozen_now ()) -> ()
                      | None | Some _ ->
                        Eio.Time.sleep real_clock 0.002;
                        wait_for_park ()
                    in
                    Eio.Time.with_timeout_exn real_clock 5. wait_for_park;
                    let module Repl = Developer_host.Repl in
                    let module Trace = Developer_host.Trace in
                    let trace_snapshot lookup =
                      Trace.snapshot phase.host ~actor:shared.actor ~lookup
                      |> required "retained developer trace"
                    in
                    let retained_trace lookup =
                      match trace_snapshot lookup with
                      | Developer_trace_retention.Availability.Retained snapshot ->
                        snapshot
                      | Expired | Unavailable -> failwith "expected a retained trace"
                    in
                    let complete_trace lookup =
                      let snapshot = retained_trace lookup in
                      (match Chatml_trace.Recorder.Snapshot.state snapshot with
                       | Complete terminal
                         when Chatml_trace.Terminal.Disposition.equal
                                (Chatml_trace.Terminal.disposition terminal)
                                Chatml_trace.Terminal.Disposition.Succeeded -> ()
                       | Complete _ | Incomplete ->
                         failwith "joined trace terminal was not successful");
                      snapshot
                    in
                    let has_prior_snippet_site ~statement task =
                      let sources =
                        Chatml_trace.Recorder.Snapshot.header statement
                        |> Chatml_trace.Header.source_inventory
                      in
                      List.exists
                        (Chatml_trace.Recorder.Snapshot.prefix task)
                        ~f:(fun event ->
                          Option.value_map
                            (Chatml_trace.Event.site event)
                            ~default:false
                            ~f:(fun site ->
                              List.exists sources ~f:(fun source ->
                                Chatml_source.Id.equal
                                  (Chatml_source.Location.source site)
                                  (Chatml_trace.Header.Source_ref.id source)
                                && Chatml_source.Digest.equal
                                     (Chatml_source.Location.source_digest site)
                                     (Chatml_trace.Header.Source_ref.digest source))))
                    in
                    let run_id = P.Id.Developer_run.create () in
                    let key name =
                      P.Idempotency_key.of_string name |> required "Task key"
                    in
                    let route =
                      Developer_command_original.Route.Document_script
                        { expected = shared.expected
                        ; selector =
                            P.Authoring.Analysis.Target.selected_script "s"
                            |> required "Task selector"
                        }
                    in
                    let wrong_original =
                      Repl.Original.create
                        ~route
                        ~target:Chatml.Chatml_compiler_contract.Target.One_off_v1
                        ~prompt:(Catalog_identity.prompt_definition "restart.prompt")
                        ~workspace:
                          (Catalog_identity.workspace_definition "restart.workspace")
                        ~requested_execution:None
                        ~requested_max_output_bytes:None
                        ~checkpoint:Eio.Fiber.check
                      |> required "mismatched REPL original"
                    in
                    (match
                       Repl.create
                         phase.host
                         ~actor:shared.actor
                         ~run_id:(P.Id.Developer_run.create ())
                         ~key:(key "repl-wrong-target")
                         ~original:wrong_original
                         ~fresh_source:
                           (Some
                              (Developer_command_original.Fresh_source.Document_script
                                 { connection = shared.connection }))
                     with
                     | Error { code = Lease_stale; _ } -> ()
                     | Ok _ | Error _ ->
                       failwith "mismatched selected REPL ABI was accepted");
                    let original =
                      Repl.Original.create
                        ~route
                        ~target:Chatml.Chatml_compiler_contract.Target.Developer_tool_v1
                        ~prompt:(Catalog_identity.prompt_definition "restart.prompt")
                        ~workspace:
                          (Catalog_identity.workspace_definition "restart.workspace")
                        ~requested_execution:None
                        ~requested_max_output_bytes:(Some 256)
                        ~checkpoint:Eio.Fiber.check
                      |> required "REPL original"
                    in
                    let view =
                      Repl.create
                        phase.host
                        ~actor:shared.actor
                        ~run_id
                        ~key:(key "repl-create")
                        ~original
                        ~fresh_source:
                          (Some
                             (Developer_command_original.Fresh_source.Document_script
                                { connection = shared.connection }))
                      |> required "REPL create"
                    in
                    let source =
                      sprintf
                        {|Task.bind(Tool.call("approval_touch", `Object([])), fun approval ->
  match approval with
  | `Ok(_) ->
    Task.bind(Job.start_tool("child", `Object([{key="input"; value=`String("go")}])), fun child ->
      Task.bind(Model.spawn("agent_prompt_v1", `Object([{key="prompt"; value=`String(%S)}, {key="input"; value=`String("from Task")}, {key="is_local"; value=`Bool(true)}, {key="source_mode"; value=`String("file")}])), fun model ->
        Task.pure(`Object([{key="child"; value=`String(child)}, {key="model"; value=`String(model)}]))))
  | `Error(message) -> Task.fail(message))|}
                        (Filename.concat root "model.chatmd")
                    in
                    let submission =
                      Repl.submit
                        phase.host
                        ~actor:shared.actor
                        ~instance:(Repl.View.instance view)
                        ~epoch:(Repl.View.epoch view)
                        ~expected_revision:(Repl.View.revision view)
                        ~key:(key "repl-expression")
                        ~source
                      |> required "REPL Task expression"
                    in
                    [%test_eq: int] (Repl.Submission.statement_count submission) 1;
                    (match Repl.Submission.details submission with
                     | Output_limited -> ()
                     | Available -> failwith "large frozen Task type was not bounded");
                    [%test_eq: int] (Repl.Submission.task_reference_count submission) 1;
                    let statement_trace =
                      retained_trace
                        (Trace.Repl_statement
                           { instance = Repl.View.instance view
                           ; epoch = Repl.View.epoch view
                           ; key = key "repl-expression"
                           })
                    in
                    assert (
                      not
                        (List.is_empty
                           (Chatml_trace.Recorder.Snapshot.prefix statement_trace)));
                    let reference =
                      match Repl.Submission.task_references submission with
                      | reference :: _ -> reference
                      | [] ->
                        resume ();
                        let status =
                          match Repl.Submission.status submission with
                          | Finished -> "finished"
                          | Need_more _ -> "need_more"
                          | Rejected diagnostic ->
                            "rejected: " ^ Repl.Submission.Diagnostic.message diagnostic
                          | Stopped (Some diagnostic) ->
                            "stopped: " ^ Repl.Submission.Diagnostic.message diagnostic
                          | Stopped None -> "stopped"
                        in
                        let details =
                          match Repl.Submission.details submission with
                          | Available -> "available"
                          | Output_limited -> "output_limited"
                        in
                        failwith ("REPL Task reference absent: " ^ status ^ "/" ^ details)
                    in
                    let replayed_submission =
                      Repl.submit
                        phase.host
                        ~actor:shared.actor
                        ~instance:(Repl.View.instance view)
                        ~epoch:(Repl.View.epoch view)
                        ~expected_revision:(Repl.View.revision view)
                        ~key:(key "repl-expression")
                        ~source
                      |> required "bounded REPL submission replay"
                    in
                    [%test_eq: int]
                      (Repl.Submission.task_reference_count replayed_submission)
                      1;
                    assert (
                      List.exists
                        (Repl.Submission.task_references replayed_submission)
                        ~f:(Repl.Task_reference.equal reference));
                    let page =
                      Repl.task_references_page
                        phase.host
                        ~actor:shared.actor
                        ~instance:(Repl.View.instance view)
                        ~epoch:(Repl.View.epoch view)
                        ~key:(key "repl-expression")
                        ~after:None
                        ~limit:1
                      |> required "REPL reference page"
                    in
                    [%test_eq: int] (Repl.Task_reference_page.total page) 1;
                    (match Repl.Task_reference_page.items page with
                     | [ only ] -> assert (Repl.Task_reference.equal only reference)
                     | _ -> failwith "REPL reference page changed origin count");
                    assert (Option.is_none (Repl.Task_reference_page.next page));
                    let inspected =
                      Repl.inspect_type
                        phase.host
                        ~actor:shared.actor
                        ~instance:(Repl.View.instance view)
                        ~epoch:(Repl.View.epoch view)
                        ~key:(key "repl-expression")
                        ~statement_index:0
                        ~selector:Repl.Type_selector.Expression
                        ~max_bytes:512
                      |> required "frozen REPL type inspection"
                    in
                    assert (Option.is_none (Repl.Type_inspection.binding_name inspected));
                    assert (
                      not
                        (String.is_empty
                           (Chatml_type_display.text
                              (Repl.Type_inspection.rendered inspected))));
                    let task_original =
                      Repl.Task.Original.create
                        ~reference
                        ~requested_execution:None
                        ~requested_max_output_bytes:None
                        ~checkpoint:Eio.Fiber.check
                      |> required "Task original"
                    in
                    let initial =
                      Repl.Task.submit
                        phase.host
                        ~actor:shared.actor
                        ~key:(key "task-run")
                        ~original:task_original
                        ~fresh_epoch:
                          (Some (Repl.View.instance view, Repl.View.epoch view))
                      |> required "Task submit"
                    in
                    assert (not (Eio.Path.is_file approval_marker));
                    let rec wait_for_approval () =
                      match
                        Developer_host.Approval.pending
                          phase.host
                          ~actor:shared.actor
                          ~run_id
                      with
                      | Ok [ pending ] -> pending
                      | Ok [] | Error { code = Invalid_request; _ } ->
                        (match
                           Repl.Task.observe
                             phase.host
                             ~actor:shared.actor
                             ~run_id
                             ~key:(key "task-run")
                         with
                         | Ok (Some current)
                           when match
                                  Store.Developer_command_row.phase
                                    (Repl.Task.row current)
                                with
                                | Terminal _ -> true
                                | Reserved | Waiting_permission | Execution_started ->
                                  false ->
                           let receipt =
                             Option.map
                               (Repl.Task.receipt current)
                               ~f:Store.Developer_command_receipt.value
                           in
                           resume ();
                           raise_s
                             [%sexp
                               "Task ended before Shell approval"
                             , (receipt : Store.Developer_command_receipt.value option)
                             , (!approval_attempts : int)]
                         | Ok None | Ok (Some _) | Error _ -> ());
                        Eio.Time.sleep real_clock 0.002;
                        wait_for_approval ()
                      | Ok _ -> failwith "Task published multiple approvals"
                      | Error error ->
                        raise_s [%sexp "Task approval lookup failed", (error : P.Error.t)]
                    in
                    let pending =
                      Eio.Time.with_timeout_exn real_clock 5. wait_for_approval
                    in
                    (match Developer_host.Approval.Pending.owner pending with
                     | Command receipt_id ->
                       assert (
                         Store.Developer_command_receipt.Id.equal
                           receipt_id
                           (Store.Developer_command_row.id (Repl.Task.row initial)))
                     | Claimed_job _ -> failwith "Task approval belonged to a Job");
                    (match Developer_host.Approval.Pending.execution pending with
                     | Native_invocation _ -> ()
                     | Claimed_model_job _ ->
                       failwith "Task approval had a Model claim origin");
                    [%test_eq: string]
                      (Developer_host.Approval.Pending.runtime_id pending)
                      "approval";
                    Developer_host.Approval.respond
                      phase.host
                      ~actor:shared.actor
                      ~run_id
                      ~id:(Developer_host.Approval.Pending.id pending)
                      Developer_host.Approval.Approve_once
                    |> required "Task approval response";
                    let terminal =
                      Repl.Task.await_terminal
                        phase.host
                        ~actor:shared.actor
                        ~run_id
                        ~key:(key "task-run")
                      |> required "Task terminal"
                    in
                    let receipt =
                      Option.map
                        (Repl.Task.receipt terminal)
                        ~f:Store.Developer_command_receipt.value
                    in
                    (match receipt with
                     | Some
                         (Task
                            (Store.Developer_command_receipt.Task.Succeeded
                               { value_preview = Some _ })) -> ()
                     | _ ->
                       resume ();
                       raise_s
                         [%sexp
                           "Task Job/Model receipt was not successful"
                         , (receipt : Store.Developer_command_receipt.value option)]);
                    let task_trace =
                      complete_trace (Trace.Repl_task { run_id; key = key "task-run" })
                    in
                    assert (has_prior_snippet_site ~statement:statement_trace task_trace);
                    assert (
                      not
                        (List.is_empty (Chatml_trace.Recorder.Snapshot.prefix task_trace)));
                    let staged = persisted_jobs shared ~run_id in
                    [%test_eq: int] (List.length staged) 2;
                    List.iter staged ~f:(fun job ->
                      let state = P.Developer_job.state job in
                      [%test_eq: int] state.attempt 0;
                      match state.status with
                      | Queued -> ()
                      | _ -> failwith "Task child claimed before scheduler resume");
                    resume ();
                    let rec await_jobs () =
                      let jobs = persisted_jobs shared ~run_id in
                      if
                        List.for_all jobs ~f:(fun job ->
                          match (P.Developer_job.state job).status with
                          | Succeeded | Failed _ | Cancelled | Interrupted _ -> true
                          | Queued | Running | Waiting_permission _ | Waiting_completion _
                            -> false)
                      then jobs
                      else (
                        Eio.Time.sleep real_clock 0.002;
                        await_jobs ())
                    in
                    let finished = Eio.Time.with_timeout_exn real_clock 15. await_jobs in
                    let replay =
                      Repl.Task.submit
                        phase.host
                        ~actor:shared.actor
                        ~key:(key "task-run")
                        ~original:task_original
                        ~fresh_epoch:None
                      |> required "Task replay"
                    in
                    assert (
                      Store.Developer_command_row.same_identity
                        (Repl.Task.row initial)
                        (Repl.Task.row replay));
                    let second =
                      Repl.submit
                        phase.host
                        ~actor:shared.actor
                        ~instance:(Repl.View.instance view)
                        ~epoch:(Repl.View.epoch view)
                        ~expected_revision:(Repl.Submission.revision submission)
                        ~key:(key "repl-second-expression")
                        ~source:"Task.pure(8)"
                      |> required "second retained Task"
                    in
                    [%test_eq: int] (Repl.Submission.task_reference_count second) 1;
                    (match
                       Repl.inspect_type
                         phase.host
                         ~actor:shared.actor
                         ~instance:(Repl.View.instance view)
                         ~epoch:(Repl.View.epoch view)
                         ~key:(key "repl-expression")
                         ~statement_index:0
                         ~selector:Repl.Type_selector.Expression
                         ~max_bytes:512
                     with
                     | Error { code = Lease_stale; _ } -> ()
                     | Ok _ | Error _ ->
                       failwith "older frozen type detail did not expire at its cap");
                    let retained_page =
                      Repl.task_references_page
                        phase.host
                        ~actor:shared.actor
                        ~instance:(Repl.View.instance view)
                        ~epoch:(Repl.View.epoch view)
                        ~key:(key "repl-expression")
                        ~after:None
                        ~limit:1
                      |> required "old Task reference after type-detail eviction"
                    in
                    (match Repl.Task_reference_page.items retained_page with
                     | [ old ] -> assert (Repl.Task_reference.equal old reference)
                     | _ -> failwith "type-detail eviction lost a Task origin");
                    let reset =
                      Repl.reset_and_wait
                        phase.host
                        ~actor:shared.actor
                        ~instance:(Repl.View.instance view)
                        ~expected_epoch:(Repl.View.epoch view)
                        ~key:(key "repl-reset")
                      |> required "REPL reset"
                    in
                    assert (
                      not
                        (Repl.Epoch.equal (Repl.View.epoch view) (Repl.View.epoch reset)));
                    let statement_lookup =
                      Trace.Repl_statement
                        { instance = Repl.View.instance view
                        ; epoch = Repl.View.epoch view
                        ; key = key "repl-expression"
                        }
                    in
                    let task_lookup = Trace.Repl_task { run_id; key = key "task-run" } in
                    ignore
                      (complete_trace statement_lookup : Chatml_trace.Recorder.Snapshot.t);
                    assert (
                      has_prior_snippet_site
                        ~statement:statement_trace
                        (complete_trace task_lookup));
                    let historical =
                      Repl.submit
                        phase.host
                        ~actor:shared.actor
                        ~instance:(Repl.View.instance view)
                        ~epoch:(Repl.View.epoch view)
                        ~expected_revision:(Repl.View.revision view)
                        ~key:(key "repl-expression")
                        ~source
                      |> required "historical compact REPL replay"
                    in
                    assert (
                      Repl.Revision.equal
                        (Repl.Submission.revision submission)
                        (Repl.Submission.revision historical));
                    [%test_eq: int] (Repl.Submission.task_reference_count historical) 1;
                    assert (List.is_empty (Repl.Submission.task_references historical));
                    assert (List.is_empty (Repl.Submission.task_handles historical));
                    assert (List.is_empty (Repl.Submission.statements historical));
                    (match
                       Repl.task_references_page
                         phase.host
                         ~actor:shared.actor
                         ~instance:(Repl.View.instance view)
                         ~epoch:(Repl.View.epoch view)
                         ~key:(key "repl-expression")
                         ~after:None
                         ~limit:1
                     with
                     | Error { code = Lease_stale; _ } -> ()
                     | Ok _ | Error _ ->
                       failwith "reset did not expire the old reference page");
                    let replacement =
                      Repl.submit
                        phase.host
                        ~actor:shared.actor
                        ~instance:(Repl.View.instance reset)
                        ~epoch:(Repl.View.epoch reset)
                        ~expected_revision:(Repl.View.revision reset)
                        ~key:(key "trace-replacement")
                        ~source:"Task.pure(9)"
                      |> required "replacement trace submission"
                    in
                    (match Repl.Submission.status replacement with
                     | Finished -> ()
                     | Need_more _ | Rejected _ | Stopped _ ->
                       failwith "replacement trace submission did not finish");
                    (match trace_snapshot statement_lookup with
                     | Developer_trace_retention.Availability.Unavailable -> ()
                     | Expired | Retained _ ->
                       failwith "oldest settled trace was not evicted");
                    ignore (complete_trace task_lookup : Chatml_trace.Recorder.Snapshot.t);
                    Eio.Path.save
                      ~create:(`Or_truncate 0o600)
                      Eio.Path.(Eio.Stdenv.fs env / root / "prompt.chatmd")
                      "<developer>edited after execution</developer>";
                    (match trace_snapshot statement_lookup with
                     | Developer_trace_retention.Availability.Unavailable -> ()
                     | Expired | Retained _ ->
                       failwith "evicted trace was restored by source edit");
                    assert (
                      has_prior_snippet_site
                        ~statement:statement_trace
                        (complete_trace task_lookup));
                    trace_read_allowed := false;
                    (match
                       Trace.snapshot phase.host ~actor:shared.actor ~lookup:task_lookup
                     with
                     | Error { code = Permission_denied; _ } -> ()
                     | Ok _ | Error _ -> failwith "revoked trace read was admitted");
                    trace_read_allowed := true;
                    close_phase_and_wait phase;
                    [%test_eq: int] (List.length (persisted_jobs shared ~run_id)) 2;
                    [%test_eq: int] !child_requests 1;
                    [%test_eq: int] !model_requests 1;
                    [%test_eq: int] !requests 2;
                    [%test_eq: int] !approval_attempts 1;
                    assert (Eio.Path.is_file approval_marker);
                    let kinds = List.map finished ~f:P.Developer_job.kind in
                    assert (List.mem kinds Async_tool ~equal:P.Job.equal_kind);
                    assert (List.mem kinds Model_call ~equal:P.Job.equal_kind);
                    List.iter finished ~f:(fun job ->
                      match (P.Developer_job.state job).status with
                      | Succeeded -> ()
                      | status ->
                        raise_s
                          [%sexp
                            "Task child Job did not succeed", (status : P.Job.status)]);
                    print_endline "Task Job and Model completed after durable staging")))));
      [%expect {| Task Job and Model completed after durable staging |}]
    ;;
  end)
;;
