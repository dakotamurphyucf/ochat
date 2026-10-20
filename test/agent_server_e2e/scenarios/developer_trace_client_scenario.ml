open! Core
module P = Agent_protocol
module S = Agent_server
module D = P.Developer_common
module W = P.Developer_wire
module T = P.Developer_trace_wire
module R = P.Developer_repl_wire
module Client = Agent_client

let fail message = raise_s [%sexp "typed trace E2E", (message : string)]
let require value message = if not value then fail message

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp "typed trace E2E protocol", (error : P.Error.t)]
;;

let admitted = function
  | Ok value -> value
  | Error _ -> fail "typed trace E2E lower limits failed"
;;

let document ~bytes ~depth ~fields ~nodes =
  Document_schema.Limits.create
    ~max_bytes:bytes
    ~max_depth:depth
    ~max_fields:fields
    ~max_nodes:nodes
  |> admitted
;;

let developer_limits () =
  D.Limits.create
    ~request:(document ~bytes:(1024 * 1024) ~depth:24 ~fields:2048 ~nodes:4096)
    ~receipt:(document ~bytes:((1024 * 1024) + 362) ~depth:17 ~fields:1025 ~nodes:2049)
    ~response:(document ~bytes:(2 * 1024 * 1024) ~depth:24 ~fields:2048 ~nodes:4096)
    ~approval_page:(document ~bytes:(1024 * 1024) ~depth:16 ~fields:1024 ~nodes:2048)
    ~max_output_bytes:65536
    ~max_fixture_steps:8
    ~max_fixture_bytes:4096
    ~max_approval_display_bytes:256
    ~max_approval_decision_reason_bytes:256
    ~max_approval_page:2
    ~max_event_bytes:4096
  |> ok
;;

let trace_limits () =
  T.Limits.create
    ~request:(document ~bytes:4096 ~depth:12 ~fields:128 ~nodes:256)
    ~page:(document ~bytes:80000 ~depth:12 ~fields:256 ~nodes:256)
    ~response:(document ~bytes:96000 ~depth:16 ~fields:300 ~nodes:300)
    ~max_page_items:16
    ~max_cursor_bytes:2048
  |> ok
;;

let repl_wire_limits () =
  R.Limits.create
    ~request:(document ~bytes:512000 ~depth:24 ~fields:2048 ~nodes:4096)
    ~result:(document ~bytes:(2 * 1024 * 1024) ~depth:20 ~fields:2048 ~nodes:4096)
    ~page:(document ~bytes:8192 ~depth:8 ~fields:128 ~nodes:128)
    ~response:(document ~bytes:(3 * 1024 * 1024) ~depth:24 ~fields:4096 ~nodes:8192)
    ~max_source_bytes:4096
    ~max_reference_bytes:384
    ~max_output_bytes:32768
    ~max_page_items:8
    ~max_event_bytes:4096
    ~max_registry_entries:18
  |> ok
;;

let repl_configuration () =
  let parse =
    Chatml.Chatml_parse.Submission.Limits.create
      ~max_source_bytes:4096
      ~max_tokens:4096
      ~max_work_steps:100_000
      ~sites:
        (Chatml.Chatml_syntax_sites.Limits.create ~max_sites:4096 ~max_name_bytes:16384
         |> admitted)
    |> admitted
  in
  let display =
    Chatml_typechecker.Type_snapshot.Limits.create
      ~max_nodes:4096
      ~max_edges:4096
      ~max_label_bytes:16384
    |> admitted
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
    |> admitted
  in
  let typing_limits =
    Chatml_repl_typing.Limits.create
      ~parse
      ~display
      ~max_display_name_bytes:4096
      ~max_statements:16
      ~root_copy
    |> admitted
  in
  S.Daemon.Developer_configuration.Repl.create
    ~scope_limits:
      (Chatml_configured_lookup.Repl_scope.Limits.create
         ~max_snippets:16
         ~max_identity_bytes:4096
       |> admitted)
    ~root_limits:
      (Chatml_dependency_session.Roots.Limits.create
         ~max_checked_modules:16
         ~max_edges:64
         ~max_dependency_depth:16
       |> admitted)
    ~typing_limits
    ~evaluator_limits:
      (Chatml_repl_execution.Limits.create
         ~max_preview_bytes:4096
         ~max_preview_nodes:1024
         ~max_preview_depth:16
         ~max_diagnostic_bytes:1024
         ~max_retained_tasks:8
       |> admitted)
    ~execution_policy:(Bounded Chatml_execution.default_limits)
    ~type_display_limits:
      (Chatml_type_display.Limits.create
         ~max_nodes:4096
         ~max_edges:8192
         ~max_depth:32
         ~max_work_steps:131072
         ~max_text_bytes:4096
         ~max_output_bytes:32768
       |> admitted)
    ~max_instances:2
    ~max_keys_per_instance:4
    ~max_submissions_per_epoch:8
    ~max_source_bytes:4096
    ~max_receipt_bytes:4096
    ~max_total_receipt_bytes:(1024 * 1024)
    ~max_total_type_detail_bytes:(1024 * 1024)
    ~max_task_origins:8
    ~max_reference_page_bytes:4096
  |> ok
;;

let daemon_options source =
  let module Config = S.Daemon.Developer_configuration in
  let recorder =
    Chatml_trace.Recorder.Limits.create
      ~max_events:256
      ~max_normal_bytes:65536
      ~max_terminal_bytes:1024
    |> admitted
  in
  let retention_limits =
    Config.Traces.Limits.create
      ~max_traces:4
      ~max_retained_bytes:(2 * 1024 * 1024)
      ~max_source_refs_per_trace:8
      ~max_provenance_bytes_per_trace:4096
      ~max_expired_tombstones:4
      ~max_age_seconds:60.
      ~max_header_bytes:4096
      ~recorder
    |> admitted
  in
  let traces =
    Config.Traces.create ~mode:Chatml_trace.Recorder.Mode.Full ~limits:retention_limits
  in
  let portable =
    Config.Portable.create
      ~wire_limits:(developer_limits ())
      ~max_connections:8
      ~max_watches_per_connection:4
      ~max_pending_hints_per_connection:8
    |> ok
    |> fun portable ->
    Config.Portable.with_trace_wire portable (trace_limits ())
    |> ok
    |> fun portable -> Config.Portable.with_repl_wire portable (repl_wire_limits ()) |> ok
  in
  let developer =
    Support.Developer_fixture.developer_configuration
      ~capture_policy:(Support.Trace_source_fixture.capture_policy source)
      ~max_runs:4
      ()
    |> fun configuration ->
    Config.with_portable_wire configuration portable
    |> fun configuration ->
    Config.with_traces configuration traces
    |> fun configuration -> Config.with_repl configuration (repl_configuration ())
  in
  { (Support.Daemon_host.with_offline_inference S.Daemon.default_options) with
    authoring = Some (Support.Trace_source_fixture.authoring_policy source)
  ; developer = Some developer
  }
;;

let initialize connection =
  let implementation =
    P.Initialize.Implementation.create ~name:"typed-trace-e2e" ~version:"1" |> ok
  in
  let request =
    P.Initialize.Request.create
      ~implementation
      ~protocol_min:P.Version.current
      ~protocol_max:P.Version.current
      ~features:[ D.Features.run; R.Features.repl; T.Features.trace ]
      ~event_encodings:[ Json ]
      ~max_inbound_event_bytes:(2 * 1024 * 1024)
      ()
    |> ok
  in
  let response =
    match
      Client.Connection.request_without_history connection (Protocol_initialize request)
    with
    | Ok (Protocol_initialize response) -> response
    | Ok _ -> fail "Initialize changed method"
    | Error error -> raise_s [%sexp "trace Initialize failed", (error : P.Error.t)]
  in
  require
    (List.mem response.enabled_features T.Features.trace ~equal:String.equal)
    "trace feature was not selected";
  Client.Developer_trace_connection.create connection |> ok
;;

let key text = P.Idempotency_key.of_string text |> ok

let submit_fixture connection source ~key_text =
  let developer = Client.Developer_connection.create connection |> ok in
  let limits =
    Client.Developer_connection.capabilities developer |> D.Capabilities.limits
  in
  let run_id = P.Id.Developer_run.create () in
  let key = key key_text in
  let route =
    D.Route.local_project
      ~limits:(D.Limits.request limits)
      ~root_index:0
      ~dependency_root_indices:[ 1 ]
      ~project:(Support.Trace_source_fixture.project source)
      ~selection:(Entry { name = "main"; environment = None })
    |> ok
  in
  let fixtures =
    D.Fixture.create
      ~limits:(D.Limits.request limits)
      ~max_steps:(D.Limits.max_fixture_steps limits)
      ~max_bytes:(D.Limits.max_fixture_bytes limits)
      ~steps:[]
    |> ok
  in
  let request =
    D.Submit.create
      ~limits
      ~run_id
      ~key
      ~kind:P.Developer_receipt.Kind.Fixture_run
      ~route
      ~target:D.Target.One_off_v1
      ~prompt:None
      ~workspace:None
      ~entrypoint:"main"
      ~input:`Null
      ~fixtures:(Some fixtures)
      ~requested_execution:None
      ~requested_max_output_bytes:None
    |> ok
  in
  (match Client.Developer_connection.send developer (W.Request.Submit request) |> ok with
   | W.Response.Submit _ -> ()
   | _ -> fail "Fixture Submit changed method");
  let lookup = D.Lookup.create ~run_id ~key in
  (match
     Client.Developer_connection.send developer (W.Request.Await_terminal lookup) |> ok
   with
   | W.Response.Await_terminal view ->
     (match D.View.phase view with
      | Terminal { receipt; _ } ->
        (match P.Developer_receipt.value receipt with
         | Fixture (Completed { outcome = Succeeded _; _ }) -> ()
         | _ ->
           raise_s
             [%sexp
               "two-root Fixture did not succeed"
             , (P.Developer_receipt.to_json receipt : Jsonaf.t)])
      | _ -> fail "two-root Fixture did not settle")
   | _ -> fail "Fixture Await changed method");
  lookup
;;

let page_locator trace locator =
  let limits =
    Client.Developer_trace_connection.capabilities trace |> T.Capabilities.limits
  in
  let request = T.Page_request.create ~limits ~locator ~after:None ~limit:16 |> ok in
  request, Client.Developer_trace_connection.page trace request
;;

let page trace lookup = page_locator trace (T.Locator.Command lookup)

let require_retained = function
  | Ok (T.Availability.Retained retained) -> retained
  | Ok (Unavailable | Expired) -> fail "recorded Fixture trace was unavailable"
  | Error error -> raise_s [%sexp "trace page failed", (error : P.Error.t)]
;;

let require_repl_traces connection trace source =
  let repl = Client.Developer_repl_connection.create connection |> ok in
  let limits =
    Client.Developer_repl_connection.capabilities repl |> R.Capabilities.limits
  in
  let route =
    D.Route.local_project
      ~limits:(R.Limits.request limits)
      ~root_index:0
      ~dependency_root_indices:[ 1 ]
      ~project:(Support.Trace_source_fixture.project source)
      ~selection:(Entry { name = "main"; environment = None })
    |> ok
  in
  let create =
    R.Create.create
      ~limits
      ~run_id:(P.Id.Developer_run.create ())
      ~key:(key "trace-repl-create")
      ~route
      ~target:One_off_v1
      ~prompt:(S.Catalog_identity.prompt_definition "smoke")
      ~workspace:(S.Catalog_identity.workspace_definition "physical")
      ~requested_execution:None
      ~requested_max_output_bytes:None
    |> ok
  in
  let view = Client.Developer_repl_connection.create_instance repl create |> ok in
  let submit_key = key "trace-repl-submit" in
  let submit =
    R.Submit.create
      ~limits
      ~instance:(R.View.instance view)
      ~epoch:(R.View.epoch view)
      ~expected_revision:(R.View.revision view)
      ~key:submit_key
      ~source:"Task.pure(8)"
    |> ok
  in
  let submitted = Client.Developer_repl_connection.submit repl submit |> ok in
  let statement =
    T.Locator.Repl_statement
      { instance = R.View.instance view; epoch = R.View.epoch view; key = submit_key }
  in
  let _, result = page_locator trace statement in
  ignore (require_retained result : T.Page.t);
  let reference =
    match R.Submission.task_references submitted with
    | [ reference ] -> reference
    | _ -> fail "REPL submission did not expose one Task reference"
  in
  let task_key = key "trace-repl-task" in
  let task_submit =
    R.Task_submit.create
      ~limits
      ~key:task_key
      ~reference
      ~requested_execution:None
      ~requested_max_output_bytes:None
      ~fresh_epoch:(Some (R.View.instance view, R.View.epoch view))
    |> ok
  in
  ignore
    (Client.Developer_repl_connection.task_submit repl ~view task_submit |> ok
     : R.Task_view.t);
  let task_lookup = D.Lookup.create ~run_id:(R.View.run_id view) ~key:task_key in
  ignore
    (Client.Developer_repl_connection.task_await_terminal repl task_lookup |> ok
     : R.Task_view.t);
  let _, result = page_locator trace (T.Locator.Repl_task task_lookup) in
  ignore (require_retained result : T.Page.t);
  let reset =
    R.Current_epoch.create
      ~instance:(R.View.instance view)
      ~epoch:(R.View.epoch view)
      ~key:(key "trace-repl-reset")
  in
  ignore (Client.Developer_repl_connection.reset_and_wait repl reset |> ok : R.View.t);
  let _, result = page_locator trace statement in
  ignore (require_retained result : T.Page.t)
;;

let require_dependency_site retained =
  let site_in_dependency event =
    Option.value_map (T.Event.site event) ~default:false ~f:(fun location ->
      let source = T.Location.source location in
      String.equal (Chatml_source.Id.relative_path source) "util.chatml"
      && String.equal (Chatml_source.Id.package_namespace source) "dep")
  in
  require
    (List.exists (T.Page.events retained) ~f:site_in_dependency)
    "trace lost authentic dependency authored site";
  let encoded =
    T.Response.to_json
      (T.Response.Page (T.Availability.Retained retained))
      ~limits:(trace_limits ())
    |> ok
    |> Jsonaf.to_string
  in
  require
    (not (String.is_substring encoded ~substring:"let value = 1"))
    "trace page disclosed dependency source bytes"
;;

let require_revoked source trace request =
  Support.Trace_source_fixture.revoke_dependency source;
  (match Client.Developer_trace_connection.page trace request with
   | Error { code = P.Error.Permission_denied; _ } -> ()
   | Error error ->
     raise_s [%sexp "revoked dependency had another error", (error : P.Error.t)]
   | Ok _ -> fail "revoked dependency root still authorized retained trace");
  Support.Trace_source_fixture.restore_dependency source
;;

let reserve_port env =
  Eio.Switch.run (fun sw ->
    let reservation = Support.Port_reservation.create ~sw ~env in
    let port = Support.Port_reservation.port reservation in
    Support.Port_reservation.release reservation;
    port)
;;

let start_socket_listener ~sw env fixture daemon =
  let socket_path = Support.Config_fixture.unix_socket fixture in
  Agent_transport_socket.Server.prepare_path ~env ~socket_path |> ok;
  Eio.Fiber.fork_daemon ~sw (fun () ->
    Eio.Switch.run (fun listener_sw ->
      Agent_transport_socket.Server.run
        ~sw:listener_sw
        ~net:(Eio.Stdenv.net env)
        ~socket_path
        ~backlog:8
        ~dispatcher:(S.Daemon.dispatcher daemon)
        ~close_connection:(S.Daemon.close_connection daemon)
        ~authenticate:(fun flow _ ->
          Agent_transport_socket.Peer_credentials.authenticate_same_user_actor
            ~scopes:Support.Config_fixture.all_scopes
            flow)
        ~max_line_length:(16 * 1024 * 1024)
        ~outgoing_capacity:32
        ~max_attachments:
          S.Daemon.default_options.protocol_limits.max_attachments_per_connection
        ~on_error:raise
        ~on_protocol_error:(fun error ->
          raise_s [%sexp "trace socket protocol", (error : P.Error.t)]));
    `Stop_daemon);
  let rec ready attempts =
    if attempts = 0 then fail "trace Unix socket did not become ready";
    match Eio.Path.kind ~follow:false Eio.Path.(Eio.Stdenv.fs env / socket_path) with
    | `Socket -> ()
    | _ ->
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.02;
      ready (attempts - 1)
  in
  ready 100
;;

let run_remote env environment source fixture =
  let options = daemon_options source in
  Support.Daemon_host.with_ env fixture ~options (fun sw daemon ->
    start_socket_listener ~sw env fixture daemon;
    let uri =
      Uri.of_string
        (sprintf "http://127.0.0.1:%d" (Support.Config_fixture.http_port fixture))
    in
    let http () =
      Agent_transport_http.Client.connect
        ~sw
        ~env
        ~uri
        ~bearer_token:(Some (Support.Config_fixture.public_token fixture))
        ~notification_capacity:32
      |> ok
    in
    let first = http () in
    let trace = initialize first in
    let lookup = submit_fixture first source ~key_text:"trace-primary-fixture" in
    let request, result = page trace lookup in
    require_dependency_site (require_retained result);
    require_repl_traces first trace source;
    let second = http () in
    let _ = initialize second in
    let reconnected = Client.Developer_trace_connection.reconnect trace second |> ok in
    require_retained (Client.Developer_trace_connection.page reconnected request)
    |> require_dependency_site;
    require_revoked source reconnected request;
    Client.Connection.close first;
    Client.Connection.close second;
    let unix =
      Agent_transport_socket.Client.connect
        ~sw
        ~net:(Eio.Stdenv.net env)
        ~socket_path:(Support.Config_fixture.unix_socket fixture)
        ~max_line_length:(16 * 1024 * 1024)
        ~notification_capacity:32
    in
    let unix_trace = initialize unix in
    let unix_lookup = submit_fixture unix source ~key_text:"trace-unix-fixture" in
    let _, result = page unix_trace unix_lookup in
    require_dependency_site (require_retained result);
    Client.Connection.close unix;
    let token_file =
      Filename.concat
        (Filename.dirname (Support.Config_fixture.config_path fixture))
        "trace-bearer.token"
    in
    Eio.Path.save
      ~create:(`Exclusive 0o600)
      Eio.Path.(Eio.Stdenv.fs env / token_file)
      (Support.Config_fixture.public_token fixture ^ "\n");
    let executable = Sys.getenv "OCHAT_E2E_STDIO_EXE" |> Option.value_exn in
    let argv =
      [ executable; "--connect"; Uri.to_string uri; "--bearer-token-file"; token_file ]
    in
    let limits =
      Agent_transport_stdio.Client.Limits.create
        ~max_line_length:(16 * 1024 * 1024)
        ~notification_capacity:32
        ~max_stderr_bytes:65536
        ~shutdown_grace_seconds:2.
      |> ok
    in
    Agent_transport_stdio.Client.with_spawned ~env ~argv ~limits ~f:(fun child ->
      let connection = Agent_transport_stdio.Client.connection child in
      let trace = initialize connection in
      let lookup = submit_fixture connection source ~key_text:"trace-stdio-fixture" in
      let _, result = page trace lookup in
      require_dependency_site (require_retained result);
      Ok ())
    |> ok;
    ignore environment)
;;

let run_embedded env source fixture =
  let directory = Filename.dirname (Support.Config_fixture.prompt_path fixture) in
  let policy_path = Filename.concat directory "embedded-trace-policy.sexp" in
  let primary =
    Support.Trace_source_fixture.primary_root source |> Sexp.Atom |> Sexp.to_string_mach
  in
  let dependency =
    Support.Trace_source_fixture.dependency_root source
    |> Sexp.Atom
    |> Sexp.to_string_mach
  in
  let capacity =
    S.Developer_startup_configuration.Capacity.standard_v1
    |> S.Developer_startup_configuration.Capacity.sexp_of_t
    |> Sexp.to_string_mach
  in
  let policy =
    sprintf
      {|(project_sources ((external_roots (%s %s))))
        (authoring_project_access
         ((read_grants
           (((root %s) (principal_id Embedded_operator)
             (authentication_kind embedded.local))
            ((root %s) (principal_id Embedded_operator)
             (authentication_kind embedded.local))))
          (selections
           (((root %s) (project dependent.project.json)
             (request (entry ((name main)))) (target one_off_v1))))))
        (developer
         ((selections
           (((source
              (Local_project
               (root %s) (project dependent.project.json)
               (request (Entry (name main) (environment ())))))
             (target One_off_v1)
             (principals
              (((principal_id Embedded_operator)
                (authentication_kind embedded.local)))))))
          (capacity %s) (model Disabled) (approvals Disabled)
          (repl Standard_v1) (portable true) (traces Trace_full)))
        (developer_scopes (developer.fixture.run developer.repl))|}
      primary
      dependency
      primary
      dependency
      primary
      primary
      capacity
  in
  Eio.Path.save
    ~create:(`Exclusive 0o600)
    Eio.Path.(Eio.Stdenv.fs env / policy_path)
    policy;
  let local_policy =
    S.Config_validator.Embedded_local.load ~env ~path:policy_path
    |> Result.map_error ~f:(fun diagnostics ->
      Sexp.to_string_hum ([%sexp_of: S.Config.Diagnostic.t list] diagnostics))
    |> Result.ok_or_failwith
  in
  let options : S.Embedded.options =
    { prompt_file = Support.Config_fixture.prompt_path fixture
    ; workspace = Support.Config_fixture.physical_workspace fixture
    ; tool_dir = directory
    ; home = Some directory
    ; storage = Transient
    ; start_immediately = false
    ; permission_profile = S.Embedded.default_permission_profile
    ; attachment_mode = Read_write
    ; event_capacity = 32
    }
  in
  Eio.Switch.run (fun sw ->
    let host = S.Embedded.open_local_host ~sw ~env ~local_policy options |> ok in
    Exn.protect
      ~f:(fun () ->
        let connection = S.Embedded.host_connection host in
        let trace = Client.Developer_trace_connection.create connection |> ok in
        let lookup =
          submit_fixture connection source ~key_text:"trace-embedded-fixture"
        in
        let _, result = page trace lookup in
        require_dependency_site (require_retained result))
      ~finally:(fun () -> S.Embedded.close_host host))
;;

let run env =
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 90. (fun () ->
    Support.Temporary_environment.with_
      ~scenario:"developer-trace-client"
      ~env
      (fun environment ->
         let fixture =
           Support.Config_fixture.create
             environment
             ~name:"developer-trace-client"
             ~http_port:(reserve_port env)
         in
         Support.Config_fixture.grant_public_all_scopes fixture;
         let source = Support.Trace_source_fixture.create ~env ~fixture in
         Support.Trace_source_fixture.install_sources source ~env ~fixture;
         run_remote env environment source fixture;
         run_embedded env source fixture));
  Eio.Flow.copy_string "typed trace client transports passed\n" (Eio.Stdenv.stdout env)
;;
