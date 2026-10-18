open! Core
module P = Agent_protocol
module S = Agent_server
module D = P.Developer_common
module W = P.Developer_wire

let fail message = raise_s [%sexp "developer transport E2E failed", (message : string)]
let require condition message = if not condition then fail message

let ok = function
  | Ok value -> value
  | Error error ->
    raise_s [%sexp "developer transport protocol error", (error : P.Error.t)]
;;

let transport_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp "developer transport failure", (error : string)]
;;

let reserve_port env =
  Eio.Switch.run (fun sw ->
    let reservation = Support.Port_reservation.create ~sw ~env in
    let port = Support.Port_reservation.port reservation in
    Support.Port_reservation.release reservation;
    port)
;;

let wire_limits () =
  let document ~bytes ~depth ~fields ~nodes =
    match
      Document_schema.Limits.create
        ~max_bytes:bytes
        ~max_depth:depth
        ~max_fields:fields
        ~max_nodes:nodes
    with
    | Ok value -> value
    | Error _ -> fail "invalid Developer wire fixture limits"
  in
  D.Limits.create
    ~request:(document ~bytes:(1024 * 1024) ~depth:16 ~fields:1024 ~nodes:2048)
    ~receipt:(document ~bytes:(1024 * 1024) ~depth:16 ~fields:1024 ~nodes:2048)
    ~response:(document ~bytes:(2 * 1024 * 1024) ~depth:24 ~fields:2048 ~nodes:4096)
    ~approval_page:(document ~bytes:(1024 * 1024) ~depth:16 ~fields:1024 ~nodes:2048)
    ~max_output_bytes:65536
    ~max_fixture_steps:4
    ~max_fixture_bytes:4096
    ~max_approval_display_bytes:4096
    ~max_approval_decision_reason_bytes:1024
    ~max_approval_page:8
    ~max_event_bytes:4096
  |> ok
;;

let configured_wire_limits () =
  let document ~bytes ~depth ~fields ~nodes =
    match
      Document_schema.Limits.create
        ~max_bytes:bytes
        ~max_depth:depth
        ~max_fields:fields
        ~max_nodes:nodes
    with
    | Ok value -> value
    | Error _ -> fail "invalid configured Developer wire fixture limits"
  in
  D.Limits.create
    ~request:(document ~bytes:(1024 * 1024) ~depth:24 ~fields:2048 ~nodes:4096)
    ~receipt:(document ~bytes:65536 ~depth:16 ~fields:1024 ~nodes:2048)
    ~response:(document ~bytes:69632 ~depth:24 ~fields:1056 ~nodes:2080)
    ~approval_page:(document ~bytes:14080 ~depth:8 ~fields:32 ~nodes:48)
    ~max_output_bytes:65536
    ~max_fixture_steps:64
    ~max_fixture_bytes:65536
    ~max_approval_display_bytes:256
    ~max_approval_decision_reason_bytes:256
    ~max_approval_page:1
    ~max_event_bytes:4096
  |> ok
;;

type raw_client =
  { send : P.Envelope.t -> P.Envelope.t
  ; close : unit -> unit
  ; mutable next_id : int
  }

let call_outcome client ~method_ ~params =
  let id = P.Envelope.Request_id.of_json (`Number (Int.to_string client.next_id)) |> ok in
  client.next_id <- client.next_id + 1;
  let request = P.Envelope.request ~id ~method_ ~params () in
  match client.send request with
  | P.Envelope.Response { id = received; outcome } ->
    require (P.Envelope.Request_id.compare id received = 0) "request ID changed";
    outcome
  | P.Envelope.Request _ | P.Envelope.Notification _ ->
    fail "request did not receive a response"
;;

let call client ~method_ ~params = call_outcome client ~method_ ~params |> ok

let call_authoring client request =
  let method_ = P.Authoring_wire.Request.method_name request in
  let result = call client ~method_ ~params:(P.Authoring_wire.Request.params request) in
  P.Authoring_wire.Response.of_method_and_json ~method_ result |> ok
;;

let call_developer client ~limits request =
  let method_ = W.Request.method_name request in
  let result = call client ~method_ ~params:(W.Request.params request) in
  W.Response.of_method_and_json ~limits ~method_ result |> ok
;;

let initialize_response client =
  let implementation =
    P.Initialize.Implementation.create ~name:"developer-wire-e2e" ~version:"1" |> ok
  in
  let request =
    P.Initialize.Request.create
      ~implementation
      ~protocol_min:P.Version.current
      ~protocol_max:P.Version.current
      ~features:
        [ P.Authoring.Features.core; P.Authoring.Features.semantic; D.Features.run ]
      ~event_encodings:[ Json ]
      ~max_inbound_event_bytes:(2 * 1024 * 1024)
      ()
    |> ok
  in
  call
    client
    ~method_:"protocol.initialize"
    ~params:(P.Initialize.Request.to_json request)
  |> P.Initialize.Response.of_json
  |> ok
;;

let initialize client =
  let response = initialize_response client in
  require
    (List.mem response.enabled_features D.Features.run ~equal:String.equal)
    "Developer run feature was not selected"
;;

let qualify_unconfigured_client client =
  Exn.protect
    ~f:(fun () ->
      let response = initialize_response client in
      require
        (not (List.mem response.enabled_features D.Features.run ~equal:String.equal))
        "unconfigured daemon selected Developer run";
      match
        call_outcome client ~method_:"developer.capabilities" ~params:(`Object [])
      with
      | Error { code = P.Error.Method_not_found; _ } -> ()
      | Error error ->
        raise_s
          [%sexp
            "unconfigured Developer method returned another error", (error : P.Error.t)]
      | Ok _ -> fail "unconfigured daemon exposed Developer capabilities")
    ~finally:client.close
;;

let open_and_analyze client =
  let source = {|let main input = Task.pure(input)|} in
  let opened =
    P.Authoring.Context.Open_request.create_supplied_document
      ~member:"draft.chatmd"
      ~default_environment:None
      ~contents:
        ("<script id=\"s\" language=\"chatml\" kind=\"tool\">" ^ source ^ "</script>")
    |> ok
  in
  let expected =
    match call_authoring client (P.Authoring_wire.Request.Context_open opened) with
    | P.Authoring_wire.Response.Context_open context ->
      P.Authoring.Context.Expected.of_view context
    | _ -> fail "Authoring Context_open returned another method"
  in
  let selector = P.Authoring.Analysis.Target.selected_script "s" |> ok in
  let analyze =
    P.Authoring.Analysis.Analyze.Request.create ~context:expected ~target:selector |> ok
  in
  (match call_authoring client (P.Authoring_wire.Request.Analyze analyze) with
   | P.Authoring_wire.Response.Analyze _ -> ()
   | _ -> fail "Authoring Analyze returned another method");
  expected, selector
;;

let qualify_client ?expected_wire_limits ?(after_check = fun _ _ -> ()) client =
  Exn.protect
    ~f:(fun () ->
      initialize client;
      let caps =
        match call_developer client ~limits:(wire_limits ()) W.Request.Capabilities with
        | W.Response.Capabilities caps -> caps
        | _ -> fail "Developer capabilities returned another method"
      in
      require (D.Capabilities.supports caps D.Method.Submit) "Submit is not installed";
      require (D.Capabilities.supports caps D.Method.Observe) "Observe is not installed";
      require
        (D.Capabilities.supports caps D.Method.Await_terminal)
        "Await_terminal is not installed";
      let limits = D.Capabilities.limits caps in
      let expected_wire_limits =
        Option.value expected_wire_limits ~default:(wire_limits ())
      in
      require
        (String.equal
           (D.Limits.to_json limits |> Jsonaf.to_string)
           (D.Limits.to_json expected_wire_limits |> Jsonaf.to_string))
        "advertised Developer limits differ from installed policy";
      let expected, selector = open_and_analyze client in
      let route =
        D.Route.document_script ~limits:(D.Limits.request limits) ~expected ~selector
        |> ok
      in
      let run_id = P.Id.Developer_run.create () in
      let key = P.Idempotency_key.of_string "developer-wire-check" |> ok in
      let input = `Object [ "transport", `String "raw" ] in
      let submit =
        D.Submit.create
          ~limits
          ~run_id
          ~key
          ~kind:P.Developer_receipt.Kind.Check
          ~route
          ~target:D.Target.One_off_v1
          ~prompt:None
          ~workspace:None
          ~entrypoint:"main"
          ~input
          ~fixtures:None
          ~requested_execution:None
          ~requested_max_output_bytes:None
        |> ok
      in
      let submitted =
        match call_developer client ~limits (W.Request.Submit submit) with
        | W.Response.Submit view -> view
        | _ -> fail "Developer Submit returned another method"
      in
      let lookup = D.Lookup.create ~run_id ~key in
      let terminal =
        match call_developer client ~limits (W.Request.Await_terminal lookup) with
        | W.Response.Await_terminal view -> view
        | _ -> fail "Developer Await_terminal returned another method"
      in
      let observed =
        match call_developer client ~limits (W.Request.Observe lookup) with
        | W.Response.Observe (Some view) -> view
        | W.Response.Observe None -> fail "submitted Developer Check was not observable"
        | _ -> fail "Developer Observe returned another method"
      in
      let retried =
        match call_developer client ~limits (W.Request.Submit submit) with
        | W.Response.Submit view -> view
        | _ -> fail "same-key Developer retry returned another method"
      in
      require
        (P.Developer_command_id.equal
           (D.View.command_id retried)
           (D.View.command_id observed))
        "same-key retry created another command";
      require
        (Chatml_source.Digest.equal
           (D.View.capture_fingerprint retried)
           (D.View.capture_fingerprint observed))
        "same-key retry changed the captured source";
      require
        (P.Developer_command_id.equal
           (D.View.command_id submitted)
           (D.View.command_id observed))
        "Submit and Observe identified different commands";
      require
        (P.Developer_command_id.equal
           (D.View.command_id terminal)
           (D.View.command_id observed))
        "Await and Observe identified different commands";
      require
        (P.Id.Developer_run.equal (D.View.owner observed).run_id run_id)
        "observed Developer owner belongs to another run";
      require
        (P.Idempotency_key.equal (D.View.key observed) key)
        "observed Developer key changed";
      require
        (Chatml_source.Digest.equal
           (D.View.request_fingerprint submitted)
           (D.View.request_fingerprint observed))
        "Submit and Observe request fingerprints differ";
      require
        (Chatml_source.Digest.equal
           (D.View.capture_fingerprint submitted)
           (D.View.capture_fingerprint observed))
        "Submit and Observe capture fingerprints differ";
      (match D.View.phase observed with
       | D.View.Terminal { execution_started = false; receipt } ->
         (match P.Developer_receipt.value receipt with
          | P.Developer_receipt.Check P.Developer_receipt.Check.Ready -> ()
          | _ -> fail "Developer Check did not produce Ready")
       | _ -> fail "Developer Check was not terminal");
      after_check run_id key)
    ~finally:client.close
;;

let qualify_unselected_principal client =
  Exn.protect
    ~f:(fun () ->
      initialize client;
      let limits =
        match
          call_developer client ~limits:(configured_wire_limits ()) W.Request.Capabilities
        with
        | W.Response.Capabilities caps -> D.Capabilities.limits caps
        | _ -> fail "unselected principal could not read installed capabilities"
      in
      let expected, selector = open_and_analyze client in
      let route =
        D.Route.document_script ~limits:(D.Limits.request limits) ~expected ~selector
        |> ok
      in
      let submit =
        D.Submit.create
          ~limits
          ~run_id:(P.Id.Developer_run.create ())
          ~key:(P.Idempotency_key.of_string "unselected-principal" |> ok)
          ~kind:P.Developer_receipt.Kind.Check
          ~route
          ~target:D.Target.One_off_v1
          ~prompt:None
          ~workspace:None
          ~entrypoint:"main"
          ~input:`Null
          ~fixtures:None
          ~requested_execution:None
          ~requested_max_output_bytes:None
        |> ok
      in
      match
        call_outcome
          client
          ~method_:(W.Request.method_name (W.Request.Submit submit))
          ~params:(W.Request.params (W.Request.Submit submit))
      with
      | Error { code = P.Error.Permission_denied; _ } -> ()
      | Error error ->
        raise_s [%sexp "unselected principal had another denial", (error : P.Error.t)]
      | Ok _ -> fail "unselected principal admitted a Developer command")
    ~finally:client.close
;;

let qualify_repl_capacity daemon fixture =
  let module Repl = S.Daemon.Developer.Repl in
  let principal =
    P.Principal.create
      ~id:(Support.Config_fixture.public_principal fixture)
      ~authentication_kind:"http.static_bearer"
      ~scopes:Support.Config_fixture.all_scopes
      ~attributes:[]
    |> ok
  in
  let actor = Operator_authorization.trusted_local principal in
  let connection =
    S.Connection_context.create_authenticated
      ~actor
      ~connection_id:P.Id.Transaction.(create () |> to_string)
      ~transport:In_memory
      ~publish_notification:(fun _ -> ())
      ~max_attachments:8
  in
  Exn.protect
    ~f:(fun () ->
      let dispatch command =
        S.Dispatcher.dispatch_command
          (S.Daemon.dispatcher daemon)
          ~context:connection
          command
        |> ok
      in
      let implementation =
        P.Initialize.Implementation.create ~name:"configured-repl" ~version:"1" |> ok
      in
      let initialize =
        P.Initialize.Request.create
          ~implementation
          ~protocol_min:P.Version.current
          ~protocol_max:P.Version.current
          ~features:
            [ P.Authoring.Features.core; P.Authoring.Features.semantic; D.Features.run ]
          ~event_encodings:[ Json ]
          ~max_inbound_event_bytes:(2 * 1024 * 1024)
          ()
        |> ok
      in
      (match dispatch (P.Command.Protocol_initialize initialize) with
       | P.Public.Result.Non_history result ->
         (match P.Public.Result.Non_history.value result with
          | P.Method_result.Protocol_initialize _ -> ()
          | _ -> fail "in-process REPL connection did not initialize")
       | _ -> fail "in-process REPL connection did not initialize");
      let opened =
        P.Authoring.Context.Open_request.create_supplied_document
          ~member:"repl.chatmd"
          ~default_environment:None
          ~contents:
            "<script id=\"s\" language=\"chatml\" kind=\"tool\">let main input = \
             Task.pure(input)</script>"
        |> ok
      in
      let expected =
        match
          dispatch (P.Command.Authoring (P.Authoring_wire.Request.Context_open opened))
        with
        | P.Public.Result.Non_history result ->
          (match P.Public.Result.Non_history.value result with
           | P.Method_result.Authoring (P.Authoring_wire.Response.Context_open context) ->
             P.Authoring.Context.Expected.of_view context
           | _ -> fail "in-process REPL source did not open")
        | _ -> fail "in-process REPL source did not open"
      in
      let selector = P.Authoring.Analysis.Target.selected_script "s" |> ok in
      let analyze =
        P.Authoring.Analysis.Analyze.Request.create ~context:expected ~target:selector
        |> ok
      in
      (match
         dispatch (P.Command.Authoring (P.Authoring_wire.Request.Analyze analyze))
       with
       | P.Public.Result.Non_history result ->
         (match P.Public.Result.Non_history.value result with
          | P.Method_result.Authoring (P.Authoring_wire.Response.Analyze _) -> ()
          | _ -> fail "in-process REPL source analysis did not complete")
       | _ -> fail "in-process REPL source analysis did not complete");
      let route =
        S.Daemon.Developer.Request.Route.Document_script { expected; selector }
      in
      let original =
        Repl.Original.create
          ~route
          ~target:Chatml.Chatml_compiler_contract.Target.One_off_v1
          ~prompt:(S.Catalog_identity.prompt_definition "smoke")
          ~workspace:(S.Catalog_identity.workspace_definition "physical")
          ~requested_execution:None
          ~requested_max_output_bytes:None
          ~checkpoint:ignore
        |> ok
      in
      let view =
        Repl.create
          daemon
          ~actor
          ~run_id:(P.Id.Developer_run.create ())
          ~key:(P.Idempotency_key.of_string "configured-repl-create" |> ok)
          ~original
          ~fresh_source:
            (Some (S.Daemon.Developer.Request.Fresh_source.Document_script { connection }))
        |> ok
      in
      let rec submit_many revision index =
        if index = 10
        then ()
        else (
          let key =
            P.Idempotency_key.of_string ("configured-repl-" ^ Int.to_string index) |> ok
          in
          let result =
            Repl.submit
              daemon
              ~actor
              ~instance:(Repl.View.instance view)
              ~epoch:(Repl.View.epoch view)
              ~expected_revision:revision
              ~key
              ~source:(sprintf "let value_%d = %d" index index)
            |> ok
          in
          (match Repl.Submission.status result with
           | Finished -> ()
           | Need_more _ | Rejected _ | Stopped _ ->
             fail "configured REPL did not retain a submitted statement");
          submit_many (Repl.Submission.revision result) (index + 1))
      in
      submit_many (Repl.View.revision view) 0)
    ~finally:(fun () -> S.Daemon.close_connection daemon connection)
;;

let qualify_own_control_without_approval_scope daemon fixture run_id key =
  let principal =
    P.Principal.create
      ~id:(Support.Config_fixture.public_principal fixture)
      ~authentication_kind:"http.static_bearer"
      ~scopes:(P.Scope.Set.of_list [ P.Scope.Developer_check ])
      ~attributes:[]
    |> ok
  in
  let actor = Operator_authorization.trusted_local principal in
  let view = S.Daemon.Developer.cancel_and_wait daemon ~actor ~run_id ~key |> ok in
  require
    (P.Id.Developer_run.equal (S.Daemon.Developer.View.owner view).run_id run_id)
    "own Check control returned a foreign run"
;;

let http_client ?token ~sw env fixture =
  let token = Option.value token ~default:(Support.Config_fixture.public_token fixture) in
  let driver =
    Support.Http_driver.create
      ~sw
      ~env
      ~port:(Support.Config_fixture.http_port fixture)
      ~token:(Some token)
    |> transport_ok
  in
  { next_id = 1
  ; send =
      (fun request ->
        let response =
          Support.Http_driver.rpc_raw
            driver
            (P.Envelope.to_json request |> Jsonaf.to_string)
          |> transport_ok
        in
        require (Int.equal response.status 200) "HTTP Developer RPC failed";
        Jsonaf.parse response.body |> Or_error.ok_exn |> P.Envelope.of_json |> ok)
  ; close = (fun () -> Support.Http_driver.shutdown driver)
  }
;;

let rec read_unix_response raw clock remaining =
  if remaining = 0 then fail "too many Unix notifications before response";
  match Support.Unix_driver.read_envelope raw ~clock ~timeout_seconds:5. with
  | Support.Unix_driver.Envelope (P.Envelope.Notification _) ->
    read_unix_response raw clock (remaining - 1)
  | Support.Unix_driver.Envelope response -> response
  | Support.Unix_driver.End_of_file
  | Support.Unix_driver.Timeout
  | Support.Unix_driver.Invalid_response _ -> fail "Unix Developer response unavailable"
;;

let unix_client ~sw env fixture =
  let raw =
    Support.Unix_driver.connect_raw
      ~sw
      ~env
      ~socket_path:(Support.Config_fixture.unix_socket fixture)
      ~max_response_bytes:(2 * 1024 * 1024)
  in
  { next_id = 1
  ; send =
      (fun request ->
        Support.Unix_driver.send_line raw (P.Envelope.to_json request |> Jsonaf.to_string);
        read_unix_response raw (Eio.Stdenv.clock env) 4)
  ; close = (fun () -> Support.Unix_driver.close_raw raw)
  }
;;

let absolute env path =
  if Filename.is_absolute path
  then path
  else Filename.concat (Eio.Path.native_exn (Eio.Stdenv.cwd env)) path
;;

let stdio_executable env =
  let default =
    Filename.concat
      (Eio.Path.native_exn (Eio.Stdenv.cwd env))
      "_build/default/bin/ochat_agent_stdio.exe"
  in
  let path = Sys.getenv "OCHAT_E2E_STDIO_EXE" |> Option.value ~default |> absolute env in
  require
    (Eio.Path.is_file Eio.Path.(Eio.Stdenv.fs env / path))
    "stdio relay is unavailable";
  path
;;

let rec read_stdio_response client remaining =
  if remaining = 0 then fail "too many stdio notifications before response";
  match Support.Stdio_client.next_envelope client ~timeout_seconds:5. |> ok with
  | P.Envelope.Notification _ -> read_stdio_response client (remaining - 1)
  | response -> response
;;

let stdio_client ~sw env fixture =
  let environment =
    Support.Temporary_environment.child_environment
      (Support.Config_fixture.environment fixture)
      ~base:(Core_unix.environment ())
  in
  let process =
    Support.Stdio_process.spawn
      ~sw
      ~env
      ~environment
      ~max_output_bytes:(2 * 1024 * 1024)
      [ stdio_executable env
      ; "--connect"
      ; "unix://" ^ Support.Config_fixture.unix_socket fixture
      ]
  in
  let client = Support.Stdio_client.create ~process ~clock:(Eio.Stdenv.clock env) in
  { next_id = 1
  ; send =
      (fun request ->
        Support.Stdio_client.send_raw_line
          client
          (P.Envelope.to_json request |> Jsonaf.to_string)
        |> ok;
        read_stdio_response client 4)
  ; close =
      (fun () ->
        Support.Stdio_process.close_stdin process;
        match
          Eio.Time.with_timeout (Eio.Stdenv.clock env) 3. (fun () ->
            Ok (Support.Stdio_process.await process))
        with
        | Ok _ -> ()
        | Error `Timeout ->
          ignore
            (Support.Stdio_process.terminate
               process
               ~clock:(Eio.Stdenv.clock env)
               ~grace_seconds:1.
             : Support.Process_manager.termination))
  }
;;

let start_socket_listener ~sw env fixture daemon =
  let path = Support.Config_fixture.unix_socket fixture in
  Agent_transport_socket.Server.prepare_path ~env ~socket_path:path |> ok;
  Eio.Fiber.fork_daemon ~sw (fun () ->
    Eio.Switch.run (fun listener_sw ->
      Agent_transport_socket.Server.run
        ~sw:listener_sw
        ~net:(Eio.Stdenv.net env)
        ~socket_path:path
        ~backlog:16
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
          raise_s [%sexp "Developer socket protocol error", (error : P.Error.t)]));
    `Stop_daemon);
  let rec await_socket attempts =
    if attempts = 0 then fail "Developer Unix listener did not become ready";
    match Eio.Path.kind ~follow:false Eio.Path.(Eio.Stdenv.fs env / path) with
    | `Socket -> ()
    | _ ->
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.02;
      await_socket (attempts - 1)
  in
  await_socket 250
;;

let configured_developer fixture =
  let http_principal =
    Support.Config_fixture.public_principal fixture |> P.Id.Principal.to_string
  in
  let unix_principal =
    Digestif.SHA256.(
      digest_string
        ("unix-peer:v1:"
         ^ Int.to_string Agent_transport_socket.Peer_credentials.(effective_uid ()))
      |> to_hex)
    |> fun digest -> "pri_" ^ digest
  in
  sprintf
    {|((selections
       (((source (Document_source (origin Supplied_document) (script_id s)))
         (target One_off_v1)
         (principals
          (((principal_id %s) (authentication_kind http.static_bearer))
           ((principal_id %s) (authentication_kind unix.peer)))))))
      (capacity
       ((max_runs 128)
        (max_active_commands 4)
        (max_active_agent_calls 8)
        (max_agent_sources 6)
        (max_native_bindings 32)
        (max_retained_tasks 8)
        (max_watches_per_connection 4)
        (max_pending_approvals 4)
        (max_repl_instances 8)
        (max_repl_keys_per_instance 128)
        (max_repl_submissions_per_epoch 64)
        (max_repl_source_bytes 65536)))
      (model Disabled)
      (approvals Disabled)
      (repl Standard_v1)
      (portable true))|}
    http_principal
    unix_principal
;;

let install_configured_developer env fixture =
  let original = Support.Config_fixture.configuration fixture () in
  let anchor = "(unsafe_allow_unauthenticated_remote_http false)))" in
  require (String.is_substring original ~substring:anchor) "missing server config anchor";
  let configured =
    String.substr_replace_first
      original
      ~pattern:anchor
      ~with_:("(developer " ^ configured_developer fixture ^ ")\n  " ^ anchor)
  in
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    Eio.Path.(Eio.Stdenv.fs env / Support.Config_fixture.config_path fixture)
    configured
;;

let reject_uninstalled_source_and_reopen env environment fixture options f =
  let raw =
    match
      S.Config_parser.load ~env ~path:(Support.Config_fixture.config_path fixture)
    with
    | Ok raw -> raw
    | Error _ -> fail "valid configured fixture did not parse"
  in
  let config =
    match S.Config_validator.validate ~env raw with
    | Ok config -> config
    | Error _ -> fail "valid configured fixture did not validate"
  in
  let developer = Option.value_exn config.server.developer in
  let selected = List.hd_exn developer.selections in
  let selected =
    { selected with
      source =
        S.Developer_startup_configuration.Source.Document_source
          { origin =
              S.Developer_startup_configuration.Source.Host_document
                { root = "uninstalled-root"; member = "draft.chatmd" }
          ; script_id = "s"
          }
    }
  in
  let invalid =
    { config with
      server =
        { config.server with
          developer = Some { developer with selections = [ selected ] }
        }
    }
  in
  let tool_dir = Eio.Path.native_exn (Eio.Stdenv.cwd env) in
  let home = (Support.Temporary_environment.roots environment).home in
  Eio.Switch.run (fun sw ->
    match
      S.Daemon.start
        ~sw
        ~env
        ~config:invalid
        ~tool_dir
        ~home
        ~process_start_identity:None
        ~options
        ()
    with
    | Error { code = P.Error.Invalid_request; _ } -> ()
    | Error error ->
      raise_s [%sexp "invalid startup had another error", (error : P.Error.t)]
    | Ok daemon ->
      ignore (S.Daemon.shutdown daemon : (unit, P.Error.t) result);
      fail "uninstalled Developer source was accepted");
  Support.Daemon_host.with_ env fixture ~options f
;;

let run_configured env =
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 45. (fun () ->
    Support.Temporary_environment.with_
      ~scenario:"developer-disabled-startup"
      ~env
      (fun environment ->
         let fixture =
           Support.Config_fixture.create
             environment
             ~name:"developer-disabled-startup"
             ~http_port:(reserve_port env)
         in
         Support.Config_fixture.grant_public_all_scopes fixture;
         let options =
           Support.Daemon_host.with_offline_inference S.Daemon.default_options
         in
         Support.Daemon_host.with_ env fixture ~options (fun sw _daemon ->
           qualify_unconfigured_client (http_client ~sw env fixture)));
    Support.Temporary_environment.with_
      ~scenario:"developer-configured-startup"
      ~env
      (fun environment ->
         let fixture =
           Support.Config_fixture.create
             environment
             ~name:"developer-configured-startup"
             ~http_port:(reserve_port env)
         in
         Support.Config_fixture.grant_public_all_scopes fixture;
         install_configured_developer env fixture;
         let options =
           let options =
             Support.Daemon_host.with_offline_inference S.Daemon.default_options
           in
           { options with
             authoring = Some (Support.Developer_fixture.semantic_authoring_policy env)
           ; inference_policy =
               { options.inference_policy with
                 capture_developer_default_target =
                   (fun () ->
                     failwith "disabled Developer model attempted default target capture")
               }
           }
         in
         reject_uninstalled_source_and_reopen
           env
           environment
           fixture
           options
           (fun sw daemon ->
              qualify_repl_capacity daemon fixture;
              start_socket_listener ~sw env fixture daemon;
              qualify_client
                ~expected_wire_limits:(configured_wire_limits ())
                ~after_check:(qualify_own_control_without_approval_scope daemon fixture)
                (http_client ~sw env fixture);
              qualify_unselected_principal
                (http_client
                   ~token:(Support.Config_fixture.admin_token fixture)
                   ~sw
                   env
                   fixture);
              qualify_client
                ~expected_wire_limits:(configured_wire_limits ())
                (unix_client ~sw env fixture))));
  Eio.Flow.copy_string
    "configured Developer HTTP and Unix passed\n"
    (Eio.Stdenv.stdout env)
;;

let run_embedded env =
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 45. (fun () ->
    Support.Temporary_environment.with_
      ~scenario:"developer-wire"
      ~env
      (fun environment ->
         let fixture =
           Support.Config_fixture.create
             environment
             ~name:"developer-wire"
             ~http_port:(reserve_port env)
         in
         Support.Config_fixture.grant_public_all_scopes fixture;
         let developer = Support.Developer_fixture.developer_configuration () in
         let portable =
           S.Daemon.Developer_configuration.Portable.create
             ~wire_limits:(wire_limits ())
             ~max_connections:4
             ~max_watches_per_connection:4
             ~max_pending_hints_per_connection:8
           |> ok
         in
         let options =
           { (Support.Daemon_host.with_offline_inference S.Daemon.default_options) with
             authoring = Some (Support.Developer_fixture.semantic_authoring_policy env)
           ; developer =
               Some
                 (S.Daemon.Developer_configuration.with_portable_wire developer portable)
           }
         in
         Support.Daemon_host.with_ env fixture ~options (fun sw daemon ->
           start_socket_listener ~sw env fixture daemon;
           qualify_client (http_client ~sw env fixture);
           qualify_client (unix_client ~sw env fixture);
           qualify_client (stdio_client ~sw env fixture))));
  Eio.Flow.copy_string "developer wire transports passed\n" (Eio.Stdenv.stdout env)
;;

let run env ~case =
  match case with
  | None -> run_embedded env
  | Some "configured" -> run_configured env
  | Some case -> raise_s [%sexp "unknown Developer wire E2E case", (case : string)]
;;
