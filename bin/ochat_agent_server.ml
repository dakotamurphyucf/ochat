open! Core

exception Shutdown_requested

let write_line sink value = Eio.Flow.copy_string (value ^ "\n") sink

let diagnostic_to_string diagnostic =
  Sexp.to_string_hum ([%sexp_of: Agent_server.Config.Diagnostic.t] diagnostic)
;;

let report_diagnostics env diagnostics =
  List.iter diagnostics ~f:(fun value ->
    write_line (Eio.Stdenv.stderr env) (diagnostic_to_string value))
;;

let load_config env path =
  let open Result.Let_syntax in
  let%bind raw = Agent_server.Config_parser.load ~env ~path in
  Agent_server.Config_validator.validate ~env raw
;;

let all_scopes =
  Agent_protocol.Scope.Set.of_list
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
    ]
;;

let protocol_error_to_string error =
  Sexp.to_string_hum ([%sexp_of: Agent_protocol.Error.t] error)
;;

let report_protocol_error env error =
  write_line (Eio.Stdenv.stderr env) (protocol_error_to_string error)
;;

let report_store_error env error =
  write_line
    (Eio.Stdenv.stderr env)
    (Sexp.to_string_hum ([%sexp_of: Agent_store.Store_error.t] error))
;;

let report_exception env exn = write_line (Eio.Stdenv.stderr env) (Exn.to_string exn)

let prepare_socket env socket_path =
  Agent_transport_socket.Server.prepare_path ~env ~socket_path
;;

let install_signal_handlers shutdown_requested reload_requested =
  Signal.Expert.handle Signal.int (fun _ -> Atomic.set shutdown_requested true);
  Signal.Expert.handle Signal.term (fun _ -> Atomic.set shutdown_requested true);
  Signal.Expert.handle Signal.hup (fun _ -> Atomic.set reload_requested true)
;;

let reload daemon env =
  match Agent_server.Daemon.reload_config daemon with
  | Ok _ -> ()
  | Error diagnostics -> report_diagnostics env diagnostics
;;

let rec await_shutdown env daemon clock shutdown_requested reload_requested =
  if Atomic.get shutdown_requested
  then ()
  else (
    if Atomic.compare_and_set reload_requested true false then reload daemon env;
    Eio.Time.sleep clock 0.1;
    await_shutdown env daemon clock shutdown_requested reload_requested)
;;

let socket_listener env daemon config sw =
  Agent_transport_socket.Server.run
    ~sw
    ~net:(Eio.Stdenv.net env)
    ~socket_path:config.Agent_server.Config.Server.unix_socket
    ~backlog:128
    ~dispatcher:(Agent_server.Daemon.dispatcher daemon)
    ~close_connection:(Agent_server.Daemon.close_connection daemon)
    ~authenticate:(fun flow _ ->
      Agent_transport_socket.Peer_credentials.authenticate_same_user
        ~scopes:all_scopes
        flow)
    ~max_line_length:(16 * 1024 * 1024)
    ~outgoing_capacity:1_024
    ~max_attachments:
      Agent_server.Daemon.default_options.protocol_limits.max_attachments_per_connection
    ~on_error:(report_exception env)
    ~on_protocol_error:(report_protocol_error env)
;;

let http_address env http =
  let addresses =
    Eio.Net.getaddrinfo_stream
      ~service:(Int.to_string http.Agent_server.Config.Server.port)
      (Eio.Stdenv.net env)
      http.address
  in
  List.find addresses ~f:(function
    | `Tcp _ -> true
    | `Unix _ -> false)
  |> Option.value_exn
;;

let http_listener env daemon config sw =
  let http = config.Agent_server.Config.Server.http in
  Agent_transport_http.Server.run
    ~sw
    ~env
    ~address:(http_address env http)
    ~dispatcher:(Agent_server.Daemon.dispatcher daemon)
    ~registry:(Agent_server.Daemon.registry daemon)
    ~blob_store:(Agent_server.Daemon.blob_store daemon)
    ~health:(Agent_server.Daemon.health daemon)
    ~close_connection:(Agent_server.Daemon.close_connection daemon)
    ~authenticate:(Agent_server.Daemon.authenticate_http daemon)
    ~max_body_bytes:(16 * 1024 * 1024)
    ~max_batch_size:128
    ~batch_concurrency:16
    ~outgoing_capacity:1_024
    ~max_connections:http.max_connections
    ~max_attachments:
      Agent_server.Daemon.default_options.protocol_limits.max_attachments_per_connection
    ~idle_connection_timeout:
      (Time_ns.Span.of_ms (Float.of_int http.idle_connection_timeout_ms)
       |> Time_ns.Span.to_sec)
    ~on_error:(report_exception env)
;;

let run_listeners env daemon config shutdown_requested reload_requested =
  let socket () = Eio.Switch.run (socket_listener env daemon config) in
  let listeners =
    if config.Agent_server.Config.Server.http.enabled
    then [ socket; (fun () -> Eio.Switch.run (http_listener env daemon config)) ]
    else [ socket ]
  in
  Eio.Fiber.first
    (fun () ->
       await_shutdown
         env
         daemon
         (Eio.Stdenv.clock env)
         shutdown_requested
         reload_requested)
    (fun () -> Eio.Fiber.all listeners)
;;

let home_directory () = Sys.getenv "HOME" |> Option.value ~default:"/"

let process_identity () =
  Some (Agent_protocol.Id.Transaction.create () |> Agent_protocol.Id.Transaction.to_string)
;;

let migration_principal () =
  Agent_protocol.Principal.create
    ~id:(Agent_protocol.Id.Principal.create ())
    ~authentication_kind:"local.legacy_import"
    ~scopes:all_scopes
    ~attributes:[]
  |> function
  | Ok principal -> principal
  | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
;;

let legacy_import_request ~legacy_id ~prompt ~workspace =
  let open Result.Let_syntax in
  let%bind spec =
    Agent_protocol.Session.Spec.create
      ~execution_host:Daemon
      ~prompt:(Catalog (Agent_server.Catalog_identity.prompt_definition prompt))
      ~workspace:
        (Configured (Agent_server.Catalog_identity.workspace_definition workspace))
      ~liveness:Detached
      ~persistence:Durable
      ~start_immediately:false
      ~display_name:("Imported legacy session " ^ legacy_id)
      ~labels:[ "migration.source", "legacy"; "migration.legacy_id", legacy_id ]
      ()
  in
  let%map idempotency_key =
    Agent_protocol.Idempotency_key.of_string
      ("legacy-import:"
       ^ legacy_id
       ^ ":"
       ^ (Agent_protocol.Id.Transaction.create ()
          |> Agent_protocol.Id.Transaction.to_string))
  in
  Agent_protocol.Session.Create_request.
    { spec; requested_mode = None; subscribe = false; idempotency_key }
;;

let working_directory env =
  let native = Eio.Path.native_exn (Eio.Stdenv.cwd env) in
  if Filename.is_absolute native
  then native
  else
    Sys.getenv "PWD"
    |> Option.filter ~f:Filename.is_absolute
    |> Option.value_map ~default:native ~f:(fun pwd -> Filename.concat pwd native)
;;

let absolute_path env path =
  if Filename.is_absolute path then path else Filename.concat (working_directory env) path
;;

let print_migration_plan env plan =
  write_line
    (Eio.Stdenv.stdout env)
    (Sexp.to_string_hum ([%sexp_of: Agent_store.Migration.plan] plan))
;;

let inspect_store root =
  Eio_main.run (fun env ->
    match Agent_store.Migration.inspect ~env ~root ~mode:Validate_only with
    | Ok plan -> print_migration_plan env plan
    | Error error ->
      report_store_error env error;
      Core.exit 1)
;;

let migrate_store root ~dry_run =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    Eio.Switch.run (fun sw ->
      let mode =
        if dry_run then Agent_store.Migration.Dry_run else Agent_store.Migration.Apply
      in
      match
        Agent_store.Migration.run
          ~env
          ~sw
          ~root
          ~server_id:(Agent_protocol.Id.Server.create ())
          ~process_start_identity:(process_identity ())
          ~lock_nonce:
            (Agent_protocol.Id.Transaction.create ()
             |> Agent_protocol.Id.Transaction.to_string)
          ~mode
      with
      | Ok plan -> print_migration_plan env plan
      | Error error ->
        report_store_error env error;
        Core.exit 1))
;;

let import_legacy config_path ~legacy_id ~prompt ~workspace =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    match load_config env config_path with
    | Error diagnostics ->
      report_diagnostics env diagnostics;
      Core.exit 2
    | Ok config ->
      if
        String.is_empty legacy_id
        || (not (String.equal legacy_id (Filename.basename legacy_id)))
        || List.mem [ "."; ".." ] legacy_id ~equal:String.equal
      then (
        write_line (Eio.Stdenv.stderr env) "legacy session ID must be one path component";
        Core.exit 2)
      else (
        match Session_store.read_existing ~env ~id:legacy_id with
        | None ->
          write_line (Eio.Stdenv.stderr env) "legacy session is missing or unreadable";
          Core.exit 1
        | Some legacy ->
          Eio.Switch.run (fun sw ->
            let result =
              let open Result.Let_syntax in
              let%bind request = legacy_import_request ~legacy_id ~prompt ~workspace in
              let%bind daemon =
                Agent_server.Daemon.start
                  ~sw
                  ~env
                  ~config
                  ~tool_dir:(working_directory env)
                  ~home:(home_directory ())
                  ~process_start_identity:(process_identity ())
                  ()
              in
              Exn.protect
                ~f:(fun () ->
                  Agent_server.Daemon.import_legacy
                    daemon
                    ~principal:(migration_principal ())
                    ~source_id:legacy_id
                    ~source_path:(Session_store.rel_path legacy_id |> absolute_path env)
                    ~legacy
                    request)
                ~finally:(fun () ->
                  ignore
                    (Agent_server.Daemon.shutdown daemon
                     : (unit, Agent_protocol.Error.t) result))
            in
            match result with
            | Ok session ->
              write_line
                (Eio.Stdenv.stdout env)
                (Agent_protocol.Id.Session.to_string session.id)
            | Error error ->
              report_protocol_error env error;
              Core.exit 1)))
;;

let run_daemon env config =
  let open Result.Let_syntax in
  let%bind () = prepare_socket env config.Agent_server.Config.server.unix_socket in
  Eio.Switch.run (fun sw ->
    let tool_dir = working_directory env in
    let%bind daemon =
      Agent_server.Daemon.start
        ~sw
        ~env
        ~config
        ~tool_dir
        ~home:(home_directory ())
        ~process_start_identity:(process_identity ())
        ()
    in
    let shutdown_requested = Atomic.make false in
    let reload_requested = Atomic.make false in
    install_signal_handlers shutdown_requested reload_requested;
    Exn.protect
      ~f:(fun () ->
        run_listeners env daemon config.server shutdown_requested reload_requested;
        Ok ())
      ~finally:(fun () ->
        ignore
          (Agent_server.Daemon.shutdown daemon : (unit, Agent_protocol.Error.t) result)))
;;

let run_config config_path ~validate_only ~print_config =
  Eio_main.run (fun env ->
    match load_config env config_path with
    | Error diagnostics ->
      report_diagnostics env diagnostics;
      Core.exit 2
    | Ok config when print_config ->
      write_line
        (Eio.Stdenv.stdout env)
        (Sexp.to_string_hum ([%sexp_of: Agent_server.Config.t] config))
    | Ok _ when validate_only ->
      write_line (Eio.Stdenv.stdout env) "configuration is valid"
    | Ok config ->
      (match run_daemon env config with
       | Ok () -> ()
       | Error error ->
         report_protocol_error env error;
         Core.exit 1))
;;

let run
      ~config_path
      ~validate_only
      ~print_config
      ~inspect
      ~migrate
      ~dry_run
      ~legacy_id
      ~prompt
      ~workspace
  =
  match inspect, migrate, config_path, legacy_id, prompt, workspace with
  | Some _, Some _, _, _, _, _ ->
    failwith "--inspect-store and --migrate-store are mutually exclusive"
  | Some root, None, None, None, None, None
    when (not validate_only) && (not print_config) && not dry_run -> inspect_store root
  | None, Some root, None, None, None, None when (not validate_only) && not print_config
    -> migrate_store root ~dry_run
  | None, None, Some config_path, Some legacy_id, Some prompt, Some workspace
    when (not validate_only) && (not print_config) && not dry_run ->
    import_legacy config_path ~legacy_id ~prompt ~workspace
  | None, None, Some config_path, None, None, None when not dry_run ->
    run_config config_path ~validate_only ~print_config
  | None, None, None, None, None, None ->
    failwith "provide --config, --inspect-store, or --migrate-store"
  | _ -> failwith "configuration and store-maintenance flags cannot be combined"
;;

let command =
  Command.basic
    ~summary:"Run the Ochat durable agent daemon"
    (let open Command.Let_syntax in
     let%map_open config_path = flag "config" (optional string) ~doc:"FILE server config"
     and validate_only =
       flag "validate-only" no_arg ~doc:" validate configuration and exit"
     and print_config =
       flag "print-config" no_arg ~doc:" print normalized configuration and exit"
     and inspect = flag "inspect-store" (optional string) ~doc:"DIR inspect store schema"
     and migrate =
       flag "migrate-store" (optional string) ~doc:"DIR validate or migrate store"
     and dry_run = flag "dry-run" no_arg ~doc:" plan store migration without mutation"
     and legacy_id =
       flag "import-legacy" (optional string) ~doc:"ID import legacy session"
     and prompt = flag "prompt" (optional string) ~doc:"ID target configured prompt"
     and workspace =
       flag "workspace" (optional string) ~doc:"ID target configured workspace"
     in
     fun () ->
       run
         ~config_path
         ~validate_only
         ~print_config
         ~inspect
         ~migrate
         ~dry_run
         ~legacy_id
         ~prompt
         ~workspace)
;;

let () =
  Mirage_crypto_rng_unix.use_default ();
  Command_unix.run command
;;
