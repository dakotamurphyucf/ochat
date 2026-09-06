open Core

let protocol_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp "daemon host failure", (error : Agent_protocol.Error.t)]
;;

let load_config env fixture =
  let path = Config_fixture.config_path fixture in
  match Agent_server.Config_parser.load ~env ~path with
  | Error diagnostics -> raise_s [%sexp "config parse failed", (diagnostics : _ list)]
  | Ok raw ->
    (match Agent_server.Config_validator.validate ~env raw with
     | Ok config -> config
     | Error diagnostics ->
       raise_s [%sexp "config validation failed", (diagnostics : _ list)])
;;

let http_listener ~sw env daemon config options =
  let http = config.Agent_server.Config.server.http in
  Agent_transport_http.Server.run
    ~sw
    ~env
    ~address:(`Tcp (Eio.Net.Ipaddr.V4.loopback, http.port))
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
      options.Agent_server.Daemon.protocol_limits.max_attachments_per_connection
    ~idle_connection_timeout:
      (Time_ns.Span.of_ms (Float.of_int http.idle_connection_timeout_ms)
       |> Time_ns.Span.to_sec)
    ~on_error:raise
;;

let rec await_ready env fixture attempts =
  if attempts = 0
  then raise_s [%sexp "E2E daemon host did not become ready"]
  else (
    match
      Http_probe.get_health
        env
        ~port:(Config_fixture.http_port fixture)
        ~token:(Config_fixture.admin_token fixture)
    with
    | Ok response when response.ready -> ()
    | Ok _ | Error _ ->
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.02;
      await_ready env fixture (attempts - 1))
;;

let tool_dir env =
  let path = Eio.Path.native_exn (Eio.Stdenv.cwd env) in
  if Filename.is_absolute path then path else Eio_posix.Low_level.realpath path
;;

let home fixture =
  let roots = Temporary_environment.roots (Config_fixture.environment fixture) in
  roots.home
;;

exception Host_complete

let start_daemon ~sw env fixture config options =
  Agent_server.Daemon.start
    ~sw
    ~env
    ~config
    ~tool_dir:(tool_dir env)
    ~home:(home fixture)
    ~process_start_identity:
      (Some
         (Agent_protocol.Id.Transaction.create ()
          |> Agent_protocol.Id.Transaction.to_string))
    ~options
    ()
  |> protocol_ok
;;

let fork_listener ~sw env daemon config options =
  Eio.Fiber.fork_daemon ~sw (fun () ->
    Eio.Switch.run (fun listener_sw ->
      http_listener ~sw:listener_sw env daemon config options);
    `Stop_daemon)
;;

let run_host ~sw env fixture options f =
  let config = load_config env fixture in
  let daemon = start_daemon ~sw env fixture config options in
  Exn.protect
    ~f:(fun () ->
      fork_listener ~sw env daemon config options;
      await_ready env fixture 250;
      f sw daemon)
    ~finally:(fun () ->
      ignore (Agent_server.Daemon.shutdown daemon : (unit, Agent_protocol.Error.t) result))
;;

let with_ env fixture ~options f =
  let value = ref None in
  (try
     Eio.Switch.run (fun sw ->
       value := Some (run_host ~sw env fixture options f);
       Eio.Switch.fail sw Host_complete)
   with
   | Host_complete -> ());
  Option.value_exn !value
;;
