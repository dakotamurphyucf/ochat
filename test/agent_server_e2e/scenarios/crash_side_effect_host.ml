open Core
module F = Crash_recovery_fixture
module Res = Openai.Responses

let function_call marker =
  let item =
    Res.Response_stream.Item.Function_call
      { name = "append_to_file"
      ; arguments = ""
      ; call_id = "crash-unknown-call"
      ; _type = "function_call"
      ; id = Some "crash-unknown-item"
      ; status = Some "in_progress"
      }
  in
  [ Res.Response_stream.Output_item_added
      { item; output_index = 0; type_ = "response.output_item.added" }
  ; Res.Response_stream.Function_call_arguments_done
      { arguments =
          Jsonaf.to_string
            (`Object [ "path", `String marker; "content", `String "executed" ])
      ; item_id = "crash-unknown-item"
      ; output_index = 0
      ; type_ = "response.function_call_arguments.done"
      }
  ]
;;

let model_post_stream env marker ~sw:_ ~inputs:_ =
  Eio.Flow.copy_string "crash-provider-invoked\n" (Eio.Stdenv.stdout env);
  Stdlib.List.to_seq (function_call marker)
;;

let load_config env path =
  match Agent_server.Config_parser.load ~env ~path with
  | Error diagnostics ->
    raise_s [%sexp "crash host configuration failed", (diagnostics : _ list)]
  | Ok raw ->
    (match Agent_server.Config_validator.validate ~env raw with
     | Ok config -> config
     | Error diagnostics ->
       raise_s [%sexp "crash host validation failed", (diagnostics : _ list)])
;;

let listener ~sw env daemon config options =
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
    ~outgoing_capacity:1024
    ~max_connections:http.max_connections
    ~max_attachments:
      options.Agent_server.Daemon.protocol_limits.max_attachments_per_connection
    ~idle_connection_timeout:(Float.of_int http.idle_connection_timeout_ms /. 1000.)
    ~on_error:raise
;;

let run env ~config_path ~marker =
  let reached filename =
    Eio.Flow.copy_string
      ("crash-side-effect-written " ^ filename ^ "\n")
      (Eio.Stdenv.stdout env);
    Eio.Fiber.await_cancel ()
  in
  let wrapped =
    Support.Crash_fault_io.wrap
      env
      ~matches:(String.is_suffix ~suffix:(Filename.basename marker))
      ~boundary:(After_bytes (String.length "\nexecuted"))
      ~reached
  in
  let config = load_config wrapped config_path in
  let options =
    { Agent_server.Daemon.default_options with
      model_post_stream = Some (model_post_stream env marker)
    }
  in
  Eio.Switch.run (fun sw ->
    let daemon =
      Agent_server.Daemon.start
        ~sw
        ~env:wrapped
        ~config
        ~tool_dir:(Filename.dirname marker)
        ~home:(Sys.getenv_exn "HOME")
        ~process_start_identity:(Some "crash-side-effect-host")
        ~options
        ()
      |> F.protocol_ok
    in
    Eio.Fiber.fork_daemon ~sw (fun () ->
      listener ~sw wrapped daemon config options;
      `Stop_daemon);
    Eio.Flow.copy_string "crash-host-ready\n" (Eio.Stdenv.stdout env);
    Eio.Fiber.await_cancel ())
;;
