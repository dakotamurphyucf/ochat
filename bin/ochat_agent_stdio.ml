open! Core

let absolute cwd path =
  if Filename.is_absolute path then path else Filename.concat cwd path
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

let home () = Sys.getenv "HOME" |> Option.value ~default:"/"

let report_protocol_error env error =
  Sexp.to_string_hum ([%sexp_of: Agent_protocol.Error.t] error)
  |> fun line -> Eio.Flow.copy_string (line ^ "\n") (Eio.Stdenv.stderr env)
;;

let protocol_error error = Error.create_s [%sexp (error : Agent_protocol.Error.t)]

let load_bearer_token env = function
  | None -> Ok None
  | Some path ->
    Agent_transport_client.Endpoint.load_bearer_token ~env ~path
    |> Result.map ~f:Option.some
;;

let endpoint env ~uri ~bearer_token_file =
  let open Result.Let_syntax in
  let%bind bearer_token = load_bearer_token env bearer_token_file in
  Agent_transport_client.Endpoint.create ~home:(Sys.getenv "HOME") ~bearer_token uri
;;

let run_gateway env ~uri ~bearer_token_file =
  let open Or_error.Let_syntax in
  Eio.Switch.run (fun sw ->
    let%bind endpoint =
      endpoint env ~uri ~bearer_token_file |> Result.map_error ~f:protocol_error
    in
    let%map connection =
      Agent_transport_client.Endpoint.connect
        endpoint
        ~sw
        ~env
        ~notification_capacity:1_024
      |> Result.map_error ~f:protocol_error
    in
    Agent_transport_stdio.Gateway.run
      ~sw
      ~connection
      ~input:(Eio.Stdenv.stdin env)
      ~output:(Eio.Stdenv.stdout env)
      ~max_line_length:(16 * 1024 * 1024)
      ~outgoing_capacity:1_024
      ~on_error:(report_protocol_error env))
;;

let local_options env ~prompt ~workspace ~data_root =
  let cwd = working_directory env in
  Agent_server.Embedded.
    { prompt_file = absolute cwd prompt
    ; workspace = Option.value_map workspace ~default:cwd ~f:(absolute cwd)
    ; tool_dir = cwd
    ; home = home ()
    ; data_root = Option.map data_root ~f:(absolute cwd)
    ; start_immediately = true
    ; permission_profile = default_permission_profile
    ; attachment_mode = Read_write
    ; event_capacity = 1_024
    }
;;

let run_local env ~prompt ~workspace ~data_root ~authoring_package_files ~authoring_budget
  =
  let open Or_error.Let_syntax in
  Eio.Switch.run (fun sw ->
    let options = local_options env ~prompt ~workspace ~data_root in
    let authoring_package_files =
      List.map authoring_package_files ~f:(absolute (working_directory env))
    in
    let%map embedded =
      Agent_server.Embedded.start
        ~sw
        ~env
        ~authoring_package_files
        ?authoring_budget
        options
      |> Result.map_error ~f:protocol_error
    in
    Exn.protect
      ~f:(fun () ->
        Agent_transport_stdio.Server.run
          ~sw
          ~dispatcher:(Agent_server.Embedded.dispatcher embedded)
          ~close_connection:(Agent_server.Embedded.close_connection embedded)
          ~principal:(Agent_server.Embedded.principal embedded)
          ~connection_id:
            (Agent_protocol.Id.Attachment.create ()
             |> Agent_protocol.Id.Attachment.to_string)
          ~input:(Eio.Stdenv.stdin env)
          ~output:(Eio.Stdenv.stdout env)
          ~max_line_length:(16 * 1024 * 1024)
          ~outgoing_capacity:1_024
          ~max_attachments:64
          ~on_error:(report_protocol_error env))
      ~finally:(fun () -> Agent_server.Embedded.close embedded))
;;

let run
      ~local
      ~connect
      ~bearer_token_file
      ~prompt
      ~workspace
      ~data_root
      ~authoring_package_files
      ~authoring_options
  =
  match local, connect with
  | true, None when Option.is_some bearer_token_file ->
    Or_error.error_string "--bearer-token-file is only valid with --connect"
  | true, None ->
    (match prompt with
     | None -> Or_error.error_string "--local requires --prompt FILE"
     | Some prompt ->
       let open Or_error.Let_syntax in
       let%bind authoring_budget =
         Agent_server.Authoring_options.resolve authoring_options
       in
       Eio_main.run (fun env ->
         run_local
           env
           ~prompt
           ~workspace
           ~data_root
           ~authoring_package_files
           ~authoring_budget))
  | false, Some uri ->
    if
      Option.is_some prompt
      || Option.is_some workspace
      || Option.is_some data_root
      || (not (List.is_empty authoring_package_files))
      || Agent_server.Authoring_options.is_configured authoring_options
    then
      Or_error.error_string
        "--prompt, --workspace, --data-root, authoring package and budget flags are \
         local-mode options"
    else Eio_main.run (fun env -> run_gateway env ~uri ~bearer_token_file)
  | true, Some _ -> Or_error.error_string "--local and --connect are mutually exclusive"
  | false, None when Option.is_some bearer_token_file ->
    Or_error.error_string "--bearer-token-file requires --connect URI"
  | false, None -> Or_error.error_string "select --local or --connect URI"
;;

let command =
  Command.basic_or_error
    ~summary:"Run a standalone Ochat stdio host or bridge stdio to a daemon"
    (let open Command.Let_syntax in
     let%map_open local = flag "--local" no_arg ~doc:"Host a process-bound local agent."
     and connect = flag "--connect" (optional string) ~doc:"URI Connect to a daemon."
     and bearer_token_file =
       flag
         "--bearer-token-file"
         (optional string)
         ~doc:"FILE Read an HTTP daemon bearer token through Eio."
     and prompt = flag "--prompt" (optional string) ~doc:"FILE Local ChatMD prompt."
     and workspace = flag "--workspace" (optional string) ~doc:"DIR Local workspace."
     and data_root =
       flag "--data-root" (optional string) ~doc:"DIR Durable local data root."
     and authoring_options = Agent_server.Authoring_options.param
     and authoring_package_files =
       flag
         "--authoring-package"
         (listed string)
         ~doc:"FILE Capture custom documentation for the local host (repeatable)."
     in
     fun () ->
       run
         ~local
         ~connect
         ~bearer_token_file
         ~prompt
         ~workspace
         ~data_root
         ~authoring_package_files
         ~authoring_options)
;;

let () = Command_unix.run command
