open! Core

let capacity_file =
  Command.Param.flag
    ~aliases:[ "--recovery-capacity-file" ]
    "-recovery-capacity-file"
    (Command.Param.optional Command.Param.string)
    ~doc:"FILE Local host recovery capacity policy (one recovery_capacity S-expression)."
;;

let load_capacity ~env = function
  | None -> Ok None
  | Some file ->
    let path =
      if Filename.is_absolute file
      then file
      else (
        let cwd = Eio.Path.native_exn (Eio.Stdenv.cwd env) in
        let cwd =
          if Filename.is_absolute cwd then cwd else Eio_posix.Low_level.realpath cwd
        in
        Filename.concat cwd file)
    in
    Agent_server.Config_validator.load_recovery_capacity ~env ~path
    |> Result.map_error ~f:(fun errors ->
      Agent_protocol.Error.invalid_request
        (Sexp.to_string_hum ([%sexp_of: Agent_server.Config.Diagnostic.t list] errors)))
;;

let enable_flag =
  Command.Param.flag
    ~aliases:[ "--enable-recovery" ]
    "-enable-recovery"
    Command.Param.no_arg
    ~doc:" Enroll this durable local conversation in context-window capture."
;;

let enroll connection (session : Agent_protocol.Session.t) ~attachment =
  let open Result.Let_syntax in
  let%bind initialization =
    Result.of_option
      (Agent_client.Connection.initialization connection)
      ~error:(Agent_protocol.Error.invalid_request "local host has not initialized")
  in
  let reference =
    Agent_protocol.Session_ref.create
      ~server_id:initialization.server_id
      ~session_id:session.id
  in
  let%map _ =
    Agent_client.Recovery_windows.enable
      connection
      { session = reference
      ; attachment_id = attachment.Agent_protocol.Session.Attachment.id
      ; expected_generation = session.generation
      }
  in
  ()
;;
