open! Core

let configuration_error message =
  Agent_protocol.Error.create Configuration_invalid ~message ~retryable:false ()
;;

let socket_path env value = Eio.Path.(Eio.Stdenv.fs env / value)

let validate_parent env path =
  let parent = socket_path env (Filename.dirname path) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 parent;
  let stat = Eio.Path.stat ~follow:true parent in
  let expected_uid = Peer_credentials.effective_uid () |> Int64.of_int in
  if not (Poly.equal stat.kind `Directory)
  then Error (configuration_error "Unix socket parent is not a directory")
  else if not (Int64.equal stat.uid expected_uid)
  then Error (configuration_error "Unix socket parent is owned by another user")
  else if stat.perm land 0o077 <> 0
  then
    Error
      (configuration_error
         "Unix socket parent must not be accessible to group or other users")
  else Ok ()
;;

let probe env path =
  try
    Eio.Switch.run (fun sw ->
      let flow = Eio.Net.connect ~sw (Eio.Stdenv.net env) (`Unix path) in
      Eio.Flow.close flow);
    Ok `Live
  with
  | Eio.Io (Eio.Net.E (Connection_failure (Refused _)), _) -> Ok `Refused
  | exn ->
    Error
      (configuration_error ("failed to probe existing Unix socket: " ^ Exn.to_string exn))
;;

let same_identity (first : Eio.File.Stat.t) (second : Eio.File.Stat.t) =
  Int64.equal first.dev second.dev && Int64.equal first.ino second.ino
;;

let remove_stale env path first_stat =
  let file = socket_path env path in
  match probe env path with
  | Error _ as failure -> failure
  | Ok `Live -> Error (configuration_error "Unix socket already has a live listener")
  | Ok `Refused ->
    let second_stat = Eio.Path.stat ~follow:false file in
    if same_identity first_stat second_stat
    then (
      Eio.Path.unlink file;
      Ok ())
    else Error (configuration_error "Unix socket changed while checking staleness")
;;

let prepare_path ~env ~socket_path:path =
  let open Result.Let_syntax in
  let%bind () = validate_parent env path in
  let file = socket_path env path in
  match Eio.Path.kind ~follow:false file with
  | `Not_found -> Ok ()
  | `Socket -> remove_stale env path (Eio.Path.stat ~follow:false file)
  | _ -> Error (configuration_error "Unix socket path exists and is not a socket")
;;

let serve
      ~dispatcher
      ~close_connection
      ~authenticate
      ~max_line_length
      ~outgoing_capacity
      ~max_attachments
      ~on_protocol_error
      flow
      address
  =
  match authenticate flow address with
  | Error failure -> on_protocol_error failure
  | Ok principal ->
    Eio.Switch.run (fun connection_switch ->
      Agent_transport_stdio.Server.run
        ~sw:connection_switch
        ~dispatcher
        ~close_connection
        ~principal
        ~connection_id:
          (Agent_protocol.Id.Attachment.create ()
           |> Agent_protocol.Id.Attachment.to_string)
        ~input:flow
        ~output:flow
        ~max_line_length
        ~outgoing_capacity
        ~max_attachments
        ~on_error:on_protocol_error)
;;

let run
      ~sw
      ~net
      ~socket_path
      ~backlog
      ~dispatcher
      ~close_connection
      ~authenticate
      ~max_line_length
      ~outgoing_capacity
      ~max_attachments
      ~on_error
      ~on_protocol_error
  =
  let socket = Eio.Net.listen ~sw ~reuse_addr:true ~backlog net (`Unix socket_path) in
  Eio.Net.run_server
    ~on_error
    socket
    (serve
       ~dispatcher
       ~close_connection
       ~authenticate
       ~max_line_length
       ~outgoing_capacity
       ~max_attachments
       ~on_protocol_error)
;;
