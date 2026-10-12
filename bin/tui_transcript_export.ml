open! Core

let path env name =
  let base =
    if Filename.is_absolute name then Eio.Stdenv.fs env else Eio.Stdenv.cwd env
  in
  Eio.Path.(base / name)
;;

let same_definition ~env ~definition_file ~output_file =
  Frontend_transcript.Destination.conflicts_with
    ~env
    ~source_file:definition_file
    ~output_file
;;

let confirm_overwrite ~env output_file =
  if not (Eio.Path.is_file (path env output_file))
  then true
  else (
    Eio.Flow.copy_string
      (sprintf "File %s exists. Overwrite? [y/N] " output_file)
      (Eio.Stdenv.stdout env);
    let input = Eio.Buf_read.of_flow (Eio.Stdenv.stdin env) ~max_size:1_024 in
    match Eio.Buf_read.line input with
    | answer ->
      List.mem [ "y"; "yes" ] (String.lowercase (String.strip answer)) ~equal:String.equal
    | exception End_of_file -> false)
;;

let render client =
  let open Or_error.Let_syntax in
  let limits = Document_schema.Limits.default in
  let snapshot =
    Chat_tui.Agent_session_client.projection client |> Chat_tui.Agent_projection.fields
  in
  let%bind canonical =
    Chat_tui.Agent_session_client.read_canonical
      client
      ~revision:snapshot.revision
      ~limits
      ~checkpoint:Eio.Fiber.check
    |> Result.map_error ~f:(fun error ->
      Error.create_s [%sexp (error : Agent_protocol.Error.t)])
  in
  let render_error error =
    Error.create_s [%sexp (error : History_chatmd.Rendering.Error.t)]
  in
  let%bind rendering_limits =
    History_chatmd.Rendering.Limits.create
      ~max_bytes:(64 * 1024 * 1024)
      ~max_work_steps:(256 * 1024 * 1024)
      ~json_limits:limits
    |> Result.map_error ~f:render_error
  in
  let snapshot =
    Agent_client.Canonical_export.snapshot canonical
    |> Agent_protocol.Public.Snapshot.fields
  in
  Agent_session.Chatmd_export.render_public_bounded
    snapshot.canonical_history.entries
    ~limits:rendering_limits
    ~checkpoint:Eio.Fiber.check
  |> Result.map_error ~f:render_error
;;

let write client ~env ~definition_file ~output_file =
  if same_definition ~env ~definition_file ~output_file
  then
    Or_error.error_string
      "Export destination must differ from the authored ChatMD definition."
  else if not (confirm_overwrite ~env output_file)
  then Ok ()
  else
    let open Or_error.Let_syntax in
    let%bind text = render client in
    Eio.Fiber.check ();
    let%bind () =
      Agent_client.Blob_download.install_atomic
        ~path:(path env output_file)
        ~download:(fun output ->
          Eio.Flow.copy_string text output;
          Ok ())
    in
    Eio.Flow.copy_string
      (sprintf "Transcript exported to %s\n" output_file)
      (Eio.Stdenv.stdout env);
    Ok ()
;;
