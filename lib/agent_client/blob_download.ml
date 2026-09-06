open! Core

let invalid message =
  Agent_protocol.Error.create Invalid_request ~message ~retryable:false ()
;;

let same_blob
      (left : Agent_protocol.Blob.Metadata.t)
      (right : Agent_protocol.Blob.Metadata.t)
  =
  Agent_protocol.Id.Blob.compare left.id right.id = 0
  && Agent_protocol.Blob.equal_kind left.kind right.kind
  && String.equal left.media_type right.media_type
  && Int64.equal left.byte_length right.byte_length
  && String.Caseless.equal left.digest right.digest
  && Option.equal String.equal left.display_name right.display_name
;;

let request connection ~session_id ~attachment_id ~blob_id ~offset =
  let request =
    Agent_protocol.Blob.Read_request.
      { session_id; attachment_id; blob_id; offset; max_bytes = max_chunk_bytes }
  in
  match Connection.request connection (Blob_read request) with
  | Ok (Blob_read chunk) -> Ok chunk
  | Ok _ -> Error (invalid "unexpected blob.read result")
  | Error _ as failure -> failure
;;

let decode chunk =
  Base64.decode chunk.Agent_protocol.Blob.Chunk.data_base64
  |> Result.map_error ~f:(fun _ -> invalid "blob.read returned invalid base64")
;;

let validate_chunk expected offset chunk data =
  if not (same_blob expected chunk.Agent_protocol.Blob.Chunk.blob)
  then Error (invalid "blob.read metadata changed during download")
  else if not (Int64.equal chunk.offset offset)
  then Error (invalid "blob.read returned a discontinuous offset")
  else if String.is_empty data && not chunk.eof
  then Error (invalid "blob.read made no progress before end of file")
  else if not (Int64.equal chunk.next_offset Int64.(offset + of_int (String.length data)))
  then Error (invalid "blob.read returned an inconsistent next offset")
  else if Int64.(chunk.next_offset > expected.byte_length)
  then Error (invalid "blob.read exceeded the advertised byte length")
  else if String.length data > Agent_protocol.Blob.Read_request.max_chunk_bytes
  then Error (invalid "blob.read exceeded the requested chunk bound")
  else Ok ()
;;

let validate_complete (blob : Agent_protocol.Blob.Metadata.t) next_offset digest =
  let actual = Digestif.SHA256.(get digest |> to_hex) in
  if not (Int64.equal next_offset blob.byte_length)
  then Error (invalid "blob download ended at the wrong byte length")
  else if not (String.Caseless.equal actual blob.digest)
  then Error (invalid "blob download digest does not match metadata")
  else Ok ()
;;

let download
      ~connection
      ~session_id
      ~attachment_id
      ~(blob : Agent_protocol.Blob.Metadata.t)
      ~output
  =
  let rec loop offset digest =
    let open Result.Let_syntax in
    let%bind chunk =
      request connection ~session_id ~attachment_id ~blob_id:blob.id ~offset
    in
    let%bind data = decode chunk in
    let%bind () = validate_chunk blob offset chunk data in
    Eio.Flow.copy_string data output;
    let digest = Digestif.SHA256.feed_string digest data in
    if chunk.eof
    then validate_complete blob chunk.next_offset digest
    else loop chunk.next_offset digest
  in
  loop Int64.zero Digestif.SHA256.empty
;;

let write_temporary temporary download =
  Eio.Switch.run (fun sw ->
    let output = Eio.Path.open_out ~sw ~create:(`Exclusive 0o600) temporary in
    let open Or_error.Let_syntax in
    let%bind () =
      download (output :> Eio.Flow.sink_ty Eio.Resource.t)
      |> Result.map_error ~f:(fun error ->
        Error.of_string error.Agent_protocol.Error.message)
    in
    Eio.File.sync output;
    Ok ())
;;

let install_checked ~parent ~temporary ~path ~download ~installed =
  try
    let open Or_error.Let_syntax in
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 parent;
    let%bind () = write_temporary temporary download in
    Eio.Path.rename temporary path;
    installed := true;
    Ok ()
  with
  | Eio.Cancel.Cancelled _ as exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    Stdlib.Printexc.raise_with_backtrace exn backtrace
  | exn -> Error (Error.of_exn exn)
;;

let install_atomic ~path ~download =
  let parent, name = Eio.Path.split path |> Option.value_exn in
  let nonce = Agent_protocol.Id.Transaction.(create () |> to_string) in
  let temporary = Eio.Path.(parent / (name ^ ".ochat-part-" ^ nonce)) in
  let installed = ref false in
  Exn.protect
    ~f:(fun () -> install_checked ~parent ~temporary ~path ~download ~installed)
    ~finally:(fun () ->
      if not !installed
      then
        Eio.Cancel.protect (fun () ->
          ignore (Option.try_with (fun () -> Eio.Path.unlink temporary) : unit option)))
;;
