open! Core
module P = Agent_protocol
module Public = P.Public

type t =
  { snapshot : Public.Snapshot.t
  ; metadata : P.Method_result.Export.t
  }

let snapshot t = t.snapshot
let metadata t = t.metadata
let invalid message = Error (P.Error.invalid_request message)

let poll ~checkpoint =
  Eio.Fiber.check ();
  checkpoint ();
  Eio.Fiber.check ()
;;

let stage ~checkpoint f =
  poll ~checkpoint;
  let result = f () in
  poll ~checkpoint;
  result
;;

let admit_metadata metadata ~revision ~limits =
  let blob = metadata.P.Method_result.Export.blob in
  if not (Int64.equal metadata.session_revision revision)
  then invalid "canonical export returned another revision"
  else if
    (not (P.Blob.equal_kind blob.kind File))
    || not (String.equal blob.media_type "application/json")
  then invalid "canonical export returned another format"
  else if Int64.(blob.byte_length <= zero)
  then invalid "canonical export has an empty or negative byte length"
  else if Int64.(blob.byte_length > of_int (Document_schema.Limits.max_bytes limits))
  then
    Error
      (P.Error.create
         Resource_limit
         ~message:"canonical export exceeds the download byte limit"
         ~retryable:false
         ())
  else Ok ()
;;

let decode_snapshot json ~checkpoint =
  let open Result.Let_syntax in
  let%bind fields = P.Json_codec.fields json in
  let%bind snapshot =
    stage ~checkpoint (fun () ->
      P.Json_codec.required_as fields "snapshot" Public.Snapshot.of_json)
  in
  let%bind history =
    stage ~checkpoint (fun () ->
      P.Json_codec.required_as fields "history" (P.Json_codec.list Public.History.of_json))
  in
  let window = (Public.Snapshot.fields snapshot).canonical_history in
  if
    (not (window.reached_start && window.reached_end && window.structurally_complete))
    || Option.is_some window.previous_cursor
    || Option.is_some window.next_cursor
  then invalid "canonical export contains a partial history window"
  else (
    let%bind () =
      List.fold_result window.entries ~init:() ~f:(fun () entry ->
        poll ~checkpoint;
        match entry.Public.History.body with
        | Full _ -> Ok ()
        | Visible _ | Redacted _ ->
          Error
            (P.Error.create
               Permission_denied
               ~message:"canonical export requires fully disclosed history"
               ~retryable:false
               ()))
    in
    if
      stage ~checkpoint (fun () -> List.equal Public.History.equal window.entries history)
    then Ok snapshot
    else invalid "canonical export history disagrees with its snapshot")
;;

let admit_snapshot snapshot metadata ~session_id =
  let fields = Public.Snapshot.fields snapshot in
  if not (P.Id.Session.equal fields.session.id session_id)
  then invalid "canonical export belongs to another session"
  else if
    (not (Int64.equal fields.revision metadata.P.Method_result.Export.session_revision))
    || not (Int64.equal fields.latest_event_sequence metadata.latest_event_sequence)
  then invalid "canonical export positions disagree with its metadata"
  else Ok { snapshot; metadata }
;;

let read handle ~revision ~limits ~checkpoint =
  let open Result.Let_syntax in
  let%bind () =
    if Int64.(revision < zero)
    then invalid "canonical export revision must be nonnegative"
    else Ok ()
  in
  let%bind metadata =
    stage ~checkpoint (fun () ->
      Session_handle.export handle ~format:Json ~revision:(Some revision))
  in
  let%bind () = admit_metadata metadata ~revision ~limits in
  (* The existing downloader checks every chunk against the admitted metadata
     before writing it, then authenticates the complete digest. *)
  let output = Buffer.create (Int.min 65536 (Document_schema.Limits.max_bytes limits)) in
  let%bind () =
    stage ~checkpoint (fun () ->
      Session_handle.download_blob
        handle
        ~blob:metadata.blob
        ~output:(Eio.Flow.buffer_sink output))
  in
  let%bind json =
    stage ~checkpoint (fun () ->
      Document_schema.Json.decode ~limits (Buffer.contents output))
    |> Result.map_error ~f:(fun error ->
      P.Error.invalid_request
        ("invalid canonical export document: "
         ^ Sexp.to_string_hum (Document_schema.Error.sexp_of_t error)))
  in
  let%bind snapshot = decode_snapshot json ~checkpoint in
  stage ~checkpoint (fun () ->
    admit_snapshot snapshot metadata ~session_id:(Session_handle.session_id handle))
;;
