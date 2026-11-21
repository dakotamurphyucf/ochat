open! Core
module P = Agent_protocol
module Source = Recovery_source_reader
module Ref = Agent_store.Recovery_asset_reference
module Store = Agent_store.Recovery_asset_store
module Payload = History_entry.Payload

type t =
  { sessions : Agent_store.Session_store.t
  ; handle : Agent_store.Session_store.Handle.t
  ; store : Store.t
  ; max_frame_bytes : int
  }

type current = unit -> (Session_state.t, P.Error.t) result

let store_error = function
  | Store.Denied -> Source.Denied
  | Unavailable -> Unavailable
  | Missing -> Missing
  | Corrupt -> Corrupt
  | Unsupported -> Unsupported
  | Limit -> Limit
  | Conflict -> Stale
;;

let create blobs ~sessions ~handle ~max_frame_bytes =
  let%map.Result store =
    Store.create blobs ~sessions ~handle |> Result.map_error ~f:store_error
  in
  { sessions; handle; store; max_frame_bytes }
;;

type local_part =
  | Text of string
  | Image_uri of string

let projection_limits =
  Document_schema.Limits.create
    ~max_bytes:(4 * 1024 * 1024)
    ~max_fields:65_536
    ~max_nodes:131_072
    ~max_depth:64
  |> Result.ok
  |> Option.value_exn
;;

let selected row ~part =
  let open Result.Let_syntax in
  if part < 0 || part >= 4096
  then Error Source.Limit
  else (
    let%bind () =
      Document_schema.Json.validate ~limits:projection_limits row.P.History.payload
      |> Result.map_error ~f:(function
        | Document_schema.Error.Limit_exceeded _ -> Source.Limit
        | _ -> Corrupt)
    in
    let%bind entry =
      History_codec.of_protocol row |> Result.map_error ~f:(fun _ -> Source.Corrupt)
    in
    let semantic =
      History_entry.payload entry |> Payload.semantic |> Payload.Semantic.view
    in
    let decode content =
      if Stdlib.List.compare_length_with content 4096 > 0
      then Error Source.Limit
      else (
        match List.nth content part with
        | None -> Error Source.Unavailable
        | Some (Payload.Content.Text { text; _ }) -> Ok (Text text)
        | Some (Image { uri; _ }) -> Ok (Image_uri uri)
        | Some (Refusal _ | Unknown _) -> Error Source.Unsupported)
    in
    match semantic with
    | Message { content; _ } | Result { output = Content content; _ } -> decode content
    | Result { output = Text text; _ } when Int.equal part 0 -> Ok (Text text)
    | Result { output = Text _; _ } -> Error Source.Unavailable
    | Call _ | Reasoning _ | Unknown _ -> Error Source.Unsupported)
;;

let with_part t ~current citation ~part ~f =
  Source.with_entry
    t.sessions
    ~handle:t.handle
    ~current
    ~max_frame_bytes:t.max_frame_bytes
    citation
    ~f:(fun row ~check ->
      let open Result.Let_syntax in
      let%bind selected = selected row ~part in
      f row selected ~check:(fun () -> Result.is_ok (check ())))
;;

let publish t ~sw ~check citation ~part ~kind ~media_type ~principal ~now ~max_bytes bytes
  =
  Store.publish
    t.store
    ~sw
    ~check
    citation
    ~part
    ~kind
    ~media_type
    ~principal
    ~now
    ~max_bytes
    bytes
  |> Result.map_error ~f:store_error
;;

let inline uri ~max_bytes =
  P.Inline_image.of_data_uri uri ~max_bytes:(Int.min max_bytes 1_048_576)
  |> Result.map_error ~f:(function
    | Unsupported -> Source.Unsupported
    | Size_limit -> Limit
    | Invalid_encoding | Media_mismatch -> Corrupt)
;;

let publish_inline t ~sw ~current citation ~part ~principal ~now ~max_bytes =
  with_part t ~current citation ~part ~f:(fun _ selected ~check ->
    match selected with
    | Text _ -> Error Source.Unsupported
    | Image_uri uri ->
      let%bind.Result image = inline uri ~max_bytes in
      publish
        t
        ~sw
        ~check
        citation
        ~part
        ~kind:Image
        ~media_type:(P.Inline_image.media_type image)
        ~principal
        ~now
        ~max_bytes
        (P.Inline_image.contents image))
;;

module Attribution = struct
  type t =
    { citation : P.Recovery_window.Citation.t
    ; part : int
    ; row : P.History.entry
    ; media_type : string
    ; original : string
    }

  let create citation ~part ~row ~media_type ~original =
    let module Citation = P.Recovery_window.Citation in
    let open Result.Let_syntax in
    if
      (not (P.History.Id.equal row.P.History.id (Citation.occurrence citation)))
      || (not
            (P.Recovery_item_version.equal
               (P.Recovery_item_version.canonical row.content_revision)
               (Citation.version citation)))
      || row.redacted
    then Error Source.Stale
    else (
      let%bind selection = selected row ~part in
      match selection with
      | Image_uri _ -> Error Source.Unsupported
      | Text _ -> Ok { citation; part; row; media_type; original })
  ;;

  let citation t = t.citation
end

let publish_converted_file t ~sw ~current ~principal ~now ~max_bytes attribution =
  let { Attribution.citation; part; row = expected; media_type; original } =
    attribution
  in
  with_part t ~current citation ~part ~f:(fun row selected ~check ->
    match selected with
    | Text _ when P.History.equal_entry row expected ->
      publish
        t
        ~sw
        ~check
        citation
        ~part
        ~kind:File
        ~media_type
        ~principal
        ~now
        ~max_bytes
        original
    | Text _ -> Error Source.Stale
    | Image_uri _ -> Error Source.Unsupported)
;;

let accepts selected ~max_bytes =
  let%map.Result image =
    match selected with
    | Text _ -> Ok None
    | Image_uri uri -> Result.map (inline uri ~max_bytes) ~f:Option.some
  in
  fun reference bytes ->
    let blob = Ref.blob reference in
    match image, blob.kind with
    | None, P.Blob.File -> true
    | Some image, Image ->
      String.equal (P.Inline_image.media_type image) blob.media_type
      && String.equal (P.Inline_image.contents image) bytes
    | _ -> false
;;

let resume t ~sw ~current citation ~part ~max_bytes =
  with_part t ~current citation ~part ~f:(fun _ selected ~check ->
    let%bind.Result accepts = accepts selected ~max_bytes in
    Store.resume t.store ~sw ~check ~accepts citation ~part ~max_bytes
    |> Result.map_error ~f:store_error)
;;

let has_recorded_file_commit t ~current citation ~part =
  let check () = Result.is_ok (current ()) in
  Store.has_recorded_file_commit t.store ~check citation ~part
  |> Result.map_error ~f:store_error
;;

let read t ~sw ~current citation ~part ~max_bytes =
  with_part t ~current citation ~part ~f:(fun _ selected ~check ->
    let open Result.Let_syntax in
    let%bind accepts = accepts selected ~max_bytes in
    let%bind reference, bytes =
      Store.read t.store ~sw ~check citation ~part ~max_bytes
      |> Result.map_error ~f:store_error
    in
    if accepts reference bytes then Ok (reference, bytes) else Error Source.Corrupt)
;;
