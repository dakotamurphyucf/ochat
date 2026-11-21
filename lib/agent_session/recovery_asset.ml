open! Core
module P = Agent_protocol
module Source = Recovery_source_reader
module Ref = Agent_store.Recovery_asset_reference
module Store = Agent_store.Recovery_asset_store
module Payload = History_entry.Payload
module Ledger = Recovery_window_ledger
module Citation = P.Recovery_window.Citation

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

let ordinary_provenance (row : P.History.entry) =
  match row.provenance with
  | Canonical | Moderator_inserted | Moderator_replaced _ -> true
  | Runtime_notification _ | Runtime_authoring _ -> false
;;

let selected ?(ordinary = false) row ~part =
  let open Result.Let_syntax in
  if part < 0 || part >= 4096
  then Error Source.Limit
  else if ordinary && not (ordinary_provenance row)
  then Error Source.Unavailable
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
    if ordinary && Context_compaction.History_view.is_reminder entry
    then Error Source.Unavailable
    else (
      match semantic with
      | Message { role = User | Assistant; content; _ } -> decode content
      | Message { content; _ } when not ordinary -> decode content
      | Result { output = Content content; _ } when not ordinary -> decode content
      | Result { output = Text text; _ } when (not ordinary) && Int.equal part 0 ->
        Ok (Text text)
      | Result { output = Text _; _ } when not ordinary -> Error Source.Unavailable
      | _ when ordinary -> Error Source.Unavailable
      | Message _ | Result _ | Call _ | Reasoning _ | Unknown _ ->
        Error Source.Unsupported))
;;

let ordinary_visible citation (state : Session_state.t) =
  not
    (Option.exists state.recovery_windows ~f:(fun ledger ->
       Ledger.is_derived_summary ledger citation))
;;

let with_part ?(ordinary = false) t ~current citation ~part ~f =
  let visible = if ordinary then Some (ordinary_visible citation) else None in
  Source.with_entry
    ?visible
    t.sessions
    ~handle:t.handle
    ~current
    ~max_frame_bytes:t.max_frame_bytes
    citation
    ~f:(fun row ~check ->
      let open Result.Let_syntax in
      let%bind selected = selected ~ordinary row ~part in
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

let read_with_view t ~ordinary ~sw ~current citation ~part ~max_bytes =
  with_part t ~ordinary ~current citation ~part ~f:(fun _ selected ~check ->
    let open Result.Let_syntax in
    let%bind accepts = accepts selected ~max_bytes in
    let%bind reference, bytes =
      Store.read t.store ~sw ~check citation ~part ~max_bytes
      |> Result.map_error ~f:store_error
    in
    if accepts reference bytes then Ok (reference, bytes) else Error Source.Corrupt)
;;

let read t ~sw ~current citation ~part ~max_bytes =
  read_with_view t ~ordinary:false ~sw ~current citation ~part ~max_bytes
;;

let release_selection accepted citation =
  let owner = P.Recovery_window.Scope.session (Citation.scope citation) in
  let current_member (state : Session_state.t) =
    let open Result.Let_syntax in
    if not (ordinary_visible citation state)
    then Error Source.Unavailable
    else (
      let%bind visible =
        Recovery_visibility.create state ~owner
        |> Result.map_error ~f:(fun _ -> Source.Corrupt)
      in
      match Recovery_visibility.select visible citation with
      | Eligible member -> Ok member
      | Unavailable -> Error Source.Unavailable
      | Withdrawn -> Error Source.Withdrawn
      | Expired -> Error Source.Expired)
  in
  let%map.Result selected = current_member accepted in
  let windows = accepted.Session_state.recovery_windows in
  let moderator = accepted.moderator in
  let generation = accepted.identity.generation in
  fun (latest : Session_state.t) ->
    Int.equal generation latest.identity.generation
    &&
    if
      Option.equal phys_equal latest.recovery_windows windows
      && Option.equal phys_equal latest.moderator moderator
    then true
    else
      Result.ok (current_member latest)
      |> Option.exists ~f:(fun member ->
        Ledger.Source.equal (Ledger.Member.source selected) (Ledger.Member.source member)
        && Int.equal
             (Ledger.Member.source_ordinal selected)
             (Ledger.Member.source_ordinal member))
;;

let read_ordinary t ~sw ~current citation ~part ~max_bytes =
  let open Result.Let_syntax in
  let%bind body =
    read_with_view t ~ordinary:true ~sw ~current citation ~part ~max_bytes
  in
  let%bind accepted = current () |> Result.map_error ~f:(fun _ -> Source.Denied) in
  let%map check = release_selection accepted citation in
  body, check
;;
