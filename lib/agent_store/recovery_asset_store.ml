open! Core
module P = Agent_protocol
module Citation = P.Recovery_window.Citation
module Scope = P.Recovery_window.Scope
module B = Blob_store
module Files = Result_intent_file
module Reference = Recovery_asset_reference
module Record = Recovery_asset_record
module Inventory = Recovery_asset_inventory

type t =
  { blobs : B.t
  ; sessions : Session_store.t
  ; handle : Session_store.Handle.t
  ; root : B.Result_root.t
  ; owner : P.Session_ref.t
  }

type error =
  | Denied
  | Unavailable
  | Missing
  | Corrupt
  | Unsupported
  | Limit
  | Conflict
[@@deriving equal, sexp_of]

type check = unit -> bool

let corruption message = Error (Store_error.Corrupt message)

let from_store = function
  | Store_error.Missing _ -> Missing
  | Admission_capacity _ -> Limit
  | Io _ | Locked _ -> Unavailable
  | Corrupt _ | Document _ | Framing _ | Schema_too_new _ | Migration_required _ ->
    Corrupt
;;

let from_reference = function
  | Reference.Corrupt -> Corrupt
  | Unsupported -> Unsupported
  | Limit -> Limit
;;

let map_store result = Result.map_error result ~f:from_store

let create blobs ~sessions ~handle =
  let open Result.Let_syntax in
  let%map root =
    B.Result_root.retained_conversation blobs ~sessions ~handle
    |> Result.map_error ~f:(fun _ -> Unavailable)
  in
  let owner =
    P.Session_ref.create
      ~server_id:(Session_store.server_id sessions)
      ~session_id:(Session_store.Handle.session_id handle)
  in
  { blobs; sessions; handle; root; owner }
;;

let owner_matches t citation =
  P.Session_ref.equal t.owner (Scope.session (Citation.scope citation))
;;

let max_assets = 256

let locate t ~env ~phase filename =
  Files.recovery_asset ~env ~session:t.handle ~phase ~filename
;;

let guard t check =
  if not (Session_store.retained_lifecycle_available t.sessions t.handle)
  then Error Unavailable
  else if check ()
  then Ok ()
  else Error Denied
;;

let authorized t ~check citation ~part ~f =
  let open Result.Let_syntax in
  let%bind () = guard t check in
  let%bind () = if owner_matches t citation then Ok () else Error Unavailable in
  let%bind filename =
    Record.filename citation ~part |> Result.map_error ~f:(fun _ -> Limit)
  in
  let%bind result =
    Session_store.with_recovery_assets t.sessions ~handle:t.handle ~f:(fun ~env ->
      let result =
        let%bind () = guard t check in
        let%bind intent = locate t ~env ~phase:Files.Intent filename |> map_store in
        let%bind commit = locate t ~env ~phase:Files.Commit filename |> map_store in
        f ~env ~intent ~commit
      in
      Ok result)
    |> Result.map_error ~f:(fun _ -> Unavailable)
  in
  let%bind () = guard t check in
  result
;;

let get file = Files.read_exact file ~max_frame_bytes:Record.max_frame_bytes

let save file contents =
  Files.save_exact file ~max_frame_bytes:Record.max_frame_bytes contents
;;

let exact_intent t ~citation ~part ~intent =
  let open Result.Let_syntax in
  let%bind prior = get intent |> map_store in
  match prior with
  | None -> Ok None
  | Some contents ->
    let%bind record = Record.decode contents |> map_store in
    let reference = Record.reference record in
    if
      owner_matches t (Reference.citation reference)
      && Citation.equal citation (Reference.citation reference)
      && Int.equal part (Reference.part reference)
    then Ok (Some record)
    else Error Corrupt
;;

let state ?(stabilize_commit = true) t ~citation ~part ~intent ~commit =
  let open Result.Let_syntax in
  let%bind pending = exact_intent t ~citation ~part ~intent in
  let%bind committed = get commit |> map_store in
  match pending, committed with
  | None, None -> Ok `Empty
  | Some record, None -> Ok (`Pending record)
  | Some record, Some contents when String.equal contents (Record.contents record) ->
    let%map () =
      if stabilize_commit
      then
        Files.stabilize_exact
          commit
          ~expected:contents
          ~max_frame_bytes:Record.max_frame_bytes
        |> map_store
      else Ok ()
    in
    `Committed record
  | None, Some _ | Some _, Some _ -> Error Corrupt
;;

let max_asset_bytes = P.Recovery_asset_read.max_original_bytes

let bounded t ~max_bytes length =
  if
    max_bytes <= 0
    || Int64.(
         length > of_int (Int.min max_bytes max_asset_bytes)
         || length > B.max_upload_bytes t.blobs)
  then Error Limit
  else Ok ()
;;

let verify t ~sw ~max_bytes record =
  let open Result.Let_syntax in
  let blob = Reference.blob (Record.reference record) in
  let%bind () = bounded t ~max_bytes blob.byte_length in
  let%bind source = B.open_result t.blobs t.root blob.id |> map_store in
  let expected =
    Blob_metadata_document.value (Blob_stage_documents.durable (Record.stage record))
  in
  if Blob_metadata.equal expected (B.Result_read.metadata source)
  then
    B.load_result_verified
      t.blobs
      ~sw
      source
      ~max_bytes:(Int.max 1 (Int64.to_int_exn blob.byte_length))
    |> map_store
  else Error Corrupt
;;

let admit_content ~kind ~media_type contents =
  match kind with
  | P.Blob.Image ->
    (match P.Inline_image.media_type_of_contents contents with
     | Some actual when String.equal media_type actual -> Ok ()
     | Some _ | None -> Error Corrupt)
  | File -> Ok ()
  | Audio | Binary -> Error Unsupported
;;

let install t ~sw ~check ~intent ~commit ~max_bytes record contents =
  let open Result.Let_syntax in
  let%bind () = guard t check in
  let frame = Record.contents record in
  let%bind () = save intent frame |> map_store in
  let%bind () = guard t check in
  let%bind _ =
    B.ensure_result_staged_content
      t.blobs
      ~sw
      t.root
      ~stage:(Record.stage record)
      contents
    |> map_store
  in
  let%bind verified = verify t ~sw ~max_bytes record in
  let%bind () = guard t check in
  let%bind () = if String.equal verified contents then Ok () else Error Corrupt in
  let%map () =
    Files.save_exact
      commit
      ~max_frame_bytes:Record.max_frame_bytes
      ~before_rename:(fun () ->
        match guard t check with
        | Ok () -> Ok ()
        | Error _ -> corruption "recovery source changed before commit")
      frame
    |> map_store
  in
  Record.reference record
;;

let inventory_reader t ~env =
  Retention_reader.create
    ~env
    ~root:(Session_store.Handle.directory t.handle)
    ~max_entries:(max_assets * 4)
    ~max_bytes:(max_assets * Record.max_frame_bytes * 4)
;;

let admit_new t ~env =
  let open Result.Let_syntax in
  let%bind reader = inventory_reader t ~env in
  let%bind count = Inventory.intent_count reader in
  if count < max_assets
  then Ok ()
  else
    Error
      (Store_error.Admission_capacity
         (Document_schema.Error.Limit_exceeded "recovery assets"))
;;

let publish
      t
      ~sw
      ~check
      citation
      ~part
      ~kind
      ~media_type
      ~principal
      ~now
      ~max_bytes
      contents
  =
  authorized t ~check citation ~part ~f:(fun ~env ~intent ~commit ->
    let open Result.Let_syntax in
    let length = Int64.of_int (String.length contents) in
    let%bind () = bounded t ~max_bytes length in
    let%bind () = admit_content ~kind ~media_type contents in
    let digest = Digestif.SHA256.(digest_string contents |> to_hex) in
    let%bind prior = state t ~citation ~part ~intent ~commit in
    let matches record =
      let blob = Reference.blob (Record.reference record) in
      P.Blob.equal_kind blob.kind kind
      && String.equal blob.media_type media_type
      && Int64.equal blob.byte_length length
      && String.equal blob.digest digest
      && P.Id.Principal.equal
           (Blob_metadata_document.value
              (Blob_stage_documents.temporary (Record.stage record)))
             .creating_principal
           principal
    in
    match prior with
    | `Committed record when matches record ->
      let%map _ = verify t ~sw ~max_bytes record in
      Record.reference record
    | `Pending record when matches record ->
      install t ~sw ~check ~intent ~commit ~max_bytes record contents
    | `Committed _ | `Pending _ -> Error Conflict
    | `Empty ->
      let%bind () = admit_new t ~env |> map_store in
      let%bind blob =
        P.Blob.Metadata.create
          ~id:(P.Id.Blob.create ())
          ~kind
          ~media_type
          ~byte_length:length
          ~digest
          ()
        |> Result.map_error ~f:(fun _ -> Corrupt)
      in
      let%bind reference =
        Reference.create citation ~part ~blob |> Result.map_error ~f:from_reference
      in
      let%bind record = Record.select reference ~principal ~now |> map_store in
      install t ~sw ~check ~intent ~commit ~max_bytes record contents)
;;

let resume t ~sw ~check ~accepts citation ~part ~max_bytes =
  authorized t ~check citation ~part ~f:(fun ~env:_ ~intent ~commit ->
    let open Result.Let_syntax in
    let%bind state = state t ~citation ~part ~intent ~commit in
    match state with
    | `Empty -> Error Unavailable
    | `Committed record ->
      let%bind contents = verify t ~sw ~max_bytes record in
      let reference = Record.reference record in
      if accepts reference contents then Ok reference else Error Corrupt
    | `Pending record ->
      let metadata =
        Blob_metadata_document.value
          (Blob_stage_documents.temporary (Record.stage record))
      in
      let%bind () = bounded t ~max_bytes metadata.blob.byte_length in
      let%bind contents =
        B.load_result_staged_content t.blobs ~sw t.root ~metadata ~max_bytes |> map_store
      in
      let%bind contents = Result.of_option contents ~error:Unavailable in
      if accepts (Record.reference record) contents
      then install t ~sw ~check ~intent ~commit ~max_bytes record contents
      else Error Corrupt)
;;

let has_recorded_file_commit t ~check citation ~part =
  authorized t ~check citation ~part ~f:(fun ~env:_ ~intent ~commit ->
    let%bind.Result state =
      state ~stabilize_commit:false t ~citation ~part ~intent ~commit
    in
    match state with
    | `Empty | `Pending _ -> Ok false
    | `Committed record ->
      if P.Blob.equal_kind (Reference.blob (Record.reference record)).kind File
      then Ok true
      else Error Conflict)
;;

let read t ~sw ~check citation ~part ~max_bytes =
  authorized t ~check citation ~part ~f:(fun ~env:_ ~intent ~commit ->
    let open Result.Let_syntax in
    let%bind state = state t ~citation ~part ~intent ~commit in
    match state with
    | `Empty | `Pending _ -> Error Unavailable
    | `Committed record ->
      let%map bytes = verify t ~sw ~max_bytes record in
      Record.reference record, bytes)
;;

let has_entries = Inventory.has_entries

let collect t ~reader ~retired =
  Session_store.try_with_recovery_assets t.sessions ~handle:t.handle ~f:(fun ~env ->
    let open Result.Let_syntax in
    let%bind () =
      if
        String.equal
          (Retention_reader.root reader)
          (Session_store.Handle.directory t.handle)
        && Retention_reader.uses_filesystem reader ~env
      then Ok ()
      else corruption "recovery collector has another physical source reader"
    in
    B.with_collection_proof t.blobs ~f:(fun retention ->
      let%bind inventory = Inventory.read reader ~owner:t.owner in
      let candidates =
        List.filter (Inventory.entries inventory) ~f:(fun entry ->
          retired (Reference.citation (Record.reference entry.Inventory.record)))
      in
      B.protect_collection_mutation retention ~f:(fun () ->
        let%bind () = Inventory.remove_temporaries inventory ~env ~reader in
        List.fold_result
          candidates
          ~init:0
          ~f:(fun count { Inventory.filename; record; committed } ->
            let remove phase =
              let%bind path = locate t ~env ~phase filename in
              Files.remove_exact
                path
                ~expected:(Record.contents record)
                ~max_frame_bytes:Record.max_frame_bytes
            in
            if not (retired (Reference.citation (Record.reference record)))
            then Ok count
            else (
              let%bind () = if committed then remove Files.Commit else Ok () in
              let%bind () =
                B.discard_recovery_stage
                  retention
                  t.root
                  ~reader
                  ~stage:(Record.stage record)
              in
              let%map () = remove Files.Intent in
              count + 1)))))
  |> Result.map ~f:Option.join
;;
