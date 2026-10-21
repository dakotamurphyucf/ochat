open Core
module P = Agent_protocol

let corrupt message = Error (Store_error.Corrupt message)

let protocol result =
  Result.map_error result ~f:(fun error -> Store_error.Corrupt error.P.Error.message)
;;

let at_directory reader directory = Retention_reader.at_root reader ~root:directory

let run_ids_with_checkpoint ~reader ~checkpoint ~directory =
  let open Result.Let_syntax in
  checkpoint ();
  let%bind reader = at_directory reader directory in
  let%bind names =
    Retention_reader.list_with_checkpoint reader ~directory:"." ~checkpoint
  in
  List.map names ~f:(fun name ->
    checkpoint ();
    let%bind id = P.Id.Developer_run.of_string name |> protocol in
    if not (String.equal name (P.Id.Developer_run.to_string id))
    then corrupt "developer blob run directory is not canonical"
    else (
      let%bind kind = Retention_reader.kind reader ~path:name in
      match kind with
      | `Directory -> Ok id
      | `File -> corrupt "developer blob run entry is not a directory"))
  |> Result.all
;;

let run_ids ~reader ~directory =
  run_ids_with_checkpoint ~reader ~checkpoint:(fun () -> ()) ~directory
;;

let blob_file name =
  let suffix =
    List.find [ ".sexp"; ".blob"; ".part" ] ~f:(fun suffix ->
      String.is_suffix name ~suffix)
  in
  match suffix with
  | None -> corrupt "unknown developer blob file"
  | Some suffix ->
    let base = String.drop_suffix name (String.length suffix) in
    let open Result.Let_syntax in
    let%bind id = P.Id.Blob.of_string base |> protocol in
    if String.equal base (P.Id.Blob.to_string id)
    then Ok (id, suffix)
    else corrupt "developer blob filename is not canonical"
;;

module Entry = struct
  module State = struct
    type t =
      | Complete of
          { metadata : Blob_metadata.t
          ; content : string
          }
      | Partial of Blob_metadata.t option
  end

  type t =
    { id : P.Id.Blob.t
    ; state : State.t
    ; durable : bool
    ; metadata_bytes : string option
    ; metadata_document : Document_schema.Document.t option
    }

  let id t = t.id
  let state t = t.state
  let durable t = t.durable
  let metadata_bytes t = t.metadata_bytes

  let iter_metadata_strings t ~checkpoint ~f =
    Option.iter t.metadata_document ~f:(fun document ->
      Document_fields.iter_strings_with_checkpoint
        (Document_schema.Document.json document)
        ~checkpoint
        ~f)
  ;;
end

let digest_string ~checkpoint data =
  let rec feed state offset =
    if offset >= String.length data
    then Digestif.SHA256.(get state |> to_hex)
    else (
      checkpoint ();
      let length = Int.min 4096 (String.length data - offset) in
      let chunk = String.sub data ~pos:offset ~len:length in
      feed (Digestif.SHA256.feed_string state chunk) (offset + length))
  in
  feed Digestif.SHA256.empty 0
;;

let scan_run ~reader ~checkpoint ~directory ~target ~durable ~max_blob_bytes =
  let open Result.Let_syntax in
  checkpoint ();
  let%bind reader = at_directory reader directory in
  let%bind names =
    Retention_reader.list_with_checkpoint reader ~directory:"." ~checkpoint
  in
  let grouped = Hashtbl.create (module P.Id.Blob) in
  let%bind () =
    List.fold_result names ~init:() ~f:(fun () name ->
      checkpoint ();
      let%bind id, suffix = blob_file name in
      let%bind kind = Retention_reader.kind reader ~path:name in
      match kind with
      | `Directory -> corrupt "developer blob file is a directory"
      | `File ->
        Hashtbl.update grouped id ~f:(fun previous ->
          Set.add (Option.value previous ~default:String.Set.empty) suffix);
        Ok ())
  in
  checkpoint ();
  Hashtbl.to_alist grouped
  |> List.sort ~compare:(fun (left, _) (right, _) -> P.Id.Blob.compare left right)
  |> List.map ~f:(fun (id, suffixes) ->
    checkpoint ();
    let name suffix = P.Id.Blob.to_string id ^ suffix in
    let has suffix = Set.mem suffixes suffix in
    let%bind metadata, metadata_bytes, metadata_document =
      if has ".sexp"
      then (
        let%bind metadata_bytes =
          Retention_reader.read_with_checkpoint
            reader
            ~path:(name ".sexp")
            ~max_bytes:(Document_schema.Limits.max_bytes Blob_metadata_document.limits)
            ~checkpoint
        in
        checkpoint ();
        let%bind source =
          Document_schema.Document.decode
            ~limits:Blob_metadata_document.limits
            metadata_bytes
          |> Document_fields.store
        in
        let%map document =
          Blob_metadata_document.of_document source |> Document_fields.store
        in
        Some (Blob_metadata_document.value document), Some metadata_bytes, Some source)
      else Ok (None, None, None)
    in
    let%bind () =
      match metadata with
      | None -> if durable then corrupt "durable blob metadata is missing" else Ok ()
      | Some metadata ->
        if
          P.Id.Blob.equal metadata.blob.id id
          && Bool.equal metadata.durable durable
          && Blob_metadata.Target.equal
               metadata.target
               (Blob_metadata.Target.Developer target)
          && Int64.(metadata.blob.byte_length <= max_blob_bytes)
        then Ok ()
        else corrupt "developer blob metadata differs from its storage root"
    in
    let max_read =
      match metadata with
      | Some metadata ->
        Int64.to_int metadata.blob.byte_length |> Option.value ~default:(-1)
      | None -> Int64.to_int max_blob_bytes |> Option.value ~default:(-1)
    in
    let%bind () =
      if max_read >= 0
      then Ok ()
      else corrupt "developer blob length exceeds bounded read"
    in
    let%bind data =
      if has ".blob"
      then
        Retention_reader.read_with_checkpoint
          reader
          ~path:(name ".blob")
          ~max_bytes:max_read
          ~checkpoint
        |> Result.map ~f:Option.some
      else Ok None
    in
    let%bind () =
      if has ".part"
      then
        if durable
        then corrupt "durable developer Blob has a partial stage"
        else
          Retention_reader.read_with_checkpoint
            reader
            ~path:(name ".part")
            ~max_bytes:max_read
            ~checkpoint
          |> Result.map ~f:(fun _ -> ())
      else Ok ()
    in
    let%bind () =
      match metadata, data with
      | Some metadata, Some content ->
        if
          Int64.equal metadata.blob.byte_length (Int64.of_int (String.length content))
          && String.equal metadata.blob.digest (digest_string ~checkpoint content)
        then Ok ()
        else corrupt "developer blob data differs from metadata"
      | Some _, None | None, Some _ | None, None -> Ok ()
    in
    let%bind state =
      match metadata, data, has ".part" with
      | Some metadata, Some content, false ->
        Ok (Entry.State.Complete { metadata; content })
      | _, _, _ when durable -> corrupt "durable developer Blob is incomplete"
      | metadata, _, _ -> Ok (Partial metadata)
    in
    checkpoint ();
    Ok { Entry.id; state; durable; metadata_bytes; metadata_document })
  |> Result.all
;;

let validate_run ~reader ~directory ~target ~durable ~max_blob_bytes =
  let open Result.Let_syntax in
  let%bind entries =
    scan_run
      ~reader
      ~checkpoint:(fun () -> ())
      ~directory
      ~target
      ~durable
      ~max_blob_bytes
  in
  List.fold_result entries ~init:() ~f:(fun () entry ->
    match Entry.state entry with
    | Complete _ -> Ok ()
    | Partial _ -> corrupt "developer blob publication is incomplete")
;;
