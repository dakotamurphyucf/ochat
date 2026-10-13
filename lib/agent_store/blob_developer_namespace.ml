open Core
module P = Agent_protocol

let corrupt message = Error (Store_error.Corrupt message)

let protocol result =
  Result.map_error result ~f:(fun error -> Store_error.Corrupt error.P.Error.message)
;;

let at_directory reader directory = Retention_reader.at_root reader ~root:directory

let run_ids ~reader ~directory =
  let open Result.Let_syntax in
  let%bind reader = at_directory reader directory in
  let%bind names = Retention_reader.list reader ~directory:"." in
  List.map names ~f:(fun name ->
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

let blob_file name =
  let suffix =
    List.find [ ".sexp"; ".blob" ] ~f:(fun suffix -> String.is_suffix name ~suffix)
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

let validate_run ~reader ~directory ~target ~durable ~max_blob_bytes =
  let open Result.Let_syntax in
  let%bind reader = at_directory reader directory in
  let%bind names = Retention_reader.list reader ~directory:"." in
  let grouped = Hashtbl.create (module P.Id.Blob) in
  let%bind () =
    List.fold_result names ~init:() ~f:(fun () name ->
      let%bind id, suffix = blob_file name in
      let%bind kind = Retention_reader.kind reader ~path:name in
      match kind with
      | `Directory -> corrupt "developer blob file is a directory"
      | `File ->
        Hashtbl.update grouped id ~f:(fun previous ->
          Set.add (Option.value previous ~default:String.Set.empty) suffix);
        Ok ())
  in
  Hashtbl.to_alist grouped
  |> List.sort ~compare:(fun (left, _) (right, _) -> P.Id.Blob.compare left right)
  |> List.fold_result ~init:() ~f:(fun () (id, suffixes) ->
    if not (Set.equal suffixes (String.Set.of_list [ ".sexp"; ".blob" ]))
    then corrupt "developer blob publication is incomplete"
    else (
      let%bind metadata_bytes =
        Retention_reader.read
          reader
          ~path:(P.Id.Blob.to_string id ^ ".sexp")
          ~max_bytes:(Document_schema.Limits.max_bytes Blob_metadata_document.limits)
      in
      let%bind document =
        Blob_metadata_document.of_bytes metadata_bytes
        |> Result.map_error ~f:(fun error -> Store_error.Document error)
      in
      let metadata = Blob_metadata_document.value document in
      let expected = Blob_metadata.Target.Developer target in
      if
        (not (P.Id.Blob.equal metadata.blob.id id))
        || (not (Bool.equal metadata.durable durable))
        || (not (Blob_metadata.Target.equal metadata.target expected))
        || Int64.(metadata.blob.byte_length > max_blob_bytes)
      then corrupt "developer blob metadata differs from its storage root"
      else (
        let%bind limit =
          match Int64.to_int metadata.blob.byte_length with
          | Some value when value >= 0 -> Ok value
          | _ -> corrupt "developer blob length exceeds bounded read"
        in
        let%bind data =
          Retention_reader.read
            reader
            ~path:(P.Id.Blob.to_string id ^ ".blob")
            ~max_bytes:limit
        in
        if
          String.length data = limit
          && String.equal
               metadata.blob.digest
               Digestif.SHA256.(digest_string data |> to_hex)
        then Ok ()
        else corrupt "developer blob data differs from metadata")))
;;
