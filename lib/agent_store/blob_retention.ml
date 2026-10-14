open Core
module P = Agent_protocol
module Id = P.Id.Blob
module Metadata = Blob_store.Metadata

type t =
  { candidates : Id.t list
  ; roots : Id.t list
  ; edges : (Id.t * Id.t list) list
  ; complete : Id.t list
  ; published : Id.t list
  ; temporary : Id.t list
  ; developer : Developer_result_inventory.Collection.t option
  }

let corrupt message = Error (Store_error.Corrupt message)

let protocol result =
  Result.map_error result ~f:(fun error -> Store_error.Corrupt error.P.Error.message)
;;

let metadata ~id contents =
  let open Result.Let_syntax in
  let%bind original =
    Document_schema.Document.decode ~limits:Blob_metadata_document.limits contents
    |> Document_fields.store
  in
  let%bind original_id =
    Blob_metadata_document.stored_blob_id original |> Document_fields.store
  in
  if not (Id.equal id original_id)
  then corrupt "original retained blob identity differs from filename"
  else (
    let%map document =
      Blob_metadata_document.of_document original |> Document_fields.store
    in
    Blob_metadata_document.value document, original)
;;

let json_media_type value =
  let value =
    String.split value ~on:';' |> List.hd_exn |> String.strip |> String.lowercase
  in
  String.equal value "application/json" || String.is_suffix value ~suffix:"+json"
;;

let files reader directory ~allow_developer_namespace ~checkpoint =
  let open Result.Let_syntax in
  let%bind names = Retention_reader.list_with_checkpoint reader ~directory ~checkpoint in
  let grouped = Hashtbl.create (module Id) in
  let temporary name =
    match Durable_file.temporary_target name with
    | None -> false
    | Some target ->
      (match String.chop_suffix target ~suffix:".sexp" with
       | None -> false
       | Some id -> Result.is_ok (Id.of_string id))
  in
  let%map () =
    List.fold_result names ~init:() ~f:(fun () name ->
      checkpoint ();
      match
        allow_developer_namespace && String.equal name "developer-runs", temporary name
      with
      | true, _ -> Ok ()
      | false, true -> Ok ()
      | false, false ->
        let%bind base, suffix =
          List.find_map [ ".sexp"; ".blob"; ".part" ] ~f:(fun suffix ->
            Option.map (String.chop_suffix name ~suffix) ~f:(fun base -> base, suffix))
          |> Result.of_option ~error:(Store_error.Corrupt "unknown retained blob file")
        in
        let%map id = Id.of_string base |> protocol in
        Hashtbl.update grouped id ~f:(fun previous ->
          Set.add (Option.value previous ~default:String.Set.empty) suffix))
  in
  Hashtbl.to_alist grouped
  |> List.sort ~compare:(fun (a, _) (b, _) ->
    checkpoint ();
    Id.compare a b)
;;

let charge_text ~checkpoint text =
  let rec loop offset =
    if offset < String.length text
    then (
      checkpoint ();
      loop (offset + Int.min 4096 (String.length text - offset)))
  in
  loop 0
;;

let digest_string ~checkpoint text =
  let rec loop state offset =
    if offset >= String.length text
    then Digestif.SHA256.(get state |> to_hex)
    else (
      checkpoint ();
      let length = Int.min 4096 (String.length text - offset) in
      let chunk = String.sub text ~pos:offset ~len:length in
      loop (Digestif.SHA256.feed_string state chunk) (offset + length))
  in
  loop Digestif.SHA256.empty 0
;;

let scan ~scope ~developer ~session ~reader ~checkpoint ~intents ~max_file_bytes =
  let open Result.Let_syntax in
  let%bind session_root, temporary_root =
    Blob_store.retention_directories scope session
  in
  let%bind () =
    match
      String.equal (Retention_reader.root reader) session_root && max_file_bytes >= 0
    with
    | true -> Ok ()
    | false -> corrupt "blob retention reader does not match its session or limits"
  in
  let%bind content_limits =
    Document_fields.limits ~max_bytes:max_file_bytes |> Document_fields.store
  in
  let%bind temporary_reader = Retention_reader.at_root reader ~root:temporary_root in
  let%bind () = Blob_store.Developer_retention.check_current developer scope in
  let%bind reserved = Blob_store.retention_reserved_directory scope in
  let%bind reserved_reader = Retention_reader.at_root reader ~root:reserved in
  let%bind reserved_entries =
    Retention_reader.list_with_checkpoint reserved_reader ~directory:"." ~checkpoint
  in
  let%bind () =
    match reserved_entries with
    | [] | [ "developer-runs" ] -> Ok ()
    | _ -> corrupt "unknown global durable blob consumer"
  in
  let%bind max_blob_bytes = Blob_store.retention_max_upload_bytes scope in
  let%bind developer_graph =
    match Blob_store.Developer_retention.inventory developer with
    | None -> Ok None
    | Some inventory ->
      let%map graph =
        Developer_result_inventory.Collection.scan
          ~inventory
          ~scope
          ~developer
          ~reader
          ~checkpoint
          ~max_result_bytes:
            (Int.min max_file_bytes (Retention_reader.remaining_bytes reader))
          ~max_blob_bytes
      in
      Some graph
  in
  let owned = Hashtbl.create (module Id) in
  let%bind () =
    List.fold_result intents ~init:() ~f:(fun () intent ->
      checkpoint ();
      let reference = Job_result_intent.reference intent in
      match
        P.Id.Session.equal reference.session_id (Session_store.Handle.session_id session)
        && not (Hashtbl.mem owned reference.blob.id)
      with
      | false -> corrupt "duplicate or foreign retained result intent"
      | true ->
        Hashtbl.add_exn owned ~key:reference.blob.id ~data:intent;
        Ok ())
  in
  let candidates = Hashtbl.keys owned in
  checkpoint ();
  let%bind scanner = Blob_reference_scan.create candidates |> protocol in
  let roots = Hash_set.create (module Id) in
  let edges = Hashtbl.create (module Id) in
  let complete = Hash_set.create (module Id) in
  let published = Hash_set.create (module Id) in
  let temporary_ids = Hash_set.create (module Id) in
  List.iter intents ~f:(fun intent ->
    checkpoint ();
    let id = (Job_result_intent.reference intent).blob.id in
    Blob_reference_scan.reset scanner;
    Job_result_intent.iter_reference_strings intent ~f:(fun text ->
      checkpoint ();
      Blob_reference_scan.begin_root scanner;
      let rec feed offset =
        if offset < String.length text
        then (
          checkpoint ();
          let length = Int.min 4096 (String.length text - offset) in
          Blob_reference_scan.feed
            ~ignore:id
            scanner
            (String.sub text ~pos:offset ~len:length);
          feed (offset + length))
      in
      feed 0);
    Hashtbl.set edges ~key:id ~data:(Blob_reference_scan.references scanner));
  let scan_directory reader directory ~temporary =
    let%bind groups =
      files reader directory ~allow_developer_namespace:temporary ~checkpoint
    in
    List.fold_result groups ~init:() ~f:(fun () (id, suffixes) ->
      checkpoint ();
      if temporary then Hash_set.add temporary_ids id;
      Blob_reference_scan.reset scanner;
      let intent = Hashtbl.find owned id in
      let ignored_id = Option.map intent ~f:(fun _ -> id) in
      let feed text =
        Blob_reference_scan.begin_root scanner;
        let rec chunks offset =
          if offset < String.length text
          then (
            checkpoint ();
            let length = Int.min 4096 (String.length text - offset) in
            Blob_reference_scan.feed
              ?ignore:ignored_id
              scanner
              (String.sub text ~pos:offset ~len:length);
            chunks (offset + length))
        in
        chunks 0
      in
      let read suffix =
        let name = Id.to_string id ^ suffix in
        let path =
          if String.equal directory "." then name else Filename.concat directory name
        in
        Retention_reader.read_with_checkpoint
          reader
          ~path
          ~max_bytes:max_file_bytes
          ~checkpoint
      in
      let%bind () =
        match Set.mem suffixes ".part", temporary, intent with
        | false, _, _ | true, true, Some _ -> Ok ()
        | _ -> corrupt "unowned or misplaced partial blob prevents retention proof"
      in
      let%bind stored_metadata =
        match Set.mem suffixes ".sexp" with
        | false ->
          (match intent with
           | Some intent ->
             Ok { (Job_result_intent.metadata intent) with durable = not temporary }
           | None -> corrupt "retained blob data has no metadata")
        | true ->
          let%bind bytes = read ".sexp" in
          charge_text ~checkpoint bytes;
          let%bind value, document = metadata ~id bytes in
          let%map () =
            match intent with
            | None -> Ok ()
            | Some intent ->
              let stage = Job_result_intent.stage intent in
              let expected =
                if temporary
                then Blob_stage_documents.temporary_bytes stage
                else Blob_stage_documents.durable_bytes stage
              in
              if String.equal bytes expected
              then Ok ()
              else
                corrupt "retained metadata bytes differ from selected private publication"
          in
          feed bytes;
          Document_fields.iter_strings_with_checkpoint
            (Document_schema.Document.json document)
            ~checkpoint
            ~f:feed;
          value
      in
      let%bind () =
        match
          Id.equal stored_metadata.blob.id id
          && Bool.equal stored_metadata.durable (not temporary)
          &&
          match stored_metadata.target with
          | Metadata.Target.Developer _ -> false
          | Unassigned -> temporary
          | Session target ->
            temporary
            || P.Id.Session.equal target (Session_store.Handle.session_id session)
        with
        | false -> corrupt "retained blob location or identity differs from metadata"
        | true ->
          (match intent with
           | None -> Ok ()
           | Some intent ->
             let expected =
               { (Job_result_intent.metadata intent) with durable = not temporary }
             in
             (match Metadata.equal expected stored_metadata with
              | true -> Ok ()
              | false ->
                corrupt "retained result metadata differs from its private intent"))
      in
      let%bind () =
        match intent, Set.mem suffixes ".blob" with
        | None, false -> corrupt "retained blob metadata has no complete data"
        | _ -> Ok ()
      in
      let%bind () =
        List.fold_result [ ".blob"; ".part" ] ~init:() ~f:(fun () suffix ->
          checkpoint ();
          match Set.mem suffixes suffix with
          | false -> Ok ()
          | true ->
            let%bind contents = read suffix in
            let length = Int64.of_int (String.length contents) in
            (match
               String.equal suffix ".part"
               && Int64.(length < stored_metadata.blob.byte_length)
             with
             | true -> Ok ()
             | false ->
               let%bind () =
                 match
                   Int64.equal length stored_metadata.blob.byte_length
                   && String.equal
                        stored_metadata.blob.digest
                        (digest_string ~checkpoint contents)
                 with
                 | true -> Ok ()
                 | false -> corrupt "retained blob length or digest mismatch"
               in
               feed contents;
               charge_text ~checkpoint contents;
               (match Document_schema.Json.decode ~limits:content_limits contents with
                | Error (Document_schema.Error.Malformed _)
                  when not (json_media_type stored_metadata.blob.media_type) -> Ok ()
                | Error error -> Error (Store_error.Document error)
                | Ok json ->
                  Document_fields.iter_strings_with_checkpoint json ~checkpoint ~f:feed;
                  (match intent with
                   | None -> Ok ()
                   | Some intent ->
                     let%bind completion = P.Completion.of_json json |> protocol in
                     let%map _ =
                       P.Stored_completion.artifact
                         (Job_result_intent.reference intent)
                         completion
                       |> protocol
                     in
                     Hash_set.add complete id;
                     if
                       (not temporary)
                       && String.equal suffix ".blob"
                       && Set.mem suffixes ".sexp"
                     then Hash_set.add published id))))
      in
      let found = Blob_reference_scan.references scanner in
      (match intent with
       | None -> List.iter found ~f:(Hash_set.add roots)
       | Some _ ->
         Hashtbl.update edges id ~f:(fun prior -> found @ Option.value prior ~default:[]));
      Ok ())
  in
  let%bind () = scan_directory reader "blobs" ~temporary:false in
  checkpoint ();
  Blob_reference_scan.reset scanner;
  let%bind () =
    match developer_graph with
    | None -> Ok ()
    | Some developer ->
      Developer_result_inventory.Collection.feed_candidate_references
        developer
        ~scanner
        ~checkpoint
  in
  List.iter (Blob_reference_scan.references scanner) ~f:(Hash_set.add roots);
  let%map () = scan_directory temporary_reader "." ~temporary:true in
  { candidates
  ; roots = Hash_set.to_list roots
  ; edges = Hashtbl.to_alist edges
  ; complete = Hash_set.to_list complete
  ; published = Hash_set.to_list published
  ; temporary = Hash_set.to_list temporary_ids
  ; developer = developer_graph
  }
;;

let published t id =
  List.mem t.published id ~equal:Id.equal && not (List.mem t.temporary id ~equal:Id.equal)
;;

let references t ~checkpoint ~roots =
  let candidates = Hash_set.of_list (module Id) t.candidates in
  let edges = Hashtbl.of_alist_exn (module Id) t.edges in
  let visited = Hash_set.create (module Id) in
  let complete = Hash_set.of_list (module Id) t.complete in
  let rec visit = function
    | [] -> Ok ()
    | id :: rest when (not (Hash_set.mem candidates id)) || Hash_set.mem visited id ->
      checkpoint ();
      visit rest
    | id :: _ when not (Hash_set.mem complete id) ->
      checkpoint ();
      corrupt "reachable result artifact has no complete verified dependency source"
    | id :: rest ->
      checkpoint ();
      Hash_set.add visited id;
      visit (Option.value (Hashtbl.find edges id) ~default:[] @ rest)
  in
  Result.map
    (visit (t.roots @ roots))
    ~f:(fun () -> Hash_set.to_list visited |> List.sort ~compare:Id.compare)
;;

let retire_published_developer_markers t ~scope ~developer ~reader ~checkpoint =
  let open Result.Let_syntax in
  let%bind () = Blob_store.Developer_retention.check_current developer scope in
  match t.developer, Blob_store.Developer_retention.inventory developer with
  | None, None -> Ok 0
  | Some graph, Some inventory ->
    Developer_result_inventory.Collection.retire_published_markers
      graph
      ~inventory
      ~reader
      ~checkpoint
  | None, Some _ | Some _, None -> corrupt "developer graph and retention borrow disagree"
;;
