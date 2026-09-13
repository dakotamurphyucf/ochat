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
  }

let corrupt message = Error (Store_error.Corrupt message)

let protocol result =
  Result.map_error result ~f:(fun error -> Store_error.Corrupt error.P.Error.message)
;;

let metadata contents =
  let open Result.Let_syntax in
  let%bind value =
    Result.try_with (fun () -> Sexp.of_string contents |> Metadata.t_of_sexp)
    |> Result.map_error ~f:(fun _ -> Store_error.Corrupt "invalid retained blob metadata")
  in
  let%bind _ = P.Blob.Metadata.of_json (P.Blob.Metadata.to_json value.blob) |> protocol in
  let%bind _ =
    P.Id.Principal.of_json (P.Id.Principal.to_json value.creating_principal) |> protocol
  in
  let%bind () =
    Option.value_map value.target_session ~default:(Ok ()) ~f:(fun id ->
      P.Id.Session.of_json (P.Id.Session.to_json id) |> protocol |> Result.map ~f:ignore)
  in
  match
    Option.for_all value.expires_at ~f:(fun at ->
      P.Timestamp.compare at value.created_at >= 0)
  with
  | true -> Ok value
  | false -> corrupt "retained blob expiry precedes creation"
;;

let json_media_type value =
  let value =
    String.split value ~on:';' |> List.hd_exn |> String.strip |> String.lowercase
  in
  String.equal value "application/json" || String.is_suffix value ~suffix:"+json"
;;

let files reader directory =
  let open Result.Let_syntax in
  let%bind names = Retention_reader.list reader ~directory in
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
      match temporary name with
      | true -> Ok ()
      | false ->
        let%bind base, suffix =
          List.find_map [ ".sexp"; ".blob"; ".part" ] ~f:(fun suffix ->
            Option.map (String.chop_suffix name ~suffix) ~f:(fun base -> base, suffix))
          |> Result.of_option ~error:(Store_error.Corrupt "unknown retained blob file")
        in
        let%map id = Id.of_string base |> protocol in
        Hashtbl.update grouped id ~f:(fun previous ->
          Set.add (Option.value previous ~default:String.Set.empty) suffix))
  in
  Hashtbl.to_alist grouped |> List.sort ~compare:(fun (a, _) (b, _) -> Id.compare a b)
;;

let scan ~scope ~session ~reader ~intents ~max_file_bytes =
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
  let%bind temporary_reader = Retention_reader.at_root reader ~root:temporary_root in
  let%bind reserved_root = Blob_store.retention_reserved_directory scope in
  let%bind reserved_reader = Retention_reader.at_root reader ~root:reserved_root in
  let%bind reserved = Retention_reader.list reserved_reader ~directory:"." in
  let%bind () =
    match reserved with
    | [] -> Ok ()
    | _ -> corrupt "unknown global durable blob consumers prevent retention proof"
  in
  let owned = Hashtbl.create (module Id) in
  let%bind () =
    List.fold_result intents ~init:() ~f:(fun () intent ->
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
  let%bind scanner = Blob_reference_scan.create candidates |> protocol in
  let roots = Hash_set.create (module Id) in
  let edges = Hashtbl.create (module Id) in
  let complete = Hash_set.create (module Id) in
  let published = Hash_set.create (module Id) in
  let temporary_ids = Hash_set.create (module Id) in
  let scan_directory reader directory ~temporary =
    let%bind groups = files reader directory in
    List.fold_result groups ~init:() ~f:(fun () (id, suffixes) ->
      if temporary then Hash_set.add temporary_ids id;
      Blob_reference_scan.reset scanner;
      let intent = Hashtbl.find owned id in
      let ignored_id = Option.map intent ~f:(fun _ -> id) in
      let feed text =
        Blob_reference_scan.begin_root scanner;
        Blob_reference_scan.feed ?ignore:ignored_id scanner text
      in
      let read suffix =
        let name = Id.to_string id ^ suffix in
        let path =
          if String.equal directory "." then name else Filename.concat directory name
        in
        Retention_reader.read reader ~path ~max_bytes:max_file_bytes
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
          let%map value = metadata bytes in
          feed bytes;
          feed (Metadata.sexp_of_t value |> Sexp.to_string_mach);
          value
      in
      let%bind () =
        match
          Id.equal stored_metadata.blob.id id
          && Bool.equal stored_metadata.durable (not temporary)
          && (temporary
              || Option.exists
                   stored_metadata.target_session
                   ~f:(P.Id.Session.equal (Session_store.Handle.session_id session)))
        with
        | false -> corrupt "retained blob location or identity differs from metadata"
        | true ->
          (match intent with
           | None -> Ok ()
           | Some intent ->
             let expected =
               { (Job_result_intent.metadata intent) with durable = not temporary }
             in
             (match
                Sexp.equal
                  (Metadata.sexp_of_t expected)
                  (Metadata.sexp_of_t stored_metadata)
              with
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
                        Digestif.SHA256.(digest_string contents |> to_hex)
                 with
                 | true -> Ok ()
                 | false -> corrupt "retained blob length or digest mismatch"
               in
               feed contents;
               (match Result.try_with (fun () -> Jsonaf.of_string contents) with
                | Error _ ->
                  (match json_media_type stored_metadata.blob.media_type with
                   | true -> corrupt "retained JSON blob is malformed"
                   | false -> Ok ())
                | Ok json ->
                  feed (Jsonaf.to_string json);
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
  let%map () = scan_directory temporary_reader "." ~temporary:true in
  { candidates
  ; roots = Hash_set.to_list roots
  ; edges = Hashtbl.to_alist edges
  ; complete = Hash_set.to_list complete
  ; published = Hash_set.to_list published
  ; temporary = Hash_set.to_list temporary_ids
  }
;;

let published t id =
  List.mem t.published id ~equal:Id.equal && not (List.mem t.temporary id ~equal:Id.equal)
;;

let references t ~roots =
  let candidates = Hash_set.of_list (module Id) t.candidates in
  let edges = Hashtbl.of_alist_exn (module Id) t.edges in
  let visited = Hash_set.create (module Id) in
  let complete = Hash_set.of_list (module Id) t.complete in
  let rec visit = function
    | [] -> Ok ()
    | id :: rest when (not (Hash_set.mem candidates id)) || Hash_set.mem visited id ->
      visit rest
    | id :: _ when not (Hash_set.mem complete id) ->
      corrupt "reachable result artifact has no complete verified dependency source"
    | id :: rest ->
      Hash_set.add visited id;
      visit (Option.value (Hashtbl.find edges id) ~default:[] @ rest)
  in
  Result.map
    (visit (t.roots @ roots))
    ~f:(fun () -> Hash_set.to_list visited |> List.sort ~compare:Id.compare)
;;
