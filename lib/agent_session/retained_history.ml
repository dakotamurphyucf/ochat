open! Core
module P = Agent_protocol
module Store = Agent_store
module D = Document_schema
module Reader = Store.Retention_reader
module Scan = Store.Blob_reference_scan
module Segment = Store.Journal_segment

let corrupt message = Error (Store.Store_error.Corrupt message)

let protocol result =
  Result.map_error result ~f:(fun e -> Store.Store_error.Corrupt e.P.Error.message)
;;

let doc result = Result.map_error result ~f:(fun e -> Store.Store_error.Document e)
let temporary name = String.is_substring name ~substring:".tmp-"

let scan
      ~reader
      ~handle
      ~state
      ~journal_current
      ~transaction_hash
      ~max_file_bytes
      ~max_frame_bytes
      ~candidates
  =
  let open Result.Let_syntax in
  let%bind limits = Persistence_codec.limits ~max_bytes:max_frame_bytes |> doc in
  let value = Session_state_document.value state in
  let session_id = Store.Session_store.Handle.session_id handle in
  let%bind () = Session_state.validate value |> protocol in
  let%bind () =
    if P.Id.Session.equal session_id value.identity.session_id
    then Ok ()
    else corrupt "retention state belongs to another session"
  in
  let%bind scan = Scan.create candidates |> protocol in
  let feed bytes =
    Scan.begin_root scan;
    Scan.feed scan bytes
  in
  let feed_document document =
    Store.Document_fields.iter_strings (D.Document.json document) ~f:feed
  in
  let archives = ref String.Map.empty in
  let remember reference =
    let name = Compaction_archive.filename reference in
    match Map.find !archives name with
    | None ->
      archives := Map.set !archives ~key:name ~data:reference;
      Ok ()
    | Some previous ->
      if
        D.Json.equal
          (Session_state_document.archive_reference_to_jsonaf previous)
          (Session_state_document.archive_reference_to_jsonaf reference)
      then Ok ()
      else corrupt "retained archive references disagree"
  in
  let observe_state state =
    let value = Session_state_document.value state in
    let%bind () =
      if P.Id.Session.equal session_id value.identity.session_id
      then Ok ()
      else corrupt "retained state belongs to another session"
    in
    let%bind document = Session_state_document.encode state ~limits |> doc in
    feed_document document;
    List.fold_result
      value.conversation.compaction_archives
      ~init:()
      ~f:(fun () reference -> remember reference)
  in
  let%bind () = observe_state state in
  (* Inspect exact stored metadata and links for every fallback before current
    domain restoration. No reference-absence decision precedes this preflight. *)
  let%bind names = Reader.list reader ~directory:"snapshot" in
  let%bind snapshots =
    List.fold_result names ~init:[] ~f:(fun snapshots name ->
      match name with
      | "CURRENT" -> Ok snapshots
      | name when temporary name -> Ok snapshots
      | name
        when String.is_prefix name ~prefix:"snapshot-"
             && String.is_suffix name ~suffix:".bin" ->
        let%bind bytes =
          Reader.read
            reader
            ~path:(Filename.concat "snapshot" name)
            ~max_bytes:max_file_bytes
        in
        let%bind stored =
          Store.Snapshot.decode_stored_file ~max_payload_length:max_frame_bytes bytes
        in
        let metadata = Store.Snapshot.Stored.metadata stored in
        let%bind () =
          if
            String.equal
              name
              (sprintf "snapshot-%016Ld.bin" metadata.transaction_sequence)
          then Ok ()
          else corrupt "noncanonical retained snapshot filename"
        in
        feed bytes;
        Ok ({ Store.Snapshot.filename = name; stored } :: snapshots)
      | _ -> corrupt "unknown retained snapshot file")
  in
  let%bind () =
    match List.mem names "CURRENT" ~equal:String.equal, snapshots with
    | false, [] -> Ok ()
    | false, _ -> corrupt "retained snapshots have no pointer"
    | true, _ ->
      let%bind pointer = Reader.read reader ~path:"snapshot/CURRENT" ~max_bytes:256 in
      if
        List.exists snapshots ~f:(fun snapshot ->
          String.equal snapshot.Store.Snapshot.filename (String.strip pointer))
      then Ok ()
      else corrupt "retained snapshot pointer is missing"
  in
  let%bind names = Reader.list reader ~directory:"journal" in
  let%bind pointer = Reader.read reader ~path:"journal/CURRENT" ~max_bytes:256 in
  let%bind () =
    if String.equal (String.strip pointer) (Segment.Id.filename journal_current)
    then Ok ()
    else corrupt "journal pointer differs from live writer"
  in
  let%bind segments =
    List.fold_result names ~init:[] ~f:(fun segments name ->
      if String.equal name "CURRENT" || temporary name
      then Ok segments
      else (
        let%bind id = Segment.Id.of_filename name in
        if String.equal name (Segment.Id.filename id)
        then Ok ((id, name) :: segments)
        else corrupt "noncanonical retained segment filename"))
  in
  let segments =
    List.sort segments ~compare:(fun (a, _) (b, _) -> Segment.Id.compare a b)
  in
  let%bind _ =
    List.fold_result segments ~init:None ~f:(fun previous (id, _) ->
      let%map () =
        match previous with
        | None -> Ok ()
        | Some previous ->
          let%bind next = Segment.Id.next previous in
          if Segment.Id.equal id next
          then Ok ()
          else corrupt "retained journal segment gap"
      in
      Some id)
  in
  let%bind () =
    match List.last segments with
    | Some (id, _) when Segment.Id.equal id journal_current -> Ok ()
    | _ -> corrupt "retained journal lacks its current segment"
  in
  let%bind transactions =
    List.fold_result segments ~init:[] ~f:(fun transactions (_, name) ->
      let%bind bytes =
        Reader.read
          reader
          ~path:(Filename.concat "journal" name)
          ~max_bytes:max_file_bytes
      in
      let%bind segment =
        Segment.scan_contents ~max_payload_length:max_frame_bytes bytes
      in
      let%bind () =
        if segment.crash_tail
        then corrupt "incomplete retained journal prevents collection"
        else Ok ()
      in
      feed bytes;
      List.fold_result segment.entries ~init:transactions ~f:(fun transactions entry ->
        match Store.Frame.flags entry.Segment.frame with
        | 1 -> Ok transactions
        | 0 ->
          let%bind record =
            Store.Document_record.of_frame entry.frame ~limits ~expected_digest:None
            |> Result.map_error ~f:Store.Document_fields.record_error
          in
          let%map stored = Store.Transaction.Stored.of_record record in
          stored :: transactions
        | _ -> corrupt "unknown retained frame flags"))
  in
  let transactions = List.rev transactions in
  let%bind head =
    Store.Recovery.validate_stored_retained ~session_id ~snapshots ~transactions
  in
  let%bind () =
    if
      Int64.equal head.transaction_sequence value.counters.transaction_sequence
      && Option.equal String.equal head.transaction_hash transaction_hash
      && Int64.equal head.session_revision value.counters.revision
      && Int64.equal head.event_sequence value.counters.event_sequence
      && Int.equal head.generation value.identity.generation
    then Ok ()
    else corrupt "retained head differs from actor checkpoint"
  in
  let%bind transactions =
    List.map transactions ~f:(fun stored ->
      feed_document
        (Store.Document_record.document (Store.Transaction.Stored.record stored));
      let%bind transaction = Store.Transaction.restore stored ~limits in
      let%map () = Session_persistence.validate_transaction ~limits transaction in
      transaction)
    |> Result.all
  in
  let rec observe_delta = function
    | Session_delta.Batch changes ->
      List.fold_result changes ~init:() ~f:(fun () change -> observe_delta change)
    | Created state ->
      List.fold_result
        state.conversation.compaction_archives
        ~init:()
        ~f:(fun () reference -> remember reference)
    | Compaction_archived reference -> remember reference
    | _ -> Ok ()
  in
  let%bind () =
    List.fold_result transactions ~init:() ~f:(fun () transaction ->
      let%bind delta =
        Session_delta_document.decode ~limits transaction.Store.Transaction.delta |> doc
      in
      observe_delta (Session_delta_document.value delta))
  in
  let%bind () =
    List.fold_result snapshots ~init:() ~f:(fun () installed ->
      feed_document
        (Store.Document_record.document
           (Store.Snapshot.Stored.record installed.Store.Snapshot.stored));
      let%bind snapshot = Store.Snapshot.restore installed.stored ~limits in
      let%bind restored = Session_persistence.restore_snapshot ~limits snapshot in
      let%bind () =
        observe_state (Session_persistence.Restored.state_document restored)
      in
      let after =
        List.filter transactions ~f:(fun transaction ->
          Int64.(
            transaction.Store.Transaction.transaction_sequence
            > snapshot.transaction_sequence))
      in
      let%bind final =
        List.fold_result
          after
          ~init:restored
          ~f:(Session_persistence.apply_transaction ~limits)
      in
      let%bind () = Session_persistence.validate final in
      observe_state (Session_persistence.Restored.state_document final))
  in
  let%bind names = Reader.list reader ~directory:"archive" in
  let%bind contents =
    List.fold_result names ~init:String.Map.empty ~f:(fun contents name ->
      if temporary name
      then Ok contents
      else (
        let%bind operation =
          List.find_map [ "compaction"; "reset"; "rebuild"; "upgrade" ] ~f:(fun prefix ->
            String.chop_prefix name ~prefix:(prefix ^ "-"))
          |> Result.of_option
               ~error:(Store.Store_error.Corrupt "unknown archive filename")
        in
        let%bind operation =
          String.chop_suffix operation ~suffix:".frame"
          |> Result.of_option
               ~error:(Store.Store_error.Corrupt "invalid archive filename")
        in
        let%bind _ = P.Id.Operation.of_string operation |> protocol in
        let%bind bytes =
          Reader.read
            reader
            ~path:(Filename.concat "archive" name)
            ~max_bytes:max_file_bytes
        in
        let%bind record, state =
          Compaction_archive.decode_record
            ~max_payload_length:max_frame_bytes
            ~expected_digest:None
            bytes
          |> protocol
        in
        feed_document (Store.Document_record.document record);
        let%bind () = observe_state state in
        feed bytes;
        Ok (Map.set contents ~key:name ~data:bytes)))
  in
  let%bind () =
    Map.fold !archives ~init:(Ok ()) ~f:(fun ~key:name ~data:reference result ->
      let%bind () = result in
      let%bind bytes =
        Map.find contents name
        |> Result.of_option
             ~error:(Store.Store_error.Corrupt "referenced archive is missing")
      in
      Compaction_archive.decode_file
        ~handle
        ~max_payload_length:max_frame_bytes
        reference
        bytes
      |> protocol
      |> Result.map ~f:ignore)
  in
  Ok (Scan.references scan)
;;
