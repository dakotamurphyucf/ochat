open Core
module P = Agent_protocol
module Store = Agent_store
module Reader = Store.Retention_reader
module Scan = Store.Blob_reference_scan
module Segment = Store.Journal_segment

let corrupt message = Error (Store.Store_error.Corrupt message)

let protocol result =
  Result.map_error result ~f:(fun failure ->
    Store.Store_error.Corrupt failure.P.Error.message)
;;

let temporary name = String.is_substring name ~substring:".tmp-"

let scan
      ~reader
      ~handle
      ~(state : Session_state.t)
      ~journal_current
      ~transaction_hash
      ~max_file_bytes
      ~max_frame_bytes
      ~candidates
  =
  let open Result.Let_syntax in
  let%bind () = Session_state.validate state |> protocol in
  let session_id = Store.Session_store.Handle.session_id handle in
  let%bind () =
    match P.Id.Session.equal session_id state.identity.session_id with
    | true -> Ok ()
    | false -> corrupt "retention state belongs to another session"
  in
  let%bind scan = Scan.create candidates |> protocol in
  let feed bytes =
    Scan.begin_root scan;
    Scan.feed scan bytes
  in
  let archives = ref String.Map.empty in
  let remember reference =
    let name = Compaction_archive.filename reference in
    match Map.find !archives name with
    | None ->
      archives := Map.set !archives ~key:name ~data:reference;
      Ok ()
    | Some previous ->
      (match
         Sexp.equal
           (Session_state.Compaction_archive.sexp_of_t previous)
           (Session_state.Compaction_archive.sexp_of_t reference)
       with
       | true -> Ok ()
       | false -> corrupt "retained archive references disagree")
  in
  let observe_state state =
    let%bind () =
      match P.Id.Session.equal state.Session_state.identity.session_id session_id with
      | true -> Ok ()
      | false -> corrupt "retained state belongs to another session"
    in
    let encoded = Session_state.sexp_of_t state |> Sexp.to_string_mach in
    let%bind () =
      match String.length encoded <= max_file_bytes with
      | true ->
        feed encoded;
        Ok ()
      | false -> corrupt "decoded retention state exceeds its byte limit"
    in
    List.fold_result
      state.conversation.compaction_archives
      ~init:()
      ~f:(fun () reference -> remember reference)
  in
  let%bind () = observe_state state in
  let%bind names = Reader.list reader ~directory:"snapshot" in
  let checkpoint_states = ref [] in
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
        let%bind snapshot =
          Store.Snapshot.decode_file ~max_payload_length:max_frame_bytes bytes
        in
        let%bind restored = Session_persistence.restore_snapshot snapshot.payload in
        let%bind () =
          match
            String.equal
              name
              (sprintf "snapshot-%016Ld.bin" snapshot.transaction_sequence)
            && Int64.equal
                 restored.counters.transaction_sequence
                 snapshot.transaction_sequence
            && Int64.equal restored.counters.event_sequence snapshot.event_sequence
            && String.equal
                 snapshot.prompt_artifact
                 (P.Id.Prompt_revision.to_string restored.spec.prompt_revision_id)
            && String.equal
                 snapshot.workspace_identity
                 restored.spec.workspace_instance.conflict_domain
            && ((not (Int64.equal snapshot.transaction_sequence 0L))
                || Option.is_none snapshot.transaction_hash)
          with
          | true -> Ok ()
          | false -> corrupt "retained snapshot metadata disagrees with its state"
        in
        let%map () = observe_state restored in
        checkpoint_states := restored :: !checkpoint_states;
        feed bytes;
        { Store.Snapshot.filename = name; snapshot } :: snapshots
      | _ -> corrupt "unknown file in retained snapshot directory")
  in
  let%bind () =
    match List.mem names "CURRENT" ~equal:String.equal, snapshots with
    | false, [] -> Ok ()
    | false, _ -> corrupt "retained snapshots have no current pointer"
    | true, _ ->
      let%bind pointer = Reader.read reader ~path:"snapshot/CURRENT" ~max_bytes:256 in
      (match
         List.exists snapshots ~f:(fun snapshot ->
           String.equal snapshot.Store.Snapshot.filename (String.strip pointer))
       with
       | true -> Ok ()
       | false -> corrupt "snapshot pointer refers to a missing retained snapshot")
  in
  let%bind names = Reader.list reader ~directory:"journal" in
  let%bind pointer = Reader.read reader ~path:"journal/CURRENT" ~max_bytes:256 in
  let%bind () =
    match String.equal (String.strip pointer) (Segment.Id.filename journal_current) with
    | true -> Ok ()
    | false -> corrupt "retained journal pointer differs from the live writer"
  in
  let%bind segments =
    List.fold_result names ~init:[] ~f:(fun segments name ->
      match name with
      | "CURRENT" -> Ok segments
      | name when temporary name -> Ok segments
      | name ->
        let%bind id = Segment.Id.of_filename name in
        (match String.equal name (Segment.Id.filename id) with
         | true -> Ok ((id, name) :: segments)
         | false -> corrupt "retained journal segment has a noncanonical name"))
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
          (match Segment.Id.equal id next with
           | true -> Ok ()
           | false -> corrupt "retained journal segment sequence has a gap")
      in
      Some id)
  in
  let%bind () =
    match List.last segments with
    | Some (id, _) when Segment.Id.equal id journal_current -> Ok ()
    | _ -> corrupt "retained journal does not end at its current segment"
  in
  let rec observe_delta = function
    | Session_delta.Batch deltas ->
      List.fold_result deltas ~init:() ~f:(fun () delta -> observe_delta delta)
    | Created state ->
      let%bind state = Session_state.upgrade_schema state |> protocol in
      let%bind () = Session_state.validate state |> protocol in
      observe_state state
    | Compaction_archived reference -> remember reference
    | _ -> Ok ()
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
      let%map transactions =
        List.fold_result segment.entries ~init:transactions ~f:(fun transactions entry ->
          match Store.Frame.flags entry.Segment.frame with
          | 1 -> Ok transactions
          | 0 ->
            let%bind transaction =
              Store.Transaction.decode (Store.Frame.payload entry.frame)
            in
            let%bind delta =
              Result.try_with (fun () ->
                Sexp.of_string transaction.delta |> Session_delta.t_of_sexp)
              |> Result.map_error ~f:(fun _ ->
                Store.Store_error.Corrupt "invalid retained session delta")
            in
            let%bind () = observe_delta delta in
            let%map events = Session_persistence.durable_events transaction in
            feed (Session_delta.sexp_of_t delta |> Sexp.to_string_mach);
            List.iter events ~f:(fun event ->
              feed (P.Event.Durable.sexp_of_t event |> Sexp.to_string_mach));
            transaction :: transactions
          | _ -> corrupt "unknown retained journal frame kind")
      in
      feed bytes;
      transactions)
  in
  let transactions = List.rev transactions in
  let%bind () =
    List.fold_result !checkpoint_states ~init:() ~f:(fun () checkpoint ->
      match checkpoint.Session_state.counters.transaction_sequence with
      | 0L ->
        (match Int64.equal checkpoint.counters.revision 0L with
         | true -> Ok ()
         | false -> corrupt "initial checkpoint has a noninitial revision")
      | sequence ->
        (match
           List.find transactions ~f:(fun transaction ->
             Int64.equal transaction.Store.Transaction.transaction_sequence sequence)
         with
         | Some transaction
           when Int64.equal checkpoint.counters.revision transaction.session_revision
                && Int.equal checkpoint.identity.generation transaction.generation ->
           Ok ()
         | _ -> corrupt "checkpoint state disagrees with its journal anchor"))
  in
  let%bind head = Store.Recovery.validate_retained ~session_id ~snapshots ~transactions in
  let%bind () =
    match
      Int64.equal head.transaction_sequence state.counters.transaction_sequence
      && Option.equal String.equal head.transaction_hash transaction_hash
      && Int64.equal head.session_revision state.counters.revision
      && Int64.equal head.event_sequence state.counters.event_sequence
      && Int.equal head.generation state.identity.generation
    with
    | true -> Ok ()
    | false -> corrupt "retained journal head differs from the actor checkpoint"
  in
  let%bind names = Reader.list reader ~directory:"archive" in
  let%bind contents =
    List.fold_result names ~init:String.Map.empty ~f:(fun contents name ->
      match temporary name with
      | true -> Ok contents
      | false ->
        let%bind operation =
          List.find_map [ "compaction"; "reset"; "rebuild"; "upgrade" ] ~f:(fun prefix ->
            String.chop_prefix name ~prefix:(prefix ^ "-"))
          |> Result.of_option
               ~error:(Store.Store_error.Corrupt "unknown retained archive filename")
        in
        let%bind operation =
          String.chop_suffix operation ~suffix:".frame"
          |> Result.of_option
               ~error:(Store.Store_error.Corrupt "invalid retained archive filename")
        in
        let%bind _ = P.Id.Operation.of_string operation |> protocol in
        let%bind bytes =
          Reader.read
            reader
            ~path:(Filename.concat "archive" name)
            ~max_bytes:max_file_bytes
        in
        let%bind payload =
          match
            Store.Frame.decode
              ~max_payload_length:max_frame_bytes
              ~contents:bytes
              ~offset:0
          with
          | Ok (Complete { frame; next_offset })
            when next_offset = String.length bytes && Store.Frame.flags frame = 0 ->
            Ok (Store.Frame.payload frame)
          | _ -> corrupt "invalid retained archive frame"
        in
        let%bind archived = Session_persistence.restore_snapshot payload in
        let%bind () = observe_state archived in
        feed bytes;
        Ok (Map.set contents ~key:name ~data:bytes))
  in
  let%bind () =
    Map.fold !archives ~init:(Ok ()) ~f:(fun ~key:name ~data:reference checked ->
      let%bind () = checked in
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
