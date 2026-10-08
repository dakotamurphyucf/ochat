open Core
open Fixtures
module P = Agent_protocol
module S = Agent_store
module State = Agent_session.Session_state
module Persistence = Agent_session.Session_persistence
module Archive = Agent_session.Compaction_archive

let frame_ok = function
  | Ok value -> value
  | Error failure -> raise_s [%sexp (failure : S.Frame.error)]
;;

let reference_checkpoint value =
  Agent_session.Moderator_checkpoint.encode
    { (handoff_snapshot 0) with current_state = Session.Snapshot.String value }
;;

let with_history f =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let snapshot_id = P.Id.Blob.create ()
      and journal_id = P.Id.Blob.create ()
      and archive_id = P.Id.Blob.create ()
      and absent_id = P.Id.Blob.create () in
      let initial =
        actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
      in
      let initial =
        { initial with
          moderator = Some (reference_checkpoint (P.Id.Blob.to_string snapshot_id))
        }
      in
      let storage = Job_artifact_fixtures.create env sw initial in
      let handle = storage.session in
      let journal =
        S.Journal.create
          ~env
          ~directory:(S.Session_store.Handle.journal_directory handle)
          ~max_payload_length:1048576
          ~max_segment_bytes:4194304L
          ~max_segment_frames:1
        |> store_ok
      in
      let writer =
        S.Commit_writer.create
          ~sw
          ~journal
          ~session_id:initial.identity.session_id
          ~next_transaction_sequence:1L
          ~previous_transaction_hash:None
          ~queue_capacity:8
        |> store_ok
      in
      Exn.protect
        ~finally:(fun () -> S.Commit_writer.close writer)
        ~f:(fun () ->
          let persistence =
            Persistence.create
              ~retention_preflight:None
              ~writer
              ~durability:Flush
              ~previous_transaction_hash:None
              ~command_accepted:(fun _ _ -> ())
              ~limits:document_limits
              ~archive_limits:document_limits
              ~restored:(Persistence.Restored.authored initial)
              ~archive:(Archive.write_document ~env ~handle ~max_payload_length:1048576)
          in
          let state = ref initial in
          let commit delta =
            let transition =
              Agent_session.Session_transition.apply
                ~now:timestamp
                !state
                ~delta
                ~payloads:[]
              |> protocol_ok
            in
            Persistence.commit persistence ~command_audit:None ~previous:!state transition
            |> protocol_ok;
            state := transition.state
          in
          let snapshot () =
            Persistence.install_snapshot
              persistence
              ~env
              ~handle
              ~max_payload_length:1048576
              ~transaction_hash:(Persistence.transaction_hash persistence)
              !state
            |> store_ok
          in
          let fallback = snapshot () in
          (* Equivalent JSON escaped strings still count after generic decoding. *)
          let id = P.Id.Blob.to_string snapshot_id in
          let escaped =
            sprintf "\"\\u%04x%s\"" (Char.to_int id.[0]) (String.drop_prefix id 1)
          in
          let directory = S.Session_store.Handle.snapshot_directory handle in
          let path =
            Eio.Path.(Eio.Stdenv.fs env / Filename.concat directory fallback.filename)
          in
          let contents = Eio.Path.load path in
          let payload =
            match
              S.Frame.decode ~max_payload_length:1048576 ~contents ~offset:0 |> frame_ok
            with
            | Complete { frame; _ } -> S.Frame.payload frame
            | _ -> assert false
          in
          let payload =
            String.substr_replace_all
              payload
              ~pattern:(sprintf "\"%s\"" id)
              ~with_:escaped
          in
          let contents =
            S.Frame.encode ~max_payload_length:1048576 ~flags:0 payload |> frame_ok
          in
          Eio.Path.save ~create:(`Or_truncate 0o600) path contents;
          commit
            (Moderator_changed
               (Some (reference_checkpoint (P.Id.Blob.to_string journal_id))));
          commit (Moderator_changed None);
          ignore (snapshot () : S.Snapshot.installed);
          (* A fully written archive can outlive a rejected reference publication. *)
          let orphan_state =
            { !state with
              moderator = Some (reference_checkpoint (P.Id.Blob.to_string archive_id))
            }
          in
          let orphan_document =
            Agent_session.Session_state_document.authored orphan_state
          in
          let orphan =
            Archive.reference
              orphan_document
              ~limits:document_limits
              (P.Id.Operation.create ())
            |> protocol_ok
          in
          Archive.write ~env ~handle ~max_payload_length:1048576 orphan orphan_document
          |> protocol_ok;
          let reference =
            Archive.reference
              (Agent_session.Session_state_document.authored !state)
              ~limits:document_limits
              (P.Id.Operation.create ())
            |> protocol_ok
          in
          commit (Compaction_archived reference);
          let current = snapshot () in
          let scan
                ?(max_bytes = 16777216)
                ?(transaction_hash = Persistence.transaction_hash persistence)
                ()
            =
            let reader =
              S.Retention_reader.create
                ~env
                ~root:(S.Session_store.Handle.directory handle)
                ~max_entries:128
                ~max_bytes
              |> store_ok
            in
            Agent_session.Retained_history.scan
              ~reader
              ~handle
              ~state:
                (Persistence.Restored.state_document (Persistence.restored persistence))
              ~journal_current:(S.Journal.current_segment journal)
              ~transaction_hash
              ~max_file_bytes:4194304
              ~max_frame_bytes:1048576
              ~candidates:[ snapshot_id; journal_id; archive_id; absent_id ]
          in
          f
            env
            handle
            journal
            !state
            fallback
            current
            reference
            [ snapshot_id; journal_id; archive_id ]
            scan)))
;;

let%expect_test "history scan protects fallback, journal-only and archived references" =
  with_history (fun _ _ _ state _ _ _ expected scan ->
    assert (Option.is_none state.moderator);
    let found = scan () |> store_ok in
    assert (
      List.equal
        P.Id.Blob.equal
        (List.sort expected ~compare:P.Id.Blob.compare)
        (List.sort found ~compare:P.Id.Blob.compare));
    assert (Result.is_error (scan ~max_bytes:1 ()));
    assert (Result.is_error (scan ~transaction_hash:(Some (String.make 64 '0')) ()));
    print_endline
      "only the three retained references found; current state contains none of their \
       values";
    print_endline
      "byte-budget exhaustion and an unacknowledged journal head refuse collection");
  [%expect
    {|
    only the three retained references found; current state contains none of their values
    byte-budget exhaustion and an unacknowledged journal head refuse collection
    |}]
;;

let%expect_test
    "corrupt fallback, missing journal segment and damaged archive invalidate the whole \
     history proof"
  =
  with_history (fun env handle journal _ fallback _ reference _ scan ->
    let root = S.Session_store.Handle.directory handle in
    let file relative = Eio.Path.(Eio.Stdenv.fs env / Filename.concat root relative) in
    let reject_then_restore relative damage =
      let path = file relative in
      let original = Eio.Path.load path in
      Eio.Path.save ~create:(`Or_truncate 0o600) path (damage original);
      assert (Result.is_error (scan ()));
      Eio.Path.save ~create:(`Or_truncate 0o600) path original;
      assert (Result.is_ok (scan ()))
    in
    reject_then_restore
      (Filename.concat "snapshot" fallback.S.Snapshot.filename)
      (fun _ -> "corrupt fallback");
    reject_then_restore "snapshot/snapshot-0000000000000002.bin" (fun bytes ->
      let decoded =
        S.Snapshot.decode_file ~max_payload_length:1048576 bytes |> store_ok
      in
      let prior =
        Persistence.restore_snapshot ~limits:document_limits decoded
        |> store_ok
        |> Persistence.Restored.state
      in
      let event_sequence = Int64.succ prior.counters.event_sequence in
      let altered = { prior with counters = { prior.counters with event_sequence } } in
      let encoded =
        S.Snapshot.with_value
          decoded
          ~limits:document_limits
          { (S.Snapshot.value decoded) with
            event_sequence
          ; payload = state_document altered
          }
        |> store_ok
      in
      let directory = Filename.concat root "counterfeit-fixture" in
      let installed =
        S.Snapshot.install ~env ~directory ~max_payload_length:1048576 encoded |> store_ok
      in
      Eio.Path.load
        Eio.Path.(Eio.Stdenv.fs env / Filename.concat directory installed.filename));
    reject_then_restore "snapshot/CURRENT" (fun _ -> "snapshot-9999999999999999.bin\n");
    reject_then_restore
      (Filename.concat
         "journal"
         (S.Journal_segment.Id.filename (S.Journal.current_segment journal)))
      (fun bytes -> bytes ^ "\001");
    let middle =
      Filename.concat
        "journal"
        (S.Journal_segment.Id.filename (S.Journal_segment.Id.of_int64 2L |> store_ok))
    in
    Eio.Path.rename (file middle) (file "held-segment");
    assert (Result.is_error (scan ()));
    Eio.Path.rename (file "held-segment") (file middle);
    let archive = Filename.concat "archive" (Archive.filename reference) in
    reject_then_restore archive (fun _ -> "corrupt archive");
    reject_then_restore archive (fun bytes ->
      let payload =
        match
          S.Frame.decode ~max_payload_length:1048576 ~contents:bytes ~offset:0 |> frame_ok
        with
        | Complete { frame; _ } -> S.Frame.payload frame
        | _ -> failwith "expected complete archive"
      in
      let document =
        Document_schema.Document.decode ~limits:document_limits payload |> document_ok
      in
      let archived_json =
        Agent_store.Document_fields.required
          (Document_schema.Document.payload document)
          "state"
          (Agent_store.Document_fields.document ~limits:document_limits)
        |> document_ok
      in
      let archived =
        Agent_session.Session_state_document.decode ~limits:document_limits archived_json
        |> document_ok
        |> Agent_session.Session_state_document.value
      in
      let altered =
        { archived with
          moderator = Some (reference_checkpoint "changed after reference commit")
        }
      in
      S.Frame.encode
        ~max_payload_length:1048576
        ~flags:0
        (Archive.archive_document
           (Agent_session.Session_state_document.authored altered)
           ~limits:document_limits
         |> document_ok
         |> Document_schema.Document.to_string)
      |> frame_ok);
    Eio.Path.rename (file archive) (file "held-archive");
    assert (Result.is_error (scan ()));
    Eio.Path.rename (file "held-archive") (file archive);
    [%test_eq: int] 3 (scan () |> store_ok |> List.length);
    print_endline
      "fallback corruption, dangling snapshot pointer, journal tail/gap and \
       missing/corrupt archive all rejected";
    print_endline
      "restoring the files restores the complete reference set; nothing was deleted by \
       the scanner");
  [%expect
    {|
    fallback corruption, dangling snapshot pointer, journal tail/gap and missing/corrupt archive all rejected
    restoring the files restores the complete reference set; nothing was deleted by the scanner
    |}]
;;

let%expect_test
    "retained transactions before the newest snapshot still require a continuous hash \
     chain"
  =
  with_history (fun _ _ journal state _ current _ _ _ ->
    let transactions =
      S.Journal.scan journal
      |> store_ok
      |> fun (scan : S.Journal.scan) ->
      List.filter_map scan.entries ~f:(fun entry ->
        match S.Frame.flags entry.S.Journal.frame with
        | 0 -> Some (S.Transaction.decode (S.Frame.payload entry.frame) |> store_ok)
        | _ -> None)
    in
    let first = List.hd_exn transactions in
    let altered =
      S.Transaction.create
        ~limits:document_limits
        ~session_id:first.session_id
        ~generation:first.generation
        ~transaction_sequence:first.transaction_sequence
        ~previous_transaction_hash:first.previous_transaction_hash
        ~session_revision:first.session_revision
        ~first_event_sequence:first.first_event_sequence
        ~last_event_sequence:first.last_event_sequence
        ~accepted_at_ns:first.accepted_at_ns
        ~command_audit:first.command_audit
        ~delta:(delta_document (Moderator_changed None))
        ~durable_events:first.durable_events
      |> store_ok
    in
    let validate transactions =
      S.Recovery.validate_retained
        ~session_id:state.State.identity.session_id
        ~snapshots:[ current ]
        ~transactions
    in
    assert (Result.is_ok (validate transactions));
    assert (Result.is_error (validate (altered :: List.tl_exn transactions)));
    print_endline
      "valid framing and a newest-snapshot anchor do not hide a broken earlier retained \
       hash link");
  [%expect
    {| valid framing and a newest-snapshot anchor do not hide a broken earlier retained hash link |}]
;;
