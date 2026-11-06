open! Core
open Fixtures
module P = Agent_protocol
module A = Agent_session
module D = Document_schema
module Store = Agent_store
module Persistence = A.Session_persistence
module Conversion = Native_goal_action_conversion_tests

(* Imported saved Resume DATA grants no actual Run/callback/constructor custody.
   This fixture tests the existing physical persistence owner, not native runtime
   admission. Both old and current documents use actual current domain codecs;
   the old durable representation is constructed as raw versioned DATA. *)
let%expect_test
    "old native snapshot/journal reopen preserves original chain through next write"
  =
  List.iter [ false; true ] ~f:(fun checkpoint ->
    with_actor_workspace (fun env workspace_instance ->
      let initial =
        actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
      in
      let request, intent, invocation = Conversion.resume_fixture () in
      let old_payload =
        state_document initial
        |> D.Document.payload
        |> fun payload ->
        Conversion.set
          payload
          "invocations"
          (`Array [ Conversion.old_row intent invocation ])
      in
      let old_state payload =
        Conversion.old_document ~kind:"session.state" ~version:10 ~payload
      in
      let old_delta =
        Conversion.old_document
          ~kind:"session.delta"
          ~version:6
          ~payload:
            (`Object
                [ ( "changes"
                  , `Array
                      [ `Object
                          [ "kind", `String "created"
                          ; "state", D.Document.json (old_state old_payload)
                          ]
                      ] )
                ])
      in
      let first =
        Store.Transaction.create
          ~limits:document_limits
          ~session_id
          ~generation:0
          ~transaction_sequence:1L
          ~previous_transaction_hash:None
          ~session_revision:1L
          ~first_event_sequence:None
          ~last_event_sequence:None
          ~accepted_at_ns:
            (P.Timestamp.to_time_ns timestamp
             |> Time_ns.to_int_ns_since_epoch
             |> Int64.of_int)
          ~command_audit:None
          ~delta:old_delta
          ~durable_events:[]
        |> store_ok
      in
      (* Deliberately noncanonical physical JSON: the chain must retain these
         exact bytes, including whitespace, before any logical conversion. *)
      let original_bytes = " \n" ^ Store.Transaction.encode first ^ "\n" in
      let original_hash = Store.Document_record.digest original_bytes in
      assert (
        not
          (String.equal
             original_hash
             (Store.Transaction.Stored.digest (Store.Transaction.stored first))));
      let directory =
        Filename.concat workspace_instance.canonical_root.native_path "native-old-journal"
      in
      let snapshots =
        Filename.concat
          workspace_instance.canonical_root.native_path
          "native-old-snapshots"
      in
      Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / snapshots);
      let journal create =
        create
          ~env
          ~directory
          ~max_payload_length:1048576
          ~max_segment_bytes:4194304L
          ~max_segment_frames:16
        |> store_ok
      in
      let first_journal = journal Store.Journal.create in
      ignore
        (Store.Journal.append
           first_journal
           ~durability:Flush
           ~flags:0
           ~payload:original_bytes
         |> store_ok
         : Store.Journal.append_result);
      let old_checkpoint =
        if not checkpoint
        then None
        else (
          let counters = Conversion.member old_payload "counters" in
          let counters =
            Conversion.set counters "revision" (`String "1")
            |> fun counters ->
            Conversion.set counters "transaction_sequence" (`String "1")
          in
          let payload = Conversion.set old_payload "counters" counters in
          let snapshot =
            Store.Snapshot.create
              ~limits:document_limits
              ~session_id
              ~transaction_sequence:1L
              ~transaction_hash:(Some original_hash)
              ~event_sequence:0L
              ~created_at:timestamp
              ~prompt_artifact:
                (P.Id.Prompt_revision.to_string initial.spec.prompt_revision_id)
              ~workspace_identity:workspace_instance.conflict_domain
              ~payload:(old_state payload)
            |> store_ok
          in
          let installed =
            Store.Snapshot.install
              snapshot
              ~env
              ~directory:snapshots
              ~max_payload_length:1048576
            |> store_ok
          in
          let path = Eio.Path.(Eio.Stdenv.fs env / snapshots / installed.filename) in
          Some (path, Eio.Path.load path))
      in
      let recover journal =
        Store.Recovery.load
          ~env
          ~journal
          ~snapshot_directory:snapshots
          ~max_snapshot_payload_length:1048576
          ~session_id
          ~initial:(Persistence.Restored.authored initial)
          ~restore_snapshot:(Persistence.restore_snapshot ~limits:document_limits)
          ~apply:(Persistence.apply_transaction ~limits:document_limits)
          ~validate_transaction:(Persistence.validate_transaction ~limits:document_limits)
          ~validate:Persistence.validate
        |> store_ok
      in
      let assert_restored restored =
        let current =
          Persistence.Restored.state_document restored
          |> A.Session_state_document.encode ~limits:document_limits
          |> document_ok
        in
        [%test_eq: int] 12 (D.Document.version current);
        let row =
          match Conversion.member (D.Document.payload current) "invocations" with
          | `Array [ row ] -> row
          | _ -> failwith "recovered Invocation changed"
        in
        Conversion.assert_preserved_row row request;
        let state = Persistence.Restored.state restored in
        match (List.hd_exn state.invocations).P.Invocation.native_goal_action with
        | Some (Resume recovered) ->
          assert (
            P.Goal_resume_request.equal
              request
              (P.Goal_native_resume_intent.request recovered));
          (match P.Goal_native_resume_intent.phase recovered with
           | Staged -> ()
           | _ -> failwith "physical recovery executed a native action")
        | None | Some (Start _) -> failwith "saved Resume branch changed"
      in
      let second_hash =
        Eio.Switch.run (fun sw ->
          (* Distinct owner opens disk after the raw old append completed. *)
          let opened = journal Store.Journal.open_existing in
          let recovered = recover opened in
          [%test_eq: bool] checkpoint (Option.is_some recovered.snapshot);
          assert (
            Option.equal
              String.equal
              recovered.latest_transaction_hash
              (Some original_hash));
          assert_restored recovered.state;
          let writer =
            Store.Commit_writer.create
              ~sw
              ~journal:opened
              ~session_id
              ~next_transaction_sequence:2L
              ~previous_transaction_hash:recovered.latest_transaction_hash
              ~queue_capacity:8
            |> store_ok
          in
          Exn.protect
            ~finally:(fun () -> Store.Commit_writer.close writer)
            ~f:(fun () ->
              let persistence =
                Run_persistence_tests.create_persistence
                  writer
                  recovered.state
                  ~previous_transaction_hash:recovered.latest_transaction_hash
              in
              let before = Persistence.Restored.state recovered.state in
              let candidate =
                A.Session_transition.apply
                  ~now:timestamp
                  before
                  ~delta:(Stop_epoch_changed 1L)
                  ~payloads:[]
                |> protocol_ok
              in
              Persistence.commit
                persistence
                ~command_audit:None
                ~previous:before
                candidate
              |> protocol_ok;
              let current = Persistence.restored persistence in
              assert_restored current;
              let state = Persistence.Restored.state current in
              [%test_eq: int64] 2L state.counters.transaction_sequence;
              ignore
                (Persistence.install_snapshot_at
                   persistence
                   ~env
                   ~directory:snapshots
                   ~max_payload_length:1048576
                   ~transaction_hash:(Persistence.transaction_hash persistence)
                   state
                 |> store_ok
                 : Store.Snapshot.installed);
              Option.value_exn (Persistence.transaction_hash persistence)))
      in
      (* Writer shutdown precedes the next real recovery owner. This also
         validates old fallback checkpoints against the new canonical checkpoint. *)
      let opened = journal Store.Journal.open_existing in
      let recovered = recover opened in
      [%test_eq: int64] 2L recovered.latest_transaction_sequence;
      assert (
        Option.equal String.equal recovered.latest_transaction_hash (Some second_hash));
      assert_restored recovered.state;
      let entries = Store.Journal.scan opened |> store_ok in
      let transactions =
        List.filter_map entries.entries ~f:(fun entry ->
          if Int.equal (Store.Frame.flags entry.frame) 0
          then
            Some (Store.Frame.payload entry.frame |> Store.Transaction.decode |> store_ok)
          else None)
      in
      (match transactions with
       | [ old; current ] ->
         assert (String.equal (Store.Transaction.encode old) original_bytes);
         assert (
           String.equal
             (Store.Transaction.Stored.digest (Store.Transaction.stored old))
             original_hash);
         assert (
           Option.equal
             String.equal
             current.previous_transaction_hash
             (Some original_hash));
         [%test_eq: int] 6 (D.Document.version old.delta);
         [%test_eq: int] 8 (D.Document.version current.delta)
       | _ -> failwith "physical chain changed transaction count");
      Option.iter old_checkpoint ~f:(fun (path, bytes) ->
        assert (String.equal bytes (Eio.Path.load path)));
      print_s [%sexp (checkpoint : bool), (recovered.latest_transaction_sequence : int64)]));
  [%expect
    {|
    (false 2)
    (true 2)
    |}]
;;
