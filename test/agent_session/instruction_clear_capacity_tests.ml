open! Core
open Fixtures
open Instruction_selection_tests
open Instruction_selection_persistence_tests

(* This fixture uses pure consistency setup for a saturated retained selection.
   It qualifies the real persistence/writer/recovery profile, not source authority
   or native activation; the daemon fixtures qualify those actual boundaries. *)
let%expect_test
    "saturated selection reserves a real tight writer Clear and rejects before write"
  =
  with_actor_workspace (fun env workspace_instance ->
    let files =
      List.init 64 ~f:(fun index ->
        let name = sprintf "skill%02d" index in
        let header = sprintf "---\nname: %s\ndescription: Capacity fixture\n---\n" name in
        loaded
          (name ^ "/SKILL.md")
          (header ^ String.make (4096 - String.length header) 'x'))
    in
    let capture = C.skills (snapshot files) |> protocol_ok in
    let selected =
      List.fold (List.init 126 ~f:Fn.id) ~init:Selection.empty ~f:(fun selection index ->
        apply selection (sprintf "capacity-%03d" index) (Replace_skills capture))
    in
    [%test_eq: int] 126 (List.length (Selection.receipts selected));
    [%test_eq: int]
      (256 * 1024)
      (List.sum
         (module Int)
         (Selection.skills selected)
         ~f:(fun file -> String.length (I.Loaded.contents file)));
    let initial =
      { (actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false) with
        instruction_selection = Some selected
      }
    in
    let basis =
      state_document initial |> SD.decode ~limits:document_limits |> document_ok
    in
    let command = request selected "capacity-126" (Replace_skills capture) in
    let transition, _ = prepare_state basis command |> prepared in
    let candidate = Option.value_exn transition.state.instruction_selection in
    let raw_bytes = Jsonaf.to_string (raw candidate) |> String.length in
    (* Deliberately small relative to normal limits: retained encoded selection
       plus16KiB for the real transaction envelope and final receipt growth. The
       assertions below inspect actual encoded and checksummed Journal frames. *)
    let funded_profile = raw_bytes + 16384 in
    let root = workspace_instance.canonical_root.native_path in
    let run ~label ~profile ~funded =
      let directory = Filename.concat root (label ^ "-journal") in
      let snapshot_directory = Filename.concat root (label ^ "-snapshot") in
      Eio.Path.mkdirs
        ~exists_ok:false
        ~perm:0o700
        Eio.Path.(Eio.Stdenv.fs env / snapshot_directory);
      Eio.Switch.run (fun sw ->
        let journal =
          Store.Journal.create
            ~env
            ~directory
            ~max_payload_length:profile
            ~max_segment_bytes:(Int64.of_int (funded_profile * 4))
            ~max_segment_frames:16
          |> store_ok
        in
        let writer =
          Store.Commit_writer.create
            ~sw
            ~journal
            ~session_id
            ~next_transaction_sequence:1L
            ~previous_transaction_hash:None
            ~queue_capacity:8
          |> store_ok
        in
        Exn.protect
          ~finally:(fun () -> Store.Commit_writer.close writer)
          ~f:(fun () ->
            let writes = ref 0 in
            let reject = ref funded in
            let durable =
              Persistence.create
                ~archive:(fun _ _ -> failwith "unexpected capacity archive")
                ~pending_archive:None
                ~before_commit:
                  (Some
                     (fun _ ->
                       incr writes;
                       if !reject
                       then
                         Error
                           (P.Error.create
                              Persistence_error
                              ~message:"injected physical pre-write rejection"
                              ~retryable:true
                              ())
                       else Ok ()))
                ~command_accepted:(fun _ _ ->
                  failwith "native Clear fabricated external audit")
                ~writer
                ~durability:Flush
                ~limits:document_limits
                ~archive_limits:document_limits
                ~retention_preflight:None
                ~restored:(Persistence.Restored.of_state_document basis)
                ~previous_transaction_hash:None
            in
            assert (
              Result.is_error
                (Persistence.commit
                   durable
                   ~command_audit:None
                   ~previous:initial
                   transition));
            [%test_eq: int] (if funded then 1 else 0) !writes;
            assert (List.is_empty (Store.Journal.scan journal |> store_ok).entries);
            [%test_eq: int]
              126
              (List.length
                 (Selection.receipts
                    (selection
                       (Persistence.Restored.state (Persistence.restored durable)))));
            if funded
            then (
              reject := false;
              Persistence.commit durable ~command_audit:None ~previous:initial transition
              |> protocol_ok;
              let before_clear =
                Persistence.Restored.state (Persistence.restored durable)
              in
              [%test_eq: int]
                127
                (List.length (Selection.receipts (selection before_clear)));
              let clear = request (selection before_clear) "final-clear" Clear in
              let cleared, _ =
                prepare_state
                  (Persistence.Restored.state_document (Persistence.restored durable))
                  clear
                |> prepared
              in
              Persistence.commit
                durable
                ~command_audit:None
                ~previous:before_clear
                cleared
              |> protocol_ok;
              [%test_eq: int] 3 !writes;
              let final = Persistence.Restored.state (Persistence.restored durable) in
              [%test_eq: int] 128 (List.length (Selection.receipts (selection final)));
              assert (List.is_empty (Selection.skills (selection final)));
              let entries = (Store.Journal.scan journal |> store_ok).entries in
              [%test_eq: int] 2 (List.length entries);
              List.iteri entries ~f:(fun index entry ->
                let payload = Store.Frame.payload entry.Store.Journal.frame in
                assert (String.length payload <= profile);
                let actual = Store.Transaction.decode payload |> store_ok in
                let value = Store.Transaction.value actual in
                assert (Option.is_none value.command_audit);
                [%test_eq: int] 1 (List.length value.durable_events);
                let event =
                  Agent_session.Durable_event_document.decode
                    ~limits:document_limits
                    (List.hd_exn value.durable_events)
                  |> document_ok
                  |> Agent_session.Durable_event_document.value
                in
                let status = P.Event.Durable.instruction_status event |> protocol_ok in
                let status = Option.value_exn status in
                [%test_eq: int] (127 + index) status.retained_receipts;
                [%test_eq: int] (if index = 0 then 64 else 0) status.skills;
                assert (
                  Bool.equal
                    (Int.equal index 0)
                    (Option.is_none
                       (Store.Transaction.value actual).previous_transaction_hash));
                let encoded =
                  Store.Frame.encode ~max_payload_length:profile ~flags:0 payload
                  |> Result.map_error ~f:(fun error ->
                    Sexp.to_string_hum (Store.Frame.sexp_of_error error))
                  |> Result.ok_or_failwith
                in
                [%test_eq: int64]
                  (Int64.of_int (String.length encoded))
                  Int64.(entry.next_offset - entry.offset)))));
      if funded
      then (
        let journal =
          Store.Journal.open_existing
            ~env
            ~directory
            ~max_payload_length:profile
            ~max_segment_bytes:(Int64.of_int (funded_profile * 4))
            ~max_segment_frames:16
          |> store_ok
        in
        let recovered =
          Store.Recovery.load
            ~env
            ~journal
            ~snapshot_directory
            ~max_snapshot_payload_length:funded_profile
            ~session_id
            ~initial:(Persistence.Restored.of_state_document basis)
            ~restore_snapshot:(Persistence.restore_snapshot ~limits:document_limits)
            ~apply:(Persistence.apply_transaction ~limits:document_limits)
            ~validate_transaction:
              (Persistence.validate_transaction ~limits:document_limits)
            ~validate:Persistence.validate
          |> store_ok
        in
        let final = Persistence.Restored.state recovered.state in
        [%test_eq: int64] 2L recovered.latest_transaction_sequence;
        [%test_eq: int] 128 (List.length (Selection.receipts (selection final)));
        assert (List.is_empty (Selection.skills (selection final)));
        [%test_eq: int] 1 (List.length (Selection.captures (selection final))))
    in
    run ~label:"insufficient" ~profile:(raw_bytes - 1) ~funded:false;
    run ~label:"funded" ~profile:funded_profile ~funded:true);
  print_endline
    "64 skills/256KiB and127 receipts funded; failed save publishes nothing; actual \
     unaudited Clear frame ACK/reopen retains128 receipts; smaller writer rejects before \
     write";
  [%expect
    {| 64 skills/256KiB and127 receipts funded; failed save publishes nothing; actual unaudited Clear frame ACK/reopen retains128 receipts; smaller writer rejects before write |}]
;;
