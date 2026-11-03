open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module I = P.Invocation
module D = Document_schema
module SD = A.Session_state_document
module Persistence = A.Session_persistence
module Store = Agent_store

let set = Goal_catalog_tests.set
let member = Goal_catalog_tests.member

let invocations_json restored =
  Persistence.Restored.state_document restored
  |> SD.encode ~limits:document_limits
  |> document_ok
  |> D.Document.json
  |> fun raw -> member (member raw "payload") "invocations"
;;

let imported state =
  let run =
    List.hd_exn (A.Run_state.runs (Option.value_exn state.A.Session_state.run_state))
  in
  let make id origin =
    I.create
      { (invocation_fixture ()).context with
        id = P.Id.Invocation.of_string id |> protocol_ok
      }
    |> protocol_ok
    |> fun value ->
    Option.value_map origin ~default:value ~f:(fun origin ->
      I.capture_inference_origin value origin |> protocol_ok)
  in
  let invocations =
    [ make "inv_disk_unknown" None
    ; make "inv_disk_unbound" (Some Unbound)
    ; make "inv_disk_run" (Some (Run run.id))
    ]
  in
  let state =
    { state with
      invocations
    ; counters = { state.counters with transaction_sequence = 0L; event_sequence = 0L }
    }
  in
  let raw =
    SD.authored state
    |> SD.encode ~limits:document_limits
    |> document_ok
    |> D.Document.json
  in
  let rows =
    List.map invocations ~f:(fun invocation ->
      let raw =
        I.Storage.to_json invocation
        |> fun raw -> set raw "future_invocation" (`String "retained")
      in
      match invocation.inference_origin with
      | None -> raw
      | Some _ ->
        set
          raw
          "inference_origin"
          (set
             (member raw "inference_origin")
             "future_origin"
             (`Object [ "exact", `True ])))
  in
  let raw = set raw "payload" (set (member raw "payload") "invocations" (`Array rows)) in
  D.Document.inspect ~limits:document_limits raw
  |> document_ok
  |> SD.decode ~limits:document_limits
  |> document_ok
  |> Persistence.Restored.of_state_document
;;

(* Imported storage facts grant no native execution, inference, worker or Goal
   authority. Genuine callback capture is qualified in the adjacent actor group. *)
let%expect_test "invocation origin survives failed save and both physical recovery paths" =
  List.iter [ false; true ] ~f:(fun checkpoint ->
    Run_admission_tests.with_startup_actor
      (fun actor _ _ request start _ _ _ _ ->
         start (request "invocation-origin-storage") |> protocol_ok |> ignore;
         A.Session_actor.state actor |> protocol_ok |> imported)
      ~after_close:(fun env workspace initial ->
        let directory =
          Filename.concat workspace.canonical_root.native_path "invocation-origin-journal"
        in
        let snapshots =
          Filename.concat
            workspace.canonical_root.native_path
            "invocation-origin-snapshots"
        in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / snapshots);
        let open_journal create =
          create
            ~env
            ~directory
            ~max_payload_length:1048576
            ~max_segment_bytes:4194304L
            ~max_segment_frames:16
          |> store_ok
        in
        let expected =
          Eio.Switch.run (fun sw ->
            let journal = open_journal Store.Journal.create in
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
                let reject = ref true in
                let persistence =
                  Persistence.create
                    ~archive:(fun _ _ -> failwith "unexpected archive")
                    ~pending_archive:None
                    ~before_commit:
                      (Some
                         (fun _ ->
                           if !reject
                           then Error (handoff_error "before origin write")
                           else Ok ()))
                    ~command_accepted:(fun _ _ -> ())
                    ~writer
                    ~durability:Flush
                    ~limits:document_limits
                    ~archive_limits:document_limits
                    ~retention_preflight:None
                    ~restored:initial
                    ~previous_transaction_hash:None
                in
                let before = Persistence.Restored.state initial in
                let dispatched =
                  List.map before.invocations ~f:(fun invocation ->
                    I.dispatch invocation |> protocol_ok)
                in
                let candidate =
                  A.Session_transition.apply
                    ~now:timestamp
                    before
                    ~delta:
                      (Batch
                         (List.map dispatched ~f:(fun invocation ->
                            A.Session_delta.Invocation_changed invocation)))
                    ~payloads:[]
                  |> protocol_ok
                in
                assert (
                  Result.is_error
                    (Persistence.commit
                       persistence
                       ~command_audit:None
                       ~previous:before
                       candidate));
                assert (
                  D.Json.equal
                    (invocations_json initial)
                    (invocations_json (Persistence.restored persistence)));
                reject := false;
                Persistence.commit
                  persistence
                  ~command_audit:None
                  ~previous:before
                  candidate
                |> protocol_ok;
                let before =
                  Persistence.Restored.state (Persistence.restored persistence)
                in
                let resolved =
                  List.map before.invocations ~f:(fun invocation ->
                    I.resolve invocation ~session_id ~generation:0 (Complete `Null)
                    |> protocol_ok)
                in
                let candidate =
                  A.Session_transition.apply
                    ~now:timestamp
                    before
                    ~delta:
                      (Batch
                         (List.map resolved ~f:(fun invocation ->
                            A.Session_delta.Invocation_changed invocation)))
                    ~payloads:[]
                  |> protocol_ok
                in
                Persistence.commit
                  persistence
                  ~command_audit:None
                  ~previous:before
                  candidate
                |> protocol_ok;
                let accepted = Persistence.restored persistence in
                let state = Persistence.Restored.state accepted in
                [%test_eq: int64] state.counters.transaction_sequence 2L;
                if checkpoint
                then
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
                accepted))
        in
        Eio.Switch.run (fun _sw ->
          let journal = open_journal Store.Journal.open_existing in
          let recovered =
            Store.Recovery.load
              ~env
              ~journal
              ~snapshot_directory:snapshots
              ~max_snapshot_payload_length:1048576
              ~session_id
              ~initial
              ~restore_snapshot:(Persistence.restore_snapshot ~limits:document_limits)
              ~apply:(Persistence.apply_transaction ~limits:document_limits)
              ~validate_transaction:
                (Persistence.validate_transaction ~limits:document_limits)
              ~validate:Persistence.validate
            |> store_ok
          in
          assert (
            D.Json.equal (invocations_json expected) (invocations_json recovered.state));
          let state = Persistence.Restored.state recovered.state in
          (match
             List.map state.invocations ~f:(fun invocation ->
               invocation.I.inference_origin)
           with
           | [ None; Some Unbound; Some (Run _) ] -> ()
           | _ -> failwith "recovery changed origin availability");
          List.iter state.invocations ~f:(fun invocation ->
            assert (I.equal_status invocation.status (Resolved (Complete `Null)))))));
  print_endline
    "failed write unchanged; exact retry; journal and checkpoint retain origins and \
     future children";
  [%expect
    {| failed write unchanged; exact retry; journal and checkpoint retain origins and future children |}]
;;
