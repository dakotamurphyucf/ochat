open Core
open Fixtures
module P = Agent_protocol
module D = Agent_store.Delegation_store
module Store = Agent_store.Session_store
module Artifacts = Agent_store.Prompt_artifact_store
module Source = Agent_session.Authored_agent_source

let%expect_test "authored reservations preserve source identity through durable replay" =
  with_temp_directory (fun env temporary ->
    Eio.Switch.run (fun sw ->
      let source =
        Authored_agent_authority_tests.source
          ~root_path:Eio.Path.(Eio.Stdenv.fs env / temporary)
          ~policy:Persistent
      in
      let data_root = Filename.concat temporary "store" in
      let store =
        Store.create
          ~env
          ~sw
          ~root:data_root
          ~server_id:(P.Id.Server.create ())
          ~process_start_identity:None
          ~lock_nonce:"authored-artifacts"
        |> store_ok
        |> ref
      in
      Exn.protect
        ~finally:(fun () -> Store.close !store |> store_ok)
        ~f:(fun () ->
          let artifacts =
            Artifacts.create
              ~env
              ~root:(Agent_store.Data_root.prompt_artifacts_path (Store.data_root !store))
            |> store_ok
          in
          let pins =
            [ "read_file", Chatmd_shell_spec.Source_ref.digest "private-roots" ]
          in
          let origin : D.Admission.authored_tool =
            { name = "researcher"; source_sha256 = Source.fingerprint source }
          in
          let candidate () =
            Source.artifact
              source
              ~revision_id:(P.Id.Prompt_revision.create ())
              ~created_at:timestamp
            |> store_ok
          in
          let reserve
                ?(origin = Some origin)
                ?(parent_revision_id = (Source.identity source).parent_revision_id)
                label
                (artifact : Artifacts.Artifact.t)
            =
            let admission : D.Admission.t =
              { child_session_id = P.Id.Session.create ()
              ; revision_id = artifact.revision_id
              ; transaction_id = P.Id.Transaction.create ()
              ; manifest_sha256 = artifact.manifest_sha256
              ; parent_revision_id
              ; parent_stop_epoch = Some 0L
              ; authority_sha256 = Chatmd_shell_spec.Source_ref.digest "parent-policy"
              ; authored_tool = origin
              ; capability_pins = pins
              ; lifetime = Owned
              ; created_at = artifact.created_at
              }
            in
            D.reserve
              (Store.delegations !store)
              ~key:
                { parent_session_id = session_id
                ; parent_generation = 0
                ; principal_id
                ; idempotency_key = P.Idempotency_key.of_string label |> protocol_ok
                }
              ~request_sha256:(Chatmd_shell_spec.Source_ref.digest label)
              ~admission
              ~max_records:100
              ~max_bytes:1_000_000
            |> store_ok
            |> function
            | D.New record | Replay record -> record
            | Conflict _ -> failwith "unexpected authored reservation conflict"
          in
          let install ?(capability_pins = pins) reservation =
            Source.install_reserved
              ~delegations:(Store.delegations !store)
              ~reservation
              ~artifact_store:artifacts
              ~capability_pins
              source
          in
          let denied = function
            | Error { P.Error.code = Permission_denied; _ } -> ()
            | Error error -> raise_s [%sexp (error : P.Error.t)]
            | Ok _ -> failwith "mismatched authored admission was accepted"
          in
          let retained = candidate () in
          let reservation = reserve "retained" retained in
          install ~capability_pins:[] reservation |> denied;
          assert (not (Artifacts.exists artifacts retained.revision_id));
          (* Simulate a lost install acknowledgement: bytes exist, while the ledger
             is still Reserved. Concurrent retries must converge on the same tree. *)
          Artifacts.install
            artifacts
            ~transaction_id:reservation.admission.transaction_id
            retained
          |> store_ok;
          Eio.Fiber.both
            (fun () -> install reservation |> protocol_ok |> ignore)
            (fun () -> install reservation |> protocol_ok |> ignore);
          let record =
            D.resolve (Store.delegations !store) (D.reference reservation) |> store_ok
          in
          assert (D.equal_stage record.stage Artifact_installed);
          let replay = reserve "retained" (candidate ()) in
          assert (D.Reference.equal (D.reference reservation) (D.reference replay));
          Store.close !store |> store_ok;
          store
          := Store.open_existing
               ~env
               ~sw
               ~root:data_root
               ~process_start_identity:None
               ~lock_nonce:"authored-artifacts-reopened"
             |> store_ok;
          install replay |> protocol_ok |> ignore;
          let installed =
            Source.load_artifact ~artifact_store:artifacts ~reservation:replay
            |> protocol_ok
          in
          [%test_eq: string] retained.manifest_sha256 installed.manifest_sha256;
          (* An inherited wrapper retains its defining source, while the ledger
             records the actual creator's different revision. Execution authority
             is checked separately; source storage must not conflate the two. *)
          let inherited =
            reserve
              ~parent_revision_id:(P.Id.Prompt_revision.create ())
              "inherited-wrapper"
              (candidate ())
          in
          install inherited |> protocol_ok |> ignore;
          [%test_eq: string]
            retained.root_chatmd
            (Source.load_artifact ~artifact_store:artifacts ~reservation:inherited
             |> protocol_ok)
              .root_chatmd;
          let wrong_tool =
            reserve
              ~origin:(Some { origin with name = "reviewer" })
              "wrong-tool"
              (candidate ())
          in
          let generated = reserve ~origin:None "generated" (candidate ()) in
          Source.load_artifact ~artifact_store:artifacts ~reservation:generated |> denied;
          List.iter [ wrong_tool; generated ] ~f:(fun record ->
            install record |> denied;
            assert (not (Artifacts.exists artifacts record.admission.revision_id)));
          let revoked = reserve "revoked" (candidate ()) in
          D.revoke (Store.delegations !store) revoked Parent_stopped |> store_ok |> ignore;
          install revoked |> denied;
          assert (not (Artifacts.exists artifacts revoked.admission.revision_id));
          (* Inspection remains available after revocation, but cannot acknowledge
             a fresh executable admission through an old record. *)
          D.revoke (Store.delegations !store) replay Authority_changed
          |> store_ok
          |> ignore;
          Source.load_artifact ~artifact_store:artifacts ~reservation:replay
          |> protocol_ok
          |> ignore;
          install replay |> denied;
          let incompatible =
            Artifacts.Artifact.create
              ~revision_id:(P.Id.Prompt_revision.create ())
              ~root_chatmd:"<developer>Generated</developer>"
              ~sources:[]
              ~parser_schema_version:4
              ~runtime_schema_version:2
              ~created_at:timestamp
              ()
            |> store_ok
          in
          let wrong_contract = reserve "wrong-contract" incompatible in
          install wrong_contract |> denied;
          assert (not (Artifacts.exists artifacts incompatible.revision_id));
          Artifacts.install
            artifacts
            ~transaction_id:wrong_contract.admission.transaction_id
            incompatible
          |> store_ok;
          Source.load_artifact ~artifact_store:artifacts ~reservation:wrong_contract
          |> denied;
          let root = Artifacts.materialized_tree artifacts retained.revision_id in
          let file = Eio.Path.(root / retained.root_relative_path) in
          Core_unix.chmod (Eio.Path.native_exn file) ~perm:0o600;
          Eio.Path.save ~create:(`Or_truncate 0o600) file "changed after installation";
          assert (
            Result.is_error
              (Source.load_artifact ~artifact_store:artifacts ~reservation:replay));
          print_endline
            "reserved source/pins, concurrent lost-ack replay, restart, origin and \
             contract separation, revoked inspection and tamper rejection PASS")));
  [%expect
    {| reserved source/pins, concurrent lost-ack replay, restart, origin and contract separation, revoked inspection and tamper rejection PASS |}]
;;
