open Core
open Fixtures
module P = Agent_protocol
module D = Agent_store.Delegation_store
module Store = Agent_store.Session_store
module Artifacts = Agent_store.Prompt_artifact_store
module State = Agent_session.Session_state
module Persistence = Agent_session.Session_persistence

let digest text = Digestif.SHA256.(digest_string text |> to_hex)

module Documents = Agent_session.Session_state_document

let limits = document_limits
let encoded state = Documents.encode (Documents.authored state) ~limits
let snapshot state = snapshot_of_state state |> store_ok
let restored state = restore_state state |> store_ok

let%expect_test
    "generated checkpoint binds the original durable admission across reopen and \
     revocation"
  =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let root =
        Filename.concat workspace_instance.canonical_root.native_path "generated-store"
      in
      let store =
        Store.create
          ~env
          ~sw
          ~root
          ~server_id:(P.Id.Server.create ())
          ~process_start_identity:None
          ~lock_nonce:"generated-checkpoint"
        |> store_ok
      in
      let ledger = Store.delegations store in
      let artifacts =
        Artifacts.create
          ~env
          ~root:(Agent_store.Data_root.prompt_artifacts_path (Store.data_root store))
        |> store_ok
      in
      let artifact =
        Artifacts.Artifact.create
          ~revision_id:(P.Id.Prompt_revision.create ())
          ~root_chatmd:"<developer>Stored generated child.</developer>"
          ~sources:[]
          ~parser_schema_version:4
          ~runtime_schema_version:2
          ~created_at:timestamp
          ()
        |> store_ok
      in
      let admission =
        D.Admission.
          { child_session_id = second_session_id
          ; revision_id = artifact.revision_id
          ; transaction_id = P.Id.Transaction.create ()
          ; manifest_sha256 = artifact.manifest_sha256
          ; parent_revision_id = prompt_revision_id
          ; parent_stop_epoch = None
          ; authored_tool = None
          ; authority_sha256 = digest "admitted host authority"
          ; capability_pins = []
          ; lifetime = Owned
          ; created_at = timestamp
          ; inference_target = None
          }
      in
      let key =
        D.Key.
          { parent_session_id = session_id
          ; parent_generation = 2
          ; principal_id
          ; idempotency_key =
              P.Idempotency_key.of_string "generated-checkpoint" |> protocol_ok
          }
      in
      let reserved =
        D.reserve
          ledger
          ~key
          ~request_sha256:(digest "complete creation inputs")
          ~admission
          ~max_records:8
          ~max_bytes:1048576
        |> store_ok
        |> function
        | D.New record -> record
        | _ -> failwith "expected original admission"
      in
      Artifacts.install artifacts ~transaction_id:admission.transaction_id artifact
      |> store_ok;
      let _ = D.advance ledger reserved Artifact_installed |> store_ok in
      let reference = D.reference reserved in
      let original =
        actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
      in
      let state =
        { original with
          identity = { original.identity with session_id = second_session_id }
        ; spec =
            { original.spec with
              prompt_revision_id = artifact.revision_id
            ; delegation = Some reference
            ; protocol =
                { original.spec.protocol with prompt = Generated artifact.revision_id }
            }
        }
      in
      State.validate state |> protocol_ok;
      assert (
        Result.is_error
          (encoded { state with schema_version = State.current_schema_version - 1 }));
      assert (
        Result.is_error
          (encoded { state with schema_version = State.current_schema_version + 1 }));
      let pending =
        { state with
          pending_initial_start = true
        ; spec =
            { state.spec with
              protocol = { state.spec.protocol with start_immediately = true }
            }
        }
      in
      State.validate pending |> protocol_ok;
      assert_same_session_snapshot pending (restored pending);
      (* Complete named state requires the pending-start field; unsupported stored
         envelope versions fail without interpreting old domain layouts. *)
      let pending_document = encoded pending |> document_ok in
      let payload =
        match Document_schema.Document.payload pending_document with
        | `Object fields ->
          `Object
            (List.filter fields ~f:(fun (key, _) ->
               not (String.equal key "pending_initial_start")))
        | _ -> assert false
      in
      let missing =
        Document_schema.Document.create ~limits ~kind:"session.state" ~version:2 ~payload
        |> document_ok
      in
      assert (Result.is_error (Documents.decode ~limits missing));
      List.iter [ 0; 3 ] ~f:(fun version ->
        let json =
          match Document_schema.Document.json pending_document with
          | `Object fields ->
            `Object
              (List.map fields ~f:(fun (key, value) ->
                 ( key
                 , if String.equal key "schema_version"
                   then `Number (Int.to_string version)
                   else value )))
          | _ -> assert false
        in
        assert (
          Result.is_error
            (Document_schema.Document.inspect ~limits json
             |> Result.bind ~f:(Documents.decode ~limits))));
      let wire =
        P.Session.to_json (State.summary state) |> P.Session.of_json |> protocol_ok
      in
      assert (
        match wire.spec.prompt with
        | Generated id -> P.Id.Prompt_revision.equal id artifact.revision_id
        | _ -> false);
      let metadata =
        Store.Metadata.
          { schema_version = Store.current_schema_version
          ; session = State.summary state
          ; prompt_artifact = P.Id.Prompt_revision.to_string artifact.revision_id
          ; workspace_identity = workspace_instance.conflict_domain
          ; data_schema_version = State.current_schema_version
          }
      in
      let handle =
        Store.create_session
          store
          ~sw
          ~transaction_id:admission.transaction_id
          ~actor_lock_nonce:"generated-child"
          metadata
        |> store_ok
      in
      let _ =
        Agent_store.Snapshot.install
          ~env
          ~directory:(Store.Handle.snapshot_directory handle)
          ~max_payload_length:1048576
          (snapshot state)
        |> store_ok
      in
      let installed = D.advance ledger reserved Child_installed |> store_ok in
      Store.close_session store handle |> store_ok;
      Store.close store |> store_ok;
      let store =
        Store.open_existing
          ~env
          ~sw
          ~root
          ~process_start_identity:None
          ~lock_nonce:"generated-reopen"
        |> store_ok
      in
      let handle =
        Store.open_session
          store
          ~sw
          ~actor_lock_nonce:"generated-child-reopen"
          second_session_id
        |> store_ok
      in
      let saved =
        Agent_store.Snapshot.load_current
          ~env
          ~directory:(Store.Handle.snapshot_directory handle)
          ~max_payload_length:1048576
        |> store_ok
        |> Option.value_exn
      in
      let restored =
        Persistence.restore_snapshot ~limits saved.snapshot
        |> store_ok
        |> Persistence.Restored.state
      in
      assert_same_session_snapshot state restored;
      let resolved =
        D.resolve (Store.delegations store) (Option.value_exn restored.spec.delegation)
        |> store_ok
      in
      assert (D.equal_record installed resolved);
      let revoked =
        D.revoke (Store.delegations store) resolved Parent_stopped |> store_ok
      in
      assert (
        D.equal_record revoked (D.resolve (Store.delegations store) reference |> store_ok));
      (* Valid structure is not authority: a different admission digest must fail
         the actual ledger lookup even though the checkpoint has consistent IDs. *)
      let substituted =
        match D.reference_to_jsonaf reference with
        | `Object fields ->
          `Object
            (List.map fields ~f:(fun (key, value) ->
               ( key
               , if String.equal key "admission_sha256"
                 then `String (digest "substituted policy")
                 else value )))
          |> D.reference_of_jsonaf
          |> store_ok
        | _ -> assert false
      in
      let substituted_state =
        { state with spec = { state.spec with delegation = Some substituted } }
      in
      State.validate substituted_state |> protocol_ok;
      assert (Result.is_error (D.resolve (Store.delegations store) substituted));
      let invalid_states =
        [ { state with spec = { state.spec with delegation = None } }
        ; { state with spec = { state.spec with prompt_definition_id = Some prompt_id } }
        ; { state with spec = { state.spec with protocol = original.spec.protocol } }
        ; { state with spec = { state.spec with prompt_revision_id } }
        ; { state with identity = original.identity }
        ; { state with
            spec =
              { state.spec with
                protocol = { state.spec.protocol with persistence = Transient }
              }
          }
        ]
      in
      List.iter invalid_states ~f:(fun invalid ->
        assert (Result.is_error (State.validate invalid));
        assert (Result.is_error (encoded invalid)));
      List.iter [ 9; 10 ] ~f:(fun schema_version ->
        let downgraded = { state with schema_version } in
        assert (Result.is_error (State.upgrade_schema downgraded));
        assert (Result.is_error (encoded downgraded)));
      let transient_json =
        (* Embedded process-bound transient sessions are otherwise supported;
           exercise the generated-only restriction, not detached policy's ban. *)
        P.Session.Spec.to_json
          { state.spec.protocol with
            execution_host = Embedded
          ; liveness = Process_bound
          ; persistence = Transient
          }
      in
      assert (Result.is_error (P.Session.Spec.of_json transient_json));
      print_s
        [%sexp
          { checkpoint_schema = (restored.schema_version : int)
          ; stage = (resolved.stage : D.stage)
          ; retained_revocation = (revoked.revocation : D.revocation option)
          ; rejected_inconsistent_checkpoints = (List.length invalid_states : int)
          }];
      Store.close_session store handle |> store_ok;
      Store.close store |> store_ok));
  [%expect
    {|
    ((checkpoint_schema 21) (stage Child_installed)
     (retained_revocation (Parent_stopped))
     (rejected_inconsistent_checkpoints 6))
    |}]
;;
