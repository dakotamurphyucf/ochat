open Core
open Agent_server_test_support
module F = Crash_recovery_fixture
module P = Agent_protocol
module S = Agent_store.Session_store
module D = Agent_store.Delegation_store
module A = Agent_store.Prompt_artifact_store
module Daemon = Agent_server.Daemon

let run_child env ~root ~boundary ~recover =
  let data_path = Filename.concat root "data" in
  (match recover with
   | false ->
     Eio.Switch.run (fun sw ->
       let store =
         S.create
           ~env
           ~sw
           ~root:data_path
           ~server_id:(P.Id.Server.create ())
           ~process_start_identity:None
           ~lock_nonce:"collection-crash"
         |> F.store_ok
       in
       let data = S.data_root store in
       let artifacts =
         A.create ~env ~root:(Agent_store.Data_root.prompt_artifacts_path data)
         |> F.store_ok
       in
       let artifact =
         A.Artifact.create
           ~revision_id:(P.Id.Prompt_revision.create ())
           ~root_chatmd:"<developer>Abandoned source.</developer>"
           ~sources:[]
           ~parser_schema_version:4
           ~runtime_schema_version:2
           ~created_at:(P.Timestamp.now ())
           ()
         |> F.store_ok
       in
       let key : D.Key.t =
         { parent_session_id = P.Id.Session.create ()
         ; parent_generation = 0
         ; principal_id = P.Id.Principal.of_string "pri_collection_crash" |> F.protocol_ok
         ; idempotency_key = F.key "collection-crash"
         }
       in
       let admission : D.Admission.t =
         { child_session_id = P.Id.Session.create ()
         ; revision_id = artifact.revision_id
         ; transaction_id = P.Id.Transaction.create ()
         ; manifest_sha256 = artifact.manifest_sha256
         ; parent_revision_id = P.Id.Prompt_revision.create ()
         ; parent_stop_epoch = Some 0L
         ; authored_tool = None
         ; authority_sha256 = Chatmd_shell_spec.Source_ref.digest "deleted parent"
         ; capability_pins = []
         ; lifetime = Owned
         ; created_at = artifact.created_at
         }
       in
       let ledger = S.delegations store in
       let reserved =
         match
           D.reserve
             ledger
             ~key
             ~request_sha256:(Chatmd_shell_spec.Source_ref.digest "abandoned")
             ~admission
             ~max_records:8
             ~max_bytes:1048576
           |> F.store_ok
         with
         | New record -> record
         | _ -> F.fail "unexpected collection reservation"
       in
       A.install artifacts ~transaction_id:admission.transaction_id artifact |> F.store_ok;
       let installed = D.advance ledger reserved Artifact_installed |> F.store_ok in
       D.revoke ledger installed Parent_deleted |> F.store_ok |> ignore;
       D.with_artifact_retention
         ledger
         ~max_records:8
         ~max_bytes:1048576
         ~max_artifact_entries:1024
         ~max_artifact_bytes:1048576
         ~f:(fun _ ->
           (match String.equal boundary "collection-partial" with
            | false -> ()
            | true ->
              Eio.Path.unlink
                (F.path
                   env
                   (Filename.concat
                      (Filename.concat
                         (Agent_store.Data_root.prompt_artifacts_path data)
                         (P.Id.Prompt_revision.to_string artifact.revision_id))
                      "root.chatmd")));
           Eio.Flow.copy_string "generated-creation-boundary\n" (Eio.Stdenv.stdout env);
           Eio.Fiber.await_cancel ())
       |> F.store_ok)
   | true ->
     let retained = ref None in
     List.iter [ 1; 2 ] ~f:(fun _ ->
       Eio.Switch.run (fun sw ->
         let daemon =
           Daemon.start
             ~sw
             ~env
             ~config:(config root root (Filename.concat root "parent.chatmd"))
             ~tool_dir:root
             ~home:root
             ~process_start_identity:None
             ~options:
               { Daemon.default_options with
                 qualify_chatml_extensions = true
               ; model_post_stream =
                   Some (fun ~sw:_ ~inputs:_ -> F.fail "collection invoked model")
               }
             ()
           |> F.protocol_ok
         in
         Exn.protect
           ~finally:(fun () -> Daemon.shutdown daemon |> F.protocol_ok)
           ~f:(fun () ->
             let store = Daemon.store daemon in
             let records =
               D.with_records
                 (S.delegations store)
                 ~max_records:8
                 ~max_bytes:1048576
                 ~f:(fun records -> Ok records)
               |> F.store_ok
             in
             let record =
               match records with
               | [ record ] -> record
               | _ -> F.fail "collection lost retry record"
             in
             F.require
               (Option.is_some record.artifact_collection)
               "collection intent not durable";
             F.require
               (Option.equal D.equal_revocation record.revocation (Some Parent_deleted))
               "revocation changed";
             (match !retained with
              | None -> retained := Some record
              | Some previous ->
                F.require
                  (D.equal_record previous record)
                  "collection retry identity changed");
             let artifacts =
               A.create
                 ~env
                 ~root:(Agent_store.Data_root.prompt_artifacts_path (S.data_root store))
               |> F.store_ok
             in
             F.require
               (not (A.exists artifacts record.admission.revision_id))
               "abandoned source survived recovery";
             F.require
               (List.is_empty (S.list_sessions store))
               "collection created a session"))));
  Eio.Flow.copy_string "generated-creation-recovered\n" (Eio.Stdenv.stdout env)
;;
