open Core
open Agent_server_test_support
module P = Agent_protocol
module D = Agent_store.Delegation_store
module G = Agent_session.Generated_definition
module S = Agent_store.Session_store
module Artifacts = Agent_store.Prompt_artifact_store
module Daemon = Agent_server.Daemon
module R = Agent_server.Session_registry

let store_ok result =
  Result.map_error result ~f:Agent_store.Store_error.to_protocol_error |> protocol_ok
;;

let%expect_test
    "startup collects only abandoned child artifacts and retains archived prompts"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    let path = Eio.Path.(Eio.Stdenv.fs env / root) in
    Exn.protect
      ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true path)
      ~f:(fun () ->
        let prompt_file = Filename.concat root "parent.chatmd" in
        let source text =
          Eio.Path.save
            ~create:(`Or_truncate 0o600)
            Eio.Path.(path / "parent.chatmd")
            text
        in
        source "<developer>Archived source.</developer>";
        let configuration = config root root prompt_file in
        let with_daemon f =
          Eio.Switch.run (fun sw ->
            let daemon =
              Daemon.start
                ~sw
                ~env
                ~config:configuration
                ~tool_dir:root
                ~home:root
                ~process_start_identity:None
                ~options:
                  { Daemon.default_options with
                    qualify_chatml_extensions = true
                  ; model_post_stream =
                      Some (fun ~sw:_ ~inputs:_ -> failwith "unexpected model request")
                  }
                ()
              |> protocol_ok
            in
            Exn.protect
              ~finally:(fun () -> Daemon.shutdown daemon |> protocol_ok)
              ~f:(fun () ->
                let client = connection daemon (principal ()) in
                Exn.protect
                  ~finally:(fun () -> Agent_client.Connection.close client)
                  ~f:(fun () ->
                    initialize client;
                    f daemon client)))
        in
        let artifacts daemon =
          Artifacts.create
            ~env
            ~root:
              (Agent_store.Data_root.prompt_artifacts_path
                 (S.data_root (Daemon.store daemon)))
          |> store_ok
        in
        let archived_revision =
          with_daemon (fun daemon client ->
            let session, _ = create_session ~key:"archived-retention" client in
            let entry = R.find (Daemon.registry daemon) session.id |> Option.value_exn in
            let state = Agent_session.Session_actor.state entry.actor |> protocol_ok in
            entry.close ();
            R.remove (Daemon.registry daemon) session.id |> ignore;
            S.archive_session (Daemon.store daemon) session.id |> store_ok;
            state.spec.prompt_revision_id)
        in
        source "<developer>Current source.</developer>";
        let abandoned, active_revision =
          with_daemon (fun daemon client ->
            Artifacts.load (artifacts daemon) archived_revision |> store_ok |> ignore;
            let parent, _ =
              create_session ~start_immediately:true ~key:"current-retention" client
            in
            let entry = R.find (Daemon.registry daemon) parent.id |> Option.value_exn in
            let state = Agent_session.Session_actor.state entry.actor |> protocol_ok in
            assert (
              not
                (P.Id.Prompt_revision.equal
                   archived_revision
                   state.spec.prompt_revision_id));
            let prepare name =
              let bundle =
                Chatmd_source_bundle.create
                  ~root_file:"child.chatmd"
                  ~sources:[ "child.chatmd", "<developer>" ^ name ^ "</developer>" ]
                  ()
                |> Result.ok_or_failwith
              in
              let definition =
                Agent_server.Runtime_owner.with_background_runtime
                  entry.runtime
                  (fun runtime ->
                     let native =
                       Option.value_exn
                         runtime.Agent_session.Runtime_builder.native_runtime
                     in
                     let caps =
                       Lazy.force native.capabilities
                       |> Result.map_error ~f:(fun error ->
                         error.Chat_response.Tool_capability.message)
                       |> Result.ok_or_failwith
                     in
                     G.prepare
                       ~env
                       ~dir:path
                       ~revision_id:(P.Id.Prompt_revision.create ())
                       ~created_at:(P.Timestamp.now ())
                       ~current_capabilities:(fun () -> caps)
                       ~references:[]
                       bundle
                     |> Result.map_error ~f:(fun errors ->
                       P.Error.invalid_request
                         (List.map errors ~f:Chatmd_shell_spec.Diagnostic.to_string
                          |> String.concat ~sep:"\n")))
                |> protocol_ok
              in
              let artifact = G.artifact definition in
              let key : D.Key.t =
                { parent_session_id = parent.id
                ; parent_generation = state.identity.generation
                ; principal_id = Option.value_exn state.identity.creating_principal
                ; idempotency_key = P.Idempotency_key.of_string name |> protocol_ok
                }
              in
              let admission : D.Admission.t =
                { child_session_id = P.Id.Session.create ()
                ; revision_id = artifact.revision_id
                ; transaction_id = P.Id.Transaction.create ()
                ; manifest_sha256 = artifact.manifest_sha256
                ; parent_revision_id = state.spec.prompt_revision_id
                ; parent_stop_epoch = Some state.stop_epoch
                ; authored_tool = None
                ; authority_sha256 =
                    Agent_session.Delegation_authority.fingerprint state |> protocol_ok
                ; capability_pins = G.capability_pins definition
                ; lifetime = Owned
                ; created_at = artifact.created_at
                }
              in
              let record =
                match
                  D.reserve
                    (S.delegations (Daemon.store daemon))
                    ~key
                    ~request_sha256:(Chatmd_shell_spec.Source_ref.digest name)
                    ~admission
                    ~max_records:32
                    ~max_bytes:1048576
                  |> store_ok
                with
                | New record -> record
                | _ -> failwith "unexpected retained reservation"
              in
              G.install_reserved
                ~delegations:(S.delegations (Daemon.store daemon))
                ~reservation:record
                ~artifact_store:(artifacts daemon)
                definition
              |> Result.map_error ~f:(fun errors ->
                List.map errors ~f:Chatmd_shell_spec.Diagnostic.to_string
                |> String.concat ~sep:"\n")
              |> Result.ok_or_failwith
            in
            let abandoned = prepare "abandoned-child" in
            let abandoned =
              D.revoke (S.delegations (Daemon.store daemon)) abandoned Admission_failed
              |> store_ok
            in
            let active = prepare "pending-child" in
            abandoned, active.admission.revision_id)
        in
        List.iter [ 1; 2 ] ~f:(fun _ ->
          with_daemon (fun daemon _ ->
            let artifacts = artifacts daemon in
            assert (not (Artifacts.exists artifacts abandoned.admission.revision_id));
            Artifacts.load artifacts archived_revision |> store_ok |> ignore;
            Artifacts.load artifacts active_revision |> store_ok |> ignore;
            let retained =
              D.resolve (S.delegations (Daemon.store daemon)) (D.reference abandoned)
              |> store_ok
            in
            assert (Option.is_some retained.artifact_collection);
            assert (Option.is_some retained.revocation)));
        print_endline
          "startup removed abandoned source, retained revoked retry identity, live \
           reservation and archived prompt across repeated restarts"));
  [%expect
    {| startup removed abandoned source, retained revoked retry identity, live reservation and archived prompt across repeated restarts |}]
;;
