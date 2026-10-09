open! Core
open Agent_server_test_support
module P = Agent_protocol

let%expect_test "stale history writer never activates an indexed session" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let workspace = Filename.concat root "workspace" in
        let prompt_file = Filename.concat root "agent.chatmd" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt_file)
          "<developer>Offline nonactivating history fixture.</developer>";
        Eio.Switch.run (fun sw ->
          let inference_policy =
            inference_policy
              ~default_model:"fixture-model"
              ~post_stream:(fun ~sw:_ ~inputs:_ -> Stdlib.Seq.empty)
          in
          let inference_policy =
            { inference_policy with
              select_inference_profile =
                (fun ~current ~profile ->
                  if String.equal profile (Inference.Request.Target.profile current)
                  then Ok current
                  else Error Inference_runtime.Preparation_error.Target_unavailable)
            }
          in
          let daemon =
            Agent_server.Daemon.start
              ~sw
              ~env
              ~config:(config root workspace prompt_file)
              ~tool_dir:root
              ~home:root
              ~process_start_identity:None
              ~options:{ Agent_server.Daemon.default_options with inference_policy }
              ()
            |> protocol_ok
          in
          Exn.protect
            ~finally:(fun () -> Agent_server.Daemon.shutdown daemon |> protocol_ok)
            ~f:(fun () ->
              let authorized = connection daemon (principal ()) in
              initialize authorized;
              let session, attachment = create_session authorized in
              let registry = Agent_server.Daemon.registry daemon in
              let entry =
                Agent_server.Session_registry.find registry session.id |> Option.value_exn
              in
              let state = Agent_session.Session_actor.state entry.actor |> protocol_ok in
              let key text = P.Idempotency_key.of_string text |> protocol_ok in
              let continue =
                P.Command.Session_continue_history
                  { session_id = session.id
                  ; attachment_id = attachment.id
                  ; expected_generation = state.identity.generation
                  ; expected_revision = state.counters.revision
                  ; idempotency_key = key "stopped-continue"
                  }
              in
              Agent_client.Connection.request_without_history authorized continue
              |> protocol_ok
              |> ignore;
              let target = List.hd_exn state.conversation.canonical_history in
              let edit =
                P.History_edit.create
                  ~history_id:target.id
                  ~expected_content_revision:target.content_revision
                  ~text:"never admitted"
                  ~mode:Save_only
                |> protocol_ok
              in
              let edit =
                P.Command.Session_edit_history
                  { session_id = session.id
                  ; attachment_id = attachment.id
                  ; expected_generation = state.identity.generation
                  ; expected_revision = state.counters.revision
                  ; edit
                  ; idempotency_key = key "stale-edit"
                  }
              in
              let index_entries =
                Agent_store.Session_index.list
                  (Agent_store.Session_store.session_index
                     (Agent_server.Daemon.store daemon))
              in
              Agent_session.Session_actor.detach entry.actor attachment.id |> protocol_ok;
              let removed =
                Agent_server.Session_registry.remove registry session.id
                |> Option.value_exn
              in
              removed.close ();
              Agent_server.Session_registry.index_all registry index_entries;
              let loads = ref 0 in
              Agent_server.Session_registry.install_loader registry (fun _ ->
                Int.incr loads;
                Error
                  (P.Error.create
                     Invalid_state
                     ~message:"unexpected activating loader"
                     ~retryable:false
                     ()));
              let denied command =
                match
                  Agent_client.Connection.request_without_history authorized command
                with
                | Error failure -> [%test_eq: P.Error.code] Lease_stale failure.code
                | Ok _ -> failwith "stale attachment admitted history mutation"
              in
              denied edit;
              denied continue;
              let fresh_continue =
                match continue with
                | P.Command.Session_continue_history request ->
                  P.Command.Session_continue_history
                    { request with idempotency_key = key "fresh-stale-continue" }
                | _ -> assert false
              in
              denied fresh_continue;
              [%test_eq: int] 0 !loads;
              print_endline "stale edit, fresh continue and replay reject without loading"))));
  [%expect {|stale edit, fresh continue and replay reject without loading|}]
;;
