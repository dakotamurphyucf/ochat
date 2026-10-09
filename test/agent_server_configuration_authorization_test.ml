open! Core
open Agent_server_test_support
module P = Agent_protocol

let%expect_test
    "stopped configuration RPC and receipts preserve runtime retirement and selection \
     scope"
  =
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
          "<developer>Offline configuration authorization fixture.</developer>";
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
              let owner = principal () in
              let authorized = connection daemon owner in
              initialize authorized;
              let session, attachment = create_session authorized in
              let entry =
                Agent_server.Session_registry.find
                  (Agent_server.Daemon.registry daemon)
                  session.id
                |> Option.value_exn
              in
              let state () =
                Agent_session.Session_actor.state entry.actor |> protocol_ok
              in
              assert (not (Agent_server.Runtime_owner.is_loaded entry.runtime));
              (match (state ()).lifecycle.observed with
               | Stopped -> ()
               | _ -> failwith "fresh stopped session unexpectedly activated");
              let current = state () in
              let target =
                match Inference.Selection.view current.spec.inference_target with
                | Captured target -> target
                | Unresolved -> failwith "fixture target unresolved"
              in
              let patch =
                P.Session_configuration.Patch.create
                  ~profile:(Inference.Request.Target.profile target)
                  ~settings:[]
                  ()
                |> protocol_ok
              in
              let command =
                P.Command.Session_configuration_update
                  { session_id = session.id
                  ; attachment_id = attachment.id
                  ; expected_generation = current.identity.generation
                  ; expected_revision = current.spec.configuration_revision
                  ; patch
                  ; idempotency_key =
                      P.Idempotency_key.of_string "profile-update" |> protocol_ok
                  }
              in
              let updated =
                Agent_client.Connection.request_without_history authorized command
                |> protocol_ok
                |> function
                | P.Method_result.Session_configuration_update view -> view
                | _ -> failwith "unexpected configuration result"
              in
              assert (not (Agent_server.Runtime_owner.is_loaded entry.runtime));
              assert (Option.is_none (state ()).active_operation);
              let reduced =
                principal_with_scopes
                  (P.Id.Principal.to_string owner.id)
                  (Set.remove owner.scopes P.Scope.Provider_select)
              in
              let revoked = connection daemon reduced in
              initialize revoked;
              let denied command =
                match Agent_client.Connection.request_without_history revoked command with
                | Error failure -> [%test_eq: P.Error.code] Permission_denied failure.code
                | Ok _ -> failwith "revoked profile scope disclosed a successful result"
              in
              denied command;
              denied
                (P.Command.Command_receipt
                   { method_name = P.Command.method_name command
                   ; original_params = P.Command.params command
                   });
              [%test_eq: int64] updated.revision (state ()).spec.configuration_revision;
              let receipt =
                Agent_client.Connection.request_without_history
                  authorized
                  (P.Command.Command_receipt
                     { method_name = P.Command.method_name command
                     ; original_params = P.Command.params command
                     })
                |> protocol_ok
              in
              (match receipt with
               | P.Method_result.Command_receipt
                   (P.Command_receipt.Committed
                      (P.Command_receipt.Configuration_updated { revision; _ })) ->
                 [%test_eq: int64] updated.revision revision
               | _ -> failwith "authorized original receipt was not retained");
              print_endline
                "revoked retry and receipt denied; original revision and receipt retained"))));
  [%expect {|revoked retry and receipt denied; original revision and receipt retained|}]
;;
