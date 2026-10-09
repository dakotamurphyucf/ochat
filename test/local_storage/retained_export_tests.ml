open! Core
open Agent_server_test_support
module P = Agent_protocol
module C = Agent_client
module D = Agent_server.Daemon

let status = function
  | Ok _ -> "ok"
  | Error (error : P.Error.t) -> P.Error.code_to_string error.code
;;

let%expect_test "archived retained export survives missing workspace without activation" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let workspace = Filename.concat root "workspace" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
        let prompt = Filename.concat root "root.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt)
          "<developer>Retained export fixture.</developer>";
        let configuration = config root workspace prompt in
        let calls = ref 0 in
        let start sw mode =
          D.start
            ~sw
            ~env
            ~config:configuration
            ~tool_dir:root
            ~home:root
            ~process_start_identity:None
            ~options:
              { D.default_options with
                startup_mode = mode
              ; inference_policy =
                  inference_policy
                    ~default_model:"fixture-model"
                    ~post_stream:(fun ~sw:_ ~inputs:_ ->
                      Int.incr calls;
                      failwith "retained export activated provider")
              }
            ()
          |> protocol_ok
        in
        let session_id, stale_attachment =
          Eio.Switch.run (fun sw ->
            let daemon = start sw Execute in
            let client = connection daemon (principal ()) in
            Exn.protect
              ~finally:(fun () ->
                C.Connection.close client;
                D.shutdown daemon |> protocol_ok)
              ~f:(fun () ->
                initialize client;
                let session, attachment = create_session client in
                let current =
                  C.Admin.get_session client session.id
                  |> protocol_ok
                  |> P.Public.Snapshot.fields
                in
                C.Connection.request_without_history
                  client
                  (Session_delete
                     { session_id = session.id
                     ; attachment_id = attachment.id
                     ; expected_revision = current.revision
                     ; policy = Archive
                     ; confirmation = P.Id.Session.to_string session.id
                     ; idempotency_key =
                         P.Idempotency_key.of_string "retained-export-archive"
                         |> protocol_ok
                     })
                |> protocol_ok
                |> ignore;
                session.id, attachment.id))
        in
        Eio.Path.rmtree Eio.Path.(Eio.Stdenv.fs env / workspace);
        Eio.Switch.run (fun sw ->
          let daemon = start sw On_demand in
          let client = connection daemon (principal ()) in
          let foreign =
            connection
              daemon
              (principal_with_scopes
                 "pri_retained_foreign"
                 (P.Scope.Set.of_list [ View_session_transcript; Own_sessions ]))
          in
          Exn.protect
            ~finally:(fun () ->
              C.Connection.close foreign;
              C.Connection.close client;
              D.shutdown daemon |> protocol_ok)
            ~f:(fun () ->
              initialize client;
              initialize foreign;
              let export connection session_id =
                C.Admin.export_session
                  connection
                  ~session_id
                  ~format:Json
                  ~revision:None
                  ~history:None
              in
              let result = export client session_id |> protocol_ok in
              let output = Buffer.create 256 in
              C.Blob_download.download
                ~connection:client
                ~session_id
                ~attachment_id:None
                ~blob:result.blob
                ~output:(Eio.Flow.buffer_sink output)
              |> protocol_ok;
              let stale =
                C.Connection.request_without_history
                  client
                  (Session_export
                     { session_id
                     ; attachment_id = Some stale_attachment
                     ; format = Json
                     ; revision = None
                     ; history = None
                     })
              in
              let hidden = export foreign session_id in
              let absent = export client (P.Id.Session.create ()) in
              let valid_json =
                Result.try_with (fun () -> Jsonaf.of_string (Buffer.contents output))
                |> Result.is_ok
              in
              print_s
                [%sexp
                  (( valid_json
                   , status stale
                   , status hidden
                   , status absent
                   , (Agent_server.Session_registry.stats (D.registry daemon)).loaded
                   , !calls )
                   : bool * string * string * string * int * int)]))));
  [%expect {| (true permission_denied permission_denied session_not_found 0 0) |}]
;;
