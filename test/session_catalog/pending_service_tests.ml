open! Core
open Agent_server_test_support
module P = Agent_protocol
module C = Agent_client
module S = Agent_server

let status = function
  | Ok _ -> "ok"
  | Error (error : P.Error.t) -> P.Error.code_to_string error.code
;;

let request session_id =
  P.Pending_query.Request.create
    ~session_id
    ~page:(P.Page.Request.create ~limit:1 () |> protocol_ok)
  |> protocol_ok
;;

let%expect_test
    "pending RPC reads cold state without activation and checks current authority"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let workspace = Filename.concat root "workspace" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
        let prompt_file = Filename.concat root "root.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt_file)
          "<developer>Pending inspection fixture.</developer>";
        let config = config root workspace prompt_file in
        let session_id =
          Eio.Switch.run (fun sw ->
            Pending_recovery_service_tests.with_daemon
              sw
              env
              ~root
              config
              (fun _ ~connect ~own:_ ->
                 let client = connect (principal ()) in
                 initialize client;
                 let session, _ = create_session client in
                 session.id))
        in
        Eio.Switch.run (fun sw ->
          Pending_recovery_service_tests.with_daemon
            sw
            env
            ~root
            config
            (fun daemon ~connect ~own:_ ->
               let registry = S.Daemon.registry daemon in
               let client = connect (principal ()) in
               initialize client;
               let query = request session_id in
               let before = S.Session_registry.stats registry in
               let response =
                 C.Connection.request_without_history
                   client
                   (Session_pending_inputs query)
                 |> protocol_ok
               in
               let queue_empty =
                 match response with
                 | Session_pending_inputs view -> List.is_empty view.page.items
                 | _ -> false
               in
               let missing =
                 History_entry.Id.create
                   ~namespace:(P.Id.Session.to_string session_id)
                   ~sequence:77
                 |> Result.ok_or_failwith
               in
               let lookup =
                 C.Connection.request_without_history
                   client
                   (Session_pending_input { session_id; history_id = missing })
                 |> protocol_ok
               in
               let unavailable =
                 match lookup with
                 | Session_pending_input (Unavailable id) -> P.History.Id.equal missing id
                 | _ -> false
               in
               let no_transcript =
                 principal_with_scopes
                   "pri_restart_test"
                   (P.Scope.Set.of_list [ List_prompts ])
                 |> connect
               in
               initialize no_transcript;
               let denied =
                 C.Connection.request_without_history
                   no_transcript
                   (Session_pending_inputs query)
               in
               let foreign =
                 connect
                   (principal_with_scopes
                      "pri_pending_foreign"
                      (P.Scope.Set.of_list [ View_session_transcript ]))
               in
               initialize foreign;
               let invisible =
                 C.Connection.request_without_history
                   foreign
                   (Session_pending_inputs query)
               in
               let after = S.Session_registry.stats registry in
               printf
                 "queue-empty=%b unknown-unavailable=%b cold-before=%b cold-after=%b \
                  no-transcript=%s foreign=%s\n"
                 queue_empty
                 unavailable
                 (Option.is_none (S.Session_registry.find registry session_id))
                 (Int.equal before.loaded after.loaded)
                 (status denied)
                 (status invisible);
               [%expect
                 {|queue-empty=true unknown-unavailable=true cold-before=true cold-after=true no-transcript=permission_denied foreign=permission_denied|}]))))
;;

let%expect_test
    "cached pending results recheck current disclosure without reconstructing raw input"
  =
  let semantic =
    History_entry.Payload.Semantic.create
      (Unknown { provider_kind = "future.pending" })
      ~metadata:History_entry.Payload.Metadata.empty
    |> Result.ok_or_failwith
  in
  let payload =
    History_entry.Payload.reconstructed
      semantic
      ~provider:"test"
      ~raw:(`Object [ "secret_raw", `String "captured" ])
    |> Result.ok_or_failwith
  in
  let id =
    History_entry.Id.create ~namespace:"pending-projection" ~sequence:0
    |> Result.ok_or_failwith
  in
  let history =
    P.Public_history.full (History_entry.create_with_id ~id payload) ~provenance:Canonical
    |> protocol_ok
  in
  let outcome =
    P.Pending_query.Item.create
      ~history
      ~generation:0
      ~binding:Agent_protocol.Pending_input.Binding.safe_boundary
    |> protocol_ok
    |> P.Pending_query.Outcome.pending
  in
  let transcript =
    principal_with_scopes
      "pri_pending_projection"
      (P.Scope.Set.of_list [ View_session_transcript ])
  in
  let narrowed = S.Pending_projection.outcome transcript outcome |> protocol_ok in
  let full =
    principal_with_scopes
      "pri_pending_projection"
      (P.Scope.Set.of_list [ View_session_transcript; View_security_state ])
  in
  let repeated = S.Pending_projection.outcome full narrowed |> protocol_ok in
  let denied =
    S.Pending_projection.outcome
      (principal_with_scopes "pri_pending_projection" P.Scope.Set.empty)
      outcome
  in
  let contains_secret value =
    String.is_substring
      (Jsonaf.to_string (P.Pending_query.Outcome.to_json value))
      ~substring:"secret"
  in
  printf
    "full-has-evidence=%b current-scope-hides=%b narrow-never-upgrades=%b no-transcript=%s\n"
    (contains_secret outcome)
    (not (contains_secret narrowed))
    (String.equal
       (Jsonaf.to_string (P.Pending_query.Outcome.to_json narrowed))
       (Jsonaf.to_string (P.Pending_query.Outcome.to_json repeated)))
    (status denied);
  [%expect
    {|full-has-evidence=true current-scope-hides=true narrow-never-upgrades=true no-transcript=permission_denied|}]
;;
