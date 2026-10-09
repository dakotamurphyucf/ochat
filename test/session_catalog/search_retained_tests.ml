open! Core
open Agent_server_test_support
module P = Agent_protocol
module C = Agent_client
module S = Agent_server

let query server_id archive labels =
  let original = Search_service_tests.query server_id ~scan_limit:100 () in
  P.Search_query.create
    ~server_id
    ~term:(P.Search_query.term original)
    ~catalog:{ (P.Search_query.catalog original) with archive; labels }
    ~scan_limit:100
  |> protocol_ok
;;

let%expect_test "reopened archived search and navigation never activate retained sessions"
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
        let prompt = Filename.concat root "root.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt)
          "<developer>needle initial prefix.</developer>";
        let configuration = config root workspace prompt in
        let server_id, session_id, history_id =
          Eio.Switch.run (fun sw ->
            let daemon =
              Catalog_service_tests.start_catalog_daemon sw env ~root configuration
            in
            let client = connection daemon (principal ()) in
            initialize client;
            let session, attachment = create_session ~key:"retained-search" client in
            let owner =
              S.Session_registry.find (S.Daemon.registry daemon) session.id
              |> Option.value_exn
            in
            let entry = Search_service_tests.history 100 "retained needle text" in
            Agent_session.Session_actor.append_history
              owner.actor
              ~attachment_id:attachment.id
              [ entry ]
            |> protocol_ok
            |> ignore;
            let patch =
              P.Session_metadata.Patch.create
                ~name:Keep
                ~set_labels:[ "group", "retained" ]
                ~remove_labels:[]
              |> protocol_ok
            in
            C.Connection.request_without_history
              client
              (Session_update_metadata
                 { session_id = session.id
                 ; attachment_id = attachment.id
                 ; expected_metadata_revision = 0L
                 ; patch
                 ; idempotency_key = Catalog_service_tests.key "retained-search-label"
                 })
            |> protocol_ok
            |> ignore;
            let current =
              Agent_session.Session_actor.snapshot owner.actor |> protocol_ok
            in
            C.Connection.request_without_history
              client
              (Session_delete
                 { session_id = session.id
                 ; attachment_id = attachment.id
                 ; expected_revision = current.revision
                 ; policy = Archive
                 ; confirmation = P.Id.Session.to_string session.id
                 ; idempotency_key = Catalog_service_tests.key "retained-search-archive"
                 })
            |> protocol_ok
            |> ignore;
            let server_id = Agent_store.Session_store.server_id (S.Daemon.store daemon) in
            C.Connection.close client;
            S.Daemon.shutdown daemon |> protocol_ok;
            server_id, session.id, entry.id)
        in
        Eio.Switch.run (fun sw ->
          let daemon =
            Catalog_service_tests.start_catalog_daemon sw env ~root configuration
          in
          let client = connection daemon (principal ()) in
          initialize client;
          let search = C.Conversation_search.create client ~server_id in
          let registry = S.Daemon.registry daemon in
          let before = S.Session_registry.stats registry in
          let selected = query server_id Archived [ "group", "retained" ] in
          let page = C.Conversation_search.page search selected |> protocol_ok in
          let hit = List.hd_exn (P.Search_page.hits page) in
          let navigation =
            C.Conversation_search.navigate
              search
              (P.Search_navigation.Request.create ~query:selected ~hit |> protocol_ok)
            |> protocol_ok
          in
          let count query =
            C.Conversation_search.page search query
            |> protocol_ok
            |> P.Search_page.hits
            |> List.length
          in
          let active_count = count (query server_id Active []) in
          let mismatched_label_count =
            count (query server_id Archived [ "group", "different" ])
          in
          let after = S.Session_registry.stats registry in
          print_s
            [%sexp
              { retained_match =
                  (List.length (P.Search_page.hits page) = 1
                   && P.History.Id.equal (P.Search_hit.history_id hit) history_id
                   && P.Id.Session.equal
                        (P.Session_ref.session_id (P.Search_hit.session hit))
                        session_id
                   : bool)
              ; current_navigation =
                  ((match navigation with
                    | Current { context; _ } -> List.length context = 1
                    | Changed _ | Unavailable -> false)
                   : bool)
              ; active_count : int
              ; mismatched_label_count : int
              ; stayed_unloaded =
                  (before.loaded = 0
                   && after.loaded = 0
                   && before.indexed = after.indexed
                   && Option.is_none (S.Session_registry.find registry session_id)
                   : bool)
              }];
          C.Connection.close client;
          S.Daemon.shutdown daemon |> protocol_ok)));
  [%expect
    {|
    ((retained_match true) (current_navigation true) (active_count 0)
     (mismatched_label_count 0) (stayed_unloaded true))
    |}]
;;
