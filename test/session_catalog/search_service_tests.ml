open! Core
open Agent_server_test_support
module P = Agent_protocol
module C = Agent_client
module S = Agent_server

let history sequence text =
  let semantic =
    History_entry.Payload.Semantic.create
      (Message
         { form = Input
         ; role = User
         ; content = [ Text { text; annotations = []; logprobs = Absent } ]
         ; phase = Absent
         })
      ~metadata:History_entry.Payload.Metadata.empty
    |> Result.ok_or_failwith
  in
  let id =
    History_entry.Id.create ~namespace:"search-service" ~sequence |> Result.ok_or_failwith
  in
  P.History.
    { id
    ; content_revision = Content_revision.zero
    ; role = User
    ; kind = Message
    ; payload = History_entry.Payload.to_json (History_entry.Payload.authored semantic)
    ; provenance = Canonical
    ; redacted = false
    }
;;

let query server_id ?cursor ~scan_limit () =
  let catalog = Catalog_service_tests.query ?cursor ~archive:All () in
  P.Search_query.create
    ~server_id
    ~term:(P.Search_term.create "needle" |> protocol_ok)
    ~catalog:
      { catalog with
        sort = { field = Created_at; direction = Ascending }
      ; page = { catalog.page with limit = 2 }
      }
    ~scan_limit
  |> protocol_ok
;;

let fixture f =
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
          "<developer>needle initial prompt must stay hidden.</developer>";
        Eio.Switch.run (fun sw ->
          let daemon =
            Catalog_service_tests.start_catalog_daemon
              sw
              env
              ~root
              (config root workspace prompt_file)
          in
          let client = connection daemon (principal ()) in
          Exn.protect
            ~finally:(fun () ->
              C.Connection.close client;
              S.Daemon.shutdown daemon |> protocol_ok)
            ~f:(fun () ->
              initialize client;
              List.iteri [ "first"; "second" ] ~f:(fun i key ->
                let session, attachment = create_session ~key client in
                let entry =
                  S.Session_registry.find (S.Daemon.registry daemon) session.id
                  |> Option.value_exn
                in
                Agent_session.Session_actor.append_history
                  entry.actor
                  ~attachment_id:attachment.id
                  [ history (i * 3) "ordinary miss"
                  ; history ((i * 3) + 1) "readable NEEDLE"
                  ; history ((i * 3) + 2) "last miss"
                  ]
                |> protocol_ok
                |> ignore);
              f daemon client))))
;;

let service
      daemon
      client
      ?(before_read = fun () -> Ok ())
      ?(change_catalog = fun x -> x)
      ()
  =
  let server_id = Agent_store.Session_store.server_id (S.Daemon.store daemon) in
  let cache = S.Search_cache.create () |> protocol_ok in
  S.Search_service.create
    ~server_id
    ~principal:(principal ())
    ~cache
    ~cursors:(S.Search_cursor.create ())
    ~read_catalog:(fun request ->
      let open Result.Let_syntax in
      let%map page =
        C.Admin.list_sessions_page
          client
          { request with page = { limit = 100; cursor = None } }
      in
      change_catalog
        S.Search_service.Catalog.{ organization_revision = 0L; sessions = page.items })
    ~read_state:(fun session_id ->
      let open Result.Let_syntax in
      let%bind () = before_read () in
      S.Session_registry.read_state
        (S.Daemon.registry daemon)
        session_id
        ~authorize:(fun _ -> Ok ()))
;;

let%expect_test "actual canonical sessions search with bounded zero-hit continuations" =
  fixture (fun daemon client ->
    let server_id = Agent_store.Session_store.server_id (S.Daemon.store daemon) in
    let search = service daemon client () in
    let rec pages cursor attempts hits partial_misses =
      assert (attempts < 20);
      let page =
        S.Search_service.query search (query server_id ?cursor ~scan_limit:1 ())
        |> protocol_ok
      in
      let hits = hits @ P.Search_page.hits page in
      let partial_misses =
        partial_misses
        + Bool.to_int
            ((not (P.Search_page.reached_end page))
             && List.is_empty (P.Search_page.hits page))
      in
      match P.Search_page.next_cursor page with
      | Some cursor -> pages (Some cursor) (attempts + 1) hits partial_misses
      | None -> hits, partial_misses
    in
    let hits, partial_misses = pages None 0 [] 0 in
    let rpc = C.Conversation_search.create client ~server_id in
    let rpc_page =
      C.Conversation_search.page rpc (query server_id ~scan_limit:100 ()) |> protocol_ok
    in
    print_s
      [%sexp
        { hits = (List.length hits : int)
        ; only_canonical_text =
            (List.for_all hits ~f:(fun hit ->
               String.equal
                 (P.Search_snippet.text (P.Search_hit.snippet hit))
                 "readable NEEDLE")
             : bool)
        ; zero_hit_progress = (partial_misses > 0 : bool)
        ; shared_rpc_matches =
            (Int.equal (List.length (P.Search_page.hits rpc_page)) 2 : bool)
        }]);
  [%expect
    {|
    ((hits 2) (only_canonical_text true) (zero_hit_progress true)
     (shared_rpc_matches true))
    |}]
;;

let%expect_test "search rechecks access and catalog before disclosing cached candidates" =
  fixture (fun daemon client ->
    let server_id = Agent_store.Session_store.server_id (S.Daemon.store daemon) in
    let reads = ref 0 in
    let before_read () =
      incr reads;
      if !reads > 2
      then
        Error
          (P.Error.create
             Permission_denied
             ~message:"current policy denied"
             ~retryable:false
             ())
      else Ok ()
    in
    let error_code = function
      | Ok _ -> "ok"
      | Error (error : P.Error.t) -> P.Error.code_to_string error.code
    in
    let denied =
      S.Search_service.query
        (service daemon client ~before_read ())
        (query server_id ~scan_limit:100 ())
      |> error_code
    in
    let catalogs = ref 0 in
    let change_catalog catalog =
      incr catalogs;
      if !catalogs = 1
      then catalog
      else { catalog with S.Search_service.Catalog.sessions = [] }
    in
    let changed =
      S.Search_service.query
        (service daemon client ~change_catalog ())
        (query server_id ~scan_limit:100 ())
      |> error_code
    in
    print_s [%sexp { denied : string; changed : string }]);
  [%expect {| ((denied permission_denied) (changed conflict)) |}]
;;

let%expect_test
    "navigation revalidates canonical edits and deletion without opening archives"
  =
  fixture (fun daemon client ->
    let server_id = Agent_store.Session_store.server_id (S.Daemon.store daemon) in
    let rpc = C.Conversation_search.create client ~server_id in
    let query = query server_id ~scan_limit:100 () in
    let page = C.Conversation_search.page rpc query |> protocol_ok in
    let hit = List.hd_exn (P.Search_page.hits page) in
    let request = P.Search_navigation.Request.create ~query ~hit |> protocol_ok in
    let navigate () = C.Conversation_search.navigate rpc request |> protocol_ok in
    let current =
      match navigate () with
      | Current { hit = current; context } ->
        P.History.Id.equal (P.Search_hit.history_id current) (P.Search_hit.history_id hit)
        && List.length context = 3
        && List.for_all context ~f:(fun entry ->
          not
            (String.is_substring
               (P.Search_navigation.Entry.text entry)
               ~substring:"initial prompt"))
      | Changed _ | Unavailable -> false
    in
    let session_id = P.Session_ref.session_id (P.Search_hit.session hit) in
    let actor =
      (S.Session_registry.find (S.Daemon.registry daemon) session_id |> Option.value_exn)
        .actor
    in
    let before = Agent_session.Session_actor.state actor |> protocol_ok in
    let attachment = List.hd_exn before.attachments in
    let edit =
      P.History_edit.create
        ~history_id:(P.Search_hit.history_id hit)
        ~expected_content_revision:(P.Search_hit.content_revision hit)
        ~text:"edited NEEDLE"
        ~mode:Save_only
      |> protocol_ok
    in
    C.Connection.request_without_history
      client
      (Session_edit_history
         { session_id
         ; attachment_id = attachment.id
         ; expected_generation = before.identity.generation
         ; expected_revision = before.counters.revision
         ; edit
         ; idempotency_key =
             P.Idempotency_key.of_string "search-navigation-edit" |> protocol_ok
         })
    |> protocol_ok
    |> ignore;
    let changed =
      match navigate () with
      | Changed revision ->
        not
          (P.History.Content_revision.equal revision (P.Search_hit.content_revision hit))
      | Current _ | Unavailable -> false
    in
    let after = Agent_session.Session_actor.state actor |> protocol_ok in
    Agent_session.Session_actor.delete_history
      actor
      ~attachment_id:attachment.id
      ~expected_revision:after.counters.revision
      (P.Search_hit.history_id hit)
    |> protocol_ok
    |> ignore;
    let unavailable =
      match navigate () with
      | Unavailable -> true
      | Current _ | Changed _ -> false
    in
    let denied =
      S.Search_service.navigate
        (service
           daemon
           client
           ~before_read:(fun () ->
             Error
               (P.Error.create Permission_denied ~message:"revoked" ~retryable:false ()))
           ())
        request
      |> function
      | Error { P.Error.code = Permission_denied; _ } -> true
      | Ok _ | Error _ -> false
    in
    print_s [%sexp { current : bool; changed : bool; unavailable : bool; denied : bool }]);
  [%expect {| ((current true) (changed true) (unavailable true) (denied true)) |}]
;;
