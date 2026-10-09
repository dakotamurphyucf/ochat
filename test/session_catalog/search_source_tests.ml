open! Core
open Agent_server_test_support
module P = Agent_protocol
module S = Agent_server

let with_state f =
  Search_service_tests.fixture (fun daemon _client ->
    let owner = S.Session_registry.entries (S.Daemon.registry daemon) |> List.hd_exn in
    let state = Agent_session.Session_actor.state owner.actor |> protocol_ok in
    let server_id = Agent_store.Session_store.server_id (S.Daemon.store daemon) in
    f server_id state)
;;

let replace_entries (state : Agent_session.Session_state.t) entries =
  { state with
    conversation =
      { state.conversation with
        canonical_history = entries
      ; initial_prompt_entry_count = 0
      }
  }
;;

let project source cache =
  S.Search_source.project
    source
    ~principal:(principal ())
    ~cache
    ~offset:0
    ~limit:1
    ~max_bytes:8_388_608
;;

let%expect_test "unreadable and oversized canonical payloads fail explicitly" =
  with_state (fun server_id state ->
    let original = Search_service_tests.history 200 "needle" in
    let newer =
      match original.payload with
      | `Object fields ->
        `Object
          (List.Assoc.add fields ~equal:String.equal "schema_version" (`Number "999"))
      | _ -> assert false
    in
    let deep =
      List.init 66 ~f:Fn.id |> List.fold ~init:`Null ~f:(fun json _ -> `Array [ json ])
    in
    List.iter
      [ "missing_document", `Null
      ; "newer_document", newer
      ; "duplicate_field", `Object [ "duplicate", `Null; "duplicate", `Null ]
      ; "oversized_payload", `String (String.make 2_097_153 'x')
      ; "deep_payload", deep
      ]
      ~f:(fun (name, payload) ->
        let source =
          replace_entries state [ { original with payload } ]
          |> S.Search_source.create ~server_id
          |> protocol_ok
        in
        let cache = S.Search_cache.create () |> protocol_ok in
        let result = project source cache |> Catalog_service_tests.status in
        print_s [%sexp { name : string; result : string }]));
  [%expect
    {|
    ((name missing_document) (result invalid_request))
    ((name newer_document) (result invalid_request))
    ((name duplicate_field) (result persistence_error))
    ((name oversized_payload) (result resource_limit))
    ((name deep_payload) (result resource_limit))
    |}]
;;

let%expect_test
    "a new canonical revision replaces cached plaintext without retaining old text"
  =
  with_state (fun server_id state ->
    let cache = S.Search_cache.create () |> protocol_ok in
    let read state entry =
      replace_entries state [ entry ]
      |> S.Search_source.create ~server_id
      |> protocol_ok
      |> fun source -> project source cache |> protocol_ok
    in
    let original = Search_service_tests.history 201 "needle old text" in
    ignore (read state original : S.Search_source.Window.t);
    let next =
      { state with
        counters = { state.counters with revision = Int64.succ state.counters.revision }
      }
    in
    let changed = Search_service_tests.history 201 "replacement text" in
    let window = read next changed in
    let selected = List.hd_exn window.entries |> Option.value_exn in
    let matcher = S.Search_text.create (P.Search_term.create "needle" |> protocol_ok) in
    let stale_match = S.Search_text.find matcher selected |> protocol_ok in
    print_s
      [%sexp
        { stale_match = (Option.is_some stale_match : bool)
        ; text =
            (List.map (S.Search_entry.parts selected) ~f:(fun part -> part.text)
             : string list)
        ; retained_entries = ((S.Search_cache.stats cache).entries : int)
        }]);
  [%expect {| ((stale_match false) (text ("replacement text")) (retained_entries 1)) |}]
;;

let%expect_test "unexpected read failures remain failures and cannot become an empty page"
  =
  Search_service_tests.fixture (fun daemon client ->
    let server_id = Agent_store.Session_store.server_id (S.Daemon.store daemon) in
    let search =
      Search_service_tests.service daemon client ~before_read:(fun () -> raise Exit) ()
    in
    let preserved =
      try
        ignore
          (S.Search_service.query
             search
             (Search_service_tests.query server_id ~scan_limit:100 ())
           : (P.Search_page.t, P.Error.t) Result.t);
        false
      with
      | Exit -> true
    in
    print_s [%sexp (preserved : bool)]);
  [%expect {| true |}]
;;
