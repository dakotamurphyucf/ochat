open! Core
module P = Agent_protocol
module Payload = History_entry.Payload

let ok = function
  | Ok value -> value
  | Error (error : P.Error.t) -> failwith error.message
;;

let string_ok = function
  | Ok value -> value
  | Error error -> failwith error
;;

let id = History_entry.Id.create ~namespace:"search" ~sequence:0 |> string_ok

let semantic role content =
  Payload.Semantic.create
    (Message { form = Input; role; content; phase = Absent })
    ~metadata:Payload.Metadata.empty
  |> string_ok
;;

let entry ?(provenance = P.History.Canonical) ?(full = false) role content =
  let semantic = semantic role content in
  if full
  then
    P.Public_history.full
      (History_entry.create_with_id ~id (Payload.authored semantic))
      ~provenance
    |> ok
  else
    P.Public_history.visible
      id
      ~provenance
      (P.Public_history.Visible.of_semantic semantic |> Option.value_exn)
    |> ok
;;

let text text = Payload.Content.Text { text; annotations = []; logprobs = Absent }

let%expect_test "navigation context truncates on scalar boundaries and omits hidden parts"
  =
  let projection =
    entry
      User
      [ text (String.make 2047 'x' ^ "😀")
      ; Image { uri = "private-image-uri"; detail = Absent }
      ; text "omitted tail"
      ]
    |> Agent_server.Search_entry.of_public
    |> ok
    |> Option.value_exn
  in
  let context = Agent_server.Search_entry.navigation_context projection |> ok in
  let value = P.Search_navigation.Entry.text context in
  print_s
    [%sexp
      { bytes = (String.length value : int)
      ; utf8 = (Stdlib.String.is_valid_utf_8 value : bool)
      ; truncated = (P.Search_navigation.Entry.truncated context : bool)
      ; hidden = (String.is_substring value ~substring:"private" : bool)
      }];
  [%expect {| ((bytes 2047) (utf8 true) (truncated true) (hidden false)) |}]
;;

let search term = P.Search_term.create term |> ok |> Agent_server.Search_text.create

let find_result matcher entry =
  let open Result.Let_syntax in
  let%bind entry = Agent_server.Search_entry.of_public entry in
  match entry with
  | None -> Ok None
  | Some entry ->
    let%map found = Agent_server.Search_text.find matcher entry in
    Option.map found ~f:(fun (found : Agent_server.Search_text.Match.t) -> found.snippet)
;;

let find term entry = find_result (search term) entry |> ok

let%expect_test "literal ASCII folding, exact non-ASCII and first matching part" =
  let message =
    entry User [ text "No match"; Refusal "Élan NEEDLE needle"; text "needle" ]
  in
  let found = find "needle" message |> Option.value_exn in
  print_s
    [%sexp
      (( P.Search_snippet.text found
       , P.Search_snippet.highlight_start found
       , P.Search_snippet.highlight_length found
       , Option.is_some (find "élan" message)
       , Option.is_some (find "ÉLAN" message)
       , Option.is_some (find ".*" message)
       , Option.is_some (find "matchÉ" message) )
       : string * int * int * bool * bool * bool * bool)];
  [%expect {| ("\195\137lan NEEDLE needle" 6 6 false true false false) |}]
;;

let%expect_test "security scope does not expand whitelist or provenance" =
  let hidden =
    [ Payload.Content.Image { uri = "secret"; detail = Absent }
    ; Unknown { kind = "future"; raw = `String "secret" }
    ; Text
        { text = "ordinary"
        ; annotations = [ `String "secret" ]
        ; logprobs = Value (`String "secret")
        }
    ]
  in
  let cases =
    [ entry ~full:true User hidden
    ; entry User hidden
    ; entry System [ text "secret" ]
    ; entry Developer [ text "secret" ]
    ; entry Tool [ text "secret" ]
    ; entry ~provenance:Moderator_inserted Assistant [ text "secret" ]
    ; entry ~provenance:(Moderator_replaced id) Assistant [ text "secret" ]
    ; entry Assistant [ text "secret" ]
    ]
  in
  print_s
    [%sexp
      (List.map cases ~f:(fun entry -> Option.is_some (find "secret" entry)) : bool list)];
  [%expect {| (false false false false false false false true) |}]
;;

let%expect_test "UTF-8 truncation preserves complete highlight at every scalar boundary" =
  let term = String.concat (List.init 64 ~f:(fun _ -> "😀")) in
  let valid =
    List.for_all (List.init 130 ~f:Fn.id) ~f:(fun prefix_count ->
      let prefix = String.concat (List.init prefix_count ~f:(fun _ -> "é")) in
      let message = entry User [ text (prefix ^ term ^ String.make 600 'z') ] in
      let snippet = find term message |> Option.value_exn in
      let text = P.Search_snippet.text snippet in
      Stdlib.String.is_valid_utf_8 text
      && String.length text <= 512
      && String.equal
           (String.sub
              text
              ~pos:(P.Search_snippet.highlight_start snippet)
              ~len:(P.Search_snippet.highlight_length snippet))
           term
      && P.Search_snippet.truncated_after snippet)
  in
  print_s [%sexp (valid : bool)];
  [%expect {| true |}]
;;

let%expect_test "all snippet entry points reject invalid byte ranges and UTF-8" =
  let snippet start length =
    P.Search_snippet.create
      ~text:"éx"
      ~highlight_start:start
      ~highlight_length:length
      ~truncated_before:false
      ~truncated_after:false
  in
  let wire =
    `Object
      [ "text", `String "éx"
      ; "highlight_start", `Number "1"
      ; "highlight_length", `Number "1"
      ; "truncated_before", `False
      ; "truncated_after", `False
      ]
  in
  print_s
    [%sexp
      (( List.map
           [ -1, 1; 1, 1; 0, 1; 0, 0; Int.max_value, Int.max_value ]
           ~f:(fun (start, length) -> Result.is_error (snippet start length))
       , Result.is_error (P.Search_snippet.of_json wire)
       , List.map
           [ ""; String.make 257 'a'; "\255" ]
           ~f:(fun term -> Result.is_error (P.Search_term.create term))
       , Result.is_ok (snippet 0 2) )
       : bool list * bool * bool list * bool)];
  [%expect {| ((true true true true true) true (true true true) true) |}]
;;

let%expect_test "bounded literal search rejects oversized text explicitly" =
  let matcher = search "needle" in
  let within = entry User [ text (String.make 1_048_576 'x') ] in
  let oversized = entry User [ text (String.make 1_048_577 'x') ] in
  let result = find_result matcher oversized in
  print_s
    [%sexp
      (( Result.is_ok (P.Search_term.create (String.make 256 'x'))
       , find_result matcher within |> ok |> Option.is_none
       , match result with
         | Ok _ -> false
         | Error error -> P.Error.equal_code error.code Resource_limit )
       : bool * bool * bool)];
  [%expect {| (true true true) |}]
;;

let%expect_test "search hit keeps revisions above JavaScript integer precision" =
  let snippet = find "needle" (entry User [ text "needle" ]) |> Option.value_exn in
  let hit =
    P.Search_hit.create
      ~session:
        (P.Session_ref.create
           ~server_id:(P.Id.Server.of_string "srv_search" |> ok)
           ~session_id:(P.Id.Session.of_string "ses_search" |> ok))
      ~generation:0
      ~session_revision:Int64.max_value
      ~history_id:id
      ~content_revision:(P.History.Content_revision.of_int64 Int64.max_value |> ok)
      ~part_index:3
      ~snippet
    |> ok
  in
  let encoded = P.Search_hit.to_json hit in
  let decoded = P.Search_hit.of_json encoded |> ok in
  let wrong_revision =
    match encoded with
    | `Object fields ->
      `Object
        (List.map fields ~f:(fun (name, value) ->
           ( name
           , if String.equal name "session_revision"
             then `Number "9223372036854775807"
             else value )))
    | _ -> assert false
  in
  print_s
    [%sexp
      (( Int64.equal (P.Search_hit.session_revision decoded) Int64.max_value
       , P.Search_hit.part_index decoded
       , Result.is_error (P.Search_hit.of_json wrong_revision) )
       : bool * int * bool)];
  [%expect {| (true 3 true) |}]
;;

let%expect_test "native search queries enforce catalog and traversal invariants" =
  let catalog : P.Session.List_request.t =
    { page = P.Page.Request.create ~limit:20 () |> ok
    ; desired_state = None
    ; prompt_id = None
    ; workspace_id = None
    ; owner_principal_id = None
    ; organization = P.Session_organization.Query.default
    ; labels = []
    ; sort = { field = Created_at; direction = Ascending }
    ; archive = All
    ; creator_principal_id = None
    ; active_owner_principal_id = None
    }
  in
  let create catalog scan_limit =
    P.Search_query.create
      ~server_id:(P.Id.Server.of_string "srv_search" |> ok)
      ~term:(P.Search_term.create "needle" |> ok)
      ~catalog
      ~scan_limit
  in
  let valid = create { catalog with labels = [ "z", "1"; "a", "2" ] } 1 |> ok in
  print_s
    [%sexp
      (( Result.is_ok (P.Search_query.of_json (P.Search_query.to_json valid))
       , List.equal
           String.equal
           (List.map (P.Search_query.catalog valid).labels ~f:fst)
           [ "a"; "z" ]
       , Result.is_error (create { catalog with page = { limit = 0; cursor = None } } 1)
       , Result.is_error (create { catalog with page = { limit = 101; cursor = None } } 1)
       , Result.is_error (create catalog 513)
       , Result.is_error
           (create
              { catalog with sort = { field = Updated_at; direction = Descending } }
              1)
       , Result.is_error
           (create
              { catalog with
                owner_principal_id = Some (P.Id.Principal.of_string "pri_a" |> ok)
              ; creator_principal_id = Some (P.Id.Principal.of_string "pri_b" |> ok)
              }
              1) )
       : bool * bool * bool * bool * bool * bool * bool)];
  [%expect {| (true true true true true true true) |}]
;;

let%expect_test "cacheable projection retains original content positions without raw data"
  =
  let message =
    entry
      ~full:true
      User
      [ Image { uri = "needle private URI"; detail = Absent }
      ; Unknown { kind = "future"; raw = `String "needle private body" }
      ; text "readable needle"
      ]
  in
  let projected = Agent_server.Search_entry.of_public message |> ok |> Option.value_exn in
  let found =
    Agent_server.Search_text.find (search "needle") projected |> ok |> Option.value_exn
  in
  print_s
    [%sexp
      (( List.map (Agent_server.Search_entry.parts projected) ~f:(fun part ->
           part.Agent_server.Search_entry.Part.text)
       , found.part_index
       , P.Search_snippet.highlight_start found.snippet )
       : string list * int * int)];
  [%expect {| (("readable needle") 2 9) |}]
;;

let%expect_test "search cache budgets, LRU, scope and revision isolation" =
  let module C = Agent_server.Search_cache in
  let reference id =
    P.Session_ref.create
      ~server_id:(P.Id.Server.of_string "srv_search" |> ok)
      ~session_id:(P.Id.Session.of_string id |> ok)
  in
  let first_session = reference "ses_first" in
  let key ?(scope = "reader") ?(revision = 1L) session index =
    C.Key.create
      ~scope_identity:scope
      ~session
      ~generation:0
      ~session_revision:revision
      ~canonical_index:index
    |> ok
  in
  let cache = C.create ~max_entries:2 ~max_session_entries:2 () |> ok in
  let first = key first_session 0 in
  let second = key first_session 1 in
  let third = key (reference "ses_second") 0 in
  C.add cache first None;
  C.add cache second None;
  let excluded_hit =
    match C.find cache first with
    | `Hit None -> true
    | _ -> false
  in
  C.add cache third None;
  let hit key =
    match C.find cache key with
    | `Hit _ -> true
    | `Miss -> false
  in
  let lru_correct = hit first && (not (hit second)) && hit third in
  let scope_isolated = not (hit (key ~scope:"another-reader" first_session 0)) in
  C.add cache (key ~revision:2L first_session 0) None;
  let stale_revision_removed = not (hit first) in
  C.invalidate_session cache first_session;
  let other_session_retained = hit third && Int.equal (C.stats cache).entries 1 in
  let tiny = C.create ~max_bytes:1 () |> ok in
  C.add tiny first None;
  let oversized_skipped = Int.equal (C.stats tiny).entries 0 in
  print_s
    [%sexp
      { excluded_hit : bool
      ; lru_correct : bool
      ; scope_isolated : bool
      ; stale_revision_removed : bool
      ; other_session_retained : bool
      ; oversized_skipped : bool
      }];
  [%expect
    {|
    ((excluded_hit true) (lru_correct true) (scope_isolated true)
     (stale_revision_removed true) (other_session_retained true)
     (oversized_skipped true))
    |}]
;;

let%expect_test "search cache limits selected text bytes and per-session pressure" =
  let module C = Agent_server.Search_cache in
  let session =
    P.Session_ref.create
      ~server_id:(P.Id.Server.of_string "srv_search" |> ok)
      ~session_id:(P.Id.Session.of_string "ses_search" |> ok)
  in
  let key index =
    C.Key.create
      ~scope_identity:"reader"
      ~session
      ~generation:0
      ~session_revision:1L
      ~canonical_index:index
    |> ok
  in
  let value =
    Agent_server.Search_entry.of_public (entry User [ text (String.make 700 'a') ]) |> ok
  in
  let cache = C.create ~max_bytes:1500 ~max_entries:8 ~max_session_entries:2 () |> ok in
  List.iter [ 0; 1; 2; 3 ] ~f:(fun index -> C.add cache (key index) value);
  let bytes_bound = (C.stats cache).accounted_bytes <= 1500 in
  let oldest_evicted =
    match C.find cache (key 0) with
    | `Miss -> true
    | `Hit _ -> false
  in
  let latest_retained =
    match C.find cache (key 3) with
    | `Hit (Some _) -> true
    | _ -> false
  in
  let cache = C.create ~max_entries:8 ~max_session_entries:2 () |> ok in
  List.iter [ 0; 1; 2; 3 ] ~f:(fun index -> C.add cache (key index) None);
  let session_bound = Int.equal (C.stats cache).entries 2 in
  C.clear cache;
  print_s
    [%sexp
      { bytes_bound : bool
      ; oldest_evicted : bool
      ; latest_retained : bool
      ; session_bound : bool
      ; cleared = (C.stats cache : C.Stats.t)
      }];
  [%expect
    {|
    ((bytes_bound true) (oldest_evicted true) (latest_retained true)
     (session_bound true) (cleared ((entries 0) (accounted_bytes 0))))
    |}]
;;

let%expect_test "signed search positions bind principal, query, source and host instance" =
  Eio_main.run (fun _ ->
    Mirage_crypto_rng_unix.use_default ();
    let module C = Agent_server.Search_cursor in
    let cursors = C.create () in
    let query term =
      let catalog = Catalog_service_tests.query ~archive:All () in
      P.Search_query.create
        ~server_id:(P.Id.Server.of_string "srv_search" |> ok)
        ~term:(P.Search_term.create term |> ok)
        ~catalog:{ catalog with sort = { field = Created_at; direction = Ascending } }
        ~scan_limit:8
      |> ok
    in
    let principal = Agent_server_test_support.principal () in
    let binding ?(principal = principal) ?(term = "needle") ?(revision = 0L) () =
      C.bind
        cursors
        ~principal
        ~query:(query term)
        ~organization_revision:revision
        ~catalog:[]
      |> ok
    in
    let original = binding () in
    let position = C.Position.create ~session:3 ~entry:12 |> ok in
    let token = C.issue cursors original position |> ok in
    let restored = C.resolve cursors original (Some token) |> ok in
    let code owner binding token =
      match C.resolve owner binding (Some token) with
      | Ok _ -> "ok"
      | Error error -> P.Error.code_to_string error.code
    in
    let another_principal = Agent_server_test_support.principal_with_id "pri_another" in
    let modified =
      let raw = Base64.decode_exn (P.Page.Cursor.to_string token) in
      let raw = String.substr_replace_all raw ~pattern:":3:12:" ~with_:":3:13:" in
      P.Page.Cursor.of_string (Base64.encode_exn raw) |> ok
    in
    print_s
      [%sexp
        { position = (restored : C.Position.t)
        ; new_query = (code cursors (binding ~term:"changed" ()) token : string)
        ; new_principal =
            (code cursors (binding ~principal:another_principal ()) token : string)
        ; changed_source = (code cursors (binding ~revision:1L ()) token : string)
        ; restarted = (code (C.create ()) original token : string)
        ; tampered = (code cursors original modified : string)
        }]);
  [%expect
    {|
    ((position ((session 3) (entry 12))) (new_query cursor_expired)
     (new_principal cursor_expired) (changed_source conflict)
     (restarted cursor_expired) (tampered cursor_expired))
    |}]
;;
