open Core
open Authoring_materialization_test
module Coverage = Chat_response.Authoring_fragment_coverage

let request ?cursor ?(budget = 1500) () =
  `Object
    [ "version", `Number "1"
    ; ( "operation"
      , `String
          (match cursor with
           | None -> "topic"
           | Some _ -> "continue") )
    ; ( "task"
      , match cursor with
        | None -> `String "one_off_script"
        | Some _ -> `Null )
    ; ( "topic_id"
      , match cursor with
        | None -> `String "chatml.programs"
        | Some _ -> `Null )
    ; "query", `Null
    ; "features", `Null
    ; ( "cursor"
      , match cursor with
        | None -> `Null
        | Some value -> `String value )
    ; "max_tokens", `Number (Int.to_string budget)
    ]
;;

let guidance entry =
  match entry.H.provenance with
  | Runtime_authoring value -> value
  | _ -> assert false
;;

let%expect_test
    "actual paged references satisfy preload only while every exact page remains \
     effective"
  =
  fixture (fun context host _ plan ->
    let policy = plan (Preload [ "chatml.programs" ]) in
    let capabilities = P.capabilities policy in
    let scope = "fragment-test:0" in
    let materialized = A.create ~context ~host ~policy ~capabilities ~scope () |> ok in
    let initial = entries (A.initial materialized) in
    let base =
      List.filter initial ~f:(fun entry ->
        not
          (List.exists (guidance entry).topics ~f:(fun topic ->
             String.equal topic.G.id "chatml.programs")))
    in
    let query request = Q.query_with_receipt context ~host ~capabilities ~scope request in
    let rec pages remaining response =
      assert (remaining > 0);
      let rest =
        match Jsonaf.member_exn "next_cursor" response.Q.json with
        | `Null -> []
        | `String cursor ->
          let budget =
            match
              Jsonaf.member_exn
                "minimum_next_tokens"
                (Jsonaf.member_exn "budget" response.json)
            with
            | `Number number -> Int.max 1500 (Int.of_string number)
            | `Null -> 1500
            | _ -> assert false
          in
          pages (remaining - 1) (query (request ~cursor ~budget ()))
        | _ -> assert false
      in
      response :: rest
    in
    let responses = pages 100 (query (request ())) in
    let history =
      List.filter_map responses ~f:(fun response ->
        Option.map response.Q.receipt ~f:(fun receipt -> response.json, receipt))
      |> List.mapi ~f:(fun index (json, receipt) ->
        let id = History_entry.Id.create ~namespace:"page" ~sequence:index |> ok in
        let entry = Codec.user_text ~id (Jsonaf.to_string json) |> Codec.to_protocol in
        let payload = entry.payload in
        let fragments =
          List.map receipt.Q.topics ~f:(fun topic ->
            G.
              { topic_id = topic.topic.id
              ; total_parts = topic.total_parts
              ; parts =
                  List.map topic.parts ~f:(fun part ->
                    { index = part.Q.index; item_sha256 = part.item_sha256 })
              })
        in
        let value =
          G.create_reference
            ~context_identity:(A.context_identity materialized)
            ~policy_fingerprint:(P.fingerprint policy)
            ~topics:(List.map receipt.topics ~f:(fun topic -> topic.Q.topic))
            ~fragments
            ~payload
          |> protocol_ok
        in
        { entry with provenance = Runtime_authoring value })
    in
    assert (List.length history > 1);
    let target_pages =
      List.filter history ~f:(fun entry ->
        List.exists (guidance entry).fragments ~f:(fun fragment ->
          String.equal fragment.G.topic_id "chatml.programs"))
    in
    assert (List.length target_pages > 1);
    let known = Presence.remember ~previous:[] ~history |> protocol_ok in
    let needs_topic ?(known = known) history =
      A.refresh materialized ~known ~effective:(base @ history)
      |> protocol_ok
      |> List.exists ~f:(fun message ->
        List.exists message.A.guidance.topics ~f:(fun topic ->
          String.equal topic.G.id "chatml.programs"))
    in
    assert (not (needs_topic history));
    assert (not (needs_topic (restored history)));
    let removed = List.hd_exn target_pages in
    let archived =
      Presence.
        { entry_id = History_entry.Id.create ~namespace:"archived-page" ~sequence:0 |> ok
        ; guidance = guidance removed
        }
    in
    assert (not (needs_topic ~known:(archived :: known) history));
    List.iter (initial @ history) ~f:(fun entry ->
      let value = guidance entry in
      assert (G.equal value (G.to_json value |> G.of_json |> protocol_ok)));
    let versioned = guidance removed in
    let downgraded =
      match G.to_json versioned with
      | `Object fields ->
        `Object (List.Assoc.add fields ~equal:String.equal "version" (`Number "1"))
      | _ -> assert false
    in
    assert (Result.is_error (G.of_json downgraded));
    assert (
      Result.is_error
        (G.create_reference
           ~context_identity:versioned.context_identity
           ~policy_fingerprint:versioned.policy_fingerprint
           ~topics:
             (List.map versioned.topics ~f:(fun topic -> { topic with complete = true }))
           ~fragments:versioned.fragments
           ~payload:removed.payload));
    let without =
      List.filter history ~f:(fun entry -> not (H.Id.equal entry.id removed.id))
    in
    assert (needs_topic without);
    let another = List.last_exn target_pages in
    let duplicate =
      { another with
        id = History_entry.Id.create ~namespace:"duplicate" ~sequence:0 |> ok
      }
    in
    assert (needs_topic (duplicate :: without));
    List.iter
      [ { removed with redacted = true }
      ; { removed with payload = `String "summary" }
      ; { removed with provenance = Canonical }
      ]
      ~f:(fun changed -> assert (needs_topic (changed :: without)));
    let original = guidance removed in
    List.iter
      [ `Context; `Policy; `Version; `Source; `Conflict; `Total ]
      ~f:(fun change ->
        let topics =
          List.map original.topics ~f:(fun topic ->
            match change with
            | `Version ->
              { topic with document_sha256 = digest "another document version" }
            | `Source -> { topic with source = Authored (digest "another owner") }
            | _ -> topic)
        in
        let fragments =
          List.map original.fragments ~f:(fun fragment ->
            match change with
            | `Conflict ->
              { fragment with
                parts =
                  List.map fragment.parts ~f:(fun part ->
                    { part with item_sha256 = digest "conflicting item" })
              }
            | `Total -> { fragment with total_parts = fragment.total_parts + 1 }
            | _ -> fragment)
        in
        let topics =
          List.map topics ~f:(fun topic ->
            match change with
            | `Total -> { topic with complete = false }
            | _ -> topic)
        in
        let altered =
          G.create_reference
            ~context_identity:
              (match change with
               | `Context -> digest "other context"
               | _ -> original.context_identity)
            ~policy_fingerprint:
              (match change with
               | `Policy -> digest "other policy"
               | _ -> original.policy_fingerprint)
            ~topics
            ~fragments
            ~payload:removed.payload
          |> protocol_ok
        in
        let changed = { removed with provenance = Runtime_authoring altered } in
        let altered_history =
          match change with
          | `Conflict -> { changed with id = duplicate.id } :: history
          | _ -> changed :: without
        in
        let known =
          Presence.remember ~previous:[] ~history:altered_history |> protocol_ok
        in
        assert (needs_topic ~known altered_history));
    let full = Coverage.complete (List.map history ~f:guidance) |> protocol_ok in
    assert (
      List.exists full ~f:(fun topic ->
        String.equal topic.G.id "chatml.programs" && topic.complete)));
  print_endline
    "real query pages union across restored history; gaps, duplicate pages, redaction, \
     replacement, context/policy/source/version changes and conflicting fragments \
     require refresh";
  [%expect
    {| real query pages union across restored history; gaps, duplicate pages, redaction, replacement, context/policy/source/version changes and conflicting fragments require refresh |}]
;;
