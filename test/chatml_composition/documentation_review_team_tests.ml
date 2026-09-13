open Core
open Agent_server_test_support
module P = Agent_protocol
module Res = Openai.Responses
module Team = Documentation_agent_team_tests

let sources =
  [ ( "team.chatmd"
    , [%blob "../../docs-src/examples/applications/persistent-review-team/team.chatmd"] )
  ; ( "server.sexp"
    , [%blob "../../docs-src/examples/applications/persistent-review-team/server.sexp"] )
  ; ( "agents/correctness-reviewer.chatmd"
    , [%blob
        "../../docs-src/examples/applications/persistent-review-team/agents/correctness-reviewer.chatmd"]
    )
  ; ( "agents/documentation-reviewer.chatmd"
    , [%blob
        "../../docs-src/examples/applications/persistent-review-team/agents/documentation-reviewer.chatmd"]
    )
  ; ( "agents/integration-reviewer.chatmd"
    , [%blob
        "../../docs-src/examples/applications/persistent-review-team/agents/integration-reviewer.chatmd"]
    )
  ; ( "scripts/coordinator.chatml"
    , [%blob
        "../../docs-src/examples/applications/persistent-review-team/scripts/coordinator.chatml"]
    )
  ; ( "schemas/review-request.json"
    , [%blob
        "../../docs-src/examples/applications/persistent-review-team/schemas/review-request.json"]
    )
  ; ( "schemas/review-findings.json"
    , [%blob
        "../../docs-src/examples/applications/persistent-review-team/schemas/review-findings.json"]
    )
  ; ( "sample-project/requirements.txt"
    , [%blob
        "../../docs-src/examples/applications/persistent-review-team/sample-project/requirements.txt"]
    )
  ; ( "sample-project/follow-up.txt"
    , [%blob
        "../../docs-src/examples/applications/persistent-review-team/sample-project/follow-up.txt"]
    )
  ; "sample-project/docs/setup.md", Documentation_shell_tests.setup
  ; "sample-project/docs/reference.md", Documentation_shell_tests.reference
  ; "sample-project/scripts/check-docs.sh", Documentation_shell_tests.checker
  ; "sample-project/expected-report.json", Documentation_shell_tests.expected_report
  ]
;;

let%expect_test
    "complete review team correlates distinct conversations, partial failure, follow-up \
     and cleanup"
  =
  let serial = ref 0 in
  let followed = String.Hash_set.create () in
  let reviewer_provider ~sw:_ ~inputs =
    let transcript = `Array (List.map inputs ~f:Res.Item.jsonaf_of_t) in
    let role =
      List.find
        [ "correctness"; "reader-experience"; "release-readiness" ]
        ~f:(fun role ->
          Team.contains transcript ("You are Lantern's " ^ role ^ " reviewer."))
    in
    Option.map role ~f:(fun role ->
      Int.incr serial;
      let id = sprintf "team-%d" !serial in
      match Team.contains transcript "SIMULATE_PROVIDER_FAILURE" with
      | true -> failwith "controlled reviewer provider failure"
      | false ->
        if Team.contains transcript "FOLLOW_UP_EVIDENCE"
        then (
          assert (
            Team.contains transcript (role ^ ": recorded evidence; proposal unverified"));
          Hash_set.add followed role;
          Team.answer ~id (role ^ ": refined earlier proposal; still needs real recheck"))
        else if Team.contains transcript "function_call_output"
        then (
          assert (Team.contains transcript "Lantern tutorial release requirements");
          Team.answer ~id (role ^ ": recorded evidence; proposal unverified"))
        else
          Fixtures.call_events
            [ ( id
              , "read_file"
              , `Object [ "root", `String "project"; "file", `String "requirements.txt" ]
              )
            ])
  in
  Team.with_team ~sources ~reviewer_provider (fun create invoke state _ ->
    let parent = create "team" in
    let call name fields = invoke parent name (`Object fields) |> Team.native_json in
    let input = "input", `String "Review the sample release evidence." in
    let correctness = call "correctness_review" [ input ] in
    let documentation =
      call "documentation_review" [ input; "mode", `String "persistent" ]
    in
    let integration =
      call "integration_review" [ "input", `String "SIMULATE_PROVIDER_FAILURE" ]
    in
    let id value = Team.field value "session_id" in
    assert (not (Jsonaf.exactly_equal (id correctness) (id documentation)));
    assert (not (Jsonaf.exactly_equal (id correctness) (id integration)));
    let request role response =
      `Object
        [ "role", `String role
        ; "session_id", id response
        ; "receipt_id", Team.field (Team.field response "receipt") "receipt_id"
        ; "cursor", `Null
        ]
    in
    let collect responses = call "collect_reviews" [ "reviewers", `Array responses ] in
    let initial =
      collect
        [ request "correctness" correctness
        ; request "documentation" documentation
        ; request "integration" integration
        ]
    in
    assert (Team.contains initial "correctness: recorded evidence");
    assert (Team.contains initial "reader-experience: recorded evidence");
    assert (Team.contains initial "failed");
    let reviews = Team.field initial "reviews" |> Jsonaf.list_exn in
    let documentation_result = List.nth_exn reviews 1 in
    let cursor =
      Team.field
        (Team.field (Team.field documentation_result "output") "value")
        "next_cursor"
    in
    let consumed_query =
      match request "documentation" documentation with
      | `Object fields ->
        `Object (List.Assoc.add fields ~equal:String.equal "cursor" cursor)
      | _ -> assert false
    in
    let consumed = collect [ consumed_query ] in
    assert (not (Team.contains consumed "reader-experience: recorded evidence"));
    assert (Team.contains consumed "caught_up");
    print_endline
      "collector: three distinct sessions; receipt-filtered evidence and one failed \
       reviewer retained";
    let one_off =
      invoke parent "documentation_review" (`Object [ input ]) |> Team.complete
    in
    assert (Team.contains one_off "reader-experience: recorded evidence");
    let another = call "documentation_review" [ input; "mode", `String "persistent" ] in
    assert (not (Jsonaf.exactly_equal (id documentation) (id another)));
    let next =
      call
        "documentation_review"
        [ "input", `String "FOLLOW_UP_EVIDENCE: refine the same proposal."
        ; "mode", `String "persistent"
        ; "session_id", id documentation
        ]
    in
    assert (Jsonaf.exactly_equal (id documentation) (id next));
    let next_request = request "documentation" next in
    let old_request = request "documentation" documentation in
    assert (
      not
        (Jsonaf.exactly_equal
           (Team.field next_request "receipt_id")
           (Team.field old_request "receipt_id")));
    let refined = collect [ next_request ] in
    assert (Team.contains refined "refined earlier proposal");
    assert (not (Team.contains refined "reader-experience: recorded evidence"));
    assert (Hash_set.mem followed "reader-experience");
    print_endline
      "optional tool: default one-off, omitted ID creates another instance, supplied ID \
       retains history with a new receipt";
    let wrong_receipt =
      `Object
        [ "role", `String "correctness"
        ; "session_id", id correctness
        ; "receipt_id", Team.field next_request "receipt_id"
        ; "cursor", `Null
        ]
    in
    let mixed = collect [ wrong_receipt; next_request ] in
    assert (Team.contains mixed "\"ok\":false");
    assert (Team.contains mixed "refined earlier proposal");
    print_endline "collector: one invalid receipt query does not discard a healthy review";
    List.iteri
      [ correctness; documentation; integration; another ]
      ~f:(fun index response ->
        let stopped =
          call
            "agent_stop"
            [ "session_id", id response
            ; "idempotency_key", `String (sprintf "team-stop-%d" index)
            ; "mode", `String "cancel"
            ]
        in
        assert (Team.contains stopped "progress");
        let child = P.Id.Session.of_json (id response) |> protocol_ok in
        let current = state child in
        [%test_eq: P.Session.desired_state] Stopped current.lifecycle.desired;
        assert (Option.is_none current.active_operation));
    let retained = collect [ next_request ] in
    assert (Team.contains retained "refined earlier proposal");
    print_endline
      "cleanup: stop requests admitted; no active child operations; retained evidence \
       readable");
  [%expect
    {|
    collector: three distinct sessions; receipt-filtered evidence and one failed reviewer retained
    optional tool: default one-off, omitted ID creates another instance, supplied ID retains history with a new receipt
    collector: one invalid receipt query does not discard a healthy review
    cleanup: stop requests admitted; no active child operations; retained evidence readable
    |}]
;;
