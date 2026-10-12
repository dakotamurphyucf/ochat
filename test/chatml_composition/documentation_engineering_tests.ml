open Core
open Agent_server_test_support
module Host = Embedded_extension_tests
module Workflow = Documentation_workflow_tests
module P = Agent_protocol
module E = Agent_server.Embedded

let engineer =
  [%blob "../../docs-src/examples/applications/guarded-engineering/engineer.chatmd"]
;;

let model_engineer =
  [%blob "../../docs-src/examples/applications/guarded-engineering/model-engineer.chatmd"]
;;

let sources =
  [ "agent.chatmd", engineer
  ; "model-engineer.chatmd", model_engineer
  ; ( "capabilities.chatmd"
    , [%blob
        "../../docs-src/examples/applications/guarded-engineering/capabilities.chatmd"] )
  ; ( "runtimes/inspection.chatmd"
    , [%blob
        "../../docs-src/examples/applications/guarded-engineering/runtimes/inspection.chatmd"]
    )
  ; ( "agents/reviewer.chatmd"
    , [%blob
        "../../docs-src/examples/applications/guarded-engineering/agents/reviewer.chatmd"]
    )
  ; "runtimes/model-checks.chatmd", Documentation_shell_review_tests.model_runtime
  ; "scripts/check-report.chatml", Documentation_program_tests.source
  ; "schemas/check-input.json", Documentation_program_tests.input_schema
  ; "schemas/check-output.json", Documentation_program_tests.output_schema
  ; ( "chatml.project.json"
    , [%blob
        "../../docs-src/examples/applications/guarded-engineering/chatml.project.json"] )
  ]
  @ Documentation_program_tests.decoder_sources
  @ List.filter Documentation_shell_review_tests.sources ~f:(fun (name, _) ->
    not
      (List.mem [ "agent.chatmd"; "runtimes/inspection.chatmd" ] name ~equal:String.equal))
;;

let arguments values =
  `Object [ "arguments", `Array (List.map values ~f:(fun value -> `String value)) ]
;;

let shell_result snapshot id =
  match Host.initial_outcome snapshot id with
  | Complete (`String text) -> Shell_runtime.Result.t_of_jsonaf (Jsonaf.of_string text)
  | other -> raise_s [%sexp (other : P.Invocation.outcome)]
;;

let%expect_test
    "engineering application connects search, guarded reports and script processing"
  =
  Mirage_crypto_rng_unix.use_default ();
  let queued = ref [] in
  let post_stream ~sw:_ ~inputs:_ =
    let calls = !queued in
    queued := [];
    Fixtures.call_events calls
  in
  Host.with_host
    ~durable:false
    ~sources
    ~workspace_files:Workflow.workspace_files
    ~permission_profile:(E.interactive_permission_profile ~authorize_shell_manifest:true)
    ~daemon_options:
      { Agent_server.Daemon.default_options with
        inference_policy =
          Agent_server_test_support.inference_policy
            ~default_model:"fixture-model"
            ~post_stream
      }
    (fun env workspace host ->
       (* Durable workflow completion is separate from individual script/tool limits.
          Match the neighboring lab fixture's bound; CI completed the denied turn
          during cleanup after the shared five-second waiter had expired. *)
       let finish_call = Workflow.finish_call ~timeout:30. in
       let call id name input =
         queued := [ id, name, input ];
         Workflow.send host id "Investigate the source and check its actual evidence.";
         finish_call env host id;
         Host.initial_outcome (Host.snapshot host) id
       in
       call "inspect" "inspect_setup" (`Object []) |> ignore;
       [%test_eq: string]
         Documentation_shell_tests.setup
         (shell_result (Host.snapshot host) "inspect").stdout;
       call "search" "search_docs" (arguments [ "Verification"; "docs" ]) |> ignore;
       let search = shell_result (Host.snapshot host) "search" in
       assert (Shell_runtime.Result.equal_status search.status (Exited 0));
       assert (String.is_substring search.stdout ~substring:"docs/reference.md");
       call
         "search-escape"
         "search_docs"
         (arguments [ "config"; "../../capabilities.chatmd" ])
       |> ignore;
       (match Host.initial_outcome (Host.snapshot host) "search-escape" with
        | Complete (`String text) ->
          [%test_eq: string]
            "capability_violation"
            (Jsonaf.of_string text
             |> Jsonaf.member_exn "error"
             |> Jsonaf.member_exn "code"
             |> Jsonaf.string_exn)
        | other -> raise_s [%sexp (other : P.Invocation.outcome)]);
       (match
          call
            "escape"
            "read_file"
            (`Object
                [ "root", `String "project"; "file", `String "../../capabilities.chatmd" ])
        with
        | Complete (`String text) ->
          assert (String.is_substring text ~substring:"outside the configured read roots")
        | other -> raise_s [%sexp (other : P.Invocation.outcome)]);
       call "selective" "check_docs" (arguments [ "--check"; "links"; "--write-report" ])
       |> ignore;
       Documentation_shell_review_tests.denied (Host.snapshot host) "selective";
       let report = Eio.Path.(Eio.Stdenv.fs env / workspace / "reports/latest.json") in
       assert (not (Eio.Path.is_file report));
       call "full" "check_docs" (arguments [ "--check"; "all"; "--write-report" ])
       |> ignore;
       assert (
         Shell_runtime.Result.equal_status
           (shell_result (Host.snapshot host) "full").status
           (Exited 1));
       assert (
         Jsonaf.exactly_equal
           (Jsonaf.of_string (Eio.Path.load report))
           (Jsonaf.of_string Documentation_shell_tests.expected_report));
       let summary =
         call
           "summary"
           "summarize_checks"
           (`Object [ "files", `Array [ `String "latest.json" ] ])
         |> Workflow.complete
       in
       print_endline (Jsonaf.to_string summary);
       let before = Eio.Path.load report in
       queued
       := [ "replace", "check_docs", arguments [ "--check"; "all"; "--write-report" ] ];
       Workflow.send host "replace" "Request another full saved report.";
       let permission = ref None in
       Background_shell_tests.wait env (fun () ->
         permission
         := List.find (Host.snapshot host).permissions ~f:(fun p ->
              P.Permission.equal_state p.state Pending);
         Option.is_some !permission);
       let permission = Option.value_exn !permission in
       Host.request
         host
         (Permission_respond
            { session_id = E.session_id host
            ; attachment_id = (E.attachment host).id
            ; permission_id = permission.id
            ; permission_generation = permission.generation
            ; choice = Deny
            ; reason = Some "Keep the first report"
            ; idempotency_key =
                P.Idempotency_key.of_string "engineering-deny" |> protocol_ok
            })
       |> ignore;
       finish_call env host "replace";
       Documentation_shell_review_tests.denied (Host.snapshot host) "replace";
       [%test_eq: string] before (Eio.Path.load report);
       [%test_eq: string]
         Documentation_shell_tests.setup
         (Eio.Path.load
            Eio.Path.(Eio.Stdenv.fs env / workspace / "sample-project/docs/setup.md"));
       print_endline
         "real search; scoped read/search denial; selective report denied; full evidence \
          saved and summarized";
       print_endline
         "later report reaches user; denial preserves report and source; no specialist \
          API called");
  [%expect
    {|
    [{"check":"verification steps","failures":1}]
    real search; scoped read/search denial; selective report denied; full evidence saved and summarized
    later report reaches user; denial preserves report and source; no specialist API called
    |}]
;;

(* The configured model variant uses the same real captured host as the
   application. Controlled inference distinguishes each actual consumer. *)
let%expect_test
    "engineering model variant separates policy review from specialist evidence handoff"
  =
  Mirage_crypto_rng_unix.use_default ();
  let queued = ref [] in
  let reviews = ref 0 in
  let handoffs = ref 0 in
  let evidence = ref "" in
  let has_user_text inputs text =
    List.exists inputs ~f:(function
      | Openai.Responses.Item.Input_message { role = User; content; _ } ->
        List.exists content ~f:(function
          | Openai.Responses.Input_message.Text { text = actual; _ } ->
            String.equal actual text
          | Image _ -> false)
      | Input_message { role = Assistant | System | Developer; _ }
      | Output_message _
      | Function_call _
      | Custom_tool_call _
      | Function_call_output _
      | Custom_tool_call_output _
      | Web_search_call _
      | File_search_call _
      | Reasoning _ -> false)
  in
  let provider_failure = ref None in
  let post_stream ~sw:_ ~inputs =
    try
      let request = `Array (List.map inputs ~f:Openai.Responses.Item.jsonaf_of_t) in
      if Documentation_agent_team_tests.contains request "engineering evidence reviewer"
      then (
        assert (Documentation_agent_team_tests.contains request "You have no tools");
        assert (has_user_text inputs !evidence);
        assert (not (String.is_empty !evidence));
        Int.incr handoffs;
        Documentation_agent_team_tests.answer
          ~id:("specialist-" ^ Int.to_string !handoffs)
          "Proposal: add verification guidance supported by docs/setup.md and the \
           failing check.")
      else if Documentation_agent_team_tests.contains request "approval reviewer"
      then (
        assert (Documentation_agent_team_tests.contains request "You have no tools");
        assert (
          not
            (Documentation_agent_team_tests.contains
               request
               "engineering evidence reviewer"));
        Int.incr reviews;
        let answer =
          match !reviews with
          | 1 -> {|{"decision":"allow_once"}|}
          | 2 -> "malformed decision"
          | _ -> failwith "unexpected shell reviewer request"
        in
        Documentation_agent_team_tests.answer
          ~id:("review-" ^ Int.to_string !reviews)
          answer)
      else if Documentation_agent_team_tests.contains request "model approval"
      then (
        let calls = !queued in
        queued := [];
        Fixtures.call_events calls)
      else failwith "unexpected engineering inference consumer"
    with
    | exn ->
      let backtrace = Backtrace.Exn.most_recent () in
      provider_failure := Some (exn, backtrace);
      Exn.raise_with_original_backtrace exn backtrace
  in
  let sources =
    List.map sources ~f:(fun (name, source) ->
      name, if String.equal name "agent.chatmd" then model_engineer else source)
  in
  Host.with_host
    ~durable:false
    ~sources
    ~workspace_files:Workflow.workspace_files
    ~permission_profile:(E.interactive_permission_profile ~authorize_shell_manifest:true)
    ~daemon_options:
      { Agent_server.Daemon.default_options with
        inference_policy =
          Agent_server_test_support.inference_policy
            ~default_model:"fixture-model"
            ~post_stream
      }
    (fun env workspace host ->
       (* This is the exact captured specialist source in the shipped bundle;
          its tool-free declaration is independent from the shell-review port. *)
       let reviewer =
         List.Assoc.find_exn sources ~equal:String.equal "agents/reviewer.chatmd"
       in
       let parsed =
         Prompt.Chat_markdown.parse_chat_inputs
           ~source:"agents/reviewer.chatmd"
           ~dir:Eio.Path.(Eio.Stdenv.fs env / Filename.dirname workspace)
           reviewer
       in
       assert (
         not
           (List.exists parsed ~f:(function
              | Prompt.Chat_markdown.Tool _ -> true
              | _ -> false)));
       let call id name input =
         queued := [ id, name, input ];
         Workflow.send host id "Inspect the actual engineering evidence.";
         Workflow.finish_call ~timeout:30. env host id;
         Option.iter !provider_failure ~f:(fun (exn, backtrace) ->
           Exn.raise_with_original_backtrace exn backtrace);
         Host.initial_outcome (Host.snapshot host) id
       in
       let denied id input =
         call id "check_docs" input |> ignore;
         Documentation_shell_review_tests.denied (Host.snapshot host) id
       in
       denied "selective" (arguments [ "--check"; "links"; "--write-report" ]);
       [%test_eq: int] 0 !reviews;
       [%test_eq: int] 0 !handoffs;
       call "full" "check_docs" (arguments [ "--check"; "all"; "--write-report" ])
       |> ignore;
       let result = shell_result (Host.snapshot host) "full" in
       assert (Shell_runtime.Result.equal_status result.status (Exited 1));
       assert (
         Jsonaf.exactly_equal
           (Jsonaf.of_string Documentation_shell_tests.expected_report)
           (Jsonaf.of_string result.stdout));
       evidence
       := "docs/setup.md\n"
          ^ Documentation_shell_tests.setup
          ^ "\nActual checker stdout:\n"
          ^ result.stdout;
       (match
          call "specialist" "review_docs" (`Object [ "input", `String !evidence ])
        with
        | Complete (`String text) -> assert (String.is_prefix text ~prefix:"Proposal:")
        | other -> raise_s [%sexp (other : P.Invocation.outcome)]);
       [%test_eq: int] 1 !handoffs;
       [%test_eq: int] 1 !reviews;
       let report = Eio.Path.(Eio.Stdenv.fs env / workspace / "reports/latest.json") in
       let before = Eio.Path.load report in
       denied "malformed" (arguments [ "--check"; "all"; "--write-report" ]);
       [%test_eq: int] 2 !reviews;
       [%test_eq: int] 1 !handoffs;
       [%test_eq: string] before (Eio.Path.load report);
       assert (
         Jsonaf.exactly_equal
           (Jsonaf.of_string Documentation_shell_tests.expected_report)
           (Jsonaf.of_string before));
       print_endline
         "static policy precedes model review; simulated approval runs real checks";
       print_endline
         "tool-free specialist receives exact evidence; malformed model decision \
          preserves report");
  [%expect
    {|
    static policy precedes model review; simulated approval runs real checks
    tool-free specialist receives exact evidence; malformed model decision preserves report
    |}]
;;
