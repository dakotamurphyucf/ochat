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
  ]
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
      { Agent_server.Daemon.default_options with model_post_stream = Some post_stream }
    (fun env workspace host ->
       let call id name input =
         queued := [ id, name, input ];
         Workflow.send host id "Investigate the source and check its actual evidence.";
         Workflow.finish_call env host id;
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
       Workflow.finish_call env host "replace";
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

(* The legacy authored runner has its own transport injection seam. Exercise its
   real binding with an explicit callback rather than letting an embedded-host
   model override accidentally fall through to a real provider. *)
let%expect_test
    "engineering model variant separates policy review from specialist evidence handoff"
  =
  let module R = Chat_response.Agent_runtime in
  let module CM = Prompt.Chat_markdown in
  let ok = function
    | Ok value -> value
    | Error diagnostics ->
      List.map diagnostics ~f:R.diagnostic_to_string
      |> String.concat ~sep:"\n"
      |> failwith
  in
  Eio_main.run (fun env ->
    let name = temporary_root env |> Eio_posix.Low_level.realpath in
    let root = Eio.Path.(Eio.Stdenv.fs env / name) in
    Exn.protect
      ~finally:(fun () -> Eio.Path.rmtree root)
      ~f:(fun () ->
        let save (path, text) =
          Eio.Path.mkdirs
            ~exists_ok:true
            ~perm:0o700
            Eio.Path.(root / Filename.dirname path);
          Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(root / path) text
        in
        List.iter (sources @ Workflow.workspace_files) ~f:save;
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(root / "reports");
        let prompt_elements =
          CM.parse_chat_inputs ~source:"model-engineer.chatmd" ~dir:root model_engineer
        in
        let cache = Chat_response.Cache.create ~max_size:1 () in
        let ctx = Chat_response.Ctx.create ~env ~dir:root ~tool_dir:root ~cache in
        let host =
          R.host
            ~env
            ~workspace:root
            ~tool_dir:root
            ~prompt_dir:root
            ~session_dir:root
            ~cache_dir:root
            ~home:root
            ~session_id:"engineering-model"
            ~prompt_elements
          |> ok
        in
        let reviews = ref 0 in
        let handoffs = ref 0 in
        let evidence = ref "" in
        let run_agent ?prompt_dir:_ ?session_id:_ ?observer:_ ~source ~ctx:_ prompt items =
          let parsed = CM.parse_chat_inputs ~source:"reviewer.chatmd" ~dir:root prompt in
          assert (
            not
              (List.exists parsed ~f:(function
                 | CM.Tool _ -> true
                 | _ -> false)));
          if String.is_prefix source ~prefix:"shell-model-reviewer:"
          then (
            Int.incr reviews;
            match !reviews with
            | 1 -> {|{"decision":"allow_once"}|}
            | _ -> "malformed decision")
          else (
            Int.incr handoffs;
            assert (
              String.is_substring
                prompt
                ~substring:"Lantern's engineering evidence reviewer");
            (match items with
             | [ CM.Basic { text = Some text; _ } ] -> [%test_eq: string] !evidence text
             | _ -> failwith "specialist did not receive the supplied evidence");
            "Proposal: add verification guidance supported by docs/setup.md and the \
             failing check.")
        in
        Eio.Switch.run (fun sw ->
          let resources =
            R.prepare_extensions
              ~sw
              ~ctx
              ~host
              ~platform:(R.platform ())
              ~prompt_elements
              ~manifest_authorizer:Shell_runtime.Manifest_authorizer.assume_authorized
              ~approval_provider:Shell_runtime.Approval_broker.None_available
              ~approval_store:(Shell_access.Approval.create_store ())
              ~run_agent
              ()
            |> ok
          in
          let runtime = resources.native in
          let call name input =
            let tool =
              List.find_exn runtime.functions ~f:(fun t ->
                String.equal t.Ochat_function.info.function_.name name)
            in
            match tool.run (Jsonaf.to_string input) with
            | Text text -> text
            | _ -> failwith "expected text output"
          in
          let denied input =
            let json = call "check_docs" input |> Jsonaf.of_string in
            [%test_eq: string]
              "denied"
              (Jsonaf.member_exn "error" json
               |> Jsonaf.member_exn "code"
               |> Jsonaf.string_exn)
          in
          denied (arguments [ "--check"; "links"; "--write-report" ]);
          [%test_eq: int] 0 !reviews;
          let result =
            call "check_docs" (arguments [ "--check"; "all"; "--write-report" ])
            |> Jsonaf.of_string
            |> Shell_runtime.Result.t_of_jsonaf
          in
          assert (
            Jsonaf.exactly_equal
              (Jsonaf.of_string Documentation_shell_tests.expected_report)
              (Jsonaf.of_string result.stdout));
          evidence
          := "docs/setup.md\n"
             ^ Documentation_shell_tests.setup
             ^ "\nActual checker stdout:\n"
             ^ result.stdout;
          let result = call "review_docs" (`Object [ "input", `String !evidence ]) in
          assert (String.is_prefix result ~prefix:"Proposal:");
          [%test_eq: int] 1 !handoffs;
          denied (arguments [ "--check"; "all"; "--write-report" ]);
          [%test_eq: int] 2 !reviews;
          assert (
            Jsonaf.exactly_equal
              (Jsonaf.of_string Documentation_shell_tests.expected_report)
              (Jsonaf.of_string (Eio.Path.load Eio.Path.(root / "reports/latest.json"))));
          print_endline
            "static policy precedes model review; simulated approval runs real checks";
          print_endline
            "tool-free specialist receives exact evidence; malformed model decision \
             preserves report")));
  [%expect
    {|
    static policy precedes model review; simulated approval runs real checks
    tool-free specialist receives exact evidence; malformed model decision preserves report
    |}]
;;
