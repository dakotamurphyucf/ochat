open Core
open Fixtures
module Host = Embedded_extension_tests
module P = Agent_protocol
module E = Agent_server.Embedded

let sources =
  [ ( "agent.chatmd"
    , [%blob "../../docs-src/examples/learning/shell-customization/agent.chatmd"] )
  ; ( "tools.chatmd"
    , [%blob "../../docs-src/examples/learning/shell-customization/tools.chatmd"] )
  ; ( "runtimes/inspection.chatmd"
    , [%blob
        "../../docs-src/examples/learning/shell-customization/runtimes/inspection.chatmd"]
    )
  ; ( "runtimes/checks-base.chatmd"
    , [%blob
        "../../docs-src/examples/learning/shell-customization/runtimes/checks-base.chatmd"]
    )
  ; ( "runtimes/checks.chatmd"
    , [%blob
        "../../docs-src/examples/learning/shell-customization/runtimes/checks.chatmd"] )
  ; ( "scripts/report-reviewer.chatml"
    , [%blob
        "../../docs-src/examples/learning/shell-customization/scripts/report-reviewer.chatml"]
    )
  ]
;;

let denied snapshot id =
  match Host.initial_outcome snapshot id with
  | Complete (`String text) ->
    let error = Jsonaf.member_exn "error" (Jsonaf.of_string text) in
    [%test_eq: string] "denied" (Jsonaf.member_exn "code" error |> Jsonaf.string_exn)
  | other -> raise_s [%sexp (other : I.outcome)]
;;

let%expect_test "public ChatML reviewer retains decisions and defers later report writes" =
  let requests = ref 0 in
  let post_stream ~sw:_ ~inputs:_ =
    incr requests;
    let call id category =
      call_events
        [ ( id
          , "check_docs"
          , `Object
              [ ( "arguments"
                , `Array [ `String "--check"; `String category; `String "--write-report" ]
                )
              ] )
        ]
    in
    match !requests with
    | 1 -> call "selective" "links"
    | 2 -> call "first-full" "all"
    | 3 -> call "later-full" "all"
    | 4 -> Stdlib.Seq.empty
    | _ -> failwith "unexpected reviewer model request"
  in
  Host.with_host
    ~durable:false
    ~sources
    ~workspace_files:Documentation_shell_tests.workspace_files
    ~permission_profile:(E.interactive_permission_profile ~authorize_shell_manifest:true)
    ~daemon_options:
      { Agent_server.Daemon.default_options with model_post_stream = Some post_stream }
    (fun env workspace host ->
       Host.send host "Save full check evidence, then ask before replacing it.";
       let pending = ref None in
       (try
          Background_shell_tests.wait env (fun () ->
            pending
            := List.find (Host.snapshot host).permissions ~f:(fun permission ->
                 P.Permission.equal_state permission.state Pending);
            !requests = 3 && Option.is_some !pending)
        with
        | Eio.Time.Timeout -> raise_s [%sexp (Host.snapshot host : P.Snapshot.t)]);
       let snapshot = Host.snapshot host in
       denied snapshot "selective";
       (match Host.initial_outcome snapshot "first-full" with
        | Complete (`String text) ->
          let result = Shell_runtime.Result.t_of_jsonaf (Jsonaf.of_string text) in
          assert (Shell_runtime.Result.equal_status result.status (Exited 1));
          assert (
            Jsonaf.exactly_equal
              (Jsonaf.of_string result.stdout)
              (Jsonaf.of_string Documentation_shell_tests.expected_report))
        | other -> raise_s [%sexp (other : I.outcome)]);
       let report = Eio.Path.(Eio.Stdenv.fs env / workspace / "reports/latest.json") in
       assert (
         Jsonaf.exactly_equal
           (Jsonaf.of_string (Eio.Path.load report))
           (Jsonaf.of_string Documentation_shell_tests.expected_report));
       Eio.Path.save
         ~create:(`Or_truncate 0o600)
         report
         "Retain this existing evidence.\n";
       let permission = Option.value_exn !pending in
       Host.request
         host
         (Permission_respond
            { session_id = E.session_id host
            ; attachment_id = (E.attachment host).id
            ; permission_id = permission.id
            ; permission_generation = permission.generation
            ; choice = Deny
            ; reason = Some "Keep the existing report"
            ; idempotency_key =
                P.Idempotency_key.of_string "reviewer:deny"
                |> Agent_server_test_support.protocol_ok
            })
       |> ignore;
       Background_shell_tests.wait env (fun () ->
         !requests = 4 && Option.is_none (Host.snapshot host).session.active_operation);
       denied (Host.snapshot host) "later-full";
       [%test_eq: string] "Retain this existing evidence.\n" (Eio.Path.load report);
       print_endline "selective report denied; first full report approved by ChatML";
       print_endline "later request reaches user; denial preserves existing evidence");
  [%expect
    {|
    selective report denied; first full report approved by ChatML
    later request reaches user; denial preserves existing evidence
    |}]
;;

let model_agent =
  [%blob "../../docs-src/examples/learning/shell-customization/model-agent.chatmd"]
;;

let model_runtime =
  [%blob
    "../../docs-src/examples/learning/shell-customization/runtimes/model-checks.chatmd"]
;;

let%expect_test "public model-review variant preserves policy and fails closed offline" =
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
    let root_name = Agent_server_test_support.temporary_root env in
    let root_name = Eio_posix.Low_level.realpath root_name in
    let root = Eio.Path.(Eio.Stdenv.fs env / root_name) in
    Exn.protect
      ~finally:(fun () -> Eio.Path.rmtree root)
      ~f:(fun () ->
        let save (name, text) =
          let path = Eio.Path.(root / name) in
          Eio.Path.mkdirs
            ~exists_ok:true
            ~perm:0o700
            Eio.Path.(root / Filename.dirname name);
          Eio.Path.save ~create:(`Or_truncate 0o600) path text
        in
        List.iter
          (sources
           @ Documentation_shell_tests.workspace_files
           @ [ "model-agent.chatmd", model_agent
             ; "runtimes/model-checks.chatmd", model_runtime
             ])
          ~f:save;
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(root / "reports");
        let prompt_elements =
          CM.parse_chat_inputs ~source:"model-agent.chatmd" ~dir:root model_agent
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
            ~session_id:"docs-model-review"
            ~prompt_elements
          |> ok
        in
        let review_calls = ref 0 in
        let run_agent ?prompt_dir:_ ?session_id:_ ?observer:_ ~source ~ctx:_ prompt items =
          incr review_calls;
          [%test_eq: string] "shell-model-reviewer:lantern-report-review" source;
          assert (String.is_substring prompt ~substring:"gpt-6-astra");
          let reviewer =
            CM.parse_chat_inputs ~source:"reviewer.chatmd" ~dir:root prompt
          in
          assert (
            not
              (List.exists reviewer ~f:(function
                 | CM.Tool _ -> true
                 | _ -> false)));
          assert (not (List.is_empty items));
          match !review_calls with
          | 1 -> {|{"decision":"allow_once"}|}
          | 2 -> "not a decision"
          | _ -> failwith "unexpected nested model review"
        in
        Eio.Switch.run (fun sw ->
          let runtime =
            R.create
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
          let tool =
            List.find_exn runtime.functions ~f:(fun tool ->
              String.equal tool.Ochat_function.info.function_.name "check_docs")
          in
          let invoke category =
            let input =
              `Object
                [ ( "arguments"
                  , `Array
                      [ `String "--check"; `String category; `String "--write-report" ] )
                ]
            in
            match tool.run (Jsonaf.to_string input) with
            | Text text -> Jsonaf.of_string text
            | _ -> failwith "expected shell result text"
          in
          let expect_denied result =
            [%test_eq: string]
              "denied"
              (Jsonaf.member_exn "error" result
               |> Jsonaf.member_exn "code"
               |> Jsonaf.string_exn)
          in
          expect_denied (invoke "links");
          [%test_eq: int] 0 !review_calls;
          let result = invoke "all" in
          let stdout = Jsonaf.member_exn "stdout" result |> Jsonaf.string_exn in
          if String.is_empty stdout then raise_s [%sexp (result : Jsonaf.t)];
          assert (
            Jsonaf.exactly_equal
              (Jsonaf.of_string stdout)
              (Jsonaf.of_string Documentation_shell_tests.expected_report));
          [%test_eq: int] 1 !review_calls;
          save ("reports/latest.json", "Preserve earlier evidence.\n");
          expect_denied (invoke "all");
          [%test_eq: int] 2 !review_calls;
          [%test_eq: string]
            "Preserve earlier evidence.\n"
            (Eio.Path.load Eio.Path.(root / "reports/latest.json"));
          print_endline
            "policy rejects before review; stock adapter uses a tool-free prompt";
          print_endline
            "stub approval executes real checks; malformed review preserves report")));
  [%expect
    {|
    policy rejects before review; stock adapter uses a tool-free prompt
    stub approval executes real checks; malformed review preserves report
    |}]
;;
