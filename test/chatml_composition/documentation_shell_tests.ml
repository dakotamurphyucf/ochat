open Core
open Fixtures

let agent = [%blob "../../docs-src/examples/learning/shell-inspection/agent.chatmd"]

let runtime =
  [%blob "../../docs-src/examples/learning/shell-inspection/runtimes/inspection.chatmd"]
;;

let setup =
  [%blob "../../docs-src/examples/learning/lantern/sample-project/docs/setup.md"]
;;

let reference =
  [%blob "../../docs-src/examples/learning/lantern/sample-project/docs/reference.md"]
;;

let checker =
  [%blob "../../docs-src/examples/learning/lantern/sample-project/scripts/check-docs.sh"]
;;

let expected_report =
  [%blob "../../docs-src/examples/learning/lantern/sample-project/expected-report.json"]
;;

let workspace_files =
  [ "sample-project/docs/setup.md", setup
  ; "sample-project/docs/reference.md", reference
  ; "sample-project/scripts/check-docs.sh", checker
  ]
;;

let guardrail_sources =
  [ ( "agent.chatmd"
    , [%blob "../../docs-src/examples/learning/shell-guardrails/agent.chatmd"] )
  ; ( "runtimes/inspection.chatmd"
    , [%blob
        "../../docs-src/examples/learning/shell-guardrails/runtimes/inspection.chatmd"] )
  ; ( "runtimes/checks.chatmd"
    , [%blob "../../docs-src/examples/learning/shell-guardrails/runtimes/checks.chatmd"] )
  ]
;;

let%expect_test "public checker separates inspection, checks and approved report writes" =
  let module E = Agent_server.Embedded in
  let module P = Agent_protocol in
  let module Host = Embedded_extension_tests in
  let shell_result snapshot id =
    match Host.initial_outcome snapshot id with
    | Complete (`String text) -> Shell_runtime.Result.t_of_jsonaf (Jsonaf.of_string text)
    | other -> raise_s [%sexp (other : I.outcome)]
  in
  List.iter [ true; false ] ~f:(fun approve ->
    let requests = ref 0 in
    let args values =
      `Object [ "arguments", `Array (List.map values ~f:(fun s -> `String s)) ]
    in
    let post_stream ~sw:_ ~inputs:_ =
      incr requests;
      match !requests with
      | 1 ->
        call_events
          [ "inspect", "inspect_setup", `Object []
          ; "check", "check_docs", args [ "--check"; "all" ]
          ; "invalid-check", "check_docs", args [ "--check"; "all; touch injected" ]
          ; "invalid-inspect", "inspect_setup", args [ "../reports/latest.json" ]
          ; "save", "check_docs", args [ "--check"; "all"; "--write-report" ]
          ]
      | 2 -> Stdlib.Seq.empty
      | _ -> failwith "unexpected checker model request"
    in
    let daemon_options =
      { Agent_server.Daemon.default_options with model_post_stream = Some post_stream }
    in
    Host.with_host
      ~durable:false
      ~sources:guardrail_sources
      ~workspace_files
      ~permission_profile:
        (E.interactive_permission_profile ~authorize_shell_manifest:true)
      ~daemon_options
      (fun env workspace host ->
         let report_path =
           Eio.Path.(Eio.Stdenv.fs env / workspace / "reports/latest.json")
         in
         let pending = ref None in
         Host.send host "Check Lantern and save the evidence.";
         (try
            Background_shell_tests.wait env (fun () ->
              let snapshot = Host.snapshot host in
              pending
              := List.find snapshot.permissions ~f:(fun p ->
                   P.Permission.equal_state p.state Pending);
              Option.is_some !pending
              && List.count snapshot.canonical_history.entries ~f:(fun entry ->
                   P.History.equal_kind entry.kind Tool_output)
                 = 4)
          with
          | Eio.Time.Timeout -> raise_s [%sexp (Host.snapshot host : P.Snapshot.t)]);
         assert (not (Eio.Path.is_file report_path));
         let snapshot = Host.snapshot host in
         [%test_eq: string] setup (shell_result snapshot "inspect").stdout;
         let check = shell_result snapshot "check" in
         assert (Shell_runtime.Result.equal_status check.status (Exited 1));
         assert (
           Jsonaf.exactly_equal
             (Jsonaf.of_string expected_report)
             (Jsonaf.of_string check.stdout));
         assert (
           List.mem
             [ "macos-seatbelt"; "linux-bubblewrap" ]
             check.backend
             ~equal:String.equal);
         let invalid = shell_result snapshot "invalid-check" in
         assert (Shell_runtime.Result.equal_status invalid.status (Exited 2));
         assert (
           not
             (Eio.Path.is_file
                Eio.Path.(Eio.Stdenv.fs env / workspace / "sample-project/injected")));
         (match Host.initial_outcome snapshot "invalid-inspect" with
          | Fail error -> [%test_eq: string] "invocation.invalid_input" error.code
          | other -> raise_s [%sexp (other : I.outcome)]);
         let permission = Option.value_exn !pending in
         [%test_eq: string] "shell:checks" permission.tool_name;
         Host.request
           host
           (Permission_respond
              { session_id = E.session_id host
              ; attachment_id = (E.attachment host).id
              ; permission_id = permission.id
              ; permission_generation = permission.generation
              ; choice = (if approve then Approve_once else Deny)
              ; reason = Some "documentation guardrails check"
              ; idempotency_key =
                  P.Idempotency_key.of_string "guardrails:decision"
                  |> Agent_server_test_support.protocol_ok
              })
         |> ignore;
         Background_shell_tests.wait env (fun () ->
           !requests = 2 && Option.is_none (Host.snapshot host).session.active_operation);
         let snapshot = Host.snapshot host in
         if approve
         then (
           let saved = shell_result snapshot "save" in
           assert (Shell_runtime.Result.equal_status saved.status (Exited 1));
           assert (
             Jsonaf.exactly_equal
               (Jsonaf.of_string expected_report)
               (Jsonaf.of_string (Eio.Path.load report_path)));
           print_endline
             "approved: real failed checks retained as a report; literal arguments never \
              become shell source")
         else (
           assert (not (Eio.Path.is_file report_path));
           (match Host.initial_outcome snapshot "save" with
            | Complete (`String text) ->
              let error = Jsonaf.member_exn "error" (Jsonaf.of_string text) in
              [%test_eq: string]
                "denied"
                (Jsonaf.member_exn "code" error |> Jsonaf.string_exn)
            | other -> raise_s [%sexp (other : I.outcome)]);
           print_endline "declined: inspection and checks finish; no report is written")));
  [%expect
    {|
    approved: real failed checks retained as a report; literal arguments never become shell source
    declined: inspection and checks finish; no report is written
    |}]
;;

let%expect_test
    "public shell inspection reads the actual project through the required sandbox"
  =
  with_daemon
    ~workspace_files:
      [ "sample-project/docs/setup.md", setup
      ; "sample-project/docs/reference.md", reference
      ]
    ~sources:[ "agent.chatmd", agent; "runtimes/inspection.chatmd", runtime ]
    ~calls:
      [ "read", "inspect_setup", `Object []
      ; ( "extra"
        , "inspect_setup"
        , `Object [ "arguments", `Array [ `String "../secret.json" ] ] )
      ]
    (fun state ->
       (match result state "read" with
        | Complete (`String text) ->
          let result = Shell_runtime.Result.t_of_jsonaf (Jsonaf.of_string text) in
          assert (Shell_runtime.Result.equal_status result.status (Exited 0));
          [%test_eq: string] setup result.stdout;
          assert (not result.stdout_truncated);
          assert (
            List.mem
              [ "macos-seatbelt"; "linux-bubblewrap" ]
              result.backend
              ~equal:String.equal)
        | other -> raise_s [%sexp (other : I.outcome)]);
       (match result state "extra" with
        | Fail error -> [%test_eq: string] "invocation.invalid_input" error.code
        | other -> raise_s [%sexp (other : I.outcome)]);
       print_endline
         "fixed command reads the complete sample through required confinement; extra \
          arguments reject before execution");
  [%expect
    {| fixed command reads the complete sample through required confinement; extra arguments reject before execution |}]
;;
