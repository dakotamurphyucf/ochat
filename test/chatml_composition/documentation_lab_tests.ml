open Core
open Agent_server_test_support
module P = Agent_protocol
module Host = Embedded_extension_tests
module E = Agent_server.Embedded
module Workflow = Documentation_workflow_tests

let sources =
  [ ( "agent.chatmd"
    , [%blob "../../docs-src/examples/applications/documentation-lab/lab.chatmd"] )
  ; ( "agents/writer.chatmd"
    , [%blob
        "../../docs-src/examples/applications/documentation-lab/agents/writer.chatmd"] )
  ; ( "runtimes/tutorial-checks.chatmd"
    , [%blob
        "../../docs-src/examples/applications/documentation-lab/runtimes/tutorial-checks.chatmd"]
    )
  ; ( "scripts/coordinator.chatml"
    , [%blob
        "../../docs-src/examples/applications/documentation-lab/scripts/coordinator.chatml"]
    )
  ; ( "scripts/check-tutorial.chatml"
    , [%blob
        "../../docs-src/examples/applications/documentation-lab/scripts/check-tutorial.chatml"]
    )
  ; ( "scripts/probe-review.chatml"
    , [%blob
        "../../docs-src/examples/applications/documentation-lab/scripts/probe-review.chatml"]
    )
  ; ( "schemas/phase.json"
    , [%blob "../../docs-src/examples/applications/documentation-lab/schemas/phase.json"]
    )
  ; ( "schemas/proposal.json"
    , [%blob
        "../../docs-src/examples/applications/documentation-lab/schemas/proposal.json"] )
  ; ( "schemas/watch.json"
    , [%blob "../../docs-src/examples/applications/documentation-lab/schemas/watch.json"]
    )
  ; ( "schemas/object.json"
    , [%blob "../../docs-src/examples/applications/documentation-lab/schemas/object.json"]
    )
  ; ( "schemas/check-result.json"
    , [%blob
        "../../docs-src/examples/applications/documentation-lab/schemas/check-result.json"]
    )
  ; ( "schemas/probe-result.json"
    , [%blob
        "../../docs-src/examples/applications/documentation-lab/schemas/probe-result.json"]
    )
  ; ( "schemas/lab-report.json"
    , [%blob
        "../../docs-src/examples/applications/documentation-lab/schemas/lab-report.json"]
    )
  ; ( "schemas/empty.json"
    , [%blob "../../docs-src/examples/applications/documentation-lab/schemas/empty.json"]
    )
  ]
;;

let workspace_files =
  [ ( "sample-project/tutorial-inventory.json"
    , [%blob
        "../../docs-src/examples/applications/documentation-lab/sample-project/tutorial-inventory.json"]
    )
  ; ( "sample-project/tutorials/passing.md"
    , [%blob
        "../../docs-src/examples/applications/documentation-lab/sample-project/tutorials/passing.md"]
    )
  ; ( "sample-project/tutorials/broken.md"
    , [%blob
        "../../docs-src/examples/applications/documentation-lab/sample-project/tutorials/broken.md"]
    )
  ; ( "sample-project/checks/check-tutorial.sh"
    , [%blob
        "../../docs-src/examples/applications/documentation-lab/sample-project/checks/check-tutorial.sh"]
    )
  ; ( "sample-project/checks/stage-proposal.sh"
    , [%blob
        "../../docs-src/examples/applications/documentation-lab/sample-project/checks/stage-proposal.sh"]
    )
  ; ( "sample-project/staging/README.txt"
    , [%blob
        "../../docs-src/examples/applications/documentation-lab/sample-project/staging/README.txt"]
    )
  ]
;;

let field json name = Jsonaf.member_exn name json
let text json name = field json name |> Jsonaf.string_exn

let expected =
  [%blob
    "../../docs-src/examples/applications/documentation-lab/sample-project/expected/original.json"]
;;

let reviewer =
  [%blob "../../docs-src/examples/applications/documentation-lab/agents/reviewer.chatmd"]
;;

let pending_job = function
  | P.Invocation.Pending (Job id, _) -> id
  | outcome -> raise_s [%sexp "expected pending job", (outcome : P.Invocation.outcome)]
;;

let await_job env host id =
  let job () =
    List.find_exn (Host.snapshot host).jobs ~f:(fun job -> P.Id.Job.equal job.id id)
  in
  (try
     Background_shell_tests.wait env (fun () ->
       match (job ()).delivery with
       | Delivered _ -> Option.is_none (Host.snapshot host).session.active_operation
       | _ -> false)
   with
   | Eio.Time.Timeout ->
     let snapshot = Host.snapshot host in
     raise_s
       [%sexp
         "lab job did not deliver"
       , (job () : P.Job.t)
       , (snapshot.permissions : P.Permission.t list)]);
  P.Job.terminal_completion (job ()) |> protocol_ok |> Option.value_exn
;;

let%expect_test
    "lab checks and approved staging preserve originals and change real recheck evidence"
  =
  let queued = ref [] in
  let provider ~sw:_ ~inputs:_ =
    let calls = !queued in
    queued := [];
    Fixtures.call_events calls
  in
  Host.with_host
    ~durable:true
    ~sources
    ~workspace_files
    ~permission_profile:
      { (E.interactive_permission_profile ~authorize_shell_manifest:true) with
        tool_default = Allow
      }
    ~daemon_options:
      { Agent_server.Daemon.default_options with model_post_stream = Some provider }
    (fun env workspace host ->
       let invoke id name fields =
         queued := [ id, name, `Object fields ];
         Workflow.send host id "Perform the requested lab operation.";
         Workflow.finish_call env host id;
         Host.initial_outcome (Host.snapshot host) id
       in
       let begin_checks id phase =
         invoke id "begin_lab_checks" [ "phase", `String phase ] |> pending_job
       in
       let original = begin_checks "original" "original" in
       (match await_job env host original with
        | Succeeded value ->
          assert (Jsonaf.exactly_equal value (Jsonaf.of_string expected))
        | result -> raise_s [%sexp (result : P.Completion.t)]);
       let original_path =
         Eio.Path.(Eio.Stdenv.fs env / workspace / "sample-project/tutorials/broken.md")
       in
       let original_text = Eio.Path.load original_path in
       let staged =
         Eio.Path.(Eio.Stdenv.fs env / workspace / "sample-project/staging/broken.md")
       in
       let proposal =
         "## Verification\n\n\
          Expected result: the staged verification-v1 check passes its two mechanical \
          conventions."
       in
       let stage id tutorial =
         let job =
           invoke
             id
             "stage_lab_proposal"
             [ "arguments", `Array [ `String tutorial; `String proposal ] ]
           |> pending_job
         in
         await_job env host job
       in
       (match stage "reject-stage" "passing" with
        | Succeeded (`String value) ->
          let shell = Shell_runtime.Result.t_of_jsonaf (Jsonaf.of_string value) in
          assert (Shell_runtime.Result.equal_status shell.status (Exited 2))
        | result -> raise_s [%sexp (result : P.Completion.t)]);
       assert (not (Eio.Path.is_file staged));
       (match stage "apply-stage" "broken" with
        | Succeeded (`String value) ->
          let shell = Shell_runtime.Result.t_of_jsonaf (Jsonaf.of_string value) in
          assert (Shell_runtime.Result.equal_status shell.status (Exited 0))
        | result -> raise_s [%sexp (result : P.Completion.t)]);
       assert (String.is_substring (Eio.Path.load staged) ~substring:proposal);
       [%test_eq: string] original_text (Eio.Path.load original_path);
       (match await_job env host (begin_checks "recheck" "staged") with
        | Succeeded value ->
          let checks = field value "checks" |> Jsonaf.list_exn in
          assert (
            List.for_all checks ~f:(fun check ->
              String.equal (text check "status") "passed"));
          [%test_eq: string]
            "staging/broken.md"
            (text (List.nth_exn checks 1) "evidence_path")
        | result -> raise_s [%sexp (result : P.Completion.t)]);
       let report = invoke "report" "lab_report" [] |> Workflow.complete in
       [%test_eq: int] 4 (field report "runs" |> Jsonaf.list_exn |> List.length);
       let history = (Host.snapshot host).canonical_history.entries in
       let ack, _ =
         List.findi_exn history ~f:(fun _ entry ->
           match
             Agent_session.History_codec.of_protocol entry
             |> protocol_ok
             |> History_entry.item
           with
           | Openai.Responses.Item.Function_call_output { call_id = "original"; _ } ->
             true
           | _ -> false)
       in
       let notification, _ =
         List.findi_exn history ~f:(fun _ entry ->
           match entry.P.History.provenance with
           | Runtime_notification _ -> true
           | _ -> false)
       in
       assert (ack < notification);
       let closed = invoke "close" "close_lab" [] |> Workflow.complete in
       assert (Jsonaf.bool_exn (field closed "closed"));
       (match
          invoke "closed-start" "begin_lab_checks" [ "phase", `String "original" ]
        with
        | Fail error -> [%test_eq: string] "lab.unavailable" error.code
        | outcome -> raise_s [%sexp (outcome : P.Invocation.outcome)]);
       print_endline
         "original: one pass and one failure; acknowledgement precedes terminal \
          notification";
       print_endline
         "staging: unsupported target writes nothing; fixed capability writes only a \
          staged copy; real recheck passes";
       print_endline
         "report: four actual run results retained; closed lab rejects another start");
  [%expect
    {|
    original: one pass and one failure; acknowledgement precedes terminal notification
    staging: unsupported target writes nothing; fixed capability writes only a staged copy; real recheck passes
    report: four actual run results retained; closed lab rejects another start
    |}]
;;
