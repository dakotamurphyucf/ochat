open Core
module E = Embedded_extension_tests
module P = Agent_protocol
module Q = Authoring_context_tests
module Flow = Authoring_compaction_tests
module Packages = Authoring_package_config_tests

let%expect_test
    "local hosts capture and scope the same package files as daemon configuration"
  =
  List.iter [ false; true ] ~f:(fun durable ->
    let requests = ref 0 in
    let inputs = ref [] in
    let post_stream ~sw:_ ~inputs:actual =
      incr requests;
      inputs := actual :: !inputs;
      match !requests with
      | 1 ->
        Fixtures.call_events
          [ ( "local-reference"
            , "ochat_authoring_context"
            , Q.request ~task:"one_off_script" ~topic_id:"custom.reports.rules" "topic" )
          ]
      | _ -> Stdlib.Seq.empty
    in
    E.with_host
      ~durable
      ~authoring_budget:
        (Chat_response.Authoring_validation.context_budget
           ~default_tokens:16000
           ~max_tokens:24000
           ~preload_tokens:20000
         |> Result.ok_or_failwith)
      ~package_files:[ "reports.json"; "private.json" ]
      ~sources:
        [ ( "reports.json"
          , Packages.encode [ Packages.package "reports" "LOCAL-CAPTURED-CONVENTIONS" ] )
        ; ( "private.json"
          , Packages.encode [ Packages.package "private" "LOCAL-PRIVATE-CONVENTIONS" ] )
        ; ( "agent.chatmd"
          , {|<developer>Use the local author conventions.</developer>
<authoring_context policy="preload" topics="custom.reports.rules"/>
<script id="author" language="chatml" kind="tool">let run ctx input = Task.pure(`Complete(input))</script>
<tool name="report_author" type="chatml" script="author" entrypoint="run" input_schema="any.json" output_schema="any.json"/>
<authoring_help tool="report_author" package="reports" tasks="one_off_script" topics="custom.reports.rules"/>|}
          )
        ; "any.json", {|{"type":"object","properties":{},"additionalProperties":false}|}
        ]
      ~daemon_options:
        { Agent_server.Daemon.default_options with
          qualify_chatml_extensions = true
        ; model_post_stream = Some post_stream
        }
      (fun env workspace embedded ->
         let root = Filename.dirname workspace in
         List.iter [ "reports.json"; "private.json" ] ~f:(fun file ->
           Eio.Path.unlink Eio.Path.(Eio.Stdenv.fs env / root / file));
         E.send embedded "Read the captured local conventions.";
         Background_shell_tests.wait env (fun () ->
           !requests >= 2 && Option.is_none (E.snapshot embedded).session.active_operation);
         [%test_eq: int] 2 !requests;
         List.iter !inputs ~f:(fun input ->
           let text =
             `Array (List.map input ~f:Openai.Responses.Item.jsonaf_of_t)
             |> Jsonaf.to_string
           in
           assert (String.is_substring text ~substring:"LOCAL-CAPTURED-CONVENTIONS");
           assert (not (String.is_substring text ~substring:"LOCAL-PRIVATE-CONVENTIONS")));
         let response = Flow.response (List.hd_exn !inputs) "local-reference" in
         assert (not (Q.has_error response));
         Q.require_json
           (`Number "16000")
           (Q.field (Q.field response "budget") "max_tokens");
         assert (
           String.is_substring
             (Flow.content [ response ])
             ~substring:"LOCAL-CAPTURED-CONVENTIONS");
         let snapshot = E.snapshot embedded in
         assert (List.is_empty snapshot.jobs && List.is_empty snapshot.schedules);
         print_s
           [%sexp
             (durable : bool)
           , "captured preload and native query; deleted files not reread; private \
              package hidden"]));
  [%expect
    {|
    (false
     "captured preload and native query; deleted files not reread; private package hidden")
    (true
     "captured preload and native query; deleted files not reread; private package hidden")
    |}]
;;
