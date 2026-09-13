open Core
open Agent_server_test_support
open Fixtures
module F = Chat_response.Authoring_package_file
module C = Authoring_corpus
module Q = Authoring_context_tests
module Flow = Authoring_compaction_tests

let encode packages =
  let strings values = `Array (List.map values ~f:(fun value -> `String value)) in
  `Object
    [ "version", `Number "1"
    ; ( "packages"
      , `Array
          (List.map packages ~f:(fun package ->
             `Object
               [ ( "help"
                 , `Object
                     [ "version", `Number "1"
                     ; "package", `String package.C.help.package
                     ; ( "tasks"
                       , strings
                           (List.map
                              package.help.tasks
                              ~f:Chatmd_shell_spec.Authoring_metadata.task_id) )
                     ; "topics", strings package.help.topics
                     ; ( "required_helpers"
                       , strings
                           (List.map
                              package.help.required_helpers
                              ~f:Chatmd_shell_spec.Authoring_metadata.helper_name) )
                     ] )
               ; ( "topics"
                 , `Array
                     (List.map package.topics ~f:(fun topic ->
                        `Object
                          [ "id", `String topic.id
                          ; "title", `String topic.title
                          ; "prerequisites", strings topic.prerequisites
                          ; "surfaces", strings topic.surfaces
                          ; "source_name", `String topic.source_name
                          ; "text", `String topic.text
                          ])) )
               ])) )
    ]
  |> Jsonaf.to_string
;;

let package = Authored_reference_tests.package

let captured text =
  F.of_string ~source_file:"conventions.json" text |> Result.ok_or_failwith
;;

let%expect_test
    "package files reject malformed contracts and validate across captured files"
  =
  let parent = package "reports" "Keep report filenames." in
  let child = package "child" "Child conventions." in
  let dependent =
    { child with
      topics =
        List.map child.topics ~f:(fun topic ->
          { topic with prerequisites = [ "custom.reports.rules" ] })
    }
  in
  let sources = [ captured (encode [ parent ]); captured (encode [ dependent ]) ] in
  let packages = F.packages sources |> Result.ok_or_failwith in
  [%test_eq: int] 2 (List.length packages);
  assert (Result.is_error (F.packages [ List.last_exn sources ]));
  assert (Result.is_error (F.packages (List.hd_exn sources :: sources)));
  let bad name json =
    match F.of_string ~source_file:name json with
    | Error _ -> print_endline (name ^ ": rejected")
    | Ok _ -> failwith ("accepted malformed " ^ name)
  in
  bad "version" {|{"version":2,"packages":[]}|};
  bad "unknown field" {|{"version":1,"packages":[],"helper":"ochat_validate"}|};
  bad "duplicate field" {|{"version":1,"version":1,"packages":[]}|};
  bad "oversize" (String.make ((1024 * 1024) + 1) ' ');
  let reserved =
    { parent with
      topics =
        List.map parent.topics ~f:(fun topic -> { topic with id = "chatml.syntax.calls" })
    }
  in
  assert (Result.is_error (F.packages [ captured (encode [ reserved ]) ]));
  let source_label =
    { parent with
      topics =
        List.map parent.topics ~f:(fun topic ->
          { topic with source_name = "/unopened/private-conventions.md" })
    }
  in
  ignore
    (F.packages [ captured (encode [ source_label ]) ] |> Result.ok_or_failwith
     : C.authored_package list);
  print_endline
    "cross-file dependencies checked; duplicates/reserved topics rejected; source labels \
     never opened";
  [%expect
    {|
    version: rejected
    unknown field: rejected
    duplicate field: rejected
    oversize: rejected
    cross-file dependencies checked; duplicates/reserved topics rejected; source labels never opened
    |}]
;;

let server_config =
  {|(version 1)
(server ((data_dir "./data") (unix_socket "./agent.sock")
         (authoring_packages ("./reports.json" "./private.json"))))
(workspaces (((id restart.workspace) (source (physical "./workspace")) (access shared_write)
              (prompt_limits (((prompt restart.prompt) (max_root_agents 2) (overflow reject)))))))
(prompts (((id restart.prompt) (path "./agent.chatmd") (allowed_workspaces (restart.workspace))
           (permission_profile restart.permission) (enabled true))))
(permission_profiles (((id restart.permission) (tool_default allow) (approval_timeout none)
                       (approval_fallback deny) (manifest_authorization assume_authorized))))|}
;;

let%expect_test
    "daemon configuration captures packages for native preloads and rejects live \
     replacement"
  =
  let selected = package "reports" "ORIGINAL-CONFIG-CONVENTIONS" in
  let private_ = package "private" "UNSELECTED-CONFIG-CONVENTIONS" in
  let host = ref None in
  let input = ref [] in
  let response () =
    let json = Flow.response !input "query-conventions" in
    assert (not (Q.has_error json));
    let text = Flow.content [ json ] in
    assert (String.is_substring text ~substring:"ORIGINAL-CONFIG-CONVENTIONS");
    assert (not (String.is_substring text ~substring:"CHANGED-CONFIG-CONVENTIONS"));
    assert (not (String.is_substring text ~substring:"UNSELECTED-CONFIG-CONVENTIONS"));
    List.iter (Q.items json) ~f:(fun item ->
      match Jsonaf.member "topic_id" item with
      | Some (`String "custom.reports.rules") ->
        Q.require_json (`String "authored_conventions") (Q.field item "source_kind")
      | _ -> ())
  in
  let calls id =
    [ ( id
      , "ochat_authoring_context"
      , Q.request
          ~task:"one_off_script"
          ~topic_id:"custom.reports.rules"
          ~max_tokens:32000
          "topic" )
    ]
  in
  with_daemon
    ~config_file:"server.sexp"
    ~sources:
      [ "server.sexp", server_config
      ; "reports.json", encode [ selected ]
      ; "private.json", encode [ private_ ]
      ; ( "agent.chatmd"
        , {|<developer>Use the configured report conventions.</developer>
<authoring_context policy="preload" topics="custom.reports.rules"/>
<script id="author" language="chatml" kind="tool">let run ctx input = Task.pure(`Complete(input))</script>
<tool name="report_author" type="chatml" script="author" entrypoint="run" input_schema="any.json" output_schema="any.json"/>
<authoring_help tool="report_author" package="reports" tasks="one_off_script" topics="custom.reports.rules"/>|}
        )
      ; "any.json", {|{"type":"object","properties":{},"additionalProperties":false}|}
      ]
    ~connect:(fun ~sw:_ ~env:_ ~root daemon ->
      host := Some (root, daemon);
      connection daemon (principal ()))
    ~calls:(calls "query-conventions")
    ~expected_requests:4
    ~inspect_request:(fun number actual ->
      input := actual;
      let text =
        `Array (List.map actual ~f:Openai.Responses.Item.jsonaf_of_t) |> Jsonaf.to_string
      in
      assert (String.is_substring text ~substring:"ORIGINAL-CONFIG-CONVENTIONS");
      assert (not (String.is_substring text ~substring:"UNSELECTED-CONFIG-CONVENTIONS"));
      match number with
      | 4 ->
        let json = Flow.response actual "query-after-edit" in
        assert (
          String.is_substring
            (Flow.content [ json ])
            ~substring:"ORIGINAL-CONFIG-CONVENTIONS")
      | _ -> ())
    ~followup_calls:(function
      | 2 ->
        response ();
        []
      | 3 -> calls "query-after-edit"
      | 4 -> []
      | _ -> failwith "unexpected configured-authoring request")
    ~after_turn:(fun env handle _ ->
      let root, daemon = Option.value_exn !host in
      let path = Eio.Path.(Eio.Stdenv.fs env / root / "reports.json") in
      Eio.Path.save
        ~create:(`Or_truncate 0o600)
        path
        (encode [ package "reports" "CHANGED-CONFIG-CONVENTIONS" ]);
      (match Agent_server.Daemon.reload_config daemon with
       | Error [ diagnostic ] ->
         [%test_eq: string] "config.restart_required" diagnostic.code
       | _ -> failwith "package content replacement should require restart");
      Eio.Path.unlink path;
      H.send_message
        handle
        { kind = Plain_text
        ; text = "Read the captured conventions again."
        ; attachments = []
        }
      |> protocol_ok
      |> ignore)
    ~settle:Flow.wait_idle
    (fun state ->
       assert (List.is_empty (native_reads state));
       print_endline
         "normal config -> scoped authored preload/query; edit requires restart; \
          deletion does not change active snapshot");
  [%expect
    {| normal config -> scoped authored preload/query; edit requires restart; deletion does not change active snapshot |}]
;;
