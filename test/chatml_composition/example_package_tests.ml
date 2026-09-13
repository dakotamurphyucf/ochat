open Core
open Fixtures
module Q = Authoring_context_tests
module Flow = Authoring_compaction_tests

let cases =
  [ ( "one-off"
    , "one_off_script"
    , [%blob "../chatml_extensibility_fixtures/authoring-lab/one-off.chatmd"]
    , [%blob "../chatml_extensibility_fixtures/authoring-lab/one-off.json"] )
  ; ( "standalone"
    , "standalone_tool"
    , [%blob "../chatml_extensibility_fixtures/authoring-lab/standalone.chatmd"]
    , [%blob "../chatml_extensibility_fixtures/authoring-lab/standalone.json"] )
  ; ( "moderator"
    , "moderator_tool"
    , [%blob "../chatml_extensibility_fixtures/authoring-lab/moderator.chatmd"]
    , [%blob "../chatml_extensibility_fixtures/authoring-lab/moderator.json"] )
  ; ( "children"
    , "child_agent"
    , [%blob "../chatml_extensibility_fixtures/authoring-lab/children.chatmd"]
    , [%blob "../chatml_extensibility_fixtures/authoring-lab/children.json"] )
  ; ( "background"
    , "background_workflow"
    , [%blob "../chatml_extensibility_fixtures/authoring-lab/background.chatmd"]
    , [%blob "../chatml_extensibility_fixtures/authoring-lab/background.json"] )
  ]
;;

type stage =
  | Prepare
  | Read
  | Done

let%expect_test
    "shipped composition packages prepare through configured native tools and preserve \
     selected-package scope"
  =
  List.iter cases ~f:(fun (slug, task, agent, _) ->
    let config =
      [%blob "../chatml_extensibility_fixtures/authoring-lab/server.sexp"]
      |> String.substr_replace_all ~pattern:"./public" ~with_:"./workspace"
      |> String.substr_replace_all ~pattern:"./one-off.chatmd" ~with_:"./agent.chatmd"
      |> String.substr_replace_all ~pattern:"(id lab)" ~with_:"(id restart.prompt)"
      |> String.substr_replace_all
           ~pattern:"(prompt lab)"
           ~with_:"(prompt restart.prompt)"
      |> String.substr_replace_all
           ~pattern:"(id examples)"
           ~with_:"(id restart.workspace)"
      |> String.substr_replace_all
           ~pattern:"(allowed_workspaces (examples))"
           ~with_:"(allowed_workspaces (restart.workspace))"
      (* The interactive template is separately checked unchanged. This offline
         transcript uses the normal configured allow policy, without an approval
         bypass or an injected authoring host. *)
      |> String.substr_replace_all
           ~pattern:"(tool_default ask)"
           ~with_:"(tool_default allow)"
    in
    let input = ref [] in
    let pages = ref [] in
    let stage = ref Prepare in
    let pending = ref "prepare" in
    let final_count = ref Int.max_value in
    let topic = "custom.e10-" ^ slug ^ ".guide" in
    let other =
      match String.equal slug "one-off" with
      | true -> "custom.e10-standalone.guide"
      | false -> "custom.e10-one-off.guide"
    in
    let query id request =
      pending := id;
      [ id, "ochat_authoring_context", request ]
    in
    let source =
      [%blob "../chatml_extensibility_fixtures/x11-authoring-repair/invalid-call.chatml"]
    in
    with_daemon
      ~config_file:"server.sexp"
      ~sources:
        ([ "server.sexp", config
         ; "agent.chatmd", agent
         ; ( "base.chatmd"
           , [%blob "../chatml_extensibility_fixtures/authoring-lab/base.chatmd"] )
         ; "workspace/example.chatml", source
         ]
         @ List.map cases ~f:(fun (slug, _, _, package) -> slug ^ ".json", package))
      ~calls:(query "prepare" (Q.request ~task ~max_tokens:32000 "prepare"))
      ~request_counts:(fun () -> !final_count, !final_count)
      ~inspect_request:(fun number actual ->
        assert (number < 100);
        input := actual)
      ~followup_calls:(fun number ->
        match !stage with
        | Prepare ->
          let page = Flow.response !input !pending in
          assert (not (Q.has_error page));
          pages := !pages @ [ page ];
          (match Q.field page "next_cursor" with
           | `String cursor ->
             query
               ("continue-" ^ Int.to_string number)
               (Q.request ~cursor ~max_tokens:32000 "continue")
           | `Null ->
             assert (Jsonaf.bool_exn (Q.field page "complete"));
             let items = List.concat_map !pages ~f:Q.items in
             let conventions =
               List.filter items ~f:(fun item ->
                 match Jsonaf.member "topic_id" item with
                 | Some (`String id) -> String.equal id topic
                 | _ -> false)
             in
             assert (not (List.is_empty conventions));
             List.iter conventions ~f:(fun item ->
               Q.require_json
                 (`String "authored_conventions")
                 (Q.field item "source_kind"));
             let text = Flow.content !pages in
             assert (String.is_substring text ~substring:"Task");
             assert (not (String.is_substring text ~substring:other));
             stage := Read;
             [ ( "read-source"
               , "read_file"
               , `Object [ "root", `String "examples"; "file", `String "example.chatml" ]
               )
             ; ( "unselected-guide"
               , "ochat_authoring_context"
               , Q.request ~task ~topic_id:other "topic" )
             ]
           | _ -> failwith "invalid continuation")
        | Read ->
          assert (Q.has_error (Flow.response !input "unselected-guide"));
          let read =
            List.find_map_exn !input ~f:(function
              | Openai.Responses.Item.Function_call_output
                  { call_id = "read-source"; output = Text text; _ } -> Some text
              | _ -> None)
          in
          assert (String.is_substring read ~substring:(String.strip source));
          final_count := number;
          stage := Done;
          []
        | Done -> failwith "unexpected provider continuation")
      (fun _ ->
         print_s [%sexp (task : string), "prepared scoped guide; read actual source"]));
  [%expect
    {|
    (one_off_script "prepared scoped guide; read actual source")
    (standalone_tool "prepared scoped guide; read actual source")
    (moderator_tool "prepared scoped guide; read actual source")
    (child_agent "prepared scoped guide; read actual source")
    (background_workflow "prepared scoped guide; read actual source")
    |}]
;;
