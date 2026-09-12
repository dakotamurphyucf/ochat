open Core
open Agent_server_test_support
module P = Agent_protocol
module E = Agent_server.Embedded
module F = Embedded_extension_tests
module Flow = Authoring_compaction_tests
module Q = Authoring_context_tests

let%expect_test
    "X10 transient host reports unavailable persistence before and after compaction"
  =
  Flow.with_offline_compaction (fun () ->
    let requests = ref 0 in
    let child = Q.request ~task:"child_agent" "prepare" in
    let background = Q.request ~task:"background_workflow" ~max_tokens:32000 "prepare" in
    let bundle =
      [ "version", `Number "1"
      ; "root_file", `String "child.chatmd"
      ; ( "sources"
        , `Array
            [ `Object
                [ "path", `String "child.chatmd"
                ; "text", `String "<developer>Read-only validation fixture.</developer>"
                ]
            ] )
      ; "tools", `Array []
      ]
    in
    let unsupported inputs id =
      let report = Flow.response inputs id in
      assert (Q.has_error report);
      assert (
        String.is_substring
          (Q.field report "message" |> Jsonaf.string_exn)
          ~substring:"Persisted child sessions are unavailable")
    in
    let background_context inputs id =
      let report = Flow.response inputs id in
      assert (not (Q.has_error report));
      let orientation =
        Q.items report |> List.hd_exn |> fun item -> Q.field item "content"
      in
      assert (
        not
          (List.exists
             (Q.field orientation "enabled_authoring_tasks" |> Jsonaf.list_exn)
             ~f:(function
               | `String "child_agent" -> true
               | _ -> false)));
      let guide =
        Q.field orientation "guides"
        |> Jsonaf.list_exn
        |> List.find_exn ~f:(fun guide ->
          String.equal (Q.field guide "suggested_task" |> Jsonaf.string_exn) "child_agent")
      in
      Q.require_json `False (Q.field guide "suggested_task_enabled");
      assert (
        String.is_substring
          (Q.field guide "execution_unavailable_reason" |> Jsonaf.string_exn)
          ~substring:"unavailable");
      let tool =
        Q.field orientation "selected_tools"
        |> Jsonaf.list_exn
        |> List.find_exn ~f:(fun tool ->
          String.equal (Q.field tool "name" |> Jsonaf.string_exn) "agent_create")
      in
      let description = Q.field tool "description" |> Jsonaf.string_exn in
      assert (
        String.is_substring
          description
          ~substring:"Persisted child sessions are unavailable");
      assert (
        not
          (String.is_substring
             description
             ~substring:"operation=prepare, task=child_agent"))
    in
    let post_stream ~sw:_ ~inputs =
      incr requests;
      let calls =
        match !requests with
        | 1 ->
          [ "child-help", "ochat_authoring_context", child
          ; "background-help", "ochat_authoring_context", background
          ; ( "validate-child"
            , "ochat_validate"
            , `Object (("target", `String "generated_chatmd") :: bundle) )
          ]
        | 2 ->
          unsupported inputs "child-help";
          background_context inputs "background-help";
          let report = Flow.response inputs "validate-child" in
          Q.require_json `True (Q.field report "valid");
          [ ( "create-child"
            , "agent_create"
            , `Object (("idempotency_key", `String "transient-child") :: bundle) )
          ]
        | 3 -> []
        | 4 ->
          let text =
            `Array (List.map inputs ~f:Openai.Responses.Item.jsonaf_of_t)
            |> Jsonaf.to_string
          in
          assert (String.is_substring text ~substring:"[Ochat authoring rediscovery]");
          assert (not (String.is_substring text ~substring:"authoring.primer"));
          [ "child-again", "ochat_authoring_context", child
          ; "background-again", "ochat_authoring_context", background
          ; ( "child-feature"
            , "ochat_authoring_context"
            , Q.request
                ~task:"background_workflow"
                ~features:[ "child_sessions" ]
                "prepare" )
          ]
        | 5 ->
          unsupported inputs "child-again";
          unsupported inputs "child-feature";
          background_context inputs "background-again";
          []
        | _ -> failwith "unexpected transient authoring request"
      in
      Fixtures.call_events calls
    in
    F.with_host
      ~durable:false
      ~sources:
        [ ( "agent.chatmd"
          , {|<developer>Use installed authoring contracts.</developer>
<authoring_context policy="manual"/>
<tool name="ochat_authoring_context"/><tool name="ochat_validate"/>
<tool name="agent_create"/><tool name="run_chatml"/>|}
          )
        ]
      ~daemon_options:
        { Agent_server.Daemon.default_options with model_post_stream = Some post_stream }
      (fun env _ embedded ->
         let wait count =
           Background_shell_tests.wait env (fun () ->
             let current = F.snapshot embedded in
             !requests = count && Option.is_none current.session.active_operation)
         in
         F.send embedded "Check authoring and try a child.";
         wait 3;
         let before = F.snapshot embedded in
         assert (P.Session.equal_persistence before.session.spec.persistence Transient);
         (match F.initial_outcome before "create-child" with
          | Fail error -> assert (String.equal error.code "capability_unavailable")
          | _ -> failwith "validation enabled unsupported child execution");
         assert (List.is_empty before.jobs);
         let key text = P.Idempotency_key.of_string text |> protocol_ok in
         F.request
           embedded
           (Session_compact
              { session_id = E.session_id embedded
              ; attachment_id = (E.attachment embedded).id
              ; expected_revision = Some before.revision
              ; idempotency_key = key "transient-compact"
              })
         |> ignore;
         wait 3;
         let compacted = F.snapshot embedded in
         List.iter compacted.canonical_history.entries ~f:(fun entry ->
           match entry.P.History.provenance with
           | Runtime_authoring _ -> failwith "compaction retained reference output"
           | _ -> ());
         F.request
           embedded
           (Session_send_message
              { session_id = E.session_id embedded
              ; attachment_id = (E.attachment embedded).id
              ; content =
                  { kind = Plain_text
                  ; text = "Continue after compaction."
                  ; attachments = []
                  }
              ; idempotency_key = key "transient-followup"
              })
         |> ignore;
         wait 5;
         let current = F.snapshot embedded in
         assert (List.is_empty current.jobs);
         assert (
           List.exists current.canonical_history.entries ~f:(fun entry ->
             match entry.P.History.provenance with
             | Runtime_authoring { purpose = Rediscovery; _ } -> true
             | _ -> false));
         print_endline
           "transient preparation and feature map reject persistence; readonly \
            validation grants no execution; compaction preserves manual policy and \
            limitation"));
  [%expect
    {| transient preparation and feature map reject persistence; readonly validation grants no execution; compaction preserves manual policy and limitation |}]
;;
