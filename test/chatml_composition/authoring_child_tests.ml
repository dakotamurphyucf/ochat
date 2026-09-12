open Core
open Agent_server_test_support
open Fixtures
module P = Agent_protocol
module V = Chat_response.Authoring_validation
module C = Chat_response.Tool_capability

let%expect_test
    "persisted children query scoped conventions and invoke inherited handlers after \
     reload"
  =
  let convention = "Keep original report file names." in
  let host =
    V.configure_authored
      (Authoring_context_tests.host ())
      ~packages:
        [ Authored_reference_tests.package "reports" convention
        ; Authored_reference_tests.package "private" "PRIVATE-CHILD-CONVENTION"
        ]
    |> Result.ok_or_failwith
  in
  let query id topic =
    ( id
    , "ochat_authoring_context"
    , Authoring_context_tests.request ~task:"one_off_script" ~topic_id:topic "topic" )
  in
  let child_inputs = ref [] in
  with_daemon
    ~validation_host:host
    ~initial_requests:1
    ~expected_requests:5
    ~calls:[]
    ~followup_calls:(function
      | 2 ->
        [ query "before" "custom.reports.rules"; query "private" "custom.private.rules" ]
      | 4 ->
        [ query "after" "custom.reports.rules"; "handler", "report_author", `Object [] ]
      | _ -> [])
    ~inspect_request:(fun number inputs ->
      if number > 1 then child_inputs := inputs :: !child_inputs)
    ~sources:
      [ ( "agent.chatmd"
        , {|<developer>PARENT-CONVENTIONS-INSTRUCTION</developer>
<script id="author" language="chatml" kind="tool" src="author.chatml"/>
<tool name="report_author" type="chatml" script="author" entrypoint="run" input_schema="object.json" output_schema="object.json"/>
<tool name="private_author" type="chatml" script="author" entrypoint="run" input_schema="object.json" output_schema="object.json"/>
<authoring_help tool="report_author" package="reports" tasks="one_off_script" topics="custom.reports.rules"/>
<authoring_help tool="private_author" package="private" tasks="one_off_script" topics="custom.private.rules"/>|}
        )
      ; "author.chatml", "let run ctx input = Task.pure(`Complete(input))"
      ; "object.json", {|{"type":"object","properties":{},"additionalProperties":false}|}
      ]
    ~after_turn_with_daemon:(fun env daemon parent ->
      Eio.Switch.run (fun sw ->
        let caps =
          Agent_server.Runtime_owner.with_background_runtime
            parent.runtime
            (fun runtime ->
               Lazy.force
                 (Option.value_exn runtime.Agent_session.Runtime_builder.native_runtime)
                   .capabilities
               |> Result.map_error ~f:(fun e -> P.Error.invalid_request e.C.message))
          |> protocol_ok
        in
        let bundle =
          Chatmd_source_bundle.create
            ~root_file:"child.chatmd"
            ~sources:
              [ ( "child.chatmd"
                , {|<developer>CHILD-CONVENTIONS-INSTRUCTION</developer>
<authoring_context policy="preload" topics="custom.reports.rules"/>
<tool type="inherited" name="report_author"/>|}
                )
              ]
            ()
          |> Result.ok_or_failwith
        in
        let definition =
          Agent_session.Generated_definition.prepare
            ?catalog:(V.delegated_catalog host)
            ~env
            ~dir:(Eio.Stdenv.fs env)
            ~revision_id:(P.Id.Prompt_revision.create ())
            ~created_at:(P.Timestamp.now ())
            ~current_capabilities:(fun () -> caps)
            ~references:(C.references caps)
            bundle
          |> Result.map_error ~f:(fun ds ->
            String.concat
              ~sep:"\n"
              (List.map ds ~f:Chatmd_shell_spec.Diagnostic.to_string))
          |> Result.ok_or_failwith
        in
        let parent_state = A.state parent.actor |> protocol_ok in
        let child =
          Agent_server.Session_factory.create_generated_session
            ~start_immediately:true
            (Agent_server.Daemon.factory daemon)
            ~parent_session_id:parent_state.identity.session_id
            ~idempotency_key:
              (P.Idempotency_key.of_string "custom-reference-child" |> protocol_ok)
            ~display_name:None
            definition
          |> protocol_ok
        in
        let state () = A.state child.actor |> protocol_ok in
        let id = (state ()).identity.session_id in
        let connection = connection daemon (principal ()) in
        initialize connection;
        Exn.protect
          ~finally:(fun () -> Agent_client.Connection.close connection)
          ~f:(fun () ->
            let handle =
              H.attach
                ~sw
                ~clock:(Eio.Stdenv.clock env)
                ~connection
                ~session_id:id
                ~mode:Read_write
                ~subscribe:false
                ()
              |> protocol_ok
            in
            let send () =
              H.send_message
                handle
                { kind = Plain_text
                ; text = "Use your report conventions."
                ; attachments = []
                }
              |> protocol_ok
              |> ignore;
              Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
                let rec idle () =
                  match (state ()).active_operation with
                  | None -> ()
                  | Some _ ->
                    Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
                    idle ()
                in
                idle ());
              assert (P.Id.Session.equal (state ()).identity.session_id id)
            in
            let report call =
              match result (state ()) call with
              | Complete (`String text) -> Jsonaf.of_string text
              | _ -> failwith "child reference did not return its native result"
            in
            let check call =
              let item = List.last_exn (Authoring_context_tests.items (report call)) in
              Authoring_context_tests.require_json
                (`String convention)
                (Jsonaf.member_exn "text" item);
              Authoring_context_tests.require_json
                (`String "authored_conventions")
                (Jsonaf.member_exn "source_kind" item)
            in
            send ();
            check "before";
            assert (Authoring_context_tests.has_error (report "private"));
            let guidance () = Authoring_policy_integration_tests.guidance (state ()) in
            let original = guidance () in
            assert (
              List.exists original ~f:(fun entry ->
                match entry.P.History.provenance with
                | Runtime_authoring g ->
                  List.exists g.topics ~f:(fun topic ->
                    match topic.source with
                    | Authored _ -> String.equal topic.id "custom.reports.rules"
                    | Installed _ -> false)
                | _ -> false));
            H.stop handle ~mode:Graceful |> protocol_ok |> ignore;
            unload_idle_runtime env child.runtime;
            H.start handle ~queue_if_limited:false |> protocol_ok |> ignore;
            send ();
            check "after";
            (match result (state ()) "handler" with
             | Complete (`Object []) -> ()
             | _ -> failwith "inherited managed handler failed after reload");
            assert (
              List.equal
                (fun a b ->
                   Jsonaf.exactly_equal
                     (P.History.entry_to_json a)
                     (P.History.entry_to_json b))
                original
                (guidance ()));
            H.close handle)))
    (fun _ ->
       assert (List.length !child_inputs = 4);
       List.iter !child_inputs ~f:(fun inputs ->
         let text =
           `Array (List.map inputs ~f:Openai.Responses.Item.jsonaf_of_t)
           |> Jsonaf.to_string
         in
         assert (String.is_substring text ~substring:convention);
         assert (String.is_substring text ~substring:"CHILD-CONVENTIONS-INSTRUCTION");
         assert (
           not (String.is_substring text ~substring:"PARENT-CONVENTIONS-INSTRUCTION"));
         assert (not (String.is_substring text ~substring:"PRIVATE-CHILD-CONVENTION")));
       print_endline
         "persisted child: scoped native queries; private package denied; identical \
          authored guidance and managed handler after reload");
  [%expect
    {| persisted child: scoped native queries; private package denied; identical authored guidance and managed handler after reload |}]
;;
