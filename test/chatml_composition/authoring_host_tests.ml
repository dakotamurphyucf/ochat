open Core
open Agent_server_test_support
open Fixtures
module V = Chat_response.Authoring_validation
module Q = Chat_response.Authoring_context
module C = Chat_response.Tool_capability
module P = Chat_response.Authoring_policy
module G = Agent_protocol.Authoring_guidance
module Corpus = Authoring_corpus

let%expect_test
    "captured host packages drive real helpers and preloads without widening child \
     guidance"
  =
  let reports =
    Authored_reference_tests.package "reports" "Keep the source file names in reports.\n"
  in
  let private_ =
    Authored_reference_tests.package "private" "PRIVATE-HOST-PACKAGE-SENTINEL"
  in
  let ordinary =
    let package =
      Authored_reference_tests.package "ordinary" "Ordinary moderator conventions."
    in
    { Corpus.help = { package.help with tasks = [ Moderator_tool ] }
    ; topics =
        List.map package.topics ~f:(fun topic ->
          { topic with surfaces = [ "moderator_v1" ] })
    }
  in
  let host =
    V.configure_authored
      (Authoring_context_tests.host ())
      ~packages:[ reports; private_; ordinary ]
    |> Result.ok_or_failwith
  in
  let configured = V.catalog host |> Option.value_exn in
  let delegated = V.delegated_catalog host |> Option.value_exn in
  assert (
    not
      (String.equal (P.catalog_fingerprint configured) (P.catalog_fingerprint delegated)));
  assert (
    Result.is_error
      (V.configure_generated host ~limits:(V.bundle_limits host) ~catalog:None));
  let descriptions = ref [] in
  with_daemon
    ~validation_host:host
    ~sources:
      [ ( "agent.chatmd"
        , {|<developer>Use the author's conventions.</developer>
<authoring_context policy="preload" topics="custom.reports.rules"/>
<script id="author" language="chatml" kind="tool" src="author.chatml"/>
<tool name="report_author" type="chatml" script="author" entrypoint="run" input_schema="any.json" output_schema="any.json"/>
<tool name="moderator_author" type="chatml" script="author" entrypoint="run" input_schema="any.json" output_schema="any.json"/>
<authoring_help tool="report_author" package="reports" tasks="one_off_script" topics="custom.reports.rules"/>
<authoring_help tool="moderator_author" package="ordinary" tasks="moderator_tool" topics="custom.ordinary.rules"/>|}
        )
      ; "author.chatml", "let run ctx input = Task.pure(`Complete(input))"
      ; "any.json", {|{"type":"object","properties":{},"additionalProperties":false}|}
      ]
    ~calls:
      [ ( "reference"
        , "ochat_authoring_context"
        , Authoring_context_tests.request
            ~task:"one_off_script"
            ~topic_id:"custom.reports.rules"
            "topic" )
      ]
    ~inspect_request:(fun _ inputs -> descriptions := inputs :: !descriptions)
    ~after_turn:(fun env _ entry ->
      let caps =
        Agent_server.Runtime_owner.with_background_runtime entry.runtime (fun runtime ->
          Lazy.force
            (Option.value_exn runtime.Agent_session.Runtime_builder.native_runtime)
              .capabilities
          |> Result.map_error ~f:(fun error ->
            Agent_protocol.Error.invalid_request error.C.message))
        |> protocol_ok
      in
      let refs = C.references caps in
      let prepare ~tool ~topic =
        let source =
          sprintf
            {|<authoring_context policy="preload" topics="%s"/><tool type="inherited" name="%s"/>|}
            topic
            tool
        in
        let bundle =
          Chatmd_source_bundle.create
            ~root_file:"child.chatmd"
            ~sources:[ "child.chatmd", source ]
            ()
          |> Result.ok_or_failwith
        in
        Agent_session.Generated_definition.prepare
          ?catalog:(V.delegated_catalog host)
          ~env
          ~dir:(Eio.Stdenv.fs env)
          ~revision_id:(Agent_protocol.Id.Prompt_revision.create ())
          ~created_at:(Agent_protocol.Timestamp.now ())
          ~current_capabilities:(fun () -> caps)
          ~references:refs
          bundle
      in
      let rejected = function
        | Error diagnostics ->
          assert (
            List.exists diagnostics ~f:(fun d ->
              String.equal
                d.Chatmd_shell_spec.Diagnostic.code
                "authoring.incompatible_help"))
        | Ok _ -> failwith "child preload admitted unavailable package guidance"
      in
      rejected (prepare ~tool:"report_author" ~topic:"custom.private.rules");
      rejected (prepare ~tool:"moderator_author" ~topic:"custom.ordinary.rules");
      let child =
        prepare ~tool:"report_author" ~topic:"custom.reports.rules"
        |> Result.map_error ~f:(fun ds ->
          String.concat ~sep:"\n" (List.map ds ~f:Chatmd_shell_spec.Diagnostic.to_string))
        |> Result.ok_or_failwith
      in
      let admission = Agent_session.Generated_definition.admission child in
      let context =
        Q.create ~secret:"host-packages-materialization" () |> Result.ok_or_failwith
      in
      let materialized =
        Chat_response.Authoring_materialization.create
          ~context
          ~host:(V.for_delegated host)
          ~policy:(Chat_response.Generated_admission.authoring admission)
          ~capabilities:(Chat_response.Generated_admission.capabilities admission)
          ~scope:"child:1"
          ()
        |> Result.ok_or_failwith
      in
      let entries =
        Chat_response.Authoring_materialization.initial materialized
        |> List.mapi ~f:(fun sequence message ->
          Chat_response.Authoring_materialization.entry
            message
            ~id:
              (History_entry.Id.create ~namespace:"child" ~sequence
               |> Result.ok_or_failwith))
      in
      assert (
        List.is_empty
          (Chat_response.Authoring_materialization.refresh
             materialized
             ~known:[]
             ~effective:entries
           |> protocol_ok));
      let module Presence = Chat_response.Authoring_presence in
      let expected_topics =
        Chat_response.Authoring_materialization.initial materialized
        |> List.concat_map ~f:(fun message ->
          message.Chat_response.Authoring_materialization.guidance.topics)
      in
      let known = Presence.remember ~previous:[] ~history:entries |> protocol_ok in
      let inspect expected_topics =
        Presence.inspect_with_topics
          ~expected_topics
          ~policy:(Chat_response.Generated_admission.authoring admission)
          ~context_identity:
            (Chat_response.Authoring_materialization.context_identity materialized)
          ~known
          ~effective:entries
        |> protocol_ok
      in
      assert (List.is_empty (inspect expected_topics).missing_preload);
      List.iter
        [ (fun topic ->
            { topic with
              G.source = Installed (Corpus.identity (Option.value_exn (V.corpus host)))
            })
        ; (fun topic ->
            { topic with
              G.document_sha256 = Chatmd_shell_spec.Source_ref.digest "changed convention"
            })
        ]
        ~f:(fun alter ->
          let expected =
            List.map expected_topics ~f:(fun topic ->
              if String.equal topic.G.id "custom.reports.rules"
              then alter topic
              else topic)
          in
          assert (
            List.equal
              String.equal
              (inspect expected).missing_preload
              [ "custom.reports.rules" ]));
      let cross =
        { reports with
          topics =
            List.map reports.topics ~f:(fun topic ->
              { topic with
                prerequisites = topic.prerequisites @ [ "custom.private.rules" ]
              })
        }
      in
      let changed =
        V.configure_authored
          (Authoring_context_tests.host ())
          ~packages:[ cross; private_; ordinary ]
        |> Result.ok_or_failwith
      in
      assert (not (String.equal (V.host_fingerprint host) (V.host_fingerprint changed)));
      match
        P.resolve
          ?catalog:(V.catalog changed)
          ~ceiling:caps
          ~selected_names:[ "report_author" ]
          ()
      with
      | Error e -> assert (String.equal e.P.code "authoring.incompatible_help")
      | Ok _ -> failwith "automatic help restored an unselected dependency")
    (fun state ->
       let entries = state.Agent_session.Session_state.conversation.canonical_history in
       let custom =
         List.filter_map entries ~f:(fun entry ->
           match entry.Agent_protocol.History.provenance with
           | Runtime_authoring guidance when G.equal_purpose guidance.purpose Preload ->
             List.find guidance.topics ~f:(fun topic ->
               String.equal topic.G.id "custom.reports.rules")
           | _ -> None)
       in
       (match custom with
        | [ { G.source = Authored _; complete = true; _ } ] -> ()
        | _ -> failwith "custom preload was duplicated or lost its authored provenance");
       let response =
         match result state "reference" with
         | Complete (`String text) -> Jsonaf.of_string text
         | _ -> failwith "captured native helper query failed"
       in
       let item = List.last_exn (Authoring_context_tests.items response) in
       Authoring_context_tests.require_json
         (`String "authored_conventions")
         (Jsonaf.member_exn "source_kind" item);
       assert (List.length !descriptions = 2);
       List.iter !descriptions ~f:(fun inputs ->
         let text =
           `Array (List.map inputs ~f:Openai.Responses.Item.jsonaf_of_t)
           |> Jsonaf.to_string
         in
         assert (String.is_substring text ~substring:"Keep the source file names");
         assert (not (String.is_substring text ~substring:"PRIVATE-HOST-PACKAGE-SENTINEL")));
       print_endline
         "captured helper and authored preload; deduplicated child guidance; private and \
          ordinary-only child packages denied");
  [%expect
    {| captured helper and authored preload; deduplicated child guidance; private and ordinary-only child packages denied |}]
;;
