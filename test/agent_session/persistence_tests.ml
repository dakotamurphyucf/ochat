open Core
open Fixtures

let durable_event session_id sequence =
  Agent_protocol.Event.Durable.of_payload
    ~session_id
    ~sequence
    ~revision:sequence
    ~timestamp
    (Agent_protocol.Event.Durable.Payload.Moderator_notification
       (`Object [ "sequence", `Number (Int64.to_string sequence) ]))
;;

let replay_shape = function
  | Agent_session.Durable_event_log.Snapshot_required -> "snapshot"
  | Available events ->
    List.map events ~f:(fun event -> Int64.to_string event.sequence)
    |> String.concat ~sep:","
;;

let%expect_test "durable event replay detects retained and expired cursors" =
  Eio_main.run (fun _env ->
    let session_id =
      Agent_protocol.Id.Session.of_string "ses_event_replay" |> protocol_ok
    in
    let log =
      Agent_session.Durable_event_log.create
        ~capacity:2
        [ durable_event session_id 1L; durable_event session_id 2L ]
      |> protocol_ok
    in
    Agent_session.Durable_event_log.append log [ durable_event session_id 3L ];
    let expired =
      Agent_session.Durable_event_log.replay log ~after_sequence:0L ~through_sequence:3L
    in
    let retained =
      Agent_session.Durable_event_log.replay log ~after_sequence:1L ~through_sequence:3L
    in
    print_s
      [%sexp
        { expired = (replay_shape expired : string)
        ; retained = (replay_shape retained : string)
        ; oldest = (Agent_session.Durable_event_log.oldest_sequence log : int64 option)
        ; latest = (Agent_session.Durable_event_log.latest_sequence log : int64 option)
        }]);
  [%expect
    {|
    ((expired snapshot) (retained 2,3) (oldest (2)) (latest (3)))
    |}]
;;

let%expect_test
    "administrative commit failure and stale candidates preserve complete state"
  =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let actor, backend =
        audit_actor
          ~with_invocation:true
          ~sw
          ~env
          ~workspace_instance
          ~reject_archive:true
          ()
      in
      let writer, _ =
        Agent_session.Session_actor.attach actor ~mode:Read_write ~subscribe:false
        |> protocol_ok
      in
      let before = Agent_session.Session_actor.state actor |> protocol_ok in
      let target =
        Agent_protocol.Id.Prompt_revision.of_string "prv_prepared_admin" |> protocol_ok
      in
      let candidate = Agent_session.Administration.rebuild before target |> protocol_ok in
      let commit revision =
        Agent_session.Session_actor.commit_administration
          actor
          ~command_audit:None
          ~attachment_id:writer.id
          ~expected_revision:revision
          ~kind:Rebuild
          candidate
      in
      assert (Result.is_error (commit Int64.(before.counters.revision - 1L)));
      assert (Result.is_error (commit before.counters.revision));
      let after = Agent_session.Session_actor.state actor |> protocol_ok in
      assert (
        Sexp.equal
          (Agent_session.Session_state.sexp_of_t before)
          (Agent_session.Session_state.sexp_of_t after));
      assert (
        List.is_empty
          (Agent_session.Memory_backend.events_after
             backend
             before.counters.event_sequence
           |> protocol_ok));
      Agent_session.Session_actor.shutdown actor));
  print_endline "stale and failed commits preserve complete state and event position";
  [%expect {| stale and failed commits preserve complete state and event position |}]
;;

let%expect_test "history deletion pairs occurrences, not reused provider call IDs" =
  let open Openai.Responses in
  let call =
    Item.Function_call
      { name = "test"
      ; arguments = "{}"
      ; call_id = "reused"
      ; _type = "function_call"
      ; id = None
      ; status = None
      }
  in
  let output =
    Item.Function_call_output
      { output = Text "done"
      ; call_id = "reused"
      ; _type = "function_call_output"
      ; id = None
      ; status = None
      }
  in
  let custom =
    Item.Custom_tool_call
      { name = "custom"
      ; input = "input"
      ; call_id = "reused"
      ; _type = "custom_tool_call"
      ; id = None
      }
  in
  let custom_output =
    Item.Custom_tool_call_output
      { output = Text "custom done"
      ; call_id = "reused"
      ; _type = "custom_tool_call_output"
      ; id = None
      }
  in
  let entries =
    List.mapi
      [ call; custom; output; custom_output; call; output ]
      ~f:(fun sequence item ->
        History_entry.create_with_id
          ~id:
            (History_entry.Id.create ~namespace:"pairs" ~sequence |> Result.ok_or_failwith)
          item)
  in
  List.iter [ 0; 2; 1; 3; 4; 5 ] ~f:(fun index ->
    let retained =
      History_entry.remove_with_tool_pair
        entries
        ~entry_id:(History_entry.id (List.nth_exn entries index))
      |> Result.ok_or_failwith
    in
    print_s
      [%sexp
        (List.map retained ~f:(fun entry ->
           History_entry.Id.sequence (History_entry.id entry))
         : int list)]);
  [%expect
    {|
    (1 3 4 5)
    (1 3 4 5)
    (0 2 4 5)
    (0 2 4 5)
    (0 1 2 3)
    (0 1 2 3)
    |}]
;;

let rec next_replacement subscriber =
  match Agent_session.Subscriber.take subscriber with
  | Some (Ok (Durable event)) ->
    (match
       Agent_protocol.Event.Durable.Payload.of_json ~kind:event.kind event.payload
       |> protocol_ok
     with
     | History_replaced history -> history.entries
     | _ -> next_replacement subscriber)
  | Some (Ok _) -> next_replacement subscriber
  | _ -> failwith "subscriber ended before history replacement"
;;

let%expect_test "history deletion is authoritative, revision checked and broadcast" =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let actor, backend =
        audit_actor ~sw ~env ~workspace_instance ~reject_archive:false ()
      in
      let writer, first =
        Agent_session.Session_actor.attach actor ~mode:Read_write ~subscribe:true
        |> protocol_ok
      in
      let reader, second =
        Agent_session.Session_actor.attach actor ~mode:Read_only ~subscribe:true
        |> protocol_ok
      in
      let before = Agent_session.Session_actor.state actor |> protocol_ok in
      let remove attachment_id expected_revision =
        Agent_session.Session_actor.delete_history
          actor
          ~attachment_id
          ~expected_revision
          history_id
      in
      let denied = Result.is_error (remove reader.id before.counters.revision) in
      let stale =
        Result.is_error (remove writer.id Int64.(before.counters.revision - 1L))
      in
      ignore
        (remove writer.id before.counters.revision |> protocol_ok
         : Agent_protocol.Session.t);
      let events =
        Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 2. (fun () ->
          List.map [ first; second ] ~f:(fun subscriber ->
            List.length (next_replacement (Option.value_exn subscriber))))
      in
      printf
        "denied=%b stale=%b durable=%d clients=%s\n"
        denied
        stale
        (List.length
           (Agent_session.Memory_backend.state backend).conversation.canonical_history)
        (Sexp.to_string ([%sexp_of: int list] events));
      Agent_session.Session_actor.shutdown actor));
  [%expect {| denied=true stale=true durable=0 clients=(0 0) |}]
;;

let%expect_test
    "extension schemas and scripts restore from actual pinned artifact closure"
  =
  with_temp_directory (fun env temporary ->
    let directory = Eio.Path.(Eio.Stdenv.fs env / temporary / "prompt") in
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(directory / "parts");
    let save path text =
      Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(directory / path) text
    in
    save "root.chatmd" {|<import src="parts/definition.chatmd" namespace="lab"/>|};
    save
      "parts/definition.chatmd"
      {|<script id="worker" language="chatml" kind="tool" src="worker.chatml"/><tool name="work" type="chatml" script="worker" entrypoint="run" input_schema="schema.json" output_schema="schema.json"/>|};
    save "parts/worker.chatml" "let run = fun ctx input -> Task.pure(`Complete(input))";
    save "parts/schema.json" {|{"type":"object"}|};
    let definition =
      Agent_session.Prompt_definition.create
        ~id:prompt_id
        ~config_name:"extensions"
        ~root_file:(Filename.concat temporary "prompt/root.chatmd")
        ~allowed_workspaces:[ workspace_id ]
        ~permission_profile:"interactive"
        ~runtime_policy:None
        ~enabled:true
        ~description:None
      |> store_ok
    in
    let artifact_store =
      Agent_store.Prompt_artifact_store.create
        ~env
        ~root:(Filename.concat temporary "artifacts")
      |> store_ok
    in
    let get = function
      | Ok value -> value
      | Error errors ->
        raise_s [%sexp (errors : Agent_session.Prompt_revision_builder.Diagnostic.t list)]
    in
    let revision =
      Agent_session.Prompt_revision_builder.build
        ~env
        ~artifact_store
        ~transaction_id
        ~created_at:timestamp
        definition
      |> get
    in
    let revision_id = Agent_session.Prompt_revision.id revision in
    let rebuilt =
      Agent_session.Prompt_revision_builder.build
        ~env
        ~artifact_store
        ~transaction_id:(Agent_protocol.Id.Transaction.create ())
        ~created_at:(Agent_protocol.Timestamp.add_ms timestamp 1000 |> protocol_ok)
        definition
      |> get
    in
    assert (
      Agent_protocol.Id.Prompt_revision.equal
        revision_id
        (Agent_session.Prompt_revision.id rebuilt));
    let original_artifact = Agent_session.Prompt_revision.artifact revision in
    let rebuilt_artifact = Agent_session.Prompt_revision.artifact rebuilt in
    [%test_eq: string] original_artifact.manifest_sha256 rebuilt_artifact.manifest_sha256;
    assert (
      Agent_protocol.Timestamp.equal
        original_artifact.created_at
        rebuilt_artifact.created_at);
    Eio.Path.rmtree directory;
    let restored =
      Agent_session.Prompt_revision_builder.restore ~artifact_store definition revision_id
      |> get
    in
    let elements = Agent_session.Prompt_revision.elements restored in
    let tool =
      List.find_map_exn elements ~f:(function
        | Prompt.Chat_markdown.Tool (Extension tool) -> Some tool
        | _ -> None)
    in
    assert (String.equal tool.input_schema.source_text {|{"type":"object"}|});
    assert (List.is_empty tool.uses);
    let script =
      List.find_map_exn elements ~f:(function
        | Prompt.Chat_markdown.Extension_script script -> Some script
        | _ -> None)
    in
    assert (String.equal script.id "lab:worker");
    assert (
      String.equal
        (Chatmd_shell_spec.Extension_spec.script_text script)
        "let run = fun ctx input -> Task.pure(`Complete(input))");
    let materialized =
      Eio.Path.native_exn (Agent_session.Prompt_revision.materialized_tree restored)
    in
    assert (
      String.equal
        tool.input_schema.source_ref.source_dir
        (Filename.concat materialized "parts"));
    let artifact = Agent_session.Prompt_revision.artifact restored in
    assert (artifact.parser_schema_version = 5);
    assert (List.length artifact.sources = 3);
    let restore_fixture ~suffix ~version ~root ~sources =
      let id =
        Agent_protocol.Id.Prompt_revision.of_string ("prv_compat_" ^ suffix)
        |> protocol_ok
      in
      let fixture =
        Agent_store.Prompt_artifact_store.Artifact.create
          ~revision_id:id
          ~root_relative_path:"root.chatmd"
          ~root_chatmd:root
          ~sources
          ~parser_schema_version:version
          ~runtime_schema_version:1
          ~created_at:timestamp
          ()
        |> store_ok
      in
      Agent_store.Prompt_artifact_store.install artifact_store ~transaction_id fixture
      |> store_ok;
      Agent_session.Prompt_revision_builder.restore ~artifact_store definition id
    in
    assert (
      Result.is_ok
        (restore_fixture
           ~suffix:"v2_extension"
           ~version:2
           ~root:artifact.root_chatmd
           ~sources:artifact.sources));
    assert (
      Result.is_error
        (restore_fixture
           ~suffix:"v1_extension"
           ~version:1
           ~root:artifact.root_chatmd
           ~sources:artifact.sources));
    assert (
      Result.is_ok
        (restore_fixture
           ~suffix:"v1_inline_markup"
           ~version:1
           ~root:{|<user>Example: <authoring_context policy="manual"/></user>|}
           ~sources:[]));
    let inherited = {|<tool type="inherited" name="read_file"/>|} in
    let authored_help =
      {|<authoring_help tool="custom" package="one-off" tasks="one_off_script" topics="chatml/basics"/>|}
    in
    List.iter [ 1; 2; 3 ] ~f:(fun version ->
      let result =
        restore_fixture
          ~suffix:(sprintf "help_%d" version)
          ~version
          ~root:authored_help
          ~sources:[]
      in
      match result with
      | Ok _ -> failwith "old artifact accepted new help declarations"
      | Error errors ->
        assert (
          List.exists errors ~f:(fun diagnostic ->
            String.is_substring
              diagnostic.Agent_session.Prompt_revision_builder.Diagnostic.message
              ~substring:
                "authoring help declarations require prompt parser schema version 4")));
    assert (
      Result.is_ok
        (restore_fixture ~suffix:"help_v4" ~version:4 ~root:authored_help ~sources:[]));
    let persistent =
      {|<tool name="review" agent="review.chatmd" persistence="optional"/>|}
    in
    let require_persistence_floor result =
      match result with
      | Ok _ -> failwith "old artifact accepted persistent agents"
      | Error errors ->
        assert (
          List.exists errors ~f:(fun diagnostic ->
            String.is_substring
              diagnostic.Agent_session.Prompt_revision_builder.Diagnostic.message
              ~substring:
                "persistent agent declarations require prompt parser schema version 5"))
    in
    restore_fixture ~suffix:"persistent_v4" ~version:4 ~root:persistent ~sources:[]
    |> require_persistence_floor;
    let persistent_source =
      Agent_store.Prompt_artifact_store.Source.create
        ~relative_path:"nested.chatmd"
        ~contents:persistent
      |> store_ok
    in
    restore_fixture
      ~suffix:"nested_persistent_v4"
      ~version:4
      ~root:{|<tool name="nested" agent="nested.chatmd" local/>|}
      ~sources:[ persistent_source ]
    |> require_persistence_floor;
    assert (
      Result.is_ok
        (restore_fixture ~suffix:"persistent_v5" ~version:5 ~root:persistent ~sources:[]));
    List.iter [ 1; 2 ] ~f:(fun version ->
      let assert_floor result =
        match result with
        | Ok _ -> failwith "old parser accepted inherited reference"
        | Error diagnostics ->
          assert (
            List.exists diagnostics ~f:(fun diagnostic ->
              String.is_substring
                diagnostic.Agent_session.Prompt_revision_builder.Diagnostic.message
                ~substring:
                  "inherited tool references require prompt parser schema version 3"))
      in
      restore_fixture
        ~suffix:(sprintf "root_%d" version)
        ~version
        ~root:inherited
        ~sources:[]
      |> assert_floor;
      let source path contents =
        Agent_store.Prompt_artifact_store.Source.create ~relative_path:path ~contents
        |> store_ok
      in
      restore_fixture
        ~suffix:(sprintf "nested_%d" version)
        ~version
        ~root:{|<tool name="child" agent="child.chatmd" local/>|}
        ~sources:
          [ source "child.chatmd" {|<import src="parts/refs.chatmd"/>|}
          ; source "parts/refs.chatmd" inherited
          ]
      |> assert_floor);
    assert (
      Result.is_ok
        (restore_fixture ~suffix:"v3_inherited" ~version:3 ~root:inherited ~sources:[]));
    let future_id =
      Agent_protocol.Id.Prompt_revision.of_string "prv_future_extension" |> protocol_ok
    in
    let future =
      Agent_store.Prompt_artifact_store.Artifact.create
        ~revision_id:future_id
        ~root_relative_path:artifact.root_relative_path
        ~root_chatmd:artifact.root_chatmd
        ~sources:artifact.sources
        ~parser_schema_version:99
        ~runtime_schema_version:1
        ~created_at:timestamp
        ()
      |> store_ok
    in
    Agent_store.Prompt_artifact_store.install artifact_store ~transaction_id future
    |> store_ok;
    assert (
      Result.is_error
        (Agent_session.Prompt_revision_builder.restore
           ~artifact_store
           definition
           future_id));
    let legacy_id =
      Agent_protocol.Id.Prompt_revision.of_string "prv_legacy_extension" |> protocol_ok
    in
    let legacy =
      Agent_store.Prompt_artifact_store.Artifact.create
        ~revision_id:legacy_id
        ~root_relative_path:"root.chatmd"
        ~root_chatmd:"<developer>legacy artifact</developer>"
        ~sources:[]
        ~parser_schema_version:1
        ~runtime_schema_version:1
        ~created_at:timestamp
        ()
      |> store_ok
    in
    Agent_store.Prompt_artifact_store.install artifact_store ~transaction_id legacy
    |> store_ok;
    assert (
      Result.is_ok
        (Agent_session.Prompt_revision_builder.restore
           ~artifact_store
           definition
           legacy_id)));
  print_endline
    "script and schema closure survives deleted live sources without runtime execution";
  [%expect
    {| script and schema closure survives deleted live sources without runtime execution |}]
;;
