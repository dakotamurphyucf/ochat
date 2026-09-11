open Core
open Fixtures
module P = Agent_protocol
module B = Agent_session.Runtime_builder
module R = Agent_session.Prompt_revision
module Source = Agent_session.Authored_agent_source
module Artifacts = Agent_store.Prompt_artifact_store
module C = Chat_response.Tool_capability

let%expect_test "authored private resources use captured specialist source coordinates" =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let root =
        Eio.Path.(Eio.Stdenv.fs env / workspace_instance.canonical_root.native_path)
      in
      List.iter [ "agents"; "shared"; "session"; "cache" ] ~f:(fun directory ->
        Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(root / directory));
      let save name contents =
        Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(root / name) contents
      in
      save
        "root.chatmd"
        {|<developer>Parent instructions must not become specialist instructions.</developer>
<tool name="apply_patch"/>
<tool name="researcher" agent="agents/researcher.chatmd" local persistence="optional"/>|};
      save
        "agents/researcher.chatmd"
        {|<config model="specialist-model" reasoning_effort="high"/>
<developer>Captured specialist instructions.</developer>
<import src="../shared/private.chatmd"/>
<script id="coordinate" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = fail("private resource preparation must not initialize a moderator")
let on_event ctx state event = Task.pure(state)
</script>|};
      save "shared/schema.json" {|{"type":"object"}|};
      save
        "shared/private.chatmd"
        {|<tool name="read_file"><read id="source" path="${source_dir}"/></tool>
<script id="read-script" language="chatml" kind="tool" src="reader.chatml"/>
<tool name="reader" type="chatml" script="read-script" entrypoint="run" input_schema="schema.json" output_schema="schema.json"><uses tool="read_file"/></tool>|};
      save
        "shared/reader.chatml"
        {|let poison = fail("private resource preparation must not initialize a tool")
let run ctx input =
  let* result = Tool.call("read_file", input) in
  match result with
  | `Ok(value) -> Task.pure(`Complete(value))
  | `Error(message) -> Task.pure(`Fail({code = "read"; message = message; retryable = false; details = `Null}))|};
      let definition =
        Agent_session.Prompt_definition.create
          ~id:prompt_id
          ~config_name:"authored-private-resources"
          ~root_file:(Eio.Path.native_exn Eio.Path.(root / "root.chatmd"))
          ~allowed_workspaces:[ workspace_id ]
          ~permission_profile:"interactive"
          ~runtime_policy:None
          ~enabled:true
          ~description:None
        |> store_ok
      in
      let artifact_store =
        Artifacts.create ~env ~root:(Eio.Path.native_exn Eio.Path.(root / "artifacts"))
        |> store_ok
      in
      let build () =
        Agent_session.Prompt_revision_builder.build
          ~env
          ~artifact_store
          ~transaction_id:(P.Id.Transaction.create ())
          ~created_at:timestamp
          definition
        |> Authored_agent_source_tests.built
      in
      let parent = build () in
      let paths : Agent_session.Runtime_paths.t =
        { tool_dir = root
        ; workspace = root
        ; prompt_dir = R.materialized_tree parent
        ; session_dir = Eio.Path.(root / "session")
        ; cache_dir = Eio.Path.(root / "cache")
        ; home = root
        }
      in
      let prepare ?(parent_revision = parent) () =
        B.prepare_authored_resources
          ~parent_revision
          ~tool_name:"researcher"
          ~native_service_revision:None
          ~env
          ~sw
          ~paths
          ~storage_paths:paths
          ~session_id
          ~one_off_policy:Chat_response.One_off_request.default_policy
          ~authoring_validation_host:None
          ~manifest_authorizer:(fun _ -> failwith "unexpected shell authorization")
          ~approval_provider:Shell_runtime.Approval_broker.None_available
          ~approval_store:(Shell_access.Approval.create_store ())
      in
      let original = prepare () |> protocol_ok in
      let caps prepared = Runtime_resource_tests.capabilities prepared.B.resources in
      [%test_eq: string list]
        [ "read_file"; "reader" ]
        (C.references (caps original)
         |> List.map ~f:(fun reference -> reference.C.name)
         |> List.sort ~compare:String.compare);
      assert (not (Artifacts.exists artifact_store (R.id original.revision)));
      [%test_eq: string]
        "agents/researcher.chatmd"
        (R.root_relative_path original.revision);
      [%test_eq: string]
        (Eio.Path.native_exn (R.materialized_tree parent))
        (Eio.Path.native_exn (R.materialized_tree original.revision));
      assert (
        List.exists (R.elements original.revision) ~f:(function
          | Prompt.Chat_markdown.Developer { content = Some (Text text); _ } ->
            String.equal text "Captured specialist instructions."
          | _ -> false));
      (* Explicit trusted fixture invocation checks the actual read binding, not
         merely its advertised name. Preparation itself runs no tool or script. *)
      let reader =
        C.find (caps original) ~name:"read_file" |> Runtime_resource_tests.capability_ok
      in
      let _, runners =
        Ochat_function.functions [ Option.value_exn (C.native_implementation reader) ]
      in
      let read file =
        match
          (Hashtbl.find_exn runners "read_file")
            ~invocation:Ochat_function.Invocation.silent
            (Jsonaf.to_string
               (`Object [ "root", `String "source"; "file", `String file ]))
        with
        | Openai.Responses.Tool_output.Output.Text text -> text
        | _ -> failwith "unexpected private reader output"
      in
      assert (String.is_substring (read "schema.json") ~substring:{|{"type":"object"}|});
      assert (
        not
          (String.is_substring
             (read "../agents/researcher.chatmd")
             ~substring:"Captured specialist instructions."));
      save "agents/researcher.chatmd" "<developer>Changed live specialist.</developer>";
      save "shared/schema.json" {|{"type":"string"}|};
      save "shared/private.chatmd" {|<tool name="apply_patch"/>|};
      let fresh = prepare () |> protocol_ok in
      let pins prepared =
        Chat_response.Background_request.capability_pins (caps prepared) |> protocol_ok
      in
      [%test_eq: (string * string) list] (pins original) (pins fresh);
      assert (P.Id.Prompt_revision.equal (R.id original.revision) (R.id fresh.revision));
      assert (
        not (String.equal (C.fingerprint (caps original)) (C.fingerprint (caps fresh))));
      let changed_parent = build () in
      assert (
        Result.is_error (Source.resource_revision ~parent:changed_parent original.source));
      List.iter [ "Process.run"; "Model.call" ] ~f:(fun forbidden ->
        save
          "agents/researcher.chatmd"
          (sprintf
             {|<script id="restricted" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = fail("rejected compilation must not initialize")
let forbidden = %s
let on_event ctx state event = Task.pure(state)
</script>|}
             forbidden);
        let revision = build () in
        match prepare ~parent_revision:revision () with
        | Error error ->
          assert (String.is_substring error.message ~substring:"chatml.invalid_handler")
        | Ok _ -> failwith "direct delegated execution surface unexpectedly available");
      let stored = Eio.Path.(R.materialized_tree parent / "shared/reader.chatml") in
      Eio.Path.unlink stored;
      Eio.Path.save ~create:(`Exclusive 0o400) stored "tampered captured script";
      assert (Result.is_error (prepare ()));
      print_endline
        "private captured declarations and source-relative read roots; no public tools, \
         speculative artifact or initializers; stable pins across live edits; fresh \
         bindings and source-owner/tamper checks PASS"));
  [%expect
    {| private captured declarations and source-relative read roots; no public tools, speculative artifact or initializers; stable pins across live edits; fresh bindings and source-owner/tamper checks PASS |}]
;;
