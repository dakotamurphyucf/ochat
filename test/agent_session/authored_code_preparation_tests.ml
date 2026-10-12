open Core
open Fixtures
module B = Agent_session.Runtime_builder
module R = Agent_session.Prompt_revision
module Builder = Agent_session.Prompt_revision_builder
module Code = Agent_session.Authored_code_preparation
module Source = Agent_session.Authored_agent_source
module Store = Agent_store.Prompt_artifact_store
module Startup = Chat_response.Startup_compilation
module EC = Chat_response.Extension_compiler
module C = Chat_response.Tool_capability

let%expect_test "selected specialist replaces code while retaining native authority" =
  with_actor_workspace (fun env workspace ->
    Eio.Switch.run (fun sw ->
      let root = Eio.Path.(Eio.Stdenv.fs env / workspace.canonical_root.native_path) in
      List.iter [ "agents"; "session"; "cache" ] ~f:(fun name ->
        Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(root / name));
      let save name text =
        Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(root / name) text
      in
      save
        "root.chatmd"
        {|<tool name="specialist" agent="agents/child.chatmd" local persistence="persistent"/>|};
      save "agents/input.json" {|{"type":"object"}|};
      save "agents/output.json" {|{"type":"string"}|};
      let authored ?(native = "read_file") ?(hook_state = "0") label =
        sprintf
          {|<tool name="%s"/>
<script id="ghost" kind="moderator" language="chatml" api="extensibility-v1">
let initial_state = fail("moderator initialized during preparation")
let on_event ctx state event = Task.pure(state)
</script>
<script id="handler" kind="tool" language="chatml">
let run ctx input = Task.pure(`Complete(`String("%s-managed")))
</script>
<tool name="managed" type="chatml" script="handler" entrypoint="run" input_schema="input.json" output_schema="output.json"/>
<script id="reply" kind="shell_before_interceptor" language="chatml">
let initial_state = %s
let before event state = Task.bind(Intercept.respond("%s:" ++ to_string(state), ""), fun ignored -> Task.pure(state+1))
</script>
<shell_access id="private"><policy default="allow" merge="replace"/>
<interceptors><interceptor id="reply" phase="before" script="reply" lifecycle="session" failure="error"/></interceptors>
</shell_access>
<tool name="probe" type="shell" mode="fixed" runtime="private" command="/bin/echo" result="structured"/>|}
          native
          label
          hook_state
          label
      in
      save "agents/child.chatmd" (authored "A");
      let definition =
        Agent_session.Prompt_definition.create
          ~root_invocation:Ui
          ~id:prompt_id
          ~config_name:"selected-code"
          ~root_file:(Eio.Path.native_exn Eio.Path.(root / "root.chatmd"))
          ~allowed_workspaces:[ workspace_id ]
          ~permission_profile:"interactive"
          ~runtime_policy:None
          ~enabled:true
          ~description:None
        |> store_ok
      in
      let artifacts =
        Store.create ~env ~root:(Eio.Path.native_exn Eio.Path.(root / "artifacts"))
        |> store_ok
      in
      let build () =
        Builder.build
          ~env
          ~artifact_store:artifacts
          ~transaction_id:(Agent_protocol.Id.Transaction.create ())
          ~created_at:timestamp
          definition
        |> Authored_agent_source_tests.built
      in
      let parent = build () in
      let policy = script_preparation () in
      let paths : Agent_session.Runtime_paths.t =
        { workspace = root
        ; tool_dir = root
        ; prompt_dir = R.materialized_tree parent
        ; session_dir = Eio.Path.(root / "session")
        ; cache_dir = Eio.Path.(root / "cache")
        ; home = root
        }
      in
      let inference =
        Inference_ports.create
          ~config:(Chat_response.Config.of_elements (R.elements parent))
          ()
      in
      let original =
        B.prepare_authored_resources
          ~startup:None
          ~agent_source:None
          ~script_preparation:(Some policy)
          ~content_limits:None
          ~native_registrations:[]
          ~parent_revision:parent
          ~tool_name:"specialist"
          ~native_service_revision:None
          ~env
          ~inference_context:inference.context
          ~inference_identity:inference.identity
          ~on_inference_attempt:ignore
          ~on_inference_completion:ignore
          ~on_inference_observation:ignore
          ~sw
          ~paths
          ~storage_paths:paths
          ~session_id
          ~one_off_policy:Chat_response.One_off_request.default_policy
          ~authoring_validation_host:None
          ~manifest_authorizer:(fun _ ->
            Shell_runtime.Manifest_authorizer.assume_authorized)
          ~approval_provider:Shell_runtime.Approval_broker.None_available
          ~approval_store:(Shell_access.Approval.create_store ())
        |> protocol_ok
      in
      let borrowed : Code.Original.t =
        { source = original.source
        ; revision = original.revision
        ; native = original.resources.native
        ; definition = original.resources.definition
        ; managed = original.resources.managed
        ; agent_source = None
        ; prepare_persistent_source = None
        }
      in
      let selected text =
        save "agents/child.chatmd" text;
        let parent = build () in
        Agent_session.Runtime_script_preparation.with_authored_child
          (Some policy)
          ~parent
          ~tool_name:"specialist"
          ~content_limits:None
          ~env
          ~checkpoint:Eio.Fiber.check
          ~f:(fun _ ~source ~revision:_ ->
            let artifact =
              Source.artifact
                source
                ~revision_id:(Agent_protocol.Id.Prompt_revision.create ())
                ~created_at:timestamp
              |> store_ok
            in
            Store.install
              artifacts
              ~transaction_id:(Agent_protocol.Id.Transaction.create ())
              artifact
            |> store_ok;
            Ok
              (Builder.restore ~artifact_store:artifacts definition artifact.revision_id
               |> Authored_agent_source_tests.built))
        |> protocol_ok
      in
      let prepare ?(checkpoint = Eio.Fiber.check) selected =
        Startup.with_context
          ~limits:(Agent_session.Runtime_script_preparation.compilation policy)
          ~env
          ~checkpoint
          ~map_error:(fun error ->
            Agent_protocol.Error.invalid_request error.Chatml_compilation.message)
          ~f:(fun startup ->
            Code.prepare
              borrowed
              ~selected
              ~script_preparation:policy
              ~startup
              ~env
              ~sw
              ~manifest_authorizer:Shell_runtime.Manifest_authorizer.assume_authorized
              ~approval_provider:Shell_runtime.Approval_broker.None_available
              ~response_dir:root
              ~extension_snapshots:[]
              ~persist_extension_snapshots:(fun _ -> Ok ())
              ~checkpoint)
          ()
      in
      let rejected =
        selected
          (authored
             ~native:"apply_patch"
             ~hook_state:{|fail("invalid footprint reached initializer")|}
             "invalid")
      in
      (match prepare rejected with
       | Error error ->
         assert (String.is_substring error.message ~substring:"native tool authority")
       | Ok _ -> failwith "changed native footprint accepted");
      let chosen = selected (authored "B") in
      let marker = Failure "selected checkpoint origin" in
      (match prepare ~checkpoint:(fun () -> raise marker) chosen with
       | exception exn -> assert (phys_equal exn marker)
       | Ok _ | Error _ -> failwith "original checkpoint swallowed");
      let fresh = prepare chosen |> protocol_ok in
      Code.check fresh ~original:borrowed ~selected:chosen |> protocol_ok;
      assert (phys_equal borrowed.native.functions (Code.native fresh).functions);
      assert (phys_equal borrowed.native.capabilities (Code.native fresh).capabilities);
      let managed = Code.managed fresh |> Option.value_exn in
      let old_managed = borrowed.managed |> Option.value_exn in
      assert (
        phys_equal
          (Chat_response.Managed_tool_registry.capabilities old_managed)
          (Chat_response.Managed_tool_registry.capabilities managed));
      let prepared =
        EC.prepared_tools (Code.definition fresh |> Option.value_exn) |> List.hd_exn
      in
      let config =
        Chatml_host_runtime.default_runtime_config
          ~surface:Chatml.Chatml_extension_surface.tool_v1
          ()
      in
      let value =
        EC.Program.run_in_scope
          (EC.program prepared)
          ~control:None
          ~config
          ~entrypoint:"run"
          ~arguments:[ Chatml.Chatml_lang.VUnit; VVariant ("Null", []) ]
          ()
        |> Result.map_error ~f:(fun _ -> "managed execution failed")
        |> Result.ok_or_failwith
      in
      (match value with
       | VVariant ("Complete", [ VVariant ("String", [ VString text ]) ]) ->
         [%test_eq: string] "B-managed" text
       | _ -> failwith "unexpected managed value");
      let probe =
        C.find
          (Lazy.force borrowed.native.capabilities |> Background_execution_tests.cap)
          ~name:"probe"
        |> Background_execution_tests.cap
        |> C.native_implementation
        |> Option.value_exn
      in
      let view = Code.shell_code_view fresh |> Option.value_exn in
      Shell_runtime.Registry.Code_view.with_execution
        view
        ~checkpoint:Eio.Fiber.check
        (fun execution ->
           Shell_runtime.Call_context.with_services
             ~prepare_executor:
               (Shell_runtime.Registry.Code_view.Execution.prepare_executor execution)
             (fun () ->
                Ok
                  { Shell_runtime.Call_context.session_id = "selected-specialist"
                  ; approval_store = Shell_access.Approval.create_store ()
                  ; approval_provider = Auto_deny
                  ; check = (fun () -> Ok ())
                  })
             (fun () ->
                match probe.Ochat_function.run "{}" with
                | Openai.Responses.Tool_output.Output.Text text ->
                  [%test_eq: string]
                    "B:0"
                    (Jsonaf.of_string text
                     |> Jsonaf.member_exn "stdout"
                     |> Jsonaf.string_exn)
                | _ -> failwith "unexpected native output"))
      |> Result.ok_or_failwith;
      print_endline
        "native footprint rejected before new hooks; same functions/capabilities; \
         selected managed/native hook executed"));
  [%expect
    {|native footprint rejected before new hooks; same functions/capabilities; selected managed/native hook executed|}]
;;
