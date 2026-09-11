open Core
open Fixtures
module P = Agent_protocol
module B = Agent_session.Runtime_builder
module G = Agent_session.Generated_definition
module C = Chat_response.Tool_capability
module M = Chat_response.Managed_tool_registry
module E = Chat_response.Extension_compiler

let capability_ok result =
  Result.map_error result ~f:(fun error -> error.C.message) |> Result.ok_or_failwith
;;

let capabilities (resources : B.resources) =
  Lazy.force resources.native.capabilities |> capability_ok
;;

let%expect_test
    "resource reconstruction preserves captured handlers without evaluating ancestors"
  =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let root =
        Eio.Path.(Eio.Stdenv.fs env / workspace_instance.canonical_root.native_path)
      in
      List.iter [ "data"; "cache"; "session" ] ~f:(fun name ->
        Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(root / name));
      Eio.Path.save
        ~create:(`Exclusive 0o600)
        Eio.Path.(root / "data/value.txt")
        "original resource root";
      Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(root / "schema.json") "true";
      Eio.Path.save
        ~create:(`Exclusive 0o600)
        Eio.Path.(root / "root.chatmd")
        {|<developer>Ancestor resources.</developer>
<tool name="run_chatml"/>
<tool name="read_file"><read id="data" path="${workspace}/data"/></tool>
<script id="owner" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = fail("ancestor initializer must not execute")
let on_event ctx state event = Task.pure(state)
</script>
<script id="reader-script" language="chatml" kind="tool">
let poison = fail("standalone initializer must not execute")
let run ctx input =
  let* result = Tool.call("read_file", input) in
  match result with
  | `Ok(value) -> Task.pure(`Complete(value))
  | `Error(message) -> Task.pure(`Fail({code = "read"; message = message; retryable = false; details = `Null}))
</script>
<tool name="reader" type="chatml" script="reader-script" entrypoint="run" input_schema="schema.json" output_schema="schema.json"><uses tool="read_file"/></tool>|};
      let definition =
        Agent_session.Prompt_definition.create
          ~id:prompt_id
          ~config_name:"resource-preparation"
          ~root_file:(Eio.Path.native_exn Eio.Path.(root / "root.chatmd"))
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
          ~root:(Eio.Path.native_exn Eio.Path.(root / "artifacts"))
        |> store_ok
      in
      let revision =
        Agent_session.Prompt_revision_builder.build
          ~env
          ~artifact_store
          ~transaction_id
          ~created_at:timestamp
          definition
        |> Result.map_error ~f:(fun errors ->
          Sexp.to_string_hum
            [%sexp (errors : Agent_session.Prompt_revision_builder.Diagnostic.t list)])
        |> Result.ok_or_failwith
      in
      let paths : Agent_session.Runtime_paths.t =
        { tool_dir = root
        ; workspace = root
        ; prompt_dir = Agent_session.Prompt_revision.materialized_tree revision
        ; session_dir = Eio.Path.(root / "session")
        ; cache_dir = Eio.Path.(root / "cache")
        ; home = root
        }
      in
      let prepare () =
        B.prepare_resources
          ~env
          ~sw
          ~paths
          ~storage_paths:paths
          ~revision
          ~session_id
          ~one_off_policy:Chat_response.One_off_request.default_policy
          ~authoring_validation_host:None
          ~manifest_authorizer:(fun _ -> failwith "unexpected shell authorization")
          ~approval_provider:Shell_runtime.Approval_broker.None_available
          ~approval_store:(Shell_access.Approval.create_store ())
      in
      let original = prepare () |> protocol_ok in
      let names resources =
        C.references (capabilities resources)
        |> List.map ~f:(fun r -> r.C.name)
        |> List.sort ~compare:String.compare
      in
      [%test_eq: string list] [ "read_file"; "reader"; "run_chatml" ] (names original);
      let helper = C.find (capabilities original) ~name:"run_chatml" |> capability_ok in
      assert (C.equal_result_contract (C.result_contract helper) Invocation_v1);
      let child_definition parent =
        let caps = capabilities parent in
        let selected = C.select caps ~names:[ "reader" ] |> capability_ok in
        let bundle =
          Chatmd_source_bundle.create
            ~root_file:"child.chatmd"
            ~sources:
              [ ( "child.chatmd"
                , {|<developer>Child resources.</developer><tool type="inherited" name="reader"/>
<script id="child-policy" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = fail("child initializer must not execute")
let on_event ctx state event = Task.pure(state)
</script>|}
                )
              ]
            ()
          |> Result.ok_or_failwith
        in
        G.prepare
          ~env
          ~dir:root
          ~revision_id:(P.Id.Prompt_revision.create ())
          ~created_at:timestamp
          ~current_capabilities:(fun () -> caps)
          ~references:(C.references selected)
          bundle
        |> Generated_definition_tests.get
      in
      let admitted = child_definition original in
      let child =
        B.inherit_resources ~parent:original ~definition:admitted |> protocol_ok
      in
      let grandchild =
        B.inherit_resources ~parent:child ~definition:(child_definition child)
        |> protocol_ok
      in
      [%test_eq: string list] [ "reader" ] (names grandchild);
      let delegated =
        M.delegate_standalone
          (Option.value_exn grandchild.managed)
          ~selected:(capabilities grandchild)
        |> capability_ok
      in
      let delegated_definition = M.delegation_definition delegated in
      assert (List.is_empty (E.compiled_scripts delegated_definition));
      let original_handler =
        M.resolve
          (Option.value_exn original.managed)
          (C.find (capabilities original) ~name:"reader" |> capability_ok)
        |> capability_ok
      in
      (match E.prepared_tools delegated_definition with
       | [ handler ] -> assert (phys_equal handler original_handler)
       | _ -> failwith "delegated standalone handler was not preserved");
      let hidden = M.capabilities (M.delegation_registry delegated) in
      let reader = C.find hidden ~name:"read_file" |> capability_ok in
      let original_reader =
        C.find (capabilities original) ~name:"read_file" |> capability_ok
      in
      assert (phys_equal reader original_reader);
      let _, runners =
        Ochat_function.functions [ Option.value_exn (C.native_implementation reader) ]
      in
      (* Explicit trusted fixture invocation tests retained native root identity;
         resource preparation itself installs no actor invocation permissions. *)
      (match
         (Hashtbl.find_exn runners "read_file")
           ~invocation:Ochat_function.Invocation.silent
           {|{"root":"data","file":"value.txt"}|}
       with
       | Openai.Responses.Tool_output.Output.Text text ->
         assert (String.is_substring text ~substring:"original resource root")
       | _ -> failwith "unexpected resource reader output");
      Eio.Path.save
        ~create:(`Or_truncate 0o600)
        Eio.Path.(root / "root.chatmd")
        "not a replacement captured program";
      let fresh = prepare () |> protocol_ok in
      let pins resources =
        Chat_response.Background_request.capability_pins (capabilities resources)
        |> protocol_ok
      in
      [%test_eq: (string * string) list] (pins original) (pins fresh);
      assert (Result.is_error (B.inherit_resources ~parent:fresh ~definition:admitted));
      let fresh_child =
        B.inherit_resources ~parent:fresh ~definition:(child_definition fresh)
        |> protocol_ok
      in
      [%test_eq: string list] [ "reader" ] (names fresh_child);
      let stored =
        Eio.Path.(
          paths.prompt_dir / Agent_session.Prompt_revision.root_relative_path revision)
      in
      Eio.Path.unlink stored;
      Eio.Path.save ~create:(`Exclusive 0o400) stored "corrupted captured source";
      assert (Result.is_error (prepare ()));
      print_endline
        "no initializer evaluation; captured standalone closure and original root \
         retained across two edges; fresh bindings require readmission; corrupted tree \
         rejects"));
  [%expect
    {| no initializer evaluation; captured standalone closure and original root retained across two edges; fresh bindings require readmission; corrupted tree rejects |}]
;;
