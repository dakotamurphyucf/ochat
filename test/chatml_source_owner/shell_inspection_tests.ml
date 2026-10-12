open Core
open Support
module Daemon = Agent_server.Daemon
module Revision = Agent_session.Prompt_revision
module M = Chatmd_shell_spec.Manifest
module Role = Chat_response.Script_contract_plan.Program.Role.Moderator_invocation

let definition_id = Agent_server.Catalog_identity.prompt_definition "restart.prompt"

let with_daemon env parent root role f =
  Eio.Switch.run (fun sw ->
    let home = Eio.Path.native_exn parent in
    let workspace = Eio.Path.native_exn root in
    let daemon =
      Daemon.start
        ~sw
        ~env
        ~config:
          (Agent_server_test_support.config
             home
             workspace
             (Filename.concat workspace "root.chatmd"))
        ~tool_dir:workspace
        ~home
        ~process_start_identity:None
        ~options:
          { Daemon.default_options with
            startup_mode = Operator_only
          ; root_invocation = role
          ; inference_policy =
              Agent_server_test_support.inference_policy
                ~default_model:"inspection"
                ~post_stream:(fun ~sw:_ ~inputs:_ ->
                  failwith "inspection must not dispatch a provider")
          }
        ()
      |> Agent_server_test_support.protocol_ok
    in
    Exn.protect
      ~f:(fun () -> f daemon)
      ~finally:(fun () -> Daemon.shutdown daemon |> Agent_server_test_support.protocol_ok))
;;

let inspect daemon selection =
  Daemon.inspect_prompt_shell daemon ~definition_id ~selection ~checkpoint:ignore
  |> Agent_server_test_support.protocol_ok
;;

let%expect_test
    "operator inspection binds captured graph, retains old contracts and never \
     initializes"
  =
  with_workspace (fun env parent root ->
    save
      root
      "root.chatmd"
      {|<script id="owner" language="chatml" kind="moderator" project="chatml.project.json">
let initial_state = Counter.seed
let on_event event state = Task.pure(state)
</script>
<script id="reviewer" language="chatml" kind="shell_reviewer" project="chatml.project.json">
let initial_state = fail("inspection cannot initialize the reviewer")
let review ev state = Task.bind(Review.deny(to_string(Counter.seed)), fun ignored -> Task.pure(state))
</script>
<shell_access id="checks" extends="builtin:yolo@1">
<reviewers strategy="first_terminal">
<reviewer id="review" kind="chatml" script="reviewer" lifecycle="session" failure="deny"/>
</reviewers>
</shell_access>|};
    Eio.Path.mkdir ~perm:0o700 Eio.Path.(root / "lib");
    save
      root
      "chatml.project.json"
      {|{"schema_version":1,"namespace":"inspection","source_dirs":["lib"]}|};
    save root "lib/counter.chatml" "let seed = 40 let private_value = true";
    save root "lib/counter.chatmli" "val seed : int";
    let original_revision, original_manifest =
      with_daemon env parent root Role.Headless (fun daemon ->
        let revision = Daemon_history_tests.ready daemon in
        let inspected = inspect daemon Current in
        assert (Option.is_some inspected.manifest.authorization);
        [%test_eq: string]
          "ochat.shell.execution.v2"
          (M.encoding_version inspected.manifest);
        Revision.id revision, inspected.manifest)
    in
    save root "lib/counter.chatml" "let seed = 41 let private_value = true";
    with_daemon env parent root Role.Headless (fun daemon ->
      let fresh = (inspect daemon Current).manifest in
      [%test_eq: string] (M.resource_sha256 original_manifest) (M.resource_sha256 fresh);
      assert (not (String.equal original_manifest.sha256 fresh.sha256)));
    List.iter
      [ "root.chatmd"
      ; "chatml.project.json"
      ; "lib/counter.chatml"
      ; "lib/counter.chatmli"
      ]
      ~f:(fun path -> Eio.Path.unlink Eio.Path.(root / path));
    with_daemon env parent root Role.Ui (fun daemon ->
      assert (
        Result.is_error
          (Daemon.inspect_prompt_shell
             daemon
             ~definition_id
             ~selection:Current
             ~checkpoint:ignore));
      let restored =
        inspect
          daemon
          (Retained { revision_id = original_revision; invocation = Headless })
      in
      [%test_eq: string] original_manifest.canonical_json restored.manifest.canonical_json;
      assert (
        Result.is_error
          (Daemon.inspect_prompt_shell
             daemon
             ~definition_id
             ~selection:(Retained { revision_id = original_revision; invocation = Ui })
             ~checkpoint:ignore));
      let payload =
        Shell_runtime.Manifest_signature.payload
          ~manifest:restored.manifest
          ~issuer:"operator"
          ~audience:[ "ochat" ]
          ~issued_at_unix:0L
          ~expires_at_unix:None
      in
      [%test_eq: string] original_manifest.sha256 payload.canonical_manifest_sha256;
      print_endline "local dependency edit needs a new executable grant";
      print_endline "deleted sources: exact retained headless identity remains signable";
      print_endline "current UI selection cannot replace the captured contract"));
  [%expect
    {|
    local dependency edit needs a new executable grant
    deleted sources: exact retained headless identity remains signable
    current UI selection cannot replace the captured contract
    |}]
;;

let%test_unit "legacy inspection preserves its exact text allowance and v1 identity" =
  Eio_main.run (fun env ->
    let body =
      "let initial_state=0\n\
       let review ev state=Task.bind(Review.deny(\"inert\"), fun ignored -> \
       Task.pure(state))"
    in
    let elements =
      Prompt.Chat_markdown.parse_chat_inputs
        ~source:"legacy-inspection.chatmd"
        ~dir:(Eio.Stdenv.cwd env)
        (sprintf
           {|<script id="reviewer" language="chatml" kind="shell_reviewer">%s</script>
<shell_access id="checks" extends="builtin:yolo@1">
<reviewers strategy="first_terminal">
<reviewer id="review" kind="chatml" script="reviewer" lifecycle="session" failure="deny"/>
</reviewers></shell_access>|}
           body)
    in
    let module Runtime = Chat_response.Agent_runtime in
    let raw =
      Runtime.inspect_resources
        ~env
        ~platform:(Runtime.platform ())
        ~prompt_elements:elements
      |> ok
    in
    let text_allowance = 1024 in
    let complete =
      Runtime.inspect_prepared_shell
        ~env
        ~limits:
          { Chatml_compilation.wall_seconds = 10.; max_source_bytes = text_allowance }
        ~checkpoint:ignore
        ~platform:(Runtime.platform ())
        ~prompt_elements:elements
        ()
      |> Result.map_error ~f:(fun errors ->
        List.map errors ~f:Runtime.diagnostic_to_string |> String.concat ~sep:"; ")
      |> Result.ok_or_failwith
    in
    assert (String.length complete.manifest.canonical_json > text_allowance);
    assert (Option.is_none complete.manifest.authorization);
    [%test_eq: string] raw.manifest.canonical_json complete.manifest.canonical_json;
    [%test_eq: string] raw.manifest.sha256 complete.manifest.sha256)
;;
