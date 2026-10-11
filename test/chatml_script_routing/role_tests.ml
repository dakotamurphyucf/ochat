open Core
module CM = Prompt.Chat_markdown
module Plan = Chat_response.Script_contract_plan
module Role = Plan.Program.Role

let plan
      ?(moderator_invocation = Role.Moderator_invocation.Ui)
      ?(delegated = false)
      ?(shell = None)
      parsed
  =
  Plan.create
    parsed
    ~moderator_invocation
    ~moderator_surface:Chatml.Chatml_builtin_surface.empty
    ~delegated_moderator:delegated
    ~shell
    ~limits:
      (Plan.Limits.create
         ~max_programs:8
         ~max_metadata_bytes:(8 * 1024 * 1024)
         ~max_work_steps:1_000_000
       |> Or_error.ok_exn)
    ~checkpoint:ignore
  |> Routing_tests.diagnostics
;;

let parse dir raw =
  CM.prepare_chat_inputs
    ~source:"root.chatmd"
    ~source_loader:
      (Source_loader.captured_filesystem ~root:dir ~sources:[ "root.chatmd", raw ])
    ~dir
    ~preprocess:Fn.id
    ~limits:
      (CM.Limits.create
         ~max_input_bytes:1_000_000
         ~max_parse_calls:32
         ~max_tokens:10_000
         ~max_depth:32
       |> Or_error.ok_exn)
    ~checkpoint:ignore
    raw
;;

let%test_unit "authentic legacy standard and delegated roles do not use contract hashes" =
  Eio_main.run (fun env ->
    let dir = Eio.Stdenv.cwd env in
    let legacy =
      parse
        dir
        {|<script language="chatml" kind="moderator">let initial_state = ()</script>|}
    in
    (match List.map (Plan.programs (plan legacy)) ~f:Plan.Program.role with
     | [ Legacy_moderator Ui ] -> ()
     | _ -> assert false);
    let ui = List.hd_exn (Plan.programs (plan legacy)) in
    let headless =
      List.hd_exn (Plan.programs (plan ~moderator_invocation:Headless legacy))
    in
    assert (
      Chatml_source.Digest.equal
        (Plan.Program.compiler_contract ui)
        (Plan.Program.compiler_contract headless));
    assert (not (Role.equal (Plan.Program.role ui) (Plan.Program.role headless)));
    let moderator =
      parse
        dir
        {|<script id="m" language="chatml" kind="moderator" api="extensibility-v1">let initial_state = ()</script>|}
    in
    let standard = plan moderator in
    let delegated = plan ~delegated:true moderator in
    let role planned = Plan.Program.role (List.hd_exn (Plan.programs planned)) in
    assert (Role.equal (role standard) (Standard Moderator_v1));
    assert (Role.equal (role delegated) (Standard Delegated_moderator_v1)))
;;

let%expect_test "native default and selected entrypoint roles remain distinct" =
  Eio_main.run (fun env ->
    let parsed =
      parse
        (Eio.Stdenv.cwd env)
        {|<script id="matcher" language="chatml" kind="shell_matcher">let initial_state = fail("never execute in planning")</script>
<shell_access id="runtime" extends="builtin:yolo@1">
<policy><rule id="dynamic" action="deny"><chatml_match script="matcher" function="selective"/></rule></policy>
</shell_access>|}
    in
    let shell =
      Chat_response.Agent_runtime.prepare_shell_manifest
        ~platform:Macos
        ~prompt_elements:(CM.Parsed.elements parsed)
      |> function
      | Ok value -> value
      | Error errors ->
        raise_s [%sexp (errors : Chat_response.Agent_runtime.diagnostic list)]
    in
    let planned = plan ~shell:(Some shell) parsed in
    let roles = List.map (Plan.programs planned) ~f:Plan.Program.role in
    assert (
      List.equal
        Role.equal
        roles
        [ Native_hook { kind = Shell_matcher; entrypoint = "match_command" }
        ; Native_hook { kind = Shell_matcher; entrypoint = "selective" }
        ]);
    print_s [%sexp (roles : Role.t list)]);
  [%expect
    {|
    ((Native_hook (kind Shell_matcher) (entrypoint match_command))
     (Native_hook (kind Shell_matcher) (entrypoint selective)))
    |}]
;;
