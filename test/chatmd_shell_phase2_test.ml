open! Core
module CM = Prompt.Chat_markdown
module D = Chatmd_shell_spec.Diagnostic
module F = Chatmd_shell_spec.Feature
module M = Chatmd_shell_spec.Manifest
module MC = Chatmd_shell_spec.Manifest_compiler
module S = Chatmd_shell_spec.Shell_spec
module T = Chatmd_shell_spec.Shell_tool_spec

let parse source =
  Eio_main.run (fun env ->
    CM.parse_chat_inputs ~source:"phase2.chatmd" ~dir:(Eio.Stdenv.cwd env) source)
;;

let declarations source =
  List.fold (parse source) ~init:([], []) ~f:(fun (runtimes, tools) -> function
    | CM.Shell_runtime runtime -> runtime :: runtimes, tools
    | Tool (Shell tool) -> runtimes, tool :: tools
    | _ -> runtimes, tools)
  |> fun (runtimes, tools) -> List.rev runtimes, List.rev tools
;;

let compile source =
  let runtimes, tools = declarations source in
  MC.compile
    { runtimes
    ; tools
    ; scripts = []
    ; legacy_tools = []
    ; moderator_runtime = None
    ; platform = S.Macos
    ; supported_features = F.phase2
    }
;;

let compile_exn source =
  match compile source with
  | Ok manifest -> manifest
  | Error diagnostics ->
    List.map diagnostics ~f:D.to_string |> String.concat ~sep:"\n" |> failwith
;;

let runtime manifest = List.hd_exn manifest.M.payload.runtimes

let%expect_test "phase2 tool modes parse with strict mode-specific schemas" =
  let source =
    {|<shell_access id="runtime" extends="builtin:yolo@1"/>
      <tool name="chain" type="shell" mode="chain" runtime="runtime"/>
      <tool name="raw" type="shell" mode="raw" runtime="runtime"
        executable="/bin/sh" arguments_before_script='["-c"]'/>
      <tool name="script" type="shell" mode="script" runtime="runtime"
        script="scripts/check.sh" interpreter="/bin/sh"
        fixed_arguments='["-e"]' verification="sha256" max_source_bytes="4096"/>|}
  in
  let manifest = compile_exn source in
  List.iter manifest.payload.tools ~f:(fun tool ->
    let mode =
      match tool.T.mode with
      | Chain -> "chain"
      | Raw raw -> sprintf "raw:%s" (String.concat ~sep:"," raw.arguments_before_script)
      | Script_file script ->
        sprintf
          "script:%d:%d"
          script.max_source_bytes
          (List.length script.fixed_arguments)
      | Fixed _ | Structured -> "unexpected"
    in
    printf "%s=%s\n" tool.name mode);
  print_s [%sexp (manifest.payload.required_features : string list)];
  [%expect
    {|
    chain=chain
    raw=raw:-c
    script=script:4096:1
    (shell.backend.direct.v1 shell.tool.chain.v1 shell.tool.raw.v1
     shell.tool.script.v1)
    |}]
;;

let%expect_test "mode-incompatible attributes are rejected" =
  let source =
    {|<shell_access id="runtime" extends="builtin:yolo@1"/>
      <tool name="chain" type="shell" mode="chain" runtime="runtime"
        executable="/bin/sh"/>|}
  in
  let rejected =
    try
      ignore (compile source : (M.t, D.t list) result);
      false
    with
    | exn ->
      String.is_substring (Exn.to_string exn) ~substring:"shell.tool_mode_attribute"
  in
  print_s [%sexp (rejected : bool)];
  [%expect {| true |}]
;;

let%expect_test "yolo expands to explicit unrestricted local process authority" =
  let manifest = compile_exn {|<shell_access id="yolo" extends="builtin:yolo"/>|} in
  let runtime = runtime manifest in
  let capabilities = Option.value_exn runtime.capabilities in
  let policy = Option.value_exn runtime.policy in
  let approvals = Option.value_exn runtime.approvals in
  let backends = Option.value_exn runtime.backends in
  let resolver = Option.value_exn runtime.resolver in
  printf
    "requested=%s resolved=%s sandbox=%s rw=%d/%d powers=%b/%b/%b/%b search=%d policy=%s \
     rules=%d approvals=%s direct=%b\n"
    (Option.value_exn runtime.requested_profile)
    (Option.value_exn runtime.resolved_profile)
    (match capabilities.sandbox with
     | Set Direct_unsafe -> "direct_unsafe"
     | Set Required | Set Preferred | Inherit | Clear -> "other")
    (List.length capabilities.read)
    (List.length capabilities.write)
    (match capabilities.network with
     | Set value -> value
     | Inherit | Clear -> false)
    (match capabilities.child_processes with
     | Set value -> value
     | Inherit | Clear -> false)
    (match capabilities.arbitrary_code with
     | Set value -> value
     | Inherit | Clear -> false)
    (match capabilities.privilege_change with
     | Set value -> value
     | Inherit | Clear -> false)
    (List.length resolver.search_path)
    (match policy.default with
     | Set Allow -> "allow"
     | Set Ask -> "ask"
     | Set Deny -> "deny"
     | Inherit | Clear -> "other")
    (List.length policy.rules)
    (match approvals.provider with
     | Set No_provider -> "none"
     | Set Ui -> "ui"
     | Inherit | Clear -> "other")
    (List.exists backends.values ~f:(function
       | Direct _ -> true
       | Seatbelt _ | Bubblewrap _ | External _ -> false));
  [%expect
    {| requested=builtin:yolo resolved=builtin:yolo@1 sandbox=direct_unsafe rw=1/1 powers=true/true/true/true search=0 policy=allow rules=0 approvals=none direct=true |}]
;;

let%expect_test "built-in aliases pin concrete versions in the manifest" =
  let alias =
    compile_exn {|<shell_access id="dev" extends="builtin:workspace-development"/>|}
  in
  let pinned =
    compile_exn {|<shell_access id="dev" extends="builtin:workspace-development@1"/>|}
  in
  let alias_runtime = runtime alias in
  let pinned_runtime = runtime pinned in
  printf
    "alias=%s pinned=%s concrete=%s digest-diff=%b\n"
    (Option.value_exn alias_runtime.requested_profile)
    (Option.value_exn pinned_runtime.requested_profile)
    (Option.value_exn alias_runtime.resolved_profile)
    (not (String.equal alias.sha256 pinned.sha256));
  [%expect
    {| alias=builtin:workspace-development pinned=builtin:workspace-development@1 concrete=builtin:workspace-development@1 digest-diff=true |}]
;;

let%expect_test "workspace profiles preserve least privilege defaults" =
  let readonly =
    compile_exn {|<shell_access id="readonly" extends="builtin:workspace-readonly@1"/>|}
    |> runtime
  in
  let development =
    compile_exn
      {|<shell_access id="development" extends="builtin:workspace-development@1"/>|}
    |> runtime
  in
  let capability runtime = Option.value_exn runtime.S.capabilities in
  let sandbox capabilities =
    match capabilities.S.sandbox with
    | Set Required -> "required"
    | Set Preferred -> "preferred"
    | Set Direct_unsafe -> "direct_unsafe"
    | Inherit | Clear -> "unresolved"
  in
  let privilege capabilities =
    match capabilities.S.privilege_change with
    | Set value -> value
    | Inherit | Clear -> true
  in
  printf
    "readonly=%s/%b development=%s/%b\n"
    (sandbox (capability readonly))
    (privilege (capability readonly))
    (sandbox (capability development))
    (privilege (capability development));
  [%expect {| readonly=required/false development=preferred/false |}]
;;

let%expect_test "manifest authorization visibly warns for yolo" =
  let manifest = compile_exn {|<shell_access id="yolo" extends="builtin:yolo@1"/>|} in
  let summary = ref "" in
  let authorizer request =
    summary := request.Shell_runtime.Manifest_authorizer.summary;
    Shell_runtime.Manifest_authorizer.Authorize_once
  in
  ignore
    (Shell_runtime.Manifest_authorizer.authorize authorizer manifest
     : ( Shell_runtime.Manifest_authorizer.grant
         , Shell_runtime.Manifest_authorizer.error )
         result);
  printf
    "critical=%b full-authority=%b\n"
    (String.is_substring !summary ~substring:"CRITICAL")
    (String.is_substring !summary ~substring:"full local process authority");
  [%expect {| critical=true full-authority=true |}]
;;
