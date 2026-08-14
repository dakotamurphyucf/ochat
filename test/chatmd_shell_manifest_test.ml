open! Core
module CM = Prompt.Chat_markdown
module D = Chatmd_shell_spec.Diagnostic
module F = Chatmd_shell_spec.Feature
module MC = Chatmd_shell_spec.Manifest_compiler
module S = Chatmd_shell_spec.Shell_spec
module T = Chatmd_shell_spec.Shell_tool_spec

let parse source =
  Eio_main.run (fun env ->
    CM.parse_chat_inputs ~source:"manifest.chatmd" ~dir:(Eio.Stdenv.cwd env) source)
;;

let declarations source =
  List.fold (parse source) ~init:([], []) ~f:(fun (runtimes, tools) -> function
    | CM.Shell_runtime runtime -> runtime :: runtimes, tools
    | CM.Tool (CM.Shell tool) -> runtimes, tool :: tools
    | _ -> runtimes, tools)
  |> fun (runtimes, tools) -> List.rev runtimes, List.rev tools
;;

let input ?(features = F.phase1) source =
  let runtimes, tools = declarations source in
  { MC.runtimes
  ; tools
  ; scripts = []
  ; legacy_tools = []
  ; moderator_runtime = None
  ; platform = S.Macos
  ; supported_features = features
  }
;;

let compile_exn input =
  match MC.compile input with
  | Ok manifest -> manifest
  | Error diagnostics ->
    List.map diagnostics ~f:D.to_string |> String.concat ~sep:"\n" |> failwith
;;

let base_source rules =
  sprintf
    {|<shell_access id="base">
        <resolver>
          <executable id="rg" path="/usr/bin/rg" trusted="true"/>
          <executable id="git" path="/usr/bin/git" trusted="true"/>
        </resolver>
        <policy default="deny">%s</policy>
      </shell_access>
      <shell_access id="child" extends="base">
        <capabilities network="true"/>
        <resolver>
          <executable id="rg" path="/opt/bin/rg" override="true"/>
        </resolver>
      </shell_access>
      <tool name="search" type="shell" mode="fixed" runtime="child"
          executable_ref="rg"/>|}
    rules
;;

let allow_rule id program =
  sprintf {|<rule id=%S action="allow"><program value=%S/></rule>|} id program
;;

let%expect_test "manifest expands inheritance and named overrides" =
  let manifest = base_source (allow_rule "allow-git" "git") |> input |> compile_exn in
  let child =
    List.find_exn manifest.payload.runtimes ~f:(fun runtime ->
      String.equal (S.Runtime_id.to_string runtime.id) "child")
  in
  let resolver = Option.value_exn child.resolver in
  let capabilities = Option.value_exn child.capabilities in
  printf
    "extends=%b network=%b executables="
    (Option.is_none child.extends)
    (S.equal_setting Bool.equal capabilities.network (Set true));
  List.iter resolver.executables ~f:(fun executable ->
    printf "%s:%s " executable.id (Chatmd_shell_spec.Path_expr.to_string executable.path));
  printf "\n";
  [%expect {| extends=true network=true executables=rg:/opt/bin/rg git:/usr/bin/git |}]
;;

let source_with_runtime_order first second =
  sprintf
    {|<shell_access id=%S><policy default="deny"/></shell_access>
      <shell_access id=%S><policy default="ask"/></shell_access>|}
    first
    second
;;

let%expect_test "map-like declaration order does not alter manifest digest" =
  let original = source_with_runtime_order "a" "b" |> input in
  let one = compile_exn original in
  let two = compile_exn { original with runtimes = List.rev original.runtimes } in
  printf "equal=%b\n" (String.equal one.sha256 two.sha256);
  [%expect {| equal=true |}]
;;

let%expect_test "ordered policy rules alter manifest digest" =
  let first = allow_rule "one" "git" ^ allow_rule "two" "rg" in
  let second = allow_rule "two" "rg" ^ allow_rule "one" "git" in
  let one = base_source first |> input |> compile_exn in
  let two = base_source second |> input |> compile_exn in
  printf "different=%b\n" (not (String.equal one.sha256 two.sha256));
  [%expect {| different=true |}]
;;

let replace source ~pattern ~with_ =
  match String.substr_replace_first source ~pattern ~with_ with
  | replacement when String.equal replacement source ->
    failwithf "test mutation did not match %S" pattern ()
  | replacement -> replacement
;;

let security_source =
  {|<shell_access id="secure" cwd="${workspace}" pipefail="false">
      <capabilities sandbox="required" network="false" child_processes="false"
          arbitrary_code="false" privilege_change="false">
        <read path="${workspace}"/>
      </capabilities>
      <resolver><trusted_root path="/usr/bin"/></resolver>
      <environment inherit="safe"><set name="LANG" value="C"/></environment>
      <limits wall_time="30s" total_output="1MiB"/>
      <backends><seatbelt when="macos"/></backends>
      <policy default="ask"><rule id="allow-pwd" action="allow"><program value="pwd"/></rule></policy>
      <approvals provider="ui" unavailable="deny" scopes="once" durable="false"/>
      <secrets replacement="[hidden]"><from_env name="TOKEN"/></secrets>
      <audit format="stderr" content="redacted" failure="continue"/>
    </shell_access>
    <tool name="shell" type="shell" mode="structured" runtime="secure"/>|}
;;

let mutations =
  [ "cwd", "cwd=\"${workspace}\"", "cwd=\"${prompt_dir}\""
  ; "pipefail", "pipefail=\"false\"", "pipefail=\"true\""
  ; "network", "network=\"false\"", "network=\"true\""
  ; "read-root", "${workspace}\"/>", "${prompt_dir}\"/>"
  ; "resolver", "/usr/bin", "/usr/local/bin"
  ; "environment", "value=\"C\"", "value=\"C.UTF-8\""
  ; "limits", "wall_time=\"30s\"", "wall_time=\"31s\""
  ; "backend", "allow_system_reads", "allow_system_reads"
  ; "policy", "action=\"allow\"", "action=\"deny\""
  ; "approvals", "scopes=\"once\"", "scopes=\"once,exact_session\""
  ; "secrets", "name=\"TOKEN\"", "name=\"OTHER_TOKEN\""
  ; "audit", "content=\"redacted\"", "content=\"metadata\""
  ; "tool-mode", "mode=\"structured\"", "mode=\"fixed\" command=\"pwd\""
  ]
;;

let security_mutation source (name, pattern, with_) =
  let source =
    if String.equal name "backend"
    then
      String.substr_replace_first
        source
        ~pattern:"<seatbelt when=\"macos\"/>"
        ~with_:"<seatbelt when=\"macos\" allow_system_reads=\"false\"/>"
    else replace source ~pattern ~with_
  in
  name, source
;;

let%expect_test "security-sensitive fields alter the manifest digest" =
  let original = security_source |> input |> compile_exn in
  List.iter mutations ~f:(fun mutation ->
    let name, source = security_mutation security_source mutation in
    let changed = source |> input |> compile_exn in
    printf "%s=%b\n" name (not (String.equal original.sha256 changed.sha256)));
  [%expect
    {|
    cwd=true
    pipefail=true
    network=true
    read-root=true
    resolver=true
    environment=true
    limits=true
    backend=true
    policy=true
    approvals=true
    secrets=true
    audit=true
    tool-mode=true
    |}]
;;

let%expect_test "source content digest changes invalidate authorization" =
  let one = security_source |> input |> compile_exn in
  let two = "\n" ^ security_source |> input |> compile_exn in
  printf "different=%b\n" (not (String.equal one.sha256 two.sha256));
  [%expect {| different=true |}]
;;

let%expect_test "literal secret values are excluded from inspectable manifest" =
  let source value =
    sprintf
      {|<shell_access id="secret"><secrets><literal value=%S/></secrets></shell_access>|}
      value
  in
  let manifest = source "super-private-value" |> input |> compile_exn in
  printf
    "contains-secret=%b contains-descriptor=%b\n"
    (String.is_substring manifest.canonical_json ~substring:"super-private-value")
    (String.is_substring manifest.canonical_json ~substring:"literal-secret");
  [%expect {| contains-secret=false contains-descriptor=true |}]
;;

let diagnostic_codes input =
  match MC.compile input with
  | Ok _ -> failwith "expected manifest compilation failure"
  | Error diagnostics -> List.map diagnostics ~f:(fun diagnostic -> diagnostic.D.code)
;;

let%expect_test "diagnostic ordering is stable" =
  let source =
    {|<shell_access id="cycle-a" extends="cycle-b"/>
      <shell_access id="cycle-b" extends="cycle-a"/>|}
  in
  let one = diagnostic_codes (input source) in
  let two = diagnostic_codes (input source) in
  print_s [%sexp (one : string list), (List.equal String.equal one two : bool)];
  [%expect {| ((shell.inheritance_cycle) true) |}]
;;

let%expect_test "unsupported features fail before runtime instantiation" =
  let source =
    {|<shell_access id="raw"><capabilities sandbox="direct_unsafe"/>
        <backends merge="replace"><direct when="macos"/></backends>
      </shell_access>
      <tool name="raw" type="shell" mode="raw" runtime="raw" executable="/bin/zsh"/>|}
  in
  let codes = diagnostic_codes (input ~features:F.phase1 source) in
  print_s [%sexp (codes : string list)];
  [%expect {| (shell.unsupported_feature) |}]
;;
