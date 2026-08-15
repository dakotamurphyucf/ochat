open! Core
module CM = Prompt.Chat_markdown
module Spec = Chatmd_shell_spec

let parse_ast source =
  let lexbuf = Lexing.from_string source in
  Chatmd_parser.document (Chatmd_lexer.create ()) lexbuf
;;

let rec without_whitespace = function
  | Chatmd_ast.Text text when String.for_all text ~f:Char.is_whitespace -> None
  | Chatmd_ast.Text _ as node -> Some node
  | Chatmd_ast.Element (tag, attributes, children) ->
    Some
      (Chatmd_ast.Element (tag, attributes, List.filter_map children ~f:without_whitespace))
;;

let%expect_test "nested shell declarations are ChatMD AST elements" =
  let source =
    {|<shell_access id="readonly">
        <capabilities sandbox="required"><read path="${workspace}"/></capabilities>
        <policy default="ask">
          <rule id="allow-rg" action="allow">
            <all><basename value="rg"/><trusted_executable/></all>
          </rule>
        </policy>
      </shell_access>|}
  in
  let document = List.filter_map (parse_ast source) ~f:without_whitespace in
  print_s [%sexp (document : Chatmd_ast.node list)];
  [%expect
    {|
    ((Element Shell_access ((id (readonly)))
      ((Element (Shell_element Capabilities) ((sandbox (required)))
        ((Element (Shell_element Read) ((path (${workspace}))) ())))
       (Element (Shell_element Policy) ((default (ask)))
        ((Element (Shell_element Rule) ((id (allow-rg)) (action (allow)))
          ((Element (Shell_element All) ()
            ((Element (Shell_element Basename) ((value (rg))) ())
             (Element (Shell_element Trusted_executable) () ()))))))))))
    |}]
;;

let runtime_id_of_document = function
  | [ Chatmd_ast.Element (Shell_access, attributes, _) ] ->
    List.Assoc.find_exn attributes ~equal:String.equal "id" |> Option.value_exn
  | _ -> failwith "expected one shell_access declaration"
;;

let parse_repeatedly worker =
  for iteration = 0 to 199 do
    let id = sprintf "runtime-%d-%d" worker iteration in
    let source = sprintf "<shell_access id=%S/>" id in
    let parsed_id = parse_ast source |> runtime_id_of_document in
    if not (String.equal id parsed_id) then failwith "parser state crossed invocations"
  done
;;

let%expect_test "ChatMD lexer state is isolated across domains" =
  let domains =
    List.init 4 ~f:(fun worker -> Domain.spawn (fun () -> parse_repeatedly worker))
  in
  List.iter domains ~f:Domain.join;
  print_endline "800 concurrent parses retained their own lexer state";
  [%expect {| 800 concurrent parses retained their own lexer state |}]
;;

let%expect_test "comments, whitespace, and quoted entities survive shell parsing" =
  let source =
    {|<shell_access id="quoted">
        <!-- comments are ignored by the shared ChatMD lexer -->
        <policy default="deny">
          <rule id="quoted-rule" action="allow">
            <program_regex value="^echo &quot;safe&quot; &amp; sound$"/>
          </rule>
        </policy>
      </shell_access>|}
  in
  Eio_main.run
  @@ fun env ->
  let elements = CM.parse_chat_inputs ~dir:(Eio.Stdenv.cwd env) source in
  List.iter elements ~f:(function
    | CM.Shell_runtime { policy = Some { rules = [ rule ]; _ }; _ } ->
      print_s [%sexp (rule.matcher : Spec.Shell_spec.matcher)]
    | _ -> failwith "expected one shell runtime rule");
  [%expect {| (Program_regex "^echo \"safe\" & sound$") |}]
;;

let explicit_runtime =
  {|<shell_access id="readonly" cwd="${workspace}" pipefail="true">
      <capabilities sandbox="required" network="false" child_processes="false">
        <read path="${workspace}"/>
        <write path="${workspace}/_build"/>
      </capabilities>
      <resolver>
        <search_path path="/usr/bin"/>
        <trusted_root path="/usr/bin"/>
        <executable id="rg" path="/usr/bin/rg" trusted="true"/>
      </resolver>
      <environment inherit="safe">
        <set name="LANG" value="C.UTF-8"/>
        <pass name="OPAM_SWITCH_PREFIX" required="true"/>
      </environment>
      <limits wall_time="30s" idle_time="none" total_output="512KiB"/>
      <backends><seatbelt when="macos"/><bubblewrap when="linux"/></backends>
      <policy default="ask">
        <rule id="allow-rg" action="allow">
          <all><basename value="rg"/><trusted_executable/><no_unknown_effects/></all>
        </rule>
      </policy>
      <approvals provider="ui" unavailable="deny" scopes="once,exact_session"/>
      <secrets replacement="[REDACTED]"><from_env name="TOKEN" optional="true"/></secrets>
      <audit format="jsonl" path="${session_dir}/shell.jsonl" failure="deny_start"/>
    </shell_access>
    <tool name="search" type="shell" mode="fixed" runtime="readonly">
      <command program="rg"><arg value="--json"/></command>
      <arguments mode="optional" max_count="20"/>
    </tool>
    <tool name="shell" type="shell" mode="structured" runtime="readonly" result="structured"/>|}
;;

let%expect_test "Prompt lowers runtime and shell tools from the ChatMD tree" =
  Eio_main.run (fun env ->
    let dir = Eio.Stdenv.cwd env in
    let elements = CM.parse_chat_inputs ~source:"agent.chatmd" ~dir explicit_runtime in
    List.iter elements ~f:(function
      | CM.Shell_runtime runtime ->
        printf "runtime %s\n" (Spec.Shell_spec.Runtime_id.to_string runtime.id);
        printf
          "rules %d\n"
          (Option.value_map runtime.policy ~default:0 ~f:(fun value ->
             List.length value.rules));
        printf
          "backends %d\n"
          (Option.value_map runtime.backends ~default:0 ~f:(fun value ->
             List.length value.values))
      | CM.Tool (CM.Shell tool) ->
        printf
          "tool %s %s\n"
          tool.name
          (Sexp.to_string (Spec.Shell_tool_spec.sexp_of_mode tool.mode))
      | _ -> ()));
  [%expect
    {|
    runtime readonly
    rules 1
    backends 2
    tool search (Fixed(command(Argv(program(rg))(executable_ref())(arguments((Literal --json)))))(model_arguments((mode Optional_arguments)(min_count())(max_count(20))(max_item_bytes()))))
    tool shell Structured |}]
;;

let parse_error source =
  Eio_main.run (fun env ->
    try
      ignore
        (CM.parse_chat_inputs ~dir:(Eio.Stdenv.cwd env) source
         : CM.top_level_elements list);
      print_endline "accepted"
    with
    | Failure message -> print_endline message)
;;

let%expect_test "unknown and duplicate nested declarations fail closed" =
  parse_error {|<shell_access id="x"><wat/></shell_access>|};
  parse_error {|<shell_access id="x"><limits/><limits/></shell_access>|};
  parse_error {|<shell_access id="x" id="y"/>|};
  parse_error
    {|<tool name="x" type="shell" mode="structured" runtime="r" command="pwd"/>|};
  [%expect
    {|
    error[shell.unexpected_text] at shell_access: unexpected text
    error[shell.duplicate_section] at shell_access.limits: duplicate section: limits
    error[shell.duplicate_attribute] at shell_access: duplicate attribute: id
    error[shell.tool_mode_attribute]: attribute command is not valid for structured mode |}]
;;

let round_trip_source =
  {|<shell_access id="roundtrip" extends="base" cwd="${workspace}" pipefail="false">
      <capabilities sandbox="direct_unsafe" network="true" child_processes="true"
          arbitrary_code="false" privilege_change="false" merge="replace">
        <read path_env="HOME" optional="true"/>
        <write path="/tmp/output"/>
      </capabilities>
      <resolver allow_relative_search_path="true" merge="replace">
        <search_path path="${workspace}/bin"/>
        <trusted_root path="/usr/local/bin"/>
        <executable id="runner" path="${tool_dir}/runner" sha256="abc"
            trusted="true" override="true"/>
      </resolver>
      <environment inherit="raw" merge="replace">
        <set name="LANG" value="C"/>
        <pass name="TOKEN" required="true" secret="true"/>
        <unset name="DEBUG"/>
        <unset_prefix value="AWS_"/>
        <path prepend="${workspace}/bin"/>
        <path append="/opt/bin"/>
        <path_env name="EXTRA_BIN" suffix="bin" position="prepend" required="true"/>
      </environment>
      <limits wall_time="1500ms" idle_time="none" max_stdin="0B"
          stdout="1KiB" stderr="2KiB" total_output="3KiB" cpu_time="1m"
          memory="4MiB" file_size="5MiB" open_files="12"/>
      <backends accept_declared_confinement="true" merge="replace">
        <seatbelt id="mac" when="macos" executable="/usr/bin/sandbox-exec"
            allow_system_reads="false"/>
        <bubblewrap id="linux" when="linux" executable="/usr/bin/bwrap"
            private_tmp="false" proc="false" dev="full"/>
        <direct id="windows" when="windows"/>
      </backends>
      <policy default="deny" merge="replace">
        <rule id="all" action="allow" override="true">
          <all>
            <program value="git"/>
            <basename value="git"/>
            <resolved_path value="/usr/bin/git"/>
            <trusted_executable/>
            <program_regex value="^git$"/>
            <argv_prefix values="git,status"/>
            <argument value="--short"/>
            <argument_contains value="token"/>
            <effect name="write_path" under="${workspace}"/>
            <no_unknown_effects/>
            <not><any_command/></not>
            <any><effect name="network"/><effect name="unknown"/></any>
          </all>
        </rule>
      </policy>
      <approvals provider="none" unavailable="error"
          scopes="once,exact_session,prefix_session,durable_exact" durable="true"/>
      <secrets replacement="[hidden]" merge="replace">
        <from_env name="TOKEN" optional="true"/>
        <from_file path="${source_dir}/secret" optional="true" strip="false"/>
        <literal value="literal-secret"/>
      </secrets>
      <audit format="jsonl" path="${session_dir}/audit.jsonl"
          content="full" failure="terminate"/>
    </shell_access>
    <tool name="fixed-argv" type="shell" mode="fixed" runtime="roundtrip"
        description="fixed &amp; safe" stdin="required" rationale="required"
        result="stdout" stream="sanitized" nonzero="error">
      <command executable_ref="runner">
        <arg value="--flag"/>
        <secret_arg env="TOKEN" prefix="--token="/>
        <path_arg base="workspace" path="src"/>
        <path_arg path="/tmp/output"/>
      </command>
      <arguments mode="required" min_count="1" max_count="4" max_item_bytes="128"/>
    </tool>
    <tool name="fixed-compact" type="shell" mode="fixed" runtime="roundtrip"
        command="pwd"/>
    <tool name="structured" type="shell" mode="structured" runtime="roundtrip"/>
    <tool name="chain" type="shell" mode="chain" runtime="roundtrip"/>
    <tool name="raw" type="shell" mode="raw" runtime="roundtrip"
        executable="/bin/zsh"/>
    <tool name="script" type="shell" mode="script" runtime="roundtrip"
        script="${tool_dir}/run.sh" interpreter="/bin/sh" executable="false"/>|}
;;

let shell_declarations elements =
  List.partition_map elements ~f:(function
    | CM.Shell_runtime runtime -> First runtime
    | CM.Tool (CM.Shell tool) -> Second tool
    | _ -> failwith "expected only shell declarations")
;;

let equal_runtime_ignoring_source (left : Spec.Shell_spec.t) (right : Spec.Shell_spec.t) =
  Spec.Shell_spec.equal { left with source = right.source } right
;;

let equal_tool_ignoring_source
      (left : Spec.Shell_tool_spec.t)
      (right : Spec.Shell_tool_spec.t)
  =
  Spec.Shell_tool_spec.equal { left with source = right.source } right
;;

let%expect_test "multiline shell tags use XML whitespace after element names" =
  Eio_main.run
  @@ fun env ->
  let elements =
    CM.parse_chat_inputs
      ~source:"multiline.chatmd"
      ~dir:(Eio.Stdenv.cwd env)
      {|<shell_access id="local-tools" extends="builtin:yolo@1" cwd="${workspace}"/>
        <tool
          name="ocaml-type-search"
          type="shell"
          mode="fixed"
          runtime="local-tools"
          description="Search OCaml documentation.">
          <command program="sherlodoc">
            <arg value="search"/>
          </command>
          <arguments
            mode="required"
            min_count="1"
            max_count="1"/>
        </tool>|}
  in
  List.iter elements ~f:(function
    | CM.Shell_runtime runtime ->
      printf "runtime=%s\n" (Spec.Shell_spec.Runtime_id.to_string runtime.id)
    | CM.Tool (CM.Shell tool) ->
      (match tool.mode with
       | Fixed { model_arguments; _ } ->
         printf
           "tool=%s description=%s min=%d max=%d\n"
           tool.name
           (Option.value_exn tool.description)
           (Option.value_exn model_arguments.min_count)
           (Option.value_exn model_arguments.max_count)
       | Structured | Chain | Raw _ | Script_file _ -> failwith "expected fixed tool")
    | _ -> failwith "expected shell declarations");
  [%expect
    {|
    runtime=local-tools
    tool=ocaml-type-search description=Search OCaml documentation. min=1 max=1
    |}]
;;

let%expect_test "shell declarations serialize without semantic loss" =
  Eio_main.run
  @@ fun env ->
  let dir = Eio.Stdenv.cwd env in
  let original = CM.parse_chat_inputs ~source:"roundtrip.chatmd" ~dir round_trip_source in
  let runtimes, tools = shell_declarations original in
  let serialized =
    List.map runtimes ~f:Chatmd_shell_serialization.runtime
    @ List.map tools ~f:Chatmd_shell_serialization.tool
    |> String.concat ~sep:"\n"
  in
  let reparsed = CM.parse_chat_inputs ~source:"serialized.chatmd" ~dir serialized in
  let reparsed_runtimes, reparsed_tools = shell_declarations reparsed in
  printf
    "runtimes=%d equal=%b\n"
    (List.length runtimes)
    (List.equal equal_runtime_ignoring_source runtimes reparsed_runtimes);
  printf
    "tools=%d equal=%b\n"
    (List.length tools)
    (List.equal equal_tool_ignoring_source tools reparsed_tools);
  [%expect
    {|
    runtimes=1 equal=true
    tools=6 equal=true
    |}]
;;

let%expect_test "imports retain namespace and source provenance" =
  Eio_main.run
  @@ fun env ->
  let root = Eio.Path.(Eio.Stdenv.cwd env / "_build" / "chatmd-shell-import-test") in
  let definitions = Eio.Path.(root / "definitions") in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 definitions;
  let imported_source =
    {|<shell_access id="readonly" cwd="${source_dir}"/>
      <tool name="imported-search" type="shell" mode="fixed"
          runtime="readonly" command="pwd"/>
      <user>imported message</user>|}
  in
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    Eio.Path.(definitions / "runtime.chatmd")
    imported_source;
  let source = {|<import src="definitions/runtime.chatmd" namespace="common"/>|} in
  let elements = CM.parse_chat_inputs ~source:"agent.chatmd" ~dir:root source in
  List.iter elements ~f:(function
    | CM.Shell_runtime runtime ->
      printf "runtime=%s\n" (Spec.Shell_spec.Runtime_id.to_string runtime.id);
      printf "file=%s\n" runtime.source.file;
      printf "namespace=%s\n" (Option.value_exn runtime.source.namespace);
      printf
        "source-dir=%b digest=%b\n"
        (String.equal runtime.source.source_dir (Eio.Path.native_exn definitions))
        (String.equal
           runtime.source.source_sha256
           (Spec.Source_ref.digest imported_source))
    | CM.Tool (CM.Shell tool) ->
      printf "tool-runtime=%s file=%s\n" tool.runtime tool.source.file
    | CM.User message ->
      printf "message-source=%s\n" (Option.value_exn message.source_context)
    | _ -> ());
  [%expect
    {|
    runtime=common:readonly
    file=definitions/runtime.chatmd
    namespace=common
    source-dir=true digest=true
    tool-runtime=common:readonly file=definitions/runtime.chatmd
    message-source=definitions/runtime.chatmd
    |}]
;;

let%expect_test "import cycles and duplicate namespace aliases fail explicitly" =
  Eio_main.run
  @@ fun env ->
  let root = Eio.Path.(Eio.Stdenv.cwd env / "_build" / "chatmd-shell-cycle-test") in
  let definitions = Eio.Path.(root / "definitions") in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 definitions;
  let root_source = {|<import src="definitions/child.chatmd" namespace="child"/>|} in
  Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(root / "root.chatmd") root_source;
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    Eio.Path.(definitions / "child.chatmd")
    {|<import src="../root.chatmd" namespace="root"/>|};
  let parse_error source =
    try
      ignore
        (CM.parse_chat_inputs ~source:"root.chatmd" ~dir:root source
         : CM.top_level_elements list);
      print_endline "accepted"
    with
    | Failure message -> print_endline message
  in
  parse_error root_source;
  parse_error
    {|<import src="definitions/child.chatmd" namespace="shared"/>
      <import src="definitions/child.chatmd" namespace="shared"/>|};
  [%expect
    {|
    ChatMD import cycle detected at "../root.chatmd".
    Duplicate ChatMD import namespace "shared".
    |}]
;;

let%expect_test "legacy custom tools remain compatibility declarations" =
  Eio_main.run
  @@ fun env ->
  let elements =
    CM.parse_chat_inputs
      ~dir:(Eio.Stdenv.cwd env)
      {|<tool name="legacy" description="compat" command="printf ok"/>|}
  in
  List.iter elements ~f:(function
    | CM.Tool (CM.Custom tool) ->
      printf "%s %s %s\n" tool.name (Option.value_exn tool.description) tool.command
    | _ -> failwith "expected a legacy custom tool");
  [%expect {| legacy compat printf ok |}]
;;

let%expect_test "read_file declarations parse named roots and preserve defaults" =
  Eio_main.run
  @@ fun env ->
  let dir = Eio.Stdenv.cwd env in
  let print = function
    | CM.Tool (CM.Read_file specification) ->
      printf "description=%s\n" (Option.value specification.description ~default:"none");
      List.iter specification.roots ~f:(fun root ->
        printf
          "%s=%s (%s)\n"
          root.id
          (Chatmd_shell_spec.Path_expr.to_string root.path)
          (Option.value root.description ~default:"none"))
    | _ -> failwith "expected read_file declaration"
  in
  CM.parse_chat_inputs
    ~source:"agent.chatmd"
    ~dir
    {|<tool name="read_file" description="Read approved files">
        <read id="source" path="lib" description="OCaml source"/>
        <read id="computer" path="/" description="Whole computer"/>
      </tool>|}
  |> List.iter ~f:print;
  CM.parse_chat_inputs ~source:"agent.chatmd" ~dir {|<tool name="read_file"/>|}
  |> List.iter ~f:print;
  [%expect
    {|
    description=Read approved files
    source=${tool_dir}/lib (OCaml source)
    computer=/ (Whole computer)
    description=none
    cwd=${tool_dir} (ochat launch directory)
    |}]
;;

let%expect_test "read_file declarations reject invalid nested roots" =
  Eio_main.run
  @@ fun env ->
  let dir = Eio.Stdenv.cwd env in
  let parse source =
    try
      ignore (CM.parse_chat_inputs ~source:"agent.chatmd" ~dir source);
      print_endline "accepted"
    with
    | Failure message -> print_endline message
  in
  parse
    {|<tool name="read_file">
        <read id="same" path="lib"/>
        <read id="same" path="docs-src"/>
      </tool>|};
  parse {|<tool name="read_file"><read id=" " path="lib"/></tool>|};
  parse {|<tool name="read_file"><command program="cat"/></tool>|};
  [%expect
    {|
    error[read_file.duplicate_root] at tool.read: duplicate root id: same
    error[read_file.invalid_root_id] at tool.read.0.id: root id is empty
    error[read_file.unexpected_child] at tool: only <read> roots are allowed
    |}]
;;

let%expect_test "moderator runtime uses strict ChatMD parsing" =
  Eio_main.run
  @@ fun env ->
  let dir = Eio.Stdenv.cwd env in
  let valid =
    CM.parse_chat_inputs
      ~source:"moderator.chatmd"
      ~dir
      {|<moderator_runtime shell_runtime="moderator-processes"/>|}
  in
  List.iter valid ~f:(function
    | CM.Moderator_runtime moderator -> printf "%s\n" moderator.runtime
    | _ -> failwith "expected moderator runtime declaration");
  (try
     ignore
       (CM.parse_chat_inputs
          ~source:"moderator.chatmd"
          ~dir
          {|<moderator_runtime shell_runtime="commands" unknown="value"/>|}
        : CM.top_level_elements list)
   with
   | Failure message -> print_endline message);
  [%expect
    {|
    moderator-processes
    error[shell.unknown_attribute] at moderator_runtime: unknown attribute: unknown
    |}]
;;
