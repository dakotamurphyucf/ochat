open! Core
module CM = Prompt.Chat_markdown
module Lang = Chatml.Chatml_lang
module MC = Chatmd_shell_spec.Manifest_compiler
module S = Chatmd_shell_spec.Shell_spec

let source =
  {|<shell_access id="direct" cwd="${workspace}">
      <capabilities sandbox="direct_unsafe" network="false"
          child_processes="false" arbitrary_code="false" privilege_change="false">
        <read path="${workspace}"/>
      </capabilities>
      <backends merge="replace"><direct when="macos"/></backends>
      <policy default="allow"/>
      <approvals provider="none" unavailable="deny" scopes="once"/>
      <audit format="none"/>
    </shell_access>
    <tool name="fixed_echo" type="shell" mode="fixed" runtime="direct"
        command="/bin/echo" result="stdout"/>
    <tool name="structured_cat" type="shell" mode="structured" runtime="direct"
        stdin="required" result="structured"/>|}
;;

let declarations root =
  CM.parse_chat_inputs ~source:"agent.chatmd" ~dir:root source
  |> List.fold ~init:([], []) ~f:(fun (runtimes, tools) -> function
    | CM.Shell_runtime runtime -> runtime :: runtimes, tools
    | Tool (Shell tool) -> runtimes, tool :: tools
    | _ -> runtimes, tools)
;;

let registry env sw root =
  let runtimes, tools = declarations root in
  let manifest, material =
    MC.compile_with_material
      { runtimes
      ; tools
      ; scripts = []
      ; legacy_tools = []
      ; moderator_runtime = None
      ; platform = S.Macos
      ; supported_features = Chatmd_shell_spec.Feature.phase2
      }
    |> function
    | Ok value -> value
    | Error diagnostics ->
      List.map diagnostics ~f:Chatmd_shell_spec.Diagnostic.to_string
      |> String.concat ~sep:"\n"
      |> failwith
  in
  let grant =
    Shell_runtime.Manifest_authorizer.authorize
      Shell_runtime.Manifest_authorizer.assume_authorized
      manifest
    |> function
    | Ok grant -> grant
    | Error error -> failwith error.Shell_runtime.Manifest_authorizer.message
  in
  let host : Shell_runtime.Host.t =
    { env
    ; workspace = root
    ; tool_dir = root
    ; prompt_dir = root
    ; session_dir = root
    ; cache_dir = root
    ; home = root
    ; source_dirs = String.Map.singleton "agent.chatmd" root
    ; process_environment = [| "PATH=/usr/bin:/bin" |]
    ; session_id = "shell-tool-test"
    ; resource_runner = None
    }
  in
  Shell_runtime.Registry.instantiate
    ~sw
    ~host
    ~manifest
    ~grant
    ~material
    ~approval_provider:Shell_runtime.Approval_broker.None_available
  |> function
  | Ok registry -> registry
  | Error errors ->
    List.map errors ~f:(fun error -> error.Shell_runtime.Registry.message)
    |> String.concat ~sep:"\n"
    |> failwith
;;

let output_text = function
  | Openai.Responses.Tool_output.Output.Text text -> text
  | _ -> failwith "expected text tool output"
;;

let error_fields output =
  let error = Jsonaf.of_string output |> Jsonaf.member_exn "error" in
  ( Jsonaf.member_exn "code" error |> Jsonaf.string_exn
  , Jsonaf.member_exn "message" error |> Jsonaf.string_exn )
;;

let shell_function registry name =
  let specification = Shell_runtime.Registry.tool registry name |> Option.value_exn in
  match Chat_response.Shell_tool.create registry specification with
  | Ok function_ -> function_
  | Error error -> failwith error.Chat_response.Shell_tool.message
;;

let structured_output function_ input =
  function_.Ochat_function.run input |> output_text |> Jsonaf.of_string
;;

let stdout result = Jsonaf.member_exn "stdout" result |> Jsonaf.string_exn
let stderr result = Jsonaf.member_exn "stderr" result |> Jsonaf.string_exn

let command_argvs result =
  match Jsonaf.member_exn "commands" result with
  | `Array commands ->
    List.map commands ~f:(fun command ->
      match Jsonaf.member_exn "argv" command with
      | `Array argv -> List.map argv ~f:Jsonaf.string_exn
      | _ -> failwith "expected command argv array")
  | _ -> failwith "expected commands array"
;;

let phase2_source =
  {|<shell_access id="yolo" extends="builtin:yolo@1"/>
    <tool name="chain_exec" type="shell" mode="chain" runtime="yolo"
      result="structured"/>
    <tool name="raw_exec" type="shell" mode="raw" runtime="yolo"
      executable="/bin/sh" result="structured"/>
    <tool name="script_exec" type="shell" mode="script" runtime="yolo"
      script="phase2-tool.sh" interpreter="/bin/sh" result="structured"/>|}
;;

let agent_runtime_or_fail = function
  | Ok value -> value
  | Error diagnostics ->
    List.map diagnostics ~f:Chat_response.Agent_runtime.diagnostic_to_string
    |> String.concat ~sep:"\n"
    |> failwith
;;

let agent_runtime env sw root source manifest_authorizer approval_provider =
  let elements = CM.parse_chat_inputs ~source:"agent.chatmd" ~dir:root source in
  let cache = Chat_response.Cache.create ~max_size:1 () in
  let ctx = Chat_response.Ctx.create ~env ~dir:root ~tool_dir:root ~cache in
  let host =
    Chat_response.Agent_runtime.host
      ~env
      ~workspace:root
      ~tool_dir:root
      ~prompt_dir:root
      ~session_dir:root
      ~cache_dir:root
      ~home:root
      ~session_id:"agent-runtime-test"
      ~resource_runner:None
      ~prompt_elements:elements
    |> agent_runtime_or_fail
  in
  Chat_response.Agent_runtime.create
    ~sw
    ~ctx
    ~host
    ~platform:S.Macos
    ~prompt_elements:elements
    ~manifest_authorizer
    ~approval_provider
    ~approval_store:(Shell_access.Approval.create_store ())
    ~run_agent:(fun ?prompt_dir:_ ?session_id:_ ?observer:_ ~source:_ ~ctx:_ _ _ ->
      failwith "unexpected nested agent")
    ()
;;

let runtime_function runtime name =
  List.find_exn runtime.Chat_response.Agent_runtime.functions ~f:(fun function_ ->
    String.equal function_.Ochat_function.info.function_.name name)
;;

let request_description function_ =
  match Chat_response.Tool.convert_tools [ function_.Ochat_function.info ] with
  | [ Openai.Responses.Request.Tool.Function { description = Some description; _ } ] ->
    description
  | [ Function { description = None; _ } ] ->
    failwith "expected a model-visible description"
  | [ Custom_function _ ] -> failwith "expected a function tool"
  | _ -> failwith "expected exactly one tool"
;;

let description_source =
  {|<shell_access id="yolo" extends="builtin:yolo@1"/>
    <tool name="described_fixed" type="shell" mode="fixed" runtime="yolo"
      command="/bin/echo" description="Print a caller-provided value."/>
    <tool name="described_structured" type="shell" mode="structured" runtime="yolo"/>
    <tool name="described_chain" type="shell" mode="chain" runtime="yolo"/>
    <tool name="described_raw" type="shell" mode="raw" runtime="yolo"
      executable="/bin/sh"/>
    <tool name="described_script" type="shell" mode="script" runtime="yolo"
      script="description-tool.sh" interpreter="/bin/sh"/>
    <tool name="described_legacy" command="/bin/printf"
      description="Format a caller-provided value."/>|}
;;

let%expect_test "shell modes publish default descriptions and append ChatMD guidance" =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let root = Eio.Path.(Eio.Stdenv.cwd env / "_build" / "shell-tool-description") in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 root;
  Eio.Path.save
    ~create:(`Or_truncate 0o700)
    Eio.Path.(root / "description-tool.sh")
    "printf description\n";
  let runtime =
    agent_runtime
      env
      sw
      root
      description_source
      Shell_runtime.Manifest_authorizer.assume_authorized
      Shell_runtime.Approval_broker.Assume_approved
    |> agent_runtime_or_fail
  in
  let check name mode_text custom_text =
    let description = runtime_function runtime name |> request_description in
    printf
      "%s mode=%b security=%b custom=%b\n"
      name
      (String.is_substring description ~substring:mode_text)
      (String.is_substring description ~substring:"command policy")
      (Option.value_map
         custom_text
         ~default:
           (not (String.is_substring description ~substring:"Additional tool guidance:"))
         ~f:(fun custom ->
           String.is_substring
             description
             ~substring:("Additional tool guidance: " ^ custom)))
  in
  check "described_fixed" "fixed shell command" (Some "Print a caller-provided value.");
  check "described_structured" "one executable directly" None;
  check "described_chain" "parsed shell command chain" None;
  check "described_raw" "raw shell source" None;
  check "described_script" "source and executable identities" None;
  check "described_legacy" "fixed shell command" (Some "Format a caller-provided value.");
  [%expect
    {|
    described_fixed mode=true security=true custom=true
    described_structured mode=true security=true custom=true
    described_chain mode=true security=true custom=true
    described_raw mode=true security=true custom=true
    described_script mode=true security=true custom=true
    described_legacy mode=true security=true custom=true
    |}]
;;

let%expect_test "fixed arguments remain literal argv" =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let root = Eio.Path.(Eio.Stdenv.cwd env / "_build" / "shell-tool-test") in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 root;
  let function_ = shell_function (registry env sw root) "fixed_echo" in
  function_.run {|{"arguments":["safe; /bin/false","$(printf injected)"]}|}
  |> output_text
  |> printf "%s";
  [%expect
    {| safe; /bin/false $(printf injected)
    |}]
;;

let%expect_test "shell validation errors return stable code and message output" =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let root = Eio.Path.(Eio.Stdenv.cwd env / "_build" / "shell-tool-test") in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 root;
  let function_ = shell_function (registry env sw root) "fixed_echo" in
  let code, message =
    function_.run {|{"arguments":[],"script":"forbidden"}|} |> output_text |> error_fields
  in
  printf "%s: %s\n" code message;
  [%expect {| shell.tool_field_forbidden: script is not valid for this mode |}]
;;

let%expect_test "shell executor errors return stable code and message output" =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let root = Eio.Path.(Eio.Stdenv.cwd env / "_build" / "shell-tool-test") in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 root;
  let function_ = shell_function (registry env sw root) "structured_cat" in
  let code, message =
    function_.run
      {|{"program":"definitely-missing-shell-tool-executable","arguments":[],"stdin":""}|}
    |> output_text
    |> error_fields
  in
  printf
    "code=%s message=%b\n"
    code
    (String.is_substring message ~substring:"definitely-missing-shell-tool-executable");
  [%expect {| code=resolution_error message=true |}]
;;

let%expect_test "structured mode passes stdin without a shell hop" =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let root = Eio.Path.(Eio.Stdenv.cwd env / "_build" / "shell-tool-test") in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 root;
  let function_ = shell_function (registry env sw root) "structured_cat" in
  let output =
    function_.run {|{"program":"/bin/cat","arguments":[],"stdin":"hello stdin"}|}
    |> output_text
  in
  printf
    "stdout=%b backend=%b command=%b\n"
    (String.is_substring output ~substring:"hello stdin")
    (String.is_substring output ~substring:"direct-unsafe")
    (String.is_substring output ~substring:"/bin/cat");
  [%expect {| stdout=true backend=true command=true |}]
;;

let%expect_test "phase2 shell modes execute through ChatMD and return structured results" =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let root = Eio.Path.(Eio.Stdenv.cwd env / "_build" / "shell-tool-phase2") in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 root;
  Eio.Path.save
    ~create:(`Or_truncate 0o700)
    Eio.Path.(root / "phase2-tool.sh")
    "printf 'script:%s' \"$1\"\n";
  let declarations_root = root in
  let runtimes, tools =
    CM.parse_chat_inputs ~source:"agent.chatmd" ~dir:declarations_root phase2_source
    |> List.fold ~init:([], []) ~f:(fun (runtimes, tools) -> function
      | CM.Shell_runtime runtime -> runtime :: runtimes, tools
      | Tool (Shell tool) -> runtimes, tool :: tools
      | _ -> runtimes, tools)
  in
  let manifest, material =
    MC.compile_with_material
      { runtimes
      ; tools
      ; scripts = []
      ; legacy_tools = []
      ; moderator_runtime = None
      ; platform = S.Macos
      ; supported_features = Chatmd_shell_spec.Feature.phase2
      }
    |> function
    | Ok value -> value
    | Error diagnostics ->
      List.map diagnostics ~f:Chatmd_shell_spec.Diagnostic.to_string
      |> String.concat ~sep:"\n"
      |> failwith
  in
  let grant =
    Shell_runtime.Manifest_authorizer.authorize
      Shell_runtime.Manifest_authorizer.assume_authorized
      manifest
    |> function
    | Ok grant -> grant
    | Error error -> failwith error.Shell_runtime.Manifest_authorizer.message
  in
  let host : Shell_runtime.Host.t =
    { env
    ; workspace = root
    ; tool_dir = root
    ; prompt_dir = root
    ; session_dir = root
    ; cache_dir = root
    ; home = root
    ; source_dirs = String.Map.singleton "agent.chatmd" root
    ; process_environment = [| "PATH=/usr/bin:/bin" |]
    ; session_id = "shell-tool-phase2"
    ; resource_runner = None
    }
  in
  let registry =
    Shell_runtime.Registry.instantiate
      ~sw
      ~host
      ~manifest
      ~grant
      ~material
      ~approval_provider:Shell_runtime.Approval_broker.None_available
    |> function
    | Ok registry -> registry
    | Error errors ->
      List.map errors ~f:(fun error -> error.Shell_runtime.Registry.message)
      |> String.concat ~sep:"\n"
      |> failwith
  in
  let chain =
    structured_output
      (shell_function registry "chain_exec")
      {|{"command":"printf abc | wc -c"}|}
  in
  let raw =
    structured_output (shell_function registry "raw_exec") {|{"script":"printf raw"}|}
  in
  let script =
    structured_output (shell_function registry "script_exec") {|{"arguments":["value"]}|}
  in
  let script_inspection =
    List.hd_exn (Shell_runtime.Registry.inspection registry).scripts
  in
  printf
    "chain=%b commands=%d no-shell-hop=%b raw=%s raw-shell=%s script=%S script-error=%S \
     script-path=%S inspected=%b\n"
    (String.equal (String.strip (stdout chain)) "3")
    (List.length (command_argvs chain))
    (List.for_all (command_argvs chain) ~f:(fun argv ->
       not (String.equal (List.hd_exn argv) "/bin/sh")))
    (stdout raw)
    (List.hd_exn (List.hd_exn (command_argvs raw)))
    (stdout script)
    (stderr script)
    script_inspection.path
    (not (String.is_empty script_inspection.executable_sha256));
  [%expect
    {| chain=true commands=2 no-shell-hop=true raw=raw raw-shell=/bin/sh script="script:value" script-error="" script-path="phase2-tool.sh" inspected=true |}]
;;

let%expect_test "phase2 shell output is completion-only" =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let root = Eio.Path.(Eio.Stdenv.cwd env / "_build" / "shell-tool-phase2-progress") in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 root;
  Eio.Path.save
    ~create:(`Or_truncate 0o700)
    Eio.Path.(root / "phase2-tool.sh")
    "printf done\n";
  let runtime =
    agent_runtime
      env
      sw
      root
      phase2_source
      Shell_runtime.Manifest_authorizer.assume_authorized
      Shell_runtime.Approval_broker.None_available
    |> agent_runtime_or_fail
  in
  let updates = ref 0 in
  let invocation = Ochat_function.Invocation.create (fun _ -> Int.incr updates) in
  let function_ = runtime_function runtime "raw_exec" in
  let output =
    function_.run_with_progress ~invocation {|{"script":"printf complete"}|}
    |> output_text
    |> Jsonaf.of_string
  in
  printf "stdout=%s progress=%d\n" (stdout output) !updates;
  [%expect {| stdout=complete progress=0 |}]
;;

let%expect_test "agent runtime desugars legacy custom tools through the registry" =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let root = Eio.Path.(Eio.Stdenv.cwd env / "_build" / "agent-runtime-test") in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 root;
  let runtime =
    agent_runtime
      env
      sw
      root
      {|<tool name="legacy_echo" command="/bin/echo"/>|}
      Shell_runtime.Manifest_authorizer.assume_authorized
      Shell_runtime.Approval_broker.Assume_approved
    |> agent_runtime_or_fail
  in
  let function_ : Ochat_function.t = runtime_function runtime "legacy_echo" in
  function_.run {|{"arguments":["literal; value"]}|} |> output_text |> printf "%s";
  [%expect
    {| literal; value
    |}]
;;

let%expect_test "agent runtime rejects an unauthorized shell manifest" =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let root = Eio.Path.(Eio.Stdenv.cwd env / "_build" / "agent-runtime-test") in
  match
    agent_runtime
      env
      sw
      root
      source
      Shell_runtime.Manifest_authorizer.deny
      Shell_runtime.Approval_broker.None_available
  with
  | Ok _ -> failwith "expected manifest authorization to fail"
  | Error diagnostics ->
    List.iter diagnostics ~f:(fun diagnostic -> printf "%s\n" diagnostic.code);
    [%expect {| shell.manifest_rejected |}]
;;

let%expect_test "agent runtime binds the declared moderator shell runtime" =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let root = Eio.Path.(Eio.Stdenv.cwd env / "_build" / "agent-runtime-test") in
  let runtime =
    agent_runtime
      env
      sw
      root
      (source ^ {|<moderator_runtime shell_runtime="direct"/>|})
      Shell_runtime.Manifest_authorizer.assume_authorized
      Shell_runtime.Approval_broker.Assume_approved
    |> agent_runtime_or_fail
  in
  printf
    "runtime=%s handler=%b\n"
    (Option.value_exn runtime.moderator_shell_runtime)
    (Option.is_some (Chat_response.Agent_runtime.moderator_process_handler runtime));
  [%expect {| runtime=direct handler=true |}]
;;

let moderator_session =
  let source =
    {|
      type state = { count : int }
      type event = [ `Ping ]
      let initial_state = { count = 0 }
      let on_event : context -> state -> event -> state task =
        fun ctx state event -> Task.pure(state)
    |}
  in
  let compiled =
    Chatml_moderator_runtime.compile_script ~source () |> Result.ok_or_failwith
  in
  Chatml_moderator_runtime.instantiate_session
    (Chatml_moderator_runtime.default_runtime_config ())
    compiled
    ~entrypoints:{ initial_state_name = "initial_state"; on_event_name = "on_event" }
  |> Result.ok_or_failwith
;;

let%expect_test "moderator Process.run executes through the selected registry" =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let root = Eio.Path.(Eio.Stdenv.cwd env / "_build" / "agent-runtime-test") in
  let runtime =
    agent_runtime
      env
      sw
      root
      (source ^ {|<moderator_runtime shell_runtime="direct"/>|})
      Shell_runtime.Manifest_authorizer.assume_authorized
      Shell_runtime.Approval_broker.Assume_approved
    |> agent_runtime_or_fail
  in
  let handler =
    Chat_response.Agent_runtime.moderator_process_handler runtime |> Option.value_exn
  in
  handler
    moderator_session
    ~command:"/bin/echo"
    ~args:(Lang.VArray [| Lang.VString "literal; value" |])
  |> Result.ok_or_failwith
  |> printf "%s";
  [%expect
    {| literal; value
    |}]
;;

let%expect_test "moderator Process.run obeys approval denial" =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let root = Eio.Path.(Eio.Stdenv.cwd env / "_build" / "agent-runtime-test") in
  let moderated_source =
    source
    |> String.substr_replace_all
         ~pattern:"<policy default=\"allow\"/>"
         ~with_:"<policy default=\"ask\"/>"
    |> String.substr_replace_all
         ~pattern:"<approvals provider=\"none\""
         ~with_:"<approvals provider=\"ui\""
  in
  let runtime =
    agent_runtime
      env
      sw
      root
      (moderated_source ^ {|<moderator_runtime shell_runtime="direct"/>|})
      Shell_runtime.Manifest_authorizer.assume_authorized
      Shell_runtime.Approval_broker.Auto_deny
    |> agent_runtime_or_fail
  in
  let handler =
    Chat_response.Agent_runtime.moderator_process_handler runtime |> Option.value_exn
  in
  (match handler moderator_session ~command:"/bin/echo" ~args:(Lang.VArray [||]) with
   | Ok _ -> failwith "expected approval denial"
   | Error message ->
     printf
       "approval-denied=%b\n"
       (String.is_substring message ~substring:"denied automatically"));
  [%expect {| approval-denied=true |}]
;;

let%expect_test "moderator Process.run rejects invalid arguments and missing runtimes" =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let root = Eio.Path.(Eio.Stdenv.cwd env / "_build" / "shell-tool-test") in
  let registry = registry env sw root in
  let handler =
    Shell_runtime.Moderator_process_adapter.handler ~registry ~runtime_id:"missing"
  in
  let invalid =
    Shell_runtime.Moderator_process_adapter.handler
      ~registry
      ~runtime_id:"direct"
      moderator_session
      ~command:"/bin/echo"
      ~args:(Lang.VString "not-an-array")
  in
  let missing = handler moderator_session ~command:"/bin/echo" ~args:(Lang.VArray [||]) in
  printf "invalid=%b missing=%b\n" (Result.is_error invalid) (Result.is_error missing);
  [%expect {| invalid=true missing=true |}]
;;
