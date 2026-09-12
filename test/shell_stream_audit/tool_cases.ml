open! Core
module F = Fixture
module CM = Prompt.Chat_markdown
module MC = Chatmd_shell_spec.Manifest_compiler

let source extra =
  {|<shell_access id="worker" extends="builtin:yolo@1"><audit format="none"/></shell_access>
    <shell_access id="main" extends="builtin:yolo@1">
      <audit format="none"/>|}
  ^ extra
  ^ {|</shell_access>
    <tool name="live" type="shell" runtime="main" mode="structured"
      stream="sanitized" result="stdout"/>
    <tool name="normal" type="shell" runtime="main" mode="structured"
      stream="finalized" result="stdout"/>|}
;;

let material root source =
  let runtimes, tools =
    CM.parse_chat_inputs ~source:"stream.chatmd" ~dir:root source
    |> List.fold ~init:([], []) ~f:(fun (runtimes, tools) -> function
      | CM.Shell_runtime runtime -> runtime :: runtimes, tools
      | Tool (Shell tool) -> runtimes, tool :: tools
      | _ -> runtimes, tools)
  in
  match
    MC.compile_with_material
      { runtimes
      ; tools
      ; scripts = []
      ; legacy_tools = []
      ; moderator_runtime = None
      ; platform = Macos
      ; supported_features = Chatmd_shell_spec.Feature.phase4
      }
  with
  | Ok value -> value
  | Error errors ->
    List.map errors ~f:Chatmd_shell_spec.Diagnostic.to_string
    |> String.concat ~sep:"\n"
    |> failwith
;;

let host env root : Shell_runtime.Host.t =
  { env
  ; workspace = root
  ; tool_dir = root
  ; prompt_dir = root
  ; session_dir = root
  ; cache_dir = root
  ; home = root
  ; source_dirs = String.Map.singleton "stream.chatmd" root
  ; process_environment = [| "PATH=/usr/bin:/bin" |]
  ; session_id = "stream-test"
  }
;;

let registry env sw root source =
  let manifest, material = material root source in
  let grant =
    Shell_runtime.Manifest_authorizer.authorize
      Shell_runtime.Manifest_authorizer.assume_authorized
      manifest
    |> function
    | Ok grant -> grant
    | Error error -> failwith error.Shell_runtime.Manifest_authorizer.message
  in
  match
    Shell_runtime.Registry.instantiate
      ~sw
      ~host:(host env root)
      ~manifest
      ~material
      ~grant
      ~approval_provider:Shell_runtime.Approval_broker.None_available
  with
  | Ok registry -> registry
  | Error errors ->
    List.map errors ~f:(fun error -> error.Shell_runtime.Registry.message)
    |> String.concat ~sep:"\n"
    |> failwith
;;

let create registry name =
  let spec = Shell_runtime.Registry.tool registry name |> Option.value_exn in
  Chat_response.Shell_tool.create registry spec
;;

let create_exn registry name =
  match create registry name with
  | Ok tool -> tool
  | Error error -> failwith error.message
;;

let invocation output =
  Ochat_function.Invocation.create (fun progress ->
    F.check (Poly.equal progress.channel `Stdout) "sanitized progress was not combined";
    match progress.update with
    | Append text ->
      F.check (Stdlib.String.is_valid_utf_8 text) "invalid tool progress";
      Buffer.add_string output text
    | Replace _ -> failwith "unexpected replace progress")
;;

let input command =
  `Object
    [ "program", `String command.Shell_access.Command.program
    ; "arguments", `Array (List.map command.arguments ~f:(fun arg -> `String arg))
    ]
  |> Jsonaf.to_string
;;

let output_text = function
  | Openai.Responses.Tool_output.Output.Text text -> text
  | _ -> failwith "expected tool text"
;;

let wiring env root =
  Eio.Switch.run (fun sw ->
    let registry =
      registry env sw root (source {|<secrets><literal value="TOKEN"/></secrets>|})
    in
    let input = input (Shell_access.Command.create "/bin/echo" [ "TOKEN" ]) in
    let output = Buffer.create 128 in
    let observer = invocation output in
    let normal =
      (create_exn registry "normal").run_with_progress ~invocation:observer input
    in
    F.check (Buffer.length output = 0) "finalized tool emitted process progress";
    let streamed =
      (create_exn registry "live").run_with_progress ~invocation:observer input
    in
    F.equal (output_text normal) "[REDACTED]\n";
    F.equal (output_text streamed) (output_text normal);
    F.equal (Buffer.contents output) "[REDACTED]\n")
;;

let registration_rejection env root extra =
  Eio.Switch.run (fun sw ->
    let registry = registry env sw root (source extra) in
    F.check (Result.is_ok (create registry "normal")) "finalized registration changed";
    match create registry "live" with
    | Error error -> F.equal error.code "shell.tool_stream_unsupported"
    | Ok _ -> failwith "unsafe sanitized declaration registered")
;;

let rejected env root =
  List.iter
    [ {|<secrets replacement="aX"><literal value="ab"/></secrets>|}
    ; {|<secrets><literal value="["/></secrets>|}
    ; {|<interceptors><interceptor id="opaque" phase="after" executable="/bin/echo"
          runtime="worker" protocol="shell-hook-json-v1" failure="deny"/></interceptors>|}
    ]
    ~f:(registration_rejection env root)
;;

let live env root =
  Eio.Switch.run (fun sw ->
    let registry =
      registry env sw root (source {|<secrets><literal value="TOKEN"/></secrets>|})
    in
    let output = Buffer.create 128
    and released = ref false in
    let observer =
      Ochat_function.Invocation.create (fun progress ->
        Ochat_function.Invocation.emit (invocation output) progress;
        if not !released
        then (
          F.check
            (not (Eio.Path.is_file Eio.Path.(root / "done")))
            "tool progress was final-only";
          released := true;
          F.save root "release"))
    in
    let tool = create_exn registry "live" in
    let result =
      tool.run_with_progress
        ~invocation:observer
        (input (F.child_command env root "tool-live"))
      |> output_text
    in
    F.check !released "tool invocation ignored progress observer";
    F.equal result (Executor_cases.padding ^ "[REDACTED]" ^ Executor_cases.padding);
    F.equal (Buffer.contents output) result)
;;

let run env =
  List.iter [ wiring; rejected; live ] ~f:(fun test -> F.with_root env (test env))
;;
