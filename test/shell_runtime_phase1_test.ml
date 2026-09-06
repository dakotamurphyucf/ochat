open! Core
module A = Shell_access.Approval
module B = Shell_runtime.Approval_broker
module M = Chatmd_shell_spec.Manifest
module MC = Chatmd_shell_spec.Manifest_compiler
module S = Chatmd_shell_spec.Shell_spec

let source_ref source =
  let position : Chatmd_shell_spec.Source_ref.position =
    { offset = 0; line = 1; column = 1 }
  in
  Chatmd_shell_spec.Source_ref.create
    ~file:"agent.chatmd"
    ~source_dir:"/workspace/prompts"
    ~prompt_dir:"/workspace"
    ~namespace:None
    ~start_pos:position
    ~end_pos:{ position with offset = String.length source }
    ~source
;;

let empty_manifest source =
  M.create
    { encoding_version = "ochat.shell.manifest.v1"
    ; platform = S.Macos
    ; runtimes = []
    ; tools = []
    ; moderator_runtime = None
    ; extension_scripts = []
    ; dependencies = []
    ; required_features = []
    }
  |> fun manifest -> manifest, source_ref source
;;

let%expect_test "manifest grants are bound to the exact digest" =
  let first, _ = empty_manifest "one" in
  let second =
    { first with
      canonical_json = first.canonical_json ^ " "
    ; sha256 = Chatmd_shell_spec.Source_ref.digest (first.canonical_json ^ " ")
    }
  in
  let grant =
    match
      Shell_runtime.Manifest_authorizer.authorize
        Shell_runtime.Manifest_authorizer.assume_authorized
        first
    with
    | Ok grant -> grant
    | Error error -> failwith error.message
  in
  print_s
    [%sexp
      { exact =
          (Shell_runtime.Manifest_authorizer.verify grant first |> Result.is_ok : bool)
      ; changed =
          (Shell_runtime.Manifest_authorizer.verify grant second |> Result.is_error
           : bool)
      }];
  [%expect {| ((exact true) (changed true)) |}]
;;

let fingerprint : Shell_access.Executable.fingerprint =
  { device = 1
  ; inode = 2
  ; mode = 0o755
  ; uid = 3
  ; gid = 4
  ; size = 5L
  ; mtime = 6.
  ; sha256 = "executable-sha256"
  }
;;

let approval_request () =
  let command = Shell_access.Command.create "pwd" [] in
  let executable : Shell_access.Executable.t =
    { requested = "pwd"
    ; path = "/bin/pwd"
    ; canonical_path = "/bin/pwd"
    ; trusted = true
    ; fingerprint
    }
  in
  let capabilities : Shell_access.Capabilities.t =
    { read_roots = [ "/workspace" ]
    ; write_roots = []
    ; network = false
    ; allow_child_processes = false
    ; allow_arbitrary_code = false
    ; allow_privilege_change = false
    ; sandbox = Required
    }
  in
  let context : Shell_access.Context.t =
    { request_id = "request-1"
    ; runtime_id = "readonly"
    ; manifest_sha256 = "manifest-sha256"
    ; command
    ; executable
    ; cwd = "/workspace"
    ; environment = [||]
    ; request_kind = Structured
    ; stdin_kind = Empty
    ; stdin_sha256 = None
    ; stdin_bytes = 0
    ; script_sha256 = None
    ; script_preview = None
    ; origin = Host "test"
    ; effects = []
    ; capabilities
    ; policy_action = None
    ; policy_matches = []
    ; session_id = Some "session-1"
    }
  in
  let identity : A.identity =
    { manifest_sha256 = "manifest-sha256"
    ; runtime_id = "readonly"
    ; request_kind = Structured
    ; command_hash = "command-hash"
    ; executable_sha256 = fingerprint.sha256
    ; argv = [ "pwd" ]
    ; cwd_sha256 = "cwd-hash"
    ; environment_sha256 = "environment-hash"
    ; stdin_sha256 = None
    ; stdin_bytes = 0
    ; script_sha256 = None
    }
  in
  { A.context
  ; policy = { action = Ask; matches = []; reason = "review required" }
  ; identity
  ; display_command = "pwd"
  ; rationale = None
  }
;;

let print_approval_response = function
  | A.Approve -> printf "approve\n"
  | Approve_for (Exact_session { expires_at = None }) -> printf "approve-exact-session\n"
  | Approve_for _ -> printf "approve-other-scope\n"
  | Deny message -> printf "deny:%s\n" message
  | Rewrite command -> printf "rewrite:%s\n" (Shell_access.Command.to_string command)
;;

let%expect_test "approval broker waits without blocking the event loop" =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let wakeups = Eio.Stream.create 1 in
  let results = Eio.Stream.create 1 in
  let broker = B.create ~on_pending:(Eio.Stream.add wakeups) () in
  let ui_request : B.ui_request =
    { id = "approval-1"
    ; request = approval_request ()
    ; runtime_id = "readonly"
    ; manifest_sha256 = "manifest-sha256"
    ; scopes = [ Chatmd_shell_spec.Shell_spec.Once; Exact_session ]
    }
  in
  Eio.Fiber.fork ~sw (fun () -> Eio.Stream.add results (B.request broker ui_request));
  let pending = Eio.Stream.take wakeups in
  printf "pending=%s\n" pending.id;
  ignore (B.respond broker ~id:pending.id Approve_exact_session : (unit, B.error) result);
  Eio.Stream.take results |> print_approval_response;
  [%expect
    {|
    pending=approval-1
    approve-exact-session
    |}]
;;

let%expect_test "closing a broker denies pending and future requests" =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let wakeups = Eio.Stream.create 1 in
  let results = Eio.Stream.create 1 in
  let broker = B.create ~on_pending:(Eio.Stream.add wakeups) () in
  let request : B.ui_request =
    { id = "approval-close"
    ; request = approval_request ()
    ; runtime_id = "readonly"
    ; manifest_sha256 = "manifest-sha256"
    ; scopes = [ Chatmd_shell_spec.Shell_spec.Once; Exact_session ]
    }
  in
  Eio.Fiber.fork ~sw (fun () -> Eio.Stream.add results (B.request broker request));
  ignore (Eio.Stream.take wakeups : B.ui_request);
  B.close broker;
  Eio.Stream.take results |> print_approval_response;
  B.request broker { request with id = "after-close" } |> print_approval_response;
  [%expect
    {|
    deny:approval broker was closed
    deny:approval broker is closed
    |}]
;;

let%expect_test "approval broker presents requests FIFO and cancellation resolves waits" =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let wakeups = Eio.Stream.create 2 in
  let results = Eio.Stream.create 2 in
  let broker = B.create ~on_pending:(Eio.Stream.add wakeups) () in
  let make_request id =
    B.
      { id
      ; request = approval_request ()
      ; runtime_id = "readonly"
      ; manifest_sha256 = "manifest-sha256"
      ; scopes = [ Chatmd_shell_spec.Shell_spec.Once; Exact_session ]
      }
  in
  let run (request : B.ui_request) =
    Eio.Stream.add results (request.id, B.request broker request)
  in
  Eio.Fiber.fork ~sw (fun () -> run (make_request "approval-first"));
  ignore (Eio.Stream.take wakeups : B.ui_request);
  Eio.Fiber.fork ~sw (fun () -> run (make_request "approval-second"));
  ignore (Eio.Stream.take wakeups : B.ui_request);
  printf "pending=%s\n" (B.pending broker |> Option.value_exn).id;
  ignore (B.respond broker ~id:"approval-first" Approve_once : (unit, B.error) result);
  let first_id, first_response = Eio.Stream.take results in
  printf "%s:" first_id;
  print_approval_response first_response;
  printf "pending=%s\n" (B.pending broker |> Option.value_exn).id;
  B.cancel broker ~id:"approval-second";
  let second_id, second_response = Eio.Stream.take results in
  printf "%s:" second_id;
  print_approval_response second_response;
  [%expect
    {|
    pending=approval-first
    approval-first:approve
    pending=approval-second
    approval-second:deny:approval request was cancelled
    |}]
;;

let rec implementation_files path =
  Eio.Path.read_dir path
  |> List.concat_map ~f:(fun name ->
    let child = Eio.Path.(path / name) in
    if Eio.Path.is_directory child
    then implementation_files child
    else if String.is_suffix name ~suffix:".ml"
    then [ child ]
    else [])
;;

let has_direct_spawn path =
  let source = Eio.Path.load path in
  List.exists
    [ "Eio.Process.spawn"; "Eio.Process.run"; "Eio.Process.parse_out" ]
    ~f:(fun substring -> String.is_substring source ~substring)
;;

let%expect_test "shell adapters cannot introduce direct process spawning" =
  Eio_main.run
  @@ fun env ->
  let build_root = Stdlib.Sys.getcwd () |> Filename.dirname in
  let root = Eio.Path.(Eio.Stdenv.fs env / build_root) in
  let directories = [ "chat_response"; "chat_tui"; "shell_runtime" ] in
  let offenders =
    List.concat_map directories ~f:(fun name ->
      implementation_files Eio.Path.(root / "lib" / name))
    |> List.filter ~f:has_direct_spawn
    |> List.map ~f:Eio.Path.native_exn
  in
  print_s [%sexp (offenders : string list)];
  [%expect {| () |}]
;;

let live_source =
  {|<shell_access id="direct" cwd="${workspace}" pipefail="false">
      <capabilities sandbox="direct_unsafe" network="false"
          child_processes="false" arbitrary_code="false" privilege_change="false">
        <read path="${workspace}"/>
      </capabilities>
      <environment inherit="safe">
        <pass name="TOKEN" required="true" secret="true"/>
      </environment>
      <backends merge="replace"><direct when="macos"/></backends>
      <policy default="allow"/>
      <approvals provider="none" unavailable="deny" scopes="once" durable="false"/>
      <audit format="jsonl" path="${session_dir}/runtime-audit.jsonl"
          content="redacted" failure="deny_start"/>
    </shell_access>
    <tool name="echo" type="shell" mode="fixed" runtime="direct" command="/bin/echo"/>|}
;;

let compile_live root =
  let elements =
    Prompt.Chat_markdown.parse_chat_inputs ~source:"agent.chatmd" ~dir:root live_source
  in
  let runtimes, tools =
    List.fold elements ~init:([], []) ~f:(fun (runtimes, tools) -> function
      | Prompt.Chat_markdown.Shell_runtime runtime -> runtime :: runtimes, tools
      | Tool (Shell tool) -> runtimes, tool :: tools
      | _ -> runtimes, tools)
  in
  MC.compile_with_material
    { runtimes
    ; tools
    ; scripts = []
    ; legacy_tools = []
    ; moderator_runtime = None
    ; platform = S.Macos
    ; supported_features = Chatmd_shell_spec.Feature.phase1
    }
  |> function
  | Ok result -> result
  | Error diagnostics ->
    List.map diagnostics ~f:Chatmd_shell_spec.Diagnostic.to_string
    |> String.concat ~sep:"\n"
    |> failwith
;;

let live_host env root =
  { Shell_runtime.Host.env
  ; workspace = root
  ; tool_dir = root
  ; prompt_dir = root
  ; session_dir = root
  ; cache_dir = root
  ; home = root
  ; source_dirs = String.Map.singleton "agent.chatmd" root
  ; process_environment = [| "PATH=/usr/bin:/bin"; "TOKEN=super-private-value" |]
  ; session_id = "session-live"
  ; resource_runner = None
  }
;;

let%expect_test "registry instantiation produces an executable redacting runtime" =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let root = Eio.Path.(Eio.Stdenv.cwd env / "_build" / "shell-runtime-live-test") in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 root;
  let audit_path = Eio.Path.(root / "runtime-audit.jsonl") in
  Eio.Path.save ~create:(`Or_truncate 0o600) audit_path "";
  let manifest, material = compile_live root in
  let grant =
    match
      Shell_runtime.Manifest_authorizer.authorize
        Shell_runtime.Manifest_authorizer.assume_authorized
        manifest
    with
    | Ok grant -> grant
    | Error error -> failwith error.message
  in
  let registry =
    Shell_runtime.Registry.instantiate
      ~sw
      ~host:(live_host env root)
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
  let runtime = Shell_runtime.Registry.runtime registry "direct" |> Option.value_exn in
  let request =
    Shell_access.Request.command
      (Shell_access.Command.create "/bin/echo" [ "super-private-value" ])
  in
  let result =
    Shell_access.Executor.run
      (Shell_runtime.Runtime.executor_config runtime)
      { request
      ; input = Shell_access.Input.Empty
      ; rationale = None
      ; origin = Shell_access.Context.Host "test"
      }
    |> function
    | Ok result -> result
    | Error error -> failwith (Shell_access.Executor.error_to_string error)
  in
  let audit = Eio.Path.load audit_path in
  printf
    "runtimes=%d tools=%d backend=%s output=%saudit-finished=%b audit-secret=%b\n"
    (List.length (Shell_runtime.Registry.runtimes registry))
    (List.length (Shell_runtime.Registry.tools registry))
    result.backend
    result.stdout
    (String.is_substring audit ~substring:"\"event\":\"finished\"")
    (String.is_substring audit ~substring:"super-private-value");
  [%expect
    {|
    runtimes=1 tools=1 backend=direct-unsafe output=[REDACTED]
    audit-finished=true audit-secret=false
    |}]
;;
