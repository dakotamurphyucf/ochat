open! Core

let fail message = raise_s [%message message]

let resolver env executable =
  let directory = Filename.dirname executable in
  let resolver = Shell_access.Resolver.create ~trusted_roots:[ directory ] () in
  let command = Shell_access.Command.create executable [] in
  match
    Shell_access.Resolver.resolve
      resolver
      ~fs:(Eio.Stdenv.fs env)
      ~cwd:directory
      ~environment:(Core_unix.environment ())
      command
  with
  | Ok executable -> resolver, executable
  | Error message -> fail message
;;

let limits =
  { Shell_access.Limits.default with
    wall_time_seconds = 10.
  ; idle_time_seconds = Some 5.
  ; max_stdin_bytes = 65_536
  ; max_stdout_bytes = 1024
  ; max_stderr_bytes = 1024
  ; max_total_bytes = 2048
  }
;;

let capabilities directory =
  { (Shell_access.Capabilities.development ~workspace:directory) with
    sandbox = Direct_unsafe
  ; network = false
  }
;;

let worker env executable mode =
  let directory = Filename.dirname executable in
  let resolver, executable = resolver env executable in
  let config =
    Shell_access.Executor.config
      ~env
      ~runtime_id:"fixture-worker"
      ~manifest_sha256:(String.make 64 'a')
      ~policy:(Shell_access.Policy.create ~default:Allow [])
      ~capabilities:(capabilities directory)
      ~resolver
      ~backends:[ Shell_access.Backend.direct ]
      ~cwd:Eio.Path.(Eio.Stdenv.fs env / directory)
      ~process_env:[| "PATH=/usr/bin:/bin"; "HOOK_MODE=" ^ mode |]
      ~limits
      ()
  in
  Shell_runtime.Hook_worker.create
    ~env
    ~hook_id:"fixture"
    ~kind:Shell_runtime.Hook_protocol.Before_interceptor
    ~executable
    ~executor_config:config
    ~timeout_seconds:(if String.equal mode "timeout" then 0.1 else 10.)
    ~max_input_bytes:65_536
    ~max_output_bytes:1024
    ~redact:(String.substr_replace_all ~pattern:"private-key-value" ~with_:"[REDACTED]")
    ()
;;

let expected mode = function
  | Ok Shell_runtime.Hook_protocol.Continue when String.equal mode "continue" -> ()
  | Error _ when not (String.equal mode "continue") -> ()
  | Ok action ->
    fail
      ("unexpected action: "
       ^ Sexp.to_string ([%sexp_of: Shell_runtime.Hook_protocol.action] action))
  | Error (error : Shell_runtime.Hook_worker.error) ->
    fail ("unexpected error: " ^ error.message)
;;

let run env executable =
  let executable =
    if Filename.is_relative executable then Caml_unix.realpath executable else executable
  in
  List.iter
    [ "continue"
    ; "wrong_id"
    ; "malformed"
    ; "extra"
    ; "duplicate"
    ; "invalid_utf8"
    ; "overflow"
    ; "timeout"
    ; "nonzero"
    ]
    ~f:(fun mode ->
      Shell_runtime.Hook_worker.invoke (worker env executable mode) (`Object [])
      |> expected mode);
  match
    Shell_runtime.Hook_worker.invoke (worker env executable "secret_echo") (`Object [])
  with
  | Error { stderr = Some "[REDACTED]"; _ } -> ()
  | Error _ | Ok _ -> fail "hook diagnostics were not redacted"
;;

let external_backend_boundaries env =
  let resolver, wrapper = resolver env "/usr/bin/env" in
  let hostile = [ "$(touch nope)"; "a b"; "--flag=value"; "'quoted'" ] in
  let command = Shell_access.Command.create wrapper.canonical_path hostile in
  let executable =
    Shell_access.Resolver.resolve
      resolver
      ~fs:(Eio.Stdenv.fs env)
      ~cwd:"/tmp/a b"
      ~environment:[| "PATH=/usr/bin:/bin" |]
      command
    |> Result.ok_or_failwith
  in
  let context =
    Shell_access.Context.
      { request_id = "request"
      ; runtime_id = "runtime"
      ; manifest_sha256 = String.make 64 'a'
      ; command
      ; executable
      ; cwd = "/tmp/a b"
      ; environment = [| "PATH=/usr/bin:/bin" |]
      ; request_kind = Structured
      ; stdin_kind = Empty
      ; stdin_sha256 = None
      ; stdin_bytes = 0
      ; script_sha256 = None
      ; script_preview = None
      ; origin = Host "test"
      ; effects = []
      ; capabilities = capabilities "/tmp"
      ; policy_action = Some "allow"
      ; policy_matches = []
      ; session_id = None
      }
  in
  let plan =
    Shell_access.Execution_plan.
      { id = "plan"
      ; context
      ; limits
      ; environment = context.environment
      ; cwd = context.cwd
      ; resource_runner = None
      }
  in
  let backend =
    Shell_access.Backend.external_
      ~name:"test"
      ~wrapper
      ~confinement:Declared
      ~accept_declared_confinement:true
      [ Literal "--"; Cwd; Command_argv ]
    |> Result.ok_or_failwith
  in
  let spawn =
    Shell_access.Backend.For_testing.prepare backend ~fs:(Eio.Stdenv.fs env) plan
    |> Result.ok_or_failwith
  in
  let expected =
    wrapper.canonical_path :: "--" :: context.cwd :: executable.canonical_path :: hostile
  in
  if not (List.equal String.equal spawn.argv expected)
  then fail "external backend changed argv boundaries"
;;

let () =
  match Array.to_list (Sys.get_argv ()) with
  | [ _; executable ] ->
    Eio_main.run (fun env ->
      run env executable;
      external_backend_boundaries env)
  | _ -> fail "expected fixture executable path"
;;
