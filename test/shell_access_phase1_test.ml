open! Core
module S = Shell_access

let parse_exn input =
  match S.Chain.parse input with
  | Ok chain -> chain
  | Error error -> failwith (S.Chain.parse_error_to_string error)
;;

let allow_all = S.Policy.create ~default:S.Policy.Allow []
let test_runtime_id = "test-runtime"
let test_manifest_sha256 = "test-manifest"
let workspace env = Eio.Path.native_exn (Eio.Stdenv.cwd env)

let invocation ?rationale ?(input = S.Input.Empty) request : S.Executor.invocation =
  { request; input; rationale; origin = S.Context.Host "test" }
;;

let dev_caps ?(sandbox = S.Capabilities.Preferred) env =
  S.Capabilities.{ (development ~workspace:(workspace env)) with sandbox }
;;

let run_exn config request =
  match S.Executor.run config (invocation request) with
  | Ok result -> result
  | Error error -> failwith (S.Executor.error_to_string error)
;;

let fake_backend ?(output = fun command -> S.Command.to_string command ^ "\n") () =
  S.Backend.fake ~name:"fake-sandbox" (fun plan ~stdin:_ ->
    Ok
      { status = `Exited 0
      ; stdout = output plan.S.Execution_plan.context.command
      ; stderr = ""
      })
;;

let approval_config ~env ~store ~reviewer ~manifest_sha256 ~process_env =
  S.Executor.config
    ~env
    ~runtime_id:test_runtime_id
    ~manifest_sha256
    ~policy:(S.Policy.create ~default:Ask [])
    ~capabilities:(dev_caps env)
    ~reviewer
    ~approval_store:store
    ~session_id:"session-a"
    ~backends:[ fake_backend () ]
    ?process_env
    ()
;;

let%expect_test "structured parser is conservative and reports useful offsets" =
  let chain = parse_exn "printf 'hello world' | wc -c && echo done" in
  List.iter (S.Chain.commands chain) ~f:(fun command ->
    print_endline (S.Command.to_string command));
  [ "echo $(id)"; "cat secret > copy"; "sleep 1 &"; "echo ok |" ]
  |> List.iter ~f:(fun input ->
    match S.Chain.parse input with
    | Ok _ -> print_endline "unexpected success"
    | Error error -> Printf.printf "%d:%s\n" error.offset error.message);
  [%expect
    {|
    printf 'hello world'
    wc -c
    echo done
    5:command substitution is not supported
    11:redirection is not supported by structured execution
    8:background execution is not supported
    0:expected a command
    |}]
;;

let%expect_test "parser round-trips generated ordinary argv" =
  let state = Random.State.make [| 7; 11; 13 |] in
  let alphabet = "abcdefghijklmnopqrstuvwxyz0123456789_-" in
  let word () =
    String.init
      (1 + Random.State.int state 12)
      ~f:(fun _ -> alphabet.[Random.State.int state (String.length alphabet)])
  in
  let passed = ref 0 in
  for _ = 1 to 250 do
    let words = List.init (1 + Random.State.int state 6) ~f:(fun _ -> word ()) in
    let source = String.concat ~sep:" " words in
    match S.Chain.commands (parse_exn source) with
    | [ command ] when List.equal String.equal words (S.Command.to_argv command) ->
      Int.incr passed
    | _ -> failwith ("round-trip failed for " ^ source)
  done;
  print_s [%sexp (!passed : int)];
  [%expect {| 250 |}]
;;

let%expect_test "custom tool arguments remain literal argv" =
  let request =
    S.Request.custom_tool
      ~command_line:"printf --fixed"
      ~arguments:[ "; rm -rf /"; "$(id)"; "a | b" ]
    |> Result.ok_or_failwith
  in
  (match request with
   | S.Request.Structured chain ->
     List.iter (S.Chain.commands chain) ~f:(fun command ->
       print_s [%sexp (S.Command.to_argv command : string list)])
   | Script_file _ | Raw_shell _ -> print_endline "unexpected request kind");
  print_endline
    (match S.Request.custom_tool ~command_line:"echo ok | wc" ~arguments:[] with
     | Ok _ -> "unexpected chain"
     | Error _ -> "chain rejected");
  [%expect
    {|
    (printf --fixed "; rm -rf /" "$(id)" "a | b")
    chain rejected
    |}]
;;

let%expect_test "resolver exposes PATH hijacking and detects executable replacement" =
  Eio_main.run
  @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let directory = Core_unix.mkdtemp "/tmp/shell-access-resolver.XXXXXX" in
  let executable = Filename.concat directory "ls" in
  Eio.Path.save
    ~create:(`Exclusive 0o755)
    Eio.Path.(fs / executable)
    "#!/bin/sh\nprintf fake\n";
  let resolver = S.Resolver.create ~search_path:[ directory ] () in
  let command = S.Command.create "ls" [] in
  let resolved =
    S.Resolver.resolve
      resolver
      ~fs
      ~cwd:directory
      ~environment:[| "PATH=" ^ directory |]
      command
    |> Result.ok_or_failwith
  in
  Printf.printf "trusted=%b\n" resolved.trusted;
  Eio.Path.save ~append:true ~create:`Never Eio.Path.(fs / executable) "# changed\n";
  print_endline
    (match S.Resolver.verify ~fs resolved with
     | Ok () -> "unchanged"
     | Error _ -> "changed");
  [%expect
    {|
    trusted=false
    changed
    |}]
;;

let%expect_test "resolver aliases enforce pins and explicit trust" =
  Eio_main.run
  @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let baseline =
    S.Resolver.resolve
      (S.Resolver.create ())
      ~fs
      ~cwd:"/"
      ~environment:[| "PATH=/bin:/usr/bin" |]
      (S.Command.create "/bin/echo" [])
    |> Result.ok_or_failwith
  in
  let pin : S.Resolver.pinned =
    { path = baseline.canonical_path
    ; sha256 = Some baseline.fingerprint.sha256
    ; trusted = true
    }
  in
  let resolver =
    S.Resolver.create ~trusted_roots:[] ~executables:[ "echo-pin", pin ] ()
  in
  let resolved =
    S.Resolver.resolve
      resolver
      ~fs
      ~cwd:"/"
      ~environment:[||]
      (S.Command.create "echo-pin" [])
    |> Result.ok_or_failwith
  in
  printf "requested=%s trusted=%b\n" resolved.requested resolved.trusted;
  let bad_pin = { pin with sha256 = Some (String.make 64 '0') } in
  let bad_resolver =
    S.Resolver.create ~trusted_roots:[] ~executables:[ "echo-pin", bad_pin ] ()
  in
  printf
    "bad-pin=%b\n"
    (S.Resolver.resolve
       bad_resolver
       ~fs
       ~cwd:"/"
       ~environment:[||]
       (S.Command.create "echo-pin" [])
     |> Result.is_error);
  [%expect
    {|
    requested=echo-pin trusted=true
    bad-pin=true
    |}]
;;

let%expect_test "capability roots are symlink aware" =
  Eio_main.run
  @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let directory = Core_unix.mkdtemp "/tmp/shell-access-roots.XXXXXX" in
  let root = Filename.concat directory "root" in
  let outside = Filename.concat directory "outside" in
  Eio.Path.mkdir ~perm:0o700 Eio.Path.(fs / root);
  Eio.Path.mkdir ~perm:0o700 Eio.Path.(fs / outside);
  Eio.Path.symlink ~link_to:outside Eio.Path.(fs / Filename.concat root "escape");
  let capabilities = S.Capabilities.read_only ~roots:[ root ] in
  let result =
    S.Effect.check_capabilities
      capabilities
      ~fs
      ~cwd:root
      [ S.Effect.Read_path (Filename.concat root "escape/secret") ]
  in
  print_endline
    (match result with
     | Ok () -> "allowed"
     | Error _ -> "denied");
  [%expect {| denied |}]
;;

let%expect_test "unknown commands require arbitrary-code capability" =
  Eio_main.run
  @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let effects =
    S.Effect.analyze ~raw_shell:false ~cwd:"/workspace" (S.Command.create "mystery" [])
  in
  let denied =
    S.Effect.check_capabilities
      (S.Capabilities.read_only ~roots:[ "/workspace" ])
      ~fs
      ~cwd:"/workspace"
      effects
  in
  let allowed =
    S.Effect.check_capabilities
      S.Capabilities.
        { (development ~workspace:"/workspace") with sandbox = Direct_unsafe }
      ~fs
      ~cwd:"/workspace"
      effects
  in
  Printf.printf
    "%s %s\n"
    (Result.is_error denied |> Bool.to_string)
    (Result.is_ok allowed |> Bool.to_string);
  [%expect {| true true |}]
;;

let%expect_test "required sandbox never silently falls back to direct execution" =
  Eio_main.run
  @@ fun env ->
  let capabilities = S.Capabilities.{ (dev_caps env) with sandbox = Required } in
  let config =
    S.Executor.config
      ~env
      ~runtime_id:test_runtime_id
      ~manifest_sha256:test_manifest_sha256
      ~policy:allow_all
      ~capabilities
      ~backends:[ S.Backend.direct ]
      ()
  in
  (match
     S.Executor.run
       config
       (invocation (S.Request.command (S.Command.create "echo" [ "hello" ])))
   with
   | Error (S.Executor.Sandbox_unavailable _) -> print_endline "sandbox required"
   | Error error -> print_endline (S.Executor.error_to_string error)
   | Ok _ -> print_endline "unexpected execution");
  [%expect {| sandbox required |}]
;;

let%expect_test "approval rewrite is resolved and re-evaluated" =
  Eio_main.run
  @@ fun env ->
  let policy =
    S.Policy.create
      ~default:Ask
      [ S.Policy.rule ~id:"echo" ~action:Allow (S.Matcher.basename "echo") ]
  in
  let reviewer request =
    Printf.printf "review:%s\n" request.S.Approval.display_command;
    S.Approval.Rewrite (S.Command.create "echo" [ "safer" ])
  in
  let config =
    S.Executor.config
      ~env
      ~runtime_id:test_runtime_id
      ~manifest_sha256:test_manifest_sha256
      ~policy
      ~capabilities:(dev_caps env)
      ~reviewer
      ~backends:
        [ fake_backend
            ~output:(fun command -> String.concat ~sep:" " command.arguments ^ "\n")
            ()
        ]
      ()
  in
  let result = run_exn config (S.Request.command (S.Command.create "uname" [ "-a" ])) in
  print_string result.stdout;
  [%expect
    {|
    review:uname -a
    safer
    |}]
;;

let%expect_test "session approval cache is exact and executable-bound" =
  Eio_main.run
  @@ fun env ->
  let calls = ref 0 in
  let reviewer _ =
    Int.incr calls;
    S.Approval.Approve_for (Exact_session { expires_at = None })
  in
  let store = S.Approval.create_store () in
  let config =
    S.Executor.config
      ~env
      ~runtime_id:test_runtime_id
      ~manifest_sha256:test_manifest_sha256
      ~policy:(S.Policy.create ~default:Ask [])
      ~capabilities:(dev_caps env)
      ~reviewer
      ~approval_store:store
      ~session_id:"session-a"
      ~backends:[ fake_backend () ]
      ()
  in
  ignore
    (run_exn config (S.Request.command (S.Command.create "uname" [ "-s" ]))
     : S.Executor.result);
  ignore
    (run_exn config (S.Request.command (S.Command.create "uname" [ "-s" ]))
     : S.Executor.result);
  print_s [%sexp (!calls : int)];
  [%expect {| 1 |}]
;;

let%expect_test "session approvals are bound to the manifest" =
  Eio_main.run
  @@ fun env ->
  let calls = ref 0 in
  let reviewer _ =
    Int.incr calls;
    S.Approval.Approve_for (Exact_session { expires_at = None })
  in
  let store = S.Approval.create_store () in
  let run manifest_sha256 =
    let config =
      approval_config ~env ~store ~reviewer ~manifest_sha256 ~process_env:None
    in
    ignore
      (run_exn config (S.Request.command (S.Command.create "uname" [ "-s" ]))
       : S.Executor.result)
  in
  run "manifest-a";
  run "manifest-a";
  run "manifest-b";
  print_s [%sexp (!calls : int)];
  [%expect {| 2 |}]
;;

let%expect_test "environment identity is independent of entry ordering" =
  Eio_main.run
  @@ fun env ->
  let calls = ref 0 in
  let reviewer _ =
    Int.incr calls;
    S.Approval.Approve_for (Exact_session { expires_at = None })
  in
  let store = S.Approval.create_store () in
  let run process_env =
    let config =
      approval_config
        ~env
        ~store
        ~reviewer
        ~manifest_sha256:test_manifest_sha256
        ~process_env:(Some process_env)
    in
    ignore
      (run_exn config (S.Request.command (S.Command.create "uname" [ "-s" ]))
       : S.Executor.result)
  in
  run [| "Z=2"; "A=1"; "PATH=/usr/bin:/bin" |];
  run [| "PATH=/usr/bin:/bin"; "A=1"; "Z=2" |];
  print_s [%sexp (!calls : int)];
  [%expect {| 1 |}]
;;

let%expect_test "LLM moderator protocol rejects duplicates, unknown fields, and prose" =
  [ {|{"decision":"allow_once"}|}
  ; {|{"decision":"deny","reason":"no","reason":"duplicate"}|}
  ; {|{"decision":"allow_once","extra":true}|}
  ; {|{"decision":"allow_once","reason":"irrelevant"}|}
  ; {|{"decision":"allow_prefix_session","prefix":[]}|}
  ; {|sure {"decision":"allow_once"}|}
  ]
  |> List.iter ~f:(fun response ->
    print_endline
      (match S.Approval.response_of_json response with
       | Ok _ -> "ok"
       | Error _ -> "error"));
  [%expect
    {|
    ok
    error
    error
    error
    error
    error
    |}]
;;

let%expect_test "trusted substitute runs before review but cannot bypass a hard deny" =
  Eio_main.run
  @@ fun env ->
  let reviews = ref 0 in
  let reviewer _ =
    Int.incr reviews;
    S.Approval.Approve
  in
  let substitute =
    S.Interceptor.trusted_substitute ~name:"python-substitute" ~before:(fun context ->
      if String.equal (S.Command.basename context.command) "python3"
      then
        S.Interceptor.Respond
          { command = context.command
          ; executable = Some context.executable
          ; status = `Exited 0
          ; stdout = "safe substitute\n"
          ; stderr = ""
          ; stdout_truncated = false
          ; stderr_truncated = false
          ; intercepted_by = None
          ; untrusted_output = false
          }
      else Continue)
  in
  let policy =
    S.Policy.create
      ~default:Ask
      [ S.Policy.rule ~id:"deny-rm" ~action:Deny (S.Matcher.basename "rm") ]
  in
  let config =
    S.Executor.config
      ~env
      ~runtime_id:test_runtime_id
      ~manifest_sha256:test_manifest_sha256
      ~policy
      ~capabilities:(dev_caps env)
      ~reviewer
      ~interceptors:[ substitute ]
      ~backends:[ fake_backend () ]
      ()
  in
  let python =
    run_exn config (S.Request.command (S.Command.create "python3" [ "secret" ]))
  in
  print_string python.stdout;
  (match
     S.Executor.run
       config
       (invocation (S.Request.command (S.Command.create "rm" [ "file" ])))
   with
   | Error (Denied _) -> print_endline "rm denied"
   | _ -> print_endline "unexpected");
  Printf.printf "reviews=%d\n" !reviews;
  [%expect
    {|
    safe substitute
    rm denied
    reviews=0
    |}]
;;

let%expect_test
    "output is bounded, control-filtered, and secret-redacted after interception"
  =
  Eio_main.run
  @@ fun env ->
  let secret = "PRIVATE-KEY-MATERIAL" in
  let limits = S.Limits.{ default with max_stdout_bytes = 20; max_total_bytes = 100 } in
  let config =
    S.Executor.config
      ~env
      ~runtime_id:test_runtime_id
      ~manifest_sha256:test_manifest_sha256
      ~policy:allow_all
      ~capabilities:(dev_caps env)
      ~limits
      ~secret_filter:(S.Secret_filter.create [ secret ])
      ~backends:
        [ fake_backend
            ~output:(fun _ -> "\027[31m" ^ secret ^ "\027[0m-suffix-that-is-long")
            ()
        ]
      ()
  in
  let result = run_exn config (S.Request.command (S.Command.create "echo" [])) in
  let command = List.hd_exn result.commands in
  Printf.printf
    "%s\ntruncated=%b escape=%b\n"
    result.stdout
    command.stdout_truncated
    (String.contains result.stdout '\027');
  [%expect
    {|
    [REDACTED]-suffix-th
    truncated=true escape=false
    |}]
;;

let%expect_test "output filters cannot reintroduce secrets or terminal controls" =
  Eio_main.run
  @@ fun env ->
  let secret = "FILTER-SECRET" in
  let filter =
    S.Interceptor.output_filter ~name:"host-filter" ~after:(fun result ->
      { result with stdout = result.stdout ^ "\027[32m" ^ secret ^ "\027[0m" })
  in
  let config =
    S.Executor.config
      ~env
      ~runtime_id:test_runtime_id
      ~manifest_sha256:test_manifest_sha256
      ~policy:allow_all
      ~capabilities:(dev_caps env)
      ~interceptors:[ filter ]
      ~secret_filter:(S.Secret_filter.create [ secret ])
      ~backends:[ fake_backend ~output:(fun _ -> "base") () ]
      ()
  in
  let result = run_exn config (S.Request.command (S.Command.create "echo" [])) in
  Printf.printf "%s escape=%b\n" result.stdout (String.contains result.stdout '\027');
  [%expect {| base[REDACTED] escape=false |}]
;;

let%expect_test "real pipelines stream concurrently and keep stderr out of stdin" =
  Eio_main.run
  @@ fun env ->
  let limits = S.Limits.{ default with idle_time_seconds = None } in
  let config =
    S.Executor.config
      ~env
      ~runtime_id:test_runtime_id
      ~manifest_sha256:test_manifest_sha256
      ~policy:allow_all
      ~capabilities:(dev_caps ~sandbox:Direct_unsafe env)
      ~limits
      ~backends:[ S.Backend.direct ]
      ()
  in
  let request =
    S.Request.Structured (parse_exn "sh -c 'printf out; printf err >&2' | wc -c")
  in
  let result = run_exn config request in
  Printf.printf
    "stdout=%s stderr=%s\n"
    (String.strip result.stdout)
    (String.strip result.stderr);
  [%expect {| stdout=3 stderr=err |}]
;;

let%expect_test "wall and idle timeout errors remain distinct" =
  let run limits =
    Eio_main.run
    @@ fun env ->
    let config =
      S.Executor.config
        ~env
        ~runtime_id:test_runtime_id
        ~manifest_sha256:test_manifest_sha256
        ~policy:allow_all
        ~capabilities:(dev_caps ~sandbox:Direct_unsafe env)
        ~limits
        ~backends:[ S.Backend.direct ]
        ()
    in
    match
      S.Executor.run
        config
        (invocation (S.Request.command (S.Command.create "sleep" [ "1" ])))
    with
    | Error (Timed_out _) -> "wall"
    | Error (Idle_timed_out _) -> "idle"
    | Error error -> S.Executor.error_to_string error
    | Ok _ -> "unexpected"
  in
  print_endline
    (run S.Limits.{ default with wall_time_seconds = 0.05; idle_time_seconds = None });
  print_endline
    (run S.Limits.{ default with wall_time_seconds = 2.; idle_time_seconds = Some 0.05 });
  [%expect
    {|
    wall
    idle
    |}]
;;

let%expect_test "audit stream records resolution, policy, plan, start, and finish" =
  Eio_main.run
  @@ fun env ->
  let envelopes = ref [] in
  let audit =
    S.Audit.create ~failure_policy:Ignore_failure (fun envelope ->
      envelopes := envelope :: !envelopes;
      Ok ())
  in
  let config =
    S.Executor.config
      ~env
      ~runtime_id:test_runtime_id
      ~manifest_sha256:test_manifest_sha256
      ~policy:allow_all
      ~capabilities:(dev_caps env)
      ~audit
      ~backends:[ fake_backend () ]
      ()
  in
  ignore
    (run_exn config (S.Request.command (S.Command.create "echo" [ "ok" ]))
     : S.Executor.result);
  let labels =
    List.rev !envelopes
    |> List.map ~f:(fun envelope ->
      match envelope.S.Audit.event with
      | S.Audit.Resolved _ -> "resolved"
      | Policy_decided _ -> "policy"
      | Plan_created _ -> "plan"
      | Approval_requested _ -> "approval-requested"
      | Approval_answered _ -> "approval-answered"
      | Reviewer_completed _ -> "reviewer-completed"
      | Intercepted _ -> "intercepted"
      | Started _ -> "started"
      | Output _ -> "output"
      | Finished _ -> "finished"
      | Terminated _ -> "terminated"
      | Rejected _ -> "rejected")
  in
  print_endline (String.concat ~sep:"," labels);
  let envelopes = List.rev !envelopes in
  let sequences = List.map envelopes ~f:(fun envelope -> envelope.S.Audit.sequence) in
  let request_ids =
    List.map envelopes ~f:(fun envelope -> envelope.S.Audit.request_id)
    |> String.Set.of_list
    |> Set.length
  in
  print_s [%sexp (sequences : int64 list)];
  Printf.printf
    "requests=%d runtime=%s manifest=%s\n"
    request_ids
    (List.hd_exn envelopes).runtime_id
    (List.hd_exn envelopes).manifest_sha256;
  [%expect
    {|
    resolved,policy,plan,started,finished
    (0 1 2 3 4)
    requests=1 runtime=test-runtime manifest=test-manifest
    |}]
;;

let%expect_test "audit failure policy is explicit" =
  Eio_main.run
  @@ fun env ->
  let run failure_policy =
    let audit = S.Audit.create ~failure_policy (fun _envelope -> Error "offline") in
    let config =
      S.Executor.config
        ~env
        ~runtime_id:test_runtime_id
        ~manifest_sha256:test_manifest_sha256
        ~policy:allow_all
        ~capabilities:(dev_caps env)
        ~audit
        ~backends:[ fake_backend () ]
        ()
    in
    match
      S.Executor.run
        config
        (invocation (S.Request.command (S.Command.create "echo" [])))
    with
    | Ok _ -> "ok"
    | Error (Audit_unavailable _) -> "audit-unavailable"
    | Error error -> S.Executor.error_to_string error
  in
  print_endline (run Ignore_failure);
  print_endline (run Deny_start);
  print_endline (run Terminate_runtime);
  [%expect
    {|
    ok
    audit-unavailable
    audit-unavailable
    |}]
;;

let%expect_test "rewrite depth is bounded" =
  Eio_main.run (fun env ->
    let interceptor =
      S.Interceptor.trusted_substitute ~name:"loop" ~before:(fun context ->
        S.Interceptor.Rewrite context.command)
    in
    let config =
      S.Executor.config
        ~env
        ~runtime_id:test_runtime_id
        ~manifest_sha256:test_manifest_sha256
        ~policy:allow_all
        ~capabilities:(dev_caps env)
        ~interceptors:[ interceptor ]
        ~backends:[ fake_backend () ]
        ()
    in
    match S.Executor.run config (invocation (S.Request.command (S.Command.create "echo" []))) with
    | Ok _ -> print_endline "unexpected"
    | Error error -> print_endline (S.Executor.error_to_string error));
  [%expect {| command denied: too many command rewrites |}]
;;

let%expect_test "reviewer metadata is emitted without hidden reasoning" =
  Eio_main.run (fun env ->
    let events = ref [] in
    let audit =
      S.Audit.create ~failure_policy:Ignore_failure (fun envelope ->
        events := envelope.event :: !events;
        Ok ())
    in
    let reviewer_with_metadata _request =
      S.Approval.
        { response = Approve
        ; metadata =
            Some
              { reviewer_id = "reviewer"
              ; reviewer_kind = "model"
              ; model = Some "test-model"
              ; input_tokens = Some 3
              ; output_tokens = Some 1
              ; latency_ms = Some 5
              }
        }
    in
    let config =
      S.Executor.config
        ~env
        ~runtime_id:test_runtime_id
        ~manifest_sha256:test_manifest_sha256
        ~policy:(S.Policy.create ~default:Ask [])
        ~capabilities:(dev_caps env)
        ~reviewer_with_metadata
        ~audit
        ~backends:[ fake_backend () ]
        ()
    in
    ignore (run_exn config (S.Request.command (S.Command.create "echo" [])) : S.Executor.result);
    List.rev !events
    |> List.iter ~f:(function
      | S.Audit.Reviewer_completed (_, metadata, action) ->
        printf "%s:%s:%s\n" metadata.reviewer_id (Option.value_exn metadata.model) action
      | _ -> ()));
  [%expect {| reviewer:test-model:approve_once |}]
;;

let%expect_test "interceptor output is redacted again before disclosure" =
  Eio_main.run (fun env ->
    let interceptor =
      S.Interceptor.output_filter ~name:"replacement" ~after:(fun result ->
        { result with stdout = "PRIVATE_KEY" })
    in
    let config =
      S.Executor.config
        ~env
        ~runtime_id:test_runtime_id
        ~manifest_sha256:test_manifest_sha256
        ~policy:allow_all
        ~capabilities:(dev_caps env)
        ~interceptors:[ interceptor ]
        ~secret_filter:(S.Secret_filter.create [ "PRIVATE_KEY" ])
        ~backends:[ fake_backend () ]
        ()
    in
    let result = run_exn config (S.Request.command (S.Command.create "echo" [])) in
    print_endline result.stdout);
  [%expect {| [REDACTED] |}]
;;

let%expect_test "backend confinement and availability are inspectable" =
  Eio_main.run
  @@ fun env ->
  let confinement backend =
    match S.Backend.confinement backend with
    | Verified -> "verified"
    | Declared -> "declared"
    | Unconfined -> "unconfined"
  in
  let fs = Eio.Stdenv.fs env in
  Printf.printf
    "%s %s\n"
    (confinement S.Backend.direct)
    (Result.is_ok (S.Backend.availability S.Backend.direct ~fs) |> Bool.to_string);
  Printf.printf "%s\n" (confinement (fake_backend ()));
  [%expect
    {|
    unconfined true
    verified
    |}]
;;

let%expect_test "resource runner identity is verified before spawn" =
  Eio_main.run
  @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let directory = Core_unix.mkdtemp "/tmp/shell-access-runner.XXXXXX" in
  let runner = Filename.concat directory "runner" in
  Eio.Path.save
    ~create:(`Exclusive 0o755)
    Eio.Path.(fs / runner)
    "#!/bin/sh\nexec \"$@\"\n";
  let audit =
    S.Audit.create ~failure_policy:Ignore_failure (fun envelope ->
      (match envelope.S.Audit.event with
       | Plan_created _ ->
         Eio.Path.save
           ~create:(`Or_truncate 0o755)
           Eio.Path.(fs / runner)
           "#!/bin/sh\nexit 99\n"
       | _ -> ());
      Ok ())
  in
  let limits = S.Limits.{ default with cpu_seconds = Some 1 } in
  let config =
    S.Executor.config
      ~env
      ~runtime_id:test_runtime_id
      ~manifest_sha256:test_manifest_sha256
      ~policy:allow_all
      ~capabilities:(dev_caps ~sandbox:Direct_unsafe env)
      ~limits
      ~resource_runner:runner
      ~audit
      ~backends:[ S.Backend.direct ]
      ()
  in
  let result =
    S.Executor.run
      config
      (invocation (S.Request.command (S.Command.create "echo" [])))
  in
  print_endline
    (match result with
     | Error (Executable_changed _) -> "changed"
     | Error error -> S.Executor.error_to_string error
     | Ok _ -> "unexpected");
  [%expect {| changed |}]
;;
