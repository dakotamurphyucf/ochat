open! Core
module S = Shell_access

let workspace env = Eio.Path.native_exn (Eio.Stdenv.cwd env)

let capabilities ?(sandbox = S.Capabilities.Preferred) env =
  S.Capabilities.
    { (development ~workspace:(workspace env)) with sandbox }
;;

let fake_backend run = S.Backend.fake ~name:"phase2-fake" run

let config ?reviewer ?limits ?(pipefail = false) ?sandbox env backend =
  S.Executor.config
    ~env
    ~runtime_id:"phase2"
    ~manifest_sha256:"phase2-manifest"
    ~policy:(S.Policy.create ~default:(if Option.is_some reviewer then Ask else Allow) [])
    ~capabilities:(capabilities ?sandbox env)
    ?reviewer
    ?limits
    ~backends:[ backend ]
    ~pipefail
    ()
;;

let invocation ?(input = S.Input.Empty) request : S.Executor.invocation =
  { request; input; rationale = Some "phase2 test"; origin = S.Context.Host "test" }
;;

let status_code = function
  | `Exited code -> code
  | `Signaled signal -> 128 + signal
;;

let parse_chain source =
  match S.Chain.parse source with
  | Ok chain -> chain
  | Error error -> failwith (S.Chain.parse_error_to_string error)
;;

let executor_ok = function
  | Ok result -> result
  | Error error -> failwith (S.Executor.error_to_string error)
;;

let%expect_test "stdin is bounded and included in approval identity" =
  Eio_main.run
  @@ fun env ->
  let requests = ref [] in
  let reviewer request =
    requests := request :: !requests;
    S.Approval.Approve
  in
  let backend =
    fake_backend (fun _plan ~stdin -> Ok { status = `Exited 0; stdout = stdin; stderr = "" })
  in
  let limits = S.Limits.{ default with max_stdin_bytes = 3 } in
  let config = config ~reviewer ~limits env backend in
  let request = S.Request.command (S.Command.create "/bin/cat" []) in
  let too_large = S.Executor.run config (invocation ~input:(Text "four") request) in
  let accepted = S.Executor.run config (invocation ~input:(Text "abc") request) in
  let identity = (List.hd_exn !requests).S.Approval.identity in
  printf
    "too-large=%b accepted=%b bytes=%d digest=%b\n"
    (match too_large with
     | Error (Stdin_limit_exceeded 4) -> true
     | Ok _ | Error _ -> false)
    (Result.is_ok accepted)
    identity.stdin_bytes
    (Option.is_some identity.stdin_sha256);
  [%expect {| too-large=true accepted=true bytes=3 digest=true |}]
;;

let%expect_test "conditional chains prepare only selected branches" =
  Eio_main.run
  @@ fun env ->
  let reviewed = ref [] in
  let reviewer request =
    reviewed := !reviewed @ [ S.Command.basename request.S.Approval.context.command ];
    S.Approval.Approve
  in
  let backend =
    fake_backend (fun plan ~stdin:_ ->
      let name = S.Command.basename plan.S.Execution_plan.context.command in
      let status = if String.equal name "false" then `Exited 1 else `Exited 0 in
      Ok { status; stdout = name; stderr = "" })
  in
  let chain =
    parse_chain "/usr/bin/false && /usr/bin/printf skipped || /bin/echo recovered"
  in
  let result =
    S.Executor.run (config ~reviewer env backend)
      (invocation (S.Request.Structured chain))
  in
  let result = executor_ok result in
  print_s [%sexp ((!reviewed, result.stdout, status_code result.status) : string list * string * int)];
  [%expect {| ((false echo) echo 0) |}]
;;

let%expect_test "real Eio pipelines preserve backpressure and stdout flow" =
  Eio_main.run
  @@ fun env ->
  let config = config ~sandbox:Direct_unsafe env S.Backend.direct in
  let chain = parse_chain "/usr/bin/printf abc | /usr/bin/wc -c" in
  let result =
    S.Executor.run config (invocation (S.Request.Structured chain))
    |> executor_ok
  in
  printf "status=%d stdout=%s" (status_code result.status) result.stdout;
  [%expect {| status=0 stdout=       3
    |}]
;;

let%expect_test "pipefail selects the rightmost failing pipeline status" =
  Eio_main.run
  @@ fun env ->
  let backend =
    fake_backend (fun plan ~stdin:_ ->
      let name = S.Command.basename plan.S.Execution_plan.context.command in
      Ok
        { status = (if String.equal name "false" then `Exited 7 else `Exited 0)
        ; stdout = ""
        ; stderr = ""
        })
  in
  let chain = parse_chain "/usr/bin/false | /usr/bin/true" in
  let run pipefail =
    let result : S.Executor.result =
      S.Executor.run
      (config ~pipefail env backend)
        (invocation (S.Request.Structured chain))
      |> executor_ok
    in
    status_code result.status
  in
  printf "normal=%d pipefail=%d\n" (run false) (run true);
  [%expect {| normal=0 pipefail=7 |}]
;;

let%expect_test "real pipelines handle early consumer exit and total output limits" =
  Eio_main.run
  @@ fun env ->
  let direct ?limits () =
    config ?limits ~sandbox:Direct_unsafe env S.Backend.direct
  in
  let early =
    S.Executor.run
      (direct ())
      (invocation
         (S.Request.Structured
            (parse_chain "/usr/bin/yes | /usr/bin/head -n 1")))
    |> executor_ok
  in
  let constrained =
    S.Limits.
      { default with
        max_stdout_bytes = 3
      ; max_stderr_bytes = 3
      ; max_total_bytes = 3
      }
  in
  let limited =
    S.Executor.run
      (direct ~limits:constrained ())
      (invocation
         (S.Request.command (S.Command.create "/usr/bin/printf" [ "abcdef" ])))
  in
  printf
    "early=%d producer-failed=%b output-limited=%b\n"
    (status_code early.status)
    (match (List.hd_exn early.commands).status with
     | `Exited 0 -> false
     | `Exited _ | `Signaled _ -> true)
    (match limited with
     | Error (Output_limit_exceeded bytes) -> bytes > constrained.max_total_bytes
     | Ok _ | Error _ -> false);
  [%expect {| early=0 producer-failed=true output-limited=true |}]
;;

let%expect_test "wall timeout cancels an active pipeline" =
  Eio_main.run
  @@ fun env ->
  let limits =
    S.Limits.{ default with wall_time_seconds = 0.05; idle_time_seconds = None }
  in
  let result =
    S.Executor.run
      (config ~limits ~sandbox:Direct_unsafe env S.Backend.direct)
      (invocation
         (S.Request.Structured
            (parse_chain "/bin/sleep 1 | /bin/cat")))
  in
  printf
    "timed-out=%b\n"
    (match result with
     | Error (Timed_out _) -> true
     | Ok _ | Error _ -> false);
  [%expect {| timed-out=true |}]
;;

let%expect_test "raw approval identity includes the complete script digest" =
  Eio_main.run
  @@ fun env ->
  let identities = ref [] in
  let reviewer request =
    identities := !identities @ [ request.S.Approval.identity ];
    S.Approval.Approve
  in
  let backend =
    fake_backend (fun _plan ~stdin:_ -> Ok { status = `Exited 0; stdout = ""; stderr = "" })
  in
  let config = config ~reviewer env backend in
  let run script =
    S.Executor.run
      config
      (invocation (S.Request.raw_shell ~executable:"/bin/sh" script))
    |> executor_ok
    |> ignore
  in
  run "printf prefix-one";
  run "printf prefix-two";
  let first, second = List.nth_exn !identities 0, List.nth_exn !identities 1 in
  printf
    "script-diff=%b shell-same=%b\n"
    (not (Option.equal String.equal first.script_sha256 second.script_sha256))
    (String.equal first.executable_sha256 second.executable_sha256);
  [%expect {| script-diff=true shell-same=true |}]
;;

let%expect_test "script files are rehashed immediately before execution" =
  Eio_main.run
  @@ fun env ->
  let path = Eio.Path.(Eio.Stdenv.cwd env / "phase2-script.sh") in
  Eio.Path.save ~create:(`Or_truncate 0o700) path "printf original\n";
  let source_sha256 =
    Eio.Path.load path |> Digestif.SHA256.digest_string |> Digestif.SHA256.to_hex
  in
  let executable_sha256 =
    Eio.Path.(Eio.Stdenv.fs env / "/bin/sh")
    |> Eio.Path.load
    |> Digestif.SHA256.digest_string
    |> Digestif.SHA256.to_hex
  in
  let request =
    S.Request.script_file
      ~executable:"/bin/sh"
      ~arguments:[ Eio.Path.native_exn path ]
      ~path:(Eio.Path.native_exn path)
      ~source_sha256
      ~executable_sha256
      ~max_source_bytes:1024
  in
  Eio.Path.save ~create:(`Or_truncate 0o700) path "printf changed\n";
  let backend =
    fake_backend (fun _plan ~stdin:_ -> Ok { status = `Exited 0; stdout = ""; stderr = "" })
  in
  let result = S.Executor.run (config env backend) (invocation request) in
  printf
    "changed=%b\n"
    (match result with
     | Error (Script_changed _) -> true
     | Ok _ | Error _ -> false);
  [%expect {| changed=true |}]
;;

let%expect_test "script files are rehashed after approval returns" =
  Eio_main.run
  @@ fun env ->
  let path = Eio.Path.(Eio.Stdenv.cwd env / "phase2-reviewed-script.sh") in
  Eio.Path.save ~create:(`Or_truncate 0o700) path "printf original\n";
  let source_sha256 =
    Eio.Path.load path |> Digestif.SHA256.digest_string |> Digestif.SHA256.to_hex
  in
  let executable_sha256 =
    Eio.Path.(Eio.Stdenv.fs env / "/bin/sh")
    |> Eio.Path.load
    |> Digestif.SHA256.digest_string
    |> Digestif.SHA256.to_hex
  in
  let request =
    S.Request.script_file
      ~executable:"/bin/sh"
      ~arguments:[ Eio.Path.native_exn path ]
      ~path:(Eio.Path.native_exn path)
      ~source_sha256
      ~executable_sha256
      ~max_source_bytes:1024
  in
  let reviewer _request =
    Eio.Path.save ~create:(`Or_truncate 0o700) path "printf changed\n";
    S.Approval.Approve
  in
  let backend =
    fake_backend (fun _plan ~stdin:_ ->
      Ok { status = `Exited 0; stdout = ""; stderr = "" })
  in
  let result = S.Executor.run (config ~reviewer env backend) (invocation request) in
  printf
    "changed=%b\n"
    (match result with
     | Error (Script_changed _) -> true
     | Ok _ | Error _ -> false);
  [%expect {| changed=true |}]
;;

let%expect_test "script executables retain their instantiation fingerprint" =
  Eio_main.run
  @@ fun env ->
  let root =
    Eio.Path.
      (Eio.Stdenv.fs env
       / "/tmp"
       / sprintf "ochat-shell-phase2-%08x" (Random.bits ()))
  in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 root;
  let script = Eio.Path.(root / "phase2-executable-script.sh") in
  let executable = Eio.Path.(root / "phase2-executable-copy") in
  Eio.Path.save ~create:(`Or_truncate 0o700) script "printf script\n";
  let original = Eio.Path.(Eio.Stdenv.fs env / "/bin/sh") |> Eio.Path.load in
  Eio.Path.save ~create:(`Or_truncate 0o700) executable original;
  let source_sha256 =
    Eio.Path.load script |> Digestif.SHA256.digest_string |> Digestif.SHA256.to_hex
  in
  let executable_sha256 =
    Digestif.SHA256.(to_hex (digest_string original))
  in
  let request =
    S.Request.script_file
      ~executable:(Eio.Path.native_exn executable)
      ~arguments:[ Eio.Path.native_exn script ]
      ~path:(Eio.Path.native_exn script)
      ~source_sha256
      ~executable_sha256
      ~max_source_bytes:1024
  in
  Eio.Path.save ~create:(`Or_truncate 0o700) executable (original ^ "changed");
  let backend =
    fake_backend (fun _plan ~stdin:_ ->
      Ok { status = `Exited 0; stdout = ""; stderr = "" })
  in
  let result = S.Executor.run (config env backend) (invocation request) in
  printf
    "changed=%b\n"
    (match result with
     | Error (Executable_changed _) -> true
     | Ok _ | Error _ -> false);
  [%expect {| changed=true |}]
;;
