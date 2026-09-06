open! Core
module F = Fixture
module S = Shell_access

let secret_filter = S.Secret_filter.create [ "TOKEN" ]
let padding = String.make 80 '.'

let echo_backend =
  S.Backend.fake ~name:"stream-fixture" (fun plan ~stdin:_ ->
    Ok
      S.Backend.
        { status = `Exited 0
        ; stdout = String.concat plan.context.command.arguments
        ; stderr = ""
        })
;;

let canonical_equal (left : S.Executor.result) (right : S.Executor.result) =
  F.check
    (Poly.equal { left with request_id = "" } { right with request_id = "" })
    "streaming changed canonical result"
;;

let sequential env root =
  let config = F.config ~secret_filter env root echo_backend in
  let request = F.sequence [ F.command "TO"; F.command "KEN" ] in
  let stdout, _, on_progress = F.collect () in
  let normal = S.Executor.run config request |> F.executor_ok in
  let streamed = S.Executor.run_streaming config request ~on_progress |> F.executor_ok in
  canonical_equal normal streamed;
  F.equal streamed.stdout "KEN";
  F.equal (Buffer.contents stdout) "[REDACTED]"
;;

let after_rejection env root =
  let calls = ref 0 in
  let filter =
    S.Interceptor.output_filter ~name:"opaque" ~after:(fun result ->
      Int.incr calls;
      { result with stdout = "changed" })
  in
  let config = F.config ~interceptors:[ filter ] env root echo_backend in
  let stdout, _, on_progress = F.collect () in
  F.check (Result.is_error (S.Executor.streaming_support config)) "after filter accepted";
  F.check
    (Result.is_error (S.Executor.run_streaming config (F.request "raw") ~on_progress))
    "streamed through an opaque filter";
  F.check
    (!calls = 0 && Buffer.length stdout = 0)
    "unsupported execution had side effects";
  F.equal (S.Executor.run config (F.request "raw") |> F.executor_ok).stdout "changed";
  F.check (!calls = 1) "normal after hook changed"
;;

let before_substitute env root =
  let calls = ref 0 in
  let before context =
    Int.incr calls;
    S.Interceptor.Respond
      { command = context.S.Context.command
      ; executable = None
      ; status = `Exited 0
      ; stdout = "TOKEN"
      ; stderr = ""
      ; stdout_truncated = false
      ; stderr_truncated = false
      ; intercepted_by = Some "substitute"
      ; untrusted_output = false
      }
  in
  let interceptor = S.Interceptor.trusted_substitute ~name:"substitute" ~before in
  let config policy =
    F.config ~secret_filter ~policy ~interceptors:[ interceptor ] env root echo_backend
  in
  let stdout, _, on_progress = F.collect () in
  let denied =
    S.Executor.run_streaming
      (config (S.Policy.create ~default:Deny []))
      (F.request "ignored")
      ~on_progress
  in
  F.check
    (Result.is_error denied && !calls = 0 && Buffer.length stdout = 0)
    "callback or substitute ran before policy";
  let result =
    S.Executor.run_streaming
      (config (S.Policy.create ~default:Allow []))
      (F.request "ignored")
      ~on_progress
    |> F.executor_ok
  in
  F.equal result.stdout "[REDACTED]";
  F.equal (Buffer.contents stdout) "[REDACTED]"
;;

let budget_prefix env root =
  let limits = S.Limits.{ default with max_stdout_bytes = 1; max_stderr_bytes = 20 } in
  let backend =
    S.Backend.fake ~name:"unicode-fixture" (fun _ ~stdin:_ ->
      Ok S.Backend.{ status = `Exited 0; stdout = "🙂A"; stderr = "Z" })
  in
  let stdout, stderr, on_progress = F.collect () in
  let config = F.config ~limits env root backend in
  let normal = S.Executor.run config (F.request "ignored") |> F.executor_ok in
  let result =
    S.Executor.run_streaming config (F.request "ignored") ~on_progress |> F.executor_ok
  in
  canonical_equal normal result;
  F.equal (Buffer.contents stdout) "Z";
  F.equal (Buffer.contents stderr) ""
;;

let read_ack root =
  let bytes = ref 0 in
  S.Audit.create ~failure_policy:Terminate_runtime (fun envelope ->
    (match envelope.S.Audit.event with
     | Output (_, _, `Stdout, count) ->
       bytes := !bytes + count;
       if !bytes >= String.length padding + 2 then F.save root "read-1"
     | _ -> ());
    Ok ())
;;

let child_live env root =
  Eio.Flow.copy_string (padding ^ "TO") (Eio.Stdenv.stdout env);
  F.await env (fun () -> Eio.Path.is_file Eio.Path.(root / "read-1"));
  Eio.Flow.copy_string ("KEN" ^ padding ^ "🙂") (Eio.Stdenv.stdout env);
  F.await env (fun () -> Eio.Path.is_file Eio.Path.(root / "release"));
  F.save root "done"
;;

let child_cancel env root =
  Eio.Flow.copy_string (padding ^ "TO") (Eio.Stdenv.stdout env);
  F.await env (fun () -> Eio.Path.is_file Eio.Path.(root / "never"))
;;

let child_stderr env root mode =
  let first, second =
    if String.equal mode "stderr-a"
    then "\240\159", "\153\130TOKEN"
    else "\027[", "31mTOKEN\027[0m"
  in
  Eio.Flow.copy_string first (Eio.Stdenv.stderr env);
  F.await env (fun () -> Eio.Path.is_file Eio.Path.(root / (mode ^ "-read")));
  Eio.Flow.copy_string second (Eio.Stdenv.stderr env)
;;

let child env root mode =
  match mode with
  | "tool-live" ->
    Eio.Flow.copy_string (padding ^ "TOKEN" ^ padding) (Eio.Stdenv.stdout env);
    F.await env (fun () -> Eio.Path.is_file Eio.Path.(root / "release"));
    F.save root "done"
  | "live" -> child_live env root
  | "cancel" -> child_cancel env root
  | "stderr-a" | "stderr-b" -> child_stderr env root mode
  | _ -> failwith "unknown child fixture mode"
;;

let live env root =
  let stdout, _, collect = F.collect () in
  let early = ref false in
  let on_progress progress =
    collect progress;
    Eio.Fiber.yield ();
    if
      (not !early) && String.is_substring (Buffer.contents stdout) ~substring:"[REDACTED]"
    then (
      F.check (not (Eio.Path.is_file Eio.Path.(root / "done"))) "progress was final-only";
      early := true;
      F.save root "release")
  in
  let config = F.config ~secret_filter ~audit:(read_ack root) env root S.Backend.direct in
  let result =
    S.Executor.run_streaming config (F.child_request env root "live") ~on_progress
    |> F.executor_ok
  in
  F.check !early "no early sanitized progress";
  F.equal result.stdout (padding ^ "[REDACTED]" ^ padding ^ "🙂");
  F.equal (Buffer.contents stdout) result.stdout
;;

let cancel env root =
  let stdout, _, collect = F.collect () in
  let config = F.config ~secret_filter env root S.Backend.direct in
  let cancelled =
    try
      Eio.Cancel.sub (fun context ->
        let on_progress progress =
          collect progress;
          Eio.Cancel.cancel context Exit
        in
        ignore
          (S.Executor.run_streaming
             config
             (F.child_request env root "cancel")
             ~on_progress
           : (S.Executor.result, S.Executor.error) result));
      false
    with
    | Eio.Cancel.Cancelled _ -> true
  in
  F.check cancelled "Eio cancellation was swallowed";
  F.check (Buffer.length stdout > 0) "cancellation was not triggered by progress";
  F.check
    (not (String.is_substring (Buffer.contents stdout) ~substring:"TO"))
    "cancellation flushed an undecidable suffix"
;;

let limit_failure env root =
  let limits = S.Limits.{ default with max_total_bytes = 100 } in
  let config =
    F.config ~secret_filter ~limits ~audit:(read_ack root) env root S.Backend.direct
  in
  let stdout, _, on_progress = F.collect () in
  let result =
    S.Executor.run_streaming config (F.child_request env root "live") ~on_progress
  in
  F.check
    (match result with
     | Error (Output_limit_exceeded _) -> true
     | _ -> false)
    "raw total limit did not stop streaming";
  F.check (Buffer.length stdout > 0 && Buffer.length stdout <= 100) "bad failure progress";
  F.check
    (not (String.is_substring (Buffer.contents stdout) ~substring:"TO"))
    "limit failure flushed an undecidable suffix"
;;

let pipeline_ack root =
  let seen = String.Hash_set.create () in
  S.Audit.create ~failure_policy:Terminate_runtime (fun envelope ->
    (match envelope.S.Audit.event with
     | Output (_, context, `Stderr, _) ->
       let mode = List.last_exn context.command.arguments in
       Hash_set.add seen mode;
       if Hash_set.length seen = 2
       then (
         F.save root "stderr-a-read";
         F.save root "stderr-b-read")
     | _ -> ());
    Ok ())
;;

let pipeline env root =
  let commands = List.map [ "stderr-a"; "stderr-b" ] ~f:(F.child_command env root) in
  let chain = S.Chain.create ~first:commands ~rest:[] |> Result.ok_or_failwith in
  let config =
    F.config ~secret_filter ~audit:(pipeline_ack root) env root S.Backend.direct
  in
  let stdout, stderr, collect = F.collect () in
  let on_progress progress =
    collect progress;
    Eio.Fiber.yield ()
  in
  ignore
    (S.Executor.run_streaming
       config
       (F.invocation (S.Request.Structured chain))
       ~on_progress
     |> F.executor_ok
     : S.Executor.result);
  F.equal (Buffer.contents stderr) "";
  let text = Buffer.contents stdout in
  F.check (String.is_substring text ~substring:"🙂") "pipeline UTF-8 decoders mixed";
  F.check (not (String.is_substring text ~substring:"TOKEN")) "pipeline secret escaped";
  F.check
    (not (String.is_substring text ~substring:"31m"))
    "pipeline terminal states mixed";
  F.check
    (not (String.is_substring text ~substring:"�"))
    "pipeline decoder corrupted UTF-8"
;;

let observer_error env root =
  let config = F.config ~secret_filter env root echo_backend in
  let request = F.request "TOKEN normal" in
  let expected = S.Executor.run config request |> F.executor_ok in
  let actual =
    S.Executor.run_streaming config request ~on_progress:(fun _ -> failwith "observer")
    |> F.executor_ok
  in
  canonical_equal expected actual
;;

let combined_secret env root =
  List.iter
    [ "ABCDEF", "ABC", "DEF"; "AB\nCD", "AB\n", "CD" ]
    ~f:(fun (secret, stdout, stderr) ->
      let backend =
        S.Backend.fake ~name:"combined-fixture" (fun _ ~stdin:_ ->
          Ok S.Backend.{ status = `Exited 0; stdout; stderr })
      in
      let config =
        F.config ~secret_filter:(S.Secret_filter.create [ secret ]) env root backend
      in
      let combined, diagnostic, on_progress = F.collect () in
      ignore
        (S.Executor.run_streaming config (F.request "ignored") ~on_progress
         |> F.executor_ok
         : S.Executor.result);
      F.equal (Buffer.contents combined) "[REDACTED]";
      F.equal (Buffer.contents diagnostic) "")
;;

let finish_timeout env root =
  let limits = S.Limits.{ default with wall_time_seconds = 0.1 } in
  let config = F.config ~limits ~secret_filter env root echo_backend in
  let called = ref false in
  let on_progress _ =
    called := true;
    Eio.Fiber.await_cancel ()
  in
  let result =
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 2. (fun () ->
      S.Executor.run_streaming config (F.request "TO") ~on_progress)
  in
  F.check !called "final held tail was not observed";
  F.check
    (match result with
     | Error (Timed_out _) -> true
     | _ -> false)
    "final-tail observer escaped invocation timeout"
;;

let run env =
  List.iter
    [ sequential
    ; after_rejection
    ; before_substitute
    ; budget_prefix
    ; live
    ; cancel
    ; limit_failure
    ; pipeline
    ; observer_error
    ; combined_secret
    ; finish_timeout
    ]
    ~f:(fun test -> F.with_root env (test env))
;;
