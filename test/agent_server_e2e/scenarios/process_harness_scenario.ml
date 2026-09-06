open Core
module Port_reservation = Support.Port_reservation
module Process_manager = Support.Process_manager
module Temporary_environment = Support.Temporary_environment
module Stdio_process = Support.Stdio_process

let fail message = raise_s [%sexp "E2E assertion failed", (message : string)]
let require condition message = if not condition then fail message
let executable () = Sys_unix.executable_name
let child_argv behavior = [ executable (); "--child-behavior"; behavior ]

let child_environment environment =
  Temporary_environment.child_environment environment ~base:(Core_unix.environment ())
;;

let spawn ~sw ~env environment ?(max_output_bytes = 4096) behavior =
  Process_manager.spawn
    ~sw
    ~env
    ~environment:(child_environment environment)
    ~max_output_bytes
    (child_argv behavior)
;;

let require_exit actual expected message =
  if not (Process_manager.equal_exit actual expected)
  then
    raise_s
      [%sexp
        "E2E exit assertion failed"
      , { message : string
        ; expected : Process_manager.exit
        ; actual : Process_manager.exit
        }]
;;

let await_ready env child =
  Process_manager.wait_for_stdout
    child
    ~clock:(Eio.Stdenv.clock env)
    ~timeout_seconds:1.
    ~ready:(String.is_substring ~substring:"READY")
  |> function
  | Ok () -> ()
  | Error error ->
    raise_s
      [%sexp "child did not become ready", (error : Process_manager.readiness_error)]
;;

let test_normal_exit env environment =
  Eio.Switch.run (fun sw ->
    let normal = spawn ~sw ~env environment "normal-exit" in
    let normal_result = Process_manager.await normal in
    require_exit
      normal_result.exit
      (Process_manager.Exited 0)
      "normal child did not exit zero";
    require
      (String.equal normal_result.stdout.contents "normal-stdout\n")
      "normal stdout differs";
    let unexpected = spawn ~sw ~env environment "unexpected-exit" in
    let unexpected_result = Process_manager.await unexpected in
    require_exit
      unexpected_result.exit
      (Process_manager.Exited 23)
      "unexpected child exit was not classified")
;;

let test_sigterm env environment =
  Eio.Switch.run (fun sw ->
    let child = spawn ~sw ~env environment "term-exit" in
    await_ready env child;
    Process_manager.signal child Stdlib.Sys.sigterm;
    let result = Process_manager.await child in
    require_exit
      result.exit
      (Process_manager.Exited 0)
      "SIGTERM child did not exit gracefully";
    require
      (String.is_substring result.stderr.contents ~substring:"TERM")
      "SIGTERM child did not report graceful handling")
;;

let test_forced_termination env environment =
  Eio.Switch.run (fun sw ->
    let child = spawn ~sw ~env environment "ignore-term" in
    await_ready env child;
    let termination =
      Process_manager.terminate child ~clock:(Eio.Stdenv.clock env) ~grace_seconds:0.05
    in
    require termination.forced "SIGTERM-ignoring child was not forcibly terminated";
    require_exit
      termination.result.exit
      (Process_manager.Signaled Stdlib.Sys.sigkill)
      "forced child exit was not classified as SIGKILL")
;;

let cancel_owner_waiter child =
  let cancelled = Failure "cancelled owner switch" in
  (try
     Eio.Switch.run (fun sw ->
       Eio.Fiber.fork ~sw (fun () ->
         ignore (Process_manager.await child : Process_manager.result));
       Eio.Fiber.yield ();
       Eio.Switch.fail sw cancelled)
   with
   | Failure message when String.equal message "cancelled owner switch" -> ());
  ()
;;

let test_cancelled_waiter_switch env environment =
  Eio.Switch.run (fun supervisor_sw ->
    let child = spawn ~sw:supervisor_sw ~env environment "quiet-block" in
    cancel_owner_waiter child;
    let termination =
      Process_manager.terminate child ~clock:(Eio.Stdenv.clock env) ~grace_seconds:0.1
    in
    require (not termination.forced) "cancelled waiter poisoned process supervision";
    require_exit
      termination.result.exit
      (Process_manager.Signaled Stdlib.Sys.sigterm)
      "supervisor did not reap child after owner cancellation")
;;

let test_cancelled_owner_switch env environment =
  let child = ref None in
  let outcome =
    Result.try_with (fun () ->
      Eio.Switch.run (fun sw ->
        let process = spawn ~sw ~env environment "quiet-block" in
        child := Some process;
        Eio.Switch.fail sw (Failure "cancel actual process owner")))
  in
  require (Result.is_error outcome) "owner cancellation did not propagate";
  require
    (Poly.equal
       (Eio_unix.run_in_systhread (fun () ->
          Signal_unix.send
            Signal.zero
            (`Pid (Pid.of_int (Process_manager.pid (Option.value_exn !child))))))
       `No_such_process)
    "cancelled process owner's child remains alive"
;;

let test_reader_drain env _environment =
  Eio.Switch.run (fun sw ->
    let manager = Eio.Stdenv.process_mgr env in
    let source, sink = Eio.Process.pipe ~sw manager in
    let reader =
      Support.Process_reader.start ~sw (fun () ->
        ignore (Eio.Flow.single_read source (Cstruct.create 1) : int))
    in
    Support.Process_reader.drain ~clock:(Eio.Stdenv.clock env) [ reader ];
    require (Support.Process_reader.finished reader) "pipe drainage did not finish";
    require
      (Support.Process_reader.interrupted reader)
      "retained pipe was not marked incomplete";
    Eio.Flow.close sink)
;;

let test_stdio_bound behavior expected env environment =
  Eio.Switch.run (fun sw ->
    let child =
      Stdio_process.spawn
        ~sw
        ~env
        ~environment:(child_environment environment)
        ~max_output_bytes:128
        (child_argv behavior)
    in
    let result =
      Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 3. (fun () ->
        Stdio_process.await child)
    in
    require_exit result.exit (Process_manager.Exited 0) "stdout pressure child failed";
    require
      (String.equal result.stderr.contents "DRAINED\n")
      "stdout pressure blocked child completion";
    match
      Stdio_process.next_stdout child ~clock:(Eio.Stdenv.clock env) ~timeout_seconds:0.1
    with
    | `Item (Read_error message) ->
      require (String.is_substring message ~substring:expected) "wrong stdout bound error"
    | _ -> fail "stdout limit did not report an explicit failure")
;;

let test_stdio_forced_overflow env environment =
  Eio.Switch.run (fun sw ->
    let child =
      Stdio_process.spawn
        ~sw
        ~env
        ~environment:(child_environment environment)
        ~max_output_bytes:128
        (child_argv "stdio-flood-block")
    in
    let rec ready () =
      if
        not
          (String.is_substring (Stdio_process.stderr child).contents ~substring:"DRAINED")
      then (
        Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
        ready ())
    in
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 3. ready;
    let stopped =
      Stdio_process.terminate child ~clock:(Eio.Stdenv.clock env) ~grace_seconds:0.05
    in
    require stopped.forced "flooding child did not require SIGKILL";
    require_exit
      stopped.result.exit
      (Signaled Stdlib.Sys.sigkill)
      "flooding child was not reaped")
;;

let send_signal pid signal =
  Eio_unix.run_in_systhread (fun () -> Signal_unix.send signal (`Pid (Pid.of_int pid)))
;;

let await_reaped env pid =
  let rec wait () =
    match send_signal pid Signal.zero with
    | `No_such_process -> ()
    | `Ok ->
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
      wait ()
  in
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 3. wait
;;

let retained_parent ~stdio ~sw ~env environment =
  if stdio
  then (
    let child =
      Stdio_process.spawn
        ~sw
        ~env
        ~environment:(child_environment environment)
        ~max_output_bytes:128
        (child_argv "retained-pipe-parent")
    in
    ( Stdio_process.pid child
    , (fun () -> Stdio_process.poll_result child)
    , fun () -> Stdio_process.await child ))
  else (
    let child = spawn ~sw ~env environment "retained-pipe-parent" in
    ( Process_manager.pid child
    , (fun () -> Process_manager.poll_result child)
    , fun () -> Process_manager.await child ))
;;

let test_retained_pipe stdio env environment =
  Temporary_environment.with_ ~env (fun nested ->
    let pid_file =
      Temporary_environment.path
        nested
        (Filename.concat (Temporary_environment.roots nested).root "pipe-keeper.pid")
    in
    Eio.Switch.run (fun sw ->
      let pid, poll, await = retained_parent ~stdio ~sw ~env nested in
      Exn.protect
        ~f:(fun () ->
          await_reaped env pid;
          require
            (Option.is_none (Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 0.05 poll))
            "poll waited for or concealed retained output pipes";
          let result = Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 1.5 await in
          require_exit result.exit (Exited 0) "retained-pipe parent failed";
          require
            (result.stdout.truncated && result.stderr.truncated)
            "incomplete pipes were reported complete")
        ~finally:(fun () ->
          Eio.Cancel.protect (fun () ->
            if Eio.Path.is_file pid_file
            then (
              let keeper = Eio.Path.load pid_file |> Int.of_string in
              ignore (send_signal keeper Signal.kill : [ `Ok | `No_such_process ]);
              await_reaped env keeper)))));
  ignore (environment : Temporary_environment.t)
;;

let diagnostic_secret = "private-harness-diagnostic-credential"

let test_failure_diagnostic behavior env environment =
  Eio.Switch.run (fun sw ->
    let child = spawn ~sw ~env environment ~max_output_bytes:32768 behavior in
    let result = Process_manager.await child in
    require
      (not (Process_manager.equal_exit result.exit (Exited 0)))
      "failure fixture exited successfully";
    let diagnostic = result.stdout.contents ^ result.stderr.contents in
    List.iter
      [ diagnostic_secret; Base64.encode_exn diagnostic_secret ]
      ~f:(fun secret ->
        require
          (not (String.is_substring diagnostic ~substring:secret))
          "child stderr disclosed a registered secret");
    require
      (String.is_substring diagnostic ~substring:"<redacted>")
      "failure diagnostic has no redaction marker")
;;

let test_pipe_separation env environment =
  Eio.Switch.run (fun sw ->
    let child = spawn ~sw ~env environment ~max_output_bytes:16 "separate-output" in
    let result = Process_manager.await child in
    require_exit result.exit (Process_manager.Exited 0) "separate-output child failed";
    require
      (String.is_prefix result.stdout.contents ~prefix:"stdout-")
      "stdout was not captured independently";
    require
      (String.is_prefix result.stderr.contents ~prefix:"stderr-")
      "stderr was not captured independently";
    require result.stdout.truncated "bounded stdout did not report truncation";
    require result.stderr.truncated "bounded stderr did not report truncation")
;;

let test_port_reservation env _environment =
  Eio.Switch.run (fun sw ->
    let reservations = List.init 8 ~f:(fun _ -> Port_reservation.create ~sw ~env) in
    let ports = List.map reservations ~f:Port_reservation.port in
    let unique_ports = List.dedup_and_sort ports ~compare:Int.compare in
    require (Int.equal (List.length ports) (List.length unique_ports)) "ports overlapped";
    List.iter reservations ~f:Port_reservation.release)
;;

let socket_paths environment =
  let roots : Temporary_environment.roots = Temporary_environment.roots environment in
  [ "a.sock"; "b.sock" ] |> List.map ~f:(Filename.concat roots.sockets)
;;

let test_socket_paths env environment =
  let paths = socket_paths environment in
  Eio.Switch.run (fun sw ->
    List.iter paths ~f:(fun path ->
      ignore
        (Eio.Net.listen ~sw ~reuse_addr:false ~backlog:1 (Eio.Stdenv.net env) (`Unix path)
         : _ Eio.Net.listening_socket));
    List.iter paths ~f:(fun path ->
      require
        (Poly.equal
           (Eio.Path.kind ~follow:false (Temporary_environment.path environment path))
           `Socket)
        "private Unix listener path was not created"));
  List.iter paths ~f:(fun path ->
    require
      (Poly.equal
         (Eio.Path.kind ~follow:false (Temporary_environment.path environment path))
         `Not_found)
      "Unix listener path survived switch cleanup")
;;

let test_readiness_timeout env environment =
  Eio.Switch.run (fun sw ->
    let child = spawn ~sw ~env environment "quiet-block" in
    let readiness =
      Process_manager.wait_for_stdout
        child
        ~clock:(Eio.Stdenv.clock env)
        ~timeout_seconds:0.05
        ~ready:(fun _ -> false)
    in
    require
      (match readiness with
       | Error Process_manager.Timeout -> true
       | Ok () | Error (Process_manager.Exited_before_ready _) -> false)
      "readiness deadline was not classified as a timeout";
    Process_manager.signal child Stdlib.Sys.sigkill;
    ignore (Process_manager.await child : Process_manager.result))
;;

let cases =
  [ "process.normal-exit", test_normal_exit
  ; "process.sigterm", test_sigterm
  ; "process.forced-termination", test_forced_termination
  ; "process.cancelled-owner-switch", test_cancelled_owner_switch
  ; "process.cancelled-waiter-switch", test_cancelled_waiter_switch
  ; "pipe.bounded-drain", test_reader_drain
  ; "stdio.queue-overflow", test_stdio_bound "stdio-flood" "queue exceeded"
  ; "stdio.unterminated-line", test_stdio_bound "stdio-long-line" "line exceeded"
  ; "stdio.forced-overflow", test_stdio_forced_overflow
  ; "pipe.retained-descendant", test_retained_pipe false
  ; "stdio.retained-descendant", test_retained_pipe true
  ; "diagnostic.redaction", test_failure_diagnostic "secret-failure"
  ; ( "diagnostic.retention-failure-redaction"
    , test_failure_diagnostic "secret-retention-failure" )
  ; "pipe.stdout-stderr-separation", test_pipe_separation
  ; "port.concurrent-reservation", test_port_reservation
  ; "socket.concurrent-paths", test_socket_paths
  ; "deadline.readiness-timeout", test_readiness_timeout
  ]
;;

let select case =
  match case with
  | None -> cases
  | Some name ->
    (match List.Assoc.find cases name ~equal:String.equal with
     | Some test -> [ name, test ]
     | None -> raise_s [%sexp "unknown process-harness case", (name : string)])
;;

let run env ~case =
  Temporary_environment.with_ ~scenario:"process-harness" ~env (fun environment ->
    List.iter (select case) ~f:(fun (_name, test) -> test env environment);
    print_s
      [%sexp
        { scenario = ("process-harness" : string)
        ; selected_case = (case : string option)
        ; process_reaping = (true : bool)
        ; bounded_capture = (true : bool)
        ; resource_release = (true : bool)
        }])
;;

let rec wait_for_flag env flag =
  if Atomic.get flag
  then ()
  else (
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
    wait_for_flag env flag)
;;

let block env =
  let rec loop () =
    Eio.Time.sleep (Eio.Stdenv.clock env) 60.;
    loop ()
  in
  loop ()
;;

let run_child env = function
  | "retained-pipe-parent" ->
    Eio.Switch.run (fun sw ->
      let child =
        Eio.Process.spawn
          ~sw
          (Eio.Stdenv.process_mgr env)
          ~stdout:(Eio.Stdenv.stdout env)
          ~stderr:(Eio.Stdenv.stderr env)
          (child_argv "quiet-block")
      in
      let pid_file =
        Filename.concat (Sys.getenv_exn "OCHAT_E2E_ROOT") "pipe-keeper.pid"
      in
      Eio.Path.save
        ~create:(`Exclusive 0o600)
        Eio.Path.(Eio.Stdenv.fs env / pid_file)
        (Int.to_string (Eio.Process.pid child));
      Stdlib.exit 0)
  | ("stdio-flood" | "stdio-flood-block") as behavior ->
    if String.equal behavior "stdio-flood-block"
    then Signal.Expert.handle Signal.term (fun _ -> ());
    for _index = 0 to 2048 do
      Eio.Flow.copy_string
        "{\"jsonrpc\":\"2.0\",\"method\":\"fixture\",\"params\":{}}\n"
        (Eio.Stdenv.stdout env)
    done;
    Eio.Flow.copy_string "DRAINED\n" (Eio.Stdenv.stderr env);
    if String.equal behavior "stdio-flood-block" then block env
  | "stdio-long-line" ->
    Eio.Flow.copy_string (String.make (2 * 1_048_576) 'x') (Eio.Stdenv.stdout env);
    Eio.Flow.copy_string "DRAINED\n" (Eio.Stdenv.stderr env)
  | ("secret-failure" | "secret-retention-failure") as behavior ->
    let root = Sys.getenv_exn "OCHAT_E2E_ROOT" in
    let failure_artifact_root = Filename.concat root behavior in
    if String.equal behavior "secret-retention-failure"
    then
      Eio.Path.save
        ~create:(`Exclusive 0o600)
        Eio.Path.(Eio.Stdenv.fs env / failure_artifact_root)
        "not a directory";
    Temporary_environment.with_ ~env ~failure_artifact_root (fun temporary ->
      Temporary_environment.register_secret temporary diagnostic_secret;
      failwith (diagnostic_secret ^ " " ^ Base64.encode_exn diagnostic_secret))
  | "normal-exit" ->
    Eio.Flow.copy_string "normal-stdout\n" (Eio.Stdenv.stdout env);
    Eio.Flow.copy_string "normal-stderr\n" (Eio.Stdenv.stderr env)
  | "unexpected-exit" -> Stdlib.exit 23
  | "term-exit" ->
    let terminated = Atomic.make false in
    Signal.Expert.handle Signal.term (fun _ -> Atomic.set terminated true);
    Eio.Flow.copy_string "READY\n" (Eio.Stdenv.stdout env);
    wait_for_flag env terminated;
    Eio.Flow.copy_string "TERM\n" (Eio.Stdenv.stderr env)
  | "ignore-term" ->
    Signal.Expert.handle Signal.term (fun _ -> ());
    Eio.Flow.copy_string "READY\n" (Eio.Stdenv.stdout env);
    block env
  | "quiet-block" -> block env
  | "separate-output" ->
    Eio.Flow.copy_string ("stdout-" ^ String.make 64 'x') (Eio.Stdenv.stdout env);
    Eio.Flow.close (Eio.Stdenv.stdout env);
    Eio.Flow.copy_string ("stderr-" ^ String.make 64 'y') (Eio.Stdenv.stderr env)
  | behavior -> raise_s [%sexp "unknown fixture-child behavior", (behavior : string)]
;;
