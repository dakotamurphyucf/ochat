open Core
module B = Shell_access.Bounded_process
module Control = Shell_access__Process_control

external virtual_bytes : unit -> int = "ochat_test_virtual_bytes"
external mapping_available : int -> bool = "ochat_test_mapping_available"

let memory_cap () =
  let inherited = virtual_bytes () in
  let headroom = 256 * 1024 * 1024 in
  assert (inherited <= Int.max_value - headroom);
  inherited + headroom
;;

let executable env =
  Eio.Path.native_exn Eio.Path.(Eio.Stdenv.cwd env / "native_fixture.exe")
  |> Eio_posix.Low_level.realpath
;;

let limits
      ?(request = 2_000_000)
      ?(output = 2_000_000)
      ?(wall = 5.)
      ?(memory = memory_cap ())
      ()
  =
  (* Darwin rejects AS below the already-inherited OCaml parent footprint. This
     test-only cap measures that floor and admits explicit256MiB headroom.
     Production never increases a requested cap. *)
  B.Limits.create
    ~maximum_request_bytes:request
    ~maximum_output_bytes:output
    ~maximum_wall_time:(Time_ns.Span.of_sec wall)
    ~cpu_seconds_per_process:2
    ~memory_bytes_per_process:memory
    ~file_size_bytes:1_048_576
    ~open_files_per_process:64
  |> Or_error.ok_exn
;;

let request env ?(limits = limits ()) ?(stdin = "") mode =
  B.create ~limits ~executable:(executable env) ~arguments:[ mode ] ~environment:[] ~stdin
  |> function
  | Ok request -> request
  | Error error -> raise_s [%message "invalid fixture request" (error : B.Error.t)]
;;

let with_directory_fd path f =
  Eio.Path.with_open_in
    Eio.Path.(path / ".")
    (fun directory ->
       (match (Eio.File.stat directory).kind with
        | `Directory -> ()
        | _ -> failwith "native fixture requires a directory");
       match Eio_unix.Resource.fd_opt directory with
       | Some cwd -> f cwd
       | None -> failwith "native fixture requires a retained directory FD")
;;

let with_cwd f =
  Eio_main.run (fun env -> with_directory_fd (Eio.Stdenv.cwd env) (fun cwd -> f env cwd))
;;

let run env cwd ?(checkpoint = ignore) request =
  B.run request ~cwd ~clock:(Eio.Stdenv.mono_clock env) ~checkpoint
;;

let outcome = function
  | Ok result -> result
  | Error error -> raise_s [%message "invocation failed" (error : B.Error.t)]
;;

let success result =
  match B.Outcome.status result with
  | `Exited 0 -> ()
  | `Exited _ | `Signaled _ -> failwith "expected successful exit"
;;

let%expect_test "exact binary stdout and nonzero status retain no secret stderr" =
  with_cwd (fun env cwd ->
    let result = run env cwd (request env "mixed") |> outcome in
    printf
      "stdout=%S stderr-bytes=%d status=%s\n"
      (B.Outcome.stdout result)
      (B.Outcome.stderr_bytes result)
      (match B.Outcome.status result with
       | `Exited code -> sprintf "exit %d" code
       | `Signaled _ -> "signal"));
  [%expect {| stdout="abc\000d" stderr-bytes=6 status=exit 7 |}]
;;

let%expect_test "leader exit stops its live helper and completes buffered output" =
  with_cwd (fun env cwd ->
    let result = run env cwd (request env "held-pipe") |> outcome in
    success result;
    print_string (B.Outcome.stdout result));
  [%expect
    {|
    helper-ready
    leader-exit
    |}]
;;

let%test_unit "output admission follows independent combined byte count" =
  with_cwd (fun env cwd ->
    List.iter [ 0; 4; 5; 6; 10; 11; 12 ] ~f:(fun cap ->
      let actual = run env cwd (request env ~limits:(limits ~output:cap ()) "mixed") in
      if cap < String.length "abc\000d" + String.length "secret"
      then
        assert (Result.equal (fun _ _ -> false) B.Error.equal actual (Error Output_limit))
      else (
        let result = outcome actual in
        assert (String.equal (B.Outcome.stdout result) "abc\000d");
        assert (B.Outcome.stderr_bytes result = 6)));
    assert (
      Result.is_error (run env cwd (request env ~limits:(limits ~output:7 ()) "flood"))))
;;

let%test_unit "request bytes and wire validation are independent of execution" =
  Eio_main.run (fun env ->
    let executable = executable env in
    let arguments = [ "a"; "" ]
    and environment = [ "K", "v" ]
    and stdin = "\000!" in
    let bytes =
      (2 * (String.length executable + 1))
      + List.sum (module Int) arguments ~f:(fun s -> String.length s + 1)
      + List.sum
          (module Int)
          environment
          ~f:(fun (k, v) -> String.length k + String.length v + 2)
      + String.length stdin
    in
    List.iter
      [ bytes - 1; bytes; bytes + 1 ]
      ~f:(fun cap ->
        let actual =
          B.create
            ~limits:(limits ~request:cap ())
            ~executable
            ~arguments
            ~environment
            ~stdin
        in
        assert (Bool.equal (Result.is_ok actual) (cap >= bytes)));
    List.iter
      [ [ "K", "a"; "K", "b" ]; [ "", "v" ]; [ "K=V", "v" ]; [ "K", "\000" ] ]
      ~f:(fun environment ->
        assert (
          Result.is_error
            (B.create
               ~limits:(limits ())
               ~executable
               ~arguments:[]
               ~environment
               ~stdin:"")));
    assert (
      Result.is_error
        (B.create
           ~limits:(limits ())
           ~executable:"relative"
           ~arguments:[]
           ~environment:[]
           ~stdin:"")))
;;

let%test_unit "unconsumed stdin does not prevent exact successful output" =
  with_cwd (fun env cwd ->
    let result =
      run env cwd (request env ~stdin:(String.make 1_000_000 'i') "reject-input")
      |> outcome
    in
    success result;
    assert (String.equal (B.Outcome.stdout result) "whole"))
;;

exception Injected of int

let%test_unit "native address-space limit prevents additional mappings" =
  with_cwd (fun env cwd ->
    let memory = memory_cap () in
    assert (mapping_available memory);
    let result =
      run env cwd (request env ~limits:(limits ~memory ()) "memory-bound") |> outcome
    in
    success result;
    assert (String.equal (B.Outcome.stdout result) "address-space-bound\n"))
;;

let%test_unit "every observed checkpoint preserves original identity and reuse" =
  with_cwd (fun env cwd ->
    let invocation = request env "mixed" in
    let observed = ref 0 in
    ignore
      (run env cwd invocation ~checkpoint:(fun () -> incr observed) |> outcome
       : B.Outcome.t);
    assert (!observed > 0);
    for interrupt = 1 to !observed do
      let original = Injected interrupt
      and calls = ref 0 in
      let actual =
        try
          ignore
            (run env cwd invocation ~checkpoint:(fun () ->
               incr calls;
               if !calls = interrupt then raise original)
             : (B.Outcome.t, B.Error.t) Result.t);
          None
        with
        | exn -> Some exn
      in
      assert (Option.exists actual ~f:(fun exn -> phys_equal exn original));
      assert (!calls >= interrupt);
      ignore (run env cwd invocation |> outcome : B.Outcome.t)
    done)
;;

let%test_unit "operational deadline joins blocked input and allows reuse" =
  with_cwd (fun env cwd ->
    let actual =
      run
        env
        cwd
        (request
           env
           ~limits:(limits ~wall:0.02 ())
           ~stdin:(String.make 1_000_000 'i')
           "wait")
    in
    assert (Result.equal (fun _ _ -> false) B.Error.equal actual (Error Timed_out));
    let result = run env cwd (request env ~stdin:"after" "echo") |> outcome in
    success result;
    assert (String.equal (B.Outcome.stdout result) "after"))
;;

let%expect_test "child-only group limits and descriptor closure" =
  with_cwd (fun env cwd ->
    let memory = memory_cap () in
    let result =
      run env cwd (request env ~limits:(limits ~memory ()) "limits") |> outcome
    in
    success result;
    let fields = String.split (B.Outcome.stdout result) ~on:' ' in
    assert (List.mem fields (sprintf "memory=%d" memory) ~equal:String.equal);
    print_string
      (String.substr_replace_all
         (B.Outcome.stdout result)
         ~pattern:(sprintf "memory=%d" memory)
         ~with_:"memory=admitted-finite-cap"));
  [%expect
    {| group=1 cpu=2 memory=admitted-finite-cap file=1048576 nofile=64 core=0 fd3=0 fd4=0 |}]
;;

let%test_unit "executable signal status is distinct from anchor termination" =
  with_cwd (fun env cwd ->
    let result = run env cwd (request env "signal") |> outcome in
    match B.Outcome.status result with
    | `Signaled signal -> assert (signal = Stdlib.Sys.sigterm)
    | `Exited _ -> assert false)
;;

let%test_unit "unexpected anchor death fails closed and allows subsequent reuse" =
  with_cwd (fun env cwd ->
    let actual = run env cwd (request env "kill-anchor") in
    assert (Result.equal (fun _ _ -> false) B.Error.equal actual (Error Process_failed));
    let result = run env cwd (request env ~stdin:"after" "echo") |> outcome in
    success result;
    assert (String.equal (B.Outcome.stdout result) "after"))
;;

let%test_unit "private protocol validates handwritten wire frames" =
  let ready = "OCHG\001\001\000\000\000\000\000\000\000\000\000\000" in
  let exited = "OCHG\001\002\000\000\000\000\000\007\000\000\000\000" in
  let signaled = "OCHG\001\003\000\000\000\000\000\015\000\000\000\000" in
  let failed = "OCHG\001\004\000\000\000\000\000\000\000\000\000\000" in
  List.iter
    [ ready, Control.Ready; exited, Exited 7; signaled, Signaled 15; failed, Failed ]
    ~f:(fun (bytes, expected) ->
      assert (Option.equal Control.equal (Control.decode bytes) (Some expected)));
  let replace bytes offset byte =
    String.mapi bytes ~f:(fun i old -> if i = offset then byte else old)
  in
  let invalid =
    List.map [ 0; 4; 6; 7; 12; 13; 14; 15 ] ~f:(fun i -> replace exited i '\255')
    @ [ String.drop_suffix exited 1
      ; exited ^ "\000"
      ; replace exited 5 '\255'
      ; replace exited 10 '\001'
      ; replace ready 11 '\001'
      ; replace signaled 11 '\000'
      ; replace signaled 11 '\128'
      ]
  in
  assert (List.length invalid > 0);
  List.iter invalid ~f:(fun bytes -> assert (Option.is_none (Control.decode bytes)))
;;

let with_directory f =
  Eio_main.run (fun env ->
    let temporary =
      Eio_unix.run_in_systhread (fun () -> Core_unix.mkdtemp "/tmp/och199-XXXXXX")
    in
    let path = Eio.Path.(Eio.Stdenv.fs env / temporary) in
    Exn.protect ~f:(fun () -> f env path) ~finally:(fun () -> Eio.Path.rmtree path))
;;

let%expect_test "retained cwd survives rename and replacement of its pathname" =
  with_directory (fun env path ->
    let original = Eio.Path.(path / "original") in
    let retained = Eio.Path.(path / "retained") in
    Eio.Path.mkdir ~perm:0o700 original;
    Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(original / "marker") "original";
    with_directory_fd original (fun cwd ->
      Eio.Path.rename original retained;
      Eio.Path.mkdir ~perm:0o700 original;
      Eio.Path.save
        ~create:(`Exclusive 0o600)
        Eio.Path.(original / "marker")
        "replacement";
      let result = run env cwd (request env "cwd") |> outcome in
      success result;
      printf "captured directory: %s\n" (B.Outcome.stdout result)));
  [%expect {| captured directory: original |}]
;;

let%test_unit "parent groups limits and unrelated child ownership are preserved" =
  let parent_group = Core_unix.getpgid (Core_unix.getpid ()) in
  let resources =
    Core_unix.RLimit.[ cpu_seconds; core_file_size; file_size; num_file_descriptors ]
  in
  let snapshot () =
    List.map resources ~f:(fun resource ->
      Core_unix.RLimit.get resource |> Core_unix.RLimit.sexp_of_t)
  in
  let before = snapshot () in
  with_cwd (fun env cwd ->
    Eio.Path.with_open_out
      ~create:`Never
      Eio.Path.(Eio.Stdenv.fs env / "/dev/null")
      (fun discard ->
         let discard_fd =
           match Eio_unix.Resource.fd_opt discard with
           | Some fd -> fd
           | None -> failwith "native child ownership fixture requires a native FD"
         in
         (* A direct-PID sentinel makes accidental unrelated reaping observable.
         Ordinary filesystem setup uses Eio; these calls test native ownership. *)
         let input, control = Caml_unix.pipe ~cloexec:true () in
         let unrelated =
           Eio_unix.Fd.use_exn "unrelated child" discard_fd (fun fd ->
             Caml_unix.create_process "/bin/cat" [| "/bin/cat" |] input fd fd)
         in
         Caml_unix.close input;
         let rec wait () =
           match Caml_unix.waitpid [] unrelated with
           | result -> result
           | exception Caml_unix.Unix_error (EINTR, _, _) -> wait ()
         in
         let observed = ref None in
         Exn.protect
           ~f:(fun () -> success (run env cwd (request env "limits") |> outcome))
           ~finally:(fun () ->
             Caml_unix.close control;
             observed := Some (wait ()));
         let pid, status = Option.value_exn !observed in
         assert (pid = unrelated);
         match status with
         | WEXITED 0 -> ()
         | WEXITED _ | WSIGNALED _ | WSTOPPED _ -> assert false));
  assert (Option.equal Pid.equal parent_group (Core_unix.getpgid (Core_unix.getpid ())));
  assert (List.equal Sexp.equal before (snapshot ()))
;;

exception External_cancel

let%test_unit "external cancellation joins a synchronized blocked invocation" =
  with_directory (fun env path ->
    with_directory_fd path (fun cwd ->
      let cancelled = ref false in
      let original = External_cancel in
      let actual =
        try
          Eio.Fiber.first
            (fun () ->
               try
                 ignore
                   (run
                      env
                      cwd
                      (request env ~stdin:(String.make 1_000_000 'i') "wait-ready")
                    : (B.Outcome.t, B.Error.t) Result.t)
               with
               | Eio.Cancel.Cancelled _ as exn ->
                 cancelled := true;
                 raise exn)
            (fun () ->
               Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
                 let rec ready () =
                   if not (Eio.Path.is_file Eio.Path.(path / "ready"))
                   then (
                     Eio.Time.sleep (Eio.Stdenv.clock env) 0.001;
                     ready ())
                 in
                 ready ());
               raise original);
          None
        with
        | exn -> Some exn
      in
      assert !cancelled;
      assert (Option.exists actual ~f:(fun exn -> phys_equal original exn));
      let result = run env cwd (request env ~stdin:"reusable" "echo") |> outcome in
      success result;
      assert (String.equal (B.Outcome.stdout result) "reusable")))
;;
