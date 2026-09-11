open Core
module S = Shell_access

let run env helper probe runner =
  let root =
    Core_unix.mkdtemp "/tmp/ochat-request-channel.XXXXXX" |> Caml_unix.realpath
  in
  let path file = Eio.Path.(Eio.Stdenv.fs env / file) in
  Exn.protect
    ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true (path root))
    ~f:(fun () ->
      let public = Filename.concat root "public" in
      Eio.Path.mkdir ~perm:0o700 (path public);
      let helper_path = Filename.concat public "helper" in
      let probe_path = Filename.concat public "probe" in
      let runner_path = Filename.concat public "runner" in
      Eio.Path.save
        ~create:(`Exclusive 0o700)
        (path helper_path)
        (Eio.Path.load (path helper));
      Eio.Path.save
        ~create:(`Exclusive 0o700)
        (path probe_path)
        (Eio.Path.load (path probe));
      Eio.Path.save
        ~create:(`Exclusive 0o700)
        (path runner_path)
        (Eio.Path.load (path runner));
      let resolver = S.Resolver.create ~trusted_roots:[ public ] () in
      let calls = ref 0 in
      let authorized = ref true in
      let active_pid = ref None in
      let log = ref [] in
      let audit =
        S.Audit.create ~failure_policy:Deny_start (fun event ->
          (match event.S.Audit.event with
           | Started (_, _, pid) -> active_pid := pid
           | _ -> ());
          log := event :: !log;
          Ok ())
      in
      let capabilities =
        { (S.Capabilities.read_only ~roots:[ public ]) with
          allow_child_processes = true
        ; allow_arbitrary_code = true
        ; network = false
        ; sandbox = Required
        }
      in
      let config ?(capabilities = capabilities) ?backends () =
        S.Executor.config
          ~env
          ~runtime_id:"request-channel"
          ~manifest_sha256:"channel-fixture"
          ~policy:(S.Policy.create ~default:Allow [])
          ~capabilities
          ~resolver
          ?backends
          ~cwd:(path public)
          ~process_env:[||]
          ~audit
          ~resource_runner:runner_path
          ~limits:
            { S.Limits.default with wall_time_seconds = 30.; idle_time_seconds = None }
          ()
      in
      let channel ?(limits = S.Request_channel.default_limits) handle =
        S.Request_channel.create
          ~limits
          ~check:(fun () -> !authorized)
          ~handle:(fun request ->
            Int.incr calls;
            handle request)
        |> Result.ok_or_failwith
      in
      let attach config channel =
        S.Executor.with_request_channel config ~channel ~authorize:(fun context ->
          match
            List.mem
              [ helper_path; probe_path ]
              context.S.Context.executable.canonical_path
              ~equal:String.equal
            && List.equal String.equal context.capabilities.read_roots [ public ]
            && List.is_empty context.capabilities.write_roots
          with
          | true -> Ok ()
          | false -> Error "unexpected helper binding")
        |> Result.ok_or_failwith
      in
      let invoke ?(program = helper_path) ?(arguments = []) config request =
        active_pid := None;
        S.Executor.run
          config
          { request = S.Request.command (S.Command.create program arguments)
          ; input = S.Input.Text request
          ; rationale = None
          ; origin = S.Context.Host "request-channel-test"
          }
      in
      let success = function
        | Ok result ->
          (match result.S.Executor.status with
           | `Exited 0 -> result
           | `Exited code -> failwith (sprintf "helper exit %d: %s" code result.stderr)
           | `Signaled signal ->
             failwith (sprintf "helper signal %d: %s" signal result.stderr))
        | Error error -> failwith (S.Executor.error_to_string error)
      in
      let echo = channel Fn.id in
      let result =
        invoke (attach (config ()) echo) "{\n \"value\": \"hello\"\n}" |> success
      in
      [%test_eq: string] "{\"value\":\"hello\"}\n" result.stdout;
      [%test_eq: string] "" result.stderr;
      [%test_eq: int] 1 !calls;
      let inherited =
        S.Executor.with_execution_scope
          (attach (config ()) echo)
          ~session_id:"another-caller"
          ~approval_store:(S.Approval.create_store ())
          ~check:(fun () -> Ok ())
      in
      (match invoke inherited "{}" with
       | Ok { status = `Exited 1; _ } -> ()
       | _ -> failwith "a new execution scope inherited another caller's channel");
      [%test_eq: int] 1 !calls;
      List.iter
        [ config ~backends:[ S.Backend.direct ] ()
        ; config ~capabilities:{ capabilities with network = true } ()
        ; config
            ~backends:
              [ S.Backend.fake ~name:"fake" (fun _ ~stdin:_ ->
                  failwith "simulated channel ran")
              ]
            ()
        ]
        ~f:(fun config ->
          match invoke (attach config echo) "{}" with
          | Error (Sandbox_unavailable _) -> ()
          | _ -> failwith "unsafe channel backend accepted");
      [%test_eq: int] 1 !calls;
      let limits =
        { S.Request_channel.default_limits with
          max_request_bytes = 32
        ; max_response_bytes = 32
        }
      in
      (match
         invoke
           (attach (config ()) (channel ~limits Fn.id))
           (Jsonaf.to_string (`String (String.make 64 'x')))
       with
       | Error (Denied _) -> ()
       | _ -> failwith "oversized channel request accepted");
      [%test_eq: int] 1 !calls;
      (match
         invoke (attach (config ()) (channel ~limits (fun _ -> String.make 33 'x'))) "{}"
       with
       | Error (Denied _) -> ()
       | _ -> failwith "oversized channel response accepted");
      let revoked =
        channel (fun _ ->
          authorized := false;
          "\"PRIVATE_RESPONSE\"")
      in
      (match invoke (attach (config ()) revoked) "{}" with
       | Error (Denied _) -> ()
       | _ -> failwith "revoked channel disclosed response");
      authorized := true;
      let handler_exited = ref false in
      let entered, enter = Eio.Promise.create () in
      let cancellation, cancel_ready = Eio.Promise.create () in
      let blocked =
        channel (fun _ ->
          Exn.protect
            ~finally:(fun () -> handler_exited := true)
            ~f:(fun () ->
              Eio.Promise.resolve enter ();
              Eio.Fiber.await_cancel ()))
      in
      Eio.Fiber.both
        (fun () ->
           try
             Eio.Cancel.sub (fun context ->
               Eio.Promise.resolve cancel_ready context;
               ignore (invoke (attach (config ()) blocked) "{}");
               failwith "blocked channel ignored caller cancellation")
           with
           | Eio.Cancel.Cancelled _ -> ())
        (fun () ->
           Eio.Promise.await entered;
           Eio.Cancel.cancel (Eio.Promise.await cancellation) Exit);
      assert !handler_exited;
      let pid = Option.value_exn !active_pid in
      (match Caml_unix.kill pid 0 with
       | exception Caml_unix.Unix_error (ESRCH, _, _) -> ()
       | _ -> failwith "cancelled helper was not reaped");
      handler_exited := false;
      let exits_during_call =
        channel (fun _ ->
          Exn.protect
            ~finally:(fun () -> handler_exited := true)
            ~f:(fun () ->
              Caml_unix.kill (Option.value_exn !active_pid) Stdlib.Sys.sigterm;
              Eio.Fiber.await_cancel ()))
      in
      (match invoke (attach (config ()) exits_during_call) "{}" with
       | Ok { status = `Signaled _; _ } -> ()
       | Error error -> failwith (S.Executor.error_to_string error)
       | _ -> failwith "helper exit did not release its channel");
      assert !handler_exited;
      let packet =
        Jsonaf.to_string
          (`String (String.concat (List.init 3000 ~f:(fun _ -> "📚\"\\\n"))))
      in
      let large = invoke (attach (config ()) echo) packet |> success in
      [%test_eq: string] (packet ^ "\n") large.stdout;
      let secret = Filename.concat root "private-control-token" in
      let socket_path = Filename.concat root "control.sock" in
      Eio.Path.save ~create:(`Exclusive 0o600) (path secret) "fixture-private-token";
      let secret_fd = Core_unix.openfile secret ~mode:[ O_RDONLY ] in
      let listener = Caml_unix.socket PF_UNIX SOCK_STREAM 0 in
      Exn.protect
        ~finally:(fun () ->
          Core_unix.close secret_fd;
          Caml_unix.close listener)
        ~f:(fun () ->
          Caml_unix.bind listener (ADDR_UNIX socket_path);
          Caml_unix.listen listener 1;
          Caml_unix.putenv "CHANNEL_PARENT_ONLY" "fixture-parent-value";
          let descriptor = Core_unix.File_descr.to_int secret_fd in
          assert (descriptor > 4);
          let isolated =
            invoke
              ~program:probe_path
              ~arguments:
                [ secret
                ; socket_path
                ; Int.to_string descriptor
                ; Int.to_string (Core_unix.fstat secret_fd).st_ino
                ]
              (attach (config ()) echo)
              "{}"
            |> success
          in
          let flags = Jsonaf.of_string isolated.stdout in
          List.iter
            [ "file_access"; "socket_access"; "descriptor_access"; "parent_environment" ]
            ~f:(fun name ->
              match Jsonaf.exactly_equal (Jsonaf.member_exn name flags) `False with
              | true -> ()
              | false ->
                failwith
                  ("helper isolation failed: " ^ name ^ " " ^ Jsonaf.to_string flags)));
      print_endline
        "real confined helper exchange, backend denial, byte limits, revocation and \
         cancellation passed")
;;

let () =
  let helper = (Sys.get_argv ()).(1) |> Caml_unix.realpath in
  let probe = (Sys.get_argv ()).(2) |> Caml_unix.realpath in
  let runner = (Sys.get_argv ()).(3) |> Caml_unix.realpath in
  Eio_main.run (fun env -> run env helper probe runner)
;;
