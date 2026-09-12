open Core
module S = Shell_access

let run env helper probe =
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
      Eio.Path.save
        ~create:(`Exclusive 0o700)
        (path helper_path)
        (Eio.Path.load (path helper));
      Eio.Path.save
        ~create:(`Exclusive 0o700)
        (path probe_path)
        (Eio.Path.load (path probe));
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
      let config
            ?(capabilities = capabilities)
            ?backends
            ?(limits =
              { S.Limits.default with wall_time_seconds = 30.; idle_time_seconds = None })
            ()
        =
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
          ~limits
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
      let inherited = ref [ secret_fd ] in
      let listener = Caml_unix.socket PF_UNIX SOCK_STREAM 0 in
      Exn.protect
        ~finally:(fun () ->
          List.iter !inherited ~f:Core_unix.close;
          Caml_unix.close listener)
        ~f:(fun () ->
          (* Exceed the native cleanup batch size. Explicitly inheritable handles
             ensure close-on-exec defaults cannot hide incomplete cleanup. *)
          Core_unix.clear_close_on_exec secret_fd;
          for _ = 1 to 160 do
            inherited := Core_unix.dup ~close_on_exec:false secret_fd :: !inherited
          done;
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
                ; List.map !inherited ~f:(fun fd ->
                    Core_unix.File_descr.to_int fd |> Int.to_string)
                  |> String.concat ~sep:","
                ; Int.to_string (Core_unix.fstat secret_fd).st_ino
                ]
              (attach (config ()) echo)
              "{}"
            |> success
          in
          List.iter !inherited ~f:(fun fd ->
            [%test_eq: int] (Core_unix.fstat secret_fd).st_ino (Core_unix.fstat fd).st_ino);
          let flags = Jsonaf.of_string isolated.stdout in
          List.iter
            [ "file_access"; "socket_access"; "descriptor_access"; "parent_environment" ]
            ~f:(fun name ->
              match Jsonaf.exactly_equal (Jsonaf.member_exn name flags) `False with
              | true -> ()
              | false ->
                failwith
                  ("helper isolation failed: " ^ name ^ " " ^ Jsonaf.to_string flags)));
      let module R = Core_unix.RLimit in
      let parent_limits () =
        [ R.cpu_seconds; R.file_size; R.num_file_descriptors ]
        @ Option.to_list (Result.ok R.virtual_memory)
        |> List.map ~f:R.get
        |> [%sexp_of: R.t list]
      in
      let original_parent_limits = parent_limits () in
      let bound resource ceiling =
        match (R.get resource).max with
        | Infinity -> ceiling
        | Limit inherited ->
          Int64.min inherited (Int64.of_int ceiling) |> Int64.to_int_exn
      in
      let cpu = bound R.cpu_seconds 30 in
      let file_size = bound R.file_size 8192 in
      let open_files = bound R.num_file_descriptors 128 in
      let memory =
        Result.ok R.virtual_memory
        |> Option.map ~f:(fun resource -> bound resource 1_099_511_627_776)
      in
      let limited =
        { S.Limits.default with
          wall_time_seconds = 30.
        ; idle_time_seconds = None
        ; cpu_seconds = Some cpu
        ; file_size_bytes = Some file_size
        ; open_files = Some open_files
        ; memory_bytes = memory
        }
      in
      let applied =
        invoke
          ~program:probe_path
          ~arguments:[ "--limits" ]
          (attach (config ~limits:limited ()) echo)
          "{}"
        |> success
      in
      let actual = Jsonaf.of_string applied.stdout in
      List.iter
        ([ "cpu", cpu; "file_size", file_size; "open_files", open_files ]
         @ Option.to_list (Option.map memory ~f:(fun n -> "memory", n)))
        ~f:(fun (name, expected) ->
          let limits = Jsonaf.member_exn name actual in
          List.iter [ "soft"; "hard" ] ~f:(fun field ->
            let actual = Jsonaf.member_exn field limits |> Jsonaf.int_exn in
            [%test_eq: int] expected actual));
      print_endline "resource limits survive confined exec with exact soft/hard bounds";
      let before = !calls in
      (match
         invoke
           (attach (config ~limits:{ limited with file_size_bytes = Some (-1) } ()) echo)
           "{}"
       with
       | Error (Spawn_error message)
         when String.is_substring message ~substring:"resource limit must be nonnegative"
         -> ()
       | _ -> failwith "negative OS limit did not reject before target execution");
      [%test_eq: int] before !calls;
      let first_ready, mark_first_ready = Eio.Promise.create () in
      let second_ready, mark_second_ready = Eio.Promise.create () in
      let cancel_first, request_cancel_first = Eio.Promise.create () in
      let first_done, mark_first_done = Eio.Promise.create () in
      let release_second, finish_second = Eio.Promise.create () in
      let second_finished = ref false in
      let alternate_file_size = Int.max 0 (file_size / 2) in
      let alternate = { limited with file_size_bytes = Some alternate_file_size } in
      let check_file_size expected request =
        let reported = Jsonaf.of_string request |> Jsonaf.member_exn "file_size" in
        List.iter [ "soft"; "hard" ] ~f:(fun key ->
          [%test_eq: int] expected (Jsonaf.member_exn key reported |> Jsonaf.int_exn))
      in
      Eio.Fiber.all
        [ (fun () ->
            Eio.Fiber.first
              (fun () ->
                 let first =
                   channel (fun request ->
                     check_file_size file_size request;
                     Eio.Promise.resolve mark_first_ready ();
                     Eio.Fiber.await_cancel ())
                 in
                 ignore
                   (invoke
                      ~program:probe_path
                      ~arguments:[ "--limits" ]
                      (attach (config ~limits:limited ()) first)
                      "{}");
                 failwith "first child should remain active until cancelled")
              (fun () -> Eio.Promise.await cancel_first);
            Eio.Promise.resolve mark_first_done ())
        ; (fun () ->
            let second =
              channel (fun request ->
                check_file_size alternate_file_size request;
                Eio.Promise.resolve mark_second_ready ();
                Eio.Promise.await release_second;
                request)
            in
            let result =
              invoke
                ~program:probe_path
                ~arguments:[ "--limits" ]
                (attach (config ~limits:alternate ()) second)
                "{}"
              |> success
            in
            check_file_size alternate_file_size result.stdout;
            second_finished := true)
        ; (fun () ->
            Eio.Promise.await first_ready;
            Eio.Promise.await second_ready;
            assert (Sexp.equal original_parent_limits (parent_limits ()));
            Eio.Promise.resolve request_cancel_first ();
            Eio.Promise.await first_done;
            assert (not !second_finished);
            assert (Sexp.equal original_parent_limits (parent_limits ()));
            Eio.Promise.resolve finish_second ())
        ];
      assert !second_finished;
      assert (Sexp.equal original_parent_limits (parent_limits ()));
      print_endline
        "concurrent child limits are independent; cancellation is isolated; parent \
         limits unchanged";
      let domain_manager = Eio.Stdenv.domain_mgr env in
      let first_entered, enter_first = Eio.Promise.create () in
      let second_entered, enter_second = Eio.Promise.create () in
      let domain_child payload entered other_entered =
        Eio.Domain_manager.run domain_manager (fun () ->
          (* Keep per-invocation policy/audit state local to each domain. Only
             immutable paths and thread-safe synchronization are shared. *)
          let config =
            S.Executor.config
              ~env
              ~runtime_id:"domain-request-channel"
              ~manifest_sha256:"domain-channel-fixture"
              ~policy:(S.Policy.create ~default:Allow [])
              ~capabilities
              ~resolver:(S.Resolver.create ~trusted_roots:[ public ] ())
              ~cwd:(path public)
              ~process_env:[||]
              ~limits:limited
              ()
          in
          let channel =
            S.Request_channel.create
              ~limits:S.Request_channel.default_limits
              ~check:(fun () -> true)
              ~handle:(fun request ->
                Eio.Promise.resolve entered ();
                Eio.Promise.await other_entered;
                request)
            |> Result.ok_or_failwith
          in
          let result =
            S.Executor.run
              (attach config channel)
              { request = S.Request.command (S.Command.create helper_path [])
              ; input = S.Input.Text payload
              ; rationale = None
              ; origin = S.Context.Host "domain-request-channel-test"
              }
            |> success
          in
          [%test_eq: string] (payload ^ "\n") result.stdout)
      in
      Eio.Fiber.both
        (fun () -> domain_child "\"first-domain\"" enter_first second_entered)
        (fun () -> domain_child "\"second-domain\"" enter_second first_entered);
      assert (Sexp.equal original_parent_limits (parent_limits ()));
      print_endline
        "confined helper children exchange and reap across concurrent Eio domains";
      print_endline
        "real confined helper exchange, backend denial, byte limits, revocation and \
         cancellation passed")
;;

let () =
  let helper = (Sys.get_argv ()).(1) |> Caml_unix.realpath in
  let probe = (Sys.get_argv ()).(2) |> Caml_unix.realpath in
  Eio_main.run (fun env -> run env helper probe)
;;
