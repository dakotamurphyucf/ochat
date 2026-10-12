open Core
module R = Shell_runtime.Registry
module H = Shell_runtime.Chatml_extension
module SA = Shell_access
module S = Chatmd_shell_spec.Shell_spec
module Spec = Chatmd_shell_spec.Chatmd_script_spec
module Tool = Chatmd_shell_spec.Shell_tool_spec
module MC = Chatmd_shell_spec.Manifest_compiler
module View = R.Code_view
module Execution = View.Execution

let checkpoint () = Eio.Fiber.check ()
let ok = Support.ok

let with_root env f =
  let random = Eio.Stdenv.secure_random env in
  let bytes = Cstruct.create 16 in
  Eio.Flow.read_exact random bytes;
  let name =
    "code-view-"
    ^ (Cstruct.to_string bytes
       |> String.concat_map ~f:(fun character -> sprintf "%02x" (Char.to_int character)))
  in
  let root = Eio.Path.(Eio.Stdenv.fs env / "/tmp" / name) in
  Eio.Path.mkdir ~perm:0o700 root;
  Exn.protect
    ~f:(fun () -> f root)
    ~finally:(fun () -> Eio.Cancel.protect (fun () -> Eio.Path.rmtree root))
;;

let fixture
      env
      root
      label
      ?(initial_state = "0")
      ?(mutate = Fn.id)
      ?(respond = true)
      ?(extra_runtimes = [])
      ?(runtimes = fun runtime -> [ runtime ])
      ()
  =
  let action =
    if respond
    then sprintf "Intercept.respond(\"%s:\" ++ to_string(state), \"\")" label
    else "Intercept.continue()"
  in
  let body =
    sprintf
      {|let initial_state = %s
let revision_label = "%s"
let before ev state = Task.bind(%s, fun ignored -> Task.pure(state+1))|}
      initial_state
      label
      action
  in
  let script =
    { (Support.script body) with Spec.kind = Shell_before_interceptor; id = "reply" }
  in
  let base =
    Chatmd_shell_spec.Manifest_defaults.legacy_runtime ~source:script.source_ref
  in
  let runtime =
    mutate
      { base with
        cwd = Set (Chatmd_shell_spec.Path_expr.parse "${workspace}" |> ok)
      ; policy = Some { S.default = Set Allow; rules = []; merge = Replace }
      ; interceptors =
          Some
            { S.merge = Replace
            ; values =
                [ { S.id = "reply"
                  ; phase = Before
                  ; extension =
                      Chatml_extension { script = script.id; lifecycle = Session }
                  ; matcher = None
                  ; failure = Error_failure
                  }
                ]
            }
      }
  in
  let tool =
    Tool.parse_compact_attributes
      ~source:script.source_ref
      [ "name", "probe"
      ; "type", "shell"
      ; "mode", "fixed"
      ; "runtime", S.Runtime_id.to_string runtime.id
      ; "command", "/bin/echo"
      ; "result", "structured"
      ]
    |> ok
  in
  let manifest, material =
    MC.compile_with_material
      { runtimes = runtimes runtime @ extra_runtimes
      ; tools = [ tool ]
      ; scripts = [ script ]
      ; legacy_tools = []
      ; moderator_runtime = None
      ; platform = Macos
      ; supported_features = Chatmd_shell_spec.Feature.phase4
      }
    |> Result.map_error ~f:(fun diagnostics ->
      List.map diagnostics ~f:Chatmd_shell_spec.Diagnostic.to_string
      |> String.concat ~sep:"; ")
    |> Result.ok_or_failwith
  in
  let contract = H.compiler_contract script.kind ~entrypoint:"before" ~checkpoint |> ok in
  let input = Support.input contract [ "entry.chatml", body, [] ] in
  let compiled =
    H.compile_prepared
      input
      ~script
      ~entrypoint:"before"
      ~limits:Support.compiler_limits
      ~max_dependency_depth:16
      ~env
      ()
    |> ok
  in
  let prepared =
    R.prepare_with_compiler_bounded
      material
      ~compile_extension:(fun _ -> Ok compiled)
      ~admin_policy:Shell_runtime.Admin_policy.permissive
      ~manifest
      ~max_manifest_bytes:100_000
      ~checkpoint
    |> ok
  in
  let complete = R.prepared_manifest prepared in
  let grant =
    Shell_runtime.Manifest_authorizer.authorize
      Shell_runtime.Manifest_authorizer.assume_authorized
      complete
    |> ok
  in
  let host =
    Shell_runtime.Host.
      { env
      ; workspace = root
      ; tool_dir = root
      ; prompt_dir = root
      ; session_dir = root
      ; cache_dir = root
      ; home = root
      ; source_dirs = String.Map.singleton script.source_ref.file root
      ; process_environment = [| "PATH=/usr/bin:/bin" |]
      ; session_id = "code-view"
      }
  in
  prepared, grant, host, tool
;;

let instantiate
      ?(approval_provider = Shell_runtime.Approval_broker.None_available)
      sw
      (prepared, grant, host, _)
  =
  R.instantiate_prepared ~sw ~host ~grant ~approval_provider prepared |> ok
;;

let rebind sw registry (prepared, grant, _, _) =
  R.rebind_code registry ~prepared ~grant ~sw ~max_work_steps:100_000 ~checkpoint ()
;;

let config registry =
  R.runtimes registry |> List.hd_exn |> Shell_runtime.Runtime.executor_config
;;

let raw config =
  SA.Executor.run
    config
    { request = SA.Request.command (SA.Command.create "/bin/echo" [])
    ; input = Empty
    ; rationale = None
    ; origin = Host "code-view"
    }
;;

let stdout config = (raw config |> ok).SA.Executor.stdout

type snapshot_outcome =
  | Persist
  | Reject
  | Raise of exn

let%expect_test "binding snapshots serialize and callback failures preserve both states" =
  Eio_main.run (fun env ->
    with_root env (fun root ->
      let runtimes (runtime : S.t) =
        let second = S.Runtime_id.of_string ~source:runtime.source "second" |> ok in
        [ runtime; { runtime with id = second } ]
      in
      let prepared, grant, host, _ = fixture env root "shared" ~runtimes () in
      let persisted = ref [] in
      let calls = ref 0 in
      let outcome = ref Persist in
      let entered, signal_entered = Eio.Promise.create () in
      let release, signal_release = Eio.Promise.create () in
      let persist snapshots =
        Int.incr calls;
        if Int.equal !calls 1
        then (
          Eio.Promise.resolve signal_entered ();
          Eio.Promise.await release);
        match !outcome with
        | Persist ->
          persisted := snapshots;
          Ok ()
        | Reject -> Error "controlled persistence rejection"
        | Raise exn -> raise exn
      in
      let states () =
        List.map
          !persisted
          ~f:(fun (snapshot : Session.Shell_state.Extension_snapshot.t) ->
            let state =
              match snapshot.state with
              | Record fields -> List.Assoc.find_exn fields ~equal:String.equal "state"
              | Int _ | Float _ | Bool _ | String _ | Unit | Array _ | Variant _ | Tuple _
                -> failwith "expected actual extension snapshot"
            in
            snapshot.runtime_id, state)
        |> List.sort ~compare:(fun (left, _) (right, _) -> String.compare left right)
      in
      Eio.Switch.run (fun sw ->
        let registry =
          R.instantiate_prepared
            ~sw
            ~host
            ~grant
            ~approval_provider:Shell_runtime.Approval_broker.None_available
            ~persist_extension_snapshots:persist
            prepared
          |> ok
        in
        let configs =
          List.map (R.runtimes registry) ~f:Shell_runtime.Runtime.executor_config
        in
        let first, second =
          match configs with
          | [ first; second ] -> first, second
          | _ -> failwith "expected two independently stateful bindings"
        in
        let a = Eio.Fiber.fork_promise ~sw (fun () -> stdout first) in
        Eio.Promise.await entered;
        let attempted, signal_attempted = Eio.Promise.create () in
        let b =
          Eio.Fiber.fork_promise ~sw (fun () ->
            Eio.Promise.resolve signal_attempted ();
            stdout second)
        in
        Eio.Promise.await attempted;
        (* Exercise this tiny prepared program through its cooperative evaluation
           checkpoints before releasing the first publisher. The old publisher
           must fail this bounded schedule; it is not an exhaustive scheduler model. *)
        for _ = 1 to 256 do
          Eio.Fiber.yield ()
        done;
        let before_release = !calls in
        Eio.Promise.resolve signal_release ();
        [%test_eq: string] (Eio.Promise.await_exn a) "shared:0";
        [%test_eq: string] (Eio.Promise.await_exn b) "shared:0";
        [%test_eq: int] before_release 1;
        let committed = states () in
        [%test_eq: int] (List.length committed) 2;
        outcome := Reject;
        assert (Result.is_error (raw first));
        [%test_eq: (string * Session.Snapshot.t) list] (states ()) committed;
        let marker = Failure "snapshot callback origin" in
        let cancelled = Eio.Cancel.Cancelled marker in
        List.iter [ marker; cancelled ] ~f:(fun expected ->
          outcome := Raise expected;
          (* The shell executor maps interceptor exceptions to a typed rejection.
             Direct ChatML instance tests cover their original exception identity. *)
          assert (Result.is_error (raw first));
          [%test_eq: (string * Session.Snapshot.t) list] (states ()) committed);
        outcome := Persist;
        [%test_eq: string] (stdout first) "shared:1";
        [%test_eq: string] (stdout second) "shared:1";
        [%test_eq: Session.Snapshot.t list] (List.map (states ()) ~f:snd) [ Int 2; Int 2 ])));
  print_endline
    "both binding states persist; rejection/throw/cancel preserve state and reuse";
  [%expect
    {| both binding states persist; rejection/throw/cancel preserve state and reuse |}]
;;

let denied config =
  match raw config with
  | Error (Denied _) -> ()
  | Ok _ | Error _ -> failwith "expired native config did not reject"
;;

let tool_output function_ =
  match function_.Ochat_function.run "{}" with
  | Openai.Responses.Tool_output.Output.Text text -> Jsonaf.of_string text
  | _ -> failwith "expected native tool text"
;;

let moderator_session () =
  let module Host = Chatml_host_runtime in
  let compiled =
    Host.compile_script
      ~source:"let initial_state = 0 let on_event ctx state ev = Task.pure(state)"
      ()
    |> ok
  in
  Host.instantiate_session
    (Host.default_runtime_config ())
    compiled
    ~entrypoints:{ initial_state_name = "initial_state"; on_event_name = "on_event" }
  |> ok
;;

let%expect_test
    "fresh code uses actual Tool and Process routes, retaining old state and native \
     identity"
  =
  Eio_main.run (fun env ->
    with_root env (fun root ->
      let old = fixture env root "old" () in
      let fresh = fixture env root "fresh" () in
      Eio.Switch.run (fun owner ->
        let registry = instantiate owner old in
        let _, _, _, tool = old in
        let function_ = Chat_response.Shell_tool.create registry tool |> ok in
        let original = config registry in
        [%test_eq: string] (stdout original) "old:0";
        let escaped = ref None in
        let session = moderator_session () in
        Eio.Switch.run (fun code_owner ->
          let view = rebind code_owner registry fresh |> ok in
          assert (View.work_steps view > 0);
          View.with_execution view ~checkpoint (fun execution ->
            let selected = Execution.registry execution |> ok in
            escaped := Some selected;
            let fresh_config = config selected in
            assert (SA.Executor.same_resource_owner original fresh_config);
            let called =
              Shell_runtime.Call_context.with_services
                ~prepare_executor:(Execution.prepare_executor execution)
                (fun () ->
                   Ok
                     Shell_runtime.Call_context.
                       { session_id = "actual-child"
                       ; approval_store = SA.Approval.create_store ()
                       ; approval_provider = Auto_deny
                       ; check = (fun () -> Ok ())
                       })
                (fun () -> tool_output function_)
            in
            [%test_eq: string]
              (Jsonaf.member_exn "stdout" called |> Jsonaf.string_exn)
              "fresh:0";
            [%test_eq: string]
              (Jsonaf.member_exn "manifest_sha256" called |> Jsonaf.string_exn)
              (R.manifest selected).Chatmd_shell_spec.Manifest.sha256;
            let process =
              Shell_runtime.Moderator_process_adapter.handler
                ~registry:selected
                ~runtime_id:tool.runtime
                session
                ~command:"/bin/echo"
                ~args:(Chatml.Chatml_lang.VArray [||])
              |> ok
            in
            [%test_eq: string] process "fresh:1";
            print_s
              [%sexp
                ((("Tool", "fresh:0"), ("Process", process))
                 : (string * string) * (string * string))])
          |> ok;
          let selected = Option.value_exn !escaped in
          denied (config selected);
          (* The authentic old resource template, not a borrowed expired registry,
           remains the source of each new admission. *)
          View.with_execution view ~checkpoint (fun execution ->
            [%test_eq: string]
              (stdout (Execution.registry execution |> ok |> config))
              "fresh:2")
          |> ok);
        denied (config (Option.value_exn !escaped));
        [%test_eq: string] (stdout original) "old:1";
        print_s
          [%sexp
            ((("original", "old:1"), ("escaped", "denied"))
             : (string * string) * (string * string))])));
  [%expect
    {|
    ((Tool fresh:0) (Process fresh:1))
    ((original old:1) (escaped denied))
    |}]
;;

let%test_unit
    "expired admissions, owner closure and changed material reject before initialization"
  =
  Eio_main.run (fun env ->
    with_root env (fun root ->
      let old = fixture env root "old" () in
      let fresh = fixture env root "new" () in
      let poison = fixture env root "poison" ~initial_state:"1 / 0" () in
      let _, _, _, tool = old in
      let escaped = ref None in
      let retained_view = ref None in
      Eio.Switch.run (fun owner ->
        let registry = instantiate owner old in
        Eio.Switch.run (fun code_owner ->
          let view = rebind code_owner registry fresh |> ok in
          retained_view := Some view;
          View.with_execution view ~checkpoint (fun execution ->
            escaped := Some (Execution.registry execution |> ok))
          |> ok;
          (match rebind code_owner (Option.value_exn !escaped) poison with
           | Error [ error ] -> [%test_eq: string] error.code "shell.registry_closed"
           | Ok _ | Error _ -> failwith "expired registry reached initialization");
          let changed =
            fixture
              env
              root
              "poison"
              ~initial_state:"1 / 0"
              ~mutate:(fun runtime -> { runtime with S.pipefail = Set true })
              ()
          in
          (match rebind code_owner registry changed with
           | Error [ error ] ->
             [%test_eq: string] error.code "shell.native_footprint_changed"
           | Ok _ | Error _ -> failwith "changed material reached initialization");
          let interrupted = Failure "construction checkpoint identity" in
          let prepared, grant, _, _ = fresh in
          let caught =
            try
              ignore
                (R.rebind_code
                   registry
                   ~prepared
                   ~grant
                   ~sw:code_owner
                   ~max_work_steps:100_000
                   ~checkpoint:(fun () -> raise interrupted)
                   ());
              false
            with
            | error -> phys_equal error interrupted
          in
          assert caught;
          let expired = config (Option.value_exn !escaped) in
          View.with_execution view ~checkpoint (fun execution ->
            denied (Execution.prepare_executor execution expired |> ok))
          |> ok);
        let calls = ref 0 in
        assert (
          Result.is_error
            (View.with_execution (Option.value_exn !retained_view) ~checkpoint (fun _ ->
               Int.incr calls)));
        [%test_eq: int] !calls 0;
        Eio.Switch.run (fun code_owner ->
          let view = rebind code_owner registry fresh |> ok in
          retained_view := Some view));
      let calls = ref 0 in
      assert (
        Result.is_error
          (View.with_execution (Option.value_exn !retained_view) ~checkpoint (fun _ ->
             Int.incr calls)));
      [%test_eq: int] !calls 0;
      ignore tool))
;;

let%test_unit
    "Process and native Tool dispatch use actual caller approval identity and expire"
  =
  Eio_main.run (fun env ->
    with_root env (fun root ->
      let ask runtime =
        { runtime with
          S.policy = Some { default = Set Ask; rules = []; merge = Replace }
        }
      in
      let original = fixture env root "old" ~mutate:ask ~respond:false () in
      let fresh = fixture env root "fresh" ~mutate:ask ~respond:false () in
      let _, _, _, tool = original in
      Eio.Switch.run (fun owner ->
        let registry = instantiate ~approval_provider:Assume_approved owner original in
        let function_ = Chat_response.Shell_tool.create registry tool |> ok in
        let session = moderator_session () in
        Eio.Switch.run (fun code_owner ->
          let view = rebind code_owner registry fresh |> ok in
          View.with_execution view ~checkpoint (fun execution ->
            let selected = Execution.registry execution |> ok in
            let manifest = (R.manifest selected).Chatmd_shell_spec.Manifest.sha256 in
            let requests = ref [] in
            let broker_ref = ref None in
            let broker =
              Shell_runtime.Approval_broker.create
                ~on_pending:(fun request ->
                  [%test_eq: string option]
                    request.request.context.session_id
                    (Some "actual-child");
                  [%test_eq: string] request.manifest_sha256 manifest;
                  requests := request :: !requests;
                  Shell_runtime.Approval_broker.respond
                    (Option.value_exn !broker_ref)
                    ~id:request.id
                    Approve_exact_session
                  |> ok)
                ()
            in
            broker_ref := Some broker;
            let lookups = ref 0 in
            let remembered = ref 0 in
            let store =
              SA.Approval.create_store
                ~lookup:(fun ~now:_ ~session_id _ ->
                  [%test_eq: string option] session_id (Some "actual-child");
                  Int.incr lookups;
                  Ok false)
                ~remember:(fun ~session_id _ scope _ ->
                  [%test_eq: string option] session_id (Some "actual-child");
                  (match scope with
                   | Exact_session _ -> ()
                   | _ -> assert false);
                  Int.incr remembered;
                  Ok ())
                ()
            in
            let released, release = Eio.Promise.create () in
            let waiting =
              Shell_runtime.Call_context.with_services
                ~prepare_executor:(Execution.prepare_executor execution)
                (fun () ->
                   Ok
                     Shell_runtime.Call_context.
                       { session_id = "actual-child"
                       ; approval_store = store
                       ; approval_provider = Callback broker
                       ; check = (fun () -> Ok ())
                       })
                (fun () ->
                   let called = tool_output function_ in
                   [%test_eq: string]
                     (Jsonaf.member_exn "stdout" called |> Jsonaf.string_exn)
                     "\n";
                   [%test_eq: string]
                     (Shell_runtime.Moderator_process_adapter.handler
                        ~registry:selected
                        ~runtime_id:tool.runtime
                        session
                        ~command:"/bin/echo"
                        ~args:(Chatml.Chatml_lang.VArray [||])
                      |> ok)
                     "\n";
                   Eio.Fiber.fork_promise ~sw:owner (fun () ->
                     Eio.Promise.await released;
                     Shell_runtime.Moderator_process_adapter.handler
                       ~registry:selected
                       ~runtime_id:tool.runtime
                       session
                       ~command:"/bin/echo"
                       ~args:(Chatml.Chatml_lang.VArray [||])))
            in
            [%test_eq: int] !lookups 2;
            [%test_eq: int] !remembered 2;
            [%test_eq: int] (List.length !requests) 2;
            Eio.Promise.resolve release ();
            assert (Result.is_error (Eio.Promise.await waiting |> ok));
            [%test_eq: int] (List.length !requests) 2;
            Shell_runtime.Approval_broker.close broker)
          |> ok))))
;;

let%test_unit "executable reviewer uses fresh worker code and actual caller services" =
  Eio_main.run (fun env ->
    with_root env (fun root ->
      let executable = Eio.Path.(root / "reviewer") in
      let fixture_path =
        Eio.Path.native_exn Eio.Path.(Eio.Stdenv.cwd env / "reviewer_fixture.exe")
      in
      (* Dune may symlink this explicit executable dependency outside its action
         directory; the fixture's host fs grant admits that exact native path. *)
      Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / fixture_path)
      |> Eio.Path.save ~create:(`Or_truncate 0o700) executable;
      let source = (Support.script "native reviewer declaration").source_ref in
      let worker_id = S.Runtime_id.of_string ~source "worker" |> ok in
      let worker =
        { (Chatmd_shell_spec.Manifest_defaults.legacy_runtime ~source) with
          id = worker_id
        ; policy = Some { default = Set Ask; rules = []; merge = Replace }
        }
      in
      let hook =
        S.
          { executable = Chatmd_shell_spec.Path_expr.parse "${workspace}/reviewer" |> ok
          ; sha256 = None
          ; runtime = "worker"
          ; protocol = Shell_hook_json_v1
          ; timeout = None
          ; max_input = None
          ; max_output = None
          }
      in
      let mutate runtime =
        { runtime with
          S.policy = Some { default = Set Ask; rules = []; merge = Replace }
        ; reviewers =
            Some
              { merge = Replace
              ; values =
                  [ Executable_reviewer { id = "review"; hook; failure = Error_failure } ]
              }
        }
      in
      let original =
        fixture env root "old" ~mutate ~respond:false ~extra_runtimes:[ worker ] ()
      in
      let fresh =
        fixture env root "fresh" ~mutate ~respond:false ~extra_runtimes:[ worker ] ()
      in
      let _, _, _, tool = original in
      Eio.Switch.run (fun owner ->
        let registry = instantiate ~approval_provider:Assume_approved owner original in
        let function_ = Chat_response.Shell_tool.create registry tool |> ok in
        Eio.Switch.run (fun code_owner ->
          let view = rebind code_owner registry fresh |> ok in
          View.with_execution view ~checkpoint (fun execution ->
            let selected = Execution.registry execution |> ok in
            let manifest = (R.manifest selected).Chatmd_shell_spec.Manifest.sha256 in
            let requests = ref 0 in
            let remembered = ref 0 in
            let broker_ref = ref None in
            let broker =
              Shell_runtime.Approval_broker.create
                ~on_pending:(fun request ->
                  [%test_eq: string] request.request.context.runtime_id "worker";
                  [%test_eq: string option]
                    request.request.context.session_id
                    (Some "worker-child");
                  [%test_eq: string] request.manifest_sha256 manifest;
                  Int.incr requests;
                  Shell_runtime.Approval_broker.respond
                    (Option.value_exn !broker_ref)
                    ~id:request.id
                    Approve_exact_session
                  |> ok)
                ()
            in
            broker_ref := Some broker;
            let store =
              SA.Approval.create_store
                ~lookup:(fun ~now:_ ~session_id _ ->
                  [%test_eq: string option] session_id (Some "worker-child");
                  Ok false)
                ~remember:(fun ~session_id _ _ _ ->
                  [%test_eq: string option] session_id (Some "worker-child");
                  Int.incr remembered;
                  Ok ())
                ()
            in
            let released, release = Eio.Promise.create () in
            let waiting =
              Shell_runtime.Call_context.with_services
                ~prepare_executor:(Execution.prepare_executor execution)
                (fun () ->
                   Ok
                     Shell_runtime.Call_context.
                       { session_id = "worker-child"
                       ; approval_store = store
                       ; approval_provider = Callback broker
                       ; check = (fun () -> Ok ())
                       })
                (fun () ->
                   let called = tool_output function_ in
                   [%test_eq: string]
                     (Jsonaf.member_exn "stdout" called |> Jsonaf.string_exn)
                     "\n";
                   Eio.Fiber.fork_promise ~sw:owner (fun () ->
                     Eio.Promise.await released;
                     (* The main executor is intentionally called directly: the
                       nested worker must independently reject expired services. *)
                     raw (config selected)))
            in
            [%test_eq: int] !requests 1;
            [%test_eq: int] !remembered 1;
            Eio.Promise.resolve release ();
            assert (Result.is_error (Eio.Promise.await waiting |> ok));
            [%test_eq: int] !requests 1;
            [%test_eq: int] !remembered 1;
            let worker_runtime = R.runtime selected "worker" |> Option.value_exn in
            let executable =
              SA.Resolver.resolve
                (SA.Resolver.create ())
                ~fs:(Eio.Stdenv.fs env)
                ~cwd:(Eio.Path.native_exn root)
                ~environment:[||]
                (SA.Command.create (Eio.Path.native_exn executable) [])
              |> ok
            in
            let worker =
              Shell_runtime.Hook_worker.create
                ~env
                ~hook_id:"checkpoint"
                ~kind:Reviewer
                ~executable
                ~executor_config:(Shell_runtime.Runtime.executor_config worker_runtime)
                ~timeout_seconds:10.
                ~max_input_bytes:65_536
                ~max_output_bytes:1024
                ~redact:Fn.id
                ()
            in
            let callback_error = Eio.Time.Timeout in
            let caught =
              try
                Shell_runtime.Call_context.with_services
                  ~prepare_executor:(fun _ -> raise callback_error)
                  (fun () ->
                     Ok
                       Shell_runtime.Call_context.
                         { session_id = "worker-child"
                         ; approval_store = store
                         ; approval_provider = Callback broker
                         ; check = (fun () -> Ok ())
                         })
                  (fun () ->
                     ignore (Shell_runtime.Hook_worker.invoke worker (`Object [])));
                false
              with
              | error -> phys_equal error callback_error
            in
            assert caught;
            [%test_eq: int] !requests 1;
            Shell_runtime.Approval_broker.close broker)
          |> ok))))
;;

let%test_unit
    "deferred jobs explicitly enter fresh view and caller scope; either owner closure \
     rejects"
  =
  Eio_main.run (fun env ->
    with_root env (fun root ->
      let old = fixture env root "old" () in
      let fresh = fixture env root "fresh" () in
      let _, _, _, tool = old in
      Eio.Switch.run (fun jobs ->
        Eio.Switch.run (fun code_owner ->
          let view = ref None in
          let closed_job = ref None in
          let closed_release = ref None in
          Eio.Switch.run (fun resource_owner ->
            let registry = instantiate resource_owner old in
            let function_ = Chat_response.Shell_tool.create registry tool |> ok in
            let current = rebind code_owner registry fresh |> ok in
            view := Some current;
            let released, release = Eio.Promise.create () in
            let waiting =
              View.with_execution current ~checkpoint (fun execution ->
                Shell_runtime.Call_context.with_services
                  ~prepare_executor:(Execution.prepare_executor execution)
                  (fun () ->
                     Ok
                       Shell_runtime.Call_context.
                         { session_id = "creating-scope"
                         ; approval_store = SA.Approval.create_store ()
                         ; approval_provider = Auto_deny
                         ; check = (fun () -> Ok ())
                         })
                  (fun () ->
                     Eio.Fiber.fork_promise ~sw:jobs (fun () ->
                       Eio.Promise.await released;
                       (* Ambient inherited scopes have expired. Execution borrows the
                   recorded owner anew and obtains genuine current services. *)
                       View.with_execution current ~checkpoint (fun execution ->
                         Shell_runtime.Call_context.with_services
                           ~prepare_executor:(Execution.prepare_executor execution)
                           (fun () ->
                              Ok
                                Shell_runtime.Call_context.
                                  { session_id = "executing-scope"
                                  ; approval_store = SA.Approval.create_store ()
                                  ; approval_provider = Auto_deny
                                  ; check = (fun () -> Ok ())
                                  })
                           (fun () -> tool_output function_)))))
              |> ok
            in
            Eio.Promise.resolve release ();
            let called = Eio.Promise.await waiting |> ok |> ok in
            [%test_eq: string]
              (Jsonaf.member_exn "stdout" called |> Jsonaf.string_exn)
              "fresh:0";
            let released, release = Eio.Promise.create () in
            closed_release := Some release;
            closed_job
            := Some
                 (Eio.Fiber.fork_promise ~sw:jobs (fun () ->
                    Eio.Promise.await released;
                    let calls = ref 0 in
                    let result =
                      View.with_execution current ~checkpoint (fun _ -> Int.incr calls)
                    in
                    [%test_eq: int] !calls 0;
                    result)));
          Eio.Promise.resolve (Option.value_exn !closed_release) ();
          assert (Result.is_error (Eio.Promise.await (Option.value_exn !closed_job) |> ok));
          let calls = ref 0 in
          assert (
            Result.is_error
              (View.with_execution (Option.value_exn !view) ~checkpoint (fun _ ->
                 Int.incr calls)));
          [%test_eq: int] !calls 0))))
;;

exception Dispatch_cancelled of int ref

let%test_unit "blocked fresh native dispatch joins before either owner releases" =
  Eio_main.run (fun env ->
    with_root env (fun root ->
      let entered, enter = Eio.Promise.create () in
      let finished = Atomic.make false in
      let (Eio.Resource.T (data, ops)) = Eio.Stdenv.process_mgr env in
      let module Native = (val Eio.Resource.get ops Eio.Process.Pi.Mgr) in
      let module Observed = struct
        type t = Native.t
        type tag = Native.tag

        let pipe = Native.pipe

        let spawn data ~sw ?cwd ?stdin ?stdout ?stderr ?env:process_env ?executable args =
          let actual = ref None in
          (* Register BEFORE the real provider's spawn. Its later child cleanup
             runs first at switch release; this observer must then see reaped work
             before the enclosing code/native owner can release. It never signals
             or kills the child itself. A missing provider join cannot pass. *)
          Eio.Switch.on_release sw (fun () ->
            Option.iter !actual ~f:(fun process ->
              Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
                ignore (Eio.Process.await process);
                Atomic.set finished true)));
          let process =
            Native.spawn
              data
              ~sw
              ?cwd
              ?stdin
              ?stdout
              ?stderr
              ?env:process_env
              ?executable
              args
          in
          actual := Some process;
          Eio.Promise.resolve enter ();
          process
        ;;
      end
      in
      let manager = Eio.Resource.T (data, Eio.Process.Pi.mgr (module Observed)) in
      let wrapped : Eio_unix.Stdenv.base =
        object
          method domain_mgr = env#domain_mgr
          method stdin = env#stdin
          method stdout = env#stdout
          method stderr = env#stderr
          method net = env#net
          method process_mgr = manager
          method clock = env#clock
          method mono_clock = env#mono_clock
          method fs = env#fs
          method cwd = env#cwd
          method secure_random = env#secure_random
          method debug = env#debug
          method backend_id = env#backend_id
        end
      in
      let executable = Eio.Path.(root / "blocked-native") in
      let path =
        Eio.Path.native_exn Eio.Path.(Eio.Stdenv.cwd env / "reviewer_fixture.exe")
      in
      Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path)
      |> Eio.Path.save ~create:(`Or_truncate 0o700) executable;
      let original = fixture wrapped root "old" ~respond:false () in
      let fresh = fixture wrapped root "fresh" ~respond:false () in
      let _, _, _, tool = original in
      let retained_view = ref None in
      Eio.Switch.run (fun owner ->
        let registry = instantiate owner original in
        let session = moderator_session () in
        Eio.Switch.on_release owner (fun () -> assert (Atomic.get finished));
        let identity = ref 29 in
        (match
           Eio.Cancel.sub (fun cancel ->
             Eio.Switch.run (fun code_owner ->
               let view = rebind code_owner registry fresh |> ok in
               retained_view := Some view;
               Eio.Switch.on_release code_owner (fun () -> assert (Atomic.get finished));
               Eio.Fiber.both
                 (fun () ->
                    Eio.Promise.await entered;
                    Eio.Cancel.cancel cancel (Dispatch_cancelled identity))
                 (fun () ->
                    View.with_execution view ~checkpoint (fun execution ->
                      let selected = Execution.registry execution |> ok in
                      Shell_runtime.Call_context.with_services
                        ~prepare_executor:(Execution.prepare_executor execution)
                        (fun () ->
                           Ok
                             Shell_runtime.Call_context.
                               { session_id = "blocked-child"
                               ; approval_store = SA.Approval.create_store ()
                               ; approval_provider = Auto_deny
                               ; check = (fun () -> Ok ())
                               })
                        (fun () ->
                           Shell_runtime.Moderator_process_adapter.handler
                             ~registry:selected
                             ~runtime_id:tool.runtime
                             session
                             ~command:(Eio.Path.native_exn executable)
                             ~args:
                               (Chatml.Chatml_lang.VArray
                                  [| Chatml.Chatml_lang.VString "--block" |])
                           |> ok))
                    |> ok
                    |> ignore)))
         with
         | _ -> failwith "blocked native process did not report owner cancellation"
         | exception Eio.Cancel.Cancelled (Dispatch_cancelled actual) ->
           assert (phys_equal identity actual));
        assert (Atomic.get finished);
        let callbacks = ref 0 in
        assert (
          Result.is_error
            (View.with_execution (Option.value_exn !retained_view) ~checkpoint (fun _ ->
               Int.incr callbacks)));
        [%test_eq: int] !callbacks 0)))
;;

let%test_unit
    "native worker occurrences, model selector absence and raw sink are retained without \
     rereads"
  =
  Eio_main.run (fun env ->
    with_root env (fun root ->
      List.iter [ false; true ] ~f:(fun has_model_selector ->
        let hook_path = Eio.Path.(root / "worker") in
        let secret_path = Eio.Path.(root / "secret") in
        let audit_path = Eio.Path.(root / "audit.jsonl") in
        Eio.Path.save ~create:(`Or_truncate 0o700) hook_path "#!/bin/sh\nexit 0\n";
        Eio.Path.save ~create:(`Or_truncate 0o600) secret_path "retained-secret";
        let path value = Chatmd_shell_spec.Path_expr.parse value |> ok in
        let source = (Support.script "native declarations").source_ref in
        let worker_id = S.Runtime_id.of_string ~source "worker" |> ok in
        let worker =
          { (Chatmd_shell_spec.Manifest_defaults.legacy_runtime ~source) with
            id = worker_id
          }
        in
        let hook =
          S.
            { executable = path "${workspace}/worker"
            ; sha256 = None
            ; runtime = "worker"
            ; protocol = Shell_hook_json_v1
            ; timeout = None
            ; max_input = None
            ; max_output = None
            }
        in
        let mutate runtime =
          { runtime with
            S.reviewers =
              Some
                { merge = Replace
                ; values =
                    [ Executable_reviewer { id = "same"; hook; failure = Error_failure }
                    ; Executable_reviewer { id = "same"; hook; failure = Error_failure }
                    ; Model_reviewer
                        { id = "model"
                        ; agent = "reviewer"
                        ; model = Some "chosen"
                        ; failure = Error_failure
                        }
                    ]
                }
          ; secrets =
              Some
                { replacement = Set "[REDACTED]"
                ; merge = Replace
                ; sources =
                    [ From_file
                        { path = path "${workspace}/secret"
                        ; optional = false
                        ; strip = true
                        }
                    ]
                }
          ; audit =
              Some
                { format = Jsonl
                ; path = Some (path "${workspace}/audit.jsonl")
                ; content = Redacted
                ; failure = Deny_start
                ; filter = None
                }
          }
        in
        let original =
          fixture env root "old retained-secret" ~mutate ~extra_runtimes:[ worker ] ()
        in
        let fresh =
          fixture env root "new retained-secret" ~mutate ~extra_runtimes:[ worker ] ()
        in
        let selector_calls = ref 0 in
        let model_completion =
          if has_model_selector
          then
            Some
              (fun ~agent ~model ->
                [%test_eq: string] agent "reviewer";
                [%test_eq: string option] model (Some "chosen");
                Int.incr selector_calls;
                Some (fun ~prompt:_ -> failwith "unexpected model execution"))
          else None
        in
        Eio.Switch.run (fun owner ->
          let prepared, grant, host, _ = original in
          let registry =
            R.instantiate_prepared
              ~sw:owner
              ~host
              ~grant
              ~approval_provider:Shell_runtime.Approval_broker.None_available
              ?model_completion
              prepared
            |> ok
          in
          [%test_eq: int] !selector_calls (if has_model_selector then 1 else 0);
          [%test_eq: string] (stdout (config registry)) "old [REDACTED]:0";
          (* This is native lookup evidence: unchanged declared paths now fail if
           reopened. Rebinding must consume the original actual occurrences and
           retained sink/filter/service, including duplicate worker IDs. *)
          Eio.Path.unlink hook_path;
          Eio.Path.unlink secret_path;
          Eio.Switch.run (fun code_owner ->
            let view = rebind code_owner registry fresh |> ok in
            View.with_execution view ~checkpoint (fun execution ->
              let selected = Execution.registry execution |> ok |> config in
              [%test_eq: string] (stdout selected) "new [REDACTED]:0";
              Eio.Path.unlink audit_path;
              (* The retained chained sink rejects the broken history. Its
                 original sequence/failure policy must not be reset/reopened. *)
              (match raw selected with
               | Error (Audit_unavailable _) -> ()
               | Ok _ | Error _ -> failwith "retained audit failure policy changed");
              assert (not (Eio.Path.is_file audit_path)))
            |> ok);
          [%test_eq: int] !selector_calls (if has_model_selector then 1 else 0)))))
;;

let%test_unit "executor selection rechecks admission after a yielding borrowed checkpoint"
  =
  Eio_main.run (fun env ->
    with_root env (fun root ->
      let original = fixture env root "old" () in
      let fresh = fixture env root "fresh" () in
      Eio.Switch.run (fun jobs ->
        Eio.Switch.run (fun resource_owner ->
          let registry = instantiate resource_owner original in
          let ready, admit = Eio.Promise.create () in
          let released, release = Eio.Promise.create () in
          let closed, acknowledge_close = Eio.Promise.create () in
          let worker =
            Eio.Fiber.fork_promise ~sw:jobs (fun () ->
              let view = Eio.Promise.await ready in
              let armed = ref false in
              let checkpoint () =
                checkpoint ();
                if !armed
                then (
                  armed := false;
                  Eio.Promise.resolve release ();
                  Eio.Promise.await closed)
              in
              View.with_execution view ~checkpoint (fun execution ->
                armed := true;
                let result = Execution.prepare_executor execution (config registry) in
                assert (Result.is_error result)))
          in
          Eio.Switch.run (fun code_owner ->
            let view = rebind code_owner registry fresh |> ok in
            Eio.Promise.resolve admit view;
            Eio.Promise.await released);
          Eio.Promise.resolve acknowledge_close ();
          Eio.Promise.await worker |> ok |> ok))))
;;
