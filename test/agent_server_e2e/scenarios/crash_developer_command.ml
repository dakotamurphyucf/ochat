open! Core
module F = Crash_recovery_fixture
module P = Agent_protocol
module S = Agent_server
module R = Agent_store.Developer_command_receipt
module PM = Support.Process_manager

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : P.Error.t)]
;;

let checked = function
  | Ok value -> value
  | Error _ -> F.fail "developer crash fixture admission failed"
;;

let developer_configuration = Support.Developer_fixture.developer_configuration
let semantic_authoring_policy = Support.Developer_fixture.semantic_authoring_policy

type handoff =
  { run_id : P.Id.Developer_run.t
  ; key : P.Idempotency_key.t
  ; expected : P.Authoring.Context.Expected.t
  ; receipt_id : R.Id.t
  ; capture_fingerprint : Chatml_source.Digest.t
  ; request_fingerprint : Chatml_source.Digest.t
  }

let handoff_json value =
  `Object
    [ "run_id", `String (P.Id.Developer_run.to_string value.run_id)
    ; "key", `String (P.Idempotency_key.to_string value.key)
    ; "expected", P.Authoring.Context.Expected.to_json value.expected
    ; "receipt_id", `String (R.Id.to_string value.receipt_id)
    ; ( "capture_fingerprint"
      , `String (Chatml_source.Digest.to_hex value.capture_fingerprint) )
    ; ( "request_fingerprint"
      , `String (Chatml_source.Digest.to_hex value.request_fingerprint) )
    ]
;;

let handoff_of_json json =
  let field name = Jsonaf.member_exn name json in
  let string name =
    match field name with
    | `String value -> value
    | _ -> F.fail "invalid developer crash handoff string"
  in
  { run_id = P.Id.Developer_run.of_string (string "run_id") |> checked
  ; key = P.Idempotency_key.of_string (string "key") |> checked
  ; expected = P.Authoring.Context.Expected.of_json (field "expected") |> ok
  ; receipt_id = R.Id.of_string (string "receipt_id") |> checked
  ; capture_fingerprint =
      Chatml_source.Digest.of_hex (string "capture_fingerprint") |> Or_error.ok_exn
  ; request_fingerprint =
      Chatml_source.Digest.of_hex (string "request_fingerprint") |> Or_error.ok_exn
  }
;;

let request ~expected ~marker =
  let bounds =
    Chatml_developer_capture.Json_bounds.create
      ~max_bytes:4096
      ~max_depth:16
      ~max_nodes:128
    |> Or_error.ok_exn
  in
  S.Daemon.Developer.Request.create
    ~route:
      (Document_script
         { expected; selector = P.Authoring.Analysis.Target.selected_script "s" |> ok })
    ~kind:Live_run
    ~target:Chatml.Chatml_compiler_contract.Target.One_off_v1
    ~prompt:(Some (S.Catalog_identity.prompt_definition "smoke"))
    ~workspace:(Some (S.Catalog_identity.workspace_definition "physical"))
    ~entrypoint:"main"
    ~input:(`Object [ "path", `String marker; "content", `String "executed" ])
    ~fixtures:None
    ~requested_execution:None
    ~requested_max_output_bytes:None
    ~bounds
    ~checkpoint:ignore
  |> ok
;;

let open_document ~source daemon actor =
  let connection =
    S.Connection_context.create_authenticated
      ~actor
      ~connection_id:P.Id.Transaction.(create () |> to_string)
      ~transport:In_memory
      ~publish_notification:(fun _ -> ())
      ~max_attachments:8
  in
  let transport =
    Agent_client.In_memory.create
      ~notifications:(Eio.Stream.create 8)
      ~request:
        (S.Dispatcher.dispatch_command (S.Daemon.dispatcher daemon) ~context:connection)
      ~close:(fun () -> S.Daemon.close_connection daemon connection)
  in
  let implementation =
    P.Initialize.Implementation.create ~name:"developer-crash" ~version:"1" |> ok
  in
  let initialization =
    P.Initialize.Request.create
      ~implementation
      ~protocol_min:P.Version.current
      ~protocol_max:P.Version.current
      ~features:[ P.Authoring.Features.core; P.Authoring.Features.semantic ]
      ~event_encodings:[ Json ]
      ~max_inbound_event_bytes:(1024 * 1024)
      ()
    |> ok
  in
  (match
     Agent_client.Connection.request_without_history
       transport
       (Protocol_initialize initialization)
   with
   | Ok (Protocol_initialize _) -> ()
   | Ok _ -> F.fail "unexpected developer crash initialization"
   | Error error -> raise_s [%sexp (error : P.Error.t)]);
  let opened =
    P.Authoring.Context.Open_request.create_supplied_document
      ~member:"draft.chatmd"
      ~default_environment:None
      ~contents:
        ("<script id=\"s\" language=\"chatml\" kind=\"tool\">" ^ source ^ "</script>")
    |> ok
  in
  let expected =
    match
      Agent_client.Connection.request_without_history
        transport
        (Authoring (Context_open opened))
    with
    | Ok (Authoring (Context_open context)) ->
      P.Authoring.Context.Expected.of_view context
    | Ok _ -> F.fail "unexpected developer crash context"
    | Error error -> raise_s [%sexp (error : P.Error.t)]
  in
  let analyze =
    P.Authoring.Analysis.Analyze.Request.create
      ~context:expected
      ~target:(P.Authoring.Analysis.Target.selected_script "s" |> ok)
    |> ok
  in
  (match
     Agent_client.Connection.request_without_history
       transport
       (Authoring (Analyze analyze))
   with
   | Ok (Authoring (Analyze _)) -> ()
   | Ok _ -> F.fail "unexpected developer crash analysis"
   | Error error -> raise_s [%sexp (error : P.Error.t)]);
  connection, expected
;;

let actor () =
  let scopes = Set.add Agent_server_test_support.scopes P.Scope.Developer_live_run in
  Agent_server_test_support.principal_with_scopes "pri_developer_crash" scopes
  |> Operator_authorization.trusted_local
;;

let start_daemon ~sw ~root env config =
  let options =
    { S.Daemon.default_options with
      authoring = Some (semantic_authoring_policy env)
    ; developer = Some (developer_configuration ())
    }
  in
  S.Daemon.start
    ~sw
    ~env
    ~config
    ~tool_dir:root
    ~home:root
    ~process_start_identity:(Some "crash-developer-command")
    ~options
    ()
  |> ok
;;

let run_child env ~config_path ~marker ~handoff ~recover =
  let reached filename =
    Eio.Flow.copy_string
      ("crash-developer-effect-written " ^ filename ^ "\n")
      (Eio.Stdenv.stdout env);
    Eio.Fiber.await_cancel ()
  in
  let daemon_env =
    if recover
    then env
    else
      Support.Crash_fault_io.wrap
        env
        ~matches:(String.is_suffix ~suffix:(Filename.basename marker))
        ~boundary:(After_bytes (String.length "\nexecuted"))
        ~reached
  in
  let config = Crash_side_effect_host.load_config daemon_env config_path in
  Eio.Switch.run (fun sw ->
    let daemon = start_daemon ~sw ~root:(Filename.dirname marker) daemon_env config in
    Fun.protect
      ~finally:(fun () -> S.Daemon.shutdown daemon |> ok)
      (fun () ->
         if recover
         then (
           let original = F.read env handoff |> Jsonaf.parse |> Or_error.ok_exn in
           let handoff = handoff_of_json original in
           let request = request ~expected:handoff.expected ~marker in
           let actor = actor () in
           let observed =
             S.Daemon.Developer.observe
               daemon
               ~actor
               ~run_id:handoff.run_id
               ~key:handoff.key
             |> ok
             |> Option.value_exn
           in
           let recovered =
             S.Daemon.Developer.await_terminal
               daemon
               ~actor
               ~run_id:handoff.run_id
               ~key:handoff.key
             |> ok
           in
           let retry =
             S.Daemon.Developer.submit
               daemon
               ~actor
               ~run_id:handoff.run_id
               ~key:handoff.key
               ~request
               ~fresh_source:None
             |> ok
           in
           List.iter [ observed; recovered; retry ] ~f:(fun view ->
             (match Option.map (S.Daemon.Developer.View.receipt view) ~f:R.value with
              | Some (Live Interrupted) -> ()
              | result ->
                raise_s
                  [%sexp "unexpected recovered developer row", (result : R.value option)]);
             (match S.Daemon.Developer.View.phase view with
              | Agent_store.Developer_command_row.Phase.Terminal
                  { execution_started = true; _ } -> ()
              | _ -> F.fail "recovered developer command did not retain Start");
             F.require
               (R.Id.equal handoff.receipt_id (S.Daemon.Developer.View.id view))
               "reopened developer receipt ID changed";
             F.require
               (Chatml_source.Digest.equal
                  handoff.capture_fingerprint
                  (S.Daemon.Developer.View.capture_fingerprint view))
               "reopened developer capture changed";
             F.require
               (Chatml_source.Digest.equal
                  handoff.request_fingerprint
                  (S.Daemon.Developer.View.request_fingerprint view))
               "reopened developer request changed");
           Eio.Flow.copy_string "crash-developer-recovered\n" (Eio.Stdenv.stdout env))
         else (
           let actor = actor () in
           let source =
             {|let main input =
  Task.bind(Tool.call("append_to_file", input), fun result ->
    match result with
    | `Ok(value) -> Task.pure(value)
    | `Error(message) -> Task.fail(message))|}
           in
           let connection, expected = open_document ~source daemon actor in
           let request = request ~expected ~marker in
           let run_id = P.Id.Developer_run.create () in
           let key = P.Idempotency_key.of_string "developer-crash-started" |> checked in
           let submitted =
             S.Daemon.Developer.submit
               daemon
               ~actor
               ~run_id
               ~key
               ~request
               ~fresh_source:(Some (Document_script { connection }))
             |> ok
           in
           F.write
             env
             handoff
             (handoff_json
                { run_id
                ; key
                ; expected
                ; receipt_id = S.Daemon.Developer.View.id submitted
                ; capture_fingerprint =
                    S.Daemon.Developer.View.capture_fingerprint submitted
                ; request_fingerprint =
                    S.Daemon.Developer.View.request_fingerprint submitted
                }
              |> Jsonaf.to_string);
           Eio.Flow.copy_string "crash-developer-handoff-ready\n" (Eio.Stdenv.stdout env);
           let unexpected =
             S.Daemon.Developer.await_terminal daemon ~actor ~run_id ~key |> ok
           in
           raise_s
             [%sexp
               "developer command finished before crash boundary"
             , (Option.map (S.Daemon.Developer.View.receipt unexpected) ~f:R.value
                : R.value option)])))
;;

let test env environment =
  let fixture = F.fixture env environment "crash-developer-command" in
  let module Config = Support.Config_fixture in
  F.write
    env
    (Config.prompt_path fixture)
    "<developer>Developer crash command.</developer>\n\
     <tool name=\"append_to_file\"><write path=\"${workspace}\"/></tool>";
  let config =
    Config.configuration fixture ()
    |> String.substr_replace_all
         ~pattern:"(tool_default deny)"
         ~with_:"(tool_default allow)"
  in
  F.write env (Config.config_path fixture) config;
  let marker =
    Filename.concat (Config.physical_workspace fixture) "developer-crash-effect"
  in
  let handoff =
    Filename.concat (Config.physical_workspace fixture) "developer-handoff.json"
  in
  Eio.Switch.run (fun sw ->
    let arguments =
      [ "developer-command"; Config.config_path fixture; marker; handoff; "false" ]
    in
    let first = F.child ~sw env environment ~case:"developer-command" ~arguments in
    Exn.protect
      ~finally:(fun () -> F.terminate env first)
      ~f:(fun () ->
        F.await_marker env first "crash-developer-handoff-ready";
        F.await_marker env first "crash-developer-effect-written";
        F.kill env first;
        F.require
          (String.equal (F.read env marker) "\nexecuted")
          "developer native effect missing or duplicated before restart";
        let second =
          F.child
            ~sw
            env
            environment
            ~case:"developer-command"
            ~arguments:
              [ "developer-command"; Config.config_path fixture; marker; handoff; "true" ]
        in
        Exn.protect
          ~finally:(fun () -> F.terminate env second)
          ~f:(fun () ->
            F.await_marker env second "crash-developer-recovered";
            let result =
              Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
                PM.await second)
            in
            F.require
              (PM.equal_exit result.exit (Exited 0))
              "developer recovery child failed";
            F.require
              (String.equal (F.read env marker) "\nexecuted")
              "developer native effect replayed after SIGKILL")))
;;
