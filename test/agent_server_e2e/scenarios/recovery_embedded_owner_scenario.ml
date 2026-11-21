open! Core
module P = Agent_protocol
module S = Agent_server
module Embedded = S.Embedded
module Context = S.Connection_context
module Dispatcher = S.Dispatcher
module Client = Agent_client.Recovery_windows
module Fixture = Crash_recovery_fixture
module Config = Support.Config_fixture
module Temp = Support.Temporary_environment
module Wire = P.Recovery_rollover_control

let ok = Fixture.protocol_ok
let require = Fixture.require
let key = Fixture.key

let features =
  [ P.Recovery_query.Features.windows
  ; Wire.feature
  ; P.Recovery_note_share_control.feature
  ]
;;

let expect_error result code =
  match result with
  | Error (failure : P.Error.t) when P.Error.equal_code failure.code code -> ()
  | Error failure ->
    raise_s [%sexp "unexpected local recovery error", (failure : P.Error.t)]
  | Ok _ -> Fixture.fail "local recovery unexpectedly succeeded"
;;

let initialize connection =
  Agent_client.Session_handle.initialize_with_features
    connection
    ~implementation_name:"local-recovery-owner-e2e"
    ~implementation_version:"dev"
    ~features
  |> ok
;;

let attachment connection session_id ~mode ~name =
  match
    Agent_client.Connection.request
      connection
      (Session_attach
         { session_id
         ; requested_mode = mode
         ; subscribe = false
         ; after_sequence = None
         ; reclaim_token = None
         ; idempotency_key = key name
         })
    |> ok
  with
  | Session_attach result -> result.attachment.id
  | _ -> Fixture.fail "local recovery fixture did not attach a writer"
;;

let create connection spec ~name =
  match
    Agent_client.Connection.request
      connection
      (Session_create
         { spec; requested_mode = None; subscribe = false; idempotency_key = key name })
    |> ok
  with
  | Session_create result -> result.session
  | _ -> Fixture.fail "local recovery fixture did not create a session"
;;

let check_declared_daemon_denied connection ~server_id options =
  let local = Embedded.session_spec options |> ok in
  let spec =
    P.Session.Spec.create
      ~execution_host:Daemon
      ~prompt:local.prompt
      ~workspace:local.workspace
      ~liveness:Detached
      ~persistence:Durable
      ?permission_profile:local.permission_profile
      ~start_immediately:false
      ~labels:[]
      ()
    |> ok
  in
  let session = create connection spec ~name:"claimed-daemon-session" in
  let session_ref = P.Session_ref.create ~server_id ~session_id:session.id in
  let writer =
    attachment connection session.id ~mode:Owner_read_write ~name:"claimed-daemon-writer"
  in
  expect_error
    (Client.enable
       connection
       { session = session_ref
       ; attachment_id = writer
       ; expected_generation = session.generation
       })
    Permission_denied
;;

let with_context host actor f =
  let context =
    Context.create_authenticated
      ~actor
      ~connection_id:(P.Id.Transaction.create () |> P.Id.Transaction.to_string)
      ~transport:In_memory
      ~publish_notification:ignore
      ~max_attachments:8
  in
  let dispatcher = Embedded.host_dispatcher host in
  let connection =
    Agent_client.Transport.create
      ~request:(Dispatcher.dispatch_command dispatcher ~context)
      ~next_notification:(fun () -> None)
      ~close:(fun () -> Embedded.host_close_connection host context)
    |> Agent_client.Connection.create
  in
  Exn.protect
    ~f:(fun () -> f connection dispatcher context)
    ~finally:(fun () -> Agent_client.Connection.close connection)
;;

let prepare_enable dispatcher context request =
  let command = P.Command.Recovery_enable request in
  let envelope =
    P.Envelope.request
      ~id:(P.Envelope.Request_id.of_json (`String "queued-local-enrollment") |> ok)
      ~method_:(P.Command.method_name command)
      ~params:(P.Command.params command)
      ()
  in
  Dispatcher.dispatch_output dispatcher ~context envelope |> ok |> Option.value_exn
;;

let options environment fixture storage : Embedded.options =
  let roots = Temp.roots environment in
  { prompt_file = Config.prompt_path fixture
  ; workspace = Config.physical_workspace fixture
  ; tool_dir = roots.root
  ; home = Some roots.home
  ; storage
  ; start_immediately = false
  ; permission_profile = Embedded.default_permission_profile
  ; attachment_mode = Read_write
  ; event_capacity = 64
  }
;;

let durable env daemon_options recovery_capacity options =
  Eio.Switch.run (fun sw ->
    Embedded.with_local_host ~sw ~env ~daemon_options ~recovery_capacity options ~f:(fun host ->
      let connection = Embedded.host_connection host in
      let initialized = initialize connection in
      let spec = Embedded.session_spec options |> ok in
      let session = create connection spec ~name:"durable-local-enrollment" in
      let reference =
        P.Session_ref.create ~server_id:initialized.server_id ~session_id:session.id
      in
      let request attachment_id =
        P.Recovery_query.Enable.
          { session = reference; attachment_id; expected_generation = session.generation }
      in
      let ordinary =
        attachment connection session.id ~mode:Read_write ~name:"ordinary-local-writer"
      in
      expect_error (Client.enable connection (request ordinary)) Permission_denied;
      check_declared_daemon_denied connection ~server_id:initialized.server_id options;
      let fake =
        Operator_authorization.durable_local
          ~host:(Operator_authorization.Durable_local_host.create ())
          ~server_id:initialized.server_id
          ~principal:initialized.principal
          ~is_current:(fun () -> true)
      in
      with_context host fake (fun other _ _ ->
        ignore (initialize other : P.Initialize.Response.t);
        let writer =
          attachment other session.id ~mode:Owner_read_write ~name:"foreign-host-proof"
        in
        expect_error (Client.enable other (request writer)) Permission_denied);
      let local = Embedded.host_connection_authority host in
      with_context host local (fun original dispatcher context ->
        ignore (initialize original : P.Initialize.Response.t);
        let writer =
          attachment original session.id ~mode:Owner_read_write ~name:"exact-local-proof"
        in
        let selected = request writer in
        expect_error
          (Client.enable
             original
             { selected with expected_generation = session.generation + 1 })
          Conflict;
        let other_origin = Embedded.host_connection_authority host in
        expect_error
          (Dispatcher.dispatch_command
             dispatcher
             ~actor:other_origin
             ~context
             (Recovery_enable selected))
          Method_not_found;
        expect_error (Client.enable connection selected) Permission_denied;
        let output = prepare_enable dispatcher context selected in
        require
          (Dispatcher.Output.requires_check output)
          "local enrollment lost its release proof";
        Context.close_authoring context;
        match Dispatcher.Output.materialize output with
        | Response { outcome = Error failure; _ } ->
          require
            (P.Error.equal_code failure.code Permission_denied)
            "revoked local authority returned an unexpected release error"
        | _ -> Fixture.fail "revoked local authority disclosed the queued window");
      let owner =
        attachment
          connection
          session.id
          ~mode:Owner_read_write
          ~name:"fresh-local-connection"
      in
      let scope = Client.enable connection (request owner) |> ok in
      let windows =
        Client.list
          connection
          { session = reference
          ; generation = None
          ; state = None
          ; page = P.Page.Request.create ~limit:10 () |> ok
          }
        |> ok
      in
      require
        (List.length windows.items = 1
         && P.Recovery_window.Scope.equal scope (List.hd_exn windows.items).scope)
        "local retry did not preserve the single accepted window";
      let share_source =
        P.Recovery_note_share_control.Source.create
          reference
          ~attachment_id:owner
          ~expected_generation:session.generation
        |> ok
      in
      expect_error
        (Client.Note_shares.inspect connection share_source ())
        Permission_denied;
      (match
         Agent_client.Connection.request_without_history
           connection
           (Session_start
              { session_id = session.id
              ; attachment_id = owner
              ; queue_if_limited = false
              ; idempotency_key = key "local-owner-start"
              })
         |> ok
       with
       | Session_start _ -> ()
       | _ -> Fixture.fail "local recovery fixture could not start the retained session");
      Support.Background_fixture.await env "local recovery start" (fun () ->
        let snapshot = Agent_client.Admin.get_session connection session.id |> ok in
        match (P.Public.Snapshot.fields snapshot).session.observed_state with
        | Idle -> Some ()
        | _ -> None);
      let inspect attachment_id =
        Client.inspect_rollover
          connection
          { session = reference; attachment_id = Some attachment_id; key = None }
        |> ok
      in
      let basis =
        match (inspect owner).view, (inspect ordinary).view with
        | Available { current; actions = accepted; _ }, Available { actions = denied; _ }
          ->
          require
            (accepted.rollover
             && accepted.force
             && (not denied.rollover)
             && not denied.force)
            "local status did not distinguish current owner and ordinary writer";
          current
        | _ -> Fixture.fail "local enrollment did not expose rollover status"
      in
      let rollover =
        Wire.Request.
          { expected = basis
          ; attachment_id = owner
          ; key = key "local-window-rollover"
          ; force = false
          }
      in
      expect_error
        (Client.request_rollover
           connection
           { rollover with attachment_id = ordinary; force = true })
        Permission_denied;
      let receipt = Client.request_rollover connection rollover |> ok in
      (match receipt.outcome with
       | Failed Checkpoint_required -> ()
       | _ -> Fixture.fail "actual local rollover did not require its normal checkpoint");
      Ok ())
    |> ok)
;;

let transient env daemon_options options =
  Eio.Switch.run (fun sw ->
    Embedded.with_local_host ~sw ~env ~daemon_options options ~f:(fun host ->
      let connection = Embedded.host_connection host in
      let initialized = initialize connection in
      let spec = Embedded.session_spec options |> ok in
      let session = create connection spec ~name:"transient-enrollment" in
      let session_ref =
        P.Session_ref.create ~server_id:initialized.server_id ~session_id:session.id
      in
      let writer =
        attachment connection session.id ~mode:Owner_read_write ~name:"transient-writer"
      in
      (match
         Client.enable
           connection
           { session = session_ref
           ; attachment_id = writer
           ; expected_generation = session.generation
           }
       with
       | Error { code = Permission_denied | Invalid_state; _ } -> ()
       | Error failure ->
         raise_s [%sexp "unexpected transient recovery failure", (failure : P.Error.t)]
       | Ok _ -> Fixture.fail "transient local host enrolled a durable window");
      check_declared_daemon_denied connection ~server_id:initialized.server_id options;
      Ok ())
    |> ok)
;;

let run env environment =
  let fixture = Fixture.fixture env environment "recovery-embedded-owner" in
  Support.Provider_fixture.with_host ~env fixture (fun inference_host ->
    let root = S.Local_storage.Root.create ~path:(Config.data_dir fixture) () |> ok in
    let path = Filename.concat (Filename.dirname (Config.config_path fixture)) "capacity.sexp" in
    Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(Eio.Stdenv.fs env / path)
      "((mode manual) (host_total 8192))";
    let capacity =
      match S.Config_validator.load_recovery_capacity ~env ~path with
      | Ok (Some capacity) -> capacity
      | Ok None -> Fixture.fail "manual local capacity was disabled"
      | Error diagnostics -> raise_s [%sexp (diagnostics : S.Config.Diagnostic.t list)]
    in
    let daemon_options = Inference_composition.daemon_options inference_host in
    let selected = options environment fixture (Durable root) in
    durable env daemon_options capacity selected;
    transient env daemon_options (options environment fixture Transient))
;;
