open! Core
open Support
module T = Agent_server_test_support
module Daemon = Agent_server.Daemon
module Host = Agent_server.Recurrence_execution_host
module Configuration = Agent_server.Recurrence_execution_configuration
module Registry = Agent_server.Session_registry
module Owner = Agent_server.Runtime_owner
module Client = Agent_client
module Embedded = Agent_server.Embedded

let query : P.Session.List_request.t =
  { page = P.Page.Request.create ~limit:10 () |> protocol_ok
  ; desired_state = None
  ; prompt_id = None
  ; workspace_id = None
  ; owner_principal_id = None
  ; organization = P.Session_organization.Query.default
  ; labels = []
  ; sort = P.Session_catalog_query.Sort.default
  ; archive = Active
  ; creator_principal_id = None
  ; active_owner_principal_id = None
  }
;;

let expected daemon (row : P.Session_catalog.t) =
  P.Session_lifecycle.Expected.create
    ~reference:
      (P.Session_ref.create
         ~server_id:(S.server_id (Daemon.store daemon))
         ~session_id:row.session.id)
    ~generation:row.session.generation
    ~session_revision:row.session.revision
    ~lifecycle_revision:row.lifecycle_revision
  |> protocol_ok
;;

let%expect_test
    "configured daemon execution does not select owners and closes with its real host"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = T.temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let workspace = Filename.concat root "workspace" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
        let prompt = Filename.concat root "controller.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt)
          {|<developer>Configured host selection is not initialization.</developer>
<script id="controller" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = Task.pure(state)
</script>|};
        let base = T.config root workspace prompt in
        let base =
          { base with
            workspaces =
              List.map base.workspaces ~f:(fun workspace ->
                { workspace with
                  prompt_limits =
                    List.map workspace.prompt_limits ~f:(fun limit ->
                      { limit with max_root_agents = 2 })
                })
          }
        in
        let with_daemon mode config f =
          Daemon_tests.with_daemon env root config mode ~mark:ignore f
        in
        let first, second =
          with_daemon Execute base (fun _ daemon ->
            assert (Option.is_none (Daemon.recurrence_execution_host daemon));
            let client = T.connection daemon (T.principal ()) in
            Exn.protect
              ~finally:(fun () -> Client.Connection.close client)
              ~f:(fun () ->
                T.initialize client;
                let first, _ =
                  T.create_session
                    ~start_immediately:false
                    ~key:"541-first-controller"
                    client
                in
                let second, _ =
                  T.create_session
                    ~start_immediately:false
                    ~key:"541-second-controller"
                    client
                in
                assert (not (P.Id.Session.equal first.id second.id));
                first.id, second.id))
        in
        let execution =
          Configuration.create
            ~principal_id:(P.Id.Principal.of_string "pri_541_daemon" |> protocol_ok)
            ~controllers:
              (List.map [ first; second ] ~f:(fun id ->
                 Configuration.Controller.create
                   ~session_id:id
                   ~existing_targets:[ id ]
                   ~captured_children:false
                 |> protocol_ok))
            ~expires_at:None
          |> protocol_ok
        in
        let config =
          { base with
            server = { base.server with recurrence_execution = Some execution }
          }
        in
        List.iter [ Daemon.On_demand; Operator_only ] ~f:(fun mode ->
          with_daemon mode config (fun _ daemon ->
            [%test_eq: int] 0 (Registry.stats (Daemon.registry daemon)).loaded;
            let client = T.connection daemon (T.principal ()) in
            Exn.protect
              ~finally:(fun () -> Client.Connection.close client)
              ~f:(fun () ->
                T.initialize client;
                let catalog =
                  Client.Admin.list_sessions_page client query |> protocol_ok
                in
                [%test_eq: int] 2 (List.length catalog.items);
                [%test_eq: int] 0 (Registry.stats (Daemon.registry daemon)).loaded;
                let row =
                  List.find_exn catalog.items ~f:(fun row ->
                    P.Id.Session.equal row.session.id first)
                in
                match mode with
                | On_demand ->
                  let host = Option.value_exn (Daemon.recurrence_execution_host daemon) in
                  let selected =
                    Daemon.select_session
                      daemon
                      ~principal:(T.principal ())
                      ~expected:(expected daemon row)
                    |> protocol_ok
                  in
                  [%test_eq: int] 1 (Registry.stats (Daemon.registry daemon)).loaded;
                  assert (Option.is_none (Registry.find (Daemon.registry daemon) second));
                  assert (not (Owner.is_loaded selected.runtime));
                  let before = state selected.actor in
                  let controller = Host.controller host selected |> protocol_ok in
                  let target =
                    Host.existing_target controller ~target:selected |> protocol_ok
                  in
                  let proof = Host.Target.actor target in
                  assert (Operator_authorization.is_current proof);
                  Host.Target.authorize target before |> protocol_ok;
                  assert (not (Owner.is_loaded selected.runtime));
                  let after = state selected.actor in
                  assert (
                    Jsonaf.exactly_equal
                      (T.state_document before
                       |> store_ok
                       |> Document_schema.Document.json)
                      (T.state_document after |> store_ok |> Document_schema.Document.json));
                  assert (List.is_empty after.moderator_executions);
                  Option.iter after.run_state ~f:(fun index ->
                    assert (List.is_empty (A.Run_state.runs index));
                    assert (List.is_empty (A.Run_state.receipts index)));
                  Daemon.shutdown daemon |> protocol_ok;
                  assert (not (Operator_authorization.is_current proof));
                  (match Host.Target.authorize target before with
                   | Error error ->
                     [%test_eq: P.Error.code] Permission_denied error.code;
                     assert (not error.retryable)
                   | Ok () -> failwith "shutdown retained execution authority")
                | Operator_only ->
                  assert (Option.is_none (Daemon.recurrence_execution_host daemon));
                  (match
                     Daemon.select_session
                       daemon
                       ~principal:(T.principal ())
                       ~expected:(expected daemon row)
                   with
                   | Error error -> [%test_eq: P.Error.code] Invalid_state error.code
                   | Ok _ -> failwith "operator-only selected an executable owner");
                  [%test_eq: int] 0 (Registry.stats (Daemon.registry daemon)).loaded
                | Execute -> assert false)));
        (* Explicit embedded Config uses the same real daemon composition. Its
           opaque public host does not expose an alternate execution issuer. *)
        Eio.Switch.run (fun sw ->
          let host =
            Embedded.open_host
              ~sw
              ~env
              ~startup_mode:On_demand
              ~config
              ~tool_dir:root
              ~home:root
              ~event_capacity:32
              ()
            |> protocol_ok
          in
          Exn.protect
            ~finally:(fun () -> Embedded.close_host host)
            ~f:(fun () ->
              let catalog =
                Client.Admin.list_sessions (Embedded.host_connection host) |> protocol_ok
              in
              [%test_eq: int] 2 (List.length catalog);
              List.iter catalog ~f:(fun session ->
                assert (P.Session.equal_desired_state session.desired_state Stopped);
                match session.observed_state with
                | Stopped -> ()
                | _ -> raise_s [%sexp (session : P.Session.t)])));
        (* Joined embedded close releases the actual data-root owner. *)
        with_daemon Operator_only config (fun _ daemon ->
          assert (Option.is_none (Daemon.recurrence_execution_host daemon));
          [%test_eq: int] 0 (Registry.stats (Daemon.registry daemon)).loaded)));
  print_endline
    "configuration leaves both controllers unselected; On_demand selects exactly one \
     without warming it";
  print_endline
    "shutdown revokes its original; Operator_only issues none; explicit embedded Config \
     closes its real store owner";
  [%expect
    {|
    configuration leaves both controllers unselected; On_demand selects exactly one without warming it
    shutdown revokes its original; Operator_only issues none; explicit embedded Config closes its real store owner
    |}]
;;
