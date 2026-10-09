open! Core
open Agent_server_test_support
module P = Agent_protocol
module D = Agent_server.Daemon
module S = Agent_store.Session_store

let%expect_test "on-demand reads preserve identity and eager recovery remains for Execute"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let workspace = Filename.concat root "workspace" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
        let prompt = Filename.concat root "root.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt)
          "<developer>Local fixture.</developer>";
        let configuration = config root workspace prompt in
        let calls = ref 0 in
        let start sw mode =
          D.start
            ~sw
            ~env
            ~config:configuration
            ~tool_dir:root
            ~home:root
            ~process_start_identity:None
            ~options:
              { D.default_options with
                startup_mode = mode
              ; inference_policy =
                  inference_policy
                    ~default_model:"fixture-model"
                    ~post_stream:(fun ~sw:_ ~inputs:_ ->
                      Int.incr calls;
                      failwith "unexpected provider")
              }
            ()
          |> protocol_ok
        in
        let session_id =
          Eio.Switch.run (fun sw ->
            let daemon = start sw Execute in
            let client = connection daemon (principal ()) in
            Exn.protect
              ~finally:(fun () ->
                Agent_client.Connection.close client;
                D.shutdown daemon |> protocol_ok)
              ~f:(fun () ->
                initialize client;
                let session, _ = create_session client in
                session.id))
        in
        let index =
          Filename.concat configuration.server.data_dir "indexes/sessions.snapshot"
        in
        Eio.Path.unlink Eio.Path.(Eio.Stdenv.fs env / index);
        Eio.Switch.run (fun sw ->
          let daemon = start sw On_demand in
          Exn.protect
            ~finally:(fun () -> D.shutdown daemon |> protocol_ok)
            ~f:(fun () ->
              let registry = D.registry daemon in
              let state =
                Agent_server.Session_registry.read_state
                  registry
                  session_id
                  ~authorize:(fun session ->
                    if
                      Agent_server.Authorization.session_visible_to (principal ()) session
                    then Ok ()
                    else Error (P.Error.invalid_request "invisible"))
                |> protocol_ok
              in
              let before = Agent_server.Session_registry.stats registry in
              print_s
                [%sexp
                  (( P.Id.Session.equal
                       session_id
                       (Agent_session.Session_state.summary state).id
                   , before.loaded
                   , S.index_was_rebuilt (D.store daemon)
                   , !calls )
                   : bool * int * bool * int)]));
        Eio.Switch.run (fun sw ->
          let daemon = start sw On_demand in
          Exn.protect
            ~finally:(fun () -> D.shutdown daemon |> protocol_ok)
            ~f:(fun () -> print_s [%sexp (S.index_was_rebuilt (D.store daemon) : bool)]));
        Eio.Switch.run (fun sw ->
          let daemon = start sw Execute in
          Exn.protect
            ~finally:(fun () -> D.shutdown daemon |> protocol_ok)
            ~f:(fun () ->
              print_s
                [%sexp
                  (( not (S.index_was_rebuilt (D.store daemon))
                   , (Agent_server.Session_registry.stats (D.registry daemon)).loaded
                   , !calls )
                   : bool * int * int)]))));
  [%expect
    {|
    (true 0 true 0)
    true
    (true 1 0)
  |}]
;;

let%expect_test
    "explicit selection checks actual anchor visibility and keeps stopped identity"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let workspace = Filename.concat root "workspace" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
        let prompt = Filename.concat root "root.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt)
          "<developer>Select retained fixture.</developer>";
        let configuration = config root workspace prompt in
        let calls = ref 0 in
        let start sw mode =
          D.start
            ~sw
            ~env
            ~config:configuration
            ~tool_dir:root
            ~home:root
            ~process_start_identity:None
            ~options:
              { D.default_options with
                startup_mode = mode
              ; inference_policy =
                  inference_policy
                    ~default_model:"fixture-model"
                    ~post_stream:(fun ~sw:_ ~inputs:_ ->
                      Int.incr calls;
                      failwith "unexpected provider")
              }
            ()
          |> protocol_ok
        in
        let id =
          Eio.Switch.run (fun sw ->
            let daemon = start sw Execute in
            let client = connection daemon (principal ()) in
            Exn.protect
              ~finally:(fun () ->
                Agent_client.Connection.close client;
                D.shutdown daemon |> protocol_ok)
              ~f:(fun () ->
                initialize client;
                let session, _ = create_session client in
                session.id))
        in
        Eio.Switch.run (fun sw ->
          let daemon = start sw On_demand in
          Exn.protect
            ~finally:(fun () -> D.shutdown daemon |> protocol_ok)
            ~f:(fun () ->
              let store = D.store daemon in
              let registry = D.registry daemon in
              let indexed () =
                Agent_store.Session_index.find_checked (S.session_index store) id
                |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
                |> protocol_ok
                |> Option.value_exn
              in
              let expected ~revision =
                let entry = indexed () in
                P.Session_lifecycle.Expected.create
                  ~reference:
                    (P.Session_ref.create ~server_id:(S.server_id store) ~session_id:id)
                  ~generation:entry.session.generation
                  ~session_revision:revision
                  ~lifecycle_revision:entry.lifecycle_revision
                |> protocol_ok
              in
              let original = indexed () in
              let foreign =
                principal_with_scopes
                  "pri_other_local"
                  (P.Scope.Set.of_list [ Own_sessions ])
              in
              let forbidden =
                D.select_session
                  daemon
                  ~principal:foreign
                  ~expected:(expected ~revision:original.session.revision)
              in
              let stale =
                D.select_session
                  daemon
                  ~principal:(principal ())
                  ~expected:(expected ~revision:Int64.(original.session.revision + 1L))
              in
              let still_unselected =
                (Agent_server.Session_registry.stats registry).loaded = 0
              in
              let selected =
                D.select_session
                  daemon
                  ~principal:(principal ())
                  ~expected:(expected ~revision:original.session.revision)
                |> protocol_ok
              in
              let current = indexed () in
              let selected_again =
                D.select_session
                  daemon
                  ~principal:(principal ())
                  ~expected:(expected ~revision:current.session.revision)
                |> protocol_ok
              in
              let state =
                Agent_session.Session_actor.state selected.actor |> protocol_ok
              in
              let code = function
                | Ok _ -> "ok"
                | Error (error : P.Error.t) -> P.Error.code_to_string error.code
              in
              print_s
                [%sexp
                  (( code forbidden
                   , code stale
                   , still_unselected
                   , phys_equal selected.actor selected_again.actor
                   , P.Id.Session.equal state.identity.session_id id
                   , state.lifecycle.observed
                   , Agent_server.Runtime_owner.is_loaded selected.runtime
                   , (Agent_server.Session_registry.stats registry).loaded
                   , !calls )
                   : string
                     * string
                     * bool
                     * bool
                     * bool
                     * P.Session.observed_state
                     * bool
                     * int
                     * int)]))));
  [%expect {| (permission_denied conflict true true true Stopped false 1 0) |}]
;;

let%expect_test
    "invalid retained depth rejects after adoption and releases same-ID Handle"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let workspace = Filename.concat root "workspace" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
        let prompt = Filename.concat root "root.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt)
          "<developer>Depth rejection fixture.</developer>";
        let configuration = config root workspace prompt in
        let calls = ref 0 in
        let start sw mode ~depth =
          D.start
            ~sw
            ~env
            ~config:configuration
            ~tool_dir:root
            ~home:root
            ~process_start_identity:None
            ~options:
              { D.default_options with
                startup_mode = mode
              ; factory_limits =
                  { D.default_options.factory_limits with delegation_max_depth = depth }
              ; inference_policy =
                  inference_policy
                    ~default_model:"fixture-model"
                    ~post_stream:(fun ~sw:_ ~inputs:_ ->
                      Int.incr calls;
                      failwith "unexpected provider")
              }
            ()
          |> protocol_ok
        in
        let id =
          Eio.Switch.run (fun sw ->
            let daemon = start sw Execute ~depth:8 in
            let client = connection daemon (principal ()) in
            Exn.protect
              ~finally:(fun () ->
                Agent_client.Connection.close client;
                D.shutdown daemon |> protocol_ok)
              ~f:(fun () ->
                initialize client;
                let session, _ = create_session client in
                session.id))
        in
        let expected store =
          let entry =
            Agent_store.Session_index.find_checked (S.session_index store) id
            |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
            |> protocol_ok
            |> Option.value_exn
          in
          P.Session_lifecycle.Expected.create
            ~reference:
              (P.Session_ref.create ~server_id:(S.server_id store) ~session_id:id)
            ~generation:entry.session.generation
            ~session_revision:entry.session.revision
            ~lifecycle_revision:entry.lifecycle_revision
          |> protocol_ok
        in
        let rejected, retried, unloaded, closed =
          Eio.Switch.run (fun sw ->
            let daemon = start sw On_demand ~depth:0 in
            let store = D.store daemon in
            Exn.protect
              ~finally:(fun () -> D.shutdown daemon |> protocol_ok)
              ~f:(fun () ->
                let reject () =
                  match
                    D.select_session
                      daemon
                      ~principal:(principal ())
                      ~expected:(expected store)
                  with
                  | Error error -> P.Error.equal_code error.code Invalid_request
                  | Ok _ -> false
                in
                let rejected = reject () in
                (* Reacquire the actual actor lock in the same live ownership scope;
                   this fails if selection skipped releasing its adopted Handle. *)
                let handle =
                  S.open_session
                    store
                    ~sw
                    ~actor_lock_nonce:
                      (P.Id.Transaction.create () |> P.Id.Transaction.to_string)
                    id
                  |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
                  |> protocol_ok
                in
                S.close_session store handle
                |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
                |> protocol_ok;
                let retried = reject () in
                let unloaded =
                  (Agent_server.Session_registry.stats (D.registry daemon)).loaded = 0
                in
                D.shutdown daemon |> protocol_ok;
                rejected, retried, unloaded, S.is_closed store))
        in
        let selected =
          Eio.Switch.run (fun sw ->
            let daemon = start sw On_demand ~depth:8 in
            Exn.protect
              ~finally:(fun () -> D.shutdown daemon |> protocol_ok)
              ~f:(fun () ->
                D.select_session
                  daemon
                  ~principal:(principal ())
                  ~expected:(expected (D.store daemon))
                |> protocol_ok
                |> ignore;
                (Agent_server.Session_registry.stats (D.registry daemon)).loaded = 1))
        in
        print_s
          [%sexp
            ((rejected, retried, unloaded, closed, selected, !calls)
             : bool * bool * bool * bool * bool * int)]));
  [%expect {| (true true true true true 0) |}]
;;
