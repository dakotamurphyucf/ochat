open! Core
open Agent_server_test_support
module P = Agent_protocol
module C = Agent_client
module S = Agent_server
module A = Agent_session

let authenticated_connection daemon ~actor =
  let notifications = Eio.Stream.create 256 in
  let context =
    S.Connection_context.create_authenticated
      ~actor
      ~connection_id:(P.Id.Attachment.create () |> P.Id.Attachment.to_string)
      ~transport:In_memory
      ~publish_notification:(Eio.Stream.add notifications)
      ~max_attachments:64
  in
  C.In_memory.create
    ~request:(fun command ->
      S.Dispatcher.dispatch_command (S.Daemon.dispatcher daemon) ~context command)
    ~notifications
    ~close:(fun () -> S.Daemon.close_connection daemon context)
;;

let%expect_test "host RPC workflow authority survives disconnect and receipt reconnect" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let workspace = Filename.concat root "workspace" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
        let prompt_file = Filename.concat root "root.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt_file)
          {|<developer>Host run fixture.</developer>
<script id="host_run" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = []
let on_event = fun ctx state event -> match event with
| `Session_start -> Task.bind(Run.finish(`Object([
  { key = "kind"; value = `String("finish") },
  { key = "terminal"; value = `Object([{ key = "kind"; value = `String("completed") }]) },
  { key = "relinquish"; value = `Array([]) }])), fun ignored -> Task.pure(state))
| _ -> Task.pure(state)
</script>|};
        Eio.Switch.run (fun sw ->
          let daemon =
            Catalog_service_tests.start_catalog_daemon
              sw
              env
              ~root
              (config root workspace prompt_file)
          in
          let clients = ref [] in
          Exn.protect
            ~finally:(fun () ->
              List.iter !clients ~f:C.Connection.close;
              S.Daemon.shutdown daemon |> protocol_ok)
            ~f:(fun () ->
              let authorized = ref true in
              let actor =
                Operator_authorization.guarded
                  ~principal:(principal ())
                  ~is_current:(fun () -> !authorized)
              in
              let client = authenticated_connection daemon ~actor in
              clients := client :: !clients;
              initialize client;
              let session, attachment = create_session client in
              let entry =
                S.Session_registry.find (S.Daemon.registry daemon) session.id
                |> Option.value_exn
              in
              let state = A.Session_actor.state entry.actor |> protocol_ok in
              let request =
                P.Run_start.create
                  ~session_id:session.id
                  ~attachment_id:attachment.id
                  ~generation:state.identity.generation
                  ~expected_revision:state.counters.revision
                  ~mode:Workflow
                  ~input:Authored_start
                  ~key:(P.Idempotency_key.of_string "host-run-start" |> protocol_ok)
                |> protocol_ok
              in
              let command = P.Command.Session_run_start request in
              let admitted =
                match
                  C.Connection.request_without_history client command |> protocol_ok
                with
                | Session_run_start receipt -> receipt
                | _ -> failwith "run admission result"
              in
              C.Connection.close client;
              (* Invoke the actual pending lifecycle capability after transport closure.
             Its existing gate also safely joins any supervisor activation. *)
              S.Runtime_owner.with_background_runtime entry.runtime (fun runtime ->
                match runtime.moderator_activation with
                | Some activation -> activation.run ()
                | None -> Error (P.Error.invalid_request "missing startup capability"))
              |> protocol_ok
              |> ignore;
              let state = A.Session_actor.state entry.actor |> protocol_ok in
              let index = Option.value_exn state.run_state in
              let run = A.Run_state.find index admitted.run_id |> Option.value_exn in
              let reconnected = authenticated_connection daemon ~actor in
              clients := reconnected :: !clients;
              initialize reconnected;
              let receipt =
                C.Connection.request_without_history
                  reconnected
                  (Command_receipt
                     { method_name = P.Command.method_name command
                     ; original_params = P.Command.params command
                     })
                |> protocol_ok
              in
              let exact_receipt =
                match receipt with
                | Command_receipt (Committed (Accepted_run { session_id; receipt })) ->
                  P.Id.Session.equal session_id session.id
                  && P.Run_receipt.equal receipt admitted
                | _ -> false
              in
              authorized := false;
              let revoked_receipt_rejected =
                match
                  C.Connection.request_without_history
                    reconnected
                    (Command_receipt
                       { method_name = P.Command.method_name command
                       ; original_params = P.Command.params command
                       })
                with
                | Error { code = Permission_denied; _ } -> true
                | Error _ | Ok _ -> false
              in
              authorized := true;
              print_s
                [%sexp
                  { completed =
                      (P.Run.Lifecycle.equal run.lifecycle (Terminal (Completed None))
                       : bool)
                  ; terminal_receipt_once =
                      (Int.equal
                         1
                         (List.count (A.Run_state.receipts index) ~f:(fun receipt ->
                            P.Run_receipt.Kind.equal receipt.kind Terminal))
                       : bool)
                  ; exact_receipt : bool
                  ; revoked_receipt_rejected : bool
                  ; session_running =
                      (P.Session.equal_desired_state state.lifecycle.desired Running
                       : bool)
                  }];
              C.Connection.close reconnected))));
  [%expect
    {|
    ((completed true) (terminal_receipt_once true) (exact_receipt true)
     (revoked_receipt_rejected true) (session_running true))
    |}]
;;

let%expect_test
    "cold host user run checks original CAS before loading and reserves input once"
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
        let prompt_file = Filename.concat root "root.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt_file)
          {|<developer>Cold host run fixture.</developer>
<script id="host_cold_run" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = []
let on_event = fun ctx state event -> Task.pure(state)
</script>|};
        Eio.Switch.run (fun sw ->
          let daemon =
            Catalog_service_tests.start_catalog_daemon
              sw
              env
              ~root
              (config root workspace prompt_file)
          in
          let actor = Operator_authorization.trusted_local (principal ()) in
          let client = authenticated_connection daemon ~actor in
          Exn.protect
            ~finally:(fun () ->
              C.Connection.close client;
              S.Daemon.shutdown daemon |> protocol_ok)
            ~f:(fun () ->
              initialize client;
              let session, attachment = create_session client in
              let entry =
                S.Session_registry.find (S.Daemon.registry daemon) session.id
                |> Option.value_exn
              in
              (* A user run requires an existing available session. Start it through
                 the host command and join its one genuine startup capability before
                 unloading only the runtime resources. *)
              C.Connection.request_without_history
                client
                (Session_start
                   { session_id = session.id
                   ; attachment_id = attachment.id
                   ; queue_if_limited = false
                   ; idempotency_key =
                       P.Idempotency_key.of_string "cold-session-start" |> protocol_ok
                   })
              |> protocol_ok
              |> ignore;
              S.Runtime_owner.with_background_runtime entry.runtime (fun runtime ->
                match runtime.moderator_activation with
                | Some activation -> activation.run ()
                | None -> Error (P.Error.invalid_request "missing startup capability"))
              |> protocol_ok
              |> ignore;
              S.Runtime_owner.unload_and_wait entry.runtime |> protocol_ok;
              let before = A.Session_actor.state entry.actor |> protocol_ok in
              (match
                 ( before.lifecycle.desired
                 , before.lifecycle.observed
                 , before.active_operation )
               with
               | Running, Idle, None -> ()
               | _ -> failwith "cold user-run fixture requires a running idle session");
              let make ~revision ~key =
                P.Run_start.create
                  ~session_id:session.id
                  ~attachment_id:attachment.id
                  ~generation:before.identity.generation
                  ~expected_revision:revision
                  ~mode:Single_turn
                  ~input:
                    (User_submission
                       { kind = Plain_text; text = "cold run input"; attachments = [] })
                  ~key:(P.Idempotency_key.of_string key |> protocol_ok)
                |> protocol_ok
              in
              let stale =
                make ~revision:(Int64.pred before.counters.revision) ~key:"cold-stale"
              in
              let stale_rejected =
                Result.is_error
                  (C.Connection.request_without_history client (Session_run_start stale))
              in
              let rejected = A.Session_actor.state entry.actor |> protocol_ok in
              let rejected_without_loading =
                not (S.Runtime_owner.is_loaded entry.runtime)
              in
              let rejected_without_reservation =
                Int64.equal
                  before.conversation.next_history_sequence
                  rejected.conversation.next_history_sequence
              in
              let original = make ~revision:before.counters.revision ~key:"cold-fresh" in
              let command = P.Command.Session_run_start original in
              let receipt =
                match
                  C.Connection.request_without_history client command |> protocol_ok
                with
                | Session_run_start receipt -> receipt
                | _ -> failwith "cold admission result"
              in
              let admitted = A.Session_actor.state entry.actor |> protocol_ok in
              let replay =
                match
                  C.Connection.request_without_history client command |> protocol_ok
                with
                | Session_run_start receipt -> receipt
                | _ -> failwith "cold replay result"
              in
              let replayed = A.Session_actor.state entry.actor |> protocol_ok in
              let user_entries (state : A.Session_state.t) =
                List.filter state.conversation.canonical_history ~f:(fun entry ->
                  P.History.equal_role entry.role User)
                |> List.map ~f:(fun entry -> entry.id)
              in
              let constructor_and_input_reserved =
                Int64.(
                  admitted.conversation.next_history_sequence
                  > before.conversation.next_history_sequence + 1L)
              in
              let one_user_entry = Int.equal 1 (List.length (user_entries admitted)) in
              let exact_original_receipt = P.Run_receipt.equal receipt replay in
              let replay_did_not_reserve =
                Int64.equal
                  admitted.conversation.next_history_sequence
                  replayed.conversation.next_history_sequence
                && List.equal
                     P.History.Id.equal
                     (user_entries admitted)
                     (user_entries replayed)
              in
              print_s
                [%sexp
                  { stale_rejected : bool
                  ; rejected_without_loading : bool
                  ; rejected_without_reservation : bool
                  ; constructor_and_input_reserved : bool
                  ; one_user_entry : bool
                  ; exact_original_receipt : bool
                  ; replay_did_not_reserve : bool
                  }]))));
  [%expect
    {|
    ((stale_rejected true) (rejected_without_loading true)
     (rejected_without_reservation true) (constructor_and_input_reserved true)
     (one_user_entry true) (exact_original_receipt true)
     (replay_did_not_reserve true))
    |}]
;;
