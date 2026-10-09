open! Core
open Agent_server_test_support
module F = Crash_recovery_fixture
module P = Agent_protocol
module C = Agent_client
module A = Agent_session
module S = Agent_server

let file root name = Filename.concat root name

let text text : P.Session.Message_content.t =
  { kind = Plain_text; text; attachments = [] }
;;

let run_child env ~root ~recover =
  let start sw =
    S.Daemon.start
      ~sw
      ~env
      ~config:(config root root (file root "root.chatmd"))
      ~tool_dir:root
      ~home:root
      ~process_start_identity:None
      ~options:
        { S.Daemon.default_options with
          inference_policy =
            inference_policy
              ~default_model:"fixture-model"
              ~post_stream:(fun ~sw:_ ~inputs:_ ->
                F.fail "pending recovery invoked provider")
        }
      ()
    |> F.protocol_ok
  in
  List.iter
    (if recover then [ 1; 2 ] else [ 0 ])
    ~f:(fun restart ->
      Eio.Switch.run (fun sw ->
        let daemon = start sw in
        let client = connection daemon (principal ()) in
        Exn.protect
          ~finally:(fun () ->
            Eio.Cancel.protect (fun () ->
              Exn.protect
                ~finally:(fun () -> S.Daemon.shutdown daemon |> F.protocol_ok)
                ~f:(fun () -> C.Connection.close client)))
          ~f:(fun () ->
            initialize client;
            let attach session_id =
              C.Session_handle.attach
                ~sw
                ~clock:(Eio.Stdenv.clock env)
                ~connection:client
                ~session_id
                ~mode:Read_write
                ~subscribe:false
                ()
              |> F.protocol_ok
            in
            if not recover
            then (
              let session, _ = create_session client in
              let handle = attach session.id in
              C.Session_handle.start handle ~queue_if_limited:false
              |> F.protocol_ok
              |> ignore;
              let entry =
                S.Session_registry.find (S.Daemon.registry daemon) session.id
                |> Option.value_exn
              in
              let held, held_u = Eio.Promise.create () in
              let worker =
                A.Operation_worker.create ~run:(fun ~sw:_ ~input:_ _ ->
                  Eio.Promise.resolve held_u ();
                  Eio.Fiber.await_cancel ())
              in
              A.Session_actor.set_operation_worker entry.actor (Some worker)
              |> F.protocol_ok;
              C.Session_handle.send_message handle (text "acknowledged root")
              |> F.protocol_ok
              |> ignore;
              Eio.Promise.await held;
              let pending =
                C.Session_handle.send_message
                  handle
                  ~timing:After_current_operation
                  (text "adopt after actual interrupted root")
                |> F.protocol_ok
              in
              C.Session_handle.stop handle ~mode:Graceful |> F.protocol_ok |> ignore;
              let state = A.Session_actor.state entry.actor |> F.protocol_ok in
              let operation = Option.value_exn state.active_operation in
              F.require
                (P.Session.equal_desired_state state.lifecycle.desired Stopped)
                "graceful stop did not retain stopped intent";
              F.write env (file root "session-id") (P.Id.Session.to_string session.id);
              F.write
                env
                (file root "pending-id")
                (P.History.Id.to_string pending.history_id);
              F.write
                env
                (file root "operation-id")
                (P.Id.Operation.to_string operation.id);
              Eio.Flow.copy_string "pending-root-acknowledged\n" (Eio.Stdenv.stdout env);
              Eio.Fiber.await_cancel ())
            else (
              let read name = F.read env (file root name) in
              let session_id =
                P.Id.Session.of_string (read "session-id") |> F.protocol_ok
              in
              let history_id =
                P.History.Id.of_string (read "pending-id") |> F.protocol_ok
              in
              let operation_id =
                P.Id.Operation.of_string (read "operation-id") |> F.protocol_ok
              in
              let registry = S.Daemon.registry daemon in
              let before = S.Session_registry.stats registry in
              let lookup =
                C.Connection.request_without_history
                  client
                  (Session_pending_input { session_id; history_id })
                |> F.protocol_ok
              in
              F.require
                (Int.equal before.loaded (S.Session_registry.stats registry).loaded)
                "cold pending query activated a runtime";
              if Int.equal restart 1
              then (
                F.require
                  (match lookup with
                   | Session_pending_input (Pending item) ->
                     (match item.binding with
                      | After_root { operation_id = bound; terminal = None; _ } ->
                        P.Id.Operation.equal bound operation_id
                      | Safe_boundary | Await_idle | After_root { terminal = Some _; _ }
                        -> false)
                   | _ -> false)
                  "cold inspection invented terminal release";
                let handle = attach session_id in
                let entry =
                  S.Session_registry.find registry session_id |> Option.value_exn
                in
                let state = A.Session_actor.state entry.actor |> F.protocol_ok in
                let input =
                  List.hd_exn state.conversation.deferred_user_entries
                  |> A.Pending_input_document.value
                in
                F.require
                  (match P.Pending_input.binding input with
                   | After_root { terminal = Some proof; _ } ->
                     P.Id.Operation.equal
                       (P.Pending_input.Terminal_proof.operation_id proof)
                       operation_id
                     &&
                       (match
                          P.Json_codec.optional_as
                            (P.Json_codec.fields
                               (P.Pending_input.Terminal_proof.to_json proof)
                             |> F.protocol_ok)
                            "outcome"
                            P.Json_codec.string
                          |> F.protocol_ok
                        with
                       | Some "interrupted" -> true
                       | _ -> false)
                   | Safe_boundary | Await_idle | After_root { terminal = None; _ } ->
                     false)
                  "actual reopen did not record matching Interrupted proof";
                A.Session_actor.adopt_deferred entry.actor |> F.protocol_ok |> ignore;
                let still_stopped = A.Session_actor.state entry.actor |> F.protocol_ok in
                F.require
                  (List.equal
                     A.Pending_input_document.equal
                     state.conversation.deferred_user_entries
                     still_stopped.conversation.deferred_user_entries
                   && List.equal
                        P.History.equal_entry
                        state.conversation.canonical_history
                        still_stopped.conversation.canonical_history
                   && Int64.equal state.counters.revision still_stopped.counters.revision
                  )
                  "stopped recovery adopted pending input";
                let resumed_worker =
                  A.Operation_worker.create ~run:(fun ~sw:_ ~input _ ->
                    Completed
                      { final_history = input.history
                      ; runtime_requests = []
                      ; moderator_snapshot = None
                      })
                in
                A.Session_actor.set_operation_worker entry.actor (Some resumed_worker)
                |> F.protocol_ok;
                C.Session_handle.start handle ~queue_if_limited:false
                |> F.protocol_ok
                |> ignore;
                A.Session_actor.adopt_deferred entry.actor |> F.protocol_ok |> ignore;
                let first_adoption = A.Session_actor.state entry.actor |> F.protocol_ok in
                A.Session_actor.adopt_deferred entry.actor |> F.protocol_ok |> ignore;
                let resumed = A.Session_actor.state entry.actor |> F.protocol_ok in
                F.require
                  (List.is_empty resumed.conversation.deferred_user_entries
                   && List.equal
                        P.History.equal_entry
                        first_adoption.conversation.canonical_history
                        resumed.conversation.canonical_history
                   && P.Pending_input.Revision.equal
                        first_adoption.conversation.pending_revision
                        resumed.conversation.pending_revision)
                  "second boundary adopted an input twice";
                F.require
                  (List.count resumed.conversation.canonical_history ~f:(fun entry ->
                     P.History.Id.equal entry.id history_id)
                   = 1)
                  "resume did not preserve exactly one adopted canonical occurrence";
                C.Session_handle.stop handle ~mode:Cancel |> F.protocol_ok |> ignore;
                C.Session_handle.detach handle |> F.protocol_ok)
              else
                F.require
                  (match lookup with
                   | Session_pending_input
                       (Adopted { history_id = known; current = Some entry; _ }) ->
                     P.History.Id.equal known history_id
                     && P.History.Id.equal entry.id history_id
                   | _ -> false)
                  "second physical reopen lost known adoption"))));
  if recover
  then
    Eio.Flow.copy_string "pending-interrupted-recovery-passed\n" (Eio.Stdenv.stdout env)
;;

let test env environment =
  let root =
    Filename.concat
      (Support.Temporary_environment.roots environment).temporary
      "pending-interrupted-recovery"
  in
  Eio.Path.mkdir ~perm:0o700 (F.path env root);
  F.write
    env
    (file root "root.chatmd")
    "<developer>Pending Interrupted recovery.</developer>";
  Eio.Switch.run (fun sw ->
    let child =
      F.child
        ~sw
        env
        environment
        ~case:"pending-input"
        ~arguments:[ "pending-input"; root ]
    in
    Exn.protect
      ~finally:(fun () -> F.terminate env child)
      ~f:(fun () ->
        F.await_marker env child "pending-root-acknowledged";
        F.kill env child);
    let recovery =
      F.child
        ~sw
        env
        environment
        ~case:"pending-input-recover"
        ~arguments:[ "pending-input-recover"; root ]
    in
    Exn.protect
      ~finally:(fun () -> F.terminate env recovery)
      ~f:(fun () ->
        F.await_marker env recovery "pending-interrupted-recovery-passed";
        let result =
          Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
            Support.Process_manager.await recovery)
        in
        F.require
          (Support.Process_manager.equal_exit result.exit (Exited 0))
          "pending recovery child did not exit successfully"))
;;
