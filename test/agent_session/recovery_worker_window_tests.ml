open! Core
open Fixtures
open Recovery_window_actor_tests
module Worker = A.Operation_worker
module Cleanup = A.Callback_cleanup

let assert_conflict = function
  | Error { P.Error.code = Conflict; _ } -> ()
  | Ok _ | Error _ -> failwith "expected foreground publication conflict"
;;

let prepare_successor fixture base ~retained ~archived_tail =
  let original =
    { base with
      State.conversation =
        { base.State.conversation with
          canonical_history = retained @ [ archived_tail ]
        ; next_history_sequence = 4L
        ; reserved_history_through = 4L
        }
    }
  in
  let root =
    A.Recovery_window_capture.bootstrap
      original
      ~owner:fixture.owner
      ~window:(P.Id.Context_window.create ())
    |> protocol_ok
  in
  let before = { original with recovery_windows = Some root } in
  let predecessor = scope before in
  let successor =
    Scope.create
      ~session:fixture.owner
      ~generation:base.identity.generation
      ~window:(P.Id.Context_window.create ())
    |> protocol_ok
  in
  let document = A.Session_state_document.authored before in
  let archive =
    Archive.reference_for document ~kind:Rollover ~limits:document_limits operation_id
    |> protocol_ok
  in
  Archive.write
    ~env:fixture.env
    ~handle:fixture.handle
    ~max_payload_length:payload_limit
    archive
    document
  |> protocol_ok;
  let recovery_windows =
    A.Recovery_window_seal.prepare before ~predecessor ~successor ~archive ~retained
    |> protocol_ok
  in
  let successor =
    { before with
      recovery_windows = Some recovery_windows
    ; counters = { before.counters with revision = Int64.succ before.counters.revision }
    ; conversation =
        { before.conversation with
          canonical_history = retained
        ; compaction_archives = [ archive ]
        }
    }
  in
  State.validate successor |> protocol_ok;
  predecessor, Persistence.Restored.authored successor
;;

let check_worker fixture sw ~initial ~predecessor ~archived_tail ~cancel =
  let native_protocols rows = A.History_codec.all_of_protocol rows |> protocol_ok in
  let ready, ready_u = Eio.Promise.create () in
  let finish, finish_u = Eio.Promise.create () in
  let late, late_u = Eio.Promise.create () in
  let live_entry = history 2 "fresh streamed successor row" in
  let final_entry = history 3 "fresh successor completion row" in
  let old = A.History_codec.of_protocol archived_tail |> protocol_ok in
  let live = A.History_codec.of_protocol live_entry |> protocol_ok in
  let final = A.History_codec.of_protocol final_entry |> protocol_ok in
  let operation_worker =
    Worker.create ~run:(fun ~sw:_ ~input capabilities ->
      Eio.Promise.resolve ready_u (input, capabilities);
      match Eio.Promise.await finish with
      | () ->
        capabilities.commit_entry live |> protocol_ok;
        Worker.Completed
          { final_history = input.history @ [ live; final ]
          ; runtime_requests = []
          ; moderator_snapshot = None
          }
      | exception (Eio.Cancel.Cancelled _ as exn) ->
        Eio.Cancel.protect (fun () ->
          let result = capabilities.commit_entry old in
          Eio.Promise.resolve late_u result);
        raise exn)
  in
  let journal = journal fixture Store.Journal.create in
  let timeout f = Eio.Time.with_timeout_exn (Eio.Stdenv.clock fixture.env) 5. f in
  with_actor
    ~operation_worker
    fixture
    sw
    journal
    ~restored:initial
    ~previous_transaction_hash:None
    ~archive:(fun _ _ -> failwith "a foreground callback rewrote the rollover archive")
    (fun actor _persistence ->
       let attachment_id = attach actor in
       Actor.start actor ~attachment_id |> protocol_ok |> ignore;
       let before = state actor in
       let retained = before.conversation.canonical_history in
       let continued =
         Actor.continue_history
           actor
           P.History_edit.Continue_request.
             { session_id
             ; attachment_id
             ; expected_generation = before.identity.generation
             ; expected_revision = before.counters.revision
             ; idempotency_key =
                 P.Idempotency_key.of_string "foreground-window-test" |> protocol_ok
             }
         |> protocol_ok
       in
       (match continued.continuation with
        | Started _ -> ()
        | Not_requested | Not_started _ -> failwith "fixture did not start a real worker");
       let input, capabilities = timeout (fun () -> Eio.Promise.await ready) in
       let during = state actor in
       let operation_id = input.operation.id in
       let cleanup = capabilities.callback_cleanup () |> protocol_ok in
       let registration_closed () =
         Cleanup.Operation.snapshot cleanup
         |> protocol_ok
         |> Cleanup.Cleanup_snapshot.registration_closed
       in
       assert (not (registration_closed ()));
       [%test_eq: P.History.Id.t list]
         (List.map input.history ~f:History_entry.id)
         (List.map retained ~f:(fun row -> row.id));
       (* Both projections passed the old prefix check. The stale completion
          would install the actual archived row and settle the current worker. *)
       let stale = native_protocols (retained @ [ archived_tail ]) in
       Actor.For_testing.deliver_stale_foreground_entry actor ~window:predecessor old
       |> assert_conflict;
       Actor.For_testing.deliver_stale_foreground_result
         actor
         ~window:predecessor
         ~history:stale
       |> protocol_ok;
       let after_rejected = state actor in
       assert (not (registration_closed ()));
       assert (Option.is_none (Cleanup.Operation.terminal cleanup |> protocol_ok));
       [%test_eq: int64] after_rejected.counters.revision during.counters.revision;
       assert (
         List.equal
           P.History.equal_entry
           retained
           after_rejected.conversation.canonical_history);
       assert (
         Option.exists after_rejected.active_operation ~f:(fun op ->
           P.Id.Operation.equal op.id operation_id));
       if cancel
       then (
         Actor.cancel_operation actor ~attachment_id ~operation_id
         |> protocol_ok
         |> ignore;
         timeout (fun () -> Eio.Promise.await late) |> assert_conflict)
       else Eio.Promise.resolve finish_u ();
       let after = timeout (fun () -> await_idle actor) in
       assert (Option.is_none after.active_operation);
       assert (registration_closed ());
       (match Cleanup.Operation.terminal cleanup |> protocol_ok with
        | Some (Ok (Ok ())) -> ()
        | Some (Ok (Error _) | Error _) | None ->
          failwith "worker terminal did not settle");
       capabilities.commit_entry old |> assert_conflict;
       let expected =
         if cancel then retained else retained @ [ live_entry; final_entry ]
       in
       assert (
         List.equal P.History.equal_entry expected after.conversation.canonical_history);
       [%test_eq: int64] (state actor).counters.revision after.counters.revision;
       after)
;;

let replay_worker fixture ~initial after =
  let recovered =
    Store.Recovery.load
      ~env:fixture.env
      ~journal:(journal fixture Store.Journal.open_existing)
      ~snapshot_directory:(Store.Session_store.Handle.snapshot_directory fixture.handle)
      ~max_snapshot_payload_length:payload_limit
      ~session_id
      ~initial
      ~restore_snapshot:(Persistence.restore_snapshot ~limits:document_limits)
      ~apply:(Persistence.apply_transaction ~limits:document_limits)
      ~validate_transaction:(Persistence.validate_transaction ~limits:document_limits)
      ~validate:Persistence.validate
    |> store_ok
  in
  let replayed = Persistence.Restored.state recovered.state in
  assert (same_ledger after replayed);
  assert (Option.is_none replayed.active_operation);
  assert (
    List.equal
      P.History.equal_entry
      after.conversation.canonical_history
      replayed.conversation.canonical_history)
;;

let%expect_test
    "stale foreground publication preserves empty and shared-prefix successors"
  =
  List.iter [ false; true ] ~f:(fun shared ->
    List.iter [ false; true ] ~f:(fun cancel ->
      with_actor_workspace (fun env workspace_instance ->
        Eio.Switch.run (fun sw ->
          let base =
            actor_state ~workspace_instance ~liveness:Detached ~start_immediately:true
          in
          let storage = Job_artifact_fixtures.create env sw base in
          let owner =
            P.Session_ref.create
              ~server_id:(Store.Session_store.server_id storage.sessions)
              ~session_id
          in
          let fixture =
            { env; sessions = storage.sessions; handle = storage.session; owner }
          in
          let retained = if shared then [ history 0 "shared prefix" ] else [] in
          let archived_tail = history 1 "retired window private text" in
          let predecessor, initial =
            prepare_successor fixture base ~retained ~archived_tail
          in
          let after =
            Eio.Switch.run (fun actor_sw ->
              check_worker fixture actor_sw ~initial ~predecessor ~archived_tail ~cancel)
          in
          replay_worker fixture ~initial after;
          printf "shared-prefix=%b cancelled=%b replayed=true\n" shared cancel))));
  [%expect
    {|
    shared-prefix=false cancelled=false replayed=true
    shared-prefix=false cancelled=true replayed=true
    shared-prefix=true cancelled=false replayed=true
    shared-prefix=true cancelled=true replayed=true
    |}]
;;
