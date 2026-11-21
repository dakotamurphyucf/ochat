open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module D = Document_schema
module S = Agent_store
module R = A.Recovery_rollover
module Actor = A.Session_actor
module Persistence = A.Session_persistence
module Catalog = Recovery_notes.Catalog
module Scope = P.Recovery_window.Scope
module W = Recovery_window_actor_tests
module Identity = Recovery_note_actor_tests

exception Lost_reply

let rec is_lost_reply = function
  | Lost_reply -> true
  | Eio.Exn.Multiple failures ->
    List.for_all failures ~f:(fun (exn, _) -> is_lost_reply exn)
  | Exn.Finally (original, cleanup) -> is_lost_reply original && is_lost_reply cleanup
  | Eio.Cancel.Cancelled reason -> is_lost_reply reason
  | _ -> false
;;

let expected actor =
  let state = W.state actor in
  R.Expected.create (W.scope state) ~revision:state.counters.revision |> protocol_ok
;;

let set_manual actor (fixture : W.fixture) =
  let host =
    A.Session_context_capacity.Host.create
      ~mode_for_session:(fun owner ->
        Option.some_if
          (P.Session_ref.equal fixture.owner owner)
          A.Session_context_capacity.Mode.Manual)
      ~targets:[]
    |> Result.map_error ~f:(fun _ -> "invalid context fixture")
    |> Result.ok_or_failwith
  in
  Actor.set_context_capacity_host actor host |> protocol_ok
;;

let request actor attachment_id ?authorize_force ~name basis =
  Actor.request_context_rollover
    actor
    ~actor:(W.actor_proof ())
    ~attachment_id
    ~authorize:Identity.allow
    ?authorize_force
    ~key:(Identity.key name)
    basis
;;

let settle actor attachment_id ?authorize_force ~id basis command =
  Actor.settle_context_rollover
    actor
    ~actor:(W.actor_proof ())
    ~attachment_id
    ~authorize:Identity.allow
    ?authorize_force
    ~id
    basis
    command
;;

let lookup state ~id =
  Option.value_exn (R.find (Option.value_exn state.A.Session_state.recovery_rollover) ~id)
;;

let recovery (fixture : W.fixture) ~journal ~initial =
  S.Recovery.load
    ~env:fixture.env
    ~journal
    ~snapshot_directory:(S.Session_store.Handle.snapshot_directory fixture.handle)
    ~max_snapshot_payload_length:W.payload_limit
    ~session_id
    ~initial
    ~restore_snapshot:(Persistence.restore_snapshot ~limits:document_limits)
    ~apply:(Persistence.apply_transaction ~limits:document_limits)
    ~validate_transaction:(Persistence.validate_transaction ~limits:document_limits)
    ~validate:Persistence.validate
  |> store_ok
;;

let retained_turn (state : A.Session_state.t) id =
  let payload =
    A.Inference_ledger.to_document state.inference_ledger
    |> Result.map_error ~f:(fun _ -> "invalid inference ledger")
    |> Result.ok_or_failwith
    |> D.Document.payload
  in
  match Identity.get payload "turns" with
  | `Array turns ->
    List.find_map_exn turns ~f:(fun turn ->
      let operation =
        P.Operation.of_json (Identity.get turn "operation") |> protocol_ok
      in
      Option.some_if (P.Id.Operation.equal operation.id id) operation)
  | _ -> failwith "turns must be an array"
;;

let check_turn_starting state id =
  let turn = retained_turn state id in
  assert (
    match turn.state with
    | Starting -> true
    | _ -> false)
;;

let check_turn_retired state id =
  let turn = retained_turn state id in
  assert (
    match turn.state with
    | Interrupted { reason; retryable = false } ->
      String.equal reason "context window retired"
    | _ -> false)
;;

let check_attempts_preserved (before : A.Session_state.t) (after : A.Session_state.t) =
  let records ledger =
    A.Inference_ledger.rows ledger
    |> List.map ~f:(fun row ->
      A.Inference_ledger.Row.record row |> Inference.Observation.Attempt_record.to_json)
  in
  assert (
    Int64.equal
      (A.Inference_ledger.last_attempt_ordinal before.inference_ledger)
      (A.Inference_ledger.last_attempt_ordinal after.inference_ledger));
  assert (
    List.equal
      D.Json.equal
      (records before.inference_ledger)
      (records after.inference_ledger))
;;

let check_rollover_retirement transactions (state : A.Session_state.t) id =
  let transaction =
    List.find_exn transactions ~f:(fun transaction ->
      List.mem
        (W.delta_kinds transaction)
        "recovery_rollover_committed"
        ~equal:String.equal)
  in
  assert (
    List.mem
      (D.Document.required_semantics transaction.delta)
      P.Session_storage_semantics.recovery_rollover_turn_retirement
      ~equal:String.equal);
  let marker = P.Session_storage_semantics.recovery_rollover_turn_retirement in
  let without_marker =
    D.Document.required_semantics transaction.delta
    |> List.filter_map ~f:(fun name ->
      if String.equal name marker then None else Some (`String name))
    |> fun names ->
    Identity.set (D.Document.json transaction.delta) "required_semantics" (`Array names)
    |> D.Document.inspect ~limits:document_limits
    |> document_ok
  in
  (match A.Session_delta_document.decode ~limits:document_limits without_marker with
   | Error (D.Error.Required_semantics_unknown name) ->
     assert (String.equal name ("missing " ^ marker))
   | Error _ | Ok _ -> failwith "expected missing retirement semantics");
  let terminals =
    List.concat_map transactions ~f:(fun transaction ->
      A.Session_persistence.durable_events ~limits:document_limits transaction |> store_ok)
    |> List.filter_map ~f:(fun (event : P.Event.Durable.t) ->
      let payload =
        P.Event.Durable.Payload.of_json ~kind:event.kind event.payload |> protocol_ok
      in
      match payload with
      | Operation_interrupted operation
      | Operation_cancelled operation
      | Operation_completed operation
      | Operation_failed operation
        when P.Id.Operation.equal operation.id id ->
        Some (P.Event.Durable.Payload.kind payload, P.Operation.to_json operation)
      | _ -> None)
  in
  match terminals with
  | [ (Operation_interrupted, operation) ] ->
    assert (D.Json.equal operation (P.Operation.to_json (retained_turn state id)))
  | _ -> failwith "expected one durable predecessor interruption"
;;

let record_checkpoint actor writer (receipt : Catalog.Receipt.t) =
  Actor.record_context_checkpoint
    actor
    ~actor:(W.actor_proof ())
    ~attachment_id:writer
    ~authorize:Identity.allow
    (expected actor)
    ~notes:[ receipt.selection ]
  |> protocol_ok
;;

let publish ?checkpoint retain = R.Settlement.Publish { checkpoint; retain }

let check_unmodified (before : A.Session_state.t) (after : A.Session_state.t) =
  assert (P.Id.Session.equal before.identity.session_id after.identity.session_id);
  [%test_eq: int] before.identity.generation after.identity.generation;
  assert (phys_equal before.spec after.spec);
  assert (phys_equal before.recovery_notes after.recovery_notes);
  assert (phys_equal before.goal_catalog after.goal_catalog);
  assert (phys_equal before.jobs after.jobs);
  assert (phys_equal before.schedules after.schedules);
  assert (
    phys_equal
      before.conversation.deferred_user_entries
      after.conversation.deferred_user_entries);
  [%test_eq: int]
    before.conversation.compaction_generation
    after.conversation.compaction_generation
;;

let%expect_test "archive sync and journal acknowledgement have distinct replay edges" =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun storage_scope ->
      let keep = W.history 0 "retained" in
      let old = W.history 1 "archived only" in
      let initial =
        actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
      in
      let initial =
        A.Session_state.create
          ~identity:initial.identity
          ~spec:initial.spec
          ~initial_history:[ keep; old ]
      in
      let storage = Job_artifact_fixtures.create env storage_scope initial in
      let owner =
        P.Session_ref.create
          ~server_id:(S.Session_store.server_id storage.sessions)
          ~session_id
      in
      let fixture : W.fixture =
        { env; sessions = storage.sessions; handle = storage.session; owner }
      in
      let initial = Persistence.Restored.authored initial in
      let actual_archive =
        W.Archive.write_document
          ~env
          ~handle:fixture.handle
          ~max_payload_length:W.payload_limit
      in
      let orphan = ref None in
      let after_sync reference document =
        actual_archive reference document |> protocol_ok;
        orphan := Some reference;
        Error
          (P.Error.create
             Persistence_error
             ~message:"injected after archive sync"
             ~retryable:true
             ())
      in
      let open_journal () = W.journal fixture S.Journal.open_existing in
      let name = "commit-replay" in
      let checkpoint, admission, id =
        Eio.Switch.run (fun sw ->
          let journal = W.journal fixture S.Journal.create in
          W.with_actor
            fixture
            sw
            journal
            ~restored:initial
            ~previous_transaction_hash:None
            ~archive:after_sync
            (fun actor persistence ->
               let writer = W.attach actor in
               set_manual actor fixture;
               let active =
                 Actor.enable_recovery_windows
                   actor
                   ~actor:(W.actor_proof ())
                   ~attachment_id:writer
                   ~authorize:Identity.allow
                 |> protocol_ok
               in
               let access =
                 A.Recovery_note_access.operator
                   (W.actor_proof ())
                   ~owner
                   ~generation:0
                   ~attachment_id:(Some writer)
                   ~authorize:Identity.allow
               in
               let note =
                 Actor.mutate_recovery_note_operator
                   actor
                   ~access
                   ~key:(Identity.key "checkpoint-note")
                   (Identity.create "durable local checkpoint body")
                 |> protocol_ok
               in
               let stale_note = record_checkpoint actor writer note in
               let note =
                 Actor.mutate_recovery_note_operator
                   actor
                   ~access
                   ~key:(Identity.key "refresh-checkpoint-note")
                   (Replace
                      { expected = note.selection
                      ; content =
                          Catalog.Content.of_string "updated checkpoint" |> protocol_ok
                      })
                 |> protocol_ok
               in
               let before = expected actor in
               let stale =
                 request actor writer ~name:"stale-checkpoint" before |> protocol_ok
               in
               let id = stale.intent.id in
               Identity.expect_error
                 Conflict
                 (settle actor writer ~id before (publish ~checkpoint:stale_note []));
               let rejected =
                 settle
                   actor
                   writer
                   ~id
                   (expected actor)
                   (publish ~checkpoint:stale_note [])
                 |> protocol_ok
               in
               assert (
                 R.Receipt.equal_status
                   rejected.status
                   (Terminal (Superseded Checkpoint_required)));
               let cancelled =
                 request actor writer ~name:"cancel" (expected actor) |> protocol_ok
               in
               let cancelled =
                 settle actor writer ~id:cancelled.intent.id (expected actor) Cancel
                 |> protocol_ok
               in
               assert (
                 R.Receipt.equal_status cancelled.status (Terminal (Superseded Cancelled)));
               let checkpoint = record_checkpoint actor writer note in
               let admission = expected actor in
               let pending = request actor writer ~name admission |> protocol_ok in
               let id = pending.intent.id in
               let cut = expected actor in
               let state = W.state actor in
               let stored = Option.value_exn state.recovery_rollover in
               let check_history history =
                 R.check_checkpoint
                   stored
                   checkpoint
                   ~expected:cut
                   ~history
                   ~catalog:state.recovery_notes
               in
               check_history [ keep; old ] |> protocol_ok;
               Identity.expect_error Conflict (check_history [ old; keep ]);
               Identity.expect_error
                 Conflict
                 (check_history [ { keep with payload = `String "edited" }; old ]);
               let unrecorded =
                 let principal = Operator_authorization.principal (W.actor_proof ()) in
                 R.Checkpoint.capture
                   cut
                   ~history:[ keep; old ]
                   ~catalog:state.recovery_notes
                   ~notes:[ note.selection ]
                   ~recipe:(Owner_selection principal.id)
                 |> protocol_ok
               in
               Identity.expect_error
                 Conflict
                 (R.check_checkpoint
                    stored
                    unrecorded
                    ~expected:cut
                    ~history:[ keep; old ]
                    ~catalog:state.recovery_notes);
               let successor =
                 Scope.create
                   ~session:owner
                   ~generation:0
                   ~window:(P.Id.Context_window.create ())
                 |> protocol_ok
               in
               let forged =
                 R.Commit.create
                   pending.intent
                   ~expected:cut
                   ~successor
                   ~archive:
                     (Chatml_source.Digest.of_contents
                        "different prestate"
                        ~checkpoint:ignore)
                   ~checkpoint:(Checkpointed checkpoint)
                   ~retain:[ keep.id ]
                 |> protocol_ok
               in
               let delta = A.Session_delta.Recovery_rollover_committed forged in
               let raw_candidate =
                 A.Session_transition.apply ~now:timestamp state ~delta ~payloads:[]
                 |> protocol_ok
               in
               assert (
                 Result.is_error
                   (A.Session_document_transition.admit_encoded
                      (Persistence.Restored.state_document
                         (Persistence.restored persistence))
                      ~delta
                      ~next:raw_candidate.state
                      ~limits:document_limits));
               Identity.expect_error
                 Invalid_request
                 (A.Session_delta.apply
                    state
                    (Batch [ delta; Halt_changed (Some "stop") ]));
               Identity.expect_error
                 Invalid_request
                 (A.Session_delta.apply
                    state
                    (Compaction_archived
                       (A.Session_state.Compaction_archive.of_rollover forged)));
               let count = Identity.count journal in
               Identity.expect_error
                 Persistence_error
                 (settle actor writer ~id cut (publish ~checkpoint [ keep.id ]));
               [%test_eq: int] count (Identity.count journal);
               assert (R.Receipt.equal_status (lookup (W.state actor) ~id).status Pending);
               assert (Scope.equal active (W.scope (W.state actor)));
               let synced =
                 W.read_archive fixture (Option.value_exn !orphan) |> protocol_ok
               in
               assert (
                 List.equal
                   P.History.equal_entry
                   [ keep; old ]
                   synced.conversation.canonical_history);
               checkpoint, admission, id))
      in
      let first = recovery fixture ~journal:(open_journal ()) ~initial in
      let before = Persistence.Restored.state first.state in
      assert (Scope.equal admission.scope (W.scope before));
      assert (R.Receipt.equal_status (lookup before ~id).status Pending);
      assert (List.is_empty before.conversation.compaction_archives);
      Eio.Switch.run (fun sw ->
        let journal = open_journal () in
        let before_write = ref None in
        let directory = S.Session_store.Handle.journal_directory fixture.handle in
        let segment =
          Filename.concat
            directory
            (S.Journal.current_segment journal |> S.Journal_segment.Id.filename)
        in
        let segment_path = Eio.Path.(Eio.Stdenv.fs env / segment) in
        let saved_path = Eio.Path.(Eio.Stdenv.fs env / (segment ^ ".saved")) in
        let interrupted = ref false in
        let archive reference document =
          let open Result.Let_syntax in
          let%map () = actual_archive reference document in
          Eio.Path.rename segment_path saved_path;
          interrupted := true;
          Eio.Path.mkdir ~perm:0o700 segment_path
        in
        Exn.protect
          ~finally:(fun () ->
            if !interrupted
            then (
              if Eio.Path.is_directory segment_path then Eio.Path.rmdir segment_path;
              Eio.Path.rename saved_path segment_path))
          ~f:(fun () ->
            W.with_actor
              fixture
              sw
              journal
              ~restored:first.state
              ~previous_transaction_hash:first.latest_transaction_hash
              ~archive
              (fun actor _ ->
                 let writer = W.attach actor in
                 set_manual actor fixture;
                 let cut = expected actor in
                 before_write := Some (Identity.count journal);
                 (match settle actor writer ~id cut (publish ~checkpoint [ keep.id ]) with
                  | Error { code = Persistence_error; message; _ } ->
                    assert (
                      String.is_substring message ~substring:"append journal segment")
                  | Error _ | Ok _ -> failwith "rollover did not reach the actual writer");
                 Identity.expect_error Conflict (settle actor writer ~id cut Cancel);
                 Identity.expect_error Conflict (request actor writer ~name admission)));
        [%test_eq: int] (Option.value_exn !before_write) (Identity.count journal));
      let first = recovery fixture ~journal:(open_journal ()) ~initial in
      assert (
        R.Receipt.equal_status
          (lookup (Persistence.Restored.state first.state) ~id).status
          Pending);
      let interrupted =
        try
          Eio.Switch.run (fun sw ->
            let state_committed state _ =
              if not (Scope.equal (W.scope before) (W.scope state)) then raise Lost_reply
            in
            W.with_actor
              ~state_committed
              fixture
              sw
              (open_journal ())
              ~restored:first.state
              ~previous_transaction_hash:first.latest_transaction_hash
              ~archive:actual_archive
              (fun actor _ ->
                 let writer = W.attach actor in
                 set_manual actor fixture;
                 let cut = expected actor in
                 ignore
                   (settle actor writer ~id cut (publish ~checkpoint [ keep.id ])
                    |> protocol_ok
                    : R.Receipt.t)));
          false
        with
        | exn when is_lost_reply exn -> true
      in
      assert interrupted;
      let journal = open_journal () in
      let second = recovery fixture ~journal ~initial in
      let after = Persistence.Restored.state second.state in
      let receipt = lookup after ~id in
      let terminal =
        match receipt.status with
        | Terminal (Committed value) -> value
        | _ -> failwith "committed journal receipt lost"
      in
      assert (Scope.equal terminal.successor (W.scope after));
      assert (not (Scope.equal (W.scope before) (W.scope after)));
      (match terminal.continuation with
       | Awaiting_after_commit owner -> assert (P.Id.Operation.equal id owner)
       | Awaiting_settled_foreground _ | Scheduled_foreground _ ->
         failwith "idle cut acquired a live predecessor");
      let public = R.Receipt.to_public_json receipt |> Jsonaf.to_string in
      assert (not (String.is_substring public ~substring:"selection"));
      assert (not (String.is_substring public ~substring:"durable local checkpoint body"));
      assert (
        List.equal P.History.equal_entry [ keep ] after.conversation.canonical_history);
      let reference = List.hd_exn after.conversation.compaction_archives in
      assert (A.Session_state.Compaction_archive.equal_kind reference.kind Rollover);
      assert (String.equal reference.sha256 (Chatml_source.Digest.to_hex terminal.archive));
      let archive = W.read_archive fixture reference |> protocol_ok in
      assert (Scope.equal (W.scope before) (W.scope archive));
      assert (
        List.equal
          P.History.equal_entry
          [ keep; old ]
          archive.conversation.canonical_history);
      assert (Option.is_none archive.recovery_notes);
      assert (Option.is_none archive.recovery_rollover);
      let marker = P.Session_storage_semantics.recovery_rollover_commit in
      let transaction = W.data_transaction journal after in
      assert (
        List.mem
          (D.Document.required_semantics transaction.delta)
          marker
          ~equal:String.equal);
      let record =
        Persistence.Restored.state_document second.state
        |> A.Session_state_document.encode ~limits:document_limits
        |> document_ok
      in
      assert (List.mem (D.Document.required_semantics record) marker ~equal:String.equal);
      Eio.Switch.run (fun sw ->
        W.with_actor
          fixture
          sw
          journal
          ~restored:second.state
          ~previous_transaction_hash:second.latest_transaction_hash
          ~archive:actual_archive
          (fun actor _ ->
             let writer = W.attach actor in
             set_manual actor fixture;
             let count = Identity.count journal in
             assert (
               R.Receipt.equal
                 receipt
                 (request actor writer ~name admission |> protocol_ok));
             assert (
               R.Receipt.equal
                 receipt
                 (settle actor writer ~id admission Cancel |> protocol_ok));
             [%test_eq: int] count (Identity.count journal);
             let forced =
               request
                 actor
                 writer
                 ~authorize_force:Identity.allow
                 ~name:"forced"
                 (expected actor)
               |> protocol_ok
             in
             let id = forced.intent.id in
             let cut = expected actor in
             let before = W.state actor in
             Identity.expect_error
               Permission_denied
               (settle actor writer ~id cut (publish []));
             let forced =
               settle actor writer ~authorize_force:Identity.allow ~id cut (publish [])
               |> protocol_ok
             in
             (match forced.status with
              | Terminal (Committed { checkpoint = Archive_only Checkpoint_missing; _ })
                -> ()
              | _ ->
                failwith "forced rollover did not yield explicit archive-only evidence");
             let after = W.state actor in
             check_unmodified before after;
             assert (List.is_empty after.conversation.canonical_history);
             assert (List.length after.conversation.compaction_archives = 2)))));
  print_endline
    "orphan kept the old head, writer failure required reopen, lost reply replayed one \
     successor, force recorded archive-only";
  [%expect
    {| orphan kept the old head, writer failure required reopen, lost reply replayed one successor, force recorded archive-only |}]
;;
