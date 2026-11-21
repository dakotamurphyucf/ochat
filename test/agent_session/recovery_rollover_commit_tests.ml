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

let reset_options : A.Administration.reset_options =
  { keep_history = false
  ; keep_tasks = false
  ; keep_grants = false
  ; keep_labels = true
  ; workspace_instance = None
  }
;;

let publish ?checkpoint retain = R.Settlement.Publish { checkpoint; retain }

let with_retained_automatic_turn (state : A.Session_state.t) =
  let operation : P.Operation.t =
    { id = P.Id.Operation.create ()
    ; generation = state.identity.generation
    ; kind = Turn Idle_followup
    ; state = Starting
    ; started_at = timestamp
    ; updated_at = timestamp
    }
  in
  let ledger_result result =
    Result.ok_or_failwith
      (Result.map_error result ~f:(fun error ->
         Sexp.to_string_hum (A.Inference_ledger.Error.sexp_of_t error)))
  in
  let ledger, handle, _ =
    A.Inference_ledger.admit_turn state.inference_ledger operation |> ledger_result
  in
  let inference_ledger =
    A.Inference_ledger.finish_turn ledger handle { operation with state = Completed }
    |> ledger_result
  in
  let budget =
    A.Automatic_turn_budget.create Automatic_turn_budget_tests.policy
    |> fun budget ->
    A.Automatic_turn_budget.note_operation budget operation
    |> fun budget ->
    A.Automatic_turn_budget.with_pauses
      budget
      [ Pause_followup_turns; Pause_internal_event_drains ]
  in
  [%test_eq: int] 1 budget.followup_turns;
  [%test_eq: int] 1 (List.length budget.started_ms);
  assert (
    List.equal
      Chat_response.Runtime_semantics.equal_pause_condition
      [ Pause_followup_turns; Pause_internal_event_drains ]
      budget.policy.budget.pause_conditions);
  { state with inference_ledger; automatic_turn_budget = Some budget }, operation.id
;;

let check_automatic_budget (before : A.Session_state.t) (after : A.Session_state.t) =
  if
    not
      (Option.equal
         A.Automatic_turn_budget.equal
         before.automatic_turn_budget
         after.automatic_turn_budget)
  then
    raise_s
      [%sexp
        "rollover changed the automatic-turn budget"
      , (before.automatic_turn_budget : A.Automatic_turn_budget.t option)
      , (after.automatic_turn_budget : A.Automatic_turn_budget.t option)]
;;

let check_unmodified (before : A.Session_state.t) (after : A.Session_state.t) =
  assert (P.Id.Session.equal before.identity.session_id after.identity.session_id);
  [%test_eq: int] before.identity.generation after.identity.generation;
  assert (phys_equal before.spec after.spec);
  assert (phys_equal before.recovery_notes after.recovery_notes);
  assert (phys_equal before.goal_catalog after.goal_catalog);
  assert (phys_equal before.jobs after.jobs);
  assert (phys_equal before.schedules after.schedules);
  check_automatic_budget before after;
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
      let initial, automatic_turn = with_retained_automatic_turn initial in
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
                 let module Observation = Actor.Resource_observation in
                 let observation = Actor.observe_resource_state actor |> protocol_ok in
                 let release () = Observation.check_recovery_current observation in
                 ignore (release () |> protocol_ok : A.Session_state.t);
                 before_write := Some (Identity.count journal);
                 (match settle actor writer ~id cut (publish ~checkpoint [ keep.id ]) with
                  | Error { code = Persistence_error; message; _ } ->
                    assert (
                      String.is_substring message ~substring:"append journal segment")
                  | Error _ | Ok _ -> failwith "rollover did not reach the actual writer");
                 ignore
                   (Observation.check_current observation |> protocol_ok
                    : A.Session_state.t);
                 Identity.expect_error Persistence_error (release ());
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
      check_automatic_budget (Persistence.Restored.state initial) before;
      check_automatic_budget before after;
      (match (retained_turn after automatic_turn).state with
       | Completed -> ()
       | _ -> failwith "rollover rewrote a previously completed automatic turn");
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
      check_automatic_budget before archive;
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
             let observation = Actor.observe_resource_state actor |> protocol_ok in
             let published =
               Actor.Resource_observation.check_recovery_current observation
               |> protocol_ok
             in
             assert (Scope.equal terminal.successor (W.scope published));
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
             assert (List.length after.conversation.compaction_archives = 2);
             Actor.reset
               actor
               ~attachment_id:writer
               ~expected_revision:after.counters.revision
               reset_options
             |> protocol_ok
             |> ignore;
             let reset = W.state actor in
             [%test_eq: int] 1 (Scope.generation (W.scope reset));
             assert (Option.is_none reset.recovery_rollover);
             let references = reset.conversation.compaction_archives in
             let admin = List.hd_exn references in
             assert (A.Session_state.Compaction_archive.equal_kind admin.kind Reset);
             [%test_eq: int64] after.counters.revision admin.revision;
             [%test_eq: int] 3 (List.length references);
             let admin_state = W.read_archive fixture admin |> protocol_ok in
             assert (Scope.equal (W.scope after) (W.scope admin_state));
             assert (Option.is_none admin_state.recovery_rollover);
             assert (Option.is_none admin_state.recovery_notes);
             ignore (W.read_archive fixture reference |> protocol_ok : A.Session_state.t);
             Identity.expect_error Conflict (settle actor writer ~id cut Cancel);
             let reset =
               (recovery fixture ~journal ~initial).state |> Persistence.Restored.state
             in
             assert (Option.is_none reset.recovery_rollover);
             assert (Scope.equal (W.scope reset) (W.scope (W.state actor)))))));
  print_endline
    "orphan kept the old head, writer failure required reopen, lost reply replayed one \
     successor, force recorded archive-only";
  [%expect
    {| orphan kept the old head, writer failure required reopen, lost reply replayed one successor, force recorded archive-only |}]
;;

let update_field json name ~f = Identity.set json name (f (Identity.get json name))

let raw_future_initial state =
  let open A.Session_state_document in
  let json =
    authored state |> encode ~limits:document_limits |> document_ok |> D.Document.json
  in
  let json =
    update_field json "payload" ~f:(fun payload ->
      update_field payload "conversation" ~f:(fun conversation ->
        update_field conversation "canonical_history" ~f:(function
          | `Array [ row ] -> `Array [ Identity.set row "future_row" (`Number "1.00") ]
          | _ -> failwith "expected one original row"))
      |> fun payload ->
      update_field payload "identity" ~f:(fun identity ->
        Identity.set identity "future_identity" `True))
  in
  D.Document.inspect ~limits:document_limits json
  |> document_ok
  |> decode ~limits:document_limits
  |> document_ok
  |> Persistence.Restored.of_state_document
;;

let assert_private_note (state : A.Session_state.t) (receipt : Catalog.Receipt.t) =
  let catalog = Option.value_exn state.recovery_notes in
  let body = Catalog.read catalog receipt.selection |> Result.ok |> Option.value_exn in
  [%test_eq: string] "private reset note" (Catalog.Content.to_string body)
;;

let assert_physical_archive (fixture : W.fixture) reference =
  let location =
    Filename.concat
      (S.Session_store.Handle.archive_directory fixture.handle)
      (W.Archive.filename reference)
  in
  let frame =
    S.Durable_file.load_bounded ~env:fixture.env ~path:location ~max_bytes:W.payload_limit
    |> store_ok
  in
  let _, captured =
    W.Archive.decode_record
      ~max_payload_length:W.payload_limit
      ~expected_digest:(Some reference.A.Session_state.Compaction_archive.sha256)
      frame
    |> protocol_ok
  in
  let payload =
    A.Session_state_document.encode captured ~limits:document_limits
    |> document_ok
    |> D.Document.payload
  in
  let rows = Identity.get (Identity.get payload "conversation") "canonical_history" in
  (match rows with
   | `Array [ row ] ->
     assert (D.Json.equal (Identity.get row "future_row") (`Number "1.00"))
   | _ -> failwith "administrative archive lost the retired physical row");
  assert (
    D.Json.equal (Identity.get (Identity.get payload "identity") "future_identity") `True)
;;

let%expect_test
    "reset and rebuild retire checkpoint and pending intent at the archive boundary"
  =
  List.iter [ `Checkpoint; `Pending ] ~f:(fun mode ->
    with_actor_workspace (fun env workspace_instance ->
      Eio.Switch.run (fun sw ->
        let seed =
          actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
        in
        let original = W.history 0 "retired administrative history" in
        let seed =
          A.Session_state.create
            ~identity:seed.identity
            ~spec:seed.spec
            ~initial_history:[ original ]
        in
        let seed =
          { seed with
            conversation = { seed.conversation with initial_prompt_entry_count = 0 }
          }
        in
        let seed =
          match mode with
          | `Checkpoint -> seed
          | `Pending ->
            let queued =
              P.Pending_input.create
                ~entry:(W.history 1 "undelivered user input")
                ~generation:0
                ~binding:P.Pending_input.Binding.safe_boundary
              |> protocol_ok
              |> A.Pending_input_document.authored
                   ~owner:(Submitting_principal principal_id)
                   ~limits:document_limits
              |> document_ok
            in
            { seed with
              conversation =
                { seed.conversation with
                  deferred_user_entries = [ queued ]
                ; next_history_sequence = 2L
                ; reserved_history_through = 2L
                }
            }
        in
        let storage = Job_artifact_fixtures.create env sw seed in
        let owner =
          P.Session_ref.create
            ~server_id:(S.Session_store.server_id storage.sessions)
            ~session_id
        in
        let fixture : W.fixture =
          { env; sessions = storage.sessions; handle = storage.session; owner }
        in
        let initial = raw_future_initial seed in
        let journal = W.journal fixture S.Journal.create in
        let can_write = ref false in
        let archive reference document =
          if !can_write
          then
            W.Archive.write_document
              ~env
              ~handle:fixture.handle
              ~max_payload_length:W.payload_limit
              reference
              document
          else
            Error
              (P.Error.create
                 Persistence_error
                 ~retryable:false
                 ~message:"archive sync denied by fixture"
                 ())
        in
        let after, note, fresh_intent =
          W.with_actor
            fixture
            sw
            journal
            ~restored:initial
            ~previous_transaction_hash:None
            ~archive
            (fun actor persistence ->
               let writer = W.attach actor in
               set_manual actor fixture;
               ignore
                 (Actor.enable_recovery_windows
                    actor
                    ~actor:(W.actor_proof ())
                    ~attachment_id:writer
                    ~authorize:Identity.allow
                  |> protocol_ok
                  : Scope.t);
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
                   ~key:(Identity.key "private-reset-note")
                   (Identity.create "private reset note")
                 |> protocol_ok
               in
               (match mode with
                | `Checkpoint -> ()
                | `Pending ->
                  let before = W.state actor in
                  assert (Option.is_none before.recovery_rollover);
                  let wrapper = List.hd_exn before.conversation.deferred_user_entries in
                  let entry =
                    P.Pending_input.entry (A.Pending_input_document.value wrapper)
                  in
                  let target =
                    P.Pending_control.Cancel_request.create
                      ~session_id
                      ~attachment_id:writer
                      ~expected_generation:before.identity.generation
                      ~expected_pending_revision:before.conversation.pending_revision
                      ~history_id:entry.id
                      ~expected_content_revision:entry.content_revision
                      ~idempotency_key:(Identity.key "edit-before-checkpoint")
                    |> protocol_ok
                  in
                  let replacement =
                    P.Pending_control.Replace_request.create
                      ~target
                      ~text:"amended pending input"
                    |> protocol_ok
                  in
                  can_write := true;
                  ignore
                    (Actor.replace_pending
                       actor
                       ~principal:principal_id
                       ~project:Pending_control_tests.project
                       replacement
                     |> protocol_ok : P.Pending_control.Result.t);
                  can_write := false;
                  let reference =
                    List.hd_exn (W.state actor).conversation.compaction_archives
                  in
                  let archived = W.read_archive fixture reference |> protocol_ok in
                  assert (Option.is_none archived.recovery_notes);
                  assert_private_note (W.state actor) note);
               ignore (record_checkpoint actor writer note : R.Checkpoint.t);
               let old_basis = expected actor in
               let old_intent =
                 match mode with
                 | `Checkpoint -> None
                 | `Pending ->
                   Some (request actor writer ~name:"reset-key" old_basis |> protocol_ok)
               in
               let before = W.state actor in
               let citation =
                 W.membership before |> List.hd_exn |> W.Ledger.Member.citation
               in
               let unchanged = Identity.count journal in
               let forged = { before with recovery_rollover = None } in
               Identity.expect_error
                 Invalid_request
                 (A.Session_delta.apply before (Created forged));
               let physical_prior =
                 Persistence.restored persistence |> Persistence.Restored.state_document
               in
               let candidate =
                 A.Administration.reset before reset_options |> protocol_ok
               in
               let forged_no_archive = A.Session_delta.apply before (Created candidate) in
               Identity.expect_error Invalid_request forged_no_archive;
               let forged_archive =
                 A.Session_state.Compaction_archive.
                   { operation_id = P.Id.Operation.create ()
                   ; revision = before.counters.revision
                   ; sha256 = String.make 64 '0'
                   ; kind = Reset
                   ; invocation_dispositions = []
                   }
               in
               let forged =
                 { candidate with
                   conversation =
                     { candidate.conversation with
                       compaction_archives =
                         forged_archive :: before.conversation.compaction_archives
                     }
                 }
               in
               assert (
                 Result.is_error
                   (A.Session_document_transition.admit
                      physical_prior
                      ~delta:(Created forged)
                      ~next:forged
                      ~limits:document_limits));
               if
                 P.Id.Context_window.equal
                   (Scope.window (W.scope before))
                   (Scope.window (W.scope candidate))
               then failwith "planned administration reuses a retired window";
               (match mode with
                | `Pending -> ()
                | `Checkpoint ->
                  let archive =
                    W.Archive.reference_for
                      physical_prior
                      ~limits:document_limits
                      ~kind:Reset
                      (P.Id.Operation.create ())
                    |> protocol_ok
                  in
                  let exact =
                    { candidate with
                      conversation =
                        { candidate.conversation with
                          compaction_archives =
                            archive :: before.conversation.compaction_archives
                        }
                    }
                  in
                  assert (
                    Result.is_error
                      (A.Session_document_transition.admit
                         physical_prior
                         ~delta:(Batch [ Failure_changed None; Created exact ])
                         ~next:exact
                         ~limits:document_limits)));
               let replace () =
                 match mode with
                 | `Checkpoint ->
                   Actor.reset
                     actor
                     ~attachment_id:writer
                     ~expected_revision:before.counters.revision
                     reset_options
                 | `Pending ->
                   let prepared =
                     A.Administration.plan_rebuild before before.spec.prompt_revision_id
                     |> protocol_ok
                   in
                   Actor.commit_reconciled_administration
                     actor
                     ~command_audit:None
                     ~attachment_id:writer
                     ~expected:before
                     ~kind:Rebuild
                     prepared
               in
               Identity.expect_error Persistence_error (replace ());
               [%test_eq: int] unchanged (Identity.count journal);
               assert (
                 phys_equal before.recovery_rollover (W.state actor).recovery_rollover);
               assert (Scope.equal (W.scope before) (W.scope (W.state actor)));
               can_write := true;
               ignore (replace () |> protocol_ok : P.Session.t);
               let after = W.state actor in
               assert (Option.is_none after.recovery_rollover);
               assert (not (Scope.equal (W.scope before) (W.scope after)));
               [%test_eq: int] 1 after.identity.generation;
               assert (List.is_empty after.conversation.canonical_history);
               assert_private_note after note;
               let administrative = List.hd_exn after.conversation.compaction_archives in
               assert (
                 A.Session_state.Compaction_archive.equal_kind
                   administrative.kind
                   (match mode with
                    | `Checkpoint -> Reset
                    | `Pending -> Rebuild));
               let archive = W.read_archive fixture administrative |> protocol_ok in
               assert (W.same_ledger before archive);
               assert (Option.is_none archive.recovery_rollover);
               assert (Option.is_none archive.recovery_notes);
               (match mode with
                | `Checkpoint -> ()
                | `Pending ->
                  [%test_eq: int]
                    1
                    (List.length archive.conversation.deferred_user_entries);
                  assert (List.is_empty after.conversation.deferred_user_entries));
               assert_physical_archive fixture administrative;
               let source current ~project =
                 A.Recovery_source_reader.read
                   fixture.sessions
                   ~handle:fixture.handle
                   ~current
                   ~max_frame_bytes:W.payload_limit
                   citation
                   ~project
               in
               assert (
                 Result.is_ok
                   (source
                      (fun () -> Ok before)
                      ~project:(fun row -> Ok (P.History.equal_entry row original))));
               assert (
                 Result.is_error
                   (source
                      (fun () -> Ok after)
                      ~project:(fun _ -> failwith "old recovery row disclosed")));
               Option.iter old_intent ~f:(fun receipt ->
                 Identity.expect_error
                   Conflict
                   (settle actor writer ~id:receipt.intent.id old_basis Cancel));
               Identity.expect_error
                 Conflict
                 (request actor writer ~name:"reset-key" old_basis);
               let new_intent =
                 request actor writer ~name:"reset-key" (expected actor) |> protocol_ok
               in
               Option.iter old_intent ~f:(fun receipt ->
                 assert (not (P.Id.Operation.equal receipt.intent.id new_intent.intent.id)));
               after, note, new_intent)
        in
        let reloaded =
          recovery fixture ~journal:(W.journal fixture S.Journal.open_existing) ~initial
        in
        let state = Persistence.Restored.state reloaded.state in
        assert (Scope.equal (W.scope after) (W.scope state));
        assert_private_note state note;
        assert (R.Receipt.equal fresh_intent (lookup state ~id:fresh_intent.intent.id));
        let payload =
          Persistence.Restored.state_document reloaded.state
          |> A.Session_state_document.encode ~limits:document_limits
          |> document_ok
          |> D.Document.payload
        in
        assert (
          D.Json.equal
            (Identity.get (Identity.get payload "identity") "future_identity")
            `True);
        let archive = List.hd_exn state.conversation.compaction_archives in
        assert_physical_archive fixture archive;
        printf
          "%s archived before retirement and reopened with new authority\n"
          (match mode with
           | `Checkpoint -> "checkpoint-only reset"
           | `Pending -> "pending-intent rebuild"))));
  [%expect
    {|
    checkpoint-only reset archived before retirement and reopened with new authority
    pending-intent rebuild archived before retirement and reopened with new authority
    |}]
;;
