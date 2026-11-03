open! Core
open Support
module S = Agent_store.Session_store
module Archive = Agent_store.Session_archive_record
module Persistence = A.Session_persistence

(* This complete projection intentionally uses only a no-job/no-schedule fixture.
   Actual store issuance still checks its full canonical metadata/index equality. *)
let index_entry (state : A.Session_state.t) =
  assert (List.is_empty state.jobs && List.is_empty state.schedules);
  Agent_store.Session_index.Entry.
    { session = A.Session_state.summary state
    ; runnable_job_count = 0
    ; deliverable_job_count = 0
    ; earliest_schedule_due = None
    ; owner_grace_deadline = None
    ; pending_initial_start = state.pending_initial_start
    ; archived = false
    ; lifecycle_revision = Lifecycle.zero
    ; admission = Automatic
    }
;;

let metadata state =
  S.Metadata.
    { schema_version = S.current_metadata_schema_version
    ; session = A.Session_state.summary state
    ; prompt_artifact = P.Id.Prompt_revision.to_string state.spec.prompt_revision_id
    ; workspace_identity = state.spec.workspace_instance.conflict_domain
    ; data_schema_version = A.Session_state.current_schema_version
    }
;;

let initial_with_source env workspace_instance =
  let manager, _, _ = handoff_definition env in
  let snapshot =
    Chat_response.Moderator_manager.identity_snapshot manager |> Result.ok_or_failwith
  in
  let initial =
    actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
  in
  { initial with moderator = Some (A.Moderator_checkpoint.encode snapshot) }
;;

let principal =
  P.Principal.create
    ~id:principal_id
    ~authentication_kind:"fixture"
    ~scopes:(P.Scope.Set.of_list [ Send_messages; View_session_transcript ])
    ~attributes:[]
  |> protocol_ok
;;

let fail message = Error (P.Error.create Persistence_error ~message ~retryable:true ())

let audit key =
  Agent_store.Idempotency_store.Command_audit.encode
    { key =
        { principal_id
        ; session_id = Some session_id
        ; method_name = "session.stop"
        ; idempotency_key = P.Idempotency_key.of_string key |> protocol_ok
        }
    ; request_digest = String.make 64 'a'
    ; protected_record = false
    }
  |> store_ok
;;

let create_actor env sw initial persistence ~committed =
  A.Session_actor.create
    ~sw
    ~clock:(Eio.Stdenv.clock env)
    ~mailbox_capacity:32
    ~compaction_env:None
    ~initial_state:initial
    ~persistence
    ~operation_worker:None
    ~services:
      { now = (fun () -> timestamp)
      ; monotonic_now = (fun () -> Mtime.min_stamp)
      ; create_attachment_id = P.Id.Attachment.create
      ; create_reclaim_token = (fun () -> "recurrence-reclaim")
      ; state_committed = committed
      ; job_results = None
      ; subscription_limits = A.Staged_subscriptions.default_limits
      ; schedule_limits = A.Staged_schedules.default_limits
      ; notification_limits = A.Staged_notifications.default_limits
      ; ingress_limits = A.Staged_ingress.default_limits
      }
;;

let publish_lifecycle store handle state (action : Archive.Outcome.action) suffix =
  let observation = S.read_lifecycle store handle |> store_ok in
  let current = S.Lifecycle.Observation.value observation in
  let summary = A.Session_state.summary state in
  let anchor =
    Archive.Anchor.create
      ~generation:summary.generation
      ~session_revision:summary.revision
      ~latest_event_sequence:summary.latest_event_sequence
    |> protocol_ok
  in
  let key : Agent_store.Idempotency_store.Key.t =
    { principal_id
    ; session_id = Some session_id
    ; method_name =
        (match action with
         | Archive -> "session.delete"
         | Restore | Resume | Remove -> Archive.Outcome.method_name action)
    ; idempotency_key = P.Idempotency_key.of_string suffix |> protocol_ok
    }
  in
  let transition =
    Archive.prepare
      current
      ~expected:(Archive.revision current)
      ~anchor
      ~action
      ~key
      ~request_digest:(String.make 64 'b')
      ~now:timestamp
    |> protocol_ok
  in
  let entry =
    Agent_store.Session_index.find_checked (S.session_index store) session_id
    |> store_ok
    |> Option.value_exn
  in
  let prepared =
    S.prepare_lifecycle
      store
      handle
      observation
      ~current_entry:entry
      ~transition
      ~now:timestamp
    |> store_ok
  in
  S.publish_lifecycle store handle prepared |> store_ok |> ignore
;;

(* Receipt retention is a pure lifecycle contract. Its result is deliberately
   never installed as a store observation or used to manufacture Current. *)
let check_receipt_expiry record =
  let now = P.Timestamp.of_string "2026-11-06T12:00:00Z" |> protocol_ok in
  let acknowledged =
    List.fold (Archive.receipts record) ~init:record ~f:(fun record receipt ->
      Archive.acknowledge record ~key:receipt.key ~request_digest:receipt.request_digest
      |> protocol_ok)
  in
  let retained_after_noop record =
    Archive.prepare
      record
      ~expected:(Archive.revision record)
      ~anchor:(List.hd_exn (Archive.receipts record)).outcome.anchor
      ~action:Archive
      ~key:
        { principal_id
        ; session_id = Some session_id
        ; method_name = "session.archive"
        ; idempotency_key = P.Idempotency_key.of_string "expiry-noop" |> protocol_ok
        }
      ~request_digest:(String.make 64 'c')
      ~now
    |> protocol_ok
    |> Archive.Prepared.next
  in
  let expired = retained_after_noop acknowledged in
  let protected = retained_after_noop record in
  assert (Int.equal (List.length (Archive.receipts expired)) 1);
  assert (Int.equal (List.length (Archive.receipts protected)) 2);
  assert (Lifecycle.equal (Archive.revision expired) (Archive.revision record));
  assert (Lifecycle.equal (Archive.revision protected) (Archive.revision record))
;;

let%expect_test
    "actual owned Current, failed/replayed commands, Stop audit and pending-projection \
     recovery"
  =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let initial = initial_with_source env workspace_instance in
      let root =
        Filename.concat workspace_instance.canonical_root.native_path "recurrence-store"
      in
      let store =
        S.create
          ~env
          ~sw
          ~root
          ~server_id
          ~process_start_identity:None
          ~lock_nonce:"recurrence"
        |> store_ok
      in
      let handle =
        S.create_session_initialized
          store
          ~sw
          ~transaction_id:(P.Id.Transaction.create ())
          ~actor_lock_nonce:"recurrence-actor"
          ~initialize:(fun ~staging_directory:_ ->
            S.Initial_projection.create
              ~metadata:(metadata initial)
              ~entry:(index_entry initial))
        |> store_ok
      in
      Exn.protect
        ~finally:(fun () ->
          S.close_session store handle |> store_ok;
          S.close store |> store_ok)
        ~f:(fun () ->
          let directory =
            Filename.concat (S.Handle.directory handle) "recurrence-journal"
          in
          let snapshots =
            Filename.concat (S.Handle.directory handle) "recurrence-snapshots"
          in
          Eio.Path.mkdir ~perm:0o700 Eio.Path.(env#fs / snapshots);
          let journal =
            Agent_store.Journal.create
              ~env
              ~directory
              ~max_payload_length:1048576
              ~max_segment_bytes:4194304L
              ~max_segment_frames:32
            |> store_ok
          in
          let writer =
            Agent_store.Commit_writer.create
              ~sw
              ~journal
              ~session_id
              ~next_transaction_sequence:1L
              ~previous_transaction_hash:None
              ~queue_capacity:8
            |> store_ok
          in
          let accepted = ref [] in
          let durable =
            Persistence.create
              ~archive:(fun _ _ -> failwith "unexpected archive")
              ~pending_archive:None
              ~before_commit:
                (Some
                   (fun state ->
                     S.prepare_canonical_projection
                       store
                       handle
                       ~metadata:(metadata state)
                       ~entry:(index_entry state)
                     |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error))
              ~command_accepted:(fun document _ -> accepted := document :: !accepted)
              ~writer
              ~durability:Flush
              ~limits:document_limits
              ~archive_limits:document_limits
              ~retention_preflight:None
              ~restored:(Persistence.Restored.authored initial)
              ~previous_transaction_hash:None
          in
          let reject = ref true
          and reject_stop = ref false in
          let rec has_stop = function
            | A.Session_delta.Batch deltas -> List.exists deltas ~f:has_stop
            | Lifecycle_changed _ -> true
            | _ -> false
          in
          let persistence : A.Session_actor.persistence =
            { archive_reference
            ; commit =
                (fun ~command_audit ~previous transition ->
                  if !reject || (!reject_stop && has_stop transition.delta)
                  then fail "definite no-write fixture failure"
                  else Persistence.commit durable ~command_audit ~previous transition)
            }
          in
          let actor =
            create_actor env sw initial persistence ~committed:(fun state _ ->
              S.write_metadata ~entry:(index_entry state) store handle (metadata state)
              |> store_ok)
          in
          Exn.protect
            ~finally:(fun () ->
              A.Session_actor.shutdown actor;
              Agent_store.Commit_writer.close writer)
            ~f:(fun () ->
              let controller =
                A.Recurrence_controller.create store ~handle ~entry_projection:index_entry
                |> protocol_ok
              in
              A.Session_actor.set_recurrence_controller actor controller |> protocol_ok;
              let current = ref true in
              let proof =
                Operator_authorization.guarded ~principal ~is_current:(fun () -> !current)
              in
              let authorize _ = Ok () in
              let state () = A.Session_actor.state actor |> protocol_ok in
              let catalog () =
                Option.value (state ()).recurrence_catalog ~default:C.empty
              in
              let _, actual_source =
                A.Goal_catalog_source.capture initial |> protocol_ok
              in
              let call request =
                A.Session_actor.recurrence_command actor ~actor:proof ~authorize ~request
              in
              let command =
                request
                  C.empty
                  ~source:actual_source
                  "actual-create"
                  (Create (id "actual", capture ~source:actual_source ()))
              in
              assert (Result.is_error (call command));
              assert (
                Option.is_none (state ()).recurrence_catalog
                && Option.is_none (state ()).run_state);
              reject := false;
              let receipt = call command |> protocol_ok in
              let frontier = (state ()).counters.transaction_sequence in
              assert (
                A.Recurrence_command_receipt.equal receipt (call command |> protocol_ok));
              assert (Int64.equal frontier (state ()).counters.transaction_sequence);
              current := false;
              assert (Result.is_error (call command));
              assert (
                Result.is_error
                  (A.Session_actor.read_recurrences actor ~actor:proof ~authorize));
              current := true;
              let before_revocation = (state ()).counters.transaction_sequence in
              let revoke _ =
                current := false;
                Ok ()
              in
              assert (
                Result.is_error
                  (A.Session_actor.recurrence_command
                     actor
                     ~actor:proof
                     ~authorize:revoke
                     ~request:command));
              current := true;
              let reads = ref 0 in
              let revoke_after_observation _ =
                Int.incr reads;
                if Int.equal !reads 2 then current := false;
                Ok ()
              in
              assert (
                Result.is_error
                  (A.Session_actor.read_recurrences
                     actor
                     ~actor:proof
                     ~authorize:revoke_after_observation));
              assert (Int.equal !reads 2);
              assert (
                Int64.equal before_revocation (state ()).counters.transaction_sequence);
              current := true;
              let enable key =
                call
                  (request (catalog ()) ~source:actual_source key (Enable (id "actual")))
                |> protocol_ok
                |> ignore
              in
              enable "actual-enable";
              let attachment, _ =
                A.Session_actor.attach actor ~mode:Read_write ~subscribe:false
                |> protocol_ok
              in
              reject_stop := true;
              assert (
                Result.is_error
                  (A.Session_actor.stop_with_command_audit
                     actor
                     ~command_audit:(audit "failed-stop")
                     ~attachment_id:attachment.id
                     ~mode:Graceful));
              assert (
                E.equal_control
                  (E.control (entry (catalog ()) "actual"))
                  (Paused Controller_stop));
              assert (List.is_empty !accepted);
              reject_stop := false;
              enable "second-enable";
              A.Session_actor.stop_with_command_audit
                actor
                ~command_audit:(audit "accepted-stop")
                ~attachment_id:attachment.id
                ~mode:Cancel
              |> protocol_ok
              |> ignore;
              assert (Int.equal (List.length !accepted) 1);
              enable "after-stop-enable";
              A.Session_actor.detach actor attachment.id |> protocol_ok;
              assert (E.equal_control (E.control (entry (catalog ()) "actual")) Enabled);
              A.Session_actor.change_moderator actor None |> protocol_ok |> ignore;
              assert (
                E.equal_control
                  (E.control (entry (catalog ()) "actual"))
                  (Paused Source_review));
              call (request (catalog ()) "safe-no-source" (Pause (id "actual")))
              |> protocol_ok
              |> ignore;
              publish_lifecycle store handle (state ()) Archive "archive";
              S.read_lifecycle store handle
              |> store_ok
              |> S.Lifecycle.Observation.value
              |> check_receipt_expiry;
              let views =
                A.Session_actor.read_recurrences actor ~actor:proof ~authorize
                |> protocol_ok
              in
              assert (
                E.equal_control (List.hd_exn views).control (Retired Lifecycle_changed));
              let recovery_state = state () in
              let recovery_catalog = catalog () in
              A.Session_actor.shutdown actor;
              let observed = S.read_lifecycle store handle |> store_ok in
              let original_receipt =
                S.Lifecycle.Observation.value observed |> Archive.receipts |> List.hd_exn
              in
              assert (not original_receipt.completion_acknowledged);
              let receipt_store =
                Agent_store.Idempotency_store.open_or_create
                  ~env
                  ~path:(Filename.concat (S.Handle.directory handle) "lifecycle-receipts")
                |> store_ok
              in
              Agent_store.Idempotency_store.record
                receipt_store
                { key = original_receipt.key
                ; request_digest = original_receipt.request_digest
                ; accepted_transaction_sequence = None
                ; outcome = Pending
                ; created_at = timestamp
                ; expires_at = None
                ; retention = Protected
                }
              |> store_ok
              |> ignore;
              let receipt_owner =
                Agent_server.Session_lifecycle_receipts.create
                  ~store:receipt_store
                  ~server_id
              in
              let reconciled =
                A.Recurrence_recovery.reconcile_lifecycle_receipts
                  observed
                  ~store
                  ~handle
                  ~complete:
                    (Agent_server.Session_lifecycle_receipts.complete receipt_owner)
                |> protocol_ok
              in
              assert (
                Result.is_error (S.check_lifecycle_observation store handle observed));
              S.check_lifecycle_observation store handle reconciled |> store_ok;
              assert (
                List.for_all
                  (S.Lifecycle.Observation.value reconciled |> Archive.receipts)
                  ~f:(fun receipt -> receipt.completion_acknowledged));
              let unexpected_completion = ref 0 in
              assert (
                Result.is_error
                  (A.Recurrence_recovery.reconcile_lifecycle_receipts
                     observed
                     ~store
                     ~handle
                     ~complete:(fun ~key:_ ~request_digest:_ _ ->
                       Int.incr unexpected_completion;
                       Ok ())));
              assert (Int.equal !unexpected_completion 0);
              (* A real interrupted metadata/index publication rejects Current but
                 recovery's genuine Observation can still retire safely. *)
              let candidate =
                { recovery_state with
                  counters =
                    { recovery_state.counters with
                      revision = Int64.succ recovery_state.counters.revision
                    }
                }
              in
              S.prepare_canonical_projection
                store
                handle
                ~metadata:(metadata candidate)
                ~entry:(index_entry candidate)
              |> store_ok;
              assert (
                Result.is_error
                  (A.Recurrence_controller.observe controller recovery_state));
              assert (
                Result.is_error
                  (A.Recurrence_recovery.prepare
                     recovery_state
                     ~store
                     ~handle
                     ~observation:reconciled));
              let observed = S.read_lifecycle store handle |> store_ok in
              let recovery =
                A.Recurrence_recovery.prepare
                  recovery_state
                  ~store
                  ~handle
                  ~observation:observed
                |> protocol_ok
              in
              A.Recurrence_recovery.check recovery |> protocol_ok;
              let delta = A.Recurrence_recovery.delta recovery |> Option.value_exn in
              let transition =
                A.Session_transition.apply
                  recovery_state
                  ~delta
                  ~payloads:[]
                  ~now:timestamp
                |> protocol_ok
              in
              reject := true;
              assert (
                Result.is_error
                  (persistence.commit
                     ~command_audit:None
                     ~previous:recovery_state
                     transition));
              assert (
                E.equal_control
                  (E.control (entry recovery_catalog "actual"))
                  (Paused Operator_pause));
              reject := false;
              Persistence.commit
                durable
                ~command_audit:None
                ~previous:recovery_state
                transition
              |> protocol_ok;
              S.write_metadata
                ~entry:(index_entry transition.state)
                store
                handle
                (metadata transition.state)
              |> store_ok;
              let repaired =
                A.Recurrence_controller.observe controller transition.state |> protocol_ok
              in
              assert (not (A.Recurrence_controller.automatic repaired));
              assert (
                List.for_all
                  (C.entries (Option.value_exn transition.state.recurrence_catalog))
                  ~f:(fun entry ->
                    E.equal_control (E.control entry) (Retired Lifecycle_changed)));
              publish_lifecycle store handle transition.state Restore "restore";
              publish_lifecycle store handle transition.state Resume "resume";
              assert (not (A.Recurrence_controller.is_current repaired));
              let now_current =
                A.Recurrence_controller.observe controller transition.state |> protocol_ok
              in
              assert (A.Recurrence_controller.automatic now_current);
              let catalog = Option.value_exn transition.state.recurrence_catalog in
              assert (
                Result.is_error
                  (apply
                     catalog
                     ~lifecycle:(A.Recurrence_controller.revision now_current)
                     (request
                        catalog
                        ~lifecycle:(A.Recurrence_controller.revision now_current)
                        "no-resurrection"
                        (Enable (id "actual")))));
              Agent_store.Commit_writer.close writer;
              let journal =
                Agent_store.Journal.open_existing
                  ~env
                  ~directory
                  ~max_payload_length:1048576
                  ~max_segment_bytes:4194304L
                  ~max_segment_frames:32
                |> store_ok
              in
              let scan = Agent_store.Journal.scan journal |> store_ok in
              let transactions =
                List.filter_map scan.entries ~f:(fun entry ->
                  if Int.equal (Agent_store.Frame.flags entry.frame) 0
                  then
                    Some
                      (Agent_store.Transaction.decode
                         (Agent_store.Frame.payload entry.frame)
                       |> store_ok)
                  else None)
              in
              assert (
                Int.equal
                  (List.count transactions ~f:(fun t -> Option.is_some t.command_audit))
                  1);
              (* Reopening the actual journal validates exact retained receipt and
                 terminal metadata without replaying any target/runtime. *)
              let recovered =
                Agent_store.Recovery.load
                  ~env
                  ~journal
                  ~snapshot_directory:snapshots
                  ~max_snapshot_payload_length:1048576
                  ~session_id
                  ~initial:(Persistence.Restored.authored initial)
                  ~restore_snapshot:(Persistence.restore_snapshot ~limits:document_limits)
                  ~apply:(Persistence.apply_transaction ~limits:document_limits)
                  ~validate_transaction:
                    (Persistence.validate_transaction ~limits:document_limits)
                  ~validate:Persistence.validate
                |> store_ok
              in
              let restored = Persistence.Restored.state recovered.state in
              assert (
                D.Json.equal
                  (C.to_jsonaf catalog |> protocol_ok)
                  (C.to_jsonaf (Option.value_exn restored.recurrence_catalog)
                   |> protocol_ok));
              assert (
                A.Recurrence_command_receipt.equal
                  receipt
                  (List.hd_exn
                     (C.receipts (Option.value_exn restored.recurrence_catalog))))))));
  print_endline
    "real Current and Observation separated; failure/replay/Stop audits preserved; \
     pending repair and archive/resume never launch or resurrect";
  [%expect
    {| real Current and Observation separated; failure/replay/Stop audits preserved; pending repair and archive/resume never launch or resurrect |}]
;;
