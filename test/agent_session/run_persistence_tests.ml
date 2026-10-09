open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module Store = Agent_store
module Persistence = A.Session_persistence

let limits = document_limits

let create_persistence writer restored ~previous_transaction_hash =
  Persistence.create
    ~archive:(fun _ _ -> failwith "unexpected archive in run admission fixture")
    ~pending_archive:None
    ~before_commit:None
    ~command_accepted:(fun _ _ -> ())
    ~writer
    ~durability:Flush
    ~limits
    ~archive_limits:limits
    ~retention_preflight:None
    ~restored
    ~previous_transaction_hash
;;

let create_actor env sw initial persistence =
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
      ; create_attachment_id =
          (fun () -> P.Id.Attachment.of_string "att_run_persistence" |> protocol_ok)
      ; create_reclaim_token = (fun () -> "run-persistence-reclaim")
      ; state_committed = (fun _ _ -> ())
      ; job_results = None
      ; subscription_limits = A.Staged_subscriptions.default_limits
      ; schedule_limits = A.Staged_schedules.default_limits
      ; notification_limits = A.Staged_notifications.default_limits
      ; ingress_limits = A.Staged_ingress.default_limits
      }
;;

let receipt state (request : P.Run_start.t) digest =
  A.Run_state.receipt
    (Option.value state.A.Session_state.run_state ~default:A.Run_state.empty)
    ~principal_id
    ~key:request.key
    ~request_sha256:digest
  |> protocol_ok
;;

let%expect_test
    "lost admission acknowledgement reopens exact receipt and interrupts without replay"
  =
  with_actor_workspace (fun env workspace_instance ->
    let manager, _, _ = handoff_definition env in
    let snapshot =
      Chat_response.Moderator_manager.identity_snapshot manager |> Result.ok_or_failwith
    in
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let initial =
      { initial with moderator = Some (A.Moderator_checkpoint.encode snapshot) }
    in
    let observer =
      A.Moderator_checkpoint.observer initial.moderator |> protocol_ok |> Option.value_exn
    in
    let directory =
      Filename.concat workspace_instance.canonical_root.native_path "run-journal"
    in
    let snapshots =
      Filename.concat workspace_instance.canonical_root.native_path "run-snapshots"
    in
    Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / snapshots);
    let request, digest, expected, uncertain, live_did_not_claim_admission =
      Eio.Switch.run (fun sw ->
        let journal =
          Store.Journal.create
            ~env
            ~directory
            ~max_payload_length:1048576
            ~max_segment_bytes:4194304L
            ~max_segment_frames:16
          |> store_ok
        in
        let writer =
          Store.Commit_writer.create
            ~sw
            ~journal
            ~session_id
            ~next_transaction_sequence:1L
            ~previous_transaction_hash:None
            ~queue_capacity:8
          |> store_ok
        in
        let durable =
          create_persistence
            writer
            (Persistence.Restored.authored initial)
            ~previous_transaction_hash:None
        in
        let lose_acknowledgement = ref false in
        let persistence : A.Session_actor.persistence =
          { archive_reference
          ; commit =
              (fun ~command_audit ~previous transition ->
                let open Result.Let_syntax in
                let%bind () =
                  Persistence.commit durable ~command_audit ~previous transition
                in
                if !lose_acknowledgement
                then (
                  lose_acknowledgement := false;
                  Error
                    (P.Error.create
                       Persistence_error
                       ~retryable:true
                       ~message:"fixture lost acknowledgement after actual durable commit"
                       ()))
                else Ok ())
          }
        in
        let actor = create_actor env sw initial persistence in
        Exn.protect
          ~finally:(fun () ->
            A.Session_actor.shutdown actor;
            Store.Commit_writer.close writer)
          ~f:(fun () ->
            let attachment, _ =
              A.Session_actor.attach actor ~mode:Read_write ~subscribe:false
              |> protocol_ok
            in
            let state = A.Session_actor.state actor |> protocol_ok in
            let request =
              P.Run_start.create
                ~session_id
                ~attachment_id:attachment.id
                ~generation:state.identity.generation
                ~expected_revision:state.counters.revision
                ~mode:Workflow
                ~input:Authored_start
                ~key:
                  (P.Idempotency_key.of_string "physical-lost-admission-ack"
                   |> protocol_ok)
              |> protocol_ok
            in
            let digest =
              P.Json_codec.canonical_string (P.Run_start.to_json request)
              |> protocol_ok
              |> Digestif.SHA256.digest_string
              |> Digestif.SHA256.to_hex
            in
            let preparation =
              A.Session_actor.begin_run_preparation
                actor
                ~authorize:(fun _ -> Ok ())
                ~principal_id
                ~request
                ~request_sha256:digest
              |> protocol_ok
              |> function
              | A.Run_preparation.Decision.Prepare preparation -> preparation
              | Retained _ -> failwith "fresh physical fixture retained a receipt"
            in
            let scope =
              A.Run_admission.Scope.create
                ~principal_id
                ~observer
                ~startup_pending:(fun () -> true)
                ~authorize:(fun _ -> Ok ())
              |> protocol_ok
            in
            let session =
              P.Session_ref.create
                ~server_id:(P.Id.Server.of_string "srv_run_persistence" |> protocol_ok)
                ~session_id
            in
            lose_acknowledgement := true;
            let outcome =
              A.Session_actor.admit_prepared_run
                actor
                ~command_audit:None
                ~preparation
                ~scope
                ~session
                ~entry:None
              |> protocol_ok
            in
            let uncertain =
              match outcome with
              | A.Run_admission_outcome.Uncertain _ -> true
              | Admitted _ | Rejected _ -> false
            in
            let live = A.Session_actor.state actor |> protocol_ok in
            let live_did_not_claim_admission =
              Option.is_none (receipt live request digest)
            in
            let accepted = Persistence.Restored.state (Persistence.restored durable) in
            let expected = receipt accepted request digest |> Option.value_exn in
            request, digest, expected, uncertain, live_did_not_claim_admission))
    in
    Eio.Switch.run (fun sw ->
      (* The old writer and actor are joined before a distinct owner opens disk. *)
      let journal =
        Store.Journal.open_existing
          ~env
          ~directory
          ~max_payload_length:1048576
          ~max_segment_bytes:4194304L
          ~max_segment_frames:16
        |> store_ok
      in
      let recovered =
        Store.Recovery.load
          ~env
          ~journal
          ~snapshot_directory:snapshots
          ~max_snapshot_payload_length:1048576
          ~session_id
          ~initial:(Persistence.Restored.authored initial)
          ~restore_snapshot:(Persistence.restore_snapshot ~limits)
          ~apply:(Persistence.apply_transaction ~limits)
          ~validate_transaction:(Persistence.validate_transaction ~limits)
          ~validate:Persistence.validate
        |> store_ok
      in
      let restored = Persistence.Restored.state recovered.state in
      let reopened_exact_receipt =
        Option.equal P.Run_receipt.equal (Some expected) (receipt restored request digest)
      in
      let writer =
        Store.Commit_writer.create
          ~sw
          ~journal
          ~session_id
          ~next_transaction_sequence:(Int64.succ recovered.latest_transaction_sequence)
          ~previous_transaction_hash:recovered.latest_transaction_hash
          ~queue_capacity:8
        |> store_ok
      in
      let durable =
        create_persistence
          writer
          recovered.state
          ~previous_transaction_hash:recovered.latest_transaction_hash
      in
      let actor = create_actor env sw restored (Persistence.actor_persistence durable) in
      Exn.protect
        ~finally:(fun () ->
          A.Session_actor.shutdown actor;
          Store.Commit_writer.close writer)
        ~f:(fun () ->
          A.Session_actor.reconcile_run_recovery actor |> protocol_ok;
          let reconciled = A.Session_actor.state actor |> protocol_ok in
          let index = Option.value_exn reconciled.run_state in
          let run = A.Run_state.find index expected.run_id |> Option.value_exn in
          let interrupted = P.Run.Lifecycle.equal run.lifecycle (Terminal Interrupted) in
          let original_receipt_retained =
            Option.equal
              P.Run_receipt.equal
              (Some expected)
              (receipt reconciled request digest)
          in
          let one_terminal_receipt =
            Int.equal
              1
              (List.count (A.Run_state.receipts index) ~f:(fun receipt ->
                 P.Run_receipt.Kind.equal receipt.kind Terminal))
          in
          A.Session_actor.reconcile_run_recovery actor |> protocol_ok;
          let repeated = A.Session_actor.state actor |> protocol_ok in
          let recovery_did_not_replay =
            Int64.equal reconciled.counters.revision repeated.counters.revision
            && Option.is_none repeated.active_operation
          in
          print_s
            [%sexp
              { uncertain : bool
              ; live_did_not_claim_admission : bool
              ; reopened_exact_receipt : bool
              ; interrupted : bool
              ; original_receipt_retained : bool
              ; one_terminal_receipt : bool
              ; recovery_did_not_replay : bool
              }])));
  [%expect
    {|
    ((uncertain true) (live_did_not_claim_admission true)
     (reopened_exact_receipt true) (interrupted true)
     (original_receipt_retained true) (one_terminal_receipt true)
     (recovery_did_not_replay true))
  |}]
;;

let%expect_test "physical retry carrier survives reopen and complete result collection" =
  let storage = ref None in
  let fixture_env = ref None in
  let initial = ref None in
  let writer = ref None in
  let preserve_marker = ref false in
  let moved_marker = ref false in
  let marker_paths env =
    let fixture = Option.value_exn !storage in
    let directory =
      Store.Session_store.Handle.directory fixture.Job_artifact_fixtures.session
    in
    ( Eio.Path.(Eio.Stdenv.fs env / Filename.concat directory "result-preparations")
    , Eio.Path.(Eio.Stdenv.fs env / Filename.concat directory "held-result-preparations")
    )
  in
  Run_admission_tests.with_startup_actor
    ~make_job_results:(fun env sw state ->
      let fixture = Job_artifact_fixtures.create env sw state in
      storage := Some fixture;
      fixture.publisher)
    ~make_persistence:(fun env sw state ->
      fixture_env := Some env;
      initial := Some state;
      let handle = (Option.value_exn !storage).Job_artifact_fixtures.session in
      let journal =
        Store.Journal.create
          ~env
          ~directory:(Store.Session_store.Handle.journal_directory handle)
          ~max_payload_length:1048576
          ~max_segment_bytes:4194304L
          ~max_segment_frames:16
        |> store_ok
      in
      let owner =
        Store.Commit_writer.create
          ~sw
          ~journal
          ~session_id
          ~next_transaction_sequence:1L
          ~previous_transaction_hash:None
          ~queue_capacity:8
        |> store_ok
      in
      writer := Some owner;
      let durable =
        create_persistence
          owner
          (Persistence.Restored.authored state)
          ~previous_transaction_hash:None
      in
      { A.Session_actor.archive_reference
      ; commit =
          (fun ~command_audit ~previous transition ->
            let open Result.Let_syntax in
            let%map () = Persistence.commit durable ~command_audit ~previous transition in
            if !preserve_marker
            then (
              preserve_marker := false;
              let original, held = marker_paths env in
              (* Move the real checksummed intent after authoritative publication.
              Marker cleanup cannot remove it here, modelling its documented
              failure-after-success path without fabricating an ownership label. *)
              Eio.Path.rename original held;
              moved_marker := true))
      })
    ~after_close:(fun env _ (expected_frame, expected_failure, second_attempt) ->
      let old_storage = Option.value_exn !storage in
      Eio.Switch.run (fun sw ->
        let sessions =
          Store.Session_store.open_existing
            ~env
            ~sw
            ~root:
              (Store.Data_root.path (Store.Session_store.data_root old_storage.sessions))
            ~process_start_identity:None
            ~lock_nonce:"retry-carrier-reopen"
          |> store_ok
        in
        let handle =
          Store.Session_store.open_session
            sessions
            ~sw
            ~actor_lock_nonce:"retry-carrier-reopen"
            session_id
          |> store_ok
        in
        let journal =
          Store.Journal.open_existing
            ~env
            ~directory:(Store.Session_store.Handle.journal_directory handle)
            ~max_payload_length:1048576
            ~max_segment_bytes:4194304L
            ~max_segment_frames:16
          |> store_ok
        in
        let recovered =
          Store.Recovery.load
            ~env
            ~journal
            ~snapshot_directory:(Store.Session_store.Handle.snapshot_directory handle)
            ~max_snapshot_payload_length:1048576
            ~session_id
            ~initial:(Persistence.Restored.authored (Option.value_exn !initial))
            ~restore_snapshot:(Persistence.restore_snapshot ~limits)
            ~apply:(Persistence.apply_transaction ~limits)
            ~validate_transaction:(Persistence.validate_transaction ~limits)
            ~validate:Persistence.validate
          |> store_ok
        in
        let restored = Persistence.Restored.state recovered.state in
        let index = Option.value_exn restored.run_state in
        let delivery = A.Run_state.job_deliveries index |> List.hd_exn in
        let frame = A.Run_job_delivery.frame delivery in
        let exact_reopened_frame =
          Chat_response.Background_delivery.equal frame expected_frame
        in
        let newest_attempt_is_distinct =
          List.exists restored.jobs ~f:(fun job ->
            P.Id.Job.equal job.id frame.job_id && Int.equal job.attempt second_attempt)
        in
        let owner =
          Store.Commit_writer.create
            ~sw
            ~journal
            ~session_id
            ~next_transaction_sequence:(Int64.succ recovered.latest_transaction_sequence)
            ~previous_transaction_hash:recovered.latest_transaction_hash
            ~queue_capacity:8
          |> store_ok
        in
        let durable =
          create_persistence
            owner
            recovered.state
            ~previous_transaction_hash:recovered.latest_transaction_hash
        in
        let actor =
          create_actor env sw restored (Persistence.actor_persistence durable)
        in
        let runtime =
          Agent_server.Runtime_owner.create ~actor ~initial:None ~build:(fun () ->
            failwith "collection must not construct a runtime")
        in
        Exn.protect
          ~finally:(fun () ->
            Agent_server.Runtime_owner.close_and_wait runtime;
            A.Session_actor.shutdown actor;
            Store.Commit_writer.close owner;
            Store.Session_store.close_session sessions handle |> store_ok;
            Store.Session_store.close sessions |> store_ok)
          ~f:(fun () ->
            (* Actual recovered worker custody is interrupted before run recovery,
               matching the host ordering; no result or callback is rerun. *)
            let job =
              List.find_exn restored.jobs ~f:(fun job ->
                P.Id.Job.equal job.id frame.job_id)
            in
            ignore
              (A.Session_actor.interrupt_job
                 actor
                 ~job_id:job.id
                 ~generation:job.generation
                 ~attempt:job.attempt
                 ~reason:"physical fixture process restart"
               |> protocol_ok
               : P.Job.t);
            A.Session_actor.reconcile_run_recovery actor |> protocol_ok;
            let state = A.Session_actor.state actor |> protocol_ok in
            let index = Option.value_exn state.run_state in
            let no_pending_delivery =
              List.for_all (A.Run_state.job_deliveries index) ~f:(fun delivery ->
                match A.Run_job_delivery.disposition delivery with
                | Retired _ -> true
                | Pending | Enqueued _ | Claimed _ -> false)
            in
            let receipt_once =
              Int.equal
                1
                (List.count (A.Run_state.receipts index) ~f:(fun receipt ->
                   P.Run_receipt.Kind.equal receipt.kind Terminal))
            in
            A.Session_actor.reconcile_run_recovery actor |> protocol_ok;
            let repeated = A.Session_actor.state actor |> protocol_ok in
            let recovery_did_not_replay =
              Int64.equal state.counters.revision repeated.counters.revision
            in
            let publisher =
              Store.Job_result_store.Publisher.create
                ~env
                ~sw
                ~blobs:old_storage.blobs
                ~session:handle
                ~principal:principal_id
                ~inline_bytes:64
                ~max_bytes:4096
              |> protocol_ok
            in
            let idempotency_store =
              Store.Idempotency_store.open_or_create
                ~env
                ~path:
                  (Filename.concat
                     (Store.Session_store.Handle.idempotency_directory handle)
                     "carrier-collection.json")
              |> store_ok
            in
            let documents =
              List.concat_map recovered.transactions ~f:(fun transaction ->
                transaction.Store.Transaction.durable_events)
              |> List.map ~f:(fun document ->
                A.Durable_event_document.decode ~limits document |> document_ok)
            in
            let durable_events =
              A.Durable_event_log.create
                ~documents
                ~capacity:256
                (List.map documents ~f:A.Durable_event_document.value)
              |> protocol_ok
            in
            let collection =
              Agent_server.Result_retention.collect
                ~runtime
                ~actor
                ~publisher
                ~handle
                ~journal
                ~persistence:durable
                ~durable_events
                ~idempotency_store
                ~limits:
                  { max_intents = 16
                  ; max_entries = 1024
                  ; max_bytes = 16777216
                  ; max_file_bytes = 1048576
                  }
                ~max_frame_bytes:1048576
                ~max_events:256
              |> protocol_ok
              |> Option.value_exn
            in
            let collection_retained =
              Int.equal collection.retained 1 && Int.equal collection.discarded 0
            in
            let artifact_still_readable =
              match frame.result with
              | Artifact { reference = artifact; outcome = _ } ->
                P.Completion.equal
                  expected_failure
                  (Store.Job_result_store.Publisher.load publisher artifact |> protocol_ok)
              | Inline _ -> false
            in
            let frame_cannot_replay =
              let event =
                Chat_response.Background_delivery.capture frame
                |> Session.Snapshot.of_value
                |> Result.ok_or_failwith
              in
              Option.is_some
                (A.Queued_moderator_event.delivery_retirement_reason
                   ~state:repeated
                   ~observer:frame.source
                   ~event
                   ~subscription_expired:(fun _ -> Ok false)
                 |> protocol_ok)
            in
            print_s
              [%sexp
                { exact_reopened_frame : bool
                ; newest_attempt_is_distinct : bool
                ; no_pending_delivery : bool
                ; receipt_once : bool
                ; recovery_did_not_replay : bool
                ; collection_retained : bool
                ; artifact_still_readable : bool
                ; frame_cannot_replay : bool
                }])))
    (fun actor _ _ request start manager _ _ attachment ->
       Exn.protect
         ~finally:(fun () ->
           A.Session_actor.shutdown actor;
           Store.Commit_writer.close (Option.value_exn !writer))
         ~f:(fun () ->
           ignore
             (start (request "physical-retry-carrier") |> protocol_ok : P.Run_receipt.t);
           let before =
             Chat_response.Moderator_manager.identity_snapshot manager
             |> Result.ok_or_failwith
           in
           let staged = ref None in
           A.Session_actor.with_current_moderator_event
             actor
             ~operation_id:None
             ~event:Session_start
             ~snapshot:(fun () -> Ok before)
             (fun ~executing ~retirement_reason:_ ~event:_ ~execute:_ ~commit ->
                let owner = P.Job.Moderator_event executing.context.id in
                let captured =
                  Background_execution_tests.capture_tool
                    (native_registry (ref 0) ~raises:false)
                    Chat_response.One_off_request.default_policy
                in
                let job =
                  A.Session_actor.prepare_background_job_launch actor ~owner captured
                  |> protocol_ok
                in
                staged := Some job;
                A.Session_actor.stage_background_job
                  actor
                  ~job
                  ~capacity:{ publish = ignore; abort = ignore }
                |> protocol_ok;
                A.Session_actor.select_background_jobs actor ~owner ~ids:[ job.id ]
                |> protocol_ok;
                let run =
                  (A.Session_actor.state actor |> protocol_ok).run_state
                  |> Option.value_exn
                  |> A.Run_state.runs
                  |> List.hd_exn
                in
                let wake =
                  P.Run_wake.create
                    ~run_id:run.id
                    ~source:run.source
                    ~occurrence:(Job_completion { job_id = job.id; attempt = 1 })
                  |> protocol_ok
                in
                let service =
                  A.Session_actor.run_actions actor executing
                  |> protocol_ok
                  |> Option.value_exn
                in
                let transaction = A.Run_action_service.transaction service in
                let ticket =
                  transaction.handlers.stage (Wait wake) |> Result.ok_or_failwith
                in
                ignore
                  (transaction.prepare [ ticket ] |> Result.ok_or_failwith
                   : P.Run_action.t option);
                commit
                  ~snapshot:before
                  ~requests:
                    { request_turn = false
                    ; request_compaction = false
                    ; end_session = None
                    })
           |> protocol_ok
           |> ignore;
           let staged = Option.value_exn !staged in
           ignore
             (A.Session_actor.change_job
                actor
                ~attachment_id:attachment.id
                { staged with
                  retry_policy = Safe_retry { max_attempts = 2; backoff_ms = 0 }
                }
              |> protocol_ok
              : P.Session.t);
           let first =
             A.Session_actor.claim_job
               actor
               ~job_id:staged.id
               ~generation:staged.generation
             |> protocol_ok
             |> Option.value_exn
           in
           let failure =
             P.Completion.Failed
               { code = "fixture.retry"
               ; message = "durable failed attempt"
               ; retryable = true
               ; details = `String (String.make 2000 'x')
               }
           in
           preserve_marker := true;
           let queued =
             A.Session_actor.complete_background_job
               actor
               ~job_id:first.id
               ~generation:first.generation
               ~attempt:first.attempt
               failure
             |> protocol_ok
           in
           if not !moved_marker
           then failwith "completion did not preserve its actual marker";
           let original, held = marker_paths (Option.value_exn !fixture_env) in
           Eio.Path.rename held original;
           moved_marker := false;
           let second =
             A.Session_actor.claim_job
               actor
               ~job_id:queued.id
               ~generation:queued.generation
             |> protocol_ok
             |> Option.value_exn
           in
           let state = A.Session_actor.state actor |> protocol_ok in
           let delivery =
             Option.value_exn state.run_state |> A.Run_state.job_deliveries |> List.hd_exn
           in
           A.Run_job_delivery.frame delivery, failure, second.attempt));
  [%expect
    {|
    ((exact_reopened_frame true) (newest_attempt_is_distinct true)
     (no_pending_delivery true) (receipt_once true)
     (recovery_did_not_replay true) (collection_retained true)
     (artifact_still_readable true) (frame_cannot_replay true))
    |}]
;;
