open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module D = Document_schema
module Store = Agent_store
module Actor = A.Session_actor
module Persistence = A.Session_persistence
module State = A.Session_state
module Archive = A.Compaction_archive
module Ledger = A.Recovery_window_ledger
module Scope = P.Recovery_window.Scope
module Citation = P.Recovery_window.Citation

let payload_limit = 1048576

type fixture =
  { env : Eio_unix.Stdenv.base
  ; handle : Store.Session_store.Handle.t
  ; owner : P.Session_ref.t
  }

let history sequence text =
  let id =
    History_entry.Id.create ~namespace:"recovery-window-fixture" ~sequence
    |> Result.ok_or_failwith
  in
  A.History_codec.user_text ~id text |> A.History_codec.to_protocol
;;

let state actor = Actor.state actor |> protocol_ok
let ledger (state : State.t) = Option.value_exn state.recovery_windows
let scope state = Ledger.Window.scope (Ledger.active (ledger state))

let membership state =
  let window = Ledger.active (ledger state) in
  List.init (Ledger.Window.length window) ~f:(fun ordinal ->
    Ledger.Window.at window ~ordinal |> Option.value_exn)
;;

let has id state =
  List.exists (membership state) ~f:(fun member ->
    P.History.Id.equal id (Citation.occurrence (Ledger.Member.citation member)))
;;

let same_ledger before after =
  D.Json.equal (Ledger.to_jsonaf (ledger before)) (Ledger.to_jsonaf (ledger after))
;;

let actor_proof ?(id = principal_id) () =
  P.Principal.create
    ~id
    ~authentication_kind:"recovery.window.test"
    ~scopes:(P.Scope.Set.of_list [ Send_messages; View_session_transcript ])
    ~attributes:[]
  |> protocol_ok
  |> Operator_authorization.trusted_local
;;

let attach actor =
  let attachment, _, _, _ =
    Actor.attach_with_snapshot
      actor
      ~principal_id:(Some principal_id)
      ~reclaim_token:None
      ~mode:Owner_read_write
      ~subscribe:false
    |> protocol_ok
  in
  attachment.P.Session.Attachment.id
;;

let journal fixture make =
  let directory = Store.Session_store.Handle.journal_directory fixture.handle in
  make
    ~env:fixture.env
    ~directory
    ~max_payload_length:payload_limit
    ~max_segment_bytes:4194304L
    ~max_segment_frames:16
  |> store_ok
;;

let with_actor fixture sw journal ~restored ~previous_transaction_hash ~archive f =
  let initial_state = Persistence.Restored.state restored in
  let writer =
    Store.Commit_writer.create
      ~sw
      ~journal
      ~session_id
      ~next_transaction_sequence:(Int64.succ initial_state.counters.transaction_sequence)
      ~previous_transaction_hash
      ~queue_capacity:8
    |> store_ok
  in
  Exn.protect
    ~finally:(fun () -> Store.Commit_writer.close writer)
    ~f:(fun () ->
      let persistence =
        Persistence.create
          ~archive
          ~pending_archive:None
          ~before_commit:None
          ~retention_preflight:None
          ~command_accepted:(fun _ _ -> ())
          ~writer
          ~durability:Flush
          ~limits:document_limits
          ~archive_limits:document_limits
          ~restored
          ~previous_transaction_hash
      in
      let backend, canonical_persistence =
        Persistence.actor_persistence_with_startup_disposal
          persistence
          ~recovery_notes:(fixture.owner, payload_limit)
      in
      let actor =
        Actor.create_with_bound_runtime_source_owner
          ~canonical_persistence
          ~runtime_source_owner:
            (A.Runtime_source_owner.create () |> A.Runtime_source_owner.identity)
          ~ordinary_execution_admission:(fun () -> Ok ())
          ~schedule_permission_timeouts:false
          ~sw
          ~clock:(Eio.Stdenv.clock fixture.env)
          ~mailbox_capacity:16
          ~owner_lease_duration_ms:30000
          ~max_attachments:8
          ~subscriber_capacity:16
          ~compaction_env:None
          ~initial_state
          ~persistence:backend
          ~operation_worker:None
          ~services:
            { now = (fun () -> timestamp)
            ; monotonic_now = (fun () -> Mtime.min_stamp)
            ; create_attachment_id = P.Id.Attachment.create
            ; create_reclaim_token = (fun () -> "recovery-window-reclaim")
            ; state_committed = (fun _ _ -> ())
            ; job_results = None
            ; goal_descendant_profile
            ; subscription_limits = A.Staged_subscriptions.default_limits
            ; schedule_limits = A.Staged_schedules.default_limits
            ; notification_limits = A.Staged_notifications.default_limits
            ; ingress_limits = A.Staged_ingress.default_limits
            }
      in
      Exn.protect
        ~finally:(fun () -> Actor.shutdown actor)
        ~f:(fun () -> f actor persistence))
;;

let compact fixture actor attachment_id =
  Actor.compact
    actor
    ~attachment_id
    ~expected_revision:(Some (state actor).counters.revision)
  |> protocol_ok
  |> ignore;
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock fixture.env) 5. (fun () -> await_idle actor)
;;

let data_transaction journal state =
  let scan = Store.Journal.scan journal |> store_ok in
  List.find_map_exn scan.entries ~f:(fun entry ->
    let transaction =
      Store.Transaction.decode (Store.Frame.payload entry.frame) |> store_ok
    in
    Option.some_if
      (Int64.equal
         transaction.transaction_sequence
         state.State.counters.transaction_sequence)
      transaction)
;;

let delta_kinds transaction =
  match
    D.Json.field (D.Document.payload transaction.Store.Transaction.delta) ~name:"changes"
  with
  | Value (`Array changes) ->
    List.map changes ~f:(fun change ->
      match D.Json.field change ~name:"kind" with
      | Value (`String kind) -> kind
      | Absent | Null | Value _ -> failwith "delta kind absent")
  | Absent | Null | Value _ -> failwith "delta changes absent"
;;

let reference_eq (left : State.Compaction_archive.t) right =
  P.Id.Operation.equal left.operation_id right.State.Compaction_archive.operation_id
;;

let read_archive fixture =
  Archive.read ~env:fixture.env ~handle:fixture.handle ~max_payload_length:payload_limit
;;

let walk fixture sw ~fault ~initial =
  let journal = journal fixture Store.Journal.create in
  let actual_archive =
    Archive.write_document
      ~env:fixture.env
      ~handle:fixture.handle
      ~max_payload_length:payload_limit
  in
  let written = ref [] in
  let fail_next = ref false in
  let archive reference document =
    let%bind.Result () = actual_archive reference document in
    written := reference :: !written;
    if !fail_next
    then (
      fail_next := false;
      Error
        (P.Error.create
           Persistence_error
           ~message:"after archive sync"
           ~retryable:false
           ()))
    else Ok ()
  in
  with_actor
    fixture
    sw
    journal
    ~restored:initial
    ~previous_transaction_hash:None
    ~archive
    (fun actor _ ->
       let writer = attach actor in
       Actor.start actor ~attachment_id:writer |> protocol_ok |> ignore;
       let append entry =
         Actor.append_history actor ~attachment_id:writer [ entry ]
         |> protocol_ok
         |> ignore
       in
       let legacy = history 0 "this was pruned before opt-in" in
       append legacy;
       Inference_ports.install_compaction_runtime actor |> protocol_ok;
       let pre_enable = compact fixture actor writer in
       assert (Option.is_none pre_enable.recovery_windows);
       let legacy_ref = List.hd_exn pre_enable.conversation.compaction_archives in
       let legacy_archive = read_archive fixture legacy_ref |> protocol_ok in
       assert (Option.is_none legacy_archive.recovery_windows);
       let untracked = data_transaction journal pre_enable in
       assert (List.mem (delta_kinds untracked) "compaction_archived" ~equal:String.equal);
       assert (
         not
           (List.mem
              (D.Document.required_semantics untracked.delta)
              P.Session_storage_semantics.recovery_windows
              ~equal:String.equal));
       let enable =
         Actor.enable_recovery_windows actor ~actor:(actor_proof ()) ~attachment_id:writer
       in
       assert (
         Result.is_error
           (enable ~authorize:(fun _ -> Error (P.Error.invalid_request "fixture denied"))));
       let unrelated = P.Id.Principal.of_string "pri_unrelated_writer" |> protocol_ok in
       (match
          Actor.enable_recovery_windows
            actor
            ~actor:(actor_proof ~id:unrelated ())
            ~attachment_id:writer
            ~authorize:(fun _ -> Ok ())
        with
        | Error failure -> assert (P.Error.equal_code failure.code Permission_denied)
        | Ok _ -> failwith "an unrelated principal reused the creation-owner attachment");
       assert (Option.is_none (state actor).recovery_windows);
       let enable () = enable ~authorize:(fun _ -> Ok ()) |> protocol_ok in
       let active = enable () in
       let began = state actor in
       assert (List.length (membership began) = 1 && not (has legacy.id began));
       let bootstrap_delta = data_transaction journal began in
       assert (
         List.equal
           String.equal
           (delta_kinds bootstrap_delta)
           [ "recovery_window_enabled" ]);
       assert (
         List.mem
           (D.Document.required_semantics bootstrap_delta.delta)
           P.Session_storage_semantics.recovery_windows
           ~equal:String.equal);
       assert (Scope.equal active (enable ()));
       [%test_eq: int64] began.counters.revision (state actor).counters.revision;
       let live = history 1 "new member before tracked compaction" in
       append live;
       let appended = state actor in
       let append_delta = data_transaction journal appended in
       assert (
         List.equal
           String.equal
           (delta_kinds append_delta)
           [ "canonical_entries_appended" ]);
       assert (has live.id appended);
       let before_member = List.last_exn (membership appended) in
       (match Ledger.Member.source before_member with
        | Live basis -> [%test_eq: int64] appended.counters.revision basis.revision
        | Summary_archive _ -> failwith "ordinary append was already archived");
       fail_next := fault;
       let after = compact fixture actor writer in
       if List.length !written <> 2
       then (
         let terminal = data_transaction journal after in
         let events =
           Persistence.durable_events ~limits:document_limits terminal |> store_ok
         in
         let failure =
           List.find_map events ~f:(fun event ->
             match P.Event.Durable.Payload.of_json ~kind:event.kind event.payload with
             | Ok (Operation_failed { state = Failed failure; _ }) -> Some failure.message
             | Ok _ | Error _ -> None)
         in
         failwith
           (Option.value failure ~default:"tracked compaction never reached archive"));
       let captured = List.hd_exn !written in
       let archived = read_archive fixture captured |> protocol_ok in
       let archived_row =
         List.nth_exn
           archived.conversation.canonical_history
           (Ledger.Member.source_ordinal before_member)
       in
       assert (P.History.equal_entry archived_row live);
       assert (Scope.equal active (scope after) && same_ledger appended archived);
       assert (List.length (Ledger.windows (ledger after)) = 1);
       assert (not (has legacy.id after));
       if fault
       then (
         assert (same_ledger appended after);
         assert (
           List.equal reference_eq after.conversation.compaction_archives [ legacy_ref ]))
       else (
         assert (
           List.equal
             reference_eq
             after.conversation.compaction_archives
             [ captured; legacy_ref ]);
         let terminal = data_transaction journal after in
         assert (
           List.mem
             (delta_kinds terminal)
             "recovery_summary_compacted"
             ~equal:String.equal);
         let relocated =
           List.find_exn (membership after) ~f:(fun member ->
             Citation.equal
               (Ledger.Member.citation member)
               (Ledger.Member.citation before_member))
         in
         (match Ledger.Member.source relocated with
          | Summary_archive root ->
            assert (P.Id.Operation.equal root.operation captured.operation_id);
            [%test_eq: int64] root.basis.revision captured.revision;
            [%test_eq: string] (Chatml_source.Digest.to_hex root.digest) captured.sha256
          | Live _ -> failwith "pruned member still names a live row");
         let summary = List.last_exn (membership after) in
         match Ledger.Member.provenance summary with
         | Derived_summary operation ->
           assert (P.Id.Operation.equal operation captured.operation_id)
         | Observed | Carried_from _ ->
           failwith "summary lacks its exact derived provenance");
       after)
;;

let save_edit actor writer ~owner =
  let original = history 2 "canonical text before edit" in
  ignore (Actor.append_history actor ~attachment_id:writer [ original ] |> protocol_ok);
  let before = state actor in
  let member_for state entry =
    List.find_exn (membership state) ~f:(fun member -> Ledger.Member.matches member entry)
  in
  let before_member = member_for before original in
  let edit =
    P.History_edit.create
      ~history_id:original.id
      ~expected_content_revision:original.content_revision
      ~text:"canonical text after edit"
      ~mode:Edit_and_continue
    |> protocol_ok
  in
  let request : P.History_edit.Edit_request.t =
    { session_id
    ; attachment_id = writer
    ; expected_generation = before.identity.generation
    ; expected_revision = before.counters.revision
    ; edit
    ; idempotency_key = P.Idempotency_key.of_string "recovery-window-edit" |> protocol_ok
    }
  in
  ignore (Actor.edit_history actor request |> protocol_ok : Actor.History_edit_result.t);
  let after = state actor in
  let entry = List.last_exn after.conversation.canonical_history in
  assert (P.History.Id.equal original.id entry.id);
  let member = member_for after entry in
  let view = A.Recovery_visibility.create after ~owner |> protocol_ok in
  (match
     ( A.Recovery_visibility.select view (Ledger.Member.citation before_member)
     , A.Recovery_visibility.select view (Ledger.Member.citation member) )
   with
   | Withdrawn, Eligible _ -> ()
   | _ -> failwith "edit did not withdraw the old citation and expose the new revision");
  assert (
    P.Recovery_window.ordinal
      (Ledger.Window.manifest (Ledger.active (ledger before)))
      (Ledger.Member.citation before_member)
    |> Option.equal
         Int.equal
         (P.Recovery_window.ordinal
            (Ledger.Window.manifest (Ledger.active (ledger after)))
            (Ledger.Member.citation before_member)));
  assert (
    not
      (Citation.equal
         (Ledger.Member.citation before_member)
         (Ledger.Member.citation member)));
  (match Ledger.Member.source member with
   | Live basis -> [%test_eq: int64] after.counters.revision basis.revision
   | Summary_archive _ -> failwith "edited member was bound to an archive");
  after
;;

let reopen fixture sw ~initial ~after ~fault =
  let journal = journal fixture Store.Journal.open_existing in
  let replay () =
    Store.Recovery.load
      ~env:fixture.env
      ~journal
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
  let recovered = replay () in
  let restored = Persistence.Restored.state recovered.state in
  assert (same_ledger after restored);
  assert (
    List.equal
      reference_eq
      after.conversation.compaction_archives
      restored.conversation.compaction_archives);
  let archive =
    Archive.write_document
      ~env:fixture.env
      ~handle:fixture.handle
      ~max_payload_length:payload_limit
  in
  with_actor
    fixture
    sw
    journal
    ~restored:recovered.state
    ~previous_transaction_hash:recovered.latest_transaction_hash
    ~archive
    (fun actor persistence ->
       let writer = attach actor in
       let active =
         Actor.enable_recovery_windows
           actor
           ~actor:(actor_proof ())
           ~attachment_id:writer
           ~authorize:(fun _ -> Ok ())
         |> protocol_ok
       in
       assert (Scope.equal active (scope restored));
       if not fault
       then (
         Actor.stop actor ~attachment_id:writer ~mode:Cancel |> protocol_ok |> ignore;
         let edited = save_edit actor writer ~owner:fixture.owner in
         let edit_frame = data_transaction journal edited in
         assert (
           List.mem
             (D.Document.required_semantics edit_frame.delta)
             P.Session_storage_semantics.recovery_withdrawals
             ~equal:String.equal);
         let replayed = Persistence.Restored.state (replay ()).state in
         assert (same_ledger edited replayed);
         Actor.reset
           actor
           ~attachment_id:writer
           ~expected_revision:(state actor).counters.revision
           { keep_history = false
           ; keep_tasks = false
           ; keep_grants = false
           ; keep_labels = true
           ; workspace_instance = None
           }
         |> protocol_ok
         |> ignore;
         let reset = state actor in
         [%test_eq: int] 1 (Scope.generation (scope reset));
         assert (
           not
             (P.Id.Context_window.equal
                (Scope.window active)
                (Scope.window (scope reset))));
         assert (List.is_empty (membership reset));
         let reference = List.hd_exn reset.conversation.compaction_archives in
         let pre_reset = read_archive fixture reference |> protocol_ok in
         assert (same_ledger edited pre_reset);
         ignore
           (Persistence.install_snapshot
              persistence
              ~env:fixture.env
              ~handle:fixture.handle
              ~max_payload_length:payload_limit
              ~transaction_hash:(Persistence.transaction_hash persistence)
              reset
            |> store_ok
            : Store.Snapshot.installed));
       printf
         "fault=%b journal_replayed=true archive_references=%d\n"
         fault
         (List.length restored.conversation.compaction_archives))
;;

let%expect_test
    "real actor journals canonical window capture and rejects an orphan summary reference"
  =
  List.iter [ false; true ] ~f:(fun fault ->
    with_actor_workspace (fun env workspace_instance ->
      Eio.Switch.run (fun scope ->
        let state =
          actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
        in
        let storage = Job_artifact_fixtures.create env scope state in
        let owner =
          P.Session_ref.create
            ~server_id:(Store.Session_store.server_id storage.sessions)
            ~session_id
        in
        let fixture = { env; handle = storage.session; owner } in
        let initial = Persistence.Restored.authored state in
        let after = Eio.Switch.run (fun sw -> walk fixture sw ~fault ~initial) in
        Eio.Switch.run (fun sw -> reopen fixture sw ~initial ~after ~fault))));
  [%expect
    {|
    fault=false journal_replayed=true archive_references=2
    fault=true journal_replayed=true archive_references=1
    |}]
;;
