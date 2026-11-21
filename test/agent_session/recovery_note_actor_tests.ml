open! Core
open Fixtures
module A = Agent_session
module Actor = A.Session_actor
module Access = A.Recovery_note_access
module C = Recovery_notes.Catalog
module D = Document_schema
module P = Agent_protocol
module Store = Agent_store
module Persistence = A.Session_persistence

let owner =
  P.Session_ref.create
    ~server_id:(P.Id.Server.of_string "srv_recovery_notes" |> protocol_ok)
    ~session_id
;;

let principal scopes =
  P.Principal.create
    ~id:principal_id
    ~authentication_kind:"recovery.note.test"
    ~scopes:(P.Scope.Set.of_list scopes)
    ~attributes:[]
  |> protocol_ok
;;

let proof scopes = Operator_authorization.trusted_local (principal scopes)
let allow _ = Ok ()

let access actor ?attachment_id ?(authorize = allow) () =
  Access.operator actor ~owner ~generation:0 ~attachment_id ~authorize
;;

let key text = P.Idempotency_key.of_string text |> protocol_ok
let content text = C.Content.of_string text |> protocol_ok

let create text =
  C.Create
    { expected_catalog_revision = C.Catalog_revision.of_int64 0L |> protocol_ok
    ; path = C.Note_path.of_string "recovery/decisions.md" |> protocol_ok
    ; content = content text
    }
;;

let expect_error code = function
  | Error actual -> assert (P.Error.equal_code actual.P.Error.code code)
  | Ok _ -> failwith "unexpected authorized success"
;;

let set json name value =
  match json with
  | `Object fields ->
    `Object ((name, value) :: List.Assoc.remove fields name ~equal:String.equal)
  | _ -> failwith "expected object"
;;

let get json name =
  match D.Json.field json ~name with
  | Value value -> value
  | Absent | Null -> failwith ("missing " ^ name)
;;

let without_note_semantics document =
  let remaining =
    D.Document.required_semantics document
    |> List.filter ~f:(fun name ->
      not (String.equal name P.Session_storage_semantics.recovery_notes))
    |> List.map ~f:(fun value -> `String value)
  in
  set (D.Document.json document) "required_semantics" (`Array remaining)
  |> D.Document.inspect ~limits:document_limits
  |> document_ok
;;

let state_document_with_extensions initial =
  let original = state_document initial |> D.Document.json in
  let payload =
    set (get original "payload") "future-state" (`Object [ "exact", `Number "1.00" ])
  in
  let json =
    set (set original "payload" payload) "future-envelope" (`String "retained")
  in
  D.Document.inspect ~limits:document_limits json
  |> document_ok
  |> A.Session_state_document.decode ~limits:document_limits
  |> document_ok
;;

let assert_extensions (state : Persistence.Restored.t) =
  let encoded =
    Persistence.Restored.state_document state
    |> A.Session_state_document.encode ~limits:document_limits
    |> document_ok
  in
  assert (
    D.Json.equal (get (D.Document.json encoded) "future-envelope") (`String "retained"));
  assert (
    D.Json.equal
      (get (D.Document.payload encoded) "future-state")
      (`Object [ "exact", `Number "1.00" ]));
  assert (
    List.mem
      (D.Document.required_semantics encoded)
      P.Session_storage_semantics.recovery_notes
      ~equal:String.equal);
  assert (
    Result.is_error
      (A.Session_state_document.decode
         ~limits:document_limits
         (without_note_semantics encoded)))
;;

let journal ?(max_payload_length = 1048576) create env directory =
  create
    ~env
    ~directory
    ~max_payload_length
    ~max_segment_bytes:4194304L
    ~max_segment_frames:16
  |> store_ok
;;

let with_actor
      ?(owner = owner)
      ?(now = fun () -> timestamp)
      env
      sw
      journal
      restored
      ~latest_sequence
      ~previous_transaction_hash
      ~snapshot_payload_limit
      ~before_commit
      ~state_committed
      f
  =
  let session_id = P.Session_ref.session_id owner in
  let writer =
    Store.Commit_writer.create
      ~sw
      ~journal
      ~session_id
      ~next_transaction_sequence:(Int64.succ latest_sequence)
      ~previous_transaction_hash
      ~queue_capacity:8
    |> store_ok
  in
  Exn.protect
    ~finally:(fun () -> Store.Commit_writer.close writer)
    ~f:(fun () ->
      let persistence =
        Persistence.create
          ~archive:(fun _ _ -> failwith "unexpected archive")
          ~pending_archive:None
          ~before_commit
          ~command_accepted:(fun _ _ -> ())
          ~writer
          ~durability:Flush
          ~limits:document_limits
          ~archive_limits:document_limits
          ~retention_preflight:None
          ~restored
          ~previous_transaction_hash
      in
      let backend, canonical_persistence =
        Persistence.actor_persistence_with_startup_disposal
          persistence
          ~recovery_notes:(owner, snapshot_payload_limit)
      in
      let actor =
        Actor.create_with_bound_runtime_source_owner
          ~canonical_persistence
          ~interactive_permission_responder:Use_writable_attachment
          ~runtime_source_owner:
            (A.Runtime_source_owner.create () |> A.Runtime_source_owner.identity)
          ~ordinary_execution_admission:(fun () -> Ok ())
          ~schedule_permission_timeouts:false
          ~sw
          ~clock:(Eio.Stdenv.clock env)
          ~mailbox_capacity:16
          ~owner_lease_duration_ms:30000
          ~max_attachments:8
          ~subscriber_capacity:16
          ~compaction_env:None
          ~initial_state:(Persistence.Restored.state restored)
          ~persistence:backend
          ~operation_worker:None
          ~services:
            { now
            ; monotonic_now = (fun () -> Mtime.min_stamp)
            ; create_attachment_id = P.Id.Attachment.create
            ; create_reclaim_token = (fun () -> "recovery-note-reclaim")
            ; state_committed
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

let attach actor mode =
  let attachment, _, _, _ =
    Actor.attach_with_snapshot
      actor
      ~principal_id:(Some principal_id)
      ~reclaim_token:None
      ~mode
      ~subscribe:false
    |> protocol_ok
  in
  attachment.P.Session.Attachment.id
;;

let count journal = List.length (Store.Journal.scan journal |> store_ok).entries
let append selection text = C.Append { expected = selection; content = content text }

let mutate actor access name operation =
  Actor.mutate_recovery_note_operator actor ~access ~key:(key name) operation
;;

let read actor access selection =
  Actor.read_recovery_note actor ~access selection |> protocol_ok |> C.Content.to_string
;;

let%expect_test
    "private note commits replay through a reopened actor after the client loses its \
     reply"
  =
  List.iter [ false; true ] ~f:(fun checkpoint ->
    with_actor_workspace (fun env workspace_instance ->
      let root = workspace_instance.canonical_root.native_path in
      let directory = Filename.concat root "note-journal" in
      let snapshots = Filename.concat root "note-snapshots" in
      Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / snapshots);
      let initial =
        actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
      in
      let initial =
        state_document_with_extensions initial |> Persistence.Restored.of_state_document
      in
      let actor_proof = proof [ Send_messages; View_session_transcript ] in
      let original = "unique sentinel: protected note" in
      let lost_append, selection =
        Eio.Switch.run (fun sw ->
          let journal = journal Store.Journal.create env directory in
          let blocked = ref None in
          let state_committed _ events =
            match !blocked with
            | None -> ()
            | Some (entered, release) ->
              blocked := None;
              assert (List.is_empty events);
              Eio.Promise.resolve entered ();
              Eio.Promise.await release
          in
          with_actor
            env
            sw
            journal
            initial
            ~latest_sequence:0L
            ~previous_transaction_hash:None
            ~snapshot_payload_limit:1048576
            ~before_commit:None
            ~state_committed
            (fun actor durable ->
               let writer =
                 access actor_proof ~attachment_id:(attach actor Read_write) ()
               in
               let reader = access actor_proof () in
               let first =
                 mutate actor writer "create" (create original) |> protocol_ok
               in
               assert (String.equal (read actor reader first.selection) original);
               let read_only =
                 access actor_proof ~attachment_id:(attach actor Read_only) ()
               in
               expect_error
                 Permission_denied
                 (mutate actor read_only "create" (create original));
               expect_error
                 Permission_denied
                 (Actor.mutate_recovery_note_native
                    actor
                    ~access:writer
                    (create original));
               let wrong_generation =
                 Access.operator
                   actor_proof
                   ~owner
                   ~generation:1
                   ~attachment_id:None
                   ~authorize:allow
               in
               expect_error
                 Permission_denied
                 (Actor.read_recovery_note actor ~access:wrong_generation first.selection);
               let no_view = access (proof [ Send_messages ]) () in
               expect_error
                 Permission_denied
                 (Actor.list_recovery_notes actor ~access:no_view);
               let lost_append = append first.selection " β" in
               let entered, entered_u = Eio.Promise.create () in
               let release, release_u = Eio.Promise.create () in
               let cancellation, cancellation_u = Eio.Promise.create () in
               blocked := Some (entered_u, release);
               let abandoned =
                 Eio.Fiber.fork_promise ~sw (fun () ->
                   try
                     Eio.Cancel.sub (fun cancel ->
                       Eio.Promise.resolve cancellation_u cancel;
                       ignore
                         (mutate actor writer "lost" lost_append
                          : (C.Receipt.t, P.Error.t) result);
                       false)
                   with
                   | Eio.Cancel.Cancelled Exit -> true)
               in
               let competing =
                 Exn.protect
                   ~finally:(fun () -> Eio.Promise.resolve release_u ())
                   ~f:(fun () ->
                     Eio.Promise.await entered;
                     let started, started_u = Eio.Promise.create () in
                     let competing =
                       Eio.Fiber.fork_promise ~sw (fun () ->
                         Eio.Promise.resolve started_u ();
                         mutate
                           actor
                           writer
                           "losing-competitor"
                           (append first.selection "other"))
                     in
                     Eio.Promise.await started;
                     Eio.Cancel.cancel (Eio.Promise.await cancellation) Exit;
                     assert (Eio.Promise.await_exn abandoned);
                     competing)
               in
               expect_error Conflict (Eio.Promise.await_exn competing);
               let _, notes =
                 Actor.list_recovery_notes actor ~access:reader |> protocol_ok
               in
               let selection = (List.hd_exn notes).selection in
               assert (String.equal (read actor reader selection) (original ^ " β"));
               let public =
                 Actor.snapshot actor
                 |> protocol_ok
                 |> P.Snapshot.to_json
                 |> Jsonaf.to_string
               in
               assert (not (String.is_substring public ~substring:original));
               assert (not (String.is_substring public ~substring:"recovery/decisions"));
               let private_state = Actor.state actor |> protocol_ok in
               let diagnostic =
                 A.Session_state.sexp_of_t private_state |> Sexp.to_string_hum
               in
               assert (not (String.is_substring diagnostic ~substring:original));
               assert (
                 Result.is_error
                   (A.Session_delta.apply
                      private_state
                      (Created { private_state with recovery_notes = None })));
               let restored = Persistence.restored durable in
               assert_extensions restored;
               let archive =
                 A.Compaction_archive.archive_document
                   (Persistence.Restored.state_document restored)
                   ~limits:document_limits
                 |> document_ok
                 |> D.Document.to_string
               in
               assert (not (String.is_substring archive ~substring:original));
               assert (not (String.is_substring archive ~substring:"recovery/decisions"));
               if checkpoint
               then
                 ignore
                   (Persistence.install_snapshot_at
                      durable
                      ~env
                      ~directory:snapshots
                      ~max_payload_length:1048576
                      ~transaction_hash:(Persistence.transaction_hash durable)
                      (Persistence.Restored.state restored)
                    |> store_ok
                    : Store.Snapshot.installed);
               lost_append, selection))
      in
      Eio.Switch.run (fun sw ->
        let journal = journal Store.Journal.open_existing env directory in
        let recovered =
          Store.Recovery.load
            ~env
            ~journal
            ~snapshot_directory:snapshots
            ~max_snapshot_payload_length:1048576
            ~session_id
            ~initial
            ~restore_snapshot:(Persistence.restore_snapshot ~limits:document_limits)
            ~apply:(Persistence.apply_transaction ~limits:document_limits)
            ~validate_transaction:
              (Persistence.validate_transaction ~limits:document_limits)
            ~validate:Persistence.validate
          |> store_ok
        in
        assert (Bool.equal checkpoint (Option.is_some recovered.snapshot));
        assert_extensions recovered.state;
        with_actor
          env
          sw
          journal
          recovered.state
          ~latest_sequence:recovered.latest_transaction_sequence
          ~previous_transaction_hash:recovered.latest_transaction_hash
          ~snapshot_payload_limit:1048576
          ~before_commit:None
          ~state_committed:(fun _ _ -> ())
          (fun actor _ ->
             let reopened_proof = proof [ Send_messages; View_session_transcript ] in
             let writer =
               access reopened_proof ~attachment_id:(attach actor Read_write) ()
             in
             let reader = access reopened_proof () in
             let initial_count = count journal in
             let receipt = mutate actor writer "lost" lost_append |> protocol_ok in
             assert (C.Receipt.equal_kind receipt.kind Appended);
             assert (C.equal_selection receipt.selection selection);
             assert (String.equal (read actor reader receipt.selection) (original ^ " β"));
             expect_error
               Idempotency_conflict
               (mutate actor writer "lost" (append receipt.selection "another body"));
             assert (Int.equal (count journal) initial_count);
             printf "checkpoint=%b, cancelled reply recovered exactly once\n" checkpoint))));
  [%expect
    {|
    checkpoint=false, cancelled reply recovered exactly once
    checkpoint=true, cancelled reply recovered exactly once
    |}]
;;

let%expect_test
    "snapshot capacity, journal capacity and final authority reject a note atomically"
  =
  List.iter
    [ "snapshot", 1048576, 16384; "journal", 8192, 1048576 ]
    ~f:(fun (profile, max_payload_length, snapshot_payload_limit) ->
      with_actor_workspace (fun env workspace_instance ->
        Eio.Switch.run (fun sw ->
          let root = workspace_instance.canonical_root.native_path in
          let journal =
            journal
              ~max_payload_length
              Store.Journal.create
              env
              (Filename.concat root "note-bounded-journal")
          in
          let initial =
            actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
          in
          let active = ref true in
          let invalidate = ref false in
          let before_commit (candidate : A.Session_state.t) =
            if !invalidate && Option.is_some candidate.recovery_notes then active := false;
            Ok ()
          in
          let guarded =
            Operator_authorization.guarded
              ~principal:(principal [ Send_messages; View_session_transcript ])
              ~is_current:(fun () -> !active)
          in
          with_actor
            env
            sw
            journal
            (Persistence.Restored.authored initial)
            ~latest_sequence:0L
            ~previous_transaction_hash:None
            ~snapshot_payload_limit
            ~before_commit:(Some before_commit)
            ~state_committed:(fun _ _ -> ())
            (fun actor _ ->
               let attachment_id = attach actor Read_write in
               let writer = access guarded ~attachment_id () in
               let before = count journal in
               expect_error
                 Persistence_error
                 (mutate actor writer "same" (create (String.make 20000 'x')));
               assert (Int.equal before (count journal));
               invalidate := true;
               expect_error
                 Permission_denied
                 (mutate actor writer "same" (create "private"));
               assert (Int.equal before (count journal));
               assert (Option.is_none (Actor.state actor |> protocol_ok).recovery_notes);
               invalidate := false;
               let writer =
                 access
                   (proof [ Send_messages; View_session_transcript ])
                   ~attachment_id
                   ()
               in
               ignore
                 (mutate actor writer "same" (create "private") |> protocol_ok
                  : C.Receipt.t);
               assert (Int.equal (before + 1) (count journal));
               let transaction =
                 (Store.Journal.scan journal |> store_ok).entries
                 |> List.last_exn
                 |> fun (entry : Store.Journal.entry) ->
                 Store.Document_record.of_frame
                   entry.frame
                   ~limits:document_limits
                   ~expected_digest:None
                 |> Result.map_error ~f:(fun error ->
                   Sexp.to_string_hum (Store.Document_record.Error.sexp_of_t error))
                 |> Result.ok_or_failwith
                 |> Store.Transaction.decode_record ~limits:document_limits
                 |> store_ok
               in
               assert (
                 List.mem
                   (D.Document.required_semantics transaction.delta)
                   P.Session_storage_semantics.recovery_notes
                   ~equal:String.equal);
               assert (
                 Result.is_error
                   (A.Session_delta_document.decode
                      ~limits:document_limits
                      (without_note_semantics transaction.delta)));
               printf
                 "%s profile: oversized and revoked writes left no journal entry or \
                  receipt\n"
                 profile))));
  [%expect
    {|
    snapshot profile: oversized and revoked writes left no journal entry or receipt
    journal profile: oversized and revoked writes left no journal entry or receipt
    |}]
;;
