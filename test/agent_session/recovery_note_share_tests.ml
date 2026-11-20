open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module Store = Agent_store
module D = Document_schema
module C = Recovery_notes.Catalog
module S = A.Recovery_note_share
module Actor = A.Session_actor
module Shares = Actor.Recovery_note_shares
module R = Recovery_note_actor_tests
module Persistence = A.Session_persistence

let owner = R.owner

let session name =
  P.Session_ref.create
    ~server_id:(P.Session_ref.server_id owner)
    ~session_id:(P.Id.Session.of_string name |> protocol_ok)
;;

let recipient = session "ses_recovery_receiver"
let other = session "ses_recovery_other"
let check_owner () = Ok ()
let later time delay_ms = P.Timestamp.add_ms time delay_ms |> protocol_ok
let key = R.key
let private_text = "β text of an independently authorized pinned note"

let unavailable result =
  match result with
  | Error error ->
    assert (D.Json.equal (P.Error.to_json error) (P.Error.to_json (S.unavailable ())))
  | Ok _ -> failwith "private share was disclosed"
;;

let access ?attachment_id owner actor =
  A.Recovery_note_access.operator
    actor
    ~owner
    ~generation:0
    ~attachment_id
    ~authorize:(fun _ -> Ok ())
;;

let owner_proof () = R.proof [ Manage_grants; Send_messages; View_session_transcript ]
let reader_proof () = R.proof [ View_session_transcript ]

let create_note source writer =
  Actor.mutate_recovery_note_operator
    source
    ~access:writer
    ~key:(key "create-source-note")
    (R.create private_text)
  |> protocol_ok
;;

let request ~expected_revision ~expires_at (note : C.Receipt.t) =
  S.
    { expected_revision
    ; expected_catalog_revision = note.catalog_revision
    ; selection = note.selection
    ; expires_at
    }
;;

let read_target actor owner actor_proof =
  Shares.reader actor ~access:(access owner actor_proof) ~check_owner |> protocol_ok
;;

let select source reader receipt =
  Shares.select source ~check_owner ~reader (S.Receipt.reference receipt)
;;

let grant source manager target name ~expires_at note =
  let expected_revision =
    Shares.management_revision source ~access:manager ~check_owner |> protocol_ok
  in
  Shares.create
    source
    ~access:manager
    ~check_owner
    ~target
    ~key:(key name)
    (request ~expected_revision ~expires_at note)
  |> protocol_ok
;;

let source_state workspace_instance owner =
  let initial =
    actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
  in
  let identity = { initial.identity with session_id = P.Session_ref.session_id owner } in
  A.Session_state.create ~identity ~spec:initial.spec ~initial_history:[]
;;

let with_writer env sw journal restored ~owner ~before_commit ~now f =
  R.with_actor
    ~owner
    ~now
    env
    sw
    journal
    restored
    ~latest_sequence:0L
    ~previous_transaction_hash:None
    ~snapshot_payload_limit:1048576
    ~before_commit
    ~state_committed:(fun _ _ -> ())
    f
;;

let new_session env sw workspace owner ~name ?before_commit ?(now = fun () -> timestamp) f
  =
  let directory =
    Filename.concat workspace.A.Workspace_instance.canonical_root.native_path name
  in
  let journal = R.journal Store.Journal.create env directory in
  let restored = Persistence.Restored.authored (source_state workspace owner) in
  with_writer env sw journal restored ~owner ~before_commit ~now (fun actor durable ->
    f actor durable journal)
;;

let reopen_session env sw workspace owner ~name f =
  let root = workspace.A.Workspace_instance.canonical_root.native_path in
  let directory = Filename.concat root name in
  let snapshots = Filename.concat root (name ^ "-snapshots") in
  Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / snapshots);
  let journal = R.journal Store.Journal.open_existing env directory in
  let session_id = P.Session_ref.session_id owner in
  let initial = Persistence.Restored.authored (source_state workspace owner) in
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
      ~validate_transaction:(Persistence.validate_transaction ~limits:document_limits)
      ~validate:Persistence.validate
    |> store_ok
  in
  R.with_actor
    ~owner
    env
    sw
    journal
    recovered.state
    ~latest_sequence:recovered.latest_transaction_sequence
    ~previous_transaction_hash:recovered.latest_transaction_hash
    ~snapshot_payload_limit:1048576
    ~before_commit:None
    ~state_committed:(fun _ _ -> ())
    f
;;

let%expect_test "bounded grant CAS and body-free receipts survive a codec round trip" =
  let nonce = P.Id.Session_incarnation.create () in
  let ledger = S.Journal.establish ~owner ~nonce |> protocol_ok in
  let source = S.incarnation ledger ~generation:0 |> protocol_ok in
  let target =
    S.Incarnation.create
      ~session:recipient
      ~nonce:(P.Id.Session_incarnation.create ())
      ~generation:0
    |> protocol_ok
  in
  let catalog, note =
    let catalog = C.empty ~owner |> protocol_ok in
    let identity =
      C.Mutation_identity.Client { principal = principal_id; key = key "note" }
    in
    match C.apply catalog ~identity (R.create private_text) with
    | Ok (Applied (catalog, note)) -> Some catalog, note
    | Ok (Replayed _) | Error _ -> failwith "could not write source note"
  in
  let apply ledger ~name ~now command =
    S.Journal.mutate
      ledger
      ~source
      ~catalog
      ~principal:principal_id
      ~key:(key name)
      ~now
      command
  in
  let create ledger ?(now = timestamp) name =
    let expires_at = later now 1000 in
    let command =
      S.Journal.Create
        (target, request ~expected_revision:(S.revision ledger) ~expires_at note)
    in
    apply ledger ~name ~now command, command
  in
  let applied = function
    | Ok (`Applied result) -> result
    | Ok (`Replayed _) | Error _ -> failwith "expected new grant write"
  in
  let first, initial_request = create ledger "first" in
  let ledger, first = applied first in
  (match apply ledger ~name:"first" ~now:timestamp initial_request with
   | Ok (`Replayed replay) ->
     assert (S.Reference.equal (S.Receipt.reference first) (S.Receipt.reference replay))
   | _ -> failwith "valid retry did not replay");
  let ledger, second = fst (create ledger "second") |> applied in
  let ledger =
    List.fold (List.range 2 128) ~init:ledger ~f:(fun ledger index ->
      fst (create ledger (Int.to_string index)) |> applied |> fst)
  in
  R.expect_error Resource_limit (fst (create ledger "overflow"));
  let reference = S.Receipt.reference first in
  let ledger, _ =
    S.Journal.Revoke { expected_revision = S.revision ledger; reference }
    |> apply ledger ~name:"revoke" ~now:timestamp
    |> applied
  in
  R.expect_error Conflict (apply ledger ~name:"first" ~now:timestamp initial_request);
  unavailable
    (S.resolve ledger ~source_generation:0 ~recipient:target ~reference ~now:timestamp);
  let ledger, fresh = fst (create ledger "fresh") |> applied in
  assert (not (S.Reference.equal reference (S.Receipt.reference fresh)));
  let json = S.to_json ledger |> protocol_ok in
  assert (not (String.is_substring (Jsonaf.to_string json) ~substring:private_text));
  let payload = R.get json "payload" in
  let grants =
    match R.get payload "grants" with
    | `Array (entry :: rest) -> `Array (R.set entry "future" (`Number "1.00") :: rest)
    | _ -> failwith "missing live grants"
  in
  let json = R.set json "payload" (R.set payload "grants" grants) in
  let ledger =
    S.of_json json ~session_id:(P.Session_ref.session_id owner) |> protocol_ok
  in
  let extra = fst (create ledger ~now:(later timestamp 500) "too-many") in
  R.expect_error Resource_limit extra;
  let ledger, _ =
    S.Journal.Revoke
      { expected_revision = S.revision ledger; reference = S.Receipt.reference second }
    |> apply ledger ~name:"revoke-second" ~now:timestamp
    |> applied
  in
  let fresh_reference = S.Receipt.reference fresh in
  let other_reference = S.Reference.to_json fresh_reference in
  unavailable
    (S.resolve
       ledger
       ~source_generation:1
       ~recipient:target
       ~reference:fresh_reference
       ~now:timestamp);
  let future = R.get (R.get (S.to_json ledger |> protocol_ok) "payload") "grants" in
  assert (String.is_substring (Jsonaf.to_string future) ~substring:"1.00");
  assert (
    not
      (String.is_substring
         (Jsonaf.to_string other_reference)
         ~substring:"recovery/decisions"));
  let expired = later timestamp 1000 in
  let _, unexpired = fst (create ledger ~now:expired "after-expiry") |> applied in
  assert (
    not (S.Reference.equal (S.Receipt.reference fresh) (S.Receipt.reference unexpired)));
  print_endline "128 active, bounded replay, no ID reuse, expired slots reclaimed";
  [%expect {| 128 active, bounded replay, no ID reuse, expired slots reclaimed |}]
;;

let%expect_test "two physical actors recheck share authority at the disclosure cutpoint" =
  with_actor_workspace (fun env workspace ->
    Eio.Switch.run (fun sw ->
      let now = ref timestamp in
      let gate = ref None in
      let before_commit (_ : A.Session_state.t) =
        match !gate with
        | None -> Ok ()
        | Some (entered, release) ->
          gate := None;
          Eio.Promise.resolve entered ();
          Eio.Promise.await release;
          Ok ()
      in
      new_session
        env
        sw
        workspace
        owner
        ~name:"source-cutpoint"
        ~before_commit
        ~now:(fun () -> !now)
        (fun source durable source_journal ->
           new_session
             env
             sw
             workspace
             recipient
             ~name:"target-cutpoint"
             (fun target _ _ ->
                let attachment_id = R.attach source Owner_read_write in
                let manager = access owner (owner_proof ()) ~attachment_id in
                let note = create_note source manager in
                let unrelated =
                  let principal =
                    R.principal [ Manage_grants; Send_messages; View_session_transcript ]
                  in
                  { principal with
                    id = P.Id.Principal.of_string "pri_unrelated_writer" |> protocol_ok
                  }
                  |> Operator_authorization.trusted_local
                in
                R.expect_error
                  Permission_denied
                  (Shares.management_revision
                     source
                     ~access:(access owner unrelated ~attachment_id)
                     ~check_owner);
                let no_manage = access owner (R.proof [ Send_messages ]) ~attachment_id in
                R.expect_error
                  Permission_denied
                  (Shares.management_revision source ~access:no_manage ~check_owner);
                let normal_attachment = R.attach source Read_write in
                let normal_writer =
                  access owner (owner_proof ()) ~attachment_id:normal_attachment
                in
                R.expect_error
                  Permission_denied
                  (Shares.management_revision source ~access:normal_writer ~check_owner);
                unavailable
                  (Shares.reader
                     target
                     ~access:(access recipient (reader_proof ()))
                     ~check_owner);
                let target_witness =
                  Shares.establish_target target ~check_owner |> protocol_ok
                in
                assert (Option.is_none (Actor.state target |> protocol_ok).recovery_notes);
                let live = Atomic.make true in
                let original =
                  Operator_authorization.guarded
                    ~principal:(R.principal [ View_session_transcript ])
                    ~is_current:(fun () -> Atomic.get live)
                in
                let reader = read_target target recipient original in
                unavailable
                  (Shares.reader target ~access:(access owner original) ~check_owner);
                unavailable
                  (Shares.reader
                     target
                     ~access:(access recipient (R.proof [ Send_messages ]))
                     ~check_owner);
                let expires_at = later timestamp 5000 in
                let first_request =
                  request ~expected_revision:S.Revision.initial ~expires_at note
                in
                let create ~expires_at request =
                  Shares.create
                    source
                    ~access:manager
                    ~check_owner
                    ~target:target_witness
                    ~key:(key "first-share")
                    { request with S.expires_at }
                in
                let target_current = ref true in
                let provisional_target =
                  Shares.establish_target target ~check_owner:(fun () ->
                    if !target_current then Ok () else Error (S.unavailable ()))
                  |> protocol_ok
                in
                let before = R.count source_journal in
                let entered, entered_u = Eio.Promise.create () in
                let release, release_u = Eio.Promise.create () in
                gate := Some (entered_u, release);
                let rejected =
                  Eio.Fiber.fork_promise ~sw (fun () ->
                    Shares.create
                      source
                      ~access:manager
                      ~check_owner
                      ~target:provisional_target
                      ~key:(key "first-share")
                      first_request)
                in
                Exn.protect
                  ~finally:(fun () -> Eio.Promise.resolve release_u ())
                  ~f:(fun () ->
                    Eio.Promise.await entered;
                    target_current := false);
                unavailable (Eio.Promise.await_exn rejected);
                assert (Int.equal before (R.count source_journal));
                assert (
                  Option.is_none (Actor.state source |> protocol_ok).recovery_note_shares);
                let first = create ~expires_at first_request |> protocol_ok in
                let source_count = R.count source_journal in
                let replay = create ~expires_at first_request |> protocol_ok in
                assert (
                  S.Reference.equal
                    (S.Receipt.reference first)
                    (S.Receipt.reference replay));
                R.expect_error
                  Idempotency_conflict
                  (create ~expires_at:(later timestamp 4999) first_request);
                assert (Int.equal source_count (R.count source_journal));
                new_session
                  env
                  sw
                  workspace
                  other
                  ~name:"other-cutpoint"
                  (fun unrelated _ _ ->
                     ignore
                       (Shares.establish_target unrelated ~check_owner |> protocol_ok
                        : Shares.target);
                     let unrelated_reader =
                       read_target unrelated other (reader_proof ())
                     in
                     unavailable (select source unrelated_reader first));
                let hit = select source reader first |> protocol_ok in
                assert (
                  String.equal
                    private_text
                    (Shares.materialize hit |> protocol_ok |> C.Content.to_string));
                Atomic.set live false;
                unavailable (Shares.materialize hit);
                let reader = read_target target recipient (reader_proof ()) in
                let hit = select source reader first |> protocol_ok in
                let entered, entered_u = Eio.Promise.create () in
                let release, release_u = Eio.Promise.create () in
                gate := Some (entered_u, release);
                let expected_revision = S.Receipt.revision first in
                let reference = S.Receipt.reference first in
                let revocation =
                  Eio.Fiber.fork_promise ~sw (fun () ->
                    Shares.revoke
                      source
                      ~access:manager
                      ~check_owner
                      ~key:(key "revoke-first")
                      S.{ expected_revision; reference })
                in
                Exn.protect
                  ~finally:(fun () -> Eio.Promise.resolve release_u ())
                  ~f:(fun () ->
                    Eio.Promise.await entered;
                    unavailable (Shares.materialize hit));
                ignore (Eio.Promise.await_exn revocation |> protocol_ok : S.Receipt.t);
                unavailable (Shares.materialize hit);
                unavailable (select source reader first);
                let second =
                  grant source manager target_witness "second-share" ~expires_at note
                in
                let hit = select source reader second |> protocol_ok in
                let replacement =
                  Actor.mutate_recovery_note_operator
                    source
                    ~access:manager
                    ~key:(key "replace-note")
                    (C.Replace
                       { expected = note.selection
                       ; content = R.content "new private revision"
                       })
                  |> protocol_ok
                in
                unavailable (Shares.materialize hit);
                unavailable (select source reader second);
                let third =
                  grant
                    source
                    manager
                    target_witness
                    "third-share"
                    ~expires_at
                    replacement
                in
                let hit = select source reader third |> protocol_ok in
                now := expires_at;
                unavailable (Shares.materialize hit);
                unavailable (select source reader third);
                let state = Actor.state source |> protocol_ok in
                assert (
                  Result.is_error
                    (A.Session_delta.apply
                       state
                       (Created { state with recovery_note_shares = None })));
                let document =
                  Persistence.restored durable
                  |> Persistence.Restored.state_document
                  |> A.Session_state_document.encode ~limits:document_limits
                  |> document_ok
                in
                let semantics = D.Document.required_semantics document in
                let marker = P.Session_storage_semantics.recovery_note_shares in
                assert (List.mem semantics marker ~equal:String.equal);
                let without_marker =
                  List.filter semantics ~f:(fun name -> not (String.equal name marker))
                  |> List.map ~f:(fun name -> `String name)
                  |> fun names ->
                  R.set (D.Document.json document) "required_semantics" (`Array names)
                  |> D.Document.inspect ~limits:document_limits
                  |> document_ok
                in
                assert (
                  Result.is_error
                    (A.Session_state_document.decode
                       ~limits:document_limits
                       without_marker));
                let archived =
                  A.Compaction_archive.archive_document
                    (Persistence.Restored.state_document (Persistence.restored durable))
                    ~limits:document_limits
                  |> document_ok
                  |> D.Document.to_string
                in
                assert (
                  not (String.is_substring archived ~substring:"recovery_note_shares"));
                let public =
                  Actor.snapshot source
                  |> protocol_ok
                  |> P.Snapshot.to_json
                  |> Jsonaf.to_string
                in
                assert (not (String.is_substring public ~substring:"recovery_note_shares"))))));
  print_endline
    "source admission, uniform denials, lost ACK replay, revoke at writer, edit and \
     expiry";
  [%expect
    {| source admission, uniform denials, lost ACK replay, revoke at writer, edit and expiry |}]
;;

let%expect_test
    "grants resume after journal reopen but deny same-coordinate replacement owners"
  =
  with_actor_workspace (fun env workspace ->
    let receipt, disclosure =
      Eio.Switch.run (fun sw ->
        new_session env sw workspace owner ~name:"source-restart" (fun source _ _ ->
          new_session env sw workspace recipient ~name:"target-restart" (fun target _ _ ->
            let manager =
              access
                owner
                (owner_proof ())
                ~attachment_id:(R.attach source Owner_read_write)
            in
            let note = create_note source manager in
            let target_proof =
              Shares.establish_target target ~check_owner |> protocol_ok
            in
            let receipt =
              grant
                source
                manager
                target_proof
                "restart-share"
                ~expires_at:(later timestamp 10000)
                note
            in
            let reader = read_target target recipient (reader_proof ()) in
            receipt, select source reader receipt |> protocol_ok)))
    in
    unavailable (Shares.materialize disclosure);
    Eio.Switch.run (fun sw ->
      reopen_session env sw workspace owner ~name:"source-restart" (fun source _ ->
        reopen_session env sw workspace recipient ~name:"target-restart" (fun target _ ->
          let reader = read_target target recipient (reader_proof ()) in
          let hit = select source reader receipt |> protocol_ok in
          assert (
            String.equal
              private_text
              (Shares.materialize hit |> protocol_ok |> C.Content.to_string));
          new_session
            env
            sw
            workspace
            recipient
            ~name:"target-recreated"
            (fun recreated _ _ ->
               ignore
                 (Shares.establish_target recreated ~check_owner |> protocol_ok
                  : Shares.target);
               let reader = read_target recreated recipient (reader_proof ()) in
               unavailable (select source reader receipt));
          new_session
            env
            sw
            workspace
            owner
            ~name:"source-recreated"
            (fun recreated _ _ ->
               let manager =
                 access
                   owner
                   (owner_proof ())
                   ~attachment_id:(R.attach recreated Owner_read_write)
               in
               let note = create_note recreated manager in
               let target_witness =
                 Shares.establish_target target ~check_owner |> protocol_ok
               in
               let replacement =
                 grant
                   recreated
                   manager
                   target_witness
                   "restart-share"
                   ~expires_at:(later timestamp 10000)
                   note
               in
               assert (
                 not
                   (S.Reference.equal
                      (S.Receipt.reference receipt)
                      (S.Receipt.reference replacement)));
               unavailable (select recreated reader receipt))))));
  print_endline
    "real journal reopen works; same session ID, timestamp and generation cannot retarget";
  [%expect
    {| real journal reopen works; same session ID, timestamp and generation cannot retarget |}]
;;

let%expect_test
    "protected delivery needs the live physical operation and original read scope"
  =
  with_actor_workspace (fun env workspace ->
    Eio.Switch.run (fun sw ->
      new_session env sw workspace owner ~name:"operation-source" (fun source _ _ ->
        new_session
          env
          sw
          workspace
          recipient
          ~name:"operation-recipient"
          (fun target _ _ ->
             let manager =
               access
                 owner
                 (owner_proof ())
                 ~attachment_id:(R.attach source Owner_read_write)
             in
             let note = create_note source manager in
             let target_witness =
               Shares.establish_target target ~check_owner |> protocol_ok
             in
             let receipt =
               grant
                 source
                 manager
                 target_witness
                 "operation-share"
                 ~expires_at:(later timestamp 10000)
                 note
             in
             let attachment_id = R.attach target Read_write in
             Actor.start target ~attachment_id |> protocol_ok |> ignore;
             let with_operation scopes f =
               let started, started_u = Eio.Promise.create () in
               let release, release_u = Eio.Promise.create () in
               let worker = completed_worker ~started:started_u ~release in
               Actor.set_operation_worker target (Some worker) |> protocol_ok;
               let reservation =
                 Actor.reserve_history_block target ~count:1 |> protocol_ok
               in
               let user_entry =
                 Pending_input_tests.occurrence
                   (Int64.to_int_exn reservation.first_sequence)
                   "recovery"
               in
               let submission =
                 Actor.submit_message_authenticated
                   target
                   ~actor:(R.proof scopes)
                   ~attachment_id
                   user_entry
                 |> protocol_ok
               in
               let operation_id = Option.value_exn submission.operation_id in
               let result =
                 Exn.protect
                   ~finally:(fun () -> Eio.Promise.resolve release_u ())
                   ~f:(fun () ->
                     Agent_authority_tests.await_worker_checkpoint target started;
                     f operation_id)
               in
               await_idle target |> ignore;
               operation_id, result
             in
             let first, hit =
               with_operation
                 [ Send_messages; View_session_transcript ]
                 (fun operation_id ->
                    let reader =
                      Shares.reader_for_delivery target ~operation_id ~check_owner
                      |> protocol_ok
                    in
                    let hit = select source reader receipt |> protocol_ok in
                    assert (
                      String.equal
                        private_text
                        (Shares.materialize hit |> protocol_ok |> C.Content.to_string));
                    hit)
             in
             unavailable (Shares.materialize hit);
             unavailable
               (Shares.reader_for_delivery target ~operation_id:first ~check_owner);
             ignore
               (with_operation [ Send_messages ] (fun operation_id ->
                  unavailable
                    (Shares.reader_for_delivery target ~operation_id ~check_owner);
                  unavailable
                    (Shares.reader_for_delivery target ~operation_id:first ~check_owner))
                : P.Id.Operation.t * unit)))));
  print_endline
    "current scoped operation succeeds; ended, replaced and missing scope deny";
  [%expect
    {| current scoped operation succeeds; ended, replaced and missing scope deny |}]
;;

let%expect_test "same-Store Factory retains source ownership through bounded disclosure" =
  let module T = Agent_server_test_support in
  let module Host = Agent_server in
  let module Factory = Host.Session_factory.Recovery_note_shares in
  with_actor_workspace (fun env workspace ->
    Eio.Switch.run (fun sw ->
      let root = workspace.A.Workspace_instance.canonical_root.native_path in
      let prompt = Filename.concat root "note-share.chatmd" in
      Eio.Path.save
        ~create:(`Exclusive 0o600)
        Eio.Path.(Eio.Stdenv.fs env / prompt)
        "<developer>Private recovery fixture.</developer>";
      let inference_policy =
        T.inference_policy
          ~default_model:"fixture-model"
          ~post_stream:(fun ~sw:_ ~inputs:_ -> failwith "share lookup started a provider")
      in
      let daemon =
        Host.Daemon.start
          ~sw
          ~env
          ~config:(T.config root root prompt)
          ~tool_dir:root
          ~home:root
          ~process_start_identity:None
          ~options:
            { Host.Daemon.default_options with
              inference_policy
            ; qualify_chatml_extensions =
                false
                (* This Store/Registry fixture installs no authored tools or runtime. *)
            }
          ()
        |> protocol_ok
      in
      Exn.protect
        ~finally:(fun () -> Host.Daemon.shutdown daemon |> protocol_ok)
        ~f:(fun () ->
          let principal = T.principal () in
          let operator = Operator_authorization.trusted_local principal in
          let connection = T.connection daemon principal in
          Exn.protect
            ~finally:(fun () -> Agent_client.Connection.close connection)
            ~f:(fun () ->
              T.initialize connection;
              let source, _ = T.create_session ~key:"factory-note-source" connection in
              let recipient, _ =
                T.create_session ~key:"factory-note-recipient" connection
              in
              let registry = Host.Daemon.registry daemon in
              let entry id = Host.Session_registry.find registry id |> Option.value_exn in
              let source_entry = entry source.id in
              let recipient_entry = entry recipient.id in
              let server_id = Store.Session_store.server_id (Host.Daemon.store daemon) in
              let source_ref = P.Session_ref.create ~server_id ~session_id:source.id in
              let recipient_ref =
                P.Session_ref.create ~server_id ~session_id:recipient.id
              in
              let attachment, _, _, _ =
                Actor.attach_with_snapshot
                  source_entry.actor
                  ~principal_id:(Some principal.id)
                  ~reclaim_token:None
                  ~mode:Owner_read_write
                  ~subscribe:false
                |> protocol_ok
              in
              let manager = access source_ref operator ~attachment_id:attachment.id in
              let note = create_note source_entry.actor manager in
              let factory = Host.Daemon.factory daemon in
              let expected_revision =
                Factory.management_revision factory ~source:source_entry ~access:manager
                |> protocol_ok
              in
              let expires_at = later (P.Timestamp.now ()) 60000 in
              let receipt =
                Factory.create
                  factory
                  ~source:source_entry
                  ~access:manager
                  ~recipient:recipient_ref
                  ~key:(key "factory-note-share")
                  (request ~expected_revision ~expires_at note)
                |> protocol_ok
              in
              let reader = access recipient_ref operator in
              let reference = S.Receipt.reference receipt in
              let escaped =
                Factory.with_disclosure
                  factory
                  ~recipient:recipient_entry
                  ~access:reader
                  reference
                  ~f:(fun disclosure ->
                    assert (Host.Session_registry.lifecycle_reserved registry source.id);
                    let content = Shares.materialize disclosure |> protocol_ok in
                    assert (String.equal (C.Content.to_string content) private_text);
                    Ok disclosure)
                |> protocol_ok
              in
              assert (not (Host.Session_registry.lifecycle_reserved registry source.id));
              unavailable (Shares.materialize escaped);
              unavailable
                (Factory.with_disclosure
                   factory
                   ~recipient:source_entry
                   ~access:reader
                   reference
                   ~f:(fun _ -> failwith "shared to wrong host entry"));
              assert (not (Host.Runtime_owner.is_loaded source_entry.runtime));
              Actor.detach source_entry.actor attachment.id |> protocol_ok))));
  print_endline "real Store and Registry lease covers disclosure; escaped proof is closed";
  [%expect {| real Store and Registry lease covers disclosure; escaped proof is closed |}]
;;
