open! Core
open Fixtures
open Instruction_selection_tests
module State = A.Session_state
module SD = A.Session_state_document
module Persistence = A.Session_persistence
module Store = Agent_store

let selection state = Option.value_exn state.State.instruction_selection
let state_selection_json state = raw (selection state)

let prepare_state basis command =
  A.Instruction_transition.prepare
    basis
    ~request:command
    ~principal_id
    ~now:timestamp
    ~limits:document_limits
;;

let prepared = function
  | Ok (A.Instruction_transition.Prepared { transition; receipt }) -> transition, receipt
  | Ok (Retained _) -> failwith "expected prepared instruction transition"
  | Error error -> raise_s [%sexp (error : P.Error.t)]
;;

let%expect_test
    "sole carrier retains absent/null and protects unknown capture evidence from history \
     edits"
  =
  with_actor_workspace (fun _ workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let encoded = state_document initial |> D.Document.json in
    let payload = member encoded "payload" in
    let absent =
      edit_payload encoded (fun _ ->
        match payload with
        | `Object fields ->
          `Object
            (List.filter fields ~f:(fun (name, _) ->
               not (String.equal name "instruction_selection")))
        | _ -> assert false)
    in
    let basis =
      D.Document.inspect absent ~limits:document_limits
      |> document_ok
      |> SD.decode ~limits:document_limits
      |> document_ok
    in
    assert (
      match
        D.Json.field
          (D.Document.payload (SD.encode basis ~limits:document_limits |> document_ok))
          ~name:"instruction_selection"
      with
      | Absent -> true
      | Null | Value _ -> false);
    assert (
      match D.Json.field payload ~name:"instruction_selection" with
      | Null -> true
      | Absent | Value _ -> false);
    let capture = C.skills (snapshot [ a; b ]) |> protocol_ok in
    let command = request Selection.empty "carrier" (Replace_skills capture) in
    let transition, _ = prepare_state basis command |> prepared in
    let admitted =
      A.Session_document_transition.admit
        basis
        ~delta:transition.delta
        ~next:transition.state
        ~limits:document_limits
      |> document_ok
    in
    let document =
      SD.encode admitted ~limits:document_limits |> document_ok |> D.Document.json
    in
    let document =
      let first values f =
        match values with
        | `Array (row :: rest) -> `Array (f row :: rest)
        | _ -> failwith "expected retained instruction row"
      in
      edit_payload document (fun payload ->
        let instructions = member payload "instruction_selection" in
        let instructions = set instructions "future_envelope" (`String "envelope") in
        let instructions =
          edit_payload instructions (fun payload ->
            let payload = set payload "future_retained" (`String "preserve") in
            let captures =
              first (member payload "captures") (fun row ->
                set row "future_capture" (`Object [ "retained", `True ]))
            in
            let receipts =
              first (member payload "receipts") (fun row ->
                let receipt =
                  set (member row "receipt") "future_receipt" (`String "receipt")
                in
                set (set row "future_wrapper" `True) "receipt" receipt)
            in
            set (set payload "captures" captures) "receipts" receipts)
        in
        set payload "instruction_selection" instructions)
    in
    let basis =
      D.Document.inspect document ~limits:document_limits
      |> document_ok
      |> SD.decode ~limits:document_limits
      |> document_ok
    in
    let before = SD.value basis in
    let initial_selection = state_selection_json before in
    let first_sequence = before.conversation.next_history_sequence in
    let id =
      History_entry.Id.create
        ~namespace:(P.Id.Session.to_string before.identity.session_id)
        ~sequence:(Int64.to_int_exn first_sequence)
      |> Result.ok_or_failwith
    in
    let appended =
      A.History_codec.user_text ~id "original" |> A.History_codec.to_protocol
    in
    (* The durable delta reserves the allocator block before appending its member;
       neither raw fixture counter editing nor a fabricated reservation callback. *)
    let transition =
      A.Session_transition.apply
        ~now:timestamp
        before
        ~delta:
          (Batch
             [ History_block_reserved (Int64.succ first_sequence)
             ; Canonical_entries_appended [ appended ]
             ])
        ~payloads:[]
      |> protocol_ok
    in
    let basis =
      A.Session_document_transition.admit
        basis
        ~delta:transition.delta
        ~next:transition.state
        ~limits:document_limits
      |> document_ok
    in
    let before = SD.value basis in
    let edit = History_edit_tests.intent appended "edited" in
    let archive =
      A.Compaction_archive.reference_for
        basis
        ~limits:document_limits
        ~kind:Edit
        operation_id
      |> protocol_ok
    in
    let transition =
      A.Session_transition.apply
        ~now:timestamp
        before
        ~delta:(History_edited (edit, archive))
        ~payloads:[]
      |> protocol_ok
    in
    let basis =
      A.Session_document_transition.admit
        basis
        ~delta:transition.delta
        ~next:transition.state
        ~limits:document_limits
      |> document_ok
    in
    assert (D.Json.equal initial_selection (state_selection_json (SD.value basis)));
    let compacted =
      A.Session_transition.apply
        ~now:timestamp
        (SD.value basis)
        ~delta:(Batch [ Canonical_history_replaced []; Compaction_generation_changed 1 ])
        ~payloads:[]
      |> protocol_ok
    in
    let compacted =
      A.Session_document_transition.admit
        basis
        ~delta:compacted.delta
        ~next:compacted.state
        ~limits:document_limits
      |> document_ok
    in
    assert (D.Json.equal initial_selection (state_selection_json (SD.value compacted)));
    let dropped =
      SD.with_value compacted { (SD.value compacted) with instruction_selection = None }
    in
    assert (Result.is_error (SD.encode dropped ~limits:document_limits));
    let flattened =
      Selection.of_jsonaf
        (edit_payload
           (raw (selection (SD.value compacted)))
           (fun payload ->
              match payload with
              | `Object fields ->
                `Object
                  (List.filter fields ~f:(fun (name, _) ->
                     not (String.equal name "future_retained")))
              | _ -> assert false))
      |> protocol_ok
    in
    assert (
      Result.is_error
        (SD.encode
           (SD.with_value
              compacted
              { (SD.value compacted) with instruction_selection = Some flattened })
           ~limits:document_limits));
    let cleared = apply (selection (SD.value compacted)) "clear-after-history" Clear in
    let next =
      A.Session_delta_document.create
        (Instruction_selection_changed cleared)
        ~limits:document_limits
        ~state_document:SD.authored
      |> document_ok
      |> A.Session_delta_document.document
      |> A.Session_delta_document.decode ~limits:document_limits
      |> document_ok
      |> fun delta ->
      A.Session_delta_document.apply delta ~limits:document_limits compacted
      |> document_ok
      |> SD.value
    in
    assert (List.length (Selection.receipts (selection next)) = 2);
    assert (List.is_empty (Selection.skills (selection next)));
    let current_selection = state_selection_json next in
    assert (
      D.Json.equal
        (member current_selection "future_envelope")
        (member initial_selection "future_envelope"));
    let old_payload = member initial_selection "payload"
    and current_payload = member current_selection "payload" in
    assert (
      D.Json.equal (member old_payload "captures") (member current_payload "captures"));
    let first_receipt = function
      | `Array (first :: _) -> first
      | _ -> assert false
    in
    assert (
      D.Json.equal
        (first_receipt (member old_payload "receipts"))
        (first_receipt (member current_payload "receipts")));
    print_endline
      "legacy absence/null retained; history edit/compaction leave exact carrier; \
       dropping unknowns/custody rejects; Clear retains receipts");
  [%expect
    {| legacy absence/null retained; history edit/compaction leave exact carrier; dropping unknowns/custody rejects; Clear retains receipts |}]
;;

let%expect_test
    "combined goal and instruction reservations charge the actual whole host additively"
  =
  with_actor_workspace (fun _ workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let goals, _, _ = Goal_catalog_tests.create A.Goal_catalog.empty "combined-reserve" in
    let project = C.project ~root ~scope:I.Scope.root (snapshot []) |> protocol_ok in
    let instructions = apply Selection.empty "reserve" (Replace_project project) in
    let state =
      { initial with
        goal_catalog = Some goals
      ; instruction_selection = Some instructions
      }
    in
    let encoded =
      SD.encode (SD.authored state) ~limits:document_limits
      |> document_ok
      |> D.Document.json
    in
    let size = String.length (Jsonaf.to_string encoded) in
    let host_limits bytes =
      D.Limits.create
        ~max_bytes:bytes
        ~max_depth:128
        ~max_fields:500000
        ~max_nodes:1000000
      |> document_ok
    in
    let residual =
      Selection.remaining_host_limits instructions (host_limits (size + 4096))
      |> document_ok
    in
    assert (D.Limits.max_bytes residual = size);
    assert (D.Limits.max_depth residual = 128);
    assert (
      Result.is_error (Selection.remaining_host_limits instructions (host_limits 32)));
    assert (
      Result.is_error (SD.encode (SD.authored state) ~limits:(host_limits (size + 4096))));
    assert (
      Result.is_ok (SD.encode (SD.authored state) ~limits:(host_limits (size + 65536))));
    let no_counter_headroom =
      { state with
        counters = { state.counters with transaction_sequence = Int64.max_value }
      }
    in
    assert (Result.is_error (State.validate no_counter_headroom)));
  print_endline
    "goal growth cannot reuse Clear reserve; actual outer bytes/unknowns and residual \
     depth apply; host counters retain one Clear";
  [%expect
    {| goal growth cannot reuse Clear reserve; actual outer bytes/unknowns and residual depth apply; host counters retain one Clear |}]
;;

let%expect_test
    "actual Session_store closes and reopens exact inert bytes and one accepted receipt"
  =
  List.iter [ false; true ] ~f:(fun checkpoint ->
    with_actor_workspace (fun env workspace_instance ->
      let initial =
        actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
      in
      let store_root =
        Filename.concat workspace_instance.canonical_root.native_path "instruction-store"
      in
      let source_path =
        Eio.Path.(
          Eio.Stdenv.fs env
          / Filename.concat workspace_instance.canonical_root.native_path "SKILL.fixture")
      in
      Eio.Path.save ~create:(`Exclusive 0o600) source_path "# Durable captured bytes\r\n";
      let file = loaded "durable/SKILL.md" (Eio.Path.load source_path) in
      let command =
        request
          Selection.empty
          "physical-selection"
          (Replace_skills (C.skills (snapshot [ file ]) |> protocol_ok))
      in
      let server_id = P.Id.Server.of_string "srv_instruction_store" |> protocol_ok in
      let accepted_receipt =
        Eio.Switch.run (fun sw ->
          let sessions =
            Store.Session_store.create
              ~env
              ~sw
              ~root:store_root
              ~server_id
              ~process_start_identity:None
              ~lock_nonce:"instruction-first"
            |> store_ok
          in
          let handle =
            Store.Session_store.create_session
              sessions
              ~sw
              ~transaction_id
              ~actor_lock_nonce:"instruction-first"
              { schema_version = Store.Session_store.current_metadata_schema_version
              ; session = State.summary initial
              ; prompt_artifact = P.Id.Prompt_revision.to_string prompt_revision_id
              ; workspace_identity = workspace_instance.conflict_domain
              ; data_schema_version = State.current_schema_version
              }
            |> store_ok
          in
          Exn.protect
            ~finally:(fun () ->
              Store.Session_store.close_session sessions handle |> store_ok;
              Store.Session_store.close sessions |> store_ok)
            ~f:(fun () ->
              let journal =
                Store.Journal.create
                  ~env
                  ~directory:(Store.Session_store.Handle.journal_directory handle)
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
              Exn.protect
                ~finally:(fun () -> Store.Commit_writer.close writer)
                ~f:(fun () ->
                  let fail_save = ref true in
                  let durable =
                    Persistence.create
                      ~archive:(fun _ _ -> failwith "unexpected archive")
                      ~pending_archive:None
                      ~before_commit:
                        (Some
                           (fun _ ->
                             if !fail_save
                             then
                               Error
                                 (P.Error.create
                                    Persistence_error
                                    ~message:"injected pre-write save failure"
                                    ~retryable:true
                                    ())
                             else Ok ()))
                      ~command_accepted:(fun _ _ -> ())
                      ~writer
                      ~durability:Flush
                      ~limits:document_limits
                      ~archive_limits:document_limits
                      ~retention_preflight:None
                      ~restored:(Persistence.Restored.authored initial)
                      ~previous_transaction_hash:None
                  in
                  let transition, receipt =
                    prepare_state
                      (Persistence.Restored.state_document (Persistence.restored durable))
                      command
                    |> prepared
                  in
                  assert (
                    Result.is_error
                      (Persistence.commit
                         durable
                         ~command_audit:None
                         ~previous:initial
                         transition));
                  assert (
                    Option.is_none
                      (Persistence.Restored.state (Persistence.restored durable))
                        .instruction_selection);
                  fail_save := false;
                  Persistence.commit
                    durable
                    ~command_audit:None
                    ~previous:initial
                    transition
                  |> protocol_ok;
                  let accepted =
                    Persistence.Restored.state (Persistence.restored durable)
                  in
                  assert (List.length (Selection.receipts (selection accepted)) = 1);
                  if checkpoint
                  then
                    Persistence.install_snapshot
                      durable
                      ~env
                      ~handle
                      ~max_payload_length:1048576
                      ~transaction_hash:(Persistence.transaction_hash durable)
                      accepted
                    |> store_ok
                    |> ignore;
                  receipt)))
      in
      Eio.Path.unlink source_path;
      Eio.Switch.run (fun sw ->
        let sessions =
          Store.Session_store.open_existing
            ~env
            ~sw
            ~root:store_root
            ~process_start_identity:None
            ~lock_nonce:"instruction-reopen"
          |> store_ok
        in
        let handle =
          Store.Session_store.open_session
            sessions
            ~sw
            ~actor_lock_nonce:"instruction-reopen"
            session_id
          |> store_ok
        in
        Exn.protect
          ~finally:(fun () ->
            Store.Session_store.close_session sessions handle |> store_ok;
            Store.Session_store.close sessions |> store_ok)
          ~f:(fun () ->
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
                ~initial:(Persistence.Restored.authored initial)
                ~restore_snapshot:(Persistence.restore_snapshot ~limits:document_limits)
                ~apply:(Persistence.apply_transaction ~limits:document_limits)
                ~validate_transaction:
                  (Persistence.validate_transaction ~limits:document_limits)
                ~validate:Persistence.validate
              |> store_ok
            in
            let basis = Persistence.Restored.state_document recovered.state in
            let restored = SD.value basis in
            assert (
              List.equal
                String.equal
                (bytes (Selection.skills (selection restored)))
                [ "# Durable captured bytes\r\n" ]);
            assert (Option.is_none restored.active_operation);
            assert (List.is_empty restored.conversation.canonical_history);
            assert (List.length (Selection.receipts (selection restored)) = 1);
            (match prepare_state basis command |> protocol_ok with
             | Retained receipt -> assert (Command.Receipt.equal receipt accepted_receipt)
             | Prepared _ -> assert false);
            assert (Int64.equal recovered.latest_transaction_sequence 1L);
            ()))));
  print_endline
    "real save failure publishes nothing; journal/snapshot reopen preserve exact \
     captured bytes after source deletion; retry writes nothing; no source/native \
     authority claimed";
  [%expect
    {| real save failure publishes nothing; journal/snapshot reopen preserve exact captured bytes after source deletion; retry writes nothing; no source/native authority claimed |}]
;;
