open! Core
open Fixtures
module P = Agent_protocol
module A = Agent_session
module R = P.History.Content_revision

let occurrence sequence text =
  let id =
    History_entry.Id.create ~namespace:(P.Id.Session.to_string session_id) ~sequence
    |> Result.ok_or_failwith
  in
  A.History_codec.user_text ~id text |> A.History_codec.to_protocol
;;

let intent entry text =
  P.History_edit.create
    ~history_id:entry.P.History.id
    ~expected_content_revision:entry.content_revision
    ~text
    ~mode:Save_only
  |> protocol_ok
;;

let%expect_test
    "edit planner keeps occurrence identity and retires only later causal history"
  =
  with_actor_workspace (fun _ workspace_instance ->
    let original =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let before = occurrence 0 "before"
    and target = occurrence 1 "old"
    and after = occurrence 2 "after" in
    let pending = occurrence 3 "pending" in
    let state =
      { original with
        conversation =
          { original.conversation with
            canonical_history = [ before; target; after ]
          ; deferred_user_entries =
              [ pending_document ~generation:original.identity.generation pending ]
          ; next_history_sequence = 8L
          ; reserved_history_through = 8L
          ; initial_prompt_entry_count = 0
          }
      }
    in
    let plan = A.History_edit.prepare state ~edit:(intent target "new") |> protocol_ok in
    let changed = A.History_edit.edited_entry plan in
    let native =
      A.History_codec.all_of_protocol (A.History_edit.canonical_history plan)
      |> protocol_ok
    in
    let roundtrip =
      A.History_codec.all_to_protocol
        ~previous:(A.History_edit.canonical_history plan)
        native
    in
    printf
      "stable-id=%b revision=%s retired=%d kept=%d native-revision-preserved=%b\n"
      (P.History.Id.equal target.id changed.id)
      (Int64.to_string (R.to_int64 changed.content_revision))
      (List.length (A.History_edit.retired_ids plan))
      (List.length (A.History_edit.canonical_history plan))
      (List.equal P.History.equal_entry roundtrip (A.History_edit.canonical_history plan));
    printf
      "stale-basis=%b pending-basis=%b\n"
      (Result.is_error
         (A.History_edit.validate_basis
            plan
            { state with
              conversation =
                { state.conversation with canonical_history = [ before; target ] }
            }))
      (Result.is_error
         (A.History_edit.validate_basis
            plan
            { state with
              conversation = { state.conversation with deferred_user_entries = [] }
            }));
    [%expect
      {|stable-id=true revision=1 retired=1 kept=2 native-revision-preserved=true
stale-basis=true pending-basis=true|}])
;;

let%expect_test
    "retained target overlay is unsupported without resetting unrelated moderator state"
  =
  with_actor_workspace (fun _ workspace_instance ->
    let original =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let target = occurrence 0 "old" in
    let snapshot = handoff_snapshot 0 in
    let replacement : Session.Moderator_state.Identity_snapshot.Replacement.t =
      { target_id = target.id
      ; change_id = 0
      ; value = History_entry.payload (A.History_codec.user_text ~id:target.id "overlay")
      ; script_label = None
      }
    in
    let tombstone : Session.Moderator_state.Identity_snapshot.Tombstone.t =
      { target_id = target.id; change_id = 0 }
    in
    let run replacements tombstones =
      let snapshot =
        { snapshot with revision = 1; next_change_id = 1; replacements; tombstones }
      in
      let state =
        { original with
          moderator = Some (A.Moderator_checkpoint.encode snapshot)
        ; conversation =
            { original.conversation with
              canonical_history = [ target ]
            ; next_history_sequence = 8L
            ; reserved_history_through = 8L
            }
        }
      in
      match A.History_edit.prepare state ~edit:(intent target "new") with
      | Ok _ -> failwith "override silently accepted"
      | Error error ->
        (match error.data with
         | `Object fields ->
           (match List.Assoc.find fields "reason" ~equal:String.equal with
            | Some (`String reason) -> print_endline reason
            | _ -> failwith "missing unsupported reason")
         | _ -> failwith "missing unsupported data")
    in
    run [ replacement ] [];
    run [] [ tombstone ];
    [%expect
      {|overlay_override
overlay_override|}])
;;

let%expect_test
    "structural state conversion initializes revisions and preserves unknown entry \
     evidence"
  =
  with_actor_workspace (fun _ workspace_instance ->
    let original =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let entry = occurrence 0 "old" in
    let state =
      { original with
        conversation =
          { original.conversation with
            canonical_history = [ entry ]
          ; next_history_sequence = 8L
          ; reserved_history_through = 8L
          }
      }
    in
    let limits = Document_schema.Limits.default in
    let current =
      A.Session_state_document.authored state
      |> fun value -> A.Session_state_document.encode value ~limits |> document_ok
    in
    let map_field json key f =
      match json with
      | `Object fields ->
        `Object
          (List.map fields ~f:(fun (name, value) ->
             name, if String.equal name key then f value else value))
      | _ -> failwith "fixture object"
    in
    let legacy_entries = function
      | `Array [ `Object fields ] ->
        `Array
          [ `Object
              (("future", `Object [ "evidence", `String "keep" ])
               :: List.filter fields ~f:(fun (name, _) ->
                 not (String.equal name "content_revision")))
          ]
      | _ -> failwith "fixture history"
    in
    let payload =
      Document_schema.Document.payload current
      |> fun value ->
      map_field value "conversation" (fun value ->
        let value =
          match value with
          | `Object fields ->
            `Object
              (List.filter fields ~f:(fun (name, _) ->
                 not
                   (String.equal name "pending_revision"
                    || String.equal name "pending_dispositions")))
          | _ -> failwith "fixture conversation"
        in
        map_field value "canonical_history" legacy_entries)
    in
    let legacy =
      Document_schema.Document.create ~limits ~kind:"session.state" ~version:5 ~payload
      |> document_ok
    in
    let restored = A.Session_state_document.decode ~limits legacy |> document_ok in
    let saved = A.Session_state_document.encode restored ~limits |> document_ok in
    let json = Document_schema.Document.json saved |> Jsonaf.to_string in
    printf
      "version=%d revision=%s unknown=%b\n"
      (Document_schema.Document.version saved)
      (Int64.to_string
         (R.to_int64
            (List.hd_exn
               (A.Session_state_document.value restored).conversation.canonical_history)
              .content_revision))
      (String.is_substring json ~substring:"\"evidence\":\"keep\"");
    [%expect {| version=9 revision=0 unknown=true |}])
;;

let%expect_test
    "exact retirement custody admits archived suffix unknowns in live and replay paths"
  =
  with_actor_workspace (fun env workspace_instance ->
    let original =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let target = occurrence 0 "old"
    and suffix = occurrence 1 "later" in
    let state =
      { original with
        conversation =
          { original.conversation with
            canonical_history = [ target; suffix ]
          ; next_history_sequence = 8L
          ; reserved_history_through = 8L
          }
      }
    in
    let limits = Document_schema.Limits.default in
    let document =
      A.Session_state_document.authored state
      |> fun value -> A.Session_state_document.encode value ~limits |> document_ok
    in
    let map_field json key f =
      match json with
      | `Object fields ->
        `Object
          (List.map fields ~f:(fun (name, value) ->
             name, if String.equal name key then f value else value))
      | _ -> failwith "fixture object"
    in
    let raw =
      Document_schema.Document.json document
      |> fun value ->
      map_field value "payload" (fun value ->
        map_field value "conversation" (fun value ->
          map_field value "canonical_history" (function
            | `Array [ `Object target; `Object suffix ] ->
              `Array
                [ `Object (("future", `String "target-evidence") :: target)
                ; `Object (("future", `String "suffix-evidence") :: suffix)
                ]
            | _ -> failwith "fixture history")))
    in
    let previous =
      Document_schema.Document.inspect ~limits raw
      |> document_ok
      |> A.Session_state_document.decode ~limits
      |> document_ok
    in
    let edit = intent target "new" in
    let archive =
      A.Compaction_archive.reference_for previous ~limits ~kind:Edit operation_id
      |> protocol_ok
    in
    let delta = A.Session_delta.History_edited (edit, archive) in
    let next =
      A.Session_delta.apply (A.Session_state_document.value previous) delta |> protocol_ok
    in
    printf
      "unadmitted-merge-rejected=%b\n"
      (Result.is_error
         (A.Session_state_document.encode
            (A.Session_state_document.with_value previous next)
            ~limits));
    let admitted =
      A.History_retirement.admit previous ~delta ~next ~limits |> document_ok
    in
    let live = A.Session_state_document.encode admitted ~limits |> document_ok in
    let encoded =
      A.Session_delta_document.create delta ~limits ~state_document:(fun state ->
        A.Session_state_document.with_value previous state)
      |> document_ok
    in
    let decoded =
      A.Session_delta_document.decode ~limits (A.Session_delta_document.document encoded)
      |> document_ok
    in
    let replayed =
      A.Session_delta_document.apply decoded ~limits previous |> document_ok
    in
    let replay = A.Session_state_document.encode replayed ~limits |> document_ok in
    let text = Document_schema.Document.to_string live in
    let archived =
      A.Compaction_archive.archive_document previous ~limits |> document_ok
    in
    let old = Document_schema.Document.to_string archived in
    printf
      "live-replay-equal=%b target-envelope=%b suffix-retired=%b archive-evidence=%b\n"
      (Jsonaf.exactly_equal
         (Document_schema.Document.json live)
         (Document_schema.Document.json replay))
      (String.is_substring text ~substring:"target-evidence")
      (not (String.is_substring text ~substring:"suffix-evidence"))
      (String.is_substring old ~substring:"target-evidence"
       && String.is_substring old ~substring:"suffix-evidence");
    Eio.Switch.run (fun sw ->
      let module Store = Agent_store in
      let module Persistence = A.Session_persistence in
      let storage = Job_artifact_fixtures.create env sw state in
      let handle = storage.session in
      let basis =
        Store.Snapshot.create
          ~limits
          ~session_id
          ~transaction_sequence:0L
          ~transaction_hash:None
          ~event_sequence:0L
          ~created_at:state.identity.updated_at
          ~prompt_artifact:(P.Id.Prompt_revision.to_string state.spec.prompt_revision_id)
          ~workspace_identity:state.spec.workspace_instance.conflict_domain
          ~payload:(A.Session_state_document.encode previous ~limits |> document_ok)
        |> store_ok
        |> Persistence.restore_snapshot ~limits
        |> store_ok
      in
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
          let persistence =
            Persistence.create
              ~pending_archive:None
              ~before_commit:None
              ~retention_preflight:None
              ~writer
              ~durability:Flush
              ~previous_transaction_hash:None
              ~command_accepted:(fun _ _ -> ())
              ~limits
              ~archive_limits:limits
              ~restored:basis
              ~archive:
                (A.Compaction_archive.write_document
                   ~env
                   ~handle
                   ~max_payload_length:1048576)
          in
          let transition =
            A.Session_transition.apply ~now:timestamp state ~delta ~payloads:[]
            |> protocol_ok
          in
          Persistence.commit persistence ~command_audit:None ~previous:state transition
          |> protocol_ok;
          let stored =
            Persistence.restored persistence
            |> Persistence.Restored.state_document
            |> fun value -> A.Session_state_document.encode value ~limits |> document_ok
          in
          Store.Commit_writer.close writer;
          let reopened =
            Store.Journal.open_existing
              ~env
              ~directory:(Store.Session_store.Handle.journal_directory handle)
              ~max_payload_length:1048576
              ~max_segment_bytes:4194304L
              ~max_segment_frames:16
            |> store_ok
          in
          let scan = Store.Journal.scan reopened |> store_ok in
          let recovered =
            List.fold scan.entries ~init:previous ~f:(fun document entry ->
              let record =
                Store.Document_record.of_frame entry.frame ~limits ~expected_digest:None
                |> Result.map_error ~f:(fun error ->
                  Sexp.to_string_hum (Store.Document_record.Error.sexp_of_t error))
                |> Result.ok_or_failwith
              in
              let transaction =
                Store.Transaction.decode_record record ~limits |> store_ok
              in
              Persistence.apply_document document ~limits transaction |> store_ok)
          in
          let recovered =
            A.Session_state_document.encode recovered ~limits |> document_ok
          in
          let archive_path =
            Filename.concat
              (Store.Session_store.Handle.archive_directory handle)
              (A.Compaction_archive.filename archive)
          in
          let raw = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / archive_path) in
          let _, archived =
            A.Compaction_archive.decode_record
              ~max_payload_length:1048576
              ~expected_digest:(Some archive.sha256)
              raw
            |> protocol_ok
          in
          let archived =
            A.Compaction_archive.archive_document archived ~limits
            |> document_ok
            |> Document_schema.Document.to_string
          in
          printf
            "durable-replay=%b archived-unknowns=%b\n"
            (Document_schema.Json.equal
               (Document_schema.Document.json stored)
               (Document_schema.Document.json recovered))
            (String.is_substring archived ~substring:"target-evidence"
             && String.is_substring archived ~substring:"suffix-evidence")));
    let forged = { archive with sha256 = String.make 64 '0' } in
    printf
      "forged-archive-rejected=%b\n"
      (Result.is_error
         (A.History_retirement.admit
            previous
            ~delta:(History_edited (edit, forged))
            ~next
            ~limits));
    [%expect
      {|unadmitted-merge-rejected=true
live-replay-equal=true target-envelope=true suffix-retired=true archive-evidence=true
durable-replay=true archived-unknowns=true
forged-archive-rejected=true|}])
;;

let%expect_test
    "authored initial user instructions are protected while appended plain segments are \
     editable"
  =
  with_actor_workspace (fun _ workspace_instance ->
    let original =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let id = (occurrence 0 "initial").P.History.id in
    let module Payload = History_entry.Payload in
    let semantic =
      Payload.Semantic.create
        (Message
           { form = Input
           ; role = User
           ; content =
               [ Text { text = "first"; annotations = []; logprobs = Absent }
               ; Text { text = "second"; annotations = []; logprobs = Absent }
               ]
           ; phase = Absent
           })
        ~metadata:Payload.Metadata.empty
      |> Result.ok_or_failwith
    in
    let target =
      History_entry.create_with_id ~id (Payload.authored semantic)
      |> A.History_codec.to_protocol
    in
    let state count =
      { original with
        conversation =
          { original.conversation with
            canonical_history = [ target ]
          ; initial_prompt_entry_count = count
          ; next_history_sequence = 8L
          ; reserved_history_through = 8L
          }
      }
    in
    let replacement = intent target "complete replacement" in
    let rejected = A.History_edit.prepare (state 1) ~edit:replacement in
    let reason =
      match rejected with
      | Error error ->
        P.Json_codec.fields error.data
        |> protocol_ok
        |> fun fields ->
        P.Json_codec.required_as fields "reason" P.History_edit.Unsupported_target.of_json
        |> protocol_ok
      | Ok _ -> failwith "initial user instruction edited"
    in
    print_s [%sexp (reason : P.History_edit.Unsupported_target.t)];
    let admitted = A.History_edit.prepare (state 0) ~edit:replacement |> protocol_ok in
    printf
      "ordinary-revision=%s\n"
      (Int64.to_string
         (R.to_int64 (A.History_edit.edited_entry admitted).content_revision));
    [%expect
      {|Initial_instruction
ordinary-revision=1|}])
;;

let%expect_test "real stopped actor saves once without activating its absent worker" =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let actor, _ = audit_actor ~sw ~env ~workspace_instance ~reject_archive:false () in
      Exn.protect
        ~finally:(fun () -> A.Session_actor.shutdown actor)
        ~f:(fun () ->
          let writer, _ =
            A.Session_actor.attach actor ~mode:Read_write ~subscribe:false |> protocol_ok
          in
          let original = A.Session_actor.state actor |> protocol_ok in
          let target = occurrence 0 "old"
          and suffix = occurrence 1 "causal answer" in
          A.Session_actor.append_history actor ~attachment_id:writer.id [ target; suffix ]
          |> protocol_ok
          |> ignore;
          let before = A.Session_actor.state actor |> protocol_ok in
          let edit =
            P.History_edit.create
              ~history_id:target.id
              ~expected_content_revision:target.content_revision
              ~text:"new"
              ~mode:Edit_and_continue
            |> protocol_ok
          in
          let request : P.History_edit.Edit_request.t =
            { session_id
            ; attachment_id = writer.id
            ; expected_generation = before.identity.generation
            ; expected_revision = before.counters.revision
            ; edit
            ; idempotency_key = P.Idempotency_key.of_string "stopped-edit" |> protocol_ok
            }
          in
          let result = A.Session_actor.edit_history actor request |> protocol_ok in
          let after = A.Session_actor.state actor |> protocol_ok in
          assert (
            List.equal
              P.History.equal_entry
              original.conversation.canonical_history
              (List.take
                 after.conversation.canonical_history
                 (List.length original.conversation.canonical_history)));
          assert (
            not
              (List.exists after.conversation.canonical_history ~f:(fun entry ->
                 P.History.Id.equal entry.id suffix.id)));
          printf
            "kept=%d archives=%d revision=%s operation=%b\n"
            (List.length after.conversation.canonical_history)
            (List.length after.conversation.compaction_archives)
            (Int64.to_string (R.to_int64 result.content_revision))
            (Option.is_some after.active_operation);
          print_s [%sexp (result.continuation : P.History_edit.Continuation.t)];
          printf
            "stale-session-rejected=%b\n"
            (Result.is_error (A.Session_actor.edit_history actor request));
          [%expect
            {|kept=2 archives=1 revision=1 operation=false
(Not_started Stopped)
stale-session-rejected=true|}])))
;;

let%expect_test
    "retirement never splits bound or nearest same-family unresolved tool pairs"
  =
  with_actor_workspace (fun _ workspace_instance ->
    let module Payload = History_entry.Payload in
    let id sequence = (occurrence sequence "id").P.History.id in
    let entry sequence view =
      Payload.Semantic.create
        view
        ~metadata:{ Payload.Metadata.empty with call_id = Value "reused" }
      |> Result.ok_or_failwith
      |> Payload.authored
      |> History_entry.create_with_id ~id:(id sequence)
      |> A.History_codec.to_protocol
    in
    let call sequence kind =
      entry
        sequence
        (Call
           { kind
           ; name = "read_file"
           ; namespace = Absent
           ; input_bytes = "{}"
           ; async = Absent
           })
    in
    let output sequence kind relation =
      entry sequence (Result { kind; relation; output = Text "saved outcome" })
    in
    let original =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let target = occurrence 1 "edit" in
    let check history =
      let state =
        { original with
          conversation =
            { original.conversation with
              canonical_history = history
            ; next_history_sequence = 16L
            ; reserved_history_through = 16L
            }
        }
      in
      Result.is_error (A.History_edit.prepare state ~edit:(intent target "new"))
    in
    printf
      "bound=%b unresolved=%b repeated-nearest=%b cross-family=%b\n"
      (check [ call 0 Function; target; output 2 Function (Bound (id 0)) ])
      (check [ call 0 Function; target; output 2 Function Unresolved ])
      (check [ call 0 Function; target; call 2 Function; output 3 Function Unresolved ])
      (check [ call 0 Function; target; call 2 Custom; output 3 Function Unresolved ]);
    [%expect {|bound=true unresolved=true repeated-nearest=false cross-family=true|}])
;;

let%expect_test
    "actual old delta tags and overlay event migrate before strict entry decode"
  =
  let limits = Document_schema.Limits.default in
  let legacy_entry =
    match P.History.entry_to_json (occurrence 0 "retained") with
    | `Object fields ->
      `Object
        (List.filter fields ~f:(fun (name, _) ->
           not (String.equal name "content_revision")))
    | _ -> assert false
  in
  List.iter [ "canonical_entries_appended"; "deferred_entries_enqueued" ] ~f:(fun tag ->
    let payload =
      Jsonaf.of_string
        (sprintf
           {|{"changes":[{"kind":%s,"entries":[%s]}]}|}
           (Jsonaf.to_string (`String tag))
           (Jsonaf.to_string legacy_entry))
    in
    let legacy =
      Document_schema.Document.create ~limits ~kind:"session.delta" ~version:3 ~payload
      |> document_ok
    in
    let restored = A.Session_delta_document.decode ~limits legacy |> document_ok in
    let revision =
      match A.Session_delta_document.value restored with
      | Canonical_entries_appended [ entry ] | Deferred_entries_enqueued [ entry ] ->
        R.to_int64 entry.content_revision
      | Batch [ Canonical_entries_appended [ entry ] ]
      | Batch [ Deferred_entries_enqueued [ entry ] ] -> R.to_int64 entry.content_revision
      | _ -> failwith "unexpected legacy delta"
    in
    printf "%s=%s\n" tag (Int64.to_string revision));
  let legacy_window =
    let window = A.Session_state.history_window [ occurrence 0 "retained" ] in
    match P.History.Window.to_json window with
    | `Object fields ->
      `Object
        (List.map fields ~f:(fun (name, value) ->
           name, if String.equal name "entries" then `Array [ legacy_entry ] else value))
    | _ -> assert false
  in
  let event_payload =
    Jsonaf.of_string
      (sprintf
         {|{"session_id":%s,"sequence":"1","revision":"1","timestamp":%s,"kind":"moderator.overlay_changed","visibility":"full","payload":{"effective_history":%s,"overlay_evidence":"original"}}|}
         (Jsonaf.to_string (P.Id.Session.to_json session_id))
         (Jsonaf.to_string (P.Timestamp.to_json timestamp))
         (Jsonaf.to_string legacy_window))
  in
  let event version =
    Document_schema.Document.create
      ~limits
      ~kind:"session.event"
      ~version
      ~payload:event_payload
    |> document_ok
  in
  printf
    "legacy-overlay=%b current-missing-revision-rejected=%b\n"
    (Result.is_ok (A.Durable_event_document.decode ~limits (event 1)))
    (Result.is_error (A.Durable_event_document.decode ~limits (event 2)));
  [%expect
    {|canonical_entries_appended=0
deferred_entries_enqueued=0
legacy-overlay=true current-missing-revision-rejected=true|}]
;;

let%expect_test
    "actual owner cancellation after edit commit retains admitted history without \
     claiming dispatch"
  =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
        let target = occurrence 0 "old" in
        let base =
          actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
        in
        let initial =
          { base with
            lifecycle = { desired = Running; observed = Idle }
          ; conversation =
              { base.conversation with
                canonical_history = [ target ]
              ; next_history_sequence = 8L
              ; reserved_history_through = 8L
              }
          }
        in
        let backend = A.Memory_backend.create ~event_capacity:64 ~initial_state:initial in
        let committed, committed_u = Eio.Promise.create () in
        let release, release_u = Eio.Promise.create () in
        let owner_context, owner_context_u = Eio.Promise.create () in
        let launched = ref false in
        let worker =
          A.Operation_worker.create ~run:(fun ~sw:_ ~input _ ->
            launched := true;
            Completed
              { final_history = input.history
              ; runtime_requests = []
              ; moderator_snapshot = None
              })
        in
        let owner =
          Eio.Fiber.fork_promise ~sw (fun () ->
            try
              Eio.Cancel.sub (fun context ->
                Eio.Promise.resolve owner_context_u context;
                Eio.Switch.run (fun owner_sw ->
                  let actor =
                    A.Session_actor.create
                      ~sw:owner_sw
                      ~clock:(Eio.Stdenv.clock env)
                      ~mailbox_capacity:32
                      ~compaction_env:None
                      ~initial_state:initial
                      ~operation_worker:(Some worker)
                      ~persistence:
                        { (A.Memory_backend.persistence backend) with archive_reference }
                      ~services:
                        { now = (fun () -> timestamp)
                        ; create_attachment_id = P.Id.Attachment.create
                        ; create_reclaim_token = (fun () -> "history-cancel")
                        ; job_results = None
                        ; monotonic_now = (fun () -> Mtime.min_stamp)
                        ; schedule_limits = A.Staged_schedules.default_limits
                        ; notification_limits = A.Staged_notifications.default_limits
                        ; ingress_limits = A.Staged_ingress.default_limits
                        ; subscription_limits = A.Staged_subscriptions.default_limits
                        ; state_committed =
                            (fun _ events ->
                              if
                                List.exists events ~f:(fun event ->
                                  P.Event.Durable.equal_kind event.kind History_replaced)
                              then (
                                Eio.Promise.resolve committed_u ();
                                Eio.Promise.await release))
                        }
                  in
                  let writer, _ =
                    A.Session_actor.attach actor ~mode:Read_write ~subscribe:false
                    |> protocol_ok
                  in
                  let state = A.Session_actor.state actor |> protocol_ok in
                  let edit =
                    P.History_edit.create
                      ~history_id:target.id
                      ~expected_content_revision:target.content_revision
                      ~text:"new"
                      ~mode:Edit_and_continue
                    |> protocol_ok
                  in
                  let request : P.History_edit.Edit_request.t =
                    { session_id
                    ; attachment_id = writer.id
                    ; expected_generation = 0
                    ; expected_revision = state.counters.revision
                    ; edit
                    ; idempotency_key =
                        P.Idempotency_key.of_string "owner-cancel" |> protocol_ok
                    }
                  in
                  ignore (A.Session_actor.edit_history actor request);
                  A.Session_actor.shutdown actor));
              false
            with
            | Eio.Cancel.Cancelled _ -> true)
        in
        Eio.Promise.await committed;
        let admitted = A.Memory_backend.state backend in
        assert (Option.is_some admitted.active_operation);
        assert (not !launched);
        Eio.Cancel.cancel (Eio.Promise.await owner_context) Exit;
        Eio.Promise.resolve release_u ();
        let cancelled = Eio.Promise.await_exn owner in
        let retained = A.Memory_backend.state backend in
        printf
          "owner-cancelled=%b retained-edit=%b admitted-for-recovery=%b worker-started=%b\n"
          cancelled
          (Int64.equal
             (R.to_int64
                (List.hd_exn retained.conversation.canonical_history).content_revision)
             1L)
          (Option.is_some retained.active_operation)
          !launched;
        [%expect
          {|owner-cancelled=true retained-edit=true admitted-for-recovery=true worker-started=false|}])))
;;

let%expect_test
    "ready real worker receives revised input after one edit-and-Turn admission"
  =
  let target = occurrence 0 "old"
  and suffix = occurrence 1 "obsolete" in
  Job_fixtures.with_actor
    ~prepare_state:(fun state ->
      { state with
        conversation =
          { state.conversation with
            canonical_history = [ target; suffix ]
          ; next_history_sequence = 8L
          ; reserved_history_through = 8L
          }
      })
    (fun _ _ actor writer backend ->
       let captured, captured_u = Eio.Promise.create () in
       let release, release_u = Eio.Promise.create () in
       let worker =
         A.Operation_worker.create ~run:(fun ~sw:_ ~input _ ->
           let started =
             A.Memory_backend.events_after backend 0L
             |> protocol_ok
             |> List.exists ~f:(fun event ->
               match event.P.Event.Durable.kind with
               | Operation_started -> true
               | _ -> false)
           in
           Eio.Promise.resolve captured_u (input, started);
           Eio.Promise.await release;
           Completed
             { final_history = input.history
             ; runtime_requests = []
             ; moderator_snapshot = None
             })
       in
       A.Session_actor.set_operation_worker actor (Some worker) |> protocol_ok;
       let before = A.Session_actor.state actor |> protocol_ok in
       let edit =
         P.History_edit.create
           ~history_id:target.id
           ~expected_content_revision:target.content_revision
           ~text:"revised"
           ~mode:Edit_and_continue
         |> protocol_ok
       in
       let request : P.History_edit.Edit_request.t =
         { session_id
         ; attachment_id = writer.id
         ; expected_generation = before.identity.generation
         ; expected_revision = before.counters.revision
         ; edit
         ; idempotency_key = P.Idempotency_key.of_string "ready-edit" |> protocol_ok
         }
       in
       let result = A.Session_actor.edit_history actor request |> protocol_ok in
       let input, started_before_worker = Eio.Promise.await captured in
       let expected = A.History_codec.user_text ~id:target.id "revised" in
       let actual = List.hd_exn input.history in
       printf
         "one-admission=%b published-first=%b revised-input=%b same-operation=%b\n"
         (Int64.equal result.session.revision Int64.(before.counters.revision + 1L))
         started_before_worker
         (Document_schema.Json.equal
            (History_entry.Payload.to_json (History_entry.payload actual))
            (History_entry.Payload.to_json (History_entry.payload expected)))
         (match result.continuation with
          | Started id -> P.Id.Operation.equal id input.operation.id
          | _ -> false);
       let active = A.Session_actor.state actor |> protocol_ok in
       let repeat = { request with expected_revision = active.counters.revision } in
       printf
         "active-edit-rejected=%b\n"
         (Result.is_error (A.Session_actor.edit_history actor repeat));
       Eio.Promise.resolve release_u ();
       await_idle actor |> ignore;
       let saved = A.Session_actor.state actor |> protocol_ok in
       printf
         "retained-revision=%s kept=%d archives=%d\n"
         (Int64.to_string
            (R.to_int64
               (List.hd_exn saved.conversation.canonical_history).content_revision))
         (List.length saved.conversation.canonical_history)
         (List.length saved.conversation.compaction_archives));
  [%expect
    {|one-admission=true published-first=true revised-input=true same-operation=true
active-edit-rejected=true
retained-revision=1 kept=1 archives=1|}]
;;

let%expect_test
    "ordinary deletion retains later entries and distinct exact prior evidence"
  =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let actor, _ = audit_actor ~sw ~env ~workspace_instance ~reject_archive:false () in
      Exn.protect
        ~finally:(fun () -> A.Session_actor.shutdown actor)
        ~f:(fun () ->
          let writer, _ =
            A.Session_actor.attach actor ~mode:Read_write ~subscribe:false |> protocol_ok
          in
          let target = occurrence 0 "remove middle"
          and later = occurrence 1 "retain later" in
          A.Session_actor.append_history actor ~attachment_id:writer.id [ target; later ]
          |> protocol_ok
          |> ignore;
          let before = A.Session_actor.state actor |> protocol_ok in
          A.Session_actor.delete_history
            actor
            ~attachment_id:writer.id
            ~expected_revision:before.counters.revision
            target.id
          |> protocol_ok
          |> ignore;
          let after = A.Session_actor.state actor |> protocol_ok in
          let archive = List.hd_exn after.conversation.compaction_archives in
          printf
            "later-retained=%b removed=%b kind-delete=%b prior-revision=%b \
             pending-preserved=%b allocator-preserved=%b\n"
            (List.exists
               after.conversation.canonical_history
               ~f:(P.History.equal_entry later))
            (not
               (List.exists after.conversation.canonical_history ~f:(fun entry ->
                  P.History.Id.equal entry.id target.id)))
            (A.Session_state.Compaction_archive.equal_kind archive.kind Delete)
            (Int64.equal archive.revision before.counters.revision)
            (List.equal
               A.Pending_input_document.equal
               before.conversation.deferred_user_entries
               after.conversation.deferred_user_entries)
            (Int64.equal
               before.conversation.next_history_sequence
               after.conversation.next_history_sequence);
          [%expect
            {|later-retained=true removed=true kind-delete=true prior-revision=true pending-preserved=true allocator-preserved=true|}])))
;;

let%expect_test "actual archive and journal failures leave deletion and edit unpublished" =
  with_actor_workspace (fun env workspace_instance ->
    List.iter [ true; false ] ~f:(fun reference_failure ->
      List.iter [ true; false ] ~f:(fun deletion ->
        Eio.Switch.run (fun sw ->
          let actor, backend =
            audit_actor
              ~sw
              ~env
              ~workspace_instance
              ~reject_archive:(not reference_failure)
              ~reject_archive_reference:reference_failure
              ()
          in
          Exn.protect
            ~finally:(fun () -> A.Session_actor.shutdown actor)
            ~f:(fun () ->
              let writer, _ =
                A.Session_actor.attach actor ~mode:Read_write ~subscribe:false
                |> protocol_ok
              in
              let target = occurrence 0 "old" in
              A.Session_actor.append_history actor ~attachment_id:writer.id [ target ]
              |> protocol_ok
              |> ignore;
              let before = A.Session_actor.state actor |> protocol_ok in
              let events_before =
                A.Memory_backend.events_after backend 0L |> protocol_ok |> List.length
              in
              let result =
                if deletion
                then
                  A.Session_actor.delete_history
                    actor
                    ~attachment_id:writer.id
                    ~expected_revision:before.counters.revision
                    target.id
                  |> Result.map ~f:ignore
                else
                  A.Session_actor.edit_history
                    actor
                    { session_id
                    ; attachment_id = writer.id
                    ; expected_generation = before.identity.generation
                    ; expected_revision = before.counters.revision
                    ; edit = intent target "new"
                    ; idempotency_key =
                        P.Idempotency_key.of_string "failed-edit" |> protocol_ok
                    }
                  |> Result.map ~f:ignore
              in
              let after = A.Session_actor.state actor |> protocol_ok in
              printf
                "%s/%s rejected=%b same-revision=%b same-history=%b archives=%d \
                 operation=%b unpublished=%b\n"
                (if reference_failure then "archive" else "journal")
                (if deletion then "delete" else "edit")
                (Result.is_error result)
                (Int64.equal before.counters.revision after.counters.revision)
                (List.equal
                   P.History.equal_entry
                   before.conversation.canonical_history
                   after.conversation.canonical_history)
                (List.length after.conversation.compaction_archives)
                (Option.is_some after.active_operation)
                (Int.equal
                   events_before
                   (A.Memory_backend.events_after backend 0L |> protocol_ok |> List.length))))));
    [%expect
      {|archive/delete rejected=true same-revision=true same-history=true archives=0 operation=false unpublished=true
archive/edit rejected=true same-revision=true same-history=true archives=0 operation=false unpublished=true
journal/delete rejected=true same-revision=true same-history=true archives=0 operation=false unpublished=true
journal/edit rejected=true same-revision=true same-history=true archives=0 operation=false unpublished=true|}])
;;

let%expect_test "middle paired deletion transfers only archived unknown custody" =
  with_actor_workspace (fun _ workspace_instance ->
    let module Payload = History_entry.Payload in
    let before = occurrence 0 "before"
    and middle = occurrence 2 "middle retained"
    and after = occurrence 4 "after" in
    let call_id = (occurrence 1 "id").id in
    let call =
      Payload.Semantic.create
        (Call
           { kind = Function
           ; name = "read_file"
           ; namespace = Absent
           ; input_bytes = "{}"
           ; async = Absent
           })
        ~metadata:{ Payload.Metadata.empty with call_id = Value "repeated-provider-id" }
      |> Result.ok_or_failwith
      |> Payload.authored
      |> History_entry.create_with_id ~id:call_id
      |> A.History_codec.to_protocol
    in
    let output =
      Payload.Semantic.create
        (Result { kind = Function; relation = Bound call_id; output = Text "result" })
        ~metadata:{ Payload.Metadata.empty with call_id = Value "repeated-provider-id" }
      |> Result.ok_or_failwith
      |> Payload.authored
      |> History_entry.create_with_id ~id:(occurrence 3 "id").id
      |> A.History_codec.to_protocol
    in
    let base =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let state =
      { base with
        conversation =
          { base.conversation with
            canonical_history = [ before; call; middle; output; after ]
          ; initial_prompt_entry_count = 2
          ; next_history_sequence = 16L
          ; reserved_history_through = 16L
          }
      }
    in
    let limits = Document_schema.Limits.default in
    let raw =
      A.Session_state_document.authored state
      |> fun document ->
      A.Session_state_document.encode document ~limits
      |> document_ok
      |> Document_schema.Document.json
    in
    let rec annotate = function
      | `Object fields ->
        let fields = List.map fields ~f:(fun (name, value) -> name, annotate value) in
        if List.Assoc.mem fields "content_revision" ~equal:String.equal
        then `Object (("future", `String "occurrence-evidence") :: fields)
        else `Object fields
      | `Array values -> `Array (List.map values ~f:annotate)
      | value -> value
    in
    let previous =
      Document_schema.Document.inspect ~limits (annotate raw)
      |> document_ok
      |> A.Session_state_document.decode ~limits
      |> document_ok
    in
    let archive =
      A.Compaction_archive.reference_for previous ~limits ~kind:Delete operation_id
      |> protocol_ok
    in
    let delta = A.Session_delta.History_deleted (call.id, archive) in
    let next = A.Session_delta.apply state delta |> protocol_ok in
    let current =
      A.History_retirement.admit previous ~delta ~next ~limits |> document_ok
    in
    let encoded =
      A.Session_state_document.encode current ~limits
      |> document_ok
      |> Document_schema.Document.to_string
    in
    let roundtrip =
      A.Session_delta_document.create
        delta
        ~limits
        ~state_document:A.Session_state_document.authored
      |> document_ok
      |> A.Session_delta_document.document
      |> A.Session_delta_document.decode ~limits
      |> document_ok
      |> A.Session_delta_document.value
    in
    let replay = A.Session_delta.apply state roundtrip |> protocol_ok in
    let replay =
      A.History_retirement.admit previous ~delta:roundtrip ~next:replay ~limits
      |> document_ok
      |> fun document ->
      A.Session_state_document.encode document ~limits
      |> document_ok
      |> Document_schema.Document.to_string
    in
    printf
      "retained-subsequence=%b initial-count=%d retained-unknown=%b replay=%b\n"
      (List.equal
         P.History.equal_entry
         next.conversation.canonical_history
         [ before; middle; after ])
      next.conversation.initial_prompt_entry_count
      (String.is_substring encoded ~substring:"occurrence-evidence")
      (String.equal encoded replay);
    let forged = { archive with sha256 = String.make 64 '0' } in
    let reversed =
      { next with
        conversation =
          { next.conversation with
            canonical_history = List.rev next.conversation.canonical_history
          }
      }
    in
    printf
      "forged-rejected=%b reorder-rejected=%b exact-archive=%b\n"
      (Result.is_error
         (A.History_retirement.admit
            previous
            ~delta:(History_deleted (call.id, forged))
            ~next
            ~limits))
      (Result.is_error
         (A.History_retirement.admit previous ~delta ~next:reversed ~limits))
      (String.equal
         archive.sha256
         (A.Compaction_archive.archive_document previous ~limits
          |> document_ok
          |> Document_schema.Document.to_string
          |> Agent_store.Document_record.digest));
    [%expect
      {|retained-subsequence=true initial-count=1 retained-unknown=true replay=true
forged-rejected=true reorder-rejected=true exact-archive=true|}])
;;

let%expect_test "independent background outcome commits against edited current history" =
  let target = occurrence 0 "old"
  and suffix = occurrence 1 "causal suffix" in
  Job_fixtures.with_actor
    ~prepare_state:(fun state ->
      { state with
        conversation =
          { state.conversation with
            canonical_history = [ target; suffix ]
          ; next_history_sequence = 8L
          ; reserved_history_through = 8L
          }
      })
    (fun _env _sw actor writer backend ->
       let job = Job_fixtures.add_claimed_job actor in
       let resolved =
         Job_fixtures.with_job actor job (fun ~job:_ ~execute ->
           execute ~invocation:(Job_fixtures.root job) (fun ~dispatched:_ ->
             let before = A.Session_actor.state actor |> protocol_ok in
             let saved =
               A.Session_actor.edit_history
                 actor
                 { session_id
                 ; attachment_id = writer.id
                 ; expected_generation = before.identity.generation
                 ; expected_revision = before.counters.revision
                 ; edit = intent target "edited while background is running"
                 ; idempotency_key =
                     P.Idempotency_key.of_string "background-edit" |> protocol_ok
                 }
               |> protocol_ok
             in
             printf
               "edit-committed=%b\n"
               (R.equal saved.content_revision (R.of_int64 1L |> protocol_ok));
             Ok (P.Invocation.Complete (`String "background result"))))
         |> protocol_ok
       in
       Job_fixtures.complete actor job |> protocol_ok |> ignore;
       let state = A.Memory_backend.state backend in
       let current = List.hd_exn state.conversation.canonical_history in
       printf
         "background-resolved=%b edited-revision=%s kept=%d archives=%d\n"
         (match resolved.status with
          | Resolved _ -> true
          | Admitted | Dispatching | Published _ -> false)
         (Int64.to_string (R.to_int64 current.content_revision))
         (List.length state.conversation.canonical_history)
         (List.length state.conversation.compaction_archives);
       [%expect
         {|edit-committed=true
background-resolved=true edited-revision=1 kept=1 archives=1|}])
;;

let%expect_test "enqueue racing combined edit has one serialized admission winner" =
  List.iter [ true; false ] ~f:(fun enqueue_first ->
    let target = occurrence 0 "old"
    and pending = occurrence 1 "pending" in
    let committed, committed_u = Eio.Promise.create () in
    let release_commit, release_commit_u = Eio.Promise.create () in
    let armed = ref false in
    Job_fixtures.with_actor
      ~prepare_state:(fun state ->
        { state with
          conversation =
            { state.conversation with
              canonical_history = [ target ]
            ; next_history_sequence = 8L
            ; reserved_history_through = 8L
            }
        })
      ~state_committed:(fun _ events ->
        if
          !armed
          && List.exists events ~f:(fun event ->
            match event.P.Event.Durable.kind with
            | History_message_deferred when enqueue_first -> true
            | History_replaced when not enqueue_first -> true
            | _ -> false)
        then (
          armed := false;
          Eio.Promise.resolve committed_u ();
          Eio.Promise.await release_commit))
      (fun _env sw actor writer backend ->
         let worker_entered, worker_entered_u = Eio.Promise.create () in
         let release_worker, release_worker_u = Eio.Promise.create () in
         A.Session_actor.set_operation_worker
           actor
           (Some
              (A.Operation_worker.create ~run:(fun ~sw:_ ~input _ ->
                 Eio.Promise.resolve worker_entered_u ();
                 Eio.Promise.await release_worker;
                 Completed
                   { final_history = input.history
                   ; runtime_requests = []
                   ; moderator_snapshot = None
                   })))
         |> protocol_ok;
         let before = A.Session_actor.state actor |> protocol_ok in
         let edit =
           P.History_edit.create
             ~history_id:target.id
             ~expected_content_revision:target.content_revision
             ~text:"combined"
             ~mode:Edit_and_continue
           |> protocol_ok
         in
         let request : P.History_edit.Edit_request.t =
           { session_id
           ; attachment_id = writer.id
           ; expected_generation = before.identity.generation
           ; expected_revision = before.counters.revision
           ; edit
           ; idempotency_key =
               P.Idempotency_key.of_string "enqueue-edit-race" |> protocol_ok
           }
         in
         let enqueue () =
           A.Session_actor.defer_history actor ~attachment_id:writer.id [ pending ]
           |> Result.map ~f:ignore
         in
         let replace () =
           A.Session_actor.edit_history actor request |> Result.map ~f:ignore
         in
         armed := true;
         let first =
           Eio.Fiber.fork_promise ~sw (if enqueue_first then enqueue else replace)
         in
         Eio.Promise.await committed;
         let second =
           Eio.Fiber.fork_promise ~sw (if enqueue_first then replace else enqueue)
         in
         Eio.Fiber.yield ();
         Eio.Promise.resolve release_commit_u ();
         let first = Eio.Promise.await_exn first
         and second = Eio.Promise.await_exn second in
         let after = A.Session_actor.state actor |> protocol_ok in
         printf
           "%s first=%b second=%b edited=%b pending-exact=%b turn=%b archives=%d\n"
           (if enqueue_first then "enqueue" else "edit")
           (Result.is_ok first)
           (Result.is_ok second)
           (not
              (R.equal
                 (List.hd_exn after.conversation.canonical_history).content_revision
                 R.zero))
           (List.equal
              P.History.equal_entry
              (List.map
                 after.conversation.deferred_user_entries
                 ~f:A.Pending_input_document.entry)
              [ pending ])
           (Option.is_some after.active_operation)
           (List.length after.conversation.compaction_archives);
         if enqueue_first
         then (
           let fresh = { request with expected_revision = after.counters.revision } in
           let rejected = A.Session_actor.edit_history actor fresh in
           printf
             "current-pending-rejected=%b\n"
             (match rejected with
              | Error error -> P.Error.equal_code error.code Pending_input_conflict
              | Ok _ -> false);
           let pending_before_save =
             (A.Memory_backend.state backend).conversation.deferred_user_entries
           in
           let saved = { fresh with edit = intent target "save only with pending" } in
           A.Session_actor.edit_history actor saved |> protocol_ok |> ignore;
           printf
             "save-only-pending-exact=%b\n"
             (List.equal
                A.Pending_input_document.equal
                (A.Memory_backend.state backend).conversation.deferred_user_entries
                pending_before_save))
         else (
           Eio.Promise.await worker_entered;
           let busy =
             { request with
               expected_revision = after.counters.revision
             ; edit =
                 P.History_edit.create
                   ~history_id:target.id
                   ~expected_content_revision:
                     (List.hd_exn after.conversation.canonical_history).content_revision
                   ~text:"blocked"
                   ~mode:Save_only
                 |> protocol_ok
             }
           in
           printf
             "foreground-edit-blocked=%b foreground-delete-blocked=%b\n"
             (Result.is_error (A.Session_actor.edit_history actor busy))
             (Result.is_error
                (A.Session_actor.delete_history
                   actor
                   ~attachment_id:writer.id
                   ~expected_revision:after.counters.revision
                   target.id)));
         Eio.Promise.resolve release_worker_u ();
         if not enqueue_first then await_idle actor |> ignore));
  [%expect
    {|enqueue first=true second=false edited=false pending-exact=true turn=false archives=0
current-pending-rejected=true
save-only-pending-exact=true
edit first=true second=true edited=true pending-exact=true turn=true archives=1
foreground-edit-blocked=true foreground-delete-blocked=true|}]
;;

let%expect_test "failed combined intent never dispatches its prepared Turn" =
  let target = occurrence 0 "old" in
  let launches = ref 0 in
  Job_fixtures.with_actor
    ~prepare_state:(fun state ->
      { state with
        conversation =
          { state.conversation with
            canonical_history = [ target ]
          ; next_history_sequence = 8L
          ; reserved_history_through = 8L
          }
      })
    ~reject_save:(fun transition ->
      not
        (List.is_empty
           transition.A.Session_transition.state.conversation.compaction_archives))
    (fun _env _sw actor writer backend ->
       let worker =
         A.Operation_worker.create ~run:(fun ~sw:_ ~input _ ->
           Int.incr launches;
           Completed
             { final_history = input.history
             ; runtime_requests = []
             ; moderator_snapshot = None
             })
       in
       A.Session_actor.set_operation_worker actor (Some worker) |> protocol_ok;
       let before = A.Session_actor.state actor |> protocol_ok in
       let events_before =
         A.Memory_backend.events_after backend 0L |> protocol_ok |> List.length
       in
       let edit =
         P.History_edit.create
           ~history_id:target.id
           ~expected_content_revision:target.content_revision
           ~text:"must not publish"
           ~mode:Edit_and_continue
         |> protocol_ok
       in
       let result =
         A.Session_actor.edit_history
           actor
           { session_id
           ; attachment_id = writer.id
           ; expected_generation = before.identity.generation
           ; expected_revision = before.counters.revision
           ; edit
           ; idempotency_key =
               P.Idempotency_key.of_string "combined-journal-failure" |> protocol_ok
           }
       in
       let after = A.Session_actor.state actor |> protocol_ok in
       printf
         "rejected=%b launches=%d operation=%b archives=%d unchanged=%b unpublished=%b\n"
         (Result.is_error result)
         !launches
         (Option.is_some after.active_operation)
         (List.length after.conversation.compaction_archives)
         (List.equal
            P.History.equal_entry
            before.conversation.canonical_history
            after.conversation.canonical_history)
         (Int.equal
            events_before
            (A.Memory_backend.events_after backend 0L |> protocol_ok |> List.length));
       [%expect
         {|rejected=true launches=0 operation=false archives=0 unchanged=true unpublished=true|}])
;;
