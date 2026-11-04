open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module D = Document_schema
module S = Session.Moderator_state.Identity_snapshot
module C = A.Moderator_checkpoint
module Q = A.Moderator_queue
module SD = A.Session_state_document
module DD = A.Session_delta_document
module Persistence = A.Session_persistence
module Store = Agent_store

let member = Goal_catalog_tests.member

let replace json name value =
  match json with
  | `Object fields ->
    assert (List.Assoc.mem fields name ~equal:String.equal);
    `Object
      (List.map fields ~f:(fun (key, previous) ->
         key, if String.equal key name then value else previous))
  | _ -> assert false
;;

let extend json name value =
  match json with
  | `Object fields ->
    assert (not (List.Assoc.mem fields name ~equal:String.equal));
    `Object (fields @ [ name, value ])
  | _ -> assert false
;;

let remove json name =
  match json with
  | `Object fields ->
    `Object (List.filter fields ~f:(fun (key, _) -> not (String.equal key name)))
  | _ -> assert false
;;

let assert_raw expected actual =
  if not (Jsonaf.exactly_equal expected actual)
  then
    raise_s
      [%sexp "checkpoint raw tree differs", (expected : Jsonaf.t), (actual : Jsonaf.t)]
;;

let identity (snapshot : S.t) =
  extend (S.to_jsonaf snapshot) "future_identity" (`Number "1.00")
;;

let annotated snapshot queue =
  let rows =
    match member queue "entries" with
    | `Array (head :: tail) ->
      `Array (extend head "future_ordinal" (`Number "-0.0") :: tail)
    | _ -> assert false
  in
  let queue =
    replace queue "entries" rows
    |> fun queue -> extend queue "future_queue" (`Number "1e+02")
  in
  `Object
    [ "future_checkpoint", `String "retained first"
    ; "identity_snapshot", identity snapshot
    ; "queue_provenance", queue
    ]
;;

let snapshot : S.t =
  { script_id = "queue-test"
  ; script_source_hash = String.make 64 'a'
  ; current_state = Session.Snapshot.String "before"
  ; queued_internal_events = Moderator_queue_tests.values 2
  ; halted = false
  ; revision = 0
  ; next_change_id = 0
  ; prepended_items = []
  ; appended_items = []
  ; replacements = []
  ; tombstones = []
  ; halted_reason = None
  }
;;

let appended snapshot metadata =
  let after = snapshot.S.queued_internal_events @ [ Session.Snapshot.String "new" ] in
  let queue =
    Q.advance
      metadata
      ~before:snapshot.queued_internal_events
      ~after
      ~consume:Keep
      ~appended:[ Q.Capture.unavailable ]
    |> protocol_ok
  in
  ( { snapshot with queued_internal_events = after; revision = snapshot.revision + 1 }
  , queue )
;;

let%expect_test "complete checkpoint updates preserve raw custody" =
  let queue =
    Q.bootstrap
      ~source:Moderator_queue_tests.source
      ~queue:snapshot.queued_internal_events
    |> protocol_ok
  in
  let original = annotated snapshot (Q.to_jsonaf queue) in
  let after, next = appended snapshot queue in
  let expected = annotated after (Q.to_jsonaf next) in
  let actual =
    C.update_with_queue (Some original) ~snapshot:after ~queue:next |> protocol_ok
  in
  assert_raw expected actual;
  C.validate_update ~previous:(Some original) (Some actual) |> protocol_ok;
  let enriched_queue =
    extend (member actual "queue_provenance") "future_new" (`Number "2.50")
  in
  let enriched = replace actual "queue_provenance" enriched_queue in
  C.validate_update ~previous:(Some actual) (Some enriched) |> protocol_ok;
  C.of_jsonaf enriched |> protocol_ok |> C.to_jsonaf |> assert_raw enriched;
  List.iter [ None; Some `Null ] ~f:(fun queue_field ->
    let bare =
      `Object
        [ "future_checkpoint", `String "retained first"
        ; "identity_snapshot", identity snapshot
        ]
    in
    let bare =
      Option.value_map queue_field ~default:bare ~f:(fun value ->
        extend bare "queue_provenance" value)
    in
    let same_queue =
      C.of_jsonaf bare
      |> protocol_ok
      |> fun checkpoint ->
      C.update checkpoint ~snapshot ~queue:Keep |> protocol_ok |> C.to_jsonaf
    in
    assert_raw bare same_queue;
    let adopted = C.update_with_queue (Some bare) ~snapshot ~queue |> protocol_ok in
    let expected =
      match queue_field with
      | None -> extend bare "queue_provenance" (Q.to_jsonaf queue)
      | Some _ -> replace bare "queue_provenance" (Q.to_jsonaf queue)
    in
    assert_raw expected adopted);
  let reject incoming =
    assert (Result.is_error (C.validate_update ~previous:(Some original) incoming))
  in
  reject None;
  reject (Some (remove original "future_checkpoint"));
  reject (Some (replace original "future_checkpoint" (`String "changed")));
  let foreign = { snapshot with script_source_hash = String.make 64 'b' } in
  let raw_queue = member original "queue_provenance" in
  let foreign_observer =
    replace
      (member raw_queue "source")
      "source_sha256"
      (`String foreign.script_source_hash)
  in
  let foreign_checkpoint =
    replace original "identity_snapshot" (identity foreign)
    |> fun raw ->
    replace raw "queue_provenance" (replace raw_queue "source" foreign_observer)
  in
  C.of_jsonaf foreign_checkpoint |> protocol_ok |> ignore;
  reject (Some foreign_checkpoint);
  let foreign_source =
    replace (member raw_queue "source") "installation_epoch" (`String "2")
  in
  let foreign_epoch =
    replace original "queue_provenance" (replace raw_queue "source" foreign_source)
  in
  C.of_jsonaf foreign_epoch |> protocol_ok |> ignore;
  reject (Some foreign_epoch);
  let retargeted_rows =
    match member raw_queue "entries" with
    | `Array [ first; second ] ->
      `Array
        [ remove first "future_ordinal"; extend second "future_ordinal" (`Number "-0.0") ]
    | _ -> assert false
  in
  let retargeted =
    replace original "queue_provenance" (replace raw_queue "entries" retargeted_rows)
  in
  C.of_jsonaf retargeted |> protocol_ok |> ignore;
  reject (Some retargeted);
  let consumed =
    Q.advance
      queue
      ~before:snapshot.queued_internal_events
      ~after:(List.tl_exn snapshot.queued_internal_events)
      ~consume:Consume_head
      ~appended:[]
    |> protocol_ok
  in
  assert (
    Result.is_error
      (C.update_with_queue
         (Some original)
         ~snapshot:
           { snapshot with
             queued_internal_events = List.tl_exn snapshot.queued_internal_events
           }
         ~queue:consumed));
  (* Depth is a cheap exact raw-profile boundary. The internal nullable wrapper
     adds two levels; it must neither steal a raw level nor increase raw quota. *)
  let base = C.encode snapshot in
  let rec nest count json =
    if count = 0 then json else nest (count - 1) (`Array [ json ])
  in
  let boundary =
    extend base "future_depth" (nest (D.Limits.max_depth D.Limits.default - 2) `Null)
  in
  D.Json.validate ~limits:D.Limits.default boundary |> document_ok;
  C.of_jsonaf boundary |> protocol_ok |> C.to_jsonaf |> assert_raw boundary;
  let too_deep =
    extend base "future_depth" (nest (D.Limits.max_depth D.Limits.default - 1) `Null)
  in
  assert (Result.is_error (D.Json.validate ~limits:D.Limits.default too_deep));
  assert (Result.is_error (C.of_jsonaf too_deep));
  print_endline
    "exact outer/identity/ordinal custody, presence, bootstrap and raw depth; \
     incompatible changes reject";
  [%expect
    {| exact outer/identity/ordinal custody, presence, bootstrap and raw depth; incompatible changes reject |}]
;;

let moderator (state : A.Session_state.t) = Option.value_exn state.moderator

let seed state =
  let restored = Moderator_queue_persistence_tests.captured state in
  let document =
    Persistence.Restored.state_document restored
    |> SD.encode ~limits:document_limits
    |> document_ok
  in
  let raw = D.Document.json document in
  let payload = member raw "payload" in
  let checkpoint = member payload "moderator" in
  let checkpoint =
    extend checkpoint "future_checkpoint" (`String "actual native and disk")
    |> fun raw ->
    replace
      raw
      "identity_snapshot"
      (extend (member raw "identity_snapshot") "future_identity" (`Number "1.00"))
    |> fun raw ->
    replace
      raw
      "queue_provenance"
      (extend (member raw "queue_provenance") "future_queue" (`Number "1e+02"))
  in
  replace raw "payload" (replace payload "moderator" checkpoint)
  |> D.Document.inspect ~limits:document_limits
  |> document_ok
  |> SD.decode ~limits:document_limits
  |> document_ok
  |> Persistence.Restored.of_state_document
;;

let next_checkpoint before =
  let original = moderator before in
  let identity = C.decode before.moderator |> protocol_ok |> Option.value_exn in
  let after =
    { identity with
      current_state = Session.Snapshot.String "after"
    ; revision = identity.revision + 1
    }
  in
  let queue = C.queue before.moderator |> protocol_ok |> Option.value_exn in
  let incoming =
    C.update_with_queue before.moderator ~snapshot:after ~queue |> protocol_ok
  in
  let expected_identity =
    replace
      (member original "identity_snapshot")
      "current_state"
      (Session.Snapshot.to_jsonaf after.current_state)
    |> fun raw -> replace raw "revision" (`String (Int.to_string after.revision))
  in
  let expected =
    replace original "identity_snapshot" expected_identity
    |> fun raw ->
    replace
      raw
      "queue_provenance"
      (extend (member raw "queue_provenance") "future_added" (`Number "2.50"))
  in
  let incoming =
    replace
      incoming
      "queue_provenance"
      (extend (member incoming "queue_provenance") "future_added" (`Number "2.50"))
  in
  assert_raw expected incoming;
  expected, incoming
;;

let assert_native env workspace before incoming =
  Job_fixtures.with_actor
    ~environment:(env, workspace)
    ~start:false
    ~prepare_state:(fun _ -> before)
    (fun _ _ actor _ backend ->
       A.Session_actor.change_moderator actor (Some incoming) |> protocol_ok |> ignore;
       let actor_state = A.Session_actor.state actor |> protocol_ok in
       assert_raw incoming (moderator actor_state);
       assert_raw incoming (moderator (A.Memory_backend.state backend));
       let revision = actor_state.counters.revision in
       assert (
         Result.is_error
           (A.Session_actor.change_moderator
              actor
              (Some (remove incoming "future_checkpoint"))));
       let unchanged = A.Session_actor.state actor |> protocol_ok in
       [%test_eq: int64] unchanged.counters.revision revision;
       assert_raw incoming (moderator unchanged))
;;

let assert_documents initial before incoming =
  let previous = Persistence.Restored.state_document initial in
  let delta = A.Session_delta.Moderator_changed (Some incoming) in
  let transition =
    A.Session_transition.apply ~now:timestamp before ~delta ~payloads:[] |> protocol_ok
  in
  let decoded =
    SD.authored transition.state
    |> SD.encode ~limits:document_limits
    |> document_ok
    |> SD.decode ~limits:document_limits
    |> document_ok
  in
  let adopted = SD.adopt previous ~limits:document_limits decoded |> document_ok in
  assert_raw incoming (moderator (SD.value adopted));
  let delta_document =
    DD.create delta ~limits:document_limits ~state_document:SD.authored
    |> document_ok
    |> DD.document
  in
  let decoded_delta = DD.decode ~limits:document_limits delta_document |> document_ok in
  (match DD.value decoded_delta with
   | Moderator_changed checkpoint | Batch [ Moderator_changed checkpoint ] ->
     assert_raw incoming (Option.value_exn checkpoint)
   | _ -> assert false);
  let applied = DD.apply decoded_delta ~limits:document_limits previous |> document_ok in
  assert_raw incoming (moderator (SD.value applied));
  let stripped = remove incoming "future_checkpoint" in
  let bad_state = { transition.state with moderator = Some stripped } in
  (* Valid standalone DATA cannot bypass the previous carrier's custody law. *)
  SD.authored bad_state |> SD.encode ~limits:document_limits |> document_ok |> ignore;
  assert (
    Result.is_error (SD.encode (SD.with_value previous bad_state) ~limits:document_limits));
  assert (
    Result.is_error (SD.adopt previous ~limits:document_limits (SD.authored bad_state)));
  let stripped_delta =
    DD.create
      (Moderator_changed (Some stripped))
      ~limits:document_limits
      ~state_document:SD.authored
    |> document_ok
    |> DD.document
    |> DD.decode ~limits:document_limits
    |> document_ok
  in
  assert (Result.is_error (DD.apply stripped_delta ~limits:document_limits previous));
  transition
;;

let assert_journal
      env
      (workspace : A.Workspace_instance.t)
      initial
      before
      transition
      expected
  =
  let directory =
    Filename.concat workspace.canonical_root.native_path "checkpoint-journal"
  in
  let snapshots =
    Filename.concat workspace.canonical_root.native_path "checkpoint-snapshots"
  in
  Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / snapshots);
  let journal open_ =
    open_
      ~env
      ~directory
      ~max_payload_length:1048576
      ~max_segment_bytes:4194304L
      ~max_segment_frames:16
    |> store_ok
  in
  Eio.Switch.run (fun sw ->
    let writer =
      Store.Commit_writer.create
        ~sw
        ~journal:(journal Store.Journal.create)
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
            ~archive:(fun _ _ -> failwith "unexpected archive")
            ~pending_archive:None
            ~before_commit:None
            ~command_accepted:(fun _ _ -> ())
            ~writer
            ~durability:Flush
            ~limits:document_limits
            ~archive_limits:document_limits
            ~retention_preflight:None
            ~restored:initial
            ~previous_transaction_hash:None
        in
        Persistence.commit persistence ~command_audit:None ~previous:before transition
        |> protocol_ok;
        let restored = Persistence.restored persistence in
        assert_raw expected (moderator (Persistence.Restored.state restored));
        let encoded =
          Persistence.Restored.state_document restored
          |> SD.encode ~limits:document_limits
          |> document_ok
        in
        assert_raw expected (member (D.Document.payload encoded) "moderator");
        (* Deliberately malformed native data, not a receipt or successful
           admission: persistence must reject it before appending a frame. *)
        let previous = Persistence.Restored.state restored in
        let stripped = remove expected "future_checkpoint" in
        let state =
          { previous with
            moderator = Some stripped
          ; counters =
              { previous.counters with
                revision = Int64.succ previous.counters.revision
              ; transaction_sequence = Int64.succ previous.counters.transaction_sequence
              }
          }
        in
        let malformed : A.Session_transition.t =
          { state; delta = Moderator_changed (Some stripped); events = [] }
        in
        (match Persistence.commit persistence ~command_audit:None ~previous malformed with
         | Ok () -> failwith "checkpoint deletion was persisted"
         | Error failure ->
           assert (String.is_substring failure.message ~substring:"checkpoint"));
        assert_raw
          expected
          (moderator (Persistence.Restored.state (Persistence.restored persistence)))));
  (* The original initial basis and the same actual journal are reopened only
     after Commit_writer.close and its switch have both completed. *)
  let recovered =
    Store.Recovery.load
      ~env
      ~journal:(journal Store.Journal.open_existing)
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
  assert_raw expected (moderator (Persistence.Restored.state recovered.state));
  let encoded =
    Persistence.Restored.state_document recovered.state
    |> SD.encode ~limits:document_limits
    |> document_ok
  in
  assert_raw expected (member (D.Document.payload encoded) "moderator");
  [%test_eq: int64]
    (Persistence.Restored.state recovered.state).counters.transaction_sequence
    1L
;;

let%expect_test "complete checkpoint agrees across native memory and physical replay" =
  Run_admission_tests.with_startup_actor
    (fun actor _ _ request start _ _ _ _ ->
       start (request "checkpoint-carrier") |> protocol_ok |> ignore;
       A.Session_actor.state actor |> protocol_ok |> seed)
    ~after_close:(fun env workspace initial ->
      let before = Persistence.Restored.state initial in
      let expected, incoming = next_checkpoint before in
      assert_raw
        (member
           (D.Document.payload
              (Persistence.Restored.state_document initial
               |> SD.encode ~limits:document_limits
               |> document_ok))
           "moderator")
        (moderator before);
      assert_native env workspace before incoming;
      let transition = assert_documents initial before incoming in
      assert_journal env workspace initial before transition expected);
  print_endline
    "Actor/Memory, State/Delta and actual closed/reopened Journal retain the same \
     complete raw checkpoint";
  [%expect
    {| Actor/Memory, State/Delta and actual closed/reopened Journal retain the same complete raw checkpoint |}]
;;
