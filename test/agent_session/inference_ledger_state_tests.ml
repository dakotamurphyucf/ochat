open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module D = Document_schema
module L = A.Inference_ledger
module O = Inference.Observation
module SD = A.Session_state_document
module DD = A.Session_delta_document

let ledger_ok result =
  Result.map_error result ~f:(fun error -> Sexp.to_string_hum (L.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let observation_ok result =
  Result.map_error result ~f:(fun error -> Sexp.to_string_hum (O.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let member json name =
  match D.Json.field json ~name with
  | Value value -> value
  | Absent | Null -> failwith "missing test member"
;;

let replace json name value =
  match json with
  | `Object fields ->
    `Object
      (List.map fields ~f:(fun (key, old) ->
         key, if String.equal key name then value else old))
  | _ -> failwith "expected test object"
;;

let omit json name =
  match json with
  | `Object fields ->
    `Object (List.filter fields ~f:(fun (key, _) -> not (String.equal key name)))
  | _ -> failwith "expected test object"
;;

let add json name value =
  match json with
  | `Object fields -> `Object (fields @ [ name, value ])
  | _ -> failwith "expected test object"
;;

let ledger_json ledger = D.Document.json (L.to_document ledger |> ledger_ok)
let ledger_bytes ledger = Jsonaf.to_string (ledger_json ledger)

let state workspace =
  actor_state ~workspace_instance:workspace ~liveness:Detached ~start_immediately:false
;;

let raw_state before ~version ~payload =
  ignore (before : A.Session_state.t);
  D.Document.create ~limits:document_limits ~kind:"session.state" ~version ~payload
  |> document_ok
;;

let admit state ledger =
  let target =
    match Inference.Selection.view state.A.Session_state.spec.inference_target with
    | Captured target -> target
    | Unresolved -> failwith "fixture selection must be explicit"
  in
  let configuration =
    O.Configuration.of_target
      target
      ~preparation_id:"physical-preparation"
      ~transport:Http_sse
      ~capabilities:[]
      ~limits:O.Admission.observation
    |> observation_ok
  in
  L.admit
    ledger
    ~source:(Transcript.Source_id.of_string "physical-graph" |> Result.ok_or_failwith)
    ~relation:Root
    ~operation_id:None
    ~invocation_id:None
    ~configuration
  |> ledger_ok
;;

let%test_unit
    "fresh ledger is known empty; absent v1/v2 ledger migrates unknown without changing \
     stored input"
  =
  with_actor_workspace (fun _ workspace ->
    let before = state workspace in
    assert (
      not
        (P.Inference_query.Summary.coverage (L.summary before.inference_ledger))
          .before_tracking_unknown);
    let payload =
      D.Document.payload (state_document before)
      |> fun payload -> omit payload "inference_ledger"
    in
    List.iter [ 1; 2 ] ~f:(fun version ->
      let raw = raw_state before ~version ~payload in
      let original = D.Document.to_string raw in
      let decoded = SD.decode ~limits:document_limits raw |> document_ok in
      let current = SD.value decoded in
      assert
        (P.Inference_query.Summary.coverage (L.summary current.inference_ledger))
          .before_tracking_unknown;
      assert (List.is_empty (L.rows current.inference_ledger));
      assert (Int.equal (L.generation current.inference_ledger) before.identity.generation);
      let encoded = SD.encode decoded ~limits:document_limits |> document_ok in
      assert (Int.equal (D.Document.version encoded) 3);
      assert (String.equal original (D.Document.to_string raw));
      let delta =
        D.Document.create
          ~limits:document_limits
          ~kind:"session.delta"
          ~version
          ~payload:
            (`Object
                [ ( "changes"
                  , `Array
                      [ `Object
                          [ "kind", `String "created"; "state", D.Document.json raw ]
                      ] )
                ])
        |> document_ok
      in
      let delta_before = D.Document.to_string delta in
      let delta = DD.decode ~limits:document_limits delta |> document_ok in
      assert (Int.equal (D.Document.version (DD.document delta)) 3);
      match DD.value delta with
      | Batch [ Created restored ] ->
        assert
          (P.Inference_query.Summary.coverage (L.summary restored.inference_ledger))
            .before_tracking_unknown
      | _ -> failwith ("unexpected converted delta " ^ delta_before)))
;;

let%test_unit
    "conversion preserves existing child unknowns and rejects same-name collisions"
  =
  with_actor_workspace (fun _ workspace ->
    let before = state workspace in
    let child =
      ledger_json before.inference_ledger
      |> fun json -> add json "future_envelope" (`Number "1e+00")
    in
    let child =
      replace
        child
        "payload"
        (member child "payload"
         |> fun json -> add json "future_ledger" (`Object [ "empty", `Null ]))
    in
    let payload =
      D.Document.payload (state_document before)
      |> fun json -> replace json "inference_ledger" child
    in
    let raw = raw_state before ~version:2 ~payload in
    let decoded = SD.decode ~limits:document_limits raw |> document_ok in
    assert (
      String.equal
        (Jsonaf.to_string child)
        (ledger_bytes (SD.value decoded).inference_ledger));
    let encoded = SD.encode decoded ~limits:document_limits |> document_ok in
    assert (
      String.equal
        (Jsonaf.to_string child)
        (Jsonaf.to_string (member (D.Document.payload encoded) "inference_ledger")));
    List.iter
      [ `Null
      ; `Object []
      ; replace
          child
          "payload"
          (replace (member child "payload") "generation" (`Number "1"))
      ; replace
          child
          "payload"
          (replace
             (member child "payload")
             "session_id"
             (P.Id.Session.to_json (P.Id.Session.create ())))
      ]
      ~f:(fun invalid ->
        let payload = replace payload "inference_ledger" invalid in
        let raw = raw_state before ~version:2 ~payload in
        let original = D.Document.to_string raw in
        assert (Result.is_error (SD.decode ~limits:document_limits raw));
        assert (String.equal original (D.Document.to_string raw))))
;;

let%test_unit
    "ledger delta preserves complete child carrier and final native identity validation"
  =
  with_actor_workspace (fun _ workspace ->
    let before = state workspace in
    let active, handle, _ = admit before before.inference_ledger in
    let raw = ledger_json active in
    let raw =
      replace
        raw
        "payload"
        (member raw "payload"
         |> fun json -> add json "future_counter_policy" (`Number "1e+00"))
    in
    let captured =
      L.of_document
        (D.Document.inspect ~limits:document_limits raw |> document_ok)
        ~limits:L.Limits.default
      |> ledger_ok
    in
    let closed =
      L.set_state
        captured
        handle
        (Interrupted { reason = Host_interrupted; delivery = Definitely_not_submitted })
      |> ledger_ok
    in
    let delta =
      DD.create
        (Inference_ledger_changed closed)
        ~limits:document_limits
        ~state_document:SD.authored
      |> document_ok
    in
    let replayed =
      DD.apply delta ~limits:document_limits (SD.authored before) |> document_ok
    in
    let live =
      A.Session_delta.apply before (Inference_ledger_changed closed) |> protocol_ok
    in
    assert (
      String.equal
        (ledger_bytes closed)
        (ledger_bytes (SD.value replayed).inference_ledger));
    assert (
      String.equal
        (ledger_bytes live.inference_ledger)
        (ledger_bytes (SD.value replayed).inference_ledger));
    let advanced = L.with_generation closed ~generation:1 |> ledger_ok in
    assert (
      Result.is_error (A.Session_delta.apply before (Inference_ledger_changed advanced)));
    assert (
      Result.is_error
        (SD.encode
           (SD.authored { before with inference_ledger = advanced })
           ~limits:document_limits));
    assert (
      String.is_substring
        (ledger_bytes closed)
        ~substring:"\"future_counter_policy\":1e+00"))
;;

let%test_unit
    "administration plan retains active ledger; durable generation waits for actual \
     interruption"
  =
  with_actor_workspace (fun _ workspace ->
    let before = state workspace in
    let retained =
      ledger_json before.inference_ledger
      |> fun json ->
      add json "future_administration" (`Number "1e+00")
      |> fun json ->
      D.Document.inspect ~limits:document_limits json
      |> document_ok
      |> fun document -> L.of_document document ~limits:L.Limits.default |> ledger_ok
    in
    let before = { before with inference_ledger = retained } in
    let active, handle, _ = admit before before.inference_ledger in
    let before = { before with inference_ledger = active } in
    A.Session_state.validate before |> protocol_ok;
    let options =
      A.Administration.
        { keep_history = true
        ; keep_tasks = true
        ; keep_grants = true
        ; keep_labels = true
        ; workspace_instance = None
        }
    in
    let revision = P.Id.Prompt_revision.create () in
    assert (Result.is_error (A.Administration.reset before options));
    assert (Result.is_error (A.Administration.rebuild before revision));
    let candidate = A.Administration.plan_reset before options |> protocol_ok in
    assert (String.equal (ledger_bytes active) (ledger_bytes candidate.inference_ledger));
    A.Session_state.validate_administration_candidate candidate ~previous:before
    |> protocol_ok;
    assert (Result.is_error (A.Session_state.validate candidate));
    assert (Result.is_error (SD.encode (SD.authored candidate) ~limits:document_limits));
    let rebuild_candidate =
      A.Administration.plan_rebuild before revision |> protocol_ok
    in
    assert (
      String.equal (ledger_bytes active) (ledger_bytes rebuild_candidate.inference_ledger));
    A.Session_state.validate_administration_candidate rebuild_candidate ~previous:before
    |> protocol_ok;
    assert (Result.is_error (A.Session_state.validate rebuild_candidate));
    assert (Result.is_error (A.Session_delta.apply before (Reset_generation 1)));
    let closed =
      L.set_state
        active
        handle
        (Interrupted { reason = Host_interrupted; delivery = Possibly_submitted })
      |> ledger_ok
    in
    let current = { before with inference_ledger = closed } in
    let advanced = L.with_generation closed ~generation:1 |> ledger_ok in
    List.iter
      [ A.Administration.reset current options |> protocol_ok
      ; A.Administration.rebuild current revision |> protocol_ok
      ]
      ~f:(fun replacement ->
        A.Session_state.validate replacement |> protocol_ok;
        ignore (SD.encode (SD.authored replacement) ~limits:document_limits |> document_ok);
        assert (
          String.equal (ledger_bytes advanced) (ledger_bytes replacement.inference_ledger));
        assert (
          Int64.equal
            (L.revision replacement.inference_ledger)
            (Int64.succ (L.revision closed)));
        assert (
          Sexp.equal
            (A.Session_state.Counters.sexp_of_t current.counters)
            (A.Session_state.Counters.sexp_of_t replacement.counters));
        let row =
          L.find replacement.inference_ledger ~ordinal:(L.Handle.ordinal handle)
          |> Option.value_exn
        in
        assert (L.Handle.equal handle (L.Row.handle row));
        assert (
          String.is_substring
            (ledger_bytes replacement.inference_ledger)
            ~substring:"\"future_administration\":1e+00"));
    assert (
      String.equal
        (ledger_bytes closed)
        (ledger_bytes (A.Administration.upgrade current revision).inference_ledger));
    let reset = A.Session_delta.apply current (Reset_generation 1) |> protocol_ok in
    A.Session_state.validate reset |> protocol_ok;
    assert (List.length (L.rows reset.inference_ledger) = 1);
    let _, next, _ = admit reset reset.inference_ledger in
    assert (Int64.equal (L.Handle.ordinal next) (Int64.succ (L.Handle.ordinal handle)));
    assert (Int.equal (L.Handle.generation handle) 0);
    assert (Int.equal (L.Handle.generation next) 1);
    assert (
      Result.is_error
        (A.Session_state.validate_administration_candidate
           { candidate with inference_ledger = closed }
           ~previous:before)))
;;

let%test_unit
    "captured ledger cannot be replaced by an authored child or erased through Created \
     or with_value"
  =
  with_actor_workspace (fun _ workspace ->
    let before = state workspace in
    let raw =
      ledger_json before.inference_ledger
      |> fun json -> add json "future_root" (`Number "1e+00")
    in
    let ledger =
      L.of_document
        (D.Document.inspect ~limits:document_limits raw |> document_ok)
        ~limits:L.Limits.default
      |> ledger_ok
    in
    let before = { before with inference_ledger = ledger } in
    let captured =
      SD.decode ~limits:document_limits (state_document before) |> document_ok
    in
    let empty =
      L.create
        ~session_id:before.identity.session_id
        ~generation:before.identity.generation
        ~before_tracking_unknown:false
        ~limits:L.Limits.default
      |> ledger_ok
    in
    let erased = { before with inference_ledger = empty } in
    assert (Result.is_error (L.validate_update ledger ~incoming:erased.inference_ledger));
    assert (
      Result.is_error
        (A.Session_delta.apply before (Inference_ledger_changed erased.inference_ledger)));
    assert (Result.is_error (A.Session_delta.apply before (Created erased)));
    assert (
      Result.is_error (SD.encode (SD.with_value captured erased) ~limits:document_limits));
    assert (
      Result.is_error (SD.adopt captured ~limits:document_limits (SD.authored erased)));
    assert (
      Result.is_error
        (SD.adopt
           (SD.authored erased)
           ~limits:document_limits
           (SD.with_value captured erased)));
    let active, handle, _ = admit before ledger in
    L.validate_update ledger ~incoming:active |> ledger_ok;
    let closed =
      L.set_state
        active
        handle
        (Interrupted { reason = Host_interrupted; delivery = Definitely_not_submitted })
      |> ledger_ok
    in
    L.validate_update active ~incoming:closed |> ledger_ok;
    let advanced = L.with_generation closed ~generation:1 |> ledger_ok in
    L.validate_update closed ~incoming:advanced |> ledger_ok;
    assert (Result.is_error (L.validate_update advanced ~incoming:closed));
    assert (String.is_substring (ledger_bytes advanced) ~substring:"\"future_root\":1e+00"))
;;

let%test_unit "durable state refuses a ledger admitted under a looser byte profile" =
  with_actor_workspace (fun _ workspace ->
    let before = state workspace in
    let large_limits =
      A.Persistence_codec.limits ~max_bytes:(8 * 1024 * 1024) |> document_ok
    in
    let ledger_limits =
      L.Limits.create
        ~max_attempts:256
        ~max_turns:256
        ~max_retained_bytes:(6 * 1024 * 1024)
        ~document_limits:large_limits
      |> ledger_ok
    in
    let raw =
      ledger_json before.inference_ledger
      |> fun json -> add json "future_large" (`String (String.make (4 * 1024 * 1024) 'x'))
    in
    let ledger =
      L.of_document
        (D.Document.inspect ~limits:large_limits raw |> document_ok)
        ~limits:ledger_limits
      |> ledger_ok
    in
    assert (
      Result.is_error (A.Session_state.validate { before with inference_ledger = ledger }));
    assert (
      Result.is_error
        (SD.encode
           (SD.authored { before with inference_ledger = ledger })
           ~limits:large_limits));
    ())
;;

let%test_unit
    "legitimate bounded retirement preserves cumulative coverage and cannot resurrect an \
     ordinal"
  =
  with_actor_workspace (fun _ workspace ->
    let before = state workspace in
    let limits =
      L.Limits.create
        ~max_attempts:1
        ~max_turns:1
        ~max_retained_bytes:(256 * 1024)
        ~document_limits
      |> ledger_ok
    in
    let initial =
      L.create
        ~session_id:before.identity.session_id
        ~generation:0
        ~before_tracking_unknown:false
        ~limits
      |> ledger_ok
    in
    let active, first, _ = admit before initial in
    let ended =
      L.set_state
        active
        first
        (Interrupted { reason = Host_interrupted; delivery = Definitely_not_submitted })
      |> ledger_ok
    in
    let next, second, _ = admit before ended in
    L.validate_update ended ~incoming:next |> ledger_ok;
    let reclassified = ledger_json next in
    let payload = member reclassified "payload" in
    let coverage =
      member payload "coverage"
      |> fun json ->
      replace json "retired_attempts" (`String "0")
      |> fun json -> replace json "untracked_attempts" (`String "1")
    in
    let reclassified =
      replace reclassified "payload" (replace payload "coverage" coverage)
    in
    let reclassified =
      L.of_document
        (D.Document.inspect ~limits:document_limits reclassified |> document_ok)
        ~limits:L.Limits.default
      |> ledger_ok
    in
    assert (Result.is_error (L.validate_update ended ~incoming:reclassified));
    assert (Option.is_none (L.find next ~ordinal:(L.Handle.ordinal first)));
    assert (Int64.equal (L.Handle.ordinal second) 2L);
    assert (
      Int64.equal
        (P.Inference_query.Summary.coverage (L.summary next)).retired_attempts
        1L);
    assert (Result.is_error (L.validate_update next ~incoming:ended)))
;;

let%test_unit
    "ledger child survives actual writer acknowledgement, frame replay and snapshot \
     reopen"
  =
  let module Store = Agent_store in
  let module Persistence = A.Session_persistence in
  with_actor_workspace (fun env workspace ->
    Eio.Switch.run (fun sw ->
      let initial = state workspace in
      let raw =
        ledger_json initial.inference_ledger
        |> fun json -> add json "future_physical" (`Number "1e+00")
      in
      let ledger =
        L.of_document
          (D.Document.inspect ~limits:document_limits raw |> document_ok)
          ~limits:L.Limits.default
        |> ledger_ok
      in
      let initial = { initial with inference_ledger = ledger } in
      let storage = Job_artifact_fixtures.create env sw initial in
      let handle = storage.session in
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
          ~session_id:initial.identity.session_id
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
              ~retention_preflight:None
              ~writer
              ~durability:Flush
              ~limits:document_limits
              ~archive_limits:document_limits
              ~restored:(Persistence.Restored.authored initial)
              ~previous_transaction_hash:None
              ~command_accepted:(fun _ _ -> ())
              ~archive:(fun _ _ -> failwith "ledger update cannot archive conversation")
          in
          let installed =
            Persistence.install_snapshot
              persistence
              ~env
              ~handle
              ~max_payload_length:1048576
              ~transaction_hash:None
              initial
            |> store_ok
          in
          let seed =
            Persistence.restore_snapshot ~limits:document_limits installed.snapshot
            |> store_ok
          in
          let active, attempt, _ = admit initial ledger in
          let closed =
            L.set_state
              active
              attempt
              (Interrupted
                 { reason = Host_interrupted; delivery = Definitely_not_submitted })
            |> ledger_ok
          in
          let current = ref initial in
          List.iter [ active; closed ] ~f:(fun inference_ledger ->
            let transition =
              A.Session_transition.apply
                ~now:timestamp
                !current
                ~delta:(Inference_ledger_changed inference_ledger)
                ~payloads:[]
              |> protocol_ok
            in
            Persistence.commit
              persistence
              ~command_audit:None
              ~previous:!current
              transition
            |> protocol_ok;
            current := transition.state);
          let scan = Store.Journal.scan journal |> store_ok in
          assert (List.length scan.entries = 2);
          let replayed =
            List.fold scan.entries ~init:seed ~f:(fun previous entry ->
              let record =
                Store.Document_record.of_frame
                  entry.frame
                  ~limits:document_limits
                  ~expected_digest:None
                |> Result.map_error ~f:(fun error ->
                  Sexp.to_string_hum (Store.Document_record.Error.sexp_of_t error))
                |> Result.ok_or_failwith
              in
              let transaction =
                Store.Transaction.decode_record record ~limits:document_limits |> store_ok
              in
              assert (Int.equal (D.Document.version transaction.delta) 3);
              Persistence.apply_transaction ~limits:document_limits previous transaction
              |> store_ok)
          in
          assert (
            String.equal
              (ledger_bytes closed)
              (ledger_bytes (Persistence.Restored.state replayed).inference_ledger));
          let installed =
            Persistence.install_snapshot
              persistence
              ~env
              ~handle
              ~max_payload_length:1048576
              ~transaction_hash:(Persistence.transaction_hash persistence)
              !current
            |> store_ok
          in
          assert (Int.equal (D.Document.version installed.snapshot.payload) 3);
          let opened =
            Store.Snapshot.load_current
              ~env
              ~directory:(Store.Session_store.Handle.snapshot_directory handle)
              ~max_payload_length:1048576
            |> store_ok
            |> Option.value_exn
          in
          let reopened =
            Persistence.restore_snapshot ~limits:document_limits opened.snapshot
            |> store_ok
          in
          assert (
            String.equal
              (ledger_bytes closed)
              (ledger_bytes (Persistence.Restored.state reopened).inference_ledger));
          assert (
            String.equal (ledger_bytes ledger) (ledger_bytes initial.inference_ledger)))))
;;
