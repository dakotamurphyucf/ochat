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
      assert (Int.equal (D.Document.version encoded) 7);
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
      assert (Int.equal (D.Document.version (DD.document delta)) 4);
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

let%test_unit "state original ledger proof survives edits and binds the complete profile" =
  with_actor_workspace (fun _ workspace ->
    let before = state workspace in
    let raw_ledger =
      ledger_json before.inference_ledger
      |> fun json ->
      add json "future_root" (`Number "1e+00")
      |> fun json -> add json "future_nested" (`Object [ "text", `String "\195\169\n" ])
    in
    let ledger =
      L.of_document
        (D.Document.inspect ~limits:document_limits raw_ledger |> document_ok)
        ~limits:L.Limits.default
      |> ledger_ok
    in
    let original =
      state_document { before with inference_ledger = ledger }
      |> D.Document.json
      |> fun json ->
      add json "future_state" (`Number "1e+00")
      |> D.Document.inspect ~limits:document_limits
      |> document_ok
    in
    let captured = SD.decode ~limits:document_limits original |> document_ok in
    let current = SD.value captured in
    let changed =
      SD.with_value
        captured
        { current with
          counters =
            { current.counters with revision = Int64.succ current.counters.revision }
        }
    in
    let profile
          ?(max_bytes = 16 * 1024 * 1024)
          ?(max_depth = 128)
          ?(max_fields = 100_000)
          ?(max_nodes = 1_000_000)
          ()
      =
      D.Limits.create ~max_bytes ~max_depth ~max_fields ~max_nodes |> document_ok
    in
    let encoded = SD.encode changed ~limits:(profile ()) |> document_ok in
    let expected =
      D.Document.json original
      |> fun json ->
      replace
        json
        "payload"
        (let payload = member json "payload" in
         let counters = member payload "counters" in
         replace
           payload
           "counters"
           (replace
              counters
              "revision"
              (`String (Int64.to_string (SD.value changed).counters.revision))))
      |> Jsonaf.to_string
    in
    assert (String.equal (D.Document.to_string encoded) expected);
    assert (
      String.equal
        (D.Document.to_string
           (SD.encode changed ~limits:(profile ~max_depth:129 ()) |> document_ok))
        expected);
    List.iter
      [ "bytes", profile ~max_bytes:1 ()
      ; "depth", profile ~max_depth:1 ()
      ; "fields", profile ~max_fields:1 ()
      ; "nodes", profile ~max_nodes:1 ()
      ]
      ~f:(fun (bound, limits) ->
        let rejects = function
          | Error error -> assert (D.Error.equal error (Limit_exceeded bound))
          | Ok _ -> failwith "original profile bound was bypassed"
        in
        rejects (SD.encode changed ~limits);
        rejects (SD.adopt captured ~limits changed);
        rejects (SD.adopt (SD.authored before) ~limits changed));
    let erased =
      SD.with_value
        changed
        { (SD.value changed) with inference_ledger = before.inference_ledger }
    in
    assert (Result.is_error (SD.encode erased ~limits:(profile ())));
    assert (Result.is_error (SD.encode erased ~limits:(profile ~max_depth:129 ())));
    assert (Result.is_error (SD.adopt (SD.authored before) ~limits:(profile ()) erased));
    assert (
      String.equal
        (D.Document.to_string (SD.encode captured ~limits:document_limits |> document_ok))
        (D.Document.to_string original)))
;;

let%test_unit
    "reflexive ledger admission requires complete bytes rather than JSON equality"
  =
  with_actor_workspace (fun _ workspace ->
    let before = state workspace in
    let ledger =
      ledger_json before.inference_ledger
      |> fun json ->
      add json "future_first" (`Number "1e+00")
      |> fun json ->
      add json "future_second" `Null
      |> D.Document.inspect ~limits:document_limits
      |> document_ok
      |> fun document -> L.of_document document ~limits:L.Limits.default |> ledger_ok
    in
    L.validate_update ledger ~incoming:ledger |> ledger_ok;
    let copy =
      L.of_document (L.to_document ledger |> ledger_ok) ~limits:L.Limits.default
      |> ledger_ok
    in
    L.validate_update ledger ~incoming:copy |> ledger_ok;
    let reordered =
      match ledger_json ledger with
      | `Object fields ->
        `Object
          (List.filter fields ~f:(fun (name, _) -> not (String.equal name "future_first"))
           @ [ "future_first", `Number "1e+00" ])
      | _ -> failwith "expected ledger envelope"
    in
    assert (D.Json.equal reordered (ledger_json ledger));
    let reordered =
      D.Document.inspect ~limits:document_limits reordered
      |> document_ok
      |> fun document -> L.of_document document ~limits:L.Limits.default |> ledger_ok
    in
    assert (not (String.equal (ledger_bytes ledger) (ledger_bytes reordered)));
    assert (Result.is_error (L.validate_update ledger ~incoming:reordered));
    let wrong_session =
      L.create
        ~session_id:(P.Id.Session.create ())
        ~generation:before.identity.generation
        ~before_tracking_unknown:false
        ~limits:L.Limits.default
      |> ledger_ok
    in
    assert (Result.is_error (L.validate_update ledger ~incoming:wrong_session)))
;;

let%test_unit "immutable admission binds every profile bound and exact identity" =
  with_actor_workspace (fun _ workspace ->
    let before = state workspace in
    let document_profile
          ?(max_bytes = 4 * 1024 * 1024)
          ?(max_depth = 256)
          ?(max_fields = 1_000_000)
          ?(max_nodes = 2_000_000)
          ()
      =
      D.Limits.create ~max_bytes ~max_depth ~max_fields ~max_nodes |> document_ok
    in
    let profile
          ?(max_attempts = 256)
          ?(max_turns = 256)
          ?(max_retained_bytes = 4 * 1024 * 1024)
          ?(document_limits = document_profile ())
          ()
      =
      L.Limits.create ~max_attempts ~max_turns ~max_retained_bytes ~document_limits
      |> ledger_ok
    in
    let raw =
      ledger_json before.inference_ledger
      |> fun json ->
      add
        json
        "future_profile"
        (`Object
            [ "spelling", `Number "1e+00"
            ; "text", `String "\195\169\n"
            ; "padding", `String (String.make 4096 'x')
            ])
    in
    let original = D.Document.inspect ~limits:document_limits raw |> document_ok in
    let ledger = L.of_document original ~limits:(profile ()) |> ledger_ok in
    let first, first_handle, _ = admit before ledger in
    let second, _, _ = admit before first in
    let operation () =
      P.Operation.
        { id = P.Id.Operation.create ()
        ; generation = before.identity.generation
        ; kind = Turn User_submit
        ; state = Running
        ; started_at = before.identity.created_at
        ; updated_at = before.identity.updated_at
        }
    in
    let second, _, _ = L.admit_turn second (operation ()) |> ledger_ok in
    let second, _, _ = L.admit_turn second (operation ()) |> ledger_ok in
    let complete = ledger_bytes second in
    let validate limits =
      L.validate
        second
        ~limits
        ~session_id:before.identity.session_id
        ~generation:before.identity.generation
    in
    validate (profile ()) |> ledger_ok;
    assert (
      Result.is_error
        (L.validate
           second
           ~limits:(profile ())
           ~session_id:(P.Id.Session.create ())
           ~generation:before.identity.generation));
    assert (
      Result.is_error
        (L.validate
           second
           ~limits:(profile ())
           ~session_id:before.identity.session_id
           ~generation:(before.identity.generation + 1)));
    List.iter
      [ profile ~max_attempts:1 ()
      ; profile ~max_turns:1 ()
      ; profile ~max_retained_bytes:1024 ()
      ; profile ~document_limits:(document_profile ~max_bytes:128 ()) ()
      ; profile ~document_limits:(document_profile ~max_depth:2 ()) ()
      ; profile ~document_limits:(document_profile ~max_fields:1 ()) ()
      ; profile ~document_limits:(document_profile ~max_nodes:1 ()) ()
      ]
      ~f:(fun limits -> assert (Result.is_error (validate limits)));
    assert (
      Result.is_error
        (L.of_document original ~limits:(profile ~max_retained_bytes:1024 ())));
    let changed = L.set_state second first_handle Running |> ledger_ok in
    L.validate_update second ~incoming:changed |> ledger_ok;
    assert (String.equal complete (ledger_bytes second));
    assert (not (String.equal complete (ledger_bytes changed)));
    assert (
      String.is_substring
        (ledger_bytes changed)
        ~substring:
          "\"future_profile\":{\"spelling\":1e+00,\"text\":\"\195\169\\n\",\"padding\":");
    let first_row =
      L.find changed ~ordinal:(L.Handle.ordinal first_handle) |> Option.value_exn
    in
    match O.Attempt_record.state (L.Row.record first_row) with
    | Running -> ()
    | Prepared | Terminal _ | Interrupted _ -> assert false)
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

let%test_unit "failed retirement planning cannot publish an unadmitted candidate" =
  with_actor_workspace (fun _ workspace ->
    let before = state workspace in
    let close ledger handle =
      L.set_state
        ledger
        handle
        (Interrupted { reason = Host_interrupted; delivery = Definitely_not_submitted })
      |> ledger_ok
    in
    let first, first_handle, _ = admit before before.inference_ledger in
    let first = close first first_handle in
    let second, second_handle, _ = admit before first in
    let second = close second second_handle in
    let original = ledger_json second in
    let payload = member original "payload" in
    let rows =
      match member payload "rows" with
      | `Array [ first; second ] ->
        `Array [ first; add second "future_row" (`Number "1e+00") ]
      | _ -> failwith "expected two actual retained admissions"
    in
    let original =
      replace original "payload" (replace payload "rows" rows)
      |> D.Document.inspect ~limits:document_limits
      |> document_ok
    in
    let profile max_retained_bytes =
      L.Limits.create ~max_attempts:2 ~max_turns:256 ~max_retained_bytes ~document_limits
      |> ledger_ok
    in
    let fits bytes = Result.is_ok (L.of_document original ~limits:(profile bytes)) in
    let maximum = String.length (D.Document.to_string original) + 4096 in
    assert (fits maximum);
    let rec minimum lower upper =
      if Int.equal lower upper
      then lower
      else (
        let middle = lower + ((upper - lower) / 2) in
        if fits middle then minimum lower middle else minimum (middle + 1) upper)
    in
    let ledger =
      L.of_document original ~limits:(profile (minimum 1 maximum)) |> ledger_ok
    in
    let original_bytes = ledger_bytes ledger in
    let target =
      match Inference.Selection.view before.spec.inference_target with
      | Captured target -> target
      | Unresolved -> failwith "fixture selection must be explicit"
    in
    let configuration =
      O.Configuration.of_target
        target
        ~preparation_id:(String.make 500 'x')
        ~transport:Http_sse
        ~capabilities:[]
        ~limits:O.Admission.observation
      |> observation_ok
    in
    let next, handle, tracking =
      L.admit
        ledger
        ~source:(Transcript.Source_id.of_string "physical-graph" |> Result.ok_or_failwith)
        ~relation:Root
        ~operation_id:None
        ~invocation_id:None
        ~configuration
      |> ledger_ok
    in
    assert (L.equal_tracking tracking (Untracked Protected_future_data));
    assert (Int64.equal (L.Handle.ordinal handle) 3L);
    assert (List.length (L.rows next) = 2);
    assert (Option.is_some (L.find next ~ordinal:(L.Handle.ordinal first_handle)));
    assert (Option.is_some (L.find next ~ordinal:(L.Handle.ordinal second_handle)));
    assert (
      D.Json.equal
        (member (member (ledger_json ledger) "payload") "rows")
        (member (member (ledger_json next) "payload") "rows"));
    let coverage = P.Inference_query.Summary.coverage (L.summary next) in
    assert (Int64.equal coverage.retired_attempts 0L);
    assert (Int64.equal coverage.untracked_attempts 1L);
    assert (String.equal original_bytes (ledger_bytes ledger));
    assert (String.is_substring (ledger_bytes next) ~substring:"\"future_row\":1e+00");
    L.validate_update ledger ~incoming:next |> ledger_ok)
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
              ~before_commit:None
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
              assert (Int.equal (D.Document.version transaction.delta) 4);
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
          assert (Int.equal (D.Document.version installed.snapshot.payload) 7);
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

let%test_unit "validated ledger serializer rejects forged native host identities" =
  with_actor_workspace (fun _ workspace ->
    let before = state workspace in
    let original = ledger_bytes before.inference_ledger in
    let invalid_session = P.Id.Session.t_of_sexp (Sexp.Atom "invalid-session") in
    assert (
      Result.is_error
        (L.create
           ~session_id:invalid_session
           ~generation:0
           ~before_tracking_unknown:false
           ~limits:L.Limits.default));
    let target =
      match Inference.Selection.view before.spec.inference_target with
      | Captured target -> target
      | Unresolved -> assert false
    in
    let configuration =
      O.Configuration.of_target
        target
        ~preparation_id:"native-identity-check"
        ~transport:Http_sse
        ~capabilities:[]
        ~limits:O.Admission.observation
      |> observation_ok
    in
    let invalid_operation = P.Id.Operation.t_of_sexp (Sexp.Atom "invalid-operation") in
    let invalid_invocation = P.Id.Invocation.t_of_sexp (Sexp.Atom "invalid-invocation") in
    List.iter
      [ Some invalid_operation, None; None, Some invalid_invocation ]
      ~f:(fun (operation_id, invocation_id) ->
        assert (
          Result.is_error
            (L.admit
               before.inference_ledger
               ~source:
                 (Transcript.Source_id.of_string "native-identity-check"
                  |> Result.ok_or_failwith)
               ~relation:Root
               ~operation_id
               ~invocation_id
               ~configuration));
        assert (String.equal original (ledger_bytes before.inference_ledger)));
    let invalid_turn =
      P.Operation.
        { id = invalid_operation
        ; generation = before.identity.generation
        ; kind = Turn User_submit
        ; state = Starting
        ; started_at = timestamp
        ; updated_at = timestamp
        }
    in
    assert (Result.is_error (L.admit_turn before.inference_ledger invalid_turn)))
;;

let%test_unit
    "validated ledger serialization is faithful across complete native lifecycles"
  =
  with_actor_workspace (fun _ workspace ->
    let before = state workspace in
    let raw =
      ledger_json before.inference_ledger
      |> fun raw ->
      add
        raw
        "future_serializer"
        (`Object [ "literal", `Number "1e+00"; "text", `String "λ📚" ])
    in
    let ledger =
      D.Document.inspect ~limits:document_limits raw
      |> document_ok
      |> fun document -> L.of_document document ~limits:L.Limits.default |> ledger_ok
    in
    let roundtrip ledger =
      let document = L.to_document ledger |> ledger_ok in
      let restored = L.of_document document ~limits:L.Limits.default |> ledger_ok in
      assert (String.equal (D.Document.to_string document) (ledger_bytes restored));
      assert (
        List.equal
          L.Handle.equal
          (List.map (L.rows ledger) ~f:L.Row.handle)
          (List.map (L.rows restored) ~f:L.Row.handle));
      assert (
        List.equal
          (fun original restored ->
             String.equal
               (Jsonaf.to_string (O.Attempt_record.to_json (L.Row.record original)))
               (Jsonaf.to_string (O.Attempt_record.to_json (L.Row.record restored))))
          (L.rows ledger)
          (L.rows restored));
      assert (
        String.equal
          (Jsonaf.to_string (P.Inference_query.Summary.to_json (L.summary ledger)))
          (Jsonaf.to_string (P.Inference_query.Summary.to_json (L.summary restored))));
      assert (String.is_substring (ledger_bytes restored) ~substring:"\"literal\":1e+00");
      let state = { before with inference_ledger = ledger } in
      let full = SD.encode (SD.authored state) ~limits:document_limits |> document_ok in
      let current = SD.decode ~limits:document_limits full |> document_ok |> SD.value in
      assert (String.equal (ledger_bytes ledger) (ledger_bytes current.inference_ledger));
      ledger
    in
    let target =
      match Inference.Selection.view before.spec.inference_target with
      | Captured target -> target
      | Unresolved -> assert false
    in
    let configuration =
      O.Configuration.of_target
        target
        ~preparation_id:"native-serializer-lifecycle"
        ~transport:Http_sse
        ~capabilities:[]
        ~limits:O.Admission.observation
      |> observation_ok
    in
    let source =
      Transcript.Source_id.of_string "native-serializer-lifecycle"
      |> Result.ok_or_failwith
    in
    let outcomes
      : (Inference.Event.Terminal.delivery * Inference.Event.Terminal.outcome) list
      =
      [ Response_started, Completed
      ; Response_started, Refused
      ; Response_started, Incomplete Output_limit
      ; Response_started, Failed (Provider Rate_limited)
      ; Possibly_submitted, Failed (Transport Timeout)
      ; Definitely_not_submitted, Failed (Authentication Missing)
      ]
    in
    let ledger =
      List.foldi
        outcomes
        ~init:(roundtrip ledger)
        ~f:(fun index ledger (delivery, outcome) ->
          let operation_id =
            if index = 0 then None else Some (P.Id.Operation.create ())
          in
          let invocation_id =
            if index = 0 then None else Some (P.Id.Invocation.create ())
          in
          let relation =
            if index = 0
            then Transcript.Scope.Root
            else (
              let parent =
                List.hd_exn (L.rows ledger) |> L.Row.handle |> L.Handle.scope
              in
              let call_entry_id =
                History_entry.Id.create ~namespace:"actual-host" ~sequence:index
                |> Result.ok_or_failwith
              in
              Transcript.Scope.Nested
                { scope = Transcript.Scope.key parent
                ; call_entry_id = Some call_entry_id
                ; call_alias = Some "actual-call"
                })
          in
          let ledger, handle, _ =
            L.admit ledger ~source ~relation ~operation_id ~invocation_id ~configuration
            |> ledger_ok
          in
          let ledger = roundtrip ledger in
          assert (
            Option.equal P.Id.Operation.equal operation_id (L.Handle.operation_id handle));
          assert (
            Option.equal
              P.Id.Invocation.equal
              invocation_id
              (L.Handle.invocation_id handle));
          let ledger = L.set_state ledger handle Running |> ledger_ok |> roundtrip in
          let actual = O.Count.create (Actual 0L) |> observation_ok in
          let unknown = O.Count.create (Unknown Not_reported) |> observation_ok in
          let usage =
            O.Usage.create
              ~counts:
                { input = actual
                ; output = unknown
                ; reported_total = unknown
                ; cached_input = unknown
                ; cache_write_input = unknown
                ; reasoning_output = unknown
                }
              ~inclusions:[]
            |> observation_ok
          in
          let observed =
            O.create
              ~scope:(L.Handle.scope handle)
              ~id:(L.Handle.accounting_id handle)
              ~revision:0L
              ~payload:(Usage usage)
              ~limits:O.Admission.observation
            |> observation_ok
          in
          let ledger, _ = L.observe ledger handle observed |> ledger_ok in
          let ledger = roundtrip ledger in
          let terminal =
            Inference.Event.Terminal.create
              ~scope:(L.Handle.scope handle)
              ~delivery
              ~outcome
            |> Result.ok_or_failwith
          in
          L.set_state ledger handle (Terminal terminal) |> ledger_ok |> roundtrip)
    in
    let ledger, handle, _ =
      L.admit
        ledger
        ~source
        ~relation:Root
        ~operation_id:None
        ~invocation_id:None
        ~configuration
      |> ledger_ok
    in
    let ledger =
      L.set_state
        ledger
        handle
        (Interrupted { reason = Cancelled; delivery = Definitely_not_submitted })
      |> ledger_ok
      |> roundtrip
    in
    let ledger =
      List.fold
        [ P.Operation.Completed
        ; Cancelled
        ; Failed
            (P.Error.create
               Invalid_state
               ~message:"actual host failure"
               ~retryable:false
               ())
        ; Interrupted { reason = "actual host interruption"; retryable = false }
        ]
        ~init:ledger
        ~f:(fun ledger terminal_state ->
          let operation =
            P.Operation.
              { id = P.Id.Operation.create ()
              ; generation = before.identity.generation
              ; kind = Turn Administrative
              ; state = Starting
              ; started_at = timestamp
              ; updated_at = timestamp
              }
          in
          let ledger, handle, _ = L.admit_turn ledger operation |> ledger_ok in
          let ledger = roundtrip ledger in
          L.finish_turn ledger handle { operation with state = terminal_state }
          |> ledger_ok
          |> roundtrip)
    in
    let advanced =
      L.with_generation ledger ~generation:(before.identity.generation + 1) |> ledger_ok
    in
    (* Full state identity must advance alongside the ledger; the native ledger roundtrip remains exact. *)
    let document = L.to_document advanced |> ledger_ok in
    let restored = L.of_document document ~limits:L.Limits.default |> ledger_ok in
    assert (String.equal (D.Document.to_string document) (ledger_bytes restored));
    assert (
      List.equal
        L.Handle.equal
        (List.map (L.rows ledger) ~f:L.Row.handle)
        (List.map (L.rows advanced) ~f:L.Row.handle));
    assert (Int64.equal (L.revision advanced) (Int64.succ (L.revision ledger))))
;;

let%expect_test
    "protocol diagnostic survives actual ledger persistence without raw payload"
  =
  with_actor_workspace (fun _ workspace ->
    let before = state workspace in
    let ledger, handle, _ = admit before before.inference_ledger in
    let module V = O.Diagnostic.Protocol_violation in
    let diagnostic =
      O.Diagnostic.create
        ~phase:Stream
        ~reason:(Protocol_violation { V.stage = Feed; kind = Tracker Terminal_mismatch })
        ~delivery:(Some Response_started)
        ~elapsed_ms:None
      |> observation_ok
    in
    let observed =
      O.create
        ~scope:(L.Handle.scope handle)
        ~id:(O.Observation_id.of_string "protocol-proof" |> observation_ok)
        ~revision:0L
        ~payload:(Diagnostic diagnostic)
        ~limits:O.Admission.diagnostic
      |> observation_ok
    in
    let ledger, _ = L.observe ledger handle observed |> ledger_ok in
    let ledger =
      L.to_document ledger
      |> ledger_ok
      |> fun doc -> L.of_document doc ~limits:L.Limits.default |> ledger_ok
    in
    let rows = L.rows ledger in
    let retained =
      O.Attempt_record.observations (L.Row.record (List.hd_exn rows))
      |> List.filter ~f:(fun observation ->
        match O.payload observation with
        | Diagnostic _ -> true
        | _ -> false)
    in
    assert (List.length retained = 1);
    assert (O.equal observed (List.hd_exn retained));
    print_endline "exact scoped diagnostic retained once across ledger codec");
  [%expect {| exact scoped diagnostic retained once across ledger codec |}]
;;
