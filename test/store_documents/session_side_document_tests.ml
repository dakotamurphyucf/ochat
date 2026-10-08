open! Core
module S = Agent_store
module D = Document_schema

let checked = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : D.Error.t)]
;;

let stored = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : S.Store_error.t)]
;;

let parse text = D.Document.decode ~limits:D.Limits.default text |> checked

(* Independent named document: no current-domain encoder creates the admission
   fixture, so serializer/decoder mistakes cannot cancel each other out. *)
let metadata =
  {|{"format":"ochat.document","schema_version":1,"kind":"store.session_metadata","future_envelope":{"keep":true},"payload":{"session":{"id":"ses_side_document","created_at":"2026-08-15T12:00:00Z","updated_at":"2026-08-15T12:00:00Z","generation":0,"spec":{"execution_host":"daemon","prompt":{"type":"catalog","prompt_id":"prd_side_document","future_pin":"retained"},"workspace":{"type":"configured","workspace_id":"wsd_side_document"},"liveness":{"type":"detached"},"persistence":"durable","start_immediately":false,"labels":{},"future_spec":{"opaque":1}},"desired_state":"stopped","observed_state":{"type":"stopped"},"revision":"7","metadata_revision":"0","latest_event_sequence":"8","future_summary":"keep"},"prompt_artifact":"artifact","workspace_identity":"workspace","data_schema_version":"1","future_metadata":null}}|}
;;

let field json name =
  match D.Json.field json ~name with
  | Value value -> value
  | Null -> `Null
  | Absent -> failwith name
;;

let%expect_test "metadata edits retain nested fields and explicit null extensions" =
  let carrier = S.Session_metadata_document.of_document (parse metadata) |> checked in
  let value = D.Extension_carrier.value carrier in
  let session =
    { value.session with
      spec = { value.session.spec with display_name = Some "renamed" }
    }
  in
  let edited = D.Extension_carrier.with_value carrier { value with session } in
  let document = S.Session_metadata_document.to_document edited |> checked in
  let payload = D.Document.payload document in
  let summary = field payload "session" in
  let spec = field summary "spec" in
  print_s
    [%sexp
      (Jsonaf.to_string (field (D.Document.json document) "future_envelope") : string)
    , (Jsonaf.to_string (field spec "future_spec") : string)
    , (Jsonaf.to_string (field (field spec "prompt") "future_pin") : string)
    , (Jsonaf.to_string (field payload "future_metadata") : string)
    , (Jsonaf.to_string (field spec "display_name") : string)];
  [%expect {|("{\"keep\":true}" "{\"opaque\":1}" "\"retained\"" null "\"renamed\"")|}]
;;

let%expect_test "typed option null is rejected; summary presence stays distinct" =
  let document = parse metadata in
  let base = D.Document.payload document in
  let change_summary fields =
    match base with
    | `Object payload ->
      `Object (List.Assoc.add payload ~equal:String.equal "session" (`Object fields))
    | _ -> assert false
  in
  let summary =
    match field base "session" with
    | `Object fields -> fields
    | _ -> assert false
  in
  let inspect payload =
    D.Document.create
      ~limits:D.Limits.default
      ~kind:"store.session_metadata"
      ~version:1
      ~payload
    |> checked
  in
  let bad =
    inspect (change_summary (List.Assoc.add summary ~equal:String.equal "creator" `Null))
  in
  print_s [%sexp (Result.is_error (S.Session_metadata_document.of_document bad) : bool)];
  let nullable =
    inspect
      (change_summary
         (List.Assoc.add summary ~equal:String.equal "inference_summary" `Null))
  in
  let restored = S.Session_metadata_document.of_document nullable |> checked in
  let encoded = S.Session_metadata_document.to_document restored |> checked in
  print_s
    [%sexp
      (Jsonaf.to_string
         (field (field (D.Document.payload encoded) "session") "inference_summary")
       : string)];
  [%expect
    {|true
null|}]
;;

let%expect_test "unsupported semantics and beta representations fail closed" =
  let json =
    match D.Document.json (parse metadata) with
    | `Object fields ->
      `Object
        (List.Assoc.add
           fields
           ~equal:String.equal
           "required_semantics"
           (`Array [ `String "future_authority" ]))
    | _ -> assert false
  in
  let doc = D.Document.inspect ~limits:D.Limits.default json |> checked in
  print_s
    [%sexp
      (Result.is_error (S.Session_metadata_document.of_document doc) : bool)
    , (Result.is_error (D.Document.decode ~limits:D.Limits.default "((version 1))")
       : bool)];
  [%expect {|(true true)|}]
;;

let with_directory f =
  Eio_main.run (fun env ->
    let path =
      Filename.concat
        (Sys.getenv "TMPDIR" |> Option.value ~default:"/tmp")
        ("ochat-side-doc." ^ Agent_protocol.Id.Transaction.(create () |> to_string))
    in
    let root = Eio.Path.(Eio.Stdenv.fs env / path) in
    Eio.Path.mkdir ~perm:0o700 root;
    Exn.protect
      ~f:(fun () -> f env path)
      ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true root))
;;

let%expect_test "fresh and inherited projection recovery intents have distinct ownership" =
  with_directory (fun env directory ->
    let marker_path = Filename.concat directory "sessions.recovery-required" in
    let owner = S.Session_projection_update.create ~env ~marker_path () in
    S.Session_projection_update.publish owner ~f:(fun ~require_intent ->
      require_intent ())
    |> stored;
    print_s
      [%sexp (S.Session_projection_update.pending ~env ~marker_path |> stored : bool)];
    S.Session_projection_update.require ~env ~marker_path |> stored;
    S.Session_projection_update.publish owner ~f:(fun ~require_intent ->
      require_intent ())
    |> stored;
    print_s
      [%sexp (S.Session_projection_update.pending ~env ~marker_path |> stored : bool)];
    let failed =
      S.Session_projection_update.publish owner ~f:(fun ~require_intent ->
        let%bind.Result () = require_intent () in
        Error (S.Store_error.Corrupt "injected projection failure"))
    in
    print_s
      [%sexp
        (Result.is_error failed : bool)
      , (Result.is_error (S.Session_projection_update.complete_recovery owner) : bool)];
    let reopened = S.Session_projection_update.create ~env ~marker_path () in
    S.Session_projection_update.complete_recovery reopened |> stored;
    print_s
      [%sexp (S.Session_projection_update.pending ~env ~marker_path |> stored : bool)]);
  [%expect
    {|false
true
(true true)
false|}]
;;

let%expect_test "explicit publication cancellation retains recovery intent" =
  with_directory (fun env directory ->
    let marker_path = Filename.concat directory "sessions.recovery-required" in
    let owner = S.Session_projection_update.create ~env ~marker_path () in
    let cancelled =
      try
        ignore
          (S.Session_projection_update.publish owner ~f:(fun ~require_intent ->
             require_intent () |> stored;
             raise Eio.Time.Timeout)
           : (unit, S.Store_error.t) Result.t);
        false
      with
      | Eio.Time.Timeout -> true
    in
    print_s
      [%sexp
        (cancelled : bool)
      , (S.Session_projection_update.pending ~env ~marker_path |> stored : bool)
      , (Result.is_error (S.Session_projection_update.complete_recovery owner) : bool)]);
  [%expect {|(true true true)|}]
;;

let replace json name value =
  match json with
  | `Object fields -> `Object (List.Assoc.add fields ~equal:String.equal name value)
  | _ -> failwith "expected fixture object"
;;

let entry_fixture id =
  let session = field (D.Document.payload (parse metadata)) "session" in
  `Object
    [ "session_id", `String id
    ; "session", replace session "id" (`String id)
    ; "runnable_job_count", `String "0"
    ; "deliverable_job_count", `String "0"
    ; "earliest_schedule_due", `Null
    ; "owner_grace_deadline", `Null
    ; "pending_initial_start", `False
    ; "archived", `False
    ; "future_entry", `Object [ "escaped_reference", `String "future opaque" ]
    ]
;;

let index_fixture entries =
  let json =
    `Object
      [ "format", `String "ochat.document"
      ; "schema_version", `Number "1"
      ; "kind", `String "store.session_index"
      ; "future_index", `True
      ; "payload", `Object [ "entries", `Array entries ]
      ]
  in
  D.Document.inspect ~limits:S.Session_index_document.limits json |> checked
;;

let%expect_test "index keyed identities reject duplicates and mismatched summaries" =
  let entry = entry_fixture "ses_side_document" in
  let duplicate = index_fixture [ entry; entry ] in
  print_s
    [%sexp (Result.is_error (S.Session_index_document.of_document duplicate) : bool)];
  let mismatch =
    index_fixture [ replace entry "session_id" (`String "ses_other_document") ]
  in
  print_s [%sexp (Result.is_error (S.Session_index_document.of_document mismatch) : bool)];
  let negative = index_fixture [ replace entry "runnable_job_count" (`String "-1") ] in
  print_s [%sexp (Result.is_error (S.Session_index_document.of_document negative) : bool)];
  [%expect
    {|true
true
true|}]
;;

let%expect_test
    "index mutation retains unknown data on retained entries and explicitly retires \
     removed identities"
  =
  with_directory (fun env directory ->
    let path = Filename.concat directory "sessions.snapshot" in
    let raw =
      index_fixture
        [ entry_fixture "ses_side_document"; entry_fixture "ses_other_document" ]
      |> D.Document.to_string
    in
    Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(Eio.Stdenv.fs env / path) raw;
    let index = S.Session_index.open_or_create ~env ~path |> stored in
    let id =
      Agent_protocol.Id.Session.of_string "ses_side_document"
      |> function
      | Ok value -> value
      | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
    in
    let entry = S.Session_index.find_checked index id |> stored |> Option.value_exn in
    let session =
      { entry.session with
        spec = { entry.session.spec with display_name = Some "edited" }
      }
    in
    S.Session_index.upsert index { entry with session } |> stored;
    let other =
      Agent_protocol.Id.Session.of_string "ses_other_document"
      |> function
      | Ok value -> value
      | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
    in
    S.Session_index.remove index other |> stored;
    let contents = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path) in
    let document =
      D.Document.decode ~limits:S.Session_index_document.limits contents |> checked
    in
    let entries =
      match field (D.Document.payload document) "entries" with
      | `Array entries -> entries
      | _ -> assert false
    in
    let retained = List.hd_exn entries in
    print_s
      [%sexp
        (List.length entries : int)
      , (D.Json.equal (field (D.Document.json document) "future_index") `True : bool)
      , (D.Json.equal
           (field retained "future_entry")
           (field (entry_fixture "ses_side_document") "future_entry")
         : bool)
      , (D.Json.equal (field (field retained "session") "future_summary") (`String "keep")
         : bool)
      , (D.Json.equal
           (field (field (field retained "session") "spec") "display_name")
           (`String "edited")
         : bool)]);
  [%expect {|(1 true true true true)|}]
;;

let%expect_test "unsupported recovery marker never gets cleared or rewritten" =
  with_directory (fun env directory ->
    let marker_path = Filename.concat directory "sessions.recovery-required" in
    let raw =
      {|{"format":"ochat.document","schema_version":1,"kind":"store.session_index_recovery","required_semantics":["future_projection_authority"],"payload":{}}|}
    in
    Eio.Path.save
      ~create:(`Exclusive 0o600)
      Eio.Path.(Eio.Stdenv.fs env / marker_path)
      raw;
    let owner = S.Session_projection_update.create ~env ~marker_path () in
    print_s
      [%sexp
        (Result.is_error (S.Session_projection_update.pending ~env ~marker_path) : bool)
      , (Result.is_error (S.Session_projection_update.require ~env ~marker_path) : bool)
      , (Result.is_error (S.Session_projection_update.complete_recovery owner) : bool)
      , (String.equal raw (Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / marker_path))
         : bool)]);
  [%expect {|(true true true true)|}]
;;

let%expect_test
    "cancelled authoritative index publication remains unavailable without poisoning \
     owner"
  =
  with_directory (fun env directory ->
    let path = Filename.concat directory "sessions.snapshot" in
    let raw =
      index_fixture [ entry_fixture "ses_side_document" ] |> D.Document.to_string
    in
    Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(Eio.Stdenv.fs env / path) raw;
    let index = S.Session_index.open_or_create ~env ~path |> stored in
    let entry = S.Session_index.list_checked index |> stored |> List.hd_exn in
    let cancelled =
      try
        ignore
          (S.Session_index.with_prepared_upsert index entry ~publish_authority:(fun () ->
             raise Eio.Time.Timeout)
           : (unit, S.Store_error.t) Result.t);
        false
      with
      | Eio.Time.Timeout -> true
    in
    print_s
      [%sexp
        (cancelled : bool), (Result.is_ok (S.Session_index.availability index) : bool)];
    assert (Result.is_error (S.Session_index.upsert index entry));
    let reopened = S.Session_index.open_or_create ~env ~path |> stored in
    S.Session_index.upsert reopened entry |> stored;
    print_s [%sexp (List.length (S.Session_index.list_checked reopened |> stored) : int)]);
  [%expect
    {|(true false)
1|}]
;;

let%expect_test "uncertain recovery marker unlink restores exact admitted unknown fields" =
  with_directory (fun env directory ->
    let marker_path = Filename.concat directory "sessions.recovery-required" in
    let original =
      {| {"format":"ochat.document","schema_version":1,"kind":"store.session_index_recovery","future":{"ref":"retained"},"payload":{"future_payload":null}} |}
    in
    Eio.Path.save
      ~create:(`Exclusive 0o600)
      Eio.Path.(Eio.Stdenv.fs env / marker_path)
      original;
    let owner =
      S.Session_projection_update.create
        ~env
        ~marker_path
        ~sync_directory:(fun ~env:_ ~path:_ ->
          Error (S.Store_error.Corrupt "injected sync uncertainty"))
        ()
    in
    let result = S.Session_projection_update.complete_recovery owner in
    print_s
      [%sexp
        (Result.is_error result : bool)
      , (String.equal original (Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / marker_path))
         : bool)
      , (Result.is_error (S.Session_projection_update.complete_recovery owner) : bool)]);
  [%expect {|(true true true)|}]
;;

let%expect_test "temporary backing metadata preserves transient persistence" =
  let original = S.Session_metadata_document.of_document (parse metadata) |> checked in
  let value = D.Extension_carrier.value original in
  let session =
    { value.session with
      spec =
        { value.session.spec with
          persistence = Transient
        ; execution_host = Embedded
        ; liveness = Process_bound
        }
    }
  in
  let carrier = D.Extension_carrier.with_value original { value with session } in
  let document = S.Session_metadata_document.to_document carrier |> checked in
  let admitted =
    S.Session_metadata_document.of_document document
    |> checked
    |> D.Extension_carrier.value
  in
  assert (
    Agent_protocol.Session.equal_persistence admitted.session.spec.persistence Transient);
  print_endline "transient protocol semantics preserved in temporary disk backing";
  [%expect {|transient protocol semantics preserved in temporary disk backing|}]
;;

let%expect_test
    "canonical targets advance serially and independent tokens cannot clear each other"
  =
  with_directory (fun env directory ->
    let marker_path = Filename.concat directory "sessions.recovery-required" in
    let owner = S.Session_projection_update.create ~env ~marker_path () in
    let other_owner = S.Session_projection_update.create ~env ~marker_path () in
    let entries =
      index_fixture
        [ entry_fixture "ses_canonical_first"; entry_fixture "ses_canonical_second" ]
      |> S.Session_index_document.of_document
      |> checked
      |> D.Extension_carrier.value
    in
    let first = List.nth_exn entries 0
    and second = List.nth_exn entries 1 in
    let first_token =
      S.Session_projection_update.prepare_canonical
        owner
        ~previous:None
        ~prepare:(fun () -> Ok first)
      |> stored
    in
    let second_token =
      S.Session_projection_update.prepare_canonical
        owner
        ~previous:None
        ~prepare:(fun () -> Ok second)
      |> stored
    in
    S.Session_projection_update.complete_recovery owner |> stored;
    assert (S.Session_projection_update.pending ~env ~marker_path |> stored);
    let newer =
      { first with
        session = { first.session with revision = Int64.succ first.session.revision }
      ; runnable_job_count = 1
      }
    in
    let advanced =
      S.Session_projection_update.prepare_canonical
        owner
        ~previous:(Some first_token)
        ~prepare:(fun () -> Ok newer)
      |> stored
    in
    let changed_hints = { newer with runnable_job_count = 2 } in
    assert (
      Result.is_error
        (S.Session_projection_update.prepare_canonical
           owner
           ~previous:(Some advanced)
           ~prepare:(fun () -> Ok changed_hints)));
    assert (S.Session_projection_update.Pending.matches advanced newer);
    assert (
      Result.is_error
        (S.Session_projection_update.finish_canonical owner advanced ~entry:first));
    assert (
      Result.is_error
        (S.Session_projection_update.finish_canonical other_owner advanced ~entry:newer));
    S.Session_projection_update.finish_canonical owner second_token ~entry:second
    |> stored;
    assert (S.Session_projection_update.pending ~env ~marker_path |> stored);
    S.Session_projection_update.finish_canonical owner advanced ~entry:newer |> stored;
    assert (not (S.Session_projection_update.pending ~env ~marker_path |> stored));
    S.Session_projection_update.require ~env ~marker_path |> stored;
    let inherited =
      S.Session_projection_update.prepare_canonical
        owner
        ~previous:None
        ~prepare:(fun () -> Ok newer)
      |> stored
    in
    S.Session_projection_update.finish_canonical owner inherited ~entry:newer |> stored;
    assert (S.Session_projection_update.pending ~env ~marker_path |> stored);
    S.Session_projection_update.complete_recovery owner |> stored;
    assert (not (S.Session_projection_update.pending ~env ~marker_path |> stored));
    print_endline
      "startup defers active tokens; stale/foreign/equal-revision hints reject; last \
       owned completion clears only fresh intent");
  [%expect
    {|startup defers active tokens; stale/foreign/equal-revision hints reject; last owned completion clears only fresh intent|}]
;;

let%expect_test
    "canonical prevalidation cancellation has no intent effect and does not poison owner"
  =
  with_directory (fun env directory ->
    let marker_path = Filename.concat directory "sessions.recovery-required" in
    let owner = S.Session_projection_update.create ~env ~marker_path () in
    let cancelled =
      try
        S.Session_projection_update.prepare_canonical
          owner
          ~previous:None
          ~prepare:(fun () -> raise Eio.Time.Timeout)
        |> stored
        |> ignore;
        false
      with
      | Eio.Time.Timeout -> true
    in
    assert cancelled;
    assert (not (S.Session_projection_update.pending ~env ~marker_path |> stored));
    S.Session_projection_update.complete_recovery owner |> stored;
    print_endline "cancelled pure preparation leaves no intent; owner remains usable");
  [%expect {|cancelled pure preparation leaves no intent; owner remains usable|}]
;;
