open! Core
module S = Agent_store
module R = S.Session_archive_record
module C = S.Session_archive_document
module D = Document_schema
module P = Agent_protocol

let protocol = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : P.Error.t)]
;;

let checked = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : D.Error.t)]
;;

let parse bytes = D.Document.decode ~limits:C.limits bytes |> checked
let id = P.Id.Session.of_string "ses_archive_lifecycle" |> protocol
let principal = P.Id.Principal.of_string "pri_archive_owner" |> protocol
let now = P.Timestamp.of_string "2026-10-09T12:00:00Z" |> protocol
let later = P.Timestamp.add_ms now R.receipt_retention_ms |> protocol

let anchor =
  R.Anchor.create ~generation:0 ~session_revision:7L ~latest_event_sequence:9L |> protocol
;;

let digest = String.make 64 'a'

let key action suffix =
  S.Idempotency_store.Key.
    { principal_id = principal
    ; session_id = Some id
    ; method_name = R.Outcome.method_name action
    ; idempotency_key = P.Idempotency_key.of_string suffix |> protocol
    }
;;

let prepare state action suffix ~at =
  R.prepare
    state
    ~expected:(R.revision state)
    ~anchor
    ~action
    ~key:(key action suffix)
    ~request_digest:digest
    ~now:at
  |> protocol
;;

let change document action suffix ~at =
  let prepared = prepare (C.value document) action suffix ~at in
  C.prepare document prepared ~now:at |> checked
;;

let field json name =
  match D.Json.field json ~name with
  | Value value -> value
  | Null -> `Null
  | Absent -> failwith name
;;

let replace json name value =
  match json with
  | `Object fields -> `Object (List.Assoc.add fields ~equal:String.equal name value)
  | _ -> failwith "expected fixture object"
;;

let remove json name =
  match json with
  | `Object fields -> `Object (List.Assoc.remove fields name ~equal:String.equal)
  | _ -> failwith "expected fixture object"
;;

let inspect json = D.Document.inspect ~limits:C.limits json |> checked

let rewrite_payload document ~f =
  D.Document.json document
  |> fun json -> replace json "payload" (f (D.Document.payload document)) |> inspect
;;

let original =
  {|{"format":"ochat.document","schema_version":1,"kind":"store.session_archive","future_envelope":{"keep":true},"payload":{"session_id":"ses_archive_lifecycle","future_payload":null}}|}
;;

let current =
  {|{"format":"ochat.document","schema_version":2,"kind":"store.session_archive","payload":{"session_id":"ses_archive_lifecycle","status":"active","admission":"automatic","revision":"0","receipts":[]}}|}
;;

let%expect_test "original named archive conversion preserves raw extensions and owner" =
  let admitted = C.of_document (parse original) |> checked in
  let encoded = C.to_document admitted |> checked in
  [%test_eq: int] (D.Document.version encoded) 2;
  assert (P.Id.Session.equal (C.stored_session_id (parse original) |> checked) id);
  assert (R.Status.equal (R.status (C.value admitted)) Archived);
  assert (R.Admission.equal (R.admission (C.value admitted)) Explicit_resume_required);
  [%test_eq: int64] (R.Revision.to_int64 (R.revision (C.value admitted))) 1L;
  assert (
    Jsonaf.exactly_equal
      (field (D.Document.json encoded) "future_envelope")
      (`Object [ "keep", `True ]));
  assert (Jsonaf.exactly_equal (field (D.Document.payload encoded) "future_payload") `Null);
  let wrong =
    rewrite_payload (parse original) ~f:(fun p ->
      replace p "session_id" (`String "ses_other_owner"))
  in
  assert (not (P.Id.Session.equal (C.stored_session_id wrong |> checked) id));
  print_endline "v1 -> v2; original owner exposed before conversion; extensions retained";
  [%expect {|v1 -> v2; original owner exposed before conversion; extensions retained|}]
;;

let%expect_test "restore no-op preserves automatic; removed is terminal; replay is exact" =
  let initial = C.of_document (parse current) |> checked in
  let noop = change initial Restore "noop" ~at:now in
  [%test_eq: int64] (R.Revision.to_int64 (R.revision (C.value noop))) 0L;
  assert (R.Admission.equal (R.admission (C.value noop)) Automatic);
  let archived = change noop Archive "archive" ~at:now in
  let restored = change archived Restore "restore" ~at:now in
  assert (R.Admission.equal (R.admission (C.value restored)) Explicit_resume_required);
  let resumed = change restored Resume "resume" ~at:now in
  let active_noop = change resumed Restore "active-noop" ~at:now in
  [%test_eq: int64] (R.Revision.to_int64 (R.revision (C.value active_noop))) 3L;
  assert (R.Admission.equal (R.admission (C.value active_noop)) Automatic);
  let removed = change active_noop Remove "remove" ~at:now in
  List.iter [ R.Outcome.Archive; Restore; Resume ] ~f:(fun action ->
    assert (
      Result.is_error
        (R.prepare
           (C.value removed)
           ~expected:(R.revision (C.value removed))
           ~anchor
           ~action
           ~key:(key action "after-remove")
           ~request_digest:digest
           ~now)));
  let old =
    R.prepare
      (C.value removed)
      ~expected:R.Revision.zero
      ~anchor
      ~action:Restore
      ~key:(key Restore "restore")
      ~request_digest:digest
      ~now
    |> protocol
  in
  let original_receipt =
    R.lookup (C.value restored) ~key:(key Restore "restore") ~request_digest:digest ~now
    |> protocol
    |> Option.value_exn
  in
  assert (R.Outcome.equal (R.Prepared.outcome old) original_receipt.outcome);
  assert (R.equal (R.Prepared.next old) (C.value removed));
  assert (
    Result.is_error
      (R.prepare
         (C.value removed)
         ~expected:R.Revision.zero
         ~anchor
         ~action:Restore
         ~key:(key Restore "restore")
         ~request_digest:(String.make 64 'b')
         ~now));
  print_endline
    "active restore preserves admission/revision; removed terminal; original receipt \
     wins stale replay";
  [%expect
    {|active restore preserves admission/revision; removed terminal; original receipt wins stale replay|}]
;;

let%expect_test
    "keyed receipt outcome and nested extensions survive admission and acknowledgement"
  =
  let archived = C.of_document (parse original) |> checked in
  let restored = change archived Restore "proof" ~at:now in
  let raw = C.to_document restored |> checked in
  let extended =
    rewrite_payload raw ~f:(fun payload ->
      let entries =
        match field payload "receipts" with
        | `Array entries -> entries
        | _ -> assert false
      in
      replace
        payload
        "receipts"
        (`Array
            (List.map entries ~f:(fun r ->
               r
               |> fun r ->
               replace r "future_receipt" (`Object [ "keep", `True ])
               |> fun r ->
               replace r "outcome" (replace (field r "outcome") "future_result" `Null)
               |> fun r ->
               replace r "key" (replace (field r "key") "future_key" (`String "retained"))))))
  in
  let admitted = C.of_document extended |> checked in
  let resumed = change admitted Resume "next-proof" ~at:now in
  let acknowledged =
    C.acknowledge resumed ~key:(key Restore "proof") ~request_digest:digest |> checked
  in
  let encoded = C.to_document acknowledged |> checked in
  let entries =
    match field (D.Document.payload encoded) "receipts" with
    | `Array entries -> entries
    | _ -> assert false
  in
  let receipt =
    List.find_exn entries ~f:(fun r ->
      String.equal
        (field (field r "key") "method_name" |> Jsonaf.to_string)
        "\"session.restore\"")
  in
  assert (
    Jsonaf.exactly_equal (field receipt "future_receipt") (`Object [ "keep", `True ]));
  assert (Jsonaf.exactly_equal (field (field receipt "outcome") "future_result") `Null);
  assert (
    Jsonaf.exactly_equal (field (field receipt "key") "future_key") (`String "retained"));
  assert (Jsonaf.exactly_equal (field receipt "completion_acknowledged") `True);
  let next = prepare (C.value acknowledged) Restore "third-proof" ~at:now in
  assert (Result.is_error (C.prepare admitted next ~now));
  print_endline
    "receipt identity preserves nested unknowns; acknowledgement changes no immutable \
     proof; stale basis rejected";
  [%expect
    {|receipt identity preserves nested unknowns; acknowledgement changes no immutable proof; stale basis rejected|}]
;;

let%expect_test
    "unacknowledged proof never expires; bounded receipts retire only after acknowledged \
     expiry"
  =
  let state = R.initial ~session_id:id in
  let full =
    List.fold
      (List.init R.max_receipts ~f:Int.to_string)
      ~init:state
      ~f:(fun state suffix -> prepare state Restore suffix ~at:now |> R.Prepared.next)
  in
  assert (
    Result.is_error
      (R.prepare
         full
         ~expected:(R.revision full)
         ~anchor
         ~action:Restore
         ~key:(key Restore "overflow")
         ~request_digest:digest
         ~now:later));
  assert (
    Option.is_some
      (R.lookup full ~key:(key Restore "0") ~request_digest:digest ~now:later |> protocol));
  let document = C.authored full |> checked in
  let acknowledged =
    C.acknowledge document ~key:(key Restore "0") ~request_digest:digest |> checked
  in
  let next = change acknowledged Restore "overflow" ~at:later in
  [%test_eq: int] (List.length (R.receipts (C.value next))) R.max_receipts;
  assert (
    Option.is_none
      (R.lookup (C.value next) ~key:(key Restore "0") ~request_digest:digest ~now:later
       |> protocol));
  assert (
    Option.is_some
      (R.lookup (C.value next) ~key:(key Restore "1") ~request_digest:digest ~now:later
       |> protocol));
  print_endline
    "32 protected proofs block admission after expiry; exact completion releases only \
     expired acknowledged proof";
  [%expect
    {|32 protected proofs block admission after expiry; exact completion releases only expired acknowledged proof|}]
;;

let%expect_test
    "strict current fields and immutable receipt validation reject malformed evidence"
  =
  let document = parse current in
  List.iter [ "status"; "admission"; "revision"; "receipts" ] ~f:(fun name ->
    assert (
      Result.is_error
        (C.of_document (rewrite_payload document ~f:(fun p -> remove p name))));
    assert (
      Result.is_error
        (C.of_document (rewrite_payload document ~f:(fun p -> replace p name `Null)))));
  assert (
    Result.is_error
      (C.of_document
         (rewrite_payload document ~f:(fun p -> replace p "status" (`String "archived")))));
  assert (
    Result.is_error
      (C.of_document
         (rewrite_payload (parse original) ~f:(fun p ->
            replace p "status" (`String "active")))));
  let original_bad =
    rewrite_payload (parse original) ~f:(fun p -> replace p "admission" `Null)
  in
  assert (Result.is_error (C.of_document original_bad));
  let restored = change (C.of_document document |> checked) Archive "receipt" ~at:now in
  let raw = C.to_document restored |> checked in
  let bad_receipts f =
    rewrite_payload raw ~f:(fun p ->
      match field p "receipts" with
      | `Array entries -> replace p "receipts" (`Array (f entries))
      | _ -> assert false)
  in
  assert (
    Result.is_error (C.of_document (bad_receipts (fun entries -> entries @ entries))));
  assert (
    Result.is_error
      (C.of_document
         (bad_receipts
            (List.map ~f:(fun r -> replace r "request_digest" (`String "bad"))))));
  assert (
    Result.is_error
      (C.of_document
         (bad_receipts
            (List.map ~f:(fun r -> replace r "id" (`String (String.make 64 '0')))))));
  let too_large =
    D.Document.json raw
    |> fun json -> replace json "unknown_padding" (`String (String.make 32768 'x'))
  in
  assert (Result.is_error (D.Document.inspect ~limits:C.limits too_large));
  print_endline
    "required presence, coherent head, duplicate/digest/key evidence and full-envelope \
     bounds fail closed";
  [%expect
    {|required presence, coherent head, duplicate/digest/key evidence and full-envelope bounds fail closed|}]
;;

(* Independent raw v2 receipt: canonical composite key digest calculated outside
   the current OCaml codec. Unknown members are not part of logical key identity. *)
let literal_receipt =
  {|{"format":"ochat.document","schema_version":2,"kind":"store.session_archive","payload":{"session_id":"ses_archive_lifecycle","status":"active","admission":"explicit_resume_required","revision":"2","receipts":[{"id":"b898a2951c527f82c923e04978a64c78e32f137f68d4ad4cec213215e26ce268","key":{"principal_id":"pri_archive_owner","session_id":"ses_archive_lifecycle","method_name":"session.restore","idempotency_key":"literal-proof","future_key":null},"request_digest":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","outcome":{"session_id":"ses_archive_lifecycle","anchor":{"generation":0,"session_revision":"7","latest_event_sequence":"9","future_anchor":"retained"},"lifecycle_revision":"2","status":"active","admission":"explicit_resume_required","action":"restore","disposition":"applied","completed_at":"2026-10-09T12:00:00Z","future_outcome":{"number":1.0}},"created_at":"2026-10-09T12:00:00Z","expires_at":"2026-10-10T12:00:00Z","completion_acknowledged":false,"future_receipt":{"opaque":true}}]}}|}
;;

let%expect_test
    "raw keyed receipt admission preserves original nested proof; expiry validates exact \
     duration"
  =
  let original = parse literal_receipt in
  let admitted = C.of_document original |> checked in
  let encoded = C.to_document admitted |> checked in
  (* Owned timestamp fields use the protocol's nanosecond canonical spelling.
     Unknown fields, including the numeric lexeme [1.0], stay exact. *)
  let canonical =
    rewrite_payload original ~f:(fun payload ->
      match field payload "receipts" with
      | `Array receipts ->
        replace
          payload
          "receipts"
          (`Array
              (List.map receipts ~f:(fun receipt ->
                 receipt
                 |> fun receipt ->
                 replace receipt "created_at" (P.Timestamp.to_json now)
                 |> fun receipt ->
                 replace receipt "expires_at" (P.Timestamp.to_json later)
                 |> fun receipt ->
                 replace
                   receipt
                   "outcome"
                   (replace
                      (field receipt "outcome")
                      "completed_at"
                      (P.Timestamp.to_json now)))))
      | _ -> assert false)
  in
  assert (D.Json.equal (D.Document.json encoded) (D.Document.json canonical));
  let receipt =
    R.lookup
      (C.value admitted)
      ~key:(key Restore "literal-proof")
      ~request_digest:digest
      ~now:later
    |> protocol
    |> Option.value_exn
  in
  assert (
    R.Outcome.equal
      receipt.outcome
      (R.Prepared.outcome (prepare (C.value admitted) Restore "literal-proof" ~at:later)));
  let invalid_expiry =
    rewrite_payload original ~f:(fun payload ->
      match field payload "receipts" with
      | `Array values ->
        replace
          payload
          "receipts"
          (`Array
              (List.map values ~f:(fun r ->
                 replace r "expires_at" (`String "2026-10-11T12:00:00Z"))))
      | _ -> assert false)
  in
  assert (Result.is_error (C.of_document invalid_expiry));
  let unsupported =
    D.Document.json original
    |> fun j ->
    replace j "required_semantics" (`Array [ `String "future-lifecycle-policy" ])
    |> inspect
  in
  assert (Result.is_error (C.of_document unsupported));
  let malformed_owner =
    rewrite_payload original ~f:(fun payload -> replace payload "session_id" `Null)
  in
  assert (Result.is_error (C.stored_session_id malformed_owner));
  let invalid_action =
    rewrite_payload original ~f:(fun payload ->
      match field payload "receipts" with
      | `Array values ->
        replace
          payload
          "receipts"
          (`Array
              (List.map values ~f:(fun r ->
                 replace
                   r
                   "outcome"
                   (replace (field r "outcome") "action" (`String "archive")))))
      | _ -> assert false)
  in
  assert (Result.is_error (C.of_document invalid_action));
  print_endline
    "independent key digest and immutable raw proof admitted; expiry/required \
     semantics/owner/action rejected";
  [%expect
    {|independent key digest and immutable raw proof admitted; expiry/required semantics/owner/action rejected|}]
;;

let%expect_test "whole archive envelope has an exact unchanged 32768-byte limit" =
  let base =
    D.Document.json (parse current) |> fun j -> replace j "future_padding" (`String "")
  in
  let padding_bytes = 32768 - String.length (Jsonaf.to_string base) in
  let exact = replace base "future_padding" (`String (String.make padding_bytes 'x')) in
  let document = D.Document.inspect ~limits:C.limits exact |> checked in
  [%test_eq: int] (String.length (D.Document.to_string document)) 32768;
  ignore (C.of_document document |> checked : C.t);
  let over =
    replace base "future_padding" (`String (String.make (padding_bytes + 1) 'x'))
  in
  assert (Result.is_error (D.Document.inspect ~limits:C.limits over));
  print_endline
    "32768-byte envelope admitted; one further byte rejected without increasing limits";
  [%expect
    {|32768-byte envelope admitted; one further byte rejected without increasing limits|}]
;;

let%expect_test
    "terminal applied remove proof survives generic acknowledgement and expiry"
  =
  let document = C.of_document (parse current) |> checked in
  let removed = change document Remove "terminal-proof" ~at:now in
  let acknowledged =
    C.acknowledge removed ~key:(key Remove "terminal-proof") ~request_digest:digest
    |> checked
  in
  let expired = change acknowledged Remove "remove-again" ~at:later in
  let state = C.value expired in
  let receipt =
    R.lookup state ~key:(key Remove "terminal-proof") ~request_digest:digest ~now:later
    |> protocol
    |> Option.value_exn
  in
  assert receipt.completion_acknowledged;
  assert (R.Outcome.equal_disposition receipt.outcome.disposition Applied);
  assert (R.Revision.equal receipt.outcome.lifecycle_revision (R.revision state));
  [%test_eq: int] (List.length (R.receipts state)) 2;
  assert (
    Result.is_error
      (R.restore
         ~session_id:id
         ~status:Removed
         ~admission:Explicit_resume_required
         ~revision:(R.revision state)
         ~receipts:[]));
  assert (
    Result.is_error
      (R.restore
         ~session_id:id
         ~status:Removed
         ~admission:Explicit_resume_required
         ~revision:(R.revision state)
         ~receipts:
           (List.filter (R.receipts state) ~f:(fun r ->
              R.Outcome.equal_disposition r.outcome.disposition Already_current))));
  assert (
    Result.is_error
      (R.lookup
         state
         ~key:(key Remove "terminal-proof")
         ~request_digest:(String.make 64 'b')
         ~now:later));
  let raw = C.to_document expired |> checked in
  let without_proof =
    rewrite_payload raw ~f:(fun p -> replace p "receipts" (`Array []))
  in
  assert (Result.is_error (C.of_document without_proof));
  print_endline
    "generic completion is not cleanup: terminal Applied Remove proof remains unique and \
     retained";
  [%expect
    {|generic completion is not cleanup: terminal Applied Remove proof remains unique and retained|}]
;;

let%expect_test
    "legacy delete proof keeps original command method, policy and params digest"
  =
  let request =
    P.Session.Delete_request.
      { session_id = id
      ; attachment_id = P.Id.Attachment.of_string "att_archive_owner" |> protocol
      ; expected_revision = 7L
      ; policy = Archive
      ; confirmation = P.Id.Session.to_string id
      ; idempotency_key = P.Idempotency_key.of_string "original-delete" |> protocol
      }
  in
  let command = P.Command.Session_delete request in
  let original_digest =
    P.Command.params command
    |> P.Json_codec.canonical_string
    |> protocol
    |> Digestif.SHA256.digest_string
    |> Digestif.SHA256.to_hex
  in
  let original_key =
    S.Idempotency_store.Key.
      { principal_id = principal
      ; session_id = Some id
      ; method_name = P.Command.method_name command
      ; idempotency_key = request.idempotency_key
      }
  in
  [%test_eq: string] original_key.method_name "session.delete";
  let prepared =
    R.prepare
      (R.initial ~session_id:id)
      ~expected:R.Revision.zero
      ~anchor
      ~action:Archive
      ~key:original_key
      ~request_digest:original_digest
      ~now
    |> protocol
  in
  let document =
    C.authored (R.initial ~session_id:id)
    |> checked
    |> fun doc -> C.prepare doc prepared ~now |> checked
  in
  let admitted =
    C.to_document document |> checked |> C.of_document |> checked |> C.value
  in
  let proof =
    R.lookup admitted ~key:original_key ~request_digest:original_digest ~now
    |> protocol
    |> Option.value_exn
  in
  assert (S.Idempotency_store.Key.compare proof.key original_key = 0);
  [%test_eq: string] proof.request_digest original_digest;
  assert (R.Outcome.equal_action proof.outcome.action Archive);
  let removal = P.Command.Session_delete { request with policy = Remove } in
  let removal_digest =
    P.Command.params removal
    |> P.Json_codec.canonical_string
    |> protocol
    |> Digestif.SHA256.digest_string
    |> Digestif.SHA256.to_hex
  in
  assert (not (String.equal original_digest removal_digest));
  assert (
    Result.is_error
      (R.prepare
         admitted
         ~expected:(R.revision admitted)
         ~anchor
         ~action:Remove
         ~key:original_key
         ~request_digest:removal_digest
         ~now));
  assert (
    Result.is_error
      (R.prepare
         admitted
         ~expected:(R.revision admitted)
         ~anchor
         ~action:Restore
         ~key:original_key
         ~request_digest:original_digest
         ~now));
  print_endline
    "session.delete original key and policy-bound digest retained; changed policy \
     conflicts; restore cannot alias delete";
  [%expect
    {|session.delete original key and policy-bound digest retained; changed policy conflicts; restore cannot alias delete|}]
;;
