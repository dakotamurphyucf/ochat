open! Core
module S = Agent_store
module D = Document_schema
module P = Agent_protocol
module Event = S.Audit_event_document
module Evidence = S.Audit_evidence_document

let limits =
  S.Document_fields.limits ~max_bytes:16384
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let checked result =
  Result.map_error result ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let store_checked result =
  Result.map_error result ~f:(fun error ->
    Sexp.to_string_hum (S.Store_error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let original_event =
  " \n"
  ^ {|{"format":"ochat.document","schema_version":1,"kind":"store.audit_event","future_envelope":null,"payload":{"sequence":"1","timestamp":"2026-08-15T12:00:00Z","level":"warning","name":"independent.named","payload":{"lexical":1e+00,"safe":true},"redacted":true,"future_event":{"preserve":null}}}|}
  ^ "\n "
;;

let original_hash = "351d5aaaa991ffbce0e75c45535ff316b4a148632c8674e0bdb0d28a1d05bf00"

let outer ?(hash = original_hash) ?(event_bytes = original_event) () =
  D.Document.inspect
    ~limits
    (`Object
        [ "format", `String "ochat.document"
        ; "schema_version", `Number "1"
        ; "kind", `String "store.audit_evidence"
        ; "future_evidence", `Null
        ; ( "payload"
          , `Object
              [ "previous_hash", `Null
              ; "record_hash", `String hash
              ; "event_document_bytes", `String event_bytes
              ; "future_chain", `Number "1e+00"
              ] )
        ])
  |> checked
;;

let admit document =
  Evidence.of_document document ~previous_hash:None ~next_sequence:1L ~limits
;;

let%expect_test "audit evidence hashes original event bytes and preserves unknown custody"
  =
  let original = outer () in
  let evidence = admit original |> store_checked in
  [%test_eq: string] original_event (Evidence.event_bytes evidence);
  [%test_eq: string] original_hash (Evidence.record_hash evidence);
  let projection = Evidence.event evidence |> Event.value in
  [%test_eq: int64] 1L projection.sequence;
  [%test_eq: string] "independent.named" projection.name;
  let republished = Evidence.to_document evidence ~limits |> checked in
  assert (Jsonaf.exactly_equal (D.Document.json original) (D.Document.json republished));
  let event = Event.to_document (Evidence.event evidence) ~limits |> checked in
  assert (not (String.equal original_event (D.Document.to_string event)));
  assert (
    Result.is_error
      (Evidence.of_document
         original
         ~previous_hash:(Some original_hash)
         ~next_sequence:1L
         ~limits));
  assert (
    Result.is_error
      (Evidence.of_document original ~previous_hash:None ~next_sequence:2L ~limits));
  print_endline
    "original lexical bytes and chain hash retained; projection never replaces evidence";
  [%expect
    {|original lexical bytes and chain hash retained; projection never replaces evidence|}]
;;

let%expect_test "audit integrity precedes inner semantics and optional IDs retain absence"
  =
  (match admit (outer ~event_bytes:"not a document" ()) with
   | Error (S.Store_error.Corrupt message) ->
     assert (String.is_substring message ~substring:"hash")
   | Ok _ | Error _ -> assert false);
  let doc = D.Document.decode ~limits original_event |> checked in
  let replace payload =
    match D.Document.json doc with
    | `Object fields ->
      D.Document.inspect
        ~limits
        (`Object
            (List.map fields ~f:(fun (name, value) ->
               name, if String.equal name "payload" then payload else value)))
      |> checked
    | _ -> assert false
  in
  let fields =
    match D.Document.payload doc with
    | `Object fields -> fields
    | _ -> assert false
  in
  let absent =
    Event.of_document doc ~limits
    |> checked
    |> fun event -> Event.to_document event ~limits |> checked
  in
  (match D.Json.field (D.Document.payload absent) ~name:"session_id" with
   | Absent -> ()
   | Null | Value _ -> assert false);
  assert (
    Result.is_error
      (Event.of_document (replace (`Object (fields @ [ "session_id", `Null ]))) ~limits));
  let invalid_sequence =
    `Object
      (List.map fields ~f:(fun (name, value) ->
         name, if String.equal name "sequence" then `String "0" else value))
  in
  assert (Result.is_error (Event.of_document (replace invalid_sequence) ~limits));
  print_endline
    "wrong original hash beats inner parse; absent IDs and opaque payload are distinct";
  [%expect
    {|wrong original hash beats inner parse; absent IDs and opaque payload are distinct|}]
;;

let%expect_test "audit semantics and complete authored evidence bounds fail closed" =
  let original = D.Document.decode ~limits original_event |> checked in
  let alter name value document =
    match D.Document.json document with
    | `Object fields ->
      D.Document.inspect
        ~limits
        (`Object (List.Assoc.add fields ~equal:String.equal name value))
      |> checked
    | _ -> assert false
  in
  let inner =
    alter "required_semantics" (`Array [ `String "unknown.audit.policy" ]) original
  in
  let bytes = D.Document.to_string inner in
  let hash = Digestif.SHA256.(digest_string ("\000" ^ bytes) |> to_hex) in
  assert (Result.is_error (admit (outer ~hash ~event_bytes:bytes ())));
  assert (
    Result.is_error
      (admit
         (alter
            "required_semantics"
            (`Array [ `String "unknown.chain.policy" ])
            (outer ()))));
  assert (
    Result.is_error
      (Event.of_document (alter "schema_version" (`Number "2") original) ~limits));
  let event = Event.of_document original ~limits |> checked in
  let small = S.Document_fields.limits ~max_bytes:400 |> checked in
  Event.to_document event ~limits:small |> checked |> ignore;
  assert (Result.is_error (Evidence.create event ~previous_hash:None ~limits:small));
  print_endline
    "unknown semantics/version refused; full escaped evidence bound precedes effects";
  [%expect
    {|unknown semantics/version refused; full escaped evidence bound precedes effects|}]
;;
