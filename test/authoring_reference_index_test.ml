open Core
module R = Chat_response.Authoring_reference_index
module Presence = Chat_response.Authoring_presence
module G = Agent_protocol.Authoring_guidance
module H = Agent_protocol.History
module F = Authoring_materialization_test

let ok = F.protocol_ok
let digest = F.digest
let scope = "rediscovery-session:0"

let entry ?(source = G.Installed (digest "corpus")) number =
  let payload =
    `Object [ "role", `String "user"; "content", `String "REFERENCE-BODY-DO-NOT-RETAIN" ]
  in
  let guidance =
    G.create
      ~context_identity:(digest "context")
      ~policy_fingerprint:(digest "policy")
      ~purpose:Reference
      ~payload
      ~topics:
        [ { id = "topic-" ^ Int.to_string number
          ; document_sha256 = digest (Int.to_string number)
          ; source
          ; complete = true
          }
        ]
    |> ok
  in
  H.
    { id = History_entry.Id.create ~namespace:"rediscovery" ~sequence:number |> F.ok
    ; role = User
    ; kind = Message
    ; payload
    ; provenance = Runtime_authoring guidance
    ; redacted = false
    }
;;

let topics index =
  List.concat_map (R.receipts index) ~f:(fun receipt ->
    List.map receipt.Presence.guidance.topics ~f:(fun topic -> topic.G.id))
;;

let%expect_test "bounded chronological receipts survive restore without retaining prose" =
  let limits = R.{ max_receipts = 3; max_bytes = 65536 } in
  let initial = R.empty ~limits ~scope () |> ok in
  let index = R.remember ~limits initial ~history:(List.init 6 ~f:entry) |> ok in
  print_s [%sexp (topics index : string list), (R.truncated index : bool)];
  let index = R.remember ~limits index ~history:[ entry 4; entry 6 ] |> ok in
  print_s [%sexp (topics index : string list), (R.truncated index : bool)];
  let encoded = R.to_json index in
  assert (not (String.is_substring (Jsonaf.to_string encoded) ~substring:"REFERENCE-BODY"));
  assert (R.encoded_bytes index <= limits.max_bytes);
  let restored = R.of_json ~limits ~scope encoded |> ok in
  let repeated = R.remember ~limits restored ~history:[ entry 4; entry 6 ] |> ok in
  assert (Jsonaf.exactly_equal encoded (R.to_json repeated));
  let one = R.remember initial ~history:[ entry 6 ] |> ok in
  let byte_limit = R.{ max_receipts = 64; max_bytes = R.encoded_bytes one + 1 } in
  let bounded = R.remember ~limits:byte_limit restored ~history:[] |> ok in
  assert (R.encoded_bytes bounded <= byte_limit.max_bytes);
  print_s [%sexp (topics bounded : string list), (R.truncated bounded : bool)];
  let disabled =
    R.remember ~limits:{ limits with max_receipts = 0 } bounded ~history:[] |> ok
  in
  print_s [%sexp (topics disabled : string list), (R.truncated disabled : bool)];
  [%expect
    {|
    ((topic-3 topic-4 topic-5) true)
    ((topic-5 topic-4 topic-6) true)
    ((topic-6) true)
    (() true)
    |}]
;;

let%expect_test
    "only host provenance adds receipts and redaction removes remembered metadata"
  =
  let first = entry 1 in
  let authored = entry ~source:(G.Authored (digest "private-package")) 2 in
  let initial = R.empty ~scope () |> ok in
  let index = R.remember initial ~history:[ first; authored ] |> ok in
  let source = (List.last_exn (R.receipts index)).guidance.topics |> List.hd_exn in
  (match source.source with
   | G.Authored _ -> ()
   | Installed _ -> assert false);
  let modified = { (entry 3) with payload = `String "modified" } in
  let lookalike = { (entry 4) with provenance = Canonical } in
  let redacted = { first with redacted = true } in
  let index = R.remember index ~history:[ modified; lookalike; redacted ] |> ok in
  print_s [%sexp (topics index : string list)];
  let changed = entry ~source:(G.Installed (digest "another-corpus")) 2 in
  assert (Result.is_error (R.remember index ~history:[ changed ]));
  assert (Result.is_error (R.remember index ~history:[ authored; authored ]));
  print_s [%sexp (topics index : string list)];
  [%expect
    {|
    (topic-2)
    (topic-2)
    |}]
;;

let%expect_test
    "restoration rejects changed scope, invalid envelopes, duplicates and oversized state"
  =
  let index =
    R.empty ~scope ()
    |> ok
    |> fun index -> R.remember index ~history:[ entry 1; entry 2 ] |> ok
  in
  let encoded = R.to_json index in
  let change name value =
    match encoded with
    | `Object fields ->
      `Object
        (List.map fields ~f:(fun (key, original) ->
           key, if String.equal key name then value else original))
    | _ -> assert false
  in
  let receipts = Jsonaf.member_exn "receipts" encoded |> Jsonaf.list_exn in
  List.iter
    [ "scope", R.of_json ~scope:"another-session:0" encoded
    ; "version", R.of_json ~scope (change "version" (`Number "2"))
    ; ( "duplicate"
      , R.of_json
          ~scope
          (change "receipts" (`Array [ List.hd_exn receipts; List.hd_exn receipts ])) )
    ; ( "unknown"
      , R.of_json
          ~scope
          (match encoded with
           | `Object fields -> `Object (("extra", `Null) :: fields)
           | _ -> assert false) )
    ; "count", R.of_json ~scope ~limits:{ R.default_limits with max_receipts = 1 } encoded
    ; ( "bytes"
      , R.of_json
          ~scope
          ~limits:{ R.default_limits with max_bytes = R.encoded_bytes index - 1 }
          encoded )
    ]
    ~f:(fun (label, result) ->
      assert (Result.is_error result);
      print_endline (label ^ " rejected"));
  [%expect
    {|
    scope rejected
    version rejected
    duplicate rejected
    unknown rejected
    count rejected
    bytes rejected
    |}]
;;

let%expect_test
    "retained receipt metadata never satisfies missing or stale effective context"
  =
  F.fixture (fun context host _ plan ->
    let policy = plan Auto in
    let materialization =
      Chat_response.Authoring_materialization.create
        ~context
        ~host
        ~policy
        ~capabilities:(Chat_response.Authoring_policy.capabilities policy)
        ~scope
        ()
      |> F.ok
    in
    let entries =
      Chat_response.Authoring_materialization.initial materialization |> F.entries
    in
    let index =
      R.empty ~scope () |> ok |> fun index -> R.remember index ~history:entries |> ok
    in
    let known = R.of_json ~scope (R.to_json index) |> ok |> R.receipts in
    let inspect identity effective =
      Presence.inspect ~policy ~context_identity:identity ~known ~effective |> ok
    in
    let current =
      Chat_response.Authoring_materialization.context_identity materialization
    in
    let absent = inspect current [] in
    assert absent.refresh_primer;
    assert (
      List.for_all absent.observations ~f:(fun value ->
        Presence.equal_presence value.presence Absent));
    let stale = inspect (digest "new-runtime") entries in
    assert stale.refresh_primer;
    assert (
      List.for_all stale.observations ~f:(fun value ->
        Presence.equal_presence value.presence Stale_context));
    let present = inspect current entries in
    assert (not present.refresh_primer);
    print_endline
      "restored index: absent/stale prose requires refresh; only matching effective \
       content is present");
  [%expect
    {| restored index: absent/stale prose requires refresh; only matching effective content is present |}]
;;
