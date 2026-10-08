open! Core
module O = Inference.Observation
module R = Inference.Request
module E = Inference.Event
module T = Transcript
module D = Document_schema

let ok result =
  Result.map_error result ~f:(fun error -> Sexp.to_string_hum (O.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let scope ?(source = "source") ?(attempt = "attempt") relation =
  T.Scope.create
    ~source:(T.Source_id.of_string source |> Result.ok_or_failwith)
    ~attempt:(T.Attempt_id.of_string attempt |> Result.ok_or_failwith)
    ~relation
  |> Result.ok_or_failwith
;;

let root = scope Root
let id value = O.Observation_id.of_string value |> ok
let actual value = O.Count.create (Actual value) |> ok
let unknown reason = O.Count.create (Unknown reason) |> ok

let counts input =
  O.Usage.
    { input
    ; output = actual 0L
    ; reported_total = unknown Not_reported
    ; cached_input = unknown Not_reported
    ; cache_write_input = unknown Not_reported
    ; reasoning_output = unknown Explicit_null
    }
;;

let usage ?(inclusions = []) input =
  O.Usage.create ~counts:(counts input) ~inclusions |> ok
;;

let observation ?(scope = root) ?(id = "accounting") ?(revision = 0L) payload =
  O.create
    ~scope
    ~id:(O.Observation_id.of_string id |> ok)
    ~revision
    ~payload
    ~limits:O.Admission.observation
  |> ok
;;

let latest () = O.Latest.create ~max_observations:32 ~max_retained_bytes:(64 * 1024) |> ok

let bounded bytes =
  T.Admission.limits ~max_bytes:bytes
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let target settings =
  R.Target.create
    ~adapter:"synthetic"
    ~profile:"selected"
    ~profile_revision:(Some "revision-1")
    ~account:(Some "safe-account-alias")
    ~endpoint:"https://private-endpoint.test/path"
    ~model:"declared-model"
    ~settings
    ~limits:T.Admission.default
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (R.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let setting name value =
  R.Setting.create ~name ~value ~provenance:Execution_override ~limits:T.Admission.default
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (R.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let configuration
      ?(preparation_id = "host-preparation-1")
      ?(transport = O.Configuration.In_process)
      settings
  =
  O.Configuration.of_target
    (target settings)
    ~preparation_id
    ~transport
    ~capabilities:[ Text_input, Supported; Setting Max_output_tokens, Unknown ]
    ~limits:O.Admission.observation
  |> ok
;;

let record
      ?(scope = root)
      ?(accounting_id = "accounting")
      ?(state = O.Attempt_record.Running)
      ?(omitted_diagnostics = 0L)
      configuration
      observations
  =
  O.Attempt_record.create
    ~scope
    ~accounting_id:(id accounting_id)
    ~configuration
    ~state
    ~observations
    ~omitted_diagnostics
    ~limits:O.Admission.attempt
;;

let replace json name value =
  match json with
  | `Object fields ->
    `Object
      (List.map fields ~f:(fun (key, old) ->
         key, if String.equal name key then value else old))
  | _ -> failwith "test object required"
;;

let%expect_test "mixed unknown components and actual zero roundtrip distinctly" =
  let payload = usage (actual 7L) in
  let observed = observation (Usage payload) in
  let restored = O.of_json (O.to_json observed) ~limits:O.Admission.observation |> ok in
  assert (O.equal observed restored);
  let print component =
    match O.Count.view (O.Usage.count payload component) with
    | Actual count -> print_s (Sexp.List [ Sexp.Atom "Actual"; Int64.sexp_of_t count ])
    | Unknown reason ->
      print_s (Sexp.List [ Sexp.Atom "Unknown"; O.Count.sexp_of_unknown_reason reason ])
    | Estimated _ -> failwith "unexpected estimated fixture"
  in
  List.iter [ O.Usage.Component.Input; Output; Cached_input; Reasoning_output ] ~f:print;
  [%expect
    {|
    (Actual 7)
    (Actual 0)
    (Unknown Not_reported)
    (Unknown Explicit_null)
    |}]
;;

let%expect_test "inclusion checks transitive actual bounds across unknown components" =
  let initial_counts = counts (actual 5L) in
  let counts = { initial_counts with cached_input = actual 6L } in
  let edges =
    O.Usage.
      [ { subset = Cached_input; included_in = Cache_write_input }
      ; { subset = Cache_write_input; included_in = Input }
      ]
  in
  assert (Result.is_error (O.Usage.create ~counts ~inclusions:edges));
  assert (
    Result.is_error (O.Usage.create ~counts ~inclusions:(List.hd_exn edges :: edges)));
  assert (
    Result.is_error
      (O.Usage.create
         ~counts
         ~inclusions:
           O.Usage.
             [ { subset = Input; included_in = Output }
             ; { subset = Output; included_in = Input }
             ]));
  let admitted =
    O.Usage.create ~counts:{ counts with cached_input = actual 5L } ~inclusions:edges
    |> ok
  in
  let observed = observation (Usage admitted) in
  assert (
    O.equal observed (O.of_json (O.to_json observed) ~limits:O.Admission.observation |> ok));
  assert (Result.is_error (O.Count.create (Actual (-1L))));
  print_endline "transitive bounds, cycles, duplicate edges and negative counts checked";
  [%expect {| transitive bounds, cycles, duplicate edges and negative counts checked |}]
;;

let%expect_test "authoritative revisions replace rather than accumulating consumption" =
  let first = observation (Usage (usage (actual 10L))) in
  let owner, added = O.Latest.observe (latest ()) first |> ok in
  let owner, duplicate = O.Latest.observe owner first |> ok in
  let newer = observation ~revision:2L (Usage (usage (actual 4L))) in
  let owner, replaced = O.Latest.observe owner newer |> ok in
  let stale = observation ~revision:1L (Usage (usage (actual 100L))) in
  let owner, ignored = O.Latest.observe owner stale |> ok in
  let conflict = observation ~revision:2L (Usage (usage (actual 99L))) in
  assert (Result.is_error (O.Latest.observe owner conflict));
  let correction = observation ~revision:3L (Usage (usage (unknown Interrupted))) in
  let owner, _ = O.Latest.observe owner correction |> ok in
  assert (O.equal correction (O.Latest.find owner (O.key first) |> Option.value_exn));
  assert (Int.equal (O.Latest.retained_bytes owner) (O.encoded_bytes correction));
  print_s
    [%sexp
      ((added, duplicate, replaced, ignored)
       : O.Latest.disposition
         * O.Latest.disposition
         * O.Latest.disposition
         * O.Latest.disposition)];
  print_endline "downward and unknown corrections retained without double counting";
  [%expect
    {|
    (Added Duplicate Replaced Stale)
    downward and unknown corrections retained without double counting
    |}]
;;

let%expect_test "complete parent relation is stable across revision and observation IDs" =
  let first = observation (Usage (usage (actual 1L))) in
  let owner, _ = O.Latest.observe (latest ()) first |> ok in
  let parent = scope ~source:"parent-source" ~attempt:"parent-attempt" Root in
  let changed =
    scope
      (Nested
         { scope = T.Scope.key parent
         ; call_entry_id = None
         ; call_alias = Some "actual-parent"
         })
  in
  let incoming = observation ~scope:changed ~revision:1L (Usage (usage (actual 1L))) in
  assert (Result.is_error (O.Latest.observe owner incoming));
  let other =
    observation ~scope:changed ~id:"another-observer" (Usage (usage (actual 1L)))
  in
  assert (Result.is_error (O.Latest.observe owner other));
  assert (List.length (O.Latest.observations owner) = 1);
  print_endline
    "same key cannot acquire a different parent; nested attempts remain isolated";
  [%expect
    {| same key cannot acquire a different parent; nested attempts remain isolated |}]
;;

let%expect_test "configuration exposes closed safe values and captures actual preparation"
  =
  let config =
    configuration
      [ setting "instructions" (Value (`String "PRIVATE-INSTRUCTIONS"))
      ; setting "prompt_cache_key" (Value (`String "PRIVATE-CACHE-KEY"))
      ; setting "unknown-PRIVATE-NAME" (Value (`String "PRIVATE-VALUE"))
      ; setting "temperature" Null
      ; setting "max_output_tokens" (Value (`Number "123"))
      ; setting
          "reasoning"
          (Value (`Object [ "effort", `String "high"; "summary", `Null ]))
      ; setting
          "text"
          (Value
             (`Object
                 [ "verbosity", `String "low"
                 ; ( "format"
                   , `Object
                       [ "type", `String "json_schema"
                       ; "schema", `String "PRIVATE-SCHEMA"
                       ] )
                 ]))
      ]
  in
  let observed = observation ~id:"configuration" (Configuration config) in
  let json = Jsonaf.to_string (O.to_json observed) in
  List.iter [ "PRIVATE"; "private-endpoint"; "https://" ] ~f:(fun private_text ->
    assert (not (String.is_substring json ~substring:private_text)));
  assert (O.Configuration.withheld_settings config = 1);
  assert (String.equal (O.Configuration.preparation_id config) "host-preparation-1");
  assert (O.Configuration.equal_transport (O.Configuration.transport config) In_process);
  assert (
    O.equal observed (O.of_json (O.to_json observed) ~limits:O.Admission.observation |> ok));
  let safe_json = O.Configuration.to_json config in
  assert (
    O.Configuration.equal
      config
      (O.Configuration.of_json safe_json ~limits:O.Admission.observation |> ok));
  let safe_bytes =
    D.Json.validate_and_measure ~limits:O.Admission.observation safe_json
    |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  assert (Result.is_ok (O.Configuration.of_json safe_json ~limits:(bounded safe_bytes)));
  assert (
    Result.is_error (O.Configuration.of_json safe_json ~limits:(bounded (safe_bytes - 1))));
  let find name =
    List.find_exn (O.Configuration.settings config) ~f:(fun setting ->
      O.Configuration.Name.equal name setting.name)
  in
  (match
     ( (find Temperature).selection
     , (find Instructions).selection
     , (find Max_output_tokens).selection )
   with
   | Explicit_null, Withheld, Value (Tokens 123L) -> ()
   | _ -> failwith "incorrect safe configuration");
  let owner, _ = O.Latest.observe (latest ()) observed |> ok in
  let identical = observation ~id:"configuration" ~revision:1L (Configuration config) in
  let owner, disposition = O.Latest.observe owner identical |> ok in
  assert (O.Latest.equal_disposition disposition Replaced);
  let shuffled =
    match O.to_json identical with
    | `Object fields as json ->
      let payload = List.Assoc.find_exn fields "payload" ~equal:String.equal in
      (match payload with
       | `Object fields ->
         (match List.Assoc.find_exn fields "capabilities" ~equal:String.equal with
          | `Array values ->
            replace
              json
              "payload"
              (replace payload "capabilities" (`Array (List.rev values)))
          | _ -> failwith "capability array required")
       | _ -> failwith "configuration object required")
    | _ -> failwith "observation object required"
  in
  let shuffled = O.of_json shuffled ~limits:O.Admission.observation |> ok in
  assert (O.equal identical shuffled);
  let _, disposition = O.Latest.observe owner shuffled |> ok in
  assert (O.Latest.equal_disposition disposition Duplicate);
  let changed = configuration ~transport:Http_sse [] in
  assert (
    Result.is_error
      (O.Latest.observe
         owner
         (observation ~id:"configuration" ~revision:0L (Configuration changed))));
  assert (
    Result.is_error
      (O.Latest.observe owner (observation ~id:"another-config" (Configuration changed))));
  assert (
    Result.is_error
      (record config [ observation ~id:"another-config" (Configuration changed) ]));
  print_endline
    "private data withheld; safe settings and transport roundtrip; immutable \
     configuration enforced";
  [%expect
    {| private data withheld; safe settings and transport roundtrip; immutable configuration enforced |}]
;;

let%expect_test "context is estimated and tied to an opaque actual preparation identity" =
  let estimator = O.Estimator.create ~method_:O200k_serialized_history ~version:1 |> ok in
  let count = O.Count.create (Estimated { tokens = 100L; estimator }) |> ok in
  let context =
    O.Context_estimate.create
      ~preparation_id:"host-preparation-1"
      ~count
      ~capacity:(Declared 1000L)
    |> ok
  in
  let observed = observation ~id:"context" (Context_estimate context) in
  assert (
    O.equal observed (O.of_json (O.to_json observed) ~limits:O.Admission.observation |> ok));
  assert (
    Result.is_error
      (O.Context_estimate.create
         ~preparation_id:"host-preparation-1"
         ~count:(actual 100L)
         ~capacity:Unknown));
  assert (
    Result.is_error
      (O.Context_estimate.create
         ~preparation_id:"host-preparation-1"
         ~count
         ~capacity:(Declared 0L)));
  let config = configuration [] in
  ignore (record config [ observed ] |> ok : O.Attempt_record.t);
  let changed =
    O.Context_estimate.create
      ~preparation_id:"host-preparation-2"
      ~count
      ~capacity:Unknown
    |> ok
  in
  assert (
    Result.is_error
      (record config [ observation ~id:"context" (Context_estimate changed) ]));
  print_endline
    "context estimates remain separate from actual usage and captured preparation";
  [%expect
    {| context estimates remain separate from actual usage and captured preparation |}]
;;

let%expect_test "whole admission and bounded latest updates fail atomically" =
  let first = observation (Usage (usage (actual 1L))) in
  let bytes = O.encoded_bytes first in
  assert (Result.is_ok (O.of_json (O.to_json first) ~limits:(bounded bytes)));
  assert (Result.is_error (O.of_json (O.to_json first) ~limits:(bounded (bytes - 1))));
  assert (
    Result.is_error
      (O.of_json
         (replace (O.to_json first) "revision" (`String "01"))
         ~limits:O.Admission.observation));
  (match O.to_json first with
   | `Object fields ->
     assert (
       Result.is_error
         (O.of_json
            (`Object (("secret", `String "private") :: fields))
            ~limits:O.Admission.observation));
     assert (
       Result.is_error
         (O.of_json
            (`Object (("revision", `String "0") :: fields))
            ~limits:O.Admission.observation))
   | _ -> assert false);
  let owner = O.Latest.create ~max_observations:1 ~max_retained_bytes:bytes |> ok in
  let owner, _ = O.Latest.observe owner first |> ok in
  assert (
    Result.is_error
      (O.Latest.observe owner (observation ~id:"other" (Usage (usage (actual 1L))))));
  assert (O.equal first (O.Latest.find owner (O.key first) |> Option.value_exn));
  assert (Result.is_error (O.Observation_id.of_string (String.make 511 'x')));
  assert (Result.is_ok (O.Observation_id.of_string (String.make 510 'x')));
  print_endline
    "exact bytes, duplicate fields, closed vocabulary and atomic retention limits checked";
  [%expect
    {| exact bytes, duplicate fields, closed vocabulary and atomic retention limits checked |}]
;;

let diagnostic () =
  O.Diagnostic.create
    ~phase:Dispatch
    ~reason:(Limit Response_body_bytes)
    ~delivery:(Some Response_started)
    ~elapsed_ms:(Some 0L)
  |> ok
;;

let%expect_test
    "attempt rows preserve real interrupted state and designated accounting identity"
  =
  let config = configuration [] in
  let state =
    O.Attempt_record.Interrupted { reason = Cancelled; delivery = Possibly_submitted }
  in
  let row = record ~state config [] |> ok in
  let restored =
    O.Attempt_record.of_json (O.Attempt_record.to_json row) ~limits:O.Admission.attempt
    |> ok
  in
  (match O.Attempt_record.state restored with
   | Interrupted { reason = Cancelled; delivery = Possibly_submitted } -> ()
   | _ -> assert false);
  assert (
    O.Observation_id.equal (O.Attempt_record.accounting_id restored) (id "accounting"));
  assert (
    Result.is_error
      (record config [ observation ~id:"not-accounting" (Usage (usage (actual 0L))) ]));
  let terminal =
    E.Terminal.create ~scope:root ~delivery:Response_started ~outcome:Completed
    |> Result.ok_or_failwith
  in
  ignore
    (record ~state:(Terminal terminal) config [ observation (Usage (usage (actual 0L))) ]
     |> ok
     : O.Attempt_record.t);
  let foreign = scope ~source:"other" Root in
  let foreign_terminal =
    E.Terminal.create ~scope:foreign ~delivery:Response_started ~outcome:Completed
    |> Result.ok_or_failwith
  in
  assert (Result.is_error (record ~state:(Terminal foreign_terminal) config []));
  assert (
    Result.is_error
      (O.Diagnostic.create
         ~phase:Dispatch
         ~reason:(Http_status 99)
         ~delivery:None
         ~elapsed_ms:None));
  print_endline
    "interruption is explicit; absent usage stays absent; provider completion never \
     asserts a host turn commit";
  [%expect
    {| interruption is explicit; absent usage stays absent; provider completion never asserts a host turn commit |}]
;;

let%expect_test "diagnostic rings are bounded by both entries and encoded bytes" =
  let config = configuration [] in
  let observations scope count =
    List.init count ~f:(fun index ->
      observation
        ~scope
        ~id:("diagnostic-" ^ Int.to_string index)
        (Diagnostic (diagnostic ())))
  in
  ignore (record config (observations root 16) |> ok : O.Attempt_record.t);
  assert (Result.is_error (record config (observations root 17)));
  let large_scope =
    scope ~source:(String.make 400 's') ~attempt:(String.make 400 'a') Root
  in
  let large = observations large_scope 16 in
  assert (
    List.fold large ~init:0 ~f:(fun bytes observation ->
      bytes + O.encoded_bytes observation)
    > 16 * 1024);
  assert (Result.is_error (record ~scope:large_scope config large));
  let observation = observation (Diagnostic (diagnostic ())) in
  assert (
    O.equal
      observation
      (O.of_json (O.to_json observation) ~limits:O.Admission.diagnostic |> ok));
  print_endline
    "entry and byte overflow reject; static typed diagnostic roundtrip stays bounded";
  [%expect
    {| entry and byte overflow reject; static typed diagnostic roundtrip stays bounded |}]
;;

let%expect_test "integral floating settings encode valid JSON through safe configuration" =
  List.iter
    [ "temperature", "0"
    ; "temperature", "1.0"
    ; "temperature", "2"
    ; "top_p", "0"
    ; "top_p", "1.0"
    ]
    ~f:(fun (name, value) ->
      let config = configuration [ setting name (Value (`Number value)) ] in
      let encoded = O.Configuration.to_json config in
      assert (Result.is_ok (D.Json.validate ~limits:O.Admission.observation encoded));
      assert (
        O.Configuration.equal
          config
          (O.Configuration.of_json encoded ~limits:O.Admission.observation |> ok)));
  print_endline "zero, one and endpoint temperatures/probabilities remain valid JSON";
  [%expect {| zero, one and endpoint temperatures/probabilities remain valid JSON |}]
;;

let%test_unit
    "attempt admission binds all profile bounds and checks permissive native rows"
  =
  let profile
        ?(max_bytes = 64 * 1024)
        ?(max_depth = 160)
        ?(max_fields = 1_000_000)
        ?(max_nodes = 2_000_000)
        ()
    =
    D.Limits.create ~max_bytes ~max_depth ~max_fields ~max_nodes
    |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  let config = configuration [] in
  let row = record config [ observation (Usage (usage (actual 0L))) ] |> ok in
  let raw = O.Attempt_record.to_json row in
  let original = Jsonaf.to_string raw in
  O.Attempt_record.validate row ~limits:(profile ()) |> ok;
  O.Attempt_record.validate row ~limits:(profile ~max_depth:161 ()) |> ok;
  List.iter
    [ "bytes", profile ~max_bytes:1 ()
    ; "depth", profile ~max_depth:1 ()
    ; "fields", profile ~max_fields:1 ()
    ; "nodes", profile ~max_nodes:1 ()
    ]
    ~f:(fun (bound, limits) ->
      let reject = function
        | Error (O.Error.Json error) ->
          assert (D.Error.equal error (Limit_exceeded bound))
        | Error _ | Ok _ -> failwith "complete requested profile was not applied"
      in
      reject (O.Attempt_record.validate row ~limits);
      reject (O.Attempt_record.of_json raw ~limits));
  assert (String.equal original (Jsonaf.to_string (O.Attempt_record.to_json row)));
  let large_scope =
    scope ~source:(String.make 400 's') ~attempt:(String.make 400 'a') Root
  in
  let large_config = configuration ~preparation_id:(String.make 500 'p') [] in
  let observations =
    List.init 64 ~f:(fun index ->
      observation
        ~scope:large_scope
        ~id:("configuration-" ^ Int.to_string index)
        (Configuration large_config))
  in
  let permissive =
    O.Attempt_record.create
      ~scope:large_scope
      ~accounting_id:(id "accounting")
      ~configuration:large_config
      ~state:Running
      ~observations
      ~omitted_diagnostics:0L
      ~limits:(profile ~max_bytes:(512 * 1024) ())
    |> ok
  in
  assert (O.Attempt_record.encoded_bytes permissive > 64 * 1024);
  assert (
    Result.is_error (O.Attempt_record.validate permissive ~limits:O.Admission.attempt));
  assert (
    Result.is_error
      (O.Attempt_record.of_json
         (O.Attempt_record.to_json permissive)
         ~limits:O.Admission.attempt))
;;

let%test_unit
    "attempt constructor relationships match decoder guards before profile reuse"
  =
  let config = configuration [] in
  let row = record config [] |> ok in
  let raw = O.Attempt_record.to_json row in
  let rejects_observations observations =
    assert (Result.is_error (record config observations));
    assert (
      Result.is_error
        (O.Attempt_record.of_json
           (replace raw "observations" (`Array (List.map observations ~f:O.to_json)))
           ~limits:O.Admission.attempt))
  in
  let observed = observation (Usage (usage (actual 1L))) in
  rejects_observations [ observed; observed ];
  rejects_observations [ observation ~id:"wrong-accounting" (Usage (usage (actual 1L))) ];
  let foreign = scope ~source:"foreign" Root in
  rejects_observations [ observation ~scope:foreign (Usage (usage (actual 1L))) ];
  let foreign_config = configuration ~preparation_id:"another-preparation" [] in
  rejects_observations [ observation ~id:"configuration" (Configuration foreign_config) ];
  let context =
    O.Context_estimate.create
      ~preparation_id:"another-preparation"
      ~count:(unknown Not_reported)
      ~capacity:Unknown
    |> ok
  in
  rejects_observations [ observation ~id:"context" (Context_estimate context) ];
  let terminal =
    E.Terminal.create ~scope:foreign ~delivery:Response_started ~outcome:Completed
    |> Result.ok_or_failwith
  in
  assert (Result.is_error (record ~state:(Terminal terminal) config []));
  assert (
    Result.is_error
      (O.Attempt_record.of_json
         (replace
            raw
            "state"
            (`Object
                [ "kind", `String "terminal"; "terminal", E.Terminal.to_json terminal ]))
         ~limits:O.Admission.attempt));
  let malformed =
    match raw with
    | `Object fields -> `Object (fields @ [ "future_private", `String "refused" ])
    | _ -> assert false
  in
  assert (Result.is_error (O.Attempt_record.of_json malformed ~limits:O.Admission.attempt));
  assert (
    Result.is_error
      (O.Attempt_record.of_json
         (replace raw "schema_version" (`Number "2"))
         ~limits:O.Admission.attempt));
  let parent = T.Scope.{ scope = key root; call_entry_id = None; call_alias = Some "" } in
  assert (
    Result.is_error
      (T.Scope.create
         ~source:(T.Source_id.of_string "child" |> Result.ok_or_failwith)
         ~attempt:(T.Attempt_id.of_string "child" |> Result.ok_or_failwith)
         ~relation:(Nested parent)));
  O.Attempt_record.validate row ~limits:O.Admission.attempt |> ok
;;

let%expect_test
    "protocol diagnostics retain only closed detail and legacy schema stays strict"
  =
  let module V = O.Diagnostic.Protocol_violation in
  let detail = { V.stage = Feed; kind = Tracker Terminal_mismatch } in
  let diagnostic =
    O.Diagnostic.create
      ~phase:Stream
      ~reason:(Protocol_violation detail)
      ~delivery:(Some Response_started)
      ~elapsed_ms:None
    |> ok
  in
  let value = observation (Diagnostic diagnostic) in
  let wire = O.to_json value in
  assert (O.equal value (O.of_json wire ~limits:O.Admission.diagnostic |> ok));
  print_endline (Jsonaf.to_string (V.to_json detail));
  assert (
    Result.is_error
      (V.of_json
         (`Object
             [ "stage", `String "feed"
             ; "kind", `String "tracker"
             ; "detail", `String "terminal_mismatch"
             ; "raw", `String "PRIVATE"
             ])));
  assert (
    Result.is_error
      (V.of_json
         (`Object
             [ "stage", `String "feed"
             ; "stage", `String "eof"
             ; "kind", `String "framing"
             ; "detail", `Null
             ])));
  let reason_extra =
    `Object [ "kind", `String "malformed_protocol"; "detail", V.to_json detail ]
  in
  let changed =
    match wire with
    | `Object fields ->
      `Object
        (List.map fields ~f:(fun (key, payload) ->
           if String.equal key "payload"
           then
             ( key
             , match payload with
               | `Object inner ->
                 `Object
                   (List.map inner ~f:(fun (name, old) ->
                      name, if String.equal name "reason" then reason_extra else old))
               | _ -> assert false )
           else key, payload))
    | _ -> assert false
  in
  (* Construct the legacy malformed reason with forbidden detail via actual payload key. *)
  assert (Result.is_error (O.of_json changed ~limits:O.Admission.diagnostic));
  print_endline "bounded closed detail roundtrips; unknown and duplicate fields reject";
  [%expect
    {|
{"stage":"feed","kind":"tracker","detail":"terminal_mismatch"}
bounded closed detail roundtrips; unknown and duplicate fields reject
|}]
;;

let%test_unit "HTTP rejection diagnostic roundtrip is closed and rejects duplicate detail"
  =
  let module H = O.Diagnostic.Http_rejection in
  let rejection =
    H.create ~status:400 ~reason:Missing_required_parameter ~parameter:(Some Instructions)
    |> ok
  in
  let diagnostic =
    O.Diagnostic.create
      ~phase:Dispatch
      ~reason:(Http_rejection rejection)
      ~delivery:(Some Possibly_submitted)
      ~elapsed_ms:None
    |> ok
  in
  let original = observation (Diagnostic diagnostic) in
  let wire = O.to_json original in
  assert (O.equal original (O.of_json wire ~limits:O.Admission.observation |> ok));
  assert (
    Result.is_error
      (H.of_json
         (`Object
             [ "status", `Number "400"
             ; "reason", `String "unclassified"
             ; "parameter", `Null
             ; "parameter", `String "instructions"
             ])));
  assert (
    Result.is_error
      (H.of_json
         (`Object
             [ "status", `Number "400"
             ; "reason", `String "SECRET_CANARY"
             ; "parameter", `Null
             ])));
  assert (not (String.is_substring (Jsonaf.to_string wire) ~substring:"SECRET_CANARY"))
;;
