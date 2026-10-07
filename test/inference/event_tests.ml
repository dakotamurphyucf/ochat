open! Core
module E = Inference.Event
module T = Transcript
module P = History_entry.Payload

let limits = T.Admission.default

let document_ok result =
  Result.map_error result ~f:(fun error ->
    Sexp.to_string_hum (Document_schema.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let ok = Result.ok_or_failwith

let scope relation =
  T.Scope.create
    ~source:(T.Source_id.of_string "actual-source" |> ok)
    ~attempt:(T.Attempt_id.of_string "actual-attempt" |> ok)
    ~relation
  |> ok
;;

let root = scope Root

let item ?(entry_id = None) ?(call_name = None) header =
  T.Item.create
    ~scope:root
    ~id:(T.Item_id.of_string "actual-item" |> ok)
    ~entry_id
    ~header
    ~call_name
  |> ok
;;

let payload view = P.Semantic.create view ~metadata:P.Metadata.empty |> ok |> P.authored

let%expect_test
    "provider evidence cannot publish a canonical commit or finish local outputs"
  =
  let payload = payload (Unknown { provider_kind = "synthetic.future" }) in
  let id = History_entry.Id.create ~namespace:"host" ~sequence:1 |> ok in
  let descriptor = item ~entry_id:(Some id) (Some (Unknown "synthetic.future")) in
  let candidate =
    E.create
      (Candidate_ready { item = descriptor; payload; local_execution = Not_eligible })
      ~limits
    |> ok
  in
  assert (T.Scope.equal root (E.scope candidate));
  let entry = History_entry.create_with_id ~id payload in
  let finalized =
    T.Stream.create (Item_finalized { item = descriptor; entry }) ~limits |> ok
  in
  let finished =
    T.Stream.create (Source_finished { scope = root; completion = Complete }) ~limits
    |> ok
  in
  assert (Result.is_error (E.create (Live finalized) ~limits));
  assert (Result.is_error (E.create (Live finished) ~limits));
  print_endline
    "candidate evidence admitted; committed finalization and local finish rejected";
  [%expect
    {| candidate evidence admitted; committed finalization and local finish rejected |}]
;;

let%test_unit "candidate requires actual known matching header and call name" =
  let call =
    payload
      (Call
         { kind = Custom
         ; name = "inspect"
         ; namespace = Absent
         ; input_bytes = " { exact bytes } "
         ; async = Absent
         })
  in
  assert (
    Result.is_error
      (E.create
         (Candidate_ready
            { item = item None; payload = call; local_execution = Tool_candidate })
         ~limits));
  assert (
    Result.is_error
      (E.create
         (Candidate_ready
            { item = item ~call_name:(Some "other") (Some (Call Custom))
            ; payload = call
            ; local_execution = Tool_candidate
            })
         ~limits));
  let descriptor = item ~call_name:(Some "inspect") (Some (Call Custom)) in
  let candidate =
    E.create
      (Candidate_ready
         { item = descriptor; payload = call; local_execution = Tool_candidate })
      ~limits
    |> ok
  in
  match E.view candidate with
  | Candidate_ready { payload = retained; _ } ->
    assert (Jsonaf.exactly_equal (P.to_json call) (P.to_json retained))
  | Live _ | Terminal _ -> assert false
;;

let%test_unit "captured candidate retains unknown payload and configured aggregate bounds"
  =
  let semantic =
    P.Semantic.create
      (Unknown { provider_kind = "synthetic.future" })
      ~metadata:P.Metadata.empty
    |> ok
  in
  let origin =
    P.Origin.create
      ~adapter:"synthetic"
      ~provider:"selected"
      ~account:None
      ~endpoint:"https://example.test"
      ~profile:None
      ~model:None
      ~replay_version:1
    |> ok
  in
  let raw = `Object [ "future", `Object [ "exact", `Number "1e+00"; "null", `Null ] ] in
  let payload = P.captured semantic ~origin ~raw |> ok in
  let view =
    E.Candidate_ready
      { item = item (Some (Unknown "synthetic.future"))
      ; payload
      ; local_execution = Not_eligible
      }
  in
  let event = E.create view ~limits |> ok in
  let bounded =
    Document_schema.Limits.create
      ~max_bytes:(E.encoded_bytes event - 1)
      ~max_depth:160
      ~max_fields:1000000
      ~max_nodes:2000000
    |> document_ok
  in
  assert (Result.is_error (E.create view ~limits:bounded));
  match E.view event with
  | Candidate_ready { payload = retained; _ } ->
    assert (Jsonaf.exactly_equal (P.to_json payload) (P.to_json retained))
  | Live _ | Terminal _ -> assert false
;;

let%test_unit "presentation headroom cannot admit invalid canonical native payload" =
  let raw =
    List.init 125 ~f:Fn.id |> List.fold ~init:`Null ~f:(fun nested _ -> `Array [ nested ])
  in
  let payload =
    payload
      (Message
         { form = Input
         ; role = Developer
         ; content = [ Unknown { kind = "future.deep"; raw } ]
         ; phase = Absent
         })
  in
  assert (Result.is_error (P.validate payload));
  assert (
    Result.is_error
      (E.create
         (Candidate_ready
            { item = item (Some (Message Developer))
            ; payload
            ; local_execution = Not_eligible
            })
         ~limits))
;;

let%expect_test "terminal restore validates delivery and complete parent identity" =
  let parent_scope =
    T.Scope.create
      ~source:(T.Source_id.of_string "parent-source" |> ok)
      ~attempt:(T.Attempt_id.of_string "parent-attempt" |> ok)
      ~relation:Root
    |> ok
  in
  let parent =
    T.Scope.
      { scope = T.Scope.key parent_scope
      ; call_entry_id = None
      ; call_alias = Some "parent-call"
      }
  in
  let nested = scope (Nested parent) in
  let terminal =
    E.Terminal.create ~scope:nested ~delivery:Response_started ~outcome:(Incomplete Other)
    |> ok
  in
  let restored = E.Terminal.of_json (E.Terminal.to_json terminal) ~limits |> ok in
  assert (E.Terminal.equal terminal restored);
  assert (not (T.Scope.equal root (E.Terminal.scope restored)));
  assert (
    Result.is_error
      (E.Terminal.create ~scope:root ~delivery:Possibly_submitted ~outcome:Completed));
  assert (
    Result.is_error
      (E.Terminal.create
         ~scope:root
         ~delivery:Response_started
         ~outcome:(Failed (Authentication Missing))));
  assert (
    Result.is_error
      (E.Terminal.create
         ~scope:root
         ~delivery:Possibly_submitted
         ~outcome:(Failed (Transport (Http_status 600)))));
  print_endline "full scope restored; contradictory delivery/status rejected";
  [%expect {| full scope restored; contradictory delivery/status rejected |}]
;;

let%test_unit "terminal outcomes are closed and all known categories roundtrip" =
  let outcomes =
    E.Terminal.
      [ Completed
      ; Refused
      ; Incomplete Output_limit
      ; Incomplete Filtered
      ; Incomplete Other
      ; Incomplete Unavailable
      ; Failed (Provider Invalid_request)
      ; Failed (Provider Denied)
      ; Failed (Provider Rate_limited)
      ; Failed (Provider Unavailable)
      ; Failed (Provider Unknown)
      ; Failed (Transport Connection)
      ; Failed (Transport Timeout)
      ; Failed (Transport Protocol)
      ; Failed (Transport Invalid_http)
      ; Failed (Transport Invalid_content_type)
      ; Failed (Transport Body_limit)
      ; Failed (Transport Framing_limit)
      ; Failed (Transport (Http_status 429))
      ]
  in
  List.iter outcomes ~f:(fun outcome ->
    let terminal =
      E.Terminal.create ~scope:root ~delivery:Response_started ~outcome |> ok
    in
    assert (
      E.Terminal.equal
        terminal
        (E.Terminal.of_json (E.Terminal.to_json terminal) ~limits |> ok)));
  List.iter
    E.Terminal.[ Missing; Denied; Invalid_credential; Timed_out ]
    ~f:(fun reason ->
      let terminal =
        E.Terminal.create
          ~scope:root
          ~delivery:Definitely_not_submitted
          ~outcome:(Failed (Authentication reason))
        |> ok
      in
      assert (
        E.Terminal.equal
          terminal
          (E.Terminal.of_json (E.Terminal.to_json terminal) ~limits |> ok)));
  let malformed =
    `Object
      [ "scope", T.Scope.to_json root
      ; "delivery", `String "response_started"
      ; ( "outcome"
        , `Object
            [ "type", `String "incomplete"; "reason", `String "private provider text" ] )
      ]
  in
  assert (Result.is_error (E.Terminal.of_json malformed ~limits))
;;

let%test_unit "tool candidate has selected native guards without executing a tool" =
  let call namespace async status =
    P.Semantic.create
      (Call
         { kind = Function
         ; name = "inspect"
         ; namespace
         ; input_bytes = " exact bytes "
         ; async
         })
      ~metadata:{ P.Metadata.empty with status }
    |> ok
    |> P.authored
  in
  let descriptor = item ~call_name:(Some "inspect") (Some (Call Function)) in
  let admitted namespace async status local_execution =
    E.create
      (Candidate_ready
         { item = descriptor; payload = call namespace async status; local_execution })
      ~limits
  in
  List.iter
    [ P.Presence.Absent, P.Presence.Absent, P.Presence.Absent
    ; Absent, Value false, Value "completed"
    ]
    ~f:(fun (namespace, async, status) ->
      assert (Result.is_ok (admitted namespace async status Tool_candidate)));
  List.iter
    [ P.Presence.Null, P.Presence.Absent, P.Presence.Absent
    ; Value "future.namespace", Absent, Absent
    ; Absent, Null, Absent
    ; Absent, Value true, Absent
    ; Absent, Absent, Null
    ; Absent, Absent, Value "in_progress"
    ; Absent, Absent, Value "future.status"
    ]
    ~f:(fun (namespace, async, status) ->
      assert (Result.is_error (admitted namespace async status Tool_candidate));
      assert (Result.is_ok (admitted namespace async status Not_eligible)));
  let unknown = payload (Unknown { provider_kind = "future.item" }) in
  assert (
    Result.is_error
      (E.create
         (Candidate_ready
            { item = item (Some (Unknown "future.item"))
            ; payload = unknown
            ; local_execution = Tool_candidate
            })
         ~limits))
;;

let%expect_test "opaque caller evidence is retained without deriving local execution" =
  let semantic =
    P.Semantic.create
      (Call
         { kind = Function
         ; name = "inspect"
         ; namespace = Absent
         ; input_bytes = " exact bytes "
         ; async = Absent
         })
      ~metadata:{ P.Metadata.empty with call_id = Value "actual-provider-call" }
    |> ok
  in
  let origin =
    P.Origin.create
      ~adapter:"synthetic"
      ~provider:"selected"
      ~account:None
      ~endpoint:"local"
      ~profile:None
      ~model:None
      ~replay_version:1
    |> ok
  in
  let raw =
    `Object
      [ "type", `String "synthetic.call"
      ; "name", `String "inspect"
      ; "input", `String " exact bytes "
      ; "caller", `Object [ "type", `String "future.caller"; "opaque", `Number "1e+00" ]
      ]
  in
  let payload = P.captured semantic ~origin ~raw |> ok in
  let descriptor = item ~call_name:(Some "inspect") (Some (Call Function)) in
  let event =
    E.create
      (Candidate_ready { item = descriptor; payload; local_execution = Not_eligible })
      ~limits
    |> ok
  in
  (match E.view event with
   | Candidate_ready { payload = retained; local_execution = Not_eligible; _ } ->
     assert (Jsonaf.exactly_equal (P.to_json payload) (P.to_json retained))
   | Candidate_ready { local_execution = Tool_candidate; _ } | Live _ | Terminal _ ->
     assert false);
  print_endline
    "semantic Call and opaque caller retained; execution eligibility remains explicit \
     evidence";
  [%expect
    {| semantic Call and opaque caller retained; execution eligibility remains explicit evidence |}]
;;
