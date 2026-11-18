open! Core
open Due_support

let complete entry ~zone ~snapshot =
  let rec loop entry remaining =
    if remaining = 0 then failwith "bounded calendar failed to finish";
    let next, _, _ = step entry ~zone ~phase:None snapshot in
    if Option.is_none (H.calendar_cutoff (frontier next))
    then next
    else loop next (remaining - 1)
  in
  loop entry 32
;;

let%expect_test
    "chunked calendar search agrees with whole search and preserves existing Ready"
  =
  let zone = zone () in
  List.iter [ A.Recurrence_policy.Skip; Coalesce; Catch_up 3 ] ~f:(fun misfire ->
    List.iter [ 0; 1; 5 ] ~f:(fun days ->
      let snapshot = snapshot ~wall:(at_ms (days * 86400000)) ~elapsed_ms:0 in
      let chunked = complete (calendar ~steps:1 ~misfire () |> enabled) ~zone ~snapshot in
      let whole = complete (calendar ~steps:256 ~misfire () |> enabled) ~zone ~snapshot in
      [%test_eq: int64 list] (slots chunked) (slots whole);
      assert (H.Summary.equal (H.summary (frontier chunked)) (H.summary (frontier whole)));
      assert (A.Recurrence_cursor.equal (E.cursor chunked) (E.cursor whole));
      let _, _, changed = step (restore chunked) ~zone ~phase:None snapshot in
      assert (not changed)));
  let first =
    complete
      (calendar ~steps:1 () |> enabled)
      ~zone
      ~snapshot:(snapshot ~wall:(at_ms 0) ~elapsed_ms:0)
  in
  let collecting, _, _ =
    step first ~zone ~phase:None (snapshot ~wall:(at_ms (3 * 86400000)) ~elapsed_ms:0)
  in
  [%test_eq: int64 list] (slots collecting) [ 0L ];
  assert (Option.is_some (H.calendar_cutoff (frontier collecting)));
  assert (not (List.is_empty (H.candidates (frontier collecting))));
  print_endline
    "nine chunk/whole cases; prior Ready remains dispatchable during bounded collection";
  [%expect
    {| nine chunk/whole cases; prior Ready remains dispatchable during bounded collection |}]
;;

let%expect_test
    "selected calendar provenance survives timezone edits and control dispositions"
  =
  let zone = zone () in
  let original = calendar ~steps:1 () |> enabled in
  let before = snapshot ~wall:(at_ms (-1000)) ~elapsed_ms:0 in
  let armed, _, _ = step original ~zone ~phase:None before in
  let selected = H.lookahead (frontier armed) |> Option.value_exn in
  let alarm = H.alarm (frontier armed) |> Option.value_exn in
  let changed_zone = Due_support.zone ~edition:'b' ~offset:3600 () in
  let due =
    complete
      armed
      ~zone:changed_zone
      ~snapshot:(snapshot ~wall:(at_ms 0) ~elapsed_ms:1000)
  in
  let delivered = List.hd_exn (H.ready (frontier due)) in
  assert (N.equal selected delivered);
  let next = H.lookahead (frontier due) |> Option.value_exn in
  assert (Time_ns.equal (N.at next) (at_ms (23 * 3600000)));
  let next_alarm = H.alarm (frontier due) |> Option.value_exn in
  assert (
    not (I.Alarm.equal (A.Recurrence_alarm.key alarm) (A.Recurrence_alarm.key next_alarm)));
  let controls =
    [ E.pause armed ~reason:Operator_pause |> protocol_ok
    ; E.pause armed ~reason:Source_review |> protocol_ok
    ; E.retire armed ~reason:Operator_retire |> protocol_ok
    ]
  in
  List.iter controls ~f:(fun held ->
    let held = restore held in
    assert (Option.is_none (H.alarm (frontier held)));
    assert (Option.equal N.equal (H.lookahead (frontier held)) (Some selected));
    [%test_eq: int64 list] (slots held) []);
  assert (Result.is_error (E.enable (List.last_exn controls)));
  let paused = List.hd_exn controls in
  let resumed, _, _ = step (E.enable paused |> protocol_ok) ~zone ~phase:None before in
  let resumed_alarm = H.alarm (frontier resumed) |> Option.value_exn in
  assert (
    not
      (I.Alarm.equal
         (A.Recurrence_alarm.key alarm)
         (A.Recurrence_alarm.key resumed_alarm)));
  print_endline
    "selected instant/provenance fixed; future search uses new zone; controls retire \
     alarms without losing cursor custody";
  [%expect
    {| selected instant/provenance fixed; future search uses new zone; controls retire alarms without losing cursor custody |}]
;;

let%expect_test
    "unknown control containers and nominal bytes survive withdrawal and replay"
  =
  let zone = zone () in
  let initial = calendar ~steps:1 () |> enabled in
  let armed, _, _ =
    step initial ~zone ~phase:None (snapshot ~wall:(at_ms (-1000)) ~elapsed_ms:0)
  in
  let armed =
    edit_frontier armed ~f:(fun payload ->
      let alarm = field payload "alarm" in
      set payload "alarm" (append alarm "future_alarm" (`String "retain-alarm")))
  in
  let collecting, _, _ =
    step initial ~zone ~phase:None (snapshot ~wall:(at_ms 0) ~elapsed_ms:0)
  in
  let collecting =
    edit_frontier collecting ~f:(fun payload ->
      let batch = field payload "batch" in
      let candidates =
        match field batch "candidates" with
        | `Array [ candidate ] ->
          `Array [ append candidate "future_nominal" (`String "retain-nominal") ]
        | _ -> failwith "expected one selected candidate"
      in
      set
        payload
        "batch"
        (append
           (set batch "candidates" candidates)
           "future_batch"
           (`String "retain-batch")))
  in
  List.iter
    [ armed, [ "retain-alarm" ]; collecting, [ "retain-batch"; "retain-nominal" ] ]
    ~f:(fun (entry, markers) ->
      let paused = E.pause entry ~reason:Operator_pause |> protocol_ok |> restore in
      let json = entry_json paused in
      let printed = Jsonaf.to_string json in
      List.iter markers ~f:(fun marker ->
        assert (String.is_substring printed ~substring:marker));
      assert (Option.is_none (H.alarm (frontier paused)));
      assert (Option.is_none (H.calendar_cutoff (frontier paused)));
      let retired = E.retire paused ~reason:Operator_retire |> protocol_ok |> restore in
      assert (D.Json.equal (entry_json retired) (entry_json (restore retired))));
  let missing = edit_frontier initial ~f:Fn.id |> entry_json in
  let missing =
    set missing "payload" (set (field missing "payload") "frontier" `Null)
    |> entry_of_json
  in
  assert (Result.is_error (E.frontier missing));
  assert (Result.is_error (E.validate_transition initial ~incoming:missing));
  ignore (E.retire missing ~reason:Operator_retire |> protocol_ok : E.t);
  print_endline
    "unknown alarm/batch/nominal bytes retained; historical missing frontier stays \
     inspectable, never reset";
  [%expect
    {| unknown alarm/batch/nominal bytes retained; historical missing frontier stays inspectable, never reset |}]
;;
