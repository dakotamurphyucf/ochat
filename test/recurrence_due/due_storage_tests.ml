open! Core
open Due_support

let json frontier = H.to_document frontier |> document_ok |> D.Document.json
let decode json = D.Document.inspect ~limits:H.limits json |> Result.bind ~f:H.of_document

let%expect_test
    "complete frontier reservation covers near-limit control moves; corruption rejects"
  =
  let initial = calendar ~steps:1 () |> enabled in
  let zone = zone () in
  let armed, _, _ =
    step initial ~zone ~phase:None (snapshot ~wall:(at_ms (-1000)) ~elapsed_ms:0)
  in
  let collecting, _, _ =
    step initial ~zone ~phase:None (snapshot ~wall:(at_ms 0) ~elapsed_ms:0)
  in
  List.iter
    [ armed, "alarm"; collecting, "batch" ]
    ~f:(fun (entry, field_name) ->
      let enriched =
        edit_frontier entry ~f:(fun payload ->
          set
            payload
            field_name
            (append (field payload field_name) "future" (`Number "1.00e+2")))
      in
      let current = frontier enriched in
      let original = json current in
      let padded_empty = append original "padding" (`String "") in
      let value = decode padded_empty |> document_ok in
      let growth = H.reserved_growth value |> protocol_ok in
      let measure json = A.Capacity_growth.measure json ~limits:H.limits |> protocol_ok in
      let available =
        D.Limits.max_bytes H.limits
        - A.Capacity_growth.encoded_bytes (measure padded_empty)
        - A.Capacity_growth.encoded_bytes growth
        - 32
      in
      assert (available > 0);
      let full =
        set padded_empty "padding" (`String (String.make available 'p'))
        |> decode
        |> document_ok
      in
      let withdrawn = H.withdraw full ~definition:(E.current enriched) |> protocol_ok in
      H.validate
        withdrawn
        ~definitions:(E.definitions enriched)
        ~cursor:(E.cursor enriched)
      |> protocol_ok;
      let before = measure (json full) in
      let reserved =
        A.Capacity_growth.add before (H.reserved_growth full |> protocol_ok)
        |> protocol_ok
      in
      let after = measure (json withdrawn) in
      assert (
        A.Capacity_growth.encoded_bytes after <= A.Capacity_growth.encoded_bytes reserved);
      assert (A.Capacity_growth.fields after <= A.Capacity_growth.fields reserved);
      assert (A.Capacity_growth.nodes after <= A.Capacity_growth.nodes reserved);
      let archived = field (field (json withdrawn) "payload") "control_history" in
      let original_container = field (field original "payload") field_name in
      (match archived with
       | `Array [ detail ] ->
         assert (D.Json.equal (field detail "value") original_container)
       | _ -> failwith "expected one exact archived control container");
      assert (
        D.Json.equal (json withdrawn) (json (decode (json withdrawn) |> document_ok))));
  let selected = definition "malformed" |> enabled in
  let selected, _, _ =
    step selected ~phase:None (snapshot ~wall:(at_ms 0) ~elapsed_ms:0)
  in
  let current = frontier selected in
  let raw = json current in
  let payload = field raw "payload" in
  let ready =
    match field payload "ready" with
    | `Array [ value ] -> value
    | _ -> assert false
  in
  let marks = field payload "marks" in
  let bad_marks f =
    match marks with
    | `Array [ mark ] -> `Array [ f mark ]
    | _ -> assert false
  in
  let variants =
    [ set payload "ready" (`Array [ ready; ready ])
    ; set payload "ready" (`Array [ set ready "status" (`String "future_running") ])
    ; set payload "marks" (bad_marks (fun mark -> set mark "omitted" (`String "2")))
    ; set
        payload
        "marks"
        (bad_marks (fun mark -> set mark "through" (`String "9223372036854775807")))
    ; set
        payload
        "control_history"
        (`Array [ `Object [ "kind", `String "future_execution"; "value", `Null ] ])
    ]
  in
  List.iter variants ~f:(fun payload ->
    match decode (set raw "payload" payload) with
    | Error _ -> ()
    | Ok incoming ->
      assert (
        Result.is_error
          (H.validate
             incoming
             ~definitions:(E.definitions selected)
             ~cursor:(E.cursor selected))));
  assert (
    Result.is_error
      (H.validate_transition
         current
         ~incoming:(H.initial (E.current selected) |> protocol_ok)
         ~definitions:(E.definitions selected)
         ~cursor:(F.initial_cursor (E.current selected))));
  print_endline
    "2 MiB boundary keeps exact alarm/batch metadata; byte/field/node reserves cover \
     withdrawal; malformed/replayed frontiers reject";
  [%expect
    {| 2 MiB boundary keeps exact alarm/batch metadata; byte/field/node reserves cover withdrawal; malformed/replayed frontiers reject |}]
;;
