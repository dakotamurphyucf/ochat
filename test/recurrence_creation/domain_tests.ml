open! Core
open Creation_support
module Due = Due_support

let captured_definition workspace template =
  F.create
    ~key:(F.key template)
    ~creator:(F.creator template)
    ~source:(F.source template)
    ~birth_lifecycle_revision:(F.birth_lifecycle_revision template)
    ~schedule:(F.schedule template)
    ~target:(fresh_target workspace)
    ~policy:(F.policy template)
    ~created_at:(F.created_at template)
  |> protocol_ok
;;

let reserve_entry entry slot =
  let occurrence =
    I.Occurrence.create ~definition:(F.key (E.current entry)) ~slot |> protocol_ok
  in
  E.reserve_child
    entry
    ~principal_id
    ~child_session_id:(P.Id.Session.create ())
    ~transaction_id:(P.Id.Transaction.create ())
    ~occurrence
  |> protocol_ok
;;

let edit_definition entry =
  let previous = E.current entry in
  F.create
    ~key:
      (I.Definition.create
         ~controller:(E.controller entry)
         ~recurrence_id:(E.id entry)
         ~revision:Int64.(I.Definition.revision (F.key previous) + 1L)
       |> protocol_ok)
    ~creator:(F.creator previous)
    ~source:(F.source previous)
    ~birth_lifecycle_revision:(F.birth_lifecycle_revision previous)
    ~schedule:(F.schedule previous)
    ~target:(F.target previous)
    ~policy:(F.policy previous)
    ~created_at:timestamp
  |> protocol_ok
;;

let%expect_test
    "immutable reservation model partitions slots, retains custody and fences edits"
  =
  with_actor_workspace (fun _ workspace ->
    let initial =
      captured_definition workspace (Due.definition "reservation_model") |> Due.enabled
    in
    let first, _, _ =
      Due.step initial ~phase:None (Due.snapshot ~wall:(Due.at_ms 0) ~elapsed_ms:0)
    in
    let raw_nominal = ref `Null in
    let enriched =
      Due.edit_frontier first ~f:(fun payload ->
        let ready =
          match Due.field payload "ready" with
          | `Array [ nominal ] -> nominal
          | _ -> assert false
        in
        let nominal = Due.append ready "future_nominal" (`Number "1.00e+2") in
        raw_nominal := nominal;
        Due.set payload "ready" (`Array [ nominal ]))
    in
    let reserved, row = reserve_entry enriched 0L in
    assert (D.Json.equal !raw_nominal (Reservation.nominal_json row));
    let retry, same = reserve_entry reserved 0L in
    assert_same_ids row same;
    assert (
      Int64.equal (H.revision (Due.frontier reserved)) (H.revision (Due.frontier retry)));
    (* Independent serial oracle: one protected slot occupies the only pending
       credit; each later nominal is consumed once and omitted, never launched. *)
    let current = ref reserved in
    for second = 1 to 12 do
      let next, _, _ =
        Due.step
          !current
          ~phase:None
          (Due.snapshot ~wall:(Due.at_ms (second * 1000)) ~elapsed_ms:(second * 1000))
      in
      current := next;
      let summary = H.summary (Due.frontier next) in
      [%test_eq: int64] summary.consumed (Int64.of_int (second + 1));
      [%test_eq: int64] summary.omitted (Int64.of_int second);
      assert (List.is_empty (H.ready (Due.frontier next)));
      [%test_eq: int] (H.pending_count (Due.frontier next)) 1;
      assert (
        Reservation.equal row (List.hd_exn (H.protected_children (Due.frontier next))))
    done;
    let paused = E.pause !current ~reason:Operator_pause |> protocol_ok in
    let edited = E.edit paused (edit_definition paused) |> protocol_ok in
    let enabled = E.enable edited |> protocol_ok in
    let retired = E.retire enabled ~reason:Operator_retire |> protocol_ok in
    List.iter [ paused; edited; enabled; retired ] ~f:(fun entry ->
      assert (
        Reservation.equal row (List.hd_exn (H.protected_children (Due.frontier entry))));
      let restored = Due.restore entry in
      assert (D.Json.equal (Due.entry_json entry) (Due.entry_json restored)));
    assert (
      Result.is_error
        (E.reserve_child
           enabled
           ~principal_id
           ~child_session_id:(P.Id.Session.create ())
           ~transaction_id:(P.Id.Transaction.create ())
           ~occurrence:(Reservation.occurrence row)));
    let frontier = Due.frontier reserved in
    let document = H.to_document frontier |> document_ok in
    let json = D.Document.json document in
    let decode json =
      D.Document.inspect ~limits:H.limits json |> Result.bind ~f:H.of_document
    in
    assert (Result.is_error (decode (Due.set json "required_semantics" (`Array []))));
    assert (
      Result.is_error
        (decode
           (Due.set json "required_semantics" (`Array [ `String "future.execution" ]))));
    let malformed =
      Due.set
        json
        "payload"
        (Due.set (Due.field json "payload") "protected_children" (`Array []))
    in
    let erased = decode malformed |> document_ok in
    assert (
      Result.is_error
        (H.validate_transition
           frontier
           ~incoming:erased
           ~definitions:(E.definitions reserved)
           ~cursor:(E.cursor reserved)));
    let replacement =
      Reservation.create
        ~definition:(E.current reserved)
        ~control_revision:(E.control_revision reserved)
        ~principal_id
        ~child_session_id:(P.Id.Session.create ())
        ~transaction_id:(P.Id.Transaction.create ())
        ~nominal_json:(Reservation.nominal_json row)
      |> protocol_ok
    in
    let incoming =
      Due.set
        json
        "payload"
        (Due.set
           (Due.field json "payload")
           "protected_children"
           (`Array [ Reservation.to_json replacement ]))
      |> decode
      |> document_ok
    in
    assert (
      Result.is_error
        (H.validate_transition
           frontier
           ~incoming
           ~definitions:(E.definitions reserved)
           ~cursor:(E.cursor reserved)));
    let capacity = H.capacity_projection frontier |> document_ok in
    let original_markers = D.Document.required_semantics document in
    let association_marker = "recurrence.child-association.v1" in
    assert (not (List.mem original_markers association_marker ~equal:String.equal));
    let capacity_document = D.Document.inspect ~limits:H.limits capacity |> document_ok in
    [%test_eq: string list]
      (original_markers @ [ association_marker ])
      (D.Document.required_semantics capacity_document);
    (* Reservation accounting charges the later association fence without
       publishing an association or mutating the current document semantics. *)
    assert (List.is_empty (H.child_associations frontier));
    assert (D.Json.equal json (H.to_document frontier |> document_ok |> D.Document.json)));
  print_endline
    "13-slot serial model, exact retry, nested lexemes, controls and required semantics";
  [%expect
    {| 13-slot serial model, exact retry, nested lexemes, controls and required semantics |}]
;;

let%expect_test "private calendar selection survives Ready reservation until its cutoff" =
  with_actor_workspace (fun _ workspace ->
    let policy =
      A.Recurrence_policy.create
        ~misfire:Coalesce
        ~grace_ms:864000000L
        ~max_pending:1
        ~max_history:64
        ~max_calendar_steps:1
      |> protocol_ok
    in
    let template = Due.calendar ~steps:1 () in
    let definition =
      F.create
        ~key:(F.key template)
        ~creator:(F.creator template)
        ~source:(F.source template)
        ~birth_lifecycle_revision:(F.birth_lifecycle_revision template)
        ~schedule:(F.schedule template)
        ~target:(fresh_target workspace)
        ~policy
        ~created_at:timestamp
      |> protocol_ok
    in
    let zone = Due.zone () in
    let initial = Due.enabled definition in
    let rec until_ready entry count =
      if not (List.is_empty (H.ready (Due.frontier entry)))
      then entry
      else if count = 0
      then failwith "calendar failed to select bounded first row"
      else (
        let next, _, _ =
          Due.step
            entry
            ~zone
            ~phase:None
            (Due.snapshot ~wall:(Due.at_ms 0) ~elapsed_ms:0)
        in
        until_ready next (count - 1))
    in
    let ready = until_ready initial 8 in
    let rec collecting entry count =
      if not (List.is_empty (H.candidates (Due.frontier entry)))
      then entry
      else if count = 0
      then failwith "calendar failed to retain private cutoff candidate"
      else (
        let next, _, _ =
          Due.step
            entry
            ~zone
            ~phase:None
            (Due.snapshot ~wall:(Due.at_ms (3 * 86400000)) ~elapsed_ms:0)
        in
        collecting next (count - 1))
    in
    let collecting = collecting ready 8 in
    let selected = List.hd_exn (H.ready (Due.frontier collecting)) in
    let reserved, row = reserve_entry collecting (I.Occurrence.slot (N.key selected)) in
    assert (not (List.is_empty (H.candidates (Due.frontier reserved))));
    let final = ref reserved in
    for _ = 1 to 12 do
      let next, _, _ =
        Due.step
          !final
          ~zone
          ~phase:None
          (Due.snapshot ~wall:(Due.at_ms (3 * 86400000)) ~elapsed_ms:0)
      in
      final := next
    done;
    assert (List.is_empty (H.candidates (Due.frontier !final)));
    assert (List.is_empty (H.ready (Due.frontier !final)));
    assert (
      Reservation.equal row (List.hd_exn (H.protected_children (Due.frontier !final))));
    H.validate
      (Due.frontier !final)
      ~definitions:(E.definitions !final)
      ~cursor:(E.cursor !final)
    |> protocol_ok);
  print_endline
    "existing Ready becomes protected; private candidate gets a truthful bounded \
     disposition";
  [%expect
    {| existing Ready becomes protected; private candidate gets a truthful bounded disposition |}]
;;
