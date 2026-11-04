open! Core
open Due_support
module Intent = A.Recurrence_execution_intent
module Attempt = A.Recurrence_execution_attempt
module Execution = A.Recurrence_execution
module Origin = A.Recurrence_run_origin
module Reference = A.Recurrence_run_reference
module Settlement = A.Recurrence_settlement

let record entry slot =
  let raw = H.to_document (frontier entry) |> document_ok |> D.Document.json in
  let nominal_json =
    match field (field raw "payload") "ready" with
    | `Array rows ->
      List.find_exn rows ~f:(fun row ->
        Int64.equal slot (I.Occurrence.slot (N.key (N.of_json row |> protocol_ok))))
    | _ -> failwith "missing saved Ready array"
  in
  let intent =
    Intent.create
      ~definition:(E.current entry)
      ~control_revision:(E.control_revision entry)
      ~nominal_json
      ~target:session
      ~source:(source 1L)
      ~child:None
    |> protocol_ok
  in
  let request =
    Attempt.create
      ~intent
      ~number:0
      ~principal_id
      ~expected_revision:7L
      ~key:
        (P.Idempotency_key.of_string
           ("settlement:"
            ^ Int64.to_string (I.Definition.revision (F.key (E.current entry)))
            ^ ":"
            ^ Int64.to_string slot)
         |> protocol_ok)
    |> protocol_ok
  in
  let entry, execution =
    E.record_execution entry ~intent ~attempt:request |> protocol_ok
  in
  entry, execution
;;

(* Pure constructor facts for the controller data model. This grants no actor
   observation or authority; actual target producers/observation are separate. *)
let evidence execution ~completed_at ~outcome =
  let intent = Execution.intent execution in
  let request = Execution.current_attempt execution |> Execution.Attempt.request in
  let key = Intent.occurrence intent in
  let id =
    P.Id.Run.of_string
      ("run_settlement_"
       ^ Int64.to_string (I.Definition.revision (I.Occurrence.definition key))
       ^ "_"
       ^ Int64.to_string (I.Occurrence.slot key))
    |> protocol_ok
  in
  let run lifecycle revision =
    P.Run.create
      ~id
      ~session:(Intent.target intent)
      ~principal_id
      ~source:(Intent.source intent)
      ~mode:(Intent.mode intent)
      ~lifecycle
      ~revision
      ~owned_work:[]
      ~relinquished_work:[]
      ~terminal_work:[]
      ~created_at:timestamp
      ~updated_at:completed_at
    |> protocol_ok
  in
  let receipt kind key request_sha256 run_revision committed_at =
    P.Run_receipt.create
      ~run_id:id
      ~principal_id
      ~source:(Intent.source intent)
      ~key
      ~request_sha256
      ~kind
      ~run_revision
      ~session_revision:9L
      ~committed_at
    |> protocol_ok
  in
  let admission =
    receipt Admission (Attempt.key request) (Attempt.request_sha256 request) 0L timestamp
  in
  let origin =
    Origin.create ~intent ~attempt:request ~run:(run Admitted 0L) ~admission
    |> protocol_ok
  in
  let digest =
    P.Run.Terminal.to_json outcome
    |> Jsonaf.to_string
    |> Digestif.SHA256.digest_string
    |> Digestif.SHA256.to_hex
  in
  let receipt =
    receipt
      Terminal
      (P.Idempotency_key.of_string "settlement:terminal" |> protocol_ok)
      digest
      1L
      completed_at
  in
  let terminal =
    Reference.create ~origin ~run:(run (Terminal outcome) 1L) ~receipt |> protocol_ok
  in
  origin, terminal
;;

let import entry (origin, terminal) =
  E.import_execution
    entry
    ~origin
    ~terminal:(Some terminal)
    ~automatic:Current_policy_allows
  |> protocol_ok
;;

let assert_roundtrip entry =
  assert (D.Json.equal (entry_json entry) (entry_json (restore entry)))
;;

let%expect_test
    "atomic completion data, paused delay enable and out-of-order detail retirement"
  =
  let delay =
    definition
      "atomic_delay"
      ~schedule:
        (Interval
           { rule =
               R.Rule.Interval.create
                 ~mode:Fixed_delay
                 ~period:(R.Rule.Period.create_ms 1000L |> timing)
           ; first_due = at_ms 0
           })
  in
  let selected, _, _ =
    step (enabled delay) ~phase:None (snapshot ~wall:(at_ms 0) ~elapsed_ms:0)
  in
  let recorded, execution = record selected 0L in
  let proof = evidence execution ~completed_at:timestamp ~outcome:(Completed None) in
  let settled, result = import recorded proof in
  (match result with
   | Imported { next = Successor _; _ } -> ()
   | Imported _ | Retained_exact _ | Detail_not_retained _ ->
     failwith "missing atomic successor");
  [%test_eq: int64] (H.summary (frontier settled)).settled 1L;
  [%test_eq: int] (H.pending_count (frontier settled)) 0;
  (match A.Recurrence_cursor.position (E.cursor settled) with
   | Interval next ->
     [%test_eq: int64] (R.Cursor.Interval.slot next) 1L;
     assert (
       Time_ns.equal
         (R.Cursor.Interval.due_at next)
         (P.Timestamp.of_string "2026-08-15T12:00:01Z"
          |> protocol_ok
          |> P.Timestamp.to_time_ns))
   | Calendar _ | Awaiting_completion _ | Exhausted -> failwith "not rearmed");
  assert_roundtrip settled;
  let replay, result = import settled proof in
  assert (D.Json.equal (entry_json replay) (entry_json settled));
  (match result with
   | Retained_exact _ -> ()
   | Imported _ | Detail_not_retained _ -> failwith "not retained duplicate");
  let selected, _, _ =
    step settled ~phase:None (snapshot ~wall:(at_ms 1000) ~elapsed_ms:1000)
  in
  [%test_eq: int64 list] (slots selected) [ 1L ];
  let recorded, execution = record selected 1L in
  let paused = E.pause recorded ~reason:Operator_pause |> protocol_ok in
  let completed_at = P.Timestamp.of_string "2026-08-15T12:00:02Z" |> protocol_ok in
  let proof = evidence execution ~completed_at ~outcome:(Completed None) in
  let settled, _ = import paused proof in
  assert (E.equal_control (E.control settled) (Paused Operator_pause));
  assert (A.Recurrence_cursor.equal (E.cursor paused) (E.cursor settled));
  assert_roundtrip settled;
  let resumed = E.enable settled |> protocol_ok in
  (match A.Recurrence_cursor.position (E.cursor resumed) with
   | Interval next ->
     [%test_eq: int64] (R.Cursor.Interval.slot next) 2L;
     assert (
       Time_ns.equal
         (R.Cursor.Interval.due_at next)
         (P.Timestamp.of_string "2026-08-15T12:00:03Z"
          |> protocol_ok
          |> P.Timestamp.to_time_ns))
   | Calendar _ | Awaiting_completion _ | Exhausted ->
     failwith "enable did not consume exact basis");
  assert (D.Json.equal (entry_json resumed) (entry_json (E.enable resumed |> protocol_ok)));
  assert_roundtrip resumed;
  List.iter
    [ E.Pause.Operator_pause; Controller_stop; Source_review; Recovery_review ]
    ~f:(fun reason ->
      let paused = E.pause recorded ~reason |> protocol_ok in
      let interrupted = evidence execution ~completed_at ~outcome:Interrupted in
      let held, _ = import paused interrupted in
      let before = entry_json held in
      assert (Result.is_error (E.enable held));
      assert (D.Json.equal before (entry_json held));
      assert (E.equal_control (E.control held) (Paused reason));
      assert (Option.is_none (H.alarm (frontier held)));
      assert_roundtrip held);
  List.iter
    [ [ 1L; 2L ]; [ 2L; 1L ] ]
    ~f:(fun order ->
      let definition =
        definition
          "atomic_rate"
          ~policy:(policy ~history:1 ~misfire:(Catch_up 3) ~grace_ms:60_000L ())
      in
      let selected, _, _ =
        step
          (enabled definition)
          ~phase:None
          (snapshot ~wall:(at_ms 2000) ~elapsed_ms:2000)
      in
      [%test_eq: int64 list] (slots selected) [ 0L; 1L; 2L ];
      let recorded, executions =
        List.fold [ 0L; 1L; 2L ] ~init:(selected, []) ~f:(fun (entry, executions) slot ->
          let entry, execution = record entry slot in
          entry, executions @ [ execution ])
      in
      let proofs =
        List.map executions ~f:(fun execution ->
          evidence execution ~completed_at ~outcome:(Completed None))
      in
      let settled =
        List.foldi order ~init:recorded ~f:(fun index entry slot ->
          let next, _ = import entry (List.nth_exn proofs (Int64.to_int_exn slot)) in
          [%test_eq: int] (H.pending_count (frontier next)) (2 - index);
          [%test_eq: int64] (H.summary (frontier next)).settled (Int64.of_int (index + 1));
          let expected =
            if index = 0
            then [ 0L; 1L; 2L ]
            else List.sort [ 0L; slot ] ~compare:Int64.compare
          in
          [%test_eq: int64 list]
            (List.map
               (H.executions (frontier next))
               ~f:(fun execution ->
                 I.Occurrence.slot (Intent.occurrence (Execution.intent execution)))
             |> List.sort ~compare:Int64.compare)
            expected;
          assert_roundtrip next;
          next)
      in
      let pruned = List.hd_exn order in
      let unchanged, result =
        import settled (List.nth_exn proofs (Int64.to_int_exn pruned))
      in
      assert (D.Json.equal (entry_json unchanged) (entry_json settled));
      match result with
      | Detail_not_retained _ -> ()
      | Imported _ | Retained_exact _ -> failwith "pruned detail overclaimed");
  let history_one = policy ~history:1 ~misfire:(Catch_up 2) ~grace_ms:60_000L () in
  let a = definition "basis_late" ~schedule:(F.schedule delay) ~policy:history_one in
  let selected, _, _ =
    step (enabled a) ~phase:None (snapshot ~wall:(at_ms 0) ~elapsed_ms:0)
  in
  let waiting_a, execution_a = record selected 0L in
  let b =
    F.create
      ~key:
        (I.Definition.create
           ~controller:(E.controller waiting_a)
           ~recurrence_id:(E.id waiting_a)
           ~revision:1L
         |> protocol_ok)
      ~creator:(F.creator a)
      ~source:(F.source a)
      ~birth_lifecycle_revision:(F.birth_lifecycle_revision a)
      ~schedule:(F.schedule a)
      ~target:(F.target a)
      ~policy:history_one
      ~created_at:timestamp
    |> protocol_ok
  in
  let current_b = E.edit waiting_a b |> protocol_ok |> E.enable |> protocol_ok in
  let selected, _, _ =
    step current_b ~phase:None (snapshot ~wall:(at_ms 0) ~elapsed_ms:0)
  in
  let waiting_b, execution_b = record selected 0L in
  let proof_b = evidence execution_b ~completed_at ~outcome:(Completed None) in
  let settled_b, _ = import waiting_b proof_b in
  let settled_b =
    edit_frontier settled_b ~f:(fun payload ->
      set
        payload
        "delay_basis"
        (append (field payload "delay_basis") "future_basis" (`Number "5.00e1")))
  in
  let basis_b =
    H.delay_basis (frontier settled_b)
    |> Option.value_exn
    |> A.Recurrence_delay_basis.to_json
  in
  let proof_a = evidence execution_a ~completed_at:timestamp ~outcome:(Completed None) in
  let late_a, _ = import settled_b proof_a in
  assert (A.Recurrence_cursor.equal (E.cursor late_a) (E.cursor settled_b));
  assert (
    D.Json.equal
      basis_b
      (H.delay_basis (frontier late_a)
       |> Option.value_exn
       |> A.Recurrence_delay_basis.to_json));
  [%test_eq: int64] (H.summary (frontier late_a)).settled 2L;
  [%test_eq: int] (H.pending_count (frontier late_a)) 0;
  (match H.executions (frontier late_a) with
   | [ execution ] ->
     [%test_eq: int64]
       (I.Definition.revision
          (I.Occurrence.definition (Intent.occurrence (Execution.intent execution))))
       0L
   | _ -> failwith "history one did not retire B detail");
  assert_roundtrip late_a;
  print_endline
    "one completion candidate; receipt deadline; paused/new enable; unresolved zero \
     survives history one; missing detail makes no claim";
  [%expect
    {| one completion candidate; receipt deadline; paused/new enable; unresolved zero survives history one; missing detail makes no claim |}]
;;
