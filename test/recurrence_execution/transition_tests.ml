open! Core
open Due_support
module Intent = A.Recurrence_execution_intent
module Request = A.Recurrence_execution_attempt
module Resolution = A.Recurrence_retry_resolution
module Origin = A.Recurrence_run_origin
module Reference = A.Recurrence_run_reference
module Execution = A.Recurrence_execution
module Growth = A.Capacity_growth

let attempt intent number key =
  Request.create
    ~intent
    ~number
    ~principal_id
    ~expected_revision:7L
    ~key:(P.Idempotency_key.of_string key |> protocol_ok)
  |> protocol_ok
;;

let initial () =
  let intent = Domain_tests.capture (definition "execution-row") in
  let request = attempt intent 0 "attempt:0" in
  Execution.create ~intent ~attempt:request |> protocol_ok
;;

(* Pure constructor data for model/custody laws; actual producers are exercised
   separately by the owner fixture. This does not issue an actor observation. *)
let evidence intent request =
  let id =
    P.Id.Run.of_string ("run_attempt_" ^ Int.to_string (Request.number request))
    |> protocol_ok
  in
  let run lifecycle revision =
    P.Run.create
      ~id
      ~session:(Intent.target intent)
      ~principal_id:(Request.principal_id request)
      ~source:(Intent.source intent)
      ~mode:(Intent.mode intent)
      ~lifecycle
      ~revision
      ~owned_work:[]
      ~relinquished_work:[]
      ~terminal_work:[]
      ~created_at:timestamp
      ~updated_at:timestamp
    |> protocol_ok
  in
  let receipt kind key digest revision =
    P.Run_receipt.create
      ~run_id:id
      ~principal_id:(Request.principal_id request)
      ~source:(Intent.source intent)
      ~key
      ~request_sha256:digest
      ~kind
      ~run_revision:revision
      ~session_revision:(Int64.of_int (8 + Int64.to_int_exn revision))
      ~committed_at:timestamp
    |> protocol_ok
  in
  let admission =
    receipt Admission (Request.key request) (Request.request_sha256 request) 0L
  in
  let origin =
    Origin.create ~intent ~attempt:request ~run:(run Admitted 0L) ~admission
    |> protocol_ok
  in
  let terminal origin =
    let outcome = P.Run.Terminal.Completed None in
    let digest =
      P.Run.Terminal.to_json outcome
      |> Jsonaf.to_string
      |> Digestif.SHA256.digest_string
      |> Digestif.SHA256.to_hex
    in
    let receipt =
      receipt Terminal (P.Idempotency_key.of_string "terminal" |> protocol_ok) digest 1L
    in
    Reference.create ~origin ~run:(run (Terminal outcome) 1L) ~receipt |> protocol_ok
  in
  origin, terminal
;;

let resolution execution =
  Resolution.create
    ~intent:(Execution.intent execution)
    ~attempt:(Execution.Attempt.request (Execution.current_attempt execution))
      (* Deliberately earlier than CAS7: no inferred revision ordering. *)
    ~target_revision:0L
    ~resolved_at:timestamp
  |> protocol_ok
;;

module Model = struct
  type phase =
    | Open
    | Resolved
    | Admitted
    | Terminal

  type t =
    { ordinal : int
    ; phase : phase
    ; rejected : bool
    ; uncertain : bool
    }

  let initial = { ordinal = 0; phase = Open; rejected = false; uncertain = false }

  let step t action =
    match action, t.phase with
    | 0, Open -> Some { t with rejected = true }
    | 0, _ when t.rejected -> Some t
    | 1, Open -> Some { t with uncertain = true }
    | 1, _ when t.uncertain -> Some t
    | 2, (Open | Resolved) -> Some { t with phase = Resolved }
    | 3, Resolved when t.ordinal < 7 -> Some { initial with ordinal = t.ordinal + 1 }
    | (4 | 7), Open when action = 4 || t.ordinal = 0 -> Some { t with phase = Admitted }
    | (4 | 7), (Admitted | Terminal) when action = 4 || t.ordinal = 0 -> Some t
    | 5, (Admitted | Terminal) -> Some { t with phase = Terminal }
    | _ -> None
  ;;
end

let apply execution first_origin action =
  let request = Execution.Attempt.request (Execution.current_attempt execution) in
  let origin, terminal = evidence (Execution.intent execution) request in
  match action with
  | 0 -> Execution.record_rejection execution ~code:Conflict
  | 1 -> Execution.mark_uncertain execution ~code:Persistence_error
  | 2 -> Execution.resolve_retry execution (resolution execution)
  | 3 | 6 ->
    let number = Request.number request + 1 in
    if number >= Request.maximum_count
    then Error (P.Error.invalid_request "finite fixture attempt bound")
    else
      Execution.append_attempt
        execution
        (attempt
           (Execution.intent execution)
           number
           (if action = 6 then "attempt:0" else "attempt:" ^ Int.to_string number))
  | 4 -> Execution.associate execution origin
  | 5 -> Execution.settle execution (terminal origin)
  | 7 -> Execution.associate execution first_origin
  | _ -> assert false
;;

let%expect_test "bounded request history agrees with an independent four-state model" =
  let transitions = ref 0 in
  Quickcheck.test
    (Quickcheck.Generator.list (Int.gen_incl 0 7))
    ~seed:(`Deterministic "recurrence-attempt-history")
    ~trials:120
    ~f:(fun choices ->
      let execution = ref (initial ()) in
      let model = ref Model.initial in
      let first_origin, _ =
        evidence
          (Execution.intent !execution)
          (Execution.Attempt.request (Execution.current_attempt !execution))
      in
      List.take choices 24
      |> List.iter ~f:(fun action ->
        let expected = Model.step !model action in
        match expected, apply !execution first_origin action with
        | None, Error _ -> ()
        | Some next, Ok incoming ->
          Execution.validate_transition !execution ~incoming |> protocol_ok;
          assert (
            Bool.equal
              (Execution.is_pending incoming)
              (match next.phase with
               | Terminal -> false
               | _ -> true));
          [%test_eq: int] (List.length (Execution.attempts incoming)) (next.ordinal + 1);
          execution := incoming;
          model := next;
          incr transitions
        | None, Ok _ | Some _, Error _ ->
          failwith "execution/reference model disagreement"));
  let saturated =
    List.fold (List.init 7 ~f:Fn.id) ~init:(initial ()) ~f:(fun execution index ->
      let resolved =
        Execution.resolve_retry execution (resolution execution) |> protocol_ok
      in
      Execution.append_attempt
        resolved
        (attempt
           (Execution.intent execution)
           (index + 1)
           ("attempt:" ^ Int.to_string (index + 1)))
      |> protocol_ok)
  in
  [%test_eq: int] (List.length (Execution.attempts saturated)) 8;
  assert (Execution.is_pending saturated);
  assert (!transitions > 100);
  print_s
    [%sexp
      ("eight immutable attempts; unresolved effects never imply safe retry" : string)];
  [%expect {| "eight immutable attempts; unresolved effects never imply safe retry" |}]
;;

let map_payload json ~f = set json "payload" (f (field json "payload"))

let pad limits json =
  let json =
    append json "padding" (`String "")
    |> fun json ->
    append json "field_credit" (`Object [])
    |> fun json -> append json "node_credit" (`Array [])
  in
  let before = Growth.measure json ~limits |> protocol_ok in
  let fields = D.Limits.max_fields limits - Growth.fields before in
  let json =
    set
      json
      "field_credit"
      (`Object (List.init fields ~f:(fun index -> "f" ^ Int.to_string index, `Null)))
  in
  let after_fields = Growth.measure json ~limits |> protocol_ok in
  let nodes = D.Limits.max_nodes limits - Growth.nodes after_fields in
  let json = set json "node_credit" (`Array (List.init nodes ~f:(fun _ -> `Null))) in
  let used = D.Json.validate_and_measure ~limits json |> document_ok in
  set json "padding" (`String (String.make (D.Limits.max_bytes limits - used) 'x'))
;;

let%expect_test
    "complete unknown custody and mandatory proof growth survive a near-full row"
  =
  let original = initial () in
  let raw =
    Execution.to_json original
    |> fun json ->
    append json "future_row" (`Number "1.00e2")
    |> map_payload ~f:(fun payload ->
      let rows =
        match field payload "attempts" with
        | `Array rows -> rows
        | _ -> assert false
      in
      set
        payload
        "attempts"
        (`Array
            (List.map rows ~f:(fun row -> append row "future_attempt" (`Number "2.00e1")))))
    |> fun json -> append json "padding" (`String "")
  in
  let base = Execution.of_json raw |> protocol_ok in
  let before = Growth.measure raw ~limits:Execution.limits |> protocol_ok in
  let reserve = Execution.reserved_growth base |> protocol_ok in
  let padding =
    D.Limits.max_bytes Execution.limits
    - Growth.encoded_bytes before
    - Growth.encoded_bytes reserve
    - 1
  in
  let raw = set raw "padding" (`String (String.make padding 'x')) in
  let base = Execution.of_json raw |> protocol_ok in
  assert (D.Json.equal raw (Execution.to_json base));
  let bound =
    Growth.add (Growth.measure raw ~limits:Execution.limits |> protocol_ok) reserve
    |> protocol_ok
  in
  let check candidate =
    Execution.validate_transition base ~incoming:candidate |> protocol_ok;
    let observed =
      Growth.measure (Execution.to_json candidate) ~limits:Execution.limits |> protocol_ok
    in
    assert (Growth.encoded_bytes observed <= Growth.encoded_bytes bound);
    assert (Growth.fields observed <= Growth.fields bound);
    assert (Growth.nodes observed <= Growth.nodes bound);
    assert (
      D.Json.equal (field (Execution.to_json candidate) "future_row") (`Number "1.00e2"));
    let current =
      match field (field (Execution.to_json candidate) "payload") "attempts" with
      | `Array rows -> List.last_exn rows
      | _ -> assert false
    in
    assert (D.Json.equal (field current "future_attempt") (`Number "2.00e1"))
  in
  let audited =
    Execution.record_rejection base ~code:Organization_not_found
    |> protocol_ok
    |> fun row -> Execution.mark_uncertain row ~code:Pending_input_conflict |> protocol_ok
  in
  let origin, terminal =
    evidence
      (Execution.intent base)
      (Execution.Attempt.request (Execution.current_attempt base))
  in
  let origin =
    Origin.to_json origin |> pad Origin.limits |> Origin.of_json |> protocol_ok
  in
  let terminal =
    terminal origin
    |> Reference.to_json
    |> pad Reference.limits
    |> Reference.of_json
    |> protocol_ok
  in
  let settled =
    Execution.associate audited origin
    |> protocol_ok
    |> fun row -> Execution.settle row terminal |> protocol_ok
  in
  check settled;
  assert (not (Execution.is_pending settled));
  assert (Execution.equal settled (Execution.settle settled terminal |> protocol_ok));
  let resolution =
    resolution base
    |> Resolution.to_json
    |> pad Resolution.limits
    |> Resolution.of_json
    |> protocol_ok
  in
  let resolved = Execution.resolve_retry audited resolution |> protocol_ok in
  check resolved;
  assert (Execution.is_pending resolved);
  assert (
    Result.is_error
      (Execution.append_attempt resolved (attempt (Execution.intent base) 1 "attempt:1")));
  let erased =
    Execution.to_json settled
    |> fun json ->
    (match json with
     | `Object fields ->
       `Object
         (List.filter fields ~f:(fun (name, _) -> not (String.equal name "future_row")))
     | _ -> assert false)
    |> Execution.of_json
    |> protocol_ok
  in
  assert (Result.is_error (Execution.validate_transition settled ~incoming:erased));
  let changed =
    Execution.to_json settled
    |> map_payload ~f:(fun payload ->
      let rows =
        match field payload "attempts" with
        | `Array rows -> rows
        | _ -> assert false
      in
      set
        payload
        "attempts"
        (`Array
            (List.map rows ~f:(fun row ->
               let reference =
                 field row "terminal"
                 |> map_payload ~f:(fun payload ->
                   set payload "outcome" (`Object [ "kind", `String "limited" ]))
               in
               set row "terminal" reference))))
    |> Execution.of_json
    |> protocol_ok
  in
  assert (Result.is_error (Execution.validate_transition settled ~incoming:changed));
  print_s
    [%sexp
      ("full proof quotas and audit growth reserved; optional retry can saturate without \
        losing evidence"
       : string)];
  [%expect
    {| "full proof quotas and audit growth reserved; optional retry can saturate without losing evidence" |}]
;;

let%expect_test
    "compact fixed-delay basis preserves full proofs and checked receipt deadline"
  =
  let module Basis = A.Recurrence_delay_basis in
  let definition =
    definition
      "delay-basis"
      ~schedule:
        (F.Schedule.Interval
           { rule =
               R.Rule.Interval.create
                 ~mode:Fixed_delay
                 ~period:(R.Rule.Period.create_ms 1000L |> timing)
           ; first_due = P.Timestamp.to_time_ns timestamp
           })
  in
  let intent = Domain_tests.capture definition in
  let request = attempt intent 0 "delay:0" in
  let execution = Execution.create ~intent ~attempt:request |> protocol_ok in
  assert (Result.is_error (Basis.create execution ~definition));
  let origin, make_terminal = evidence intent request in
  let origin =
    Origin.to_json origin |> pad Origin.limits |> Origin.of_json |> protocol_ok
  in
  let terminal =
    make_terminal origin
    |> Reference.to_json
    |> pad Reference.limits
    |> Reference.of_json
    |> protocol_ok
  in
  let associated = Execution.associate execution origin |> protocol_ok in
  let settled = Execution.settle associated terminal |> protocol_ok in
  let basis = Basis.create settled ~definition |> protocol_ok in
  assert (I.Occurrence.equal (Basis.occurrence basis) (Intent.occurrence intent));
  assert (Origin.equal origin (Basis.origin basis));
  assert (Reference.equal terminal (Basis.terminal basis));
  let next = Basis.next_interval basis ~definition |> protocol_ok in
  [%test_eq: int64] (R.Cursor.Interval.slot next) 1L;
  assert (
    Time_ns.equal
      (R.Cursor.Interval.due_at next)
      (P.Timestamp.of_string "2026-08-15T12:00:01Z"
       |> protocol_ok
       |> P.Timestamp.to_time_ns));
  let rule =
    match F.schedule definition with
    | Interval { rule; _ } -> rule
    | Calendar _ -> assert false
  in
  let initial = F.initial_cursor definition in
  let waiting = A.Recurrence_cursor.consume_initial_delay initial ~rule |> protocol_ok in
  let rearmed =
    A.Recurrence_cursor.rearm_delay waiting ~occurrence:(Basis.occurrence basis) ~next
    |> protocol_ok
  in
  let nominal = A.Recurrence_nominal.of_interval_cursor definition next |> protocol_ok in
  [%test_eq: int64] (I.Occurrence.slot (A.Recurrence_nominal.key nominal)) 1L;
  assert (
    Time_ns.equal
      (A.Recurrence_nominal.at nominal)
      (P.Timestamp.of_string "2026-08-15T12:00:01Z"
       |> protocol_ok
       |> P.Timestamp.to_time_ns));
  let selected = A.Recurrence_cursor.consume_delay rearmed ~rule |> protocol_ok in
  (match A.Recurrence_cursor.position selected with
   | Awaiting_completion occurrence ->
     assert (I.Occurrence.equal occurrence (A.Recurrence_nominal.key nominal))
   | Interval _ | Calendar _ | Exhausted ->
     failwith "delay selection did not await its exact slot");
  assert (
    Result.is_error
      (A.Recurrence_cursor.rearm_delay initial ~occurrence:(Basis.occurrence basis) ~next));
  assert (
    Result.is_error
      (A.Recurrence_cursor.rearm_delay
         waiting
         ~occurrence:(A.Recurrence_nominal.key nominal)
         ~next));
  assert (
    Result.is_error
      (A.Recurrence_cursor.rearm_delay
         waiting
         ~occurrence:(Basis.occurrence basis)
         ~next:(R.Cursor.Interval.create ~first_due:(P.Timestamp.to_time_ns timestamp))));
  let raw =
    Basis.to_json basis
    |> fun json ->
    append json "future_basis" (`Number "1.00e+2")
    |> map_payload ~f:(fun payload -> append payload "future_phase" (`String "retained"))
    |> pad Basis.limits
  in
  let restored = Basis.of_json raw |> protocol_ok in
  assert (D.Json.equal raw (Basis.to_json restored));
  Basis.validate restored ~definition |> protocol_ok;
  assert (Result.is_error (Basis.of_json (append raw "overflow" `Null)));
  assert (
    Result.is_error (Basis.validate restored ~definition:(Due_support.definition "other")));
  let interrupted =
    make_terminal origin
    |> Reference.to_json
    |> map_payload ~f:(fun payload ->
      set payload "outcome" (`Object [ "kind", `String "interrupted" ]))
    |> Reference.of_json
    |> protocol_ok
  in
  let interrupted = Execution.settle associated interrupted |> protocol_ok in
  assert (Result.is_error (Basis.create interrupted ~definition));
  (* This is pure historical data, not an actual target observation. A valid
     terminal timestamp at the endpoint must be retainable even when no successor
     can be represented; the later owner must record a truthful timing hold. *)
  let terminal_at_end =
    Reference.to_json terminal
    |> map_payload ~f:(fun payload ->
      let receipt = field payload "receipt" in
      set
        payload
        "receipt"
        (set
           receipt
           "committed_at"
           (P.Timestamp.to_json (P.Timestamp.of_time_ns Time_ns.max_value_representable))))
    |> Reference.of_json
    |> protocol_ok
  in
  let at_end =
    Execution.settle associated terminal_at_end
    |> protocol_ok
    |> fun execution -> Basis.create execution ~definition |> protocol_ok
  in
  assert (Result.is_error (Basis.next_interval at_end ~definition));
  print_endline
    "full bounded proof children; exact completion + period; overflow stays data";
  [%expect
    {| full bounded proof children; exact completion + period; overflow stays data |}]
;;
