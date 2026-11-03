open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module Q = A.Moderator_queue
module D = Document_schema
module SD = A.Session_state_document

let set = Goal_catalog_tests.set
let member = Goal_catalog_tests.member

let source =
  P.Run_source.create
    ~observer:{ script_id = "queue-test"; source_sha256 = String.make 64 'a' }
    ~generation:0
    ~installation_epoch:1L
  |> protocol_ok
;;

let values count = List.init count ~f:(fun _ -> Session.Snapshot.String "equal")

let%expect_test "queue frontier agrees with independent occurrence sequence" =
  (* The model assigns integer identities to occurrences, independent of payload
     equality. Exercise every keep/consume and short append choice repeatedly. *)
  List.iter (List.init 16 ~f:Fn.id) ~f:(fun seed ->
    let metadata = ref (Q.bootstrap ~source ~queue:[] |> protocol_ok) in
    let model = ref [] in
    let next = ref 0 in
    for step = 0 to 31 do
      let consume = (not (List.is_empty !model)) && (seed + step) mod 2 = 0 in
      let appended = (seed + (step * 3)) mod 3 in
      let before = values (List.length !model) in
      let retained = if consume then List.tl_exn !model else !model in
      let added = List.init appended ~f:(fun offset -> !next + offset) in
      model := retained @ added;
      next := !next + appended;
      metadata
      := Q.advance
           !metadata
           ~before
           ~after:(values (List.length !model))
           ~consume:(if consume then Consume_head else Keep)
           ~appended:(List.init appended ~f:(fun _ -> Q.Capture.unavailable))
         |> protocol_ok;
      assert (
        List.equal
          Int64.equal
          (List.map (Q.entries !metadata) ~f:Q.Entry.ordinal)
          (List.map !model ~f:Int64.of_int));
      assert (Int64.equal (Q.next_ordinal !metadata) (Int64.of_int !next));
      let decoded = Q.to_jsonaf !metadata |> Q.of_jsonaf |> protocol_ok in
      Q.validate_queue decoded (values (List.length !model)) |> protocol_ok
    done);
  let empty = Q.bootstrap ~source ~queue:[] |> protocol_ok in
  let exhausted =
    Q.to_jsonaf empty
    |> fun raw ->
    set
      (set raw "head_ordinal" (`String (Int64.to_string Int64.max_value)))
      "next_ordinal"
      (`String (Int64.to_string Int64.max_value))
    |> Q.of_jsonaf
    |> protocol_ok
  in
  assert (
    Result.is_error
      (Q.advance
         exhausted
         ~before:[]
         ~after:(values 1)
         ~consume:Keep
         ~appended:[ Q.Capture.unavailable ]));
  let one = Q.bootstrap ~source ~queue:(values 1) |> protocol_ok in
  let raw = Q.to_jsonaf one in
  let row =
    match member raw "entries" with
    | `Array [ row ] -> row
    | _ -> assert false
  in
  List.iter [ "producer"; "origin" ] ~f:(fun field ->
    let missing =
      match row with
      | `Object fields ->
        `Object (List.filter fields ~f:(fun (name, _) -> not (String.equal name field)))
      | _ -> assert false
    in
    assert (Result.is_error (Q.of_jsonaf (set raw "entries" (`Array [ missing ])))));
  assert (
    Result.is_error
      (Q.of_jsonaf
         (set raw "entries" (`Array [ set row "payload_sha256" (`String "short") ]))));
  assert (
    Result.is_error
      (Q.advance one ~before:(values 1) ~after:[] ~consume:Keep ~appended:[]));
  assert (
    Result.is_error
      (Q.advance empty ~before:[] ~after:[] ~consume:Consume_head ~appended:[]));
  print_endline "512 transitions; strict marker, identity and frontier limits";
  [%expect {| 512 transitions; strict marker, identity and frontier limits |}]
;;

let%expect_test "queue occurrence unknowns follow ordinals through equal reemit" =
  Run_admission_tests.with_startup_actor (fun actor _ _ request start _ _ _ _ ->
    start (request "queue-carrier") |> protocol_ok |> ignore;
    let basis = A.Session_actor.state actor |> protocol_ok in
    let index = Option.value_exn basis.run_state in
    let source =
      A.Run_source_installation.captured
        (A.Run_state.installation index)
        ~generation:basis.identity.generation
      |> protocol_ok
    in
    let snapshot =
      A.Moderator_checkpoint.decode basis.moderator |> protocol_ok |> Option.value_exn
    in
    let before_snapshot = { snapshot with queued_internal_events = values 2 } in
    let metadata =
      Q.bootstrap ~source ~queue:before_snapshot.queued_internal_events |> protocol_ok
    in
    let moderator =
      A.Moderator_checkpoint.encode_with_queue before_snapshot ~queue:metadata
      |> protocol_ok
    in
    let before = { basis with moderator = Some moderator } in
    let raw =
      SD.authored before
      |> SD.encode ~limits:document_limits
      |> document_ok
      |> D.Document.json
    in
    let raw_queue =
      member (member (member raw "payload") "moderator") "queue_provenance"
    in
    let rows =
      match member raw_queue "entries" with
      | `Array rows -> rows
      | _ -> assert false
    in
    let rows =
      List.mapi rows ~f:(fun index row ->
        if index = 1
        then set row "future_occurrence" (`String (Int.to_string index))
        else row)
    in
    let captured ?(protected_head = false) future =
      let rows =
        List.mapi rows ~f:(fun index row ->
          if index = 1
          then set row "future_occurrence" (`String future)
          else if protected_head
          then set row "future_occurrence" (`String "protected-head")
          else row)
      in
      let moderator =
        set moderator "queue_provenance" (set raw_queue "entries" (`Array rows))
      in
      let raw = set raw "payload" (set (member raw "payload") "moderator" moderator) in
      D.Document.inspect ~limits:document_limits raw
      |> document_ok
      |> SD.decode ~limits:document_limits
      |> document_ok
    in
    let before = captured "1" in
    let historical_document =
      SD.authored basis |> SD.encode ~limits:document_limits |> document_ok
    in
    assert (
      not
        (List.mem
           (D.Document.required_semantics historical_document)
           A.Queue_provenance_semantics.name
           ~equal:String.equal));
    let padded name =
      let raw =
        SD.encode before ~limits:document_limits |> document_ok |> D.Document.json
      in
      let moderator = member (member raw "payload") "moderator" in
      let queue =
        set
          (member moderator "queue_provenance")
          name
          (`String (String.make (600 * 1024) 'x'))
      in
      let raw =
        set
          raw
          "payload"
          (set
             (member raw "payload")
             "moderator"
             (set moderator "queue_provenance" queue))
      in
      D.Document.inspect ~limits:document_limits raw
      |> document_ok
      |> SD.decode ~limits:document_limits
      |> document_ok
    in
    assert (
      Result.is_error
        (SD.adopt (padded "future_left") ~limits:document_limits (padded "future_right")));
    let raw_of_carrier = SD.encode before ~limits:document_limits |> document_ok in
    assert (
      List.mem
        (D.Document.required_semantics raw_of_carrier)
        A.Queue_provenance_semantics.name
        ~equal:String.equal);
    let strip document =
      let raw = D.Document.json document in
      let raw =
        match raw with
        | `Object fields ->
          `Object
            (List.filter fields ~f:(fun (name, _) ->
               not (String.equal name "required_semantics")))
        | _ -> assert false
      in
      D.Document.inspect ~limits:document_limits raw |> document_ok
    in
    assert (Result.is_error (SD.decode ~limits:document_limits (strip raw_of_carrier)));
    let oversized =
      set raw_queue "future_buffer" (`String (String.make (1024 * 1024) 'x'))
    in
    let oversized_raw =
      set
        raw
        "payload"
        (set
           (member raw "payload")
           "moderator"
           (set moderator "queue_provenance" oversized))
    in
    assert (
      Result.is_error
        (D.Document.inspect ~limits:document_limits oversized_raw
         |> document_ok
         |> SD.decode ~limits:document_limits));
    let delta_document =
      A.Session_delta_document.create
        (Moderator_changed (SD.value before).moderator)
        ~limits:document_limits
        ~state_document:SD.authored
      |> document_ok
      |> A.Session_delta_document.document
    in
    assert (
      List.mem
        (D.Document.required_semantics delta_document)
        A.Queue_provenance_semantics.name
        ~equal:String.equal);
    assert (
      Result.is_error
        (A.Session_delta_document.decode ~limits:document_limits (strip delta_document)));
    let raw_delta = D.Document.json delta_document in
    let oversized_delta =
      set
        raw_delta
        "payload"
        (`Object
            [ ( "changes"
              , `Array
                  [ `Object
                      [ "kind", `String "moderator_changed"
                      ; "moderator", set moderator "queue_provenance" oversized
                      ]
                  ] )
            ])
    in
    assert (
      Result.is_error
        (D.Document.inspect ~limits:document_limits oversized_delta
         |> document_ok
         |> A.Session_delta_document.decode ~limits:document_limits));
    let after_metadata =
      Q.advance
        metadata
        ~before:(values 2)
        ~after:(values 2)
        ~consume:Consume_head
        ~appended:[ Q.Capture.unavailable ]
      |> protocol_ok
    in
    let after_snapshot = { before_snapshot with queued_internal_events = values 2 } in
    let moderator =
      A.Moderator_checkpoint.encode_with_queue after_snapshot ~queue:after_metadata
      |> protocol_ok
    in
    let after = { (SD.value before) with moderator = Some moderator } in
    assert (
      Result.is_error
        (SD.with_value (captured ~protected_head:true "1") after
         |> SD.encode ~limits:document_limits));
    let encoded =
      SD.with_value before after
      |> SD.encode ~limits:document_limits
      |> document_ok
      |> D.Document.json
    in
    let rows =
      member
        (member (member (member encoded "payload") "moderator") "queue_provenance")
        "entries"
    in
    (match rows with
     | `Array [ retained; fresh ] ->
       assert (D.Json.equal (member retained "ordinal") (`String "1"));
       assert (D.Json.equal (member retained "future_occurrence") (`String "1"));
       assert (D.Json.equal (member fresh "ordinal") (`String "2"));
       assert (
         match D.Json.field fresh ~name:"future_occurrence" with
         | Absent -> true
         | _ -> false)
     | _ -> assert false);
    assert (
      Result.is_error (SD.adopt before ~limits:document_limits (captured "conflict")));
    assert (
      Result.is_error
        (SD.with_value
           before
           { after with moderator = Some (A.Moderator_checkpoint.encode after_snapshot) }
         |> SD.encode ~limits:document_limits));
    print_endline
      "unknowns retained by occurrence; independent conflict and same-source erasure \
       reject");
  [%expect
    {| unknowns retained by occurrence; independent conflict and same-source erasure reject |}]
;;

let%expect_test "complete ordered checkpoint admits producer then retains its evidence" =
  Run_admission_tests.with_startup_actor (fun actor _ _ request start _ _ _ _ ->
    start (request "queue-final-boundary") |> protocol_ok |> ignore;
    let basis = A.Session_actor.state actor |> protocol_ok in
    let index = Option.value_exn basis.run_state in
    let source =
      A.Run_source_installation.captured
        (A.Run_state.installation index)
        ~generation:basis.identity.generation
      |> protocol_ok
    in
    let before =
      A.Moderator_checkpoint.decode basis.moderator |> protocol_ok |> Option.value_exn
    in
    (* An explicit stored-history fixture, not execution authorization. The
       final validator must see the complete ordered result, not a leaf prefix. *)
    let context : P.Moderator_execution.context =
      { id = P.Id.Moderator_execution.of_string "mex_queue_producer" |> protocol_ok
      ; session_id
      ; generation = basis.identity.generation
      ; source = source.observer
      ; operation_id = None
      ; job = None
      ; phase = Internal_event
      ; event = `Null
      ; checkpoint_sha256 = String.make 64 'a'
      ; created_at = timestamp
      }
    in
    let running = P.Moderator_execution.create context |> protocol_ok in
    let producer =
      P.Moderator_execution.complete
        running
        ~checkpoint_sha256:(String.make 64 'b')
        ~requests:{ request_turn = false; request_compaction = false; end_session = None }
      |> protocol_ok
    in
    let metadata =
      Q.bootstrap ~source ~queue:before.queued_internal_events |> protocol_ok
    in
    let after =
      { before with queued_internal_events = before.queued_internal_events @ values 1 }
    in
    let capture =
      Q.Capture.create ~producer:(Event context.id) ~origin:(Some Unbound) |> protocol_ok
    in
    let metadata =
      Q.advance
        metadata
        ~before:before.queued_internal_events
        ~after:after.queued_internal_events
        ~consume:Keep
        ~appended:[ capture ]
      |> protocol_ok
    in
    let moderator =
      A.Moderator_checkpoint.encode_with_queue after ~queue:metadata |> protocol_ok
    in
    let queue_change = A.Session_delta.Moderator_changed (Some moderator) in
    let candidate =
      A.Session_transition.apply
        ~now:timestamp
        basis
        ~delta:
          (Batch
             [ Moderator_execution_changed running
             ; queue_change
             ; Moderator_execution_changed producer
             ])
        ~payloads:[]
      |> protocol_ok
    in
    let previous = SD.authored basis in
    let durable =
      A.Session_document_transition.admit
        previous
        ~delta:candidate.delta
        ~next:candidate.state
        ~limits:document_limits
      |> document_ok
    in
    SD.encode durable ~limits:document_limits |> document_ok |> ignore;
    assert (
      Result.is_error
        (A.Session_transition.apply
           ~now:timestamp
           basis
           ~delta:(Batch [ queue_change ])
           ~payloads:[]));
    let pruned =
      { candidate.state with moderator_executions = basis.moderator_executions }
    in
    assert (
      Result.is_error (SD.with_value durable pruned |> SD.encode ~limits:document_limits));
    let erased = { candidate.state with moderator = basis.moderator } in
    assert (
      Result.is_error
        (A.Session_transition.apply
           ~now:timestamp
           candidate.state
           ~delta:(Created erased)
           ~payloads:[]));
    print_endline
      "ordered final state accepted; missing producer, pruning and Created erasure reject");
  [%expect
    {| ordered final state accepted; missing producer, pruning and Created erasure reject |}]
;;

let%expect_test
    "positive Schedule survives actual public event and replacement snapshot carriers"
  =
  Run_admission_tests.with_startup_actor (fun actor _ _ request start _ _ _ _ ->
    start (request "schedule-public-provenance") |> protocol_ok |> ignore;
    let basis = A.Session_actor.state actor |> protocol_ok in
    let index = Option.value_exn basis.run_state in
    let source =
      A.Run_source_installation.captured
        (A.Run_state.installation index)
        ~generation:basis.identity.generation
      |> protocol_ok
    in
    let provenance =
      P.Schedule_provenance.create ~source ~origin:(Some Unbound) |> protocol_ok
    in
    List.iter [ false; true ] ~f:(fun owned ->
      let schedule : P.Schedule.t =
        { id = P.Id.Schedule.of_string "sch_public_provenance" |> protocol_ok
        ; session_id
        ; generation = basis.identity.generation
        ; payload = `String "wake"
        ; created_at = timestamp
        ; next_due_at = timestamp
        ; misfire = Deliver_once_immediately
        ; status = Scheduled
        ; delivery_count = 0
        ; last_delivery_at = None
        ; ownership =
            (if owned
             then
               Some
                 { source = source.observer
                 ; creator =
                     Moderator_event
                       (P.Id.Moderator_execution.of_string "mex_schedule_owner"
                        |> protocol_ok)
                 ; subscription = None
                 }
             else None)
        ; inference_provenance = Some provenance
        ; delivery_cancellation = None
        }
      in
      if not owned
      then (
        let document =
          SD.authored { basis with schedules = [ schedule ] }
          |> SD.encode ~limits:document_limits
          |> document_ok
        in
        assert (
          List.mem
            (D.Document.required_semantics document)
            A.Queue_provenance_semantics.name
            ~equal:String.equal);
        let document =
          A.Session_delta_document.create
            (Schedule_changed schedule)
            ~limits:document_limits
            ~state_document:SD.authored
          |> document_ok
          |> A.Session_delta_document.document
        in
        assert (
          List.mem
            (D.Document.required_semantics document)
            A.Queue_provenance_semantics.name
            ~equal:String.equal));
      let event =
        P.Event.Durable.of_payload
          ~session_id
          ~sequence:1L
          ~revision:1L
          ~timestamp
          (Schedule_created schedule)
      in
      let read event =
        let document =
          A.Durable_event_document.create event ~limits:document_limits
          |> document_ok
          |> A.Durable_event_document.document
        in
        A.Durable_event_document.decode ~limits:document_limits document
        |> document_ok
        |> A.Durable_event_document.value
      in
      let restored = read event in
      (match
         P.Event.Durable.Payload.of_json ~kind:restored.kind restored.payload
         |> protocol_ok
       with
       | Schedule_created schedule ->
         assert (
           Option.equal
             P.Schedule_provenance.equal
             (Some provenance)
             schedule.inference_provenance)
       | _ -> assert false);
      let snapshot =
        { (A.Session_state.snapshot ~now:timestamp basis) with schedules = [ schedule ] }
      in
      let sequence = Int64.succ basis.counters.event_sequence in
      let summary =
        { (A.Session_state.summary basis) with latest_event_sequence = sequence }
      in
      let session_event =
        P.Event.Durable.of_payload
          ~session_id
          ~sequence
          ~revision:basis.counters.revision
          ~timestamp
          (Session_updated summary)
      in
      let restored =
        P.Event.Durable.with_replacement_snapshot session_event snapshot |> read
      in
      let snapshot =
        P.Event.Durable.replacement_snapshot restored |> protocol_ok |> Option.value_exn
      in
      assert (
        Option.equal
          P.Schedule_provenance.equal
          (Some provenance)
          (List.hd_exn snapshot.schedules).inference_provenance));
    print_endline "owned and unowned v4 survive event and replacement snapshot codecs");
  [%expect {| owned and unowned v4 survive event and replacement snapshot codecs |}]
;;
