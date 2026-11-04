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

module Consumed_receipt = struct
  module E = P.Moderator_execution
  module C = E.Queue_occurrence

  let encoded_event event = `Object [ "snapshot", Session.Snapshot.to_jsonaf event ]

  let rec idle actor remaining =
    if remaining = 0
    then failwith "queue drainer did not settle within bounded actor polls";
    let state = A.Session_actor.state actor |> protocol_ok in
    match state.active_operation with
    | None -> state
    | Some _ ->
      Eio.Fiber.yield ();
      idle actor (remaining - 1)
  ;;

  let empty_requests : P.Invocation.follow_up =
    { request_turn = false; request_compaction = false; end_session = None }
  ;;

  let with_actual_drainer f =
    Headless_run_tests.with_service (fun fixture ->
      let original =
        fixture.start (fixture.request "queue-original-run")
        |> Headless_run_tests.admitted
      in
      assert (fixture.activate () |> protocol_ok);
      let admitted =
        fixture.request
          ~mode:Single_turn
          ~input:(Headless_run_tests.input "physical queue drainer")
          "queue-drainer-run"
        |> fixture.start
        |> Headless_run_tests.admitted
      in
      let basis = idle fixture.actor 256 in
      let index = Option.value_exn basis.run_state in
      let drainer = A.Run_state.find index admitted.run_id |> Option.value_exn in
      let operation =
        drainer.owned_work
        @ drainer.relinquished_work
        @ List.map drainer.terminal_work ~f:(fun proof -> proof.P.Run_work.Terminal.work)
        |> List.find_map ~f:(fun work ->
          match work.P.Run_work.key with
          | Operation id -> Some id
          | Retained _ -> None)
        |> Option.value_exn
      in
      assert (not (P.Id.Run.equal drainer.id original.run_id));
      f basis original.source original.run_id drainer.id operation)
  ;;

  let before basis source operation origin =
    (* The real host above admitted both Runs and the drainer operation. This
       imported producer/queue is storage evidence, not another execution permit. *)
    let parent =
      let parent = invocation_fixture () in
      let parent =
        P.Invocation.create
          { parent.context with
            id = P.Id.Invocation.of_string "inv_queue_producer_parent" |> protocol_ok
          }
        |> protocol_ok
      in
      Option.value_map origin ~default:parent ~f:(fun origin ->
        P.Invocation.capture_inference_origin parent origin |> protocol_ok)
    in
    let invocation =
      let context = (invocation_fixture ()).context in
      let invocation =
        P.Invocation.create
          ~observer:source.P.Run_source.observer
          { context with parent_invocation = Some parent.context.id }
        |> protocol_ok
      in
      let invocation =
        Option.value_map origin ~default:invocation ~f:(fun origin ->
          P.Invocation.capture_inference_origin invocation origin |> protocol_ok)
      in
      let invocation = P.Invocation.dispatch invocation |> protocol_ok in
      P.Invocation.resolve
        invocation
        ~session_id
        ~generation:basis.A.Session_state.identity.generation
        (Complete `Null)
      |> protocol_ok
    in
    let snapshot =
      A.Moderator_checkpoint.decode basis.moderator |> protocol_ok |> Option.value_exn
    in
    let queue = values 2 in
    let producer =
      Q.Capture.create ~producer:(Invocation invocation.context.id) ~origin |> protocol_ok
    in
    let metadata =
      Q.bootstrap ~source ~queue:[]
      |> protocol_ok
      |> fun metadata ->
      Q.advance
        metadata
        ~before:[]
        ~after:queue
        ~consume:Keep
        ~appended:[ producer; producer ]
      |> protocol_ok
    in
    let moderator =
      A.Moderator_checkpoint.encode_with_queue
        { snapshot with queued_internal_events = queue }
        ~queue:metadata
      |> protocol_ok
    in
    let before =
      { basis with
        moderator = Some moderator
      ; invocations = invocation :: parent :: basis.invocations
      }
    in
    A.Session_state.validate before |> protocol_ok;
    before, operation
  ;;

  let running before operation =
    let snapshot =
      A.Moderator_checkpoint.decode before.A.Session_state.moderator
      |> protocol_ok
      |> Option.value_exn
    in
    let metadata =
      A.Moderator_checkpoint.queue before.moderator |> protocol_ok |> Option.value_exn
    in
    let entry = List.hd_exn (Q.entries metadata) in
    let occurrence =
      C.create
        ~source:(Q.source metadata)
        ~ordinal:(Q.Entry.ordinal entry)
        ~origin:(Q.Entry.origin entry)
      |> protocol_ok
    in
    E.create_queued
      ~queue_occurrence:occurrence
      { id = P.Id.Moderator_execution.of_string "mex_consumed_queue" |> protocol_ok
      ; session_id
      ; generation = before.identity.generation
      ; source = (Q.source metadata).observer
      ; operation_id = operation
      ; job = None
      ; phase = Internal_event
      ; event = encoded_event (List.hd_exn snapshot.queued_internal_events)
      ; checkpoint_sha256 = A.Moderator_checkpoint.digest snapshot
      ; created_at = timestamp
      }
    |> protocol_ok
  ;;

  let completion ?(emit = true) before running =
    let snapshot =
      A.Moderator_checkpoint.decode before.A.Session_state.moderator
      |> protocol_ok
      |> Option.value_exn
    in
    let metadata =
      A.Moderator_checkpoint.queue before.moderator |> protocol_ok |> Option.value_exn
    in
    let origin = C.origin (Option.value_exn running.E.queue_occurrence) in
    let after_queue =
      List.tl_exn snapshot.queued_internal_events @ if emit then values 1 else []
    in
    let capture =
      Q.Capture.create ~producer:(Event running.context.id) ~origin |> protocol_ok
    in
    let next =
      Q.advance
        metadata
        ~before:snapshot.queued_internal_events
        ~after:after_queue
        ~consume:Consume_head
        ~appended:(if emit then [ capture ] else [])
      |> protocol_ok
    in
    let snapshot = { snapshot with queued_internal_events = after_queue } in
    let completed =
      E.complete
        running
        ~checkpoint_sha256:(A.Moderator_checkpoint.digest snapshot)
        ~requests:empty_requests
      |> protocol_ok
    in
    let moderator =
      A.Moderator_checkpoint.encode_with_queue snapshot ~queue:next |> protocol_ok
    in
    completed, moderator
  ;;

  let document state =
    SD.authored state |> SD.encode ~limits:document_limits |> document_ok
  ;;

  let decode raw =
    D.Document.inspect ~limits:document_limits raw
    |> document_ok
    |> SD.decode ~limits:document_limits
  ;;

  let replace_event raw event =
    let payload = member raw "payload" in
    let events =
      match member payload "moderator_executions" with
      | `Array events -> events
      | _ -> assert false
    in
    let events =
      List.map events ~f:(fun candidate ->
        if
          D.Json.equal
            (member candidate "id")
            (P.Id.Moderator_execution.to_json event.E.context.id)
        then E.to_json event
        else candidate)
    in
    set raw "payload" (set payload "moderator_executions" (`Array events))
  ;;
end

let%expect_test
    "consumed receipts follow original queue facts under an actual Run drainer"
  =
  let module F = Consumed_receipt in
  let module E = P.Moderator_execution in
  let module C = E.Queue_occurrence in
  F.with_actual_drainer (fun basis source original drainer operation ->
    List.iter
      [ None; Some P.Inference_origin.Unbound; Some (Run original) ]
      ~f:(fun origin ->
        let before, _ = F.before basis source operation origin in
        let running = F.running before (Some operation) in
        let completed, moderator = F.completion before running in
        let transition =
          A.Session_transition.apply
            ~now:timestamp
            before
            ~delta:
              (Batch
                 [ Moderator_execution_changed running
                 ; Moderator_execution_changed completed
                 ; Moderator_changed (Some moderator)
                 ])
            ~payloads:[]
          |> protocol_ok
        in
        let after = transition.state in
        let expected =
          A.Invocation_inference_provenance.of_event
            ~runs:after.run_state
            ~jobs:after.jobs
            completed
          |> protocol_ok
        in
        assert (Option.equal P.Inference_origin.equal origin expected);
        assert (
          Option.equal
            P.Id.Operation.equal
            completed.context.operation_id
            (Some operation));
        assert (not (Option.equal P.Inference_origin.equal expected (Some (Run drainer))));
        let encoded = F.document after |> D.Document.json in
        let decoded = F.decode encoded |> document_ok |> SD.value in
        let saved =
          List.find_exn decoded.moderator_executions ~f:(fun event ->
            P.Id.Moderator_execution.equal event.context.id completed.context.id)
        in
        assert (E.equal saved completed);
        let contradict event =
          assert (Result.is_error (F.decode (F.replace_event encoded event)))
        in
        contradict running;
        contradict (E.interrupt running ~reason:"honest failed callback" |> protocol_ok);
        let occurrence = Option.value_exn completed.queue_occurrence in
        let wrong = C.create ~source ~ordinal:1L ~origin |> protocol_ok in
        let wrong =
          E.create_queued ~queue_occurrence:wrong running.context
          |> protocol_ok
          |> fun running ->
          E.complete
            running
            ~checkpoint_sha256:(String.make 64 'a')
            ~requests:F.empty_requests
          |> protocol_ok
        in
        contradict wrong;
        let missing_frontier =
          set
            encoded
            "payload"
            (set
               (member encoded "payload")
               "moderator"
               (A.Moderator_checkpoint.decode after.moderator
                |> protocol_ok
                |> Option.value_exn
                |> A.Moderator_checkpoint.encode))
        in
        assert (Result.is_error (F.decode missing_frontier));
        let duplicate =
          E.to_json completed
          |> fun json ->
          set
            json
            "id"
            (P.Id.Moderator_execution.to_json
               (P.Id.Moderator_execution.of_string "mex_duplicate_occurrence"
                |> protocol_ok))
        in
        let payload = member encoded "payload" in
        let events =
          match member payload "moderator_executions" with
          | `Array events -> events
          | _ -> assert false
        in
        assert (
          Result.is_error
            (F.decode
               (set
                  encoded
                  "payload"
                  (set payload "moderator_executions" (`Array (duplicate :: events))))));
        (* Removing metadata cannot lend the new receipt a guessed original origin.
         Genuine bootstrap only assigns structural IDs with unavailable capture. *)
        let snapshot =
          A.Moderator_checkpoint.decode before.moderator
          |> protocol_ok
          |> Option.value_exn
        in
        let historical =
          { before with moderator = Some (A.Moderator_checkpoint.encode snapshot) }
        in
        let admitted =
          A.Session_delta.apply historical (Moderator_execution_changed running)
        in
        assert (Bool.equal (Result.is_ok admitted) (Option.is_none origin));
        let foreign_checkpoint =
          E.create_queued
            ~queue_occurrence:occurrence
            { running.context with checkpoint_sha256 = String.make 64 'c' }
          |> protocol_ok
        in
        assert (
          Result.is_error
            (A.Session_delta.apply
               before
               (Moderator_execution_changed foreign_checkpoint)));
        let other_payload =
          E.create_queued
            ~queue_occurrence:occurrence
            { running.context with
              event = F.encoded_event (Session.Snapshot.String "other")
            }
          |> protocol_ok
        in
        assert (
          Result.is_error
            (A.Session_delta.apply before (Moderator_execution_changed other_payload)));
        let cleared =
          A.Run_retirement.replace
            (Option.value_exn after.run_state)
            ~change:Remove
            ~session_revision:Int64.(after.counters.revision + 1L)
            ~now:timestamp
          |> protocol_ok
        in
        let older = { after with moderator = None; run_state = Some cleared } in
        F.document older |> D.Document.json |> F.decode |> document_ok |> ignore;
        let historical_failure =
          E.interrupt running ~reason:"old interrupted callback" |> protocol_ok
        in
        let older_failure =
          { older with
            moderator_executions = historical_failure :: basis.moderator_executions
          }
        in
        let older_carrier =
          F.document older_failure |> D.Document.json |> F.decode |> document_ok
        in
        SD.encode
          (SD.with_value
             older_carrier
             { older_failure with moderator_executions = basis.moderator_executions })
          ~limits:document_limits
        |> document_ok
        |> ignore;
        assert (
          Result.is_error
            (F.decode (F.replace_event (F.document older |> D.Document.json) running)));
        let future_source =
          P.Run_source.create
            ~observer:source.observer
            ~generation:source.generation
            ~installation_epoch:Int64.(source.installation_epoch + 1L)
          |> protocol_ok
        in
        let future = C.create ~source:future_source ~ordinal:0L ~origin |> protocol_ok in
        let future =
          E.create_queued ~queue_occurrence:future running.context
          |> protocol_ok
          |> fun running ->
          E.complete
            running
            ~checkpoint_sha256:(String.make 64 'a')
            ~requests:F.empty_requests
          |> protocol_ok
        in
        contradict future));
  print_endline
    "three original origins; exact head; standalone frontier; honest older terminal";
  [%expect
    {| three original origins; exact head; standalone frontier; honest older terminal |}]
;;

let%expect_test
    "receipt capture carriers preserve unfamiliar children and reject reclassification"
  =
  let module F = Consumed_receipt in
  let module E = P.Moderator_execution in
  let module C = E.Queue_occurrence in
  F.with_actual_drainer (fun basis source _ _ operation ->
    let before, _ = F.before basis source operation None in
    let running = F.running before (Some operation) in
    (* Known-only carrier: unfamiliar-field protection must not mask the domain
       rule against erasing an unsettled receipt and reclaiming its same head. *)
    List.iter
      [ running; E.interrupt running ~reason:"blocked callback" |> protocol_ok ]
      ~f:(fun unsettled ->
        let claimed =
          { before with moderator_executions = unsettled :: before.moderator_executions }
        in
        let carrier = F.document claimed |> D.Document.json |> F.decode |> document_ok in
        let erased = F.document before |> D.Document.json |> F.decode |> document_ok in
        assert (
          Result.is_error
            (SD.encode (SD.with_value carrier before) ~limits:document_limits));
        assert (Result.is_error (SD.adopt carrier ~limits:document_limits erased));
        let fresh =
          E.to_json running
          |> fun json ->
          set
            json
            "id"
            (P.Id.Moderator_execution.to_json
               (P.Id.Moderator_execution.of_string "mex_reclaimed_head" |> protocol_ok))
          |> E.of_json
          |> protocol_ok
        in
        assert (
          Result.is_error
            (A.Session_delta.apply
               claimed
               (Batch [ Created before; Moderator_execution_changed fresh ]))));
    let settled, settled_moderator = F.completion ~emit:false before running in
    let settled_state =
      A.Session_transition.apply
        ~now:timestamp
        before
        ~delta:
          (Batch
             [ Moderator_execution_changed running
             ; Moderator_execution_changed settled
             ; Moderator_changed (Some settled_moderator)
             ])
        ~payloads:[]
      |> protocol_ok
      |> fun (transition : A.Session_transition.t) -> transition.state
    in
    let settled_carrier =
      F.document settled_state |> D.Document.json |> F.decode |> document_ok
    in
    let pruned =
      { settled_state with moderator_executions = before.moderator_executions }
    in
    SD.encode (SD.with_value settled_carrier pruned) ~limits:document_limits
    |> document_ok
    |> ignore;
    A.Session_delta.apply settled_state (Created pruned) |> protocol_ok |> ignore;
    let completed, moderator = F.completion before running in
    let transition =
      A.Session_transition.apply
        ~now:timestamp
        before
        ~delta:
          (Batch
             [ Moderator_execution_changed running
             ; Moderator_execution_changed completed
             ; Moderator_changed (Some moderator)
             ])
        ~payloads:[]
      |> protocol_ok
    in
    let raw = F.document transition.state |> D.Document.json in
    let map_receipt raw ~f =
      let payload = member raw "payload" in
      let receipts =
        match member payload "moderator_executions" with
        | `Array receipts -> receipts
        | _ -> assert false
      in
      let receipts =
        List.map receipts ~f:(fun receipt ->
          if
            D.Json.equal
              (member receipt "id")
              (P.Id.Moderator_execution.to_json completed.context.id)
          then f receipt
          else receipt)
      in
      set raw "payload" (set payload "moderator_executions" (`Array receipts))
    in
    let future raw value =
      map_receipt raw ~f:(fun receipt ->
        set
          receipt
          "queue_occurrence"
          (set (member receipt "queue_occurrence") "future_occurrence" value))
    in
    let retained = future raw (`String "retained") in
    let carrier = F.decode retained |> document_ok in
    assert (
      D.Json.equal
        retained
        (SD.encode carrier ~limits:document_limits |> document_ok |> D.Document.json));
    let current = SD.value carrier in
    SD.with_value carrier { current with stop_epoch = Int64.succ current.stop_epoch }
    |> SD.encode ~limits:document_limits
    |> document_ok
    |> ignore;
    let conflict = F.decode (future raw (`String "conflicting")) |> document_ok in
    assert (Result.is_error (SD.adopt carrier ~limits:document_limits conflict));
    assert (Result.is_error (F.decode (future raw (`String (String.make 4096 'x')))));
    let changed = C.create ~source ~ordinal:0L ~origin:(Some Unbound) |> protocol_ok in
    let changed =
      E.create_queued ~queue_occurrence:changed running.context
      |> protocol_ok
      |> fun event ->
      E.complete
        event
        ~checkpoint_sha256:
          (A.Moderator_checkpoint.digest
             (A.Moderator_checkpoint.decode current.moderator
              |> protocol_ok
              |> Option.value_exn))
        ~requests:F.empty_requests
      |> protocol_ok
    in
    let erased =
      E.create running.context
      |> protocol_ok
      |> fun event ->
      E.complete
        event
        ~checkpoint_sha256:
          (A.Moderator_checkpoint.digest
             (A.Moderator_checkpoint.decode current.moderator
              |> protocol_ok
              |> Option.value_exn))
        ~requests:F.empty_requests
      |> protocol_ok
    in
    List.iter [ changed; erased ] ~f:(fun alternative ->
      let incoming = F.decode (F.replace_event raw alternative) |> document_ok in
      assert (Result.is_error (SD.adopt carrier ~limits:document_limits incoming));
      let alternative = SD.value incoming in
      assert (
        Result.is_error
          (SD.encode (SD.with_value carrier alternative) ~limits:document_limits));
      assert (
        Result.is_error
          (A.Session_transition.apply
             ~now:timestamp
             current
             ~delta:(Created alternative)
             ~payloads:[])));
    (* A preserved child belongs to the capture, not to a new occurrence. No
       unknown data is moved from a consumed queue head into the receipt. *)
    let delta =
      A.Session_delta_document.create
        transition.delta
        ~limits:document_limits
        ~state_document:SD.authored
      |> document_ok
      |> A.Session_delta_document.document
      |> D.Document.json
    in
    let payload = member delta "payload" in
    let changes =
      match member payload "changes" with
      | `Array changes -> changes
      | _ -> assert false
    in
    let changes =
      List.map changes ~f:(fun change ->
        match D.Json.field change ~name:"kind" with
        | Value (`String "moderator_execution_changed") ->
          let receipt = member change "value" in
          set
            change
            "value"
            (set
               receipt
               "queue_occurrence"
               (set
                  (member receipt "queue_occurrence")
                  "future_occurrence"
                  (`String (String.make 4096 'x'))))
        | Value _ | Absent | Null -> change)
    in
    let malformed = set delta "payload" (set payload "changes" (`Array changes)) in
    assert (
      Result.is_error
        (D.Document.inspect ~limits:document_limits malformed
         |> Result.bind ~f:(A.Session_delta_document.decode ~limits:document_limits))));
  print_endline "unknown capture custody; no late adoption/removal; full raw child bound";
  [%expect {| unknown capture custody; no late adoption/removal; full raw child bound |}]
;;
