open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module Settlement = A.Run_producer_settlement

(* Real admission supplies the retained Run/source. Schedule snapshots below are
   pure validated transition DATA, never execution or callback authority. *)
let with_admitted ?(mode = P.Run.Mode.Workflow) f =
  Run_admission_tests.with_startup_actor
    ~mode
    (fun actor _ _ request start manager _ _ _ ->
       ignore (start (request "schedule-evidence") |> protocol_ok : P.Run_receipt.t);
       let state = A.Session_actor.state actor |> protocol_ok in
       let index = Option.value_exn state.run_state in
       let run = List.hd_exn (A.Run_state.runs index) in
       assert (P.Run.Mode.equal run.mode mode);
       let snapshot =
         Chat_response.Moderator_manager.identity_snapshot manager
         |> Result.ok_or_failwith
       in
       let executing, _ =
         A.Queued_moderator_event.claim_ordinary
           ~state
           ~id:(P.Id.Moderator_execution.of_string "mex_schedule_evidence" |> protocol_ok)
           ~snapshot
           ~operation_id:None
           ~event:Session_start
           ~now:timestamp
         |> protocol_ok
       in
       let schedule : P.Schedule.t =
         { id = P.Id.Schedule.of_string "sch_evidence" |> protocol_ok
         ; session_id = state.identity.session_id
         ; generation = state.identity.generation
         ; payload = `String "captured timer"
         ; created_at = timestamp
         ; next_due_at = timestamp
         ; misfire = Deliver_once_immediately
         ; status = Scheduled
         ; delivery_count = 0
         ; last_delivery_at = None
         ; ownership =
             Some
               { source = run.source.observer
               ; creator = Moderator_event executing.context.id
               ; subscription = None
               }
         ; inference_provenance =
             Some
               (P.Schedule_provenance.create
                  ~source:run.source
                  ~origin:(Some (Run run.id))
                |> protocol_ok)
         ; delivery_cancellation = None
         }
       in
       P.Schedule.validate schedule |> protocol_ok;
       let work =
         P.Run_work.create
           ~key:(Retained (Schedule schedule.id))
           ~generation:schedule.generation
         |> protocol_ok
       in
       let state =
         A.Session_delta.apply state (Moderator_execution_changed executing)
         |> protocol_ok
       in
       f state index run executing schedule work)
;;

let with_work state index (run : P.Run.t) schedule work ~lifecycle =
  let run =
    P.Run.create
      ~id:run.id
      ~session:run.session
      ~principal_id:run.principal_id
      ~source:run.source
      ~mode:run.mode
      ~lifecycle
      ~revision:(Int64.succ run.revision)
      ~owned_work:[ work ]
      ~relinquished_work:[]
      ~terminal_work:[]
      ~created_at:run.created_at
      ~updated_at:timestamp
    |> protocol_ok
  in
  let index = A.Run_state.advance index ~run ~intents:[] |> protocol_ok in
  let state =
    { state with A.Session_state.run_state = Some index; schedules = [ schedule ] }
  in
  A.Session_state.validate state |> protocol_ok;
  state, run
;;

let project state delta =
  Settlement.prepare state ~delta ~now:timestamp
  |> protocol_ok
  |> A.Session_delta.apply state
  |> protocol_ok
;;

let run state id =
  Option.value_exn state.A.Session_state.run_state
  |> fun index -> A.Run_state.find index id |> Option.value_exn
;;

let check_proof (previous : P.Run.t) (current : P.Run.t) work outcome =
  [%test_eq: int64] current.revision (Int64.succ previous.revision);
  [%test_eq: int] (List.length current.terminal_work) 1;
  let proof = List.hd_exn current.terminal_work in
  assert (P.Run_work.equal proof.work work);
  assert (P.Run_work.Terminal.equal_outcome proof.outcome outcome);
  [%test_eq: int64] proof.revision current.revision
;;

let%expect_test "actual Schedule producer evidence preserves aggregate and Wait laws" =
  with_admitted ~mode:Single_turn (fun state index admitted _executing schedule work ->
    let delivered =
      { schedule with
        P.Schedule.status = Delivered
      ; delivery_count = 1
      ; last_delivery_at = Some timestamp
      }
    in
    let delivering = { schedule with P.Schedule.status = Delivering } in
    let success_delta =
      A.Session_delta.Batch [ Schedule_changed delivering; Schedule_changed delivered ]
    in
    let single, before = with_work state index admitted schedule work ~lifecycle:Active in
    let after = project single success_delta in
    let completed = run after before.id in
    check_proof before completed work Succeeded;
    assert (P.Run.Lifecycle.equal completed.lifecycle (Terminal (Completed None)));
    [%test_eq: int]
      (List.count
         (A.Run_state.receipts (Option.value_exn after.run_state))
         ~f:(fun receipt -> P.Run_receipt.Kind.equal receipt.kind Terminal))
      1;
    (* Repeating an unchanged already-terminal producer never backfills/bump. *)
    let replay = project after (Schedule_changed delivered) in
    assert (Option.equal A.Run_state.equal after.run_state replay.run_state));
  with_admitted (fun state index admitted _executing schedule work ->
    let delivered =
      { schedule with
        P.Schedule.status = Delivered
      ; delivery_count = 1
      ; last_delivery_at = Some timestamp
      }
    in
    let delivering = { schedule with P.Schedule.status = Delivering } in
    let success_delta =
      A.Session_delta.Batch [ Schedule_changed delivering; Schedule_changed delivered ]
    in
    let workflow, before =
      with_work state index admitted schedule work ~lifecycle:Active
    in
    let after = project workflow success_delta in
    let current = run after before.id in
    check_proof before current work Succeeded;
    assert (P.Run.Lifecycle.equal current.lifecycle Active);
    let ownership = Option.value_exn schedule.ownership in
    let wake =
      P.Run_wake.create
        ~run_id:admitted.id
        ~source:admitted.source
        ~occurrence:
          (Delivered_timer
             { schedule_id = schedule.id
             ; delivery_count = 1
             ; creator = ownership.creator
             ; subscription = ownership.subscription
             })
      |> protocol_ok
    in
    let waiting, before =
      with_work state index admitted schedule work ~lifecycle:(Waiting wake)
    in
    let successful = project waiting success_delta |> fun state -> run state before.id in
    check_proof before successful work Succeeded;
    assert (P.Run.Lifecycle.equal successful.lifecycle (Waiting wake));
    let failure : P.Error.t =
      P.Error.create Internal_error ~message:"producer failed" ~retryable:false ()
    in
    List.iter
      [ { schedule with status = Cancelled }, P.Run_work.Terminal.Cancelled
      ; { schedule with status = Delivered }, Limited
      ; { schedule with status = Failed failure }, Failed
      ]
      ~f:(fun (terminal, outcome) ->
        let retired =
          project waiting (Schedule_changed terminal) |> fun state -> run state before.id
        in
        check_proof before retired work outcome;
        assert (P.Run.Lifecycle.equal retired.lifecycle (Terminal Interrupted)));
    ());
  print_endline
    "delivery: same-revision proof + canonical terminal; Workflow/Wait stay live";
  print_endline
    "skip/cancel/failure: awaited producer retires Interrupted; repeat adds no evidence";
  [%expect
    {|
    delivery: same-revision proof + canonical terminal; Workflow/Wait stay live
    skip/cancel/failure: awaited producer retires Interrupted; repeat adds no evidence |}]
;;

let%expect_test
    "staged create-cancel and Finish share one revision; incomplete terminal is rejected"
  =
  with_admitted (fun state _index admitted executing schedule work ->
    let completed =
      P.Moderator_execution.complete
        executing
        ~checkpoint_sha256:executing.context.checkpoint_sha256
        ~requests:{ request_turn = false; request_compaction = false; end_session = None }
      |> protocol_ok
    in
    let work_delta =
      A.Session_delta.Batch
        [ Moderator_execution_changed executing
        ; Schedule_changed schedule
        ; Schedule_changed { schedule with status = Cancelled }
        ; Moderator_execution_changed completed
        ]
    in
    let prepared =
      A.Run_transition.prepare
        state
        ~scope:
          (A.Run_scope.create ~run:admitted ~execution_id:executing.context.id
           |> protocol_ok)
        ~executing
        ~work_delta
        ~owned_work:[ work ]
        ~action:(Some (Finish { terminal = Cancelled; relinquish = [] }))
        ~now:timestamp
      |> protocol_ok
    in
    let delta = A.Session_delta.Batch [ work_delta; A.Run_transition.delta prepared ] in
    let final = project state delta in
    let current = run final admitted.id in
    check_proof admitted current work Cancelled;
    assert (P.Run.Lifecycle.equal current.lifecycle (Terminal Cancelled));
    let wrong =
      P.Run_work.Terminal.create ~work ~outcome:Succeeded ~revision:current.revision
      |> protocol_ok
    in
    let unproved =
      P.Run.create
        ~id:current.id
        ~session:current.session
        ~principal_id:current.principal_id
        ~source:current.source
        ~mode:current.mode
        ~lifecycle:(Terminal (Completed None))
        ~revision:current.revision
        ~owned_work:current.owned_work
        ~relinquished_work:current.relinquished_work
        ~terminal_work:[ wrong ]
        ~created_at:current.created_at
        ~updated_at:current.updated_at
      |> protocol_ok
    in
    let index = Option.value_exn state.run_state in
    let incomplete = A.Run_state.advance index ~run:unproved ~intents:[] |> protocol_ok in
    assert (
      Result.is_error
        (Settlement.prepare
           state
           ~delta:(Batch [ work_delta; Run_state_changed incomplete ])
           ~now:timestamp));
    (* A whole-state restoration never supplies a new producer terminal proof. *)
    let next = A.Session_delta.apply state work_delta |> protocol_ok in
    let restored =
      Settlement.prepare state ~delta:(Created next) ~now:timestamp |> protocol_ok
    in
    assert (
      Sexp.equal
        (A.Session_delta.sexp_of_t restored)
        (A.Session_delta.sexp_of_t (Created next))));
  print_endline "actual sequential create/cancel + callback Finish: one Run revision";
  print_endline
    "central verification rejects missing new-work proof; restoration grants no proof";
  [%expect
    {|
    actual sequential create/cancel + callback Finish: one Run revision
    central verification rejects missing new-work proof; restoration grants no proof |}]
;;

let%expect_test
    "canonical retirement precedes Schedule evidence; immutable identity rejects"
  =
  with_admitted (fun state index admitted _executing schedule work ->
    let state, previous =
      with_work state index admitted schedule work ~lifecycle:Active
    in
    let index = Option.value_exn state.run_state in
    let retired =
      A.Run_retirement.interrupt_run
        index
        ~run_id:previous.id
        ~session_revision:(Int64.succ state.counters.revision)
        ~now:timestamp
      |> protocol_ok
    in
    let candidate =
      project
        state
        (Batch
           [ Schedule_changed { schedule with status = Cancelled }
           ; Run_state_changed retired
           ])
    in
    let current = run candidate previous.id in
    assert (A.Run_retirement.matches_interrupt ~previous current);
    check_proof previous current work Unconfirmed;
    let changed =
      { schedule with status = Cancelled; payload = `String "different producer" }
    in
    assert (
      Result.is_error
        (Settlement.prepare state ~delta:(Schedule_changed changed) ~now:timestamp)));
  print_endline
    "exact source/Stop retirement preserves Unconfirmed; producer replacement rejects";
  [%expect
    {| exact source/Stop retirement preserves Unconfirmed; producer replacement rejects |}]
;;
