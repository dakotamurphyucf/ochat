open! Core
open Agent_server_test_support
open Retry_support
module Birth = Descendant_goal_birth_tests
module Direct = Descendant_turn_adapter_tests
module Goal = Goal_service_tests
module Resume = Goal_resume_runtime_tests
module GD = A.Goal_descendants
module L = A.Inference_ledger

module Mode = struct
  type t =
    | Wake
    | Quiet
  [@@deriving sexp_of]
end

let child_source mode =
  let wake =
    match mode with
    | Mode.Wake -> "`Request_turn"
    | Quiet -> "`No_wake"
  in
  {|<authoring_context policy="manual"/><config model="fixture-model"/>
<developer>569 actual inherited idle notification child.</developer>
<script id="publisher" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = match event with
| `Session_start ->
  if state == 0 then
    let reference = { key = "idle-start"; invocation_id = `None; work = `None } in
    let* first = Notification.publish(reference, `Succeeded(`String("idle one")), |}
  ^ wake
  ^ {|) in
    let* second = Notification.publish(reference, `Succeeded(`String("idle two")), |}
  ^ wake
  ^ {|) in
    Task.pure(1)
  else Task.pure(state)
| _ -> Task.pure(state)
</script>|}
;;

let within env phase f =
  try Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. f with
  | Eio.Time.Timeout -> failwith ("inherited notification timed out: " ^ phase)
;;

let checkpoint child =
  Actor.with_moderator_checkpoint child.Registry.actor (fun () -> Actor.state child.actor)
  |> protocol_ok
;;

let deliveries (current : A.Session_state.t) =
  [%test_eq: int] 2 (List.length current.deliveries);
  List.sort current.deliveries ~compare:(fun left right ->
    P.Id.Delivery.compare left.P.Delivery.context.id right.context.id)
;;

let histories (current : A.Session_state.t) =
  List.filter current.conversation.canonical_history ~f:(fun entry ->
    match entry.P.History.provenance with
    | Runtime_notification _ -> true
    | Canonical | Moderator_inserted | Moderator_replaced _ | Runtime_authoring _ -> false)
;;

let assert_deliveries mode (current : A.Session_state.t) ~operation_id =
  let observer =
    A.Runtime_builder.moderator_snapshot_observer current.moderator
    |> protocol_ok
    |> Option.value_exn
  in
  [%test_eq: string] "publisher" observer.script_id;
  let values = deliveries current in
  [%test_eq: int] 2 (List.length (histories current));
  let bodies =
    List.map values ~f:(fun value ->
      let context = value.P.Delivery.context in
      assert (P.Id.Session.equal context.session_id current.identity.session_id);
      [%test_eq: int] current.identity.generation context.generation;
      assert (P.Delivery.equal_source Moderator context.source);
      [%test_eq: string] "idle-start" context.correlation;
      assert (Option.is_none context.invocation_id);
      assert (Option.is_none context.work);
      let ownership = Option.value_exn context.ownership in
      assert (P.Invocation.equal_observer ownership.source observer);
      assert (Option.is_none ownership.subscription_binding);
      assert (Option.exists value.disclosure_pins ~f:List.is_empty);
      let creator =
        match ownership.creator with
        | Moderator_event id ->
          List.find_exn current.moderator_executions ~f:(fun event ->
            P.Id.Moderator_execution.equal event.P.Moderator_execution.context.id id)
        | Invocation _ -> failwith "Session_start publication lost its real event owner"
      in
      assert (P.Moderator_execution.equal_phase Session_start creator.context.phase);
      assert (P.Invocation.equal_observer observer creator.context.source);
      (match creator.status with
       | Completed _ -> ()
       | Running | Failed _ | Interrupted _ ->
         failwith "idle delivery preceded actual publisher callback ACK");
      let history_id =
        match value.status with
        | Committed { history_id; _ } -> history_id
        | Pending | Failed _ -> failwith "notification history was not actually committed"
      in
      let history =
        List.find_exn (histories current) ~f:(fun entry ->
          P.History.Id.equal entry.id history_id)
      in
      (match history.provenance with
       | Runtime_notification id -> assert (P.Id.Delivery.equal id context.id)
       | Canonical | Moderator_inserted | Moderator_replaced _ | Runtime_authoring _ ->
         failwith "delivery history has no exact runtime provenance");
      assert (P.History.equal_role User history.role);
      let body =
        match context.completion with
        | Succeeded (`String body) -> body
        | Succeeded _ | Failed _ | Cancelled _ | Expired ->
          failwith "notification did not retain the actual fixture result"
      in
      assert (String.is_substring (Jsonaf.to_string history.payload) ~substring:body);
      (match mode, value.wake_disposition, operation_id with
       | Mode.Wake, Some (Accepted_wake actual), Some expected ->
         assert (P.Id.Operation.equal actual expected)
       | Quiet, None, None -> ()
       | (Wake | Quiet), (None | Some _), (None | Some _) ->
         failwith "two notification receipts did not share the actual wake decision");
      body)
    |> List.sort ~compare:String.compare
  in
  [%test_eq: string list] [ "idle one"; "idle two" ] bodies
;;

let assert_zero parent child scope reference ~parent_attempts ~parent_turns =
  let branch = Direct.branch parent scope reference in
  [%test_eq: int64] 0L (GD.Branch.confirmed_prefix branch);
  [%test_eq: int64] 0L (GD.Branch.last_nonce branch);
  assert (Option.is_none (GD.Branch.pending branch));
  assert (Option.is_none (GD.Branch.last branch));
  let current = state child in
  assert (List.is_empty (GD.local_included (Option.value_exn current.goal_descendants)));
  let marks = Direct.marks child in
  [%test_eq: int64] 0L marks.turn;
  [%test_eq: int64] 0L marks.attempt;
  [%test_eq: int64] 0L marks.untracked_turns;
  [%test_eq: int64] 0L marks.untracked_attempts;
  let account = Goal.accounting (state parent) in
  [%test_eq: int64] parent_attempts (A.Goal_accounting.admitted_attempts account);
  [%test_eq: int64] parent_turns (A.Goal_accounting.admitted_turns account)
;;

module Probe = struct
  type t =
    { mode : Mode.t
    ; child : Registry.entry
    ; accounting : Birth.Provider_probe.t
    ; mutable calls : int
    ; mutable operation_id : P.Id.Operation.t option
    ; mutable failure : (exn * Stdlib.Printexc.raw_backtrace) option
    }

  let create mode ~parent ~child ~scope ~reference ~parent_attempts_before =
    { mode
    ; child
    ; accounting =
        Birth.Provider_probe.create
          ~parent
          ~child
          ~scope
          ~reference
          ~parent_attempts_before
    ; calls = 0
    ; operation_id = None
    ; failure = None
    }
  ;;

  let raise_failure t =
    Option.iter t.failure ~f:(fun (exn, backtrace) ->
      Exn.raise_with_original_backtrace exn backtrace)
  ;;

  let before_provider t inputs =
    t.calls <- t.calls + 1;
    try
      (match t.mode with
       | Quiet -> failwith "quiet notification admitted a provider"
       | Wake -> ());
      [%test_eq: int] 1 t.calls;
      Birth.Provider_probe.before_submit t.accounting;
      let current = state t.child in
      let operation = Option.value_exn current.active_operation in
      let turn =
        L.find_turn_handle
          current.inference_ledger
          ~operation_id:operation.id
          ~generation:operation.generation
        |> Option.value_exn
      in
      assert (P.Id.Operation.equal (L.Turn_handle.operation_id turn) operation.id);
      [%test_eq: int] operation.generation (L.Turn_handle.generation turn);
      assert_deliveries t.mode current ~operation_id:(Some operation.id);
      List.iter [ "idle one"; "idle two" ] ~f:(fun body ->
        [%test_eq: int]
          1
          (List.count inputs ~f:(function
             | Openai.Responses.Item.Input_message message ->
               String.is_substring
                 (Openai.Responses.Item.jsonaf_of_t (Input_message message)
                  |> Jsonaf.to_string)
                 ~substring:body
             | _ -> false)));
      t.operation_id <- Some operation.id
    with
    | exn ->
      let backtrace = Stdlib.Printexc.get_raw_backtrace () in
      t.failure <- Some (exn, backtrace);
      Exn.raise_with_original_backtrace exn backtrace
  ;;
end

let await_delivered env daemon child probe =
  let rec await () =
    Probe.raise_failure probe;
    Factory.drain_idle_moderator (Daemon.factory daemon) child |> protocol_ok |> ignore;
    let current = checkpoint child in
    let committed =
      Int.equal (List.length current.deliveries) 2
      && List.for_all current.deliveries ~f:(fun value ->
        match value.P.Delivery.status with
        | Committed _ -> true
        | Pending | Failed _ -> false)
    in
    let completed =
      match probe.Probe.mode with
      | Quiet -> Int.equal probe.calls 0
      | Wake ->
        let turns =
          L.summary current.inference_ledger |> P.Inference_query.Summary.turns
        in
        Int.equal probe.calls 1 && Int64.equal turns.completed 1L
    in
    if
      committed
      && completed
      && Option.is_none current.active_operation
      && Resume.callbacks_complete current
      && Owner.source_lane_idle child.runtime
    then current
    else (
      Eio.Fiber.yield ();
      await ())
  in
  within env "actual delivery and worker terminal" await
;;

let run mode =
  let provider = ref None in
  let budget =
    P.Goal.Budget.create
      ~admitted_turns:None
      ~admitted_attempts:(Some 3L)
      ~observed_tokens:None
      ~deadline:None
    |> protocol_ok
  in
  Birth.with_native_goal_child
    ~budget
    ~child_sources:[ "child.chatmd", child_source mode ]
    ~post_creation_stream:(fun inputs ->
      Option.iter !provider ~f:(fun probe -> Probe.before_provider probe inputs);
      completed_response "569 actual idle notification worker completed")
    (fun env _sw daemon parent child scope reference counters ->
       let before = Goal.accounting (state parent) in
       let parent_attempts = A.Goal_accounting.admitted_attempts before in
       let parent_turns = A.Goal_accounting.admitted_turns before in
       [%test_eq: int64] 2L parent_attempts;
       [%test_eq: int64] 1L parent_turns;
       let probe =
         Probe.create
           mode
           ~parent
           ~child
           ~scope
           ~reference
           ~parent_attempts_before:parent_attempts
       in
       assert_zero parent child scope reference ~parent_attempts ~parent_turns;
       provider := Some probe;
       (try within env "actual startup" (fun () -> Direct.start_child daemon child) with
        | exn ->
          let backtrace = Stdlib.Printexc.get_raw_backtrace () in
          Probe.raise_failure probe;
          Exn.raise_with_original_backtrace exn backtrace);
       let delivered = await_delivered env daemon child probe in
       assert_deliveries mode delivered ~operation_id:probe.operation_id;
       let same_deliveries = deliveries delivered in
       let same_history = histories delivered in
       (match mode with
        | Wake -> Birth.Provider_probe.after_terminal probe.accounting
        | Quiet -> assert_zero parent child scope reference ~parent_attempts ~parent_turns);
       (* Receipt identities and canonical counts remain exact across subsequent
          real polls. Metadata observation is not an all-producer State barrier. *)
       List.iter [ (); () ] ~f:(fun () ->
         Factory.drain_idle_moderator (Daemon.factory daemon) child
         |> protocol_ok
         |> ignore;
         Probe.raise_failure probe;
         let current = checkpoint child in
         assert (List.equal P.Delivery.equal same_deliveries (deliveries current));
         assert (List.equal P.History.equal_entry same_history (histories current));
         assert_deliveries mode current ~operation_id:probe.operation_id;
         match mode with
         | Wake -> Birth.Provider_probe.after_terminal probe.accounting
         | Quiet ->
           assert_zero parent child scope reference ~parent_attempts ~parent_turns);
       [%test_eq: int]
         (match mode with
          | Wake -> 1
          | Quiet -> 0)
         probe.calls;
       [%test_eq: int] (2 + probe.calls) !(counters.providers);
       print_s
         [%sexp
           (mode : Mode.t), "two real histories retain one wake or no canonical admission"])
;;

let%expect_test
    "inherited idle notifications confirm once and quiet delivery reserves nothing"
  =
  List.iter [ Mode.Wake; Quiet ] ~f:run;
  [%expect
    {|
    (Wake "two real histories retain one wake or no canonical admission")
    (Quiet "two real histories retain one wake or no canonical admission")
    |}]
;;

let%expect_test
    "acknowledged idle wake never transfers its suffix to a replacement worker"
  =
  let module Barrier = Descendant_history_adapter_tests.Root_barrier in
  let barrier = Barrier.create () in
  let observe_provider = ref false in
  let child_providers = ref 0 in
  Birth.with_native_goal_child
    ~wrap_env:(Barrier.wrap barrier)
    ~child_sources:[ "child.chatmd", child_source Mode.Wake ]
    ~post_creation_stream:(fun _ ->
      if !observe_provider then Int.incr child_providers;
      completed_response "569 replacement must not receive an old idle admission")
    (fun env sw daemon parent child scope reference counters ->
       let parent_before = state parent in
       let parent_attempts =
         A.Goal_accounting.admitted_attempts (Goal.accounting parent_before)
       in
       let parent_turns =
         A.Goal_accounting.admitted_turns (Goal.accounting parent_before)
       in
       assert_zero parent child scope reference ~parent_attempts ~parent_turns;
       barrier.target
       <- Some (parent_before.identity.session_id, scope, Barrier.Kind.Import reference);
       let started = ref None in
       protect_cleanup
         ~finally:(fun () ->
           Barrier.release barrier;
           Option.iter !started ~f:(fun pending ->
             within env "join actual child start" (fun () ->
               Eio.Promise.await_exn pending)))
         (fun () ->
            observe_provider := true;
            let pending =
              Eio.Fiber.fork_promise ~sw (fun () -> Direct.start_child daemon child)
            in
            started := Some pending;
            within env "real root Host_turn Import before sync" (fun () ->
              Eio.Fiber.first
                (fun () -> Eio.Promise.await barrier.entered)
                (fun () ->
                   (* Start may finish before a scheduler-owned delivery reaches
                      Import. Only an actual early exception is a failure. *)
                   Eio.Promise.await_exn pending;
                   Eio.Promise.await barrier.entered));
            (* The actual parent writer is suspended. Inspect only the free
               child Actor, never the blocked parent mailbox. *)
            let accepted = state child in
            let operation = Option.value_exn accepted.active_operation in
            [%test_eq: P.Operation.kind] (Turn Idle_followup) operation.kind;
            (match operation.state with
             | Starting -> ()
             | Running | Cancelling | Completed | Failed _ | Cancelled | Interrupted _ ->
               failwith "idle worker started before the real root Import ACK");
            assert_deliveries Mode.Wake accepted ~operation_id:(Some operation.id);
            let before_deliveries = deliveries accepted in
            let before_histories = histories accepted in
            let included =
              match GD.local_included (Option.value_exn accepted.goal_descendants) with
              | [ included ] -> included
              | [] | _ :: _ -> failwith "idle child did not retain its one real Turn ACK"
            in
            let occurrence =
              A.Goal_descendant_included.pending included
              |> A.Goal_descendant_pending.occurrence
            in
            assert (
              A.Goal_descendant_occurrence.equal
                occurrence
                (Option.value_exn barrier.occurrence));
            (match A.Goal_descendant_occurrence.kind occurrence with
             | Host_turn { operation_id } ->
               assert (P.Id.Operation.equal operation_id operation.id)
             | Attempt _ -> failwith "idle replacement barrier did not hold a host Turn");
            assert (
              Option.is_some
                (L.find_turn_handle
                   accepted.inference_ledger
                   ~operation_id:operation.id
                   ~generation:operation.generation));
            let marks = Direct.marks child in
            [%test_eq: int64] 1L marks.turn;
            [%test_eq: int64] 0L marks.attempt;
            [%test_eq: int] 0 !child_providers;
            let replacement_calls = ref 0 in
            let replacement =
              A.Operation_worker.create ~run:(fun ~sw:_ ~input _ ->
                Int.incr replacement_calls;
                A.Operation_worker.Completed
                  { final_history = input.history
                  ; runtime_requests = []
                  ; moderator_snapshot = None
                  })
            in
            Actor.set_operation_worker child.actor (Some replacement) |> protocol_ok;
            Barrier.release barrier;
            within env "actual start and source activation join" (fun () ->
              Eio.Promise.await_exn pending);
            let retired =
              within env "actual idle ACK terminal cleanup" (fun () ->
                Goal_native_test_support.await_quiet child)
            in
            let turns =
              L.summary retired.inference_ledger |> P.Inference_query.Summary.turns
            in
            [%test_eq: int64] 0L turns.pending;
            [%test_eq: int64] 0L turns.completed;
            [%test_eq: int64] 1L turns.failed;
            [%test_eq: int64] 0L turns.cancelled;
            [%test_eq: int64] 0L turns.interrupted;
            (* Join the retained factual owner after its actual failure, rather
               than supplying the negative that causes the source-check failure. *)
            within env "join retained actual idle ACK facts" (fun () ->
              Actor.drain_descendant_cleanup child.actor |> protocol_ok);
            let assert_retained () =
              let current = checkpoint child in
              assert (Option.is_none current.active_operation);
              assert (List.equal P.Delivery.equal before_deliveries (deliveries current));
              assert (
                List.equal P.History.equal_entry before_histories (histories current));
              assert_deliveries Mode.Wake current ~operation_id:(Some operation.id);
              let actual =
                Direct.confirmed parent child scope reference ~turns:1L ~attempts:0L
              in
              assert (A.Goal_descendant_included.equal actual included);
              let root = Direct.branch parent scope reference in
              [%test_eq: int64] 1L (GD.Branch.confirmed_prefix root);
              [%test_eq: int64] 1L (GD.Branch.last_nonce root);
              assert (Option.is_none (GD.Branch.pending root));
              let account = Goal.accounting (state parent) in
              [%test_eq: int64]
                parent_attempts
                (A.Goal_accounting.admitted_attempts account);
              [%test_eq: int64] parent_turns (A.Goal_accounting.admitted_turns account);
              [%test_eq: int] 0 !child_providers;
              [%test_eq: int] 0 !replacement_calls;
              [%test_eq: int] 2 !(counters.providers)
            in
            assert_retained ();
            List.iter [ (); () ] ~f:(fun () ->
              Factory.drain_idle_moderator (Daemon.factory daemon) child
              |> protocol_ok
              |> ignore;
              assert_retained ())));
  print_endline
    "real idle Turn ACK and accepted histories survive replacement; neither worker \
     suffix launches";
  [%expect
    {| real idle Turn ACK and accepted histories survive replacement; neither worker suffix launches |}]
;;
