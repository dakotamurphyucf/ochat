open! Core
open Agent_server_test_support
open Retry_support
open Descendant_turn_adapter_tests

let terminal_operations child =
  let module Log = A.Durable_event_log in
  let log = child.Registry.durable_events in
  match Log.oldest_sequence log, Log.latest_sequence log with
  | Some oldest, Some latest ->
    (match
       Log.replay log ~after_sequence:(Int64.pred oldest) ~through_sequence:latest
     with
     | Available events ->
       List.filter_map events ~f:(fun (event : P.Event.Durable.t) ->
         match P.Event.Durable.Payload.of_json ~kind:event.kind event.payload with
         | Ok
             ( Operation_completed operation
             | Operation_failed operation
             | Operation_cancelled operation
             | Operation_interrupted operation ) -> Some operation
         | Ok _ | Error _ -> None)
     | Snapshot_required -> [])
  | (None | Some _), (None | Some _) -> []
;;

let%expect_test "managed rollover uses the original native receipt and a fresh Goal root" =
  let barrier = Writer_barrier.create () in
  let managed_text = "continue this exact managed input once" in
  let queued = ref None in
  let dispatches = ref 0 in
  let entered, signal_entered = Eio.Promise.create () in
  let release, signal_release = Eio.Promise.create () in
  let at_successor = ref ignore in
  let remaining = "context_remaining" in
  let child_sources =
    List.map Birth.attempt_sources ~f:(fun (path, text) ->
      path, text ^ {|<tool type="inherited" name="context_remaining"/>|})
  in
  let module R = A.Recovery_rollover in
  let module Scope = P.Recovery_window.Scope in
  let module Capacity = Inference_runtime.Context_capacity in
  let before_child_preparation = ref None in
  let capacity_source request =
    let module Payload = History_entry.Payload in
    let carries_managed_input =
      List.exists (Inference.Request.history request) ~f:(fun entry ->
        match Payload.Semantic.view (Payload.semantic (History_entry.payload entry)) with
        | Message { role = User; content; _ } ->
          List.exists content ~f:(function
            | Payload.Content.Text { text; _ } -> String.equal text managed_text
            | _ -> false)
        | _ -> false)
    in
    if carries_managed_input
    then Option.iter !before_child_preparation ~f:Eio.Promise.await;
    Capacity.Source.create ~encoder:Openai_responses_v1 ~read:(fun () ->
      Covered
        (Inference.Observation.Count.create (Actual 100L)
         |> Result.map_error ~f:(fun _ -> "invalid fixture count")
         |> Result.ok_or_failwith))
  in
  Birth.with_native_goal_child
    ~timeout:180.
    ~capacity_source
    ~child_sources
    ~wrap_env:(Writer_barrier.wrap ~boundary:After_sync barrier)
    ~parent_extra_tools:[ "agent_send"; "agent_wait" ]
    ~inherited_tools:[ remaining, "<tool name=\"context_remaining\"/>" ]
    ~post_creation_stream:(fun inputs ->
      let count text =
        List.count inputs ~f:(fun item ->
          Jsonaf.to_string (Openai.Responses.Item.jsonaf_of_t item)
          |> String.is_substring ~substring:text)
      in
      let is_child =
        List.exists inputs ~f:(fun item ->
          let json = Openai.Responses.Item.jsonaf_of_t item in
          match Document_schema.Json.field json ~name:"role" with
          | Value (`String "user") ->
            String.is_substring (Jsonaf.to_string json) ~substring:managed_text
          | Value _ | Absent | Null -> false)
      in
      if is_child
      then (
        Int.incr dispatches;
        match !dispatches with
        | 1 ->
          fun () ->
            Eio.Promise.resolve signal_entered ();
            Eio.Promise.await release;
            function_response remaining (`Object []) ()
        | 2 ->
          [%test_eq: int] 1 (count managed_text);
          [%test_eq: int] 0 (count remaining);
          !at_successor ();
          completed_response "735 successor completed"
        | _ -> failwith "replayed managed receipt launched extra child inference")
      else (
        match !queued with
        | None -> completed_response "735 parent completed native management"
        | Some (tool, arguments) ->
          queued := None;
          function_response tool arguments))
    (fun env _sw daemon parent child scope reference _ ->
       Descendant_ancestry_support.start_legacy daemon child;
       let initial = state child in
       let target = A.Configuration_transition.target initial |> protocol_ok in
       let policy =
         Capacity.Policy.create
           ~target
           ~encoder:Openai_responses_v1
           ~model_limit:(Bounded { total = 500L; input = Some 450L; output = None })
           ~allowance:
             { host_total = None
             ; session_total = None
             ; omitted_output_default = Some 50L
             ; checkpoint_reserve = 20L
             ; warning_lead = 10L
             }
           ~estimate:None
         |> Result.map_error ~f:(fun _ -> "invalid fixture target policy")
         |> Result.ok_or_failwith
       in
       let owner =
         P.Session_ref.create
           ~server_id:(Agent_store.Session_store.server_id (Daemon.store daemon))
           ~session_id:initial.identity.session_id
       in
       let host =
         A.Session_context_capacity.Host.create
           ~mode_for_session:(fun selected ->
             Option.some_if
               (P.Session_ref.equal owner selected)
               A.Session_context_capacity.Mode.Manual)
           ~targets:[ policy, `Manual ]
         |> Result.map_error ~f:(fun _ -> "invalid fixture policy")
         |> Result.ok_or_failwith
       in
       Actor.set_context_capacity_host child.actor host |> protocol_ok;
       let writer, _, _, _ =
         Actor.attach_with_snapshot
           child.actor
           ~principal_id:(Some (Operator_authorization.principal original).id)
           ~reclaim_token:None
           ~mode:Owner_read_write
           ~subscribe:false
         |> protocol_ok
       in
       let key = P.Idempotency_key.of_string "735-managed-message" |> protocol_ok in
       let queue_native ~command_key ~tool arguments =
         queued := Some (tool, arguments);
         let before = (state parent).invocations in
         Goal.ordinary
           daemon
           parent
           (request parent command_key "Run the managed child command.")
         |> admitted
         |> ignore;
         before
       in
       let queue_send ~command_key ~message =
         queue_native
           ~command_key
           ~tool:"agent_send"
           (`Object
               [ "session_id", P.Id.Session.to_json reference.child_session_id
               ; "idempotency_key", P.Idempotency_key.to_json key
               ; "message", `String message
               ])
       in
       let native_result ?(tool = "agent_send") before =
         within ~seconds:30. env "genuine parent managed tool" (fun () ->
           Goal_native_test_support.await_quiet parent)
         |> ignore;
         let invocation =
           List.find_exn (state parent).invocations ~f:(fun invocation ->
             String.equal invocation.P.Invocation.context.tool_name tool
             && not
                  (List.exists before ~f:(fun (previous : P.Invocation.t) ->
                     P.Id.Invocation.equal previous.context.id invocation.context.id)))
         in
         invocation.status
       in
       let send_before_preparing_child command_key =
         (* Let this actual controlling Turn retire before the child allocator
            captures its parent's exact current State. Delay only the fixture
            adapter's preparation, with all actor and provider guards intact. *)
         let parent_finished, finish_parent = Eio.Promise.create () in
         before_child_preparation := Some parent_finished;
         protect_cleanup
           ~finally:(fun () ->
             before_child_preparation := None;
             Eio.Promise.resolve finish_parent ())
           (fun () -> queue_send ~command_key ~message:managed_text |> native_result)
       in
       protect_cleanup
         ~finally:(fun () ->
           if not (Eio.Promise.is_resolved release)
           then Eio.Promise.resolve signal_release ();
           Writer_barrier.release barrier;
           Actor.detach child.actor writer.id |> protocol_ok)
         (fun () ->
            let original_window =
              Actor.enable_recovery_windows
                child.actor
                ~actor:original
                ~attachment_id:writer.id
                ~authorize
              |> protocol_ok
            in
            let reported = send_before_preparing_child "735-parent-send" in
            (match
               Result.try_with (fun () ->
                 within env "predecessor provider" (fun () -> Eio.Promise.await entered))
             with
             | Ok () -> ()
             | Error exn ->
               let usage = marks child in
               raise_s
                 [%sexp
                   (exn : exn)
                 , (terminal_operations child : P.Operation.t list)
                 , (usage.turn : int64)
                 , (usage.attempt : int64)]);
            let original_receipt = managed child reference key in
            let predecessor = Option.value_exn (state child).active_operation in
            let predecessor_marks = marks child in
            let turn_totals state =
              L.summary state.A.Session_state.inference_ledger
              |> P.Inference_query.Summary.turns
            in
            let predecessor_turns = turn_totals (state child) in
            (match reported with
             | Published (Complete reported) ->
               assert (Jsonaf.exactly_equal reported (M.to_json original_receipt))
             | status -> raise_s [%sexp (status : P.Invocation.status)]);
            let expected =
              R.Expected.create original_window ~revision:(state child).counters.revision
              |> protocol_ok
            in
            let intent =
              Actor.request_context_rollover
                child.actor
                ~actor:original
                ~attachment_id:writer.id
                ~authorize
                ~authorize_force:authorize
                ~key:(P.Idempotency_key.of_string "735-force-cut" |> protocol_ok)
                expected
              |> protocol_ok
            in
            Eio.Promise.resolve signal_release ();
            within env "predecessor committed cut" (fun () ->
              Goal_native_test_support.await_quiet child)
            |> ignore;
            let suspended = managed child reference key in
            (match suspended.status with
             | Suspended_rollover { continuation; _ } ->
               assert (P.Id.Operation.equal continuation intent.intent.id)
             | status ->
               let current = state child in
               let receipt =
                 Option.bind current.recovery_rollover ~f:(R.find ~id:intent.intent.id)
               in
               raise_s
                 [%sexp
                   (status : M.status)
                 , (Option.map receipt ~f:(fun receipt -> receipt.R.Receipt.status)
                    : R.Receipt.status option)
                 , (terminal_operations child : P.Operation.t list)
                 , (List.map current.invocations ~f:(fun invocation ->
                      invocation.P.Invocation.context.tool_name, invocation.status)
                    : (string * P.Invocation.status) list)
                 , (!dispatches : int)]);
            let retained = state child in
            let cut_turns = turn_totals retained in
            let cut_marks = marks child in
            [%test_eq: int64] predecessor_marks.turn cut_marks.turn;
            [%test_eq: int64] predecessor_marks.attempt cut_marks.attempt;
            [%test_eq: int64] (Int64.pred predecessor_turns.pending) cut_turns.pending;
            [%test_eq: int64]
              (Int64.succ predecessor_turns.interrupted)
              cut_turns.interrupted;
            assert (
              List.count (terminal_operations child) ~f:(fun operation ->
                P.Id.Operation.equal predecessor.id operation.id
                &&
                match operation.state with
                | Interrupted _ -> true
                | Starting | Running | Cancelling | Completed | Failed _ | Cancelled ->
                  false)
              = 1);
            (* A managed child Turn holds inherited dispatch authority. The
               parent's completed tool borrow grants no child transcript access. *)
            let invocation =
              List.find_exn retained.invocations ~f:(fun invocation ->
                String.equal invocation.P.Invocation.context.tool_name remaining)
            in
            (match invocation.status with
             | Published (Fail { code = "permission_denied"; _ }) -> ()
             | status -> raise_s [%sexp (status : P.Invocation.status)]);
            let current =
              A.Recovery_window_ledger.active (Option.value_exn retained.recovery_windows)
              |> A.Recovery_window_ledger.Window.scope
            in
            assert (not (Scope.equal current original_window));
            assert (P.History.Id.equal original_receipt.history_id suspended.history_id);
            [%test_eq: int] 1 !dispatches;
            barrier.child <- Some reference.child_session_id;
            ignore (queue_send ~command_key:"735-parent-lost-ack" ~message:managed_text);
            within env "physical child acknowledgement" (fun () ->
              Eio.Promise.await barrier.reached);
            let cancelled_child = Option.value_exn barrier.operation in
            let parent_pending = branch parent scope reference |> GD.Branch.pending in
            assert (
              Option.exists parent_pending ~f:(fun pending ->
                let occurrence = A.Goal_descendant_pending.occurrence pending in
                match A.Goal_descendant_occurrence.kind occurrence with
                | Host_turn { operation_id } ->
                  P.Id.Operation.equal cancelled_child operation_id
                | Attempt _ -> false));
            let parent_writer, _ =
              Actor.attach parent.actor ~mode:Read_write ~subscribe:false |> protocol_ok
            in
            let cancelled_parent = Option.value_exn (state parent).active_operation in
            protect_cleanup
              ~finally:(fun () ->
                Actor.detach parent.actor parent_writer.id |> protocol_ok)
              (fun () ->
                 Actor.cancel_operation
                   parent.actor
                   ~attachment_id:parent_writer.id
                   ~operation_id:cancelled_parent.id
                 |> protocol_ok
                 |> ignore);
            Writer_barrier.release barrier;
            let rec await_reconciled () =
              let current = branch parent scope reference in
              if
                Option.is_none (state parent).active_operation
                && Option.is_none (state child).active_operation
                && Option.is_none (GD.Branch.pending current)
              then current
              else (
                Eio.Fiber.yield ();
                await_reconciled ())
            in
            let settled_root =
              within env "lost acknowledgement reconciliation" await_reconciled
            in
            let reconciled = managed child reference key in
            assert (M.equal suspended reconciled);
            let included = Birth.Provider_probe.included settled_root in
            let occurrence =
              A.Goal_descendant_included.pending included
              |> A.Goal_descendant_pending.occurrence
            in
            (match A.Goal_descendant_occurrence.kind occurrence with
             | Host_turn { operation_id } ->
               assert (P.Id.Operation.equal operation_id cancelled_child)
             | Attempt _ -> failwith "lost child acknowledgement lacks a factual Turn");
            let reported =
              queue_native
                ~command_key:"735-parent-suspended"
                ~tool:"agent_wait"
                (`Object
                    [ "session_id", P.Id.Session.to_json reference.child_session_id
                    ; "receipt_id", P.History.Id.to_json suspended.history_id
                    ; "timeout_ms", `Number "0"
                    ])
              |> native_result ~tool:"agent_wait"
            in
            (match reported with
             | Published (Complete result) ->
               assert (
                 Jsonaf.exactly_equal
                   (Jsonaf.member_exn "receipt" result)
                   (M.to_json suspended))
             | status -> raise_s [%sexp (status : P.Invocation.status)]);
            [%test_eq: int] 1 !dispatches;
            let prior = marks child in
            let accounting_at_dispatch, signal_accounting = Eio.Promise.create () in
            (at_successor
             := fun () ->
                  let child_now = state child in
                  let fresh = Option.value_exn child_now.active_operation in
                  let root = branch parent scope reference in
                  assert (Option.is_none (GD.Branch.pending root));
                  assert (not (P.Id.Operation.equal fresh.id cancelled_child));
                  let included = Birth.Provider_probe.included root in
                  let occurrence =
                    A.Goal_descendant_included.pending included
                    |> A.Goal_descendant_pending.occurrence
                  in
                  let ordinal =
                    match A.Goal_descendant_occurrence.kind occurrence with
                    | Attempt
                        { ordinal; operation_id = Some operation; invocation_id = None }
                      when P.Id.Operation.equal operation fresh.id -> ordinal
                    | Attempt _ | Host_turn _ ->
                      failwith
                        "provider crossed without its freshly confirmed Goal attempt"
                  in
                  let row =
                    L.find child_now.inference_ledger ~ordinal |> Option.value_exn
                  in
                  assert (
                    Option.equal
                      P.Id.Operation.equal
                      (L.Handle.operation_id (L.Row.handle row))
                      (Some fresh.id));
                  let actual = marks child in
                  [%test_eq: int64] (Int64.succ prior.turn) actual.turn;
                  [%test_eq: int64] (Int64.succ prior.attempt) actual.attempt;
                  let imported =
                    A.Child_contributions.snapshot
                      (Option.value_exn (state parent).child_contributions)
                      ~reference
                    |> protocol_ok
                    |> Option.value_exn
                  in
                  let parent_scope =
                    S.goal_scopes imported
                    |> Option.value_exn
                    |> List.find_exn ~f:(fun value ->
                      Origin.Scope.equal scope (S.Goal_scope.scope value))
                  in
                  assert (
                    S.Partition.equal (S.own imported) (S.Goal_scope.own parent_scope));
                  let parent_marks = S.Partition.marks (S.own imported) in
                  [%test_eq: int64] actual.turn parent_marks.turn;
                  [%test_eq: int64] actual.attempt parent_marks.attempt;
                  assert (
                    M.equal_status
                      (Assigned fresh.id)
                      (managed child reference key).status);
                  Eio.Promise.resolve signal_accounting fresh.id);
            (match send_before_preparing_child "735-parent-successor" with
             | Published (Complete _) -> ()
             | status -> raise_s [%sexp (status : P.Invocation.status)]);
            let fresh =
              within env "fresh inherited provider" (fun () ->
                Eio.Promise.await accounting_at_dispatch)
            in
            within env "successor terminal" (fun () ->
              Goal_native_test_support.await_quiet child)
            |> ignore;
            assert (
              M.equal_status
                (Terminal (Some fresh, Completed))
                (managed child reference key).status);
            let repeated =
              queue_send ~command_key:"735-parent-replay" ~message:managed_text
              |> native_result
            in
            assert (
              match repeated with
              | Published (Complete value) ->
                Jsonaf.exactly_equal value (M.to_json (managed child reference key))
              | _ -> false);
            [%test_eq: int] 2 !dispatches;
            [%test_eq: int] 1 (List.length (state child).managed_submissions);
            [%test_eq: int]
              1
              (List.count (state child).conversation.canonical_history ~f:(fun entry ->
                 P.History.Id.equal entry.id original_receipt.history_id))));
  print_endline
    "lost acknowledgement; original receipt; one fresh inherited dispatch; both ledgers";
  [%expect
    {| lost acknowledgement; original receipt; one fresh inherited dispatch; both ledgers |}]
;;
