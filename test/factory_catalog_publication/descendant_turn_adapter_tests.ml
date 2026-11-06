open! Core
open Agent_server_test_support
open Retry_support
module Birth = Descendant_goal_birth_tests
module Goal = Goal_service_tests
module GD = A.Goal_descendants
module Origin = Agent_store.Delegation_goal_origin
module D = Agent_store.Delegation_store
module S = A.Child_usage_snapshot
module M = A.Managed_submission
module L = A.Inference_ledger

let within ?(seconds = 10.) env phase f =
  try Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) seconds f with
  | Eio.Time.Timeout -> failwith ("567 adapter timed out: " ^ phase)
;;

let start_child daemon child =
  Factory.prepare_session_start (Daemon.factory daemon) child |> protocol_ok;
  Owner.ensure_loaded_authenticated child.Registry.runtime ~actor:original |> protocol_ok;
  let writer, _ =
    Actor.attach child.actor ~mode:Read_write ~subscribe:false |> protocol_ok
  in
  protect_cleanup
    ~finally:(fun () -> Actor.detach child.actor writer.id |> protocol_ok)
    (fun () ->
       Actor.start child.actor ~actor:original ~attachment_id:writer.id
       |> protocol_ok
       |> ignore;
       let activation =
         Owner.with_loaded_runtime child.runtime (fun runtime ->
           Result.of_option
             runtime.A.Runtime_builder.moderator_activation
             ~error:(P.Error.invalid_request "real child startup activation absent"))
         |> protocol_ok
       in
       (* Actor.start ACKs Idle before the asynchronous Session_start/resume
          claim. Join the real source activation after releasing the Owner lease;
          an empty receipt list cannot prove that callback has finished. *)
       let rec activation_finished () =
         if activation.pending ()
         then (
           Eio.Fiber.yield ();
           activation_finished ())
       in
       activation_finished ();
       let rec ready () =
         let current =
           Actor.with_moderator_checkpoint child.actor (fun () -> Actor.state child.actor)
           |> protocol_ok
         in
         if
           Option.is_none current.active_operation
           && (match current.lifecycle.observed with
               | Idle -> true
               | Stopped
               | Queued_for_slot
               | Starting
               | Recovering
               | Running_turn _
               | Compacting _
               | Waiting_for_permission _
               | Stopping
               | Failed _ -> false)
           && Option.is_some current.moderator
           && (not (List.is_empty current.moderator_executions))
           && List.for_all current.moderator_executions ~f:(fun receipt ->
             match receipt.P.Moderator_execution.status with
             | Completed _ -> true
             | Running | Failed _ | Interrupted _ -> false)
         then ()
         else (
           Eio.Fiber.yield ();
           ready ())
       in
       ready ())
;;

let request entry key text =
  let current = state entry in
  A.Run_admission_request.Headless.create
    ~session_id:current.identity.session_id
    ~generation:current.identity.generation
    ~expected_revision:current.counters.revision
    ~mode:Single_turn
    ~input:(User_submission { kind = Plain_text; text; attachments = [] })
    ~key:(P.Idempotency_key.of_string key |> protocol_ok)
  |> protocol_ok
;;

let admitted = function
  | A.Run_admission_outcome.Admitted receipt -> receipt
  | Rejected error | Uncertain error -> raise_s [%sexp (error : P.Error.t)]
;;

let branch parent scope reference =
  GD.branches (Option.value_exn (state parent).goal_descendants) ~goal_id:Goal.goal_id
  |> List.find_exn ~f:(fun branch ->
    Origin.Scope.equal (GD.Branch.scope branch) scope
    && D.Reference.equal (GD.Branch.reference branch) reference)
;;

let marks child =
  A.Child_accounting.snapshot (state child)
  |> protocol_ok
  |> Option.value_exn
  |> S.own
  |> S.Partition.marks
;;

let confirmed parent child scope reference ~turns ~attempts =
  let branch = branch parent scope reference in
  assert (Option.is_none (GD.Branch.pending branch));
  let included =
    match GD.Branch.last branch with
    | Some (Included included) -> included
    | None | Some (No_writer_closed _) -> failwith "worker preceded real Included receipt"
  in
  let child_now = state child in
  assert (
    List.equal
      A.Goal_descendant_included.equal
      [ included ]
      (GD.local_included (Option.value_exn child_now.goal_descendants)));
  let observed =
    A.Child_contributions.snapshot
      (Option.value_exn (state parent).child_contributions)
      ~reference
    |> protocol_ok
    |> Option.value_exn
  in
  let scoped =
    S.goal_scopes observed
    |> Option.value_exn
    |> List.find_exn ~f:(fun entry -> Origin.Scope.equal (S.Goal_scope.scope entry) scope)
  in
  assert (
    Option.exists
      (S.Goal_scope.included scoped)
      ~f:(A.Goal_descendant_included.equal included));
  assert (S.Partition.equal (S.own observed) (S.Goal_scope.own scoped));
  let actual = S.Partition.marks (S.own observed) in
  [%test_eq: int64] turns actual.turn;
  [%test_eq: int64] attempts actual.attempt;
  [%test_eq: int64] 0L actual.untracked_turns;
  [%test_eq: int64] 0L actual.untracked_attempts;
  included
;;

let function_response name arguments =
  let open Openai.Responses.Response_stream in
  let call arguments status : Item.t =
    Function_call
      { name
      ; arguments
      ; call_id = "567-native-send"
      ; _type = "function_call"
      ; id = Some "567-native-send-item"
      ; status
      }
  in
  let arguments = Jsonaf.to_string arguments in
  [ Output_item_added
      { item = call "" None; output_index = 0; type_ = "response.output_item.added" }
  ; Function_call_arguments_done
      { arguments
      ; item_id = "567-native-send-item"
      ; output_index = 0
      ; type_ = "response.function_call_arguments.done"
      }
  ; Output_item_done
      { item = call arguments (Some "completed")
      ; output_index = 0
      ; type_ = "response.output_item.done"
      }
  ]
  |> Stdlib.List.to_seq
;;

let managed child reference key =
  List.find_exn (state child).managed_submissions ~f:(fun receipt ->
    D.Reference.equal receipt.M.reference reference
    && P.Idempotency_key.equal receipt.key key)
;;

let%expect_test
    "native managed inherited Turn confirms once through replay and busy adoption"
  =
  let queued = ref None in
  (* This flow drives five real parent Turns plus native child creation. Keep
     individual phase deadlines while budgeting the complete durable workflow. *)
  Birth.with_native_goal_child
    ~timeout:180.
    ~parent_extra_tools:[ "agent_send" ]
    ~post_creation_stream:(fun _ ->
      match !queued with
      | None -> completed_response "567 parent completed native management"
      | Some arguments ->
        queued := None;
        function_response "agent_send" arguments)
    (fun env _sw daemon parent child scope reference _counters ->
       start_child daemon child;
       let entered, enter = Eio.Promise.create () in
       let adopt, allow_adopt = Eio.Promise.create () in
       let adopted, did_adopt = Eio.Promise.create () in
       let finish, allow_finish = Eio.Promise.create () in
       let worker_calls = ref 0 in
       let release resolver promise =
         if Option.is_none (Eio.Promise.peek promise) then Eio.Promise.resolve resolver ()
       in
       let worker =
         A.Operation_worker.create ~run:(fun ~sw:_ ~input capabilities ->
           Int.incr worker_calls;
           let included = confirmed parent child scope reference ~turns:1L ~attempts:0L in
           let occurrence =
             A.Goal_descendant_included.pending included
             |> A.Goal_descendant_pending.occurrence
           in
           (match A.Goal_descendant_occurrence.kind occurrence with
            | Host_turn { operation_id } ->
              assert (P.Id.Operation.equal operation_id input.operation.id)
            | Attempt _ -> failwith "managed worker got an Attempt receipt");
           assert (
             Option.is_some
               (L.find_turn_handle
                  (state child).inference_ledger
                  ~operation_id:input.operation.id
                  ~generation:input.operation.generation));
           Eio.Promise.resolve enter input.operation.id;
           Eio.Promise.await adopt;
           let deferred = capabilities.consume_deferred () |> protocol_ok in
           Eio.Promise.resolve did_adopt deferred;
           Eio.Promise.await finish;
           A.Operation_worker.Completed
             { final_history = input.history @ deferred
             ; runtime_requests = []
             ; moderator_snapshot = None
             })
       in
       Actor.set_operation_worker child.actor (Some worker) |> protocol_ok;
       let send command_key managed_key message =
         let before = (state parent).invocations in
         queued
         := Some
              (`Object
                  [ "session_id", P.Id.Session.to_json reference.child_session_id
                  ; "idempotency_key", `String managed_key
                  ; "message", `String message
                  ]);
         Goal.ordinary
           daemon
           parent
           (request parent command_key "Perform the queued native send.")
         |> admitted
         |> ignore;
         within ~seconds:30. env "native parent invocation" (fun () ->
           Goal_native_test_support.await_quiet parent)
         |> ignore;
         let invocation =
           List.find_exn (state parent).invocations ~f:(fun invocation ->
             String.equal invocation.P.Invocation.context.tool_name "agent_send"
             && not
                  (List.exists before ~f:(fun previous ->
                     P.Id.Invocation.equal previous.context.id invocation.context.id)))
         in
         match invocation.status with
         | Published (Complete receipt) -> receipt
         | status ->
           raise_s [%sexp "actual native send failed", (status : P.Invocation.status)]
       in
       protect_cleanup
         ~finally:(fun () ->
           release allow_adopt adopt;
           release allow_finish finish)
         (fun () ->
            let key name = P.Idempotency_key.of_string name |> protocol_ok in
            let first = send "567-parent-one" "567-managed-one" "first managed input" in
            let operation =
              within env "managed worker" (fun () -> Eio.Promise.await entered)
            in
            let first_state = managed child reference (key "567-managed-one") in
            assert (M.equal_status (Assigned operation) first_state.status);
            assert (Jsonaf.exactly_equal first (M.to_json first_state));
            let replay =
              send "567-parent-replay" "567-managed-one" "first managed input"
            in
            assert (Jsonaf.exactly_equal first replay);
            let second = send "567-parent-busy" "567-managed-two" "busy managed input" in
            let deferred = managed child reference (key "567-managed-two") in
            assert (M.equal_status Deferred deferred.status);
            assert (Jsonaf.exactly_equal second (M.to_json deferred));
            assert (
              Jsonaf.exactly_equal
                second
                (send "567-parent-busy-replay" "567-managed-two" "busy managed input"));
            [%test_eq: int] 1 !worker_calls;
            [%test_eq: int64] 1L (marks child).turn;
            [%test_eq: int64]
              1L
              (GD.Branch.confirmed_prefix (branch parent scope reference));
            [%test_eq: int] 2 (List.length (state child).managed_submissions);
            release allow_adopt adopt;
            let history =
              within env "managed adoption" (fun () -> Eio.Promise.await adopted)
            in
            assert (
              List.exists history ~f:(fun entry ->
                P.History.Id.equal (History_entry.id entry) deferred.history_id));
            List.iter (state child).managed_submissions ~f:(fun receipt ->
              assert (M.equal_status (Assigned operation) receipt.status));
            release allow_finish finish;
            within env "managed terminal" (fun () ->
              Goal_native_test_support.await_quiet child)
            |> ignore;
            List.iter (state child).managed_submissions ~f:(fun receipt ->
              assert (M.equal_status (Terminal (Some operation, Completed)) receipt.status));
            let terminal = managed child reference (key "567-managed-one") in
            assert (P.History.Id.equal terminal.history_id first_state.history_id);
            assert (
              Jsonaf.exactly_equal
                (M.to_json terminal)
                (send
                   "567-parent-terminal-replay"
                   "567-managed-one"
                   "first managed input"));
            [%test_eq: int] 1 !worker_calls;
            [%test_eq: int64] 1L (marks child).turn;
            [%test_eq: int64]
              1L
              (GD.Branch.confirmed_prefix (branch parent scope reference))));
  print_endline
    "actual native borrow; one confirmed managed Turn, replay and busy adoption";
  [%expect
    {| actual native borrow; one confirmed managed Turn, replay and busy adoption |}]
;;

module Writer_barrier = struct
  type t =
    { mutable child : P.Id.Session.t option
    ; reached : unit Eio.Promise.t
    ; signal : unit Eio.Promise.u
    ; release : unit Eio.Promise.t
    ; resume : unit Eio.Promise.u
    ; mutable operation : P.Id.Operation.t option
    }

  let create () =
    let reached, signal = Eio.Promise.create () in
    let release, resume = Eio.Promise.create () in
    { child = None; reached; signal; release; resume; operation = None }
  ;;

  let release t =
    if Option.is_none (Eio.Promise.peek t.release) then Eio.Promise.resolve t.resume ()
  ;;

  let wrap t env =
    Crash_fault_io.wrap
      env
      ~boundary:Before_sync
      ~matches:(fun path ->
        String.is_substring path ~substring:"/journal/"
        && String.is_suffix path ~suffix:".log")
      ~reached:(fun path ->
        Option.iter t.child ~f:(fun child ->
          let scan =
            Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path)
            |> Agent_store.Journal_segment.scan_contents
                 ~max_payload_length:(64 * 1024 * 1024)
            |> Source_preparation_tests.store_ok
          in
          match List.last scan.entries with
          | Some frame when Int.equal (Agent_store.Frame.flags frame.frame) 0 ->
            let transaction =
              Agent_store.Transaction.decode (Agent_store.Frame.payload frame.frame)
              |> Source_preparation_tests.store_ok
            in
            if P.Id.Session.equal transaction.session_id child
            then (
              let delta =
                A.Session_delta_document.decode
                  ~limits:Document_schema.Limits.default
                  transaction.delta
                |> document_ok
                |> A.Session_delta_document.value
              in
              let operation =
                Child_usage_fault_tests.Fault.leaves delta
                |> List.find_map ~f:(function
                  | A.Session_delta.Goal_descendants_changed update ->
                    (match GD.Update.reason update with
                     | Record_included { receipts; _ } ->
                       List.find_map receipts ~f:(fun receipt ->
                         let occurrence =
                           A.Goal_descendant_included.pending receipt
                           |> A.Goal_descendant_pending.occurrence
                         in
                         match A.Goal_descendant_occurrence.kind occurrence with
                         | Host_turn { operation_id } -> Some operation_id
                         | Attempt _ -> None)
                     | Register_new_root _
                     | Register_first_unrelated_child _
                     | Register_child _
                     | Register_branch _
                     | Register_retained_branch _
                     | Reserve _
                     | Import _
                     | Close_before_writer _ -> None)
                  | _ -> None)
              in
              Option.iter operation ~f:(fun operation ->
                t.child <- None;
                t.operation <- Some operation;
                Eio.Promise.resolve t.signal ();
                Eio.Promise.await t.release))
          | Some _ | None -> ()))
  ;;
end

let run_receipt child key =
  A.Run_state.receipts (Option.value_exn (state child).run_state)
  |> List.find_exn ~f:(fun receipt ->
    P.Idempotency_key.equal receipt.P.Run_receipt.key key
    && P.Run_receipt.Kind.equal receipt.kind Admission)
;;

let%expect_test
    "prepared inherited Run keeps real binding through replay and delayed cancelled ACK"
  =
  let barrier = Writer_barrier.create () in
  let provider_check = ref None in
  let provider_calls = ref 0 in
  let provider_entered, signal_provider = Eio.Promise.create () in
  let provider_release, release_provider = Eio.Promise.create () in
  let release () =
    if Option.is_none (Eio.Promise.peek provider_release)
    then Eio.Promise.resolve release_provider ()
  in
  Birth.with_native_goal_child
    ~wrap_env:(Writer_barrier.wrap barrier)
    ~post_creation_stream:(fun inputs ->
      Option.iter !provider_check ~f:(fun check ->
        let child_input =
          List.exists inputs ~f:(function
            | Openai.Responses.Item.Input_message message ->
              let json = Openai.Responses.Item.jsonaf_of_t (Input_message message) in
              Jsonaf.exactly_equal (Jsonaf.member_exn "role" json) (`String "developer")
              && String.is_substring
                   (Jsonaf.to_string json)
                   ~substring:"554 native Goal child"
            | _ -> false)
        in
        assert child_input;
        Int.incr provider_calls;
        check ());
      completed_response "567 actual prepared Run completed")
    (fun env sw daemon parent child scope reference _counters ->
       start_child daemon child;
       let observer =
         Owner.with_loaded_runtime child.runtime (fun runtime ->
           Option.bind
             runtime.A.Runtime_builder.moderator_manager
             ~f:Chat_response.Moderator_manager.invocation_observer
           |> Result.of_option ~error:(P.Error.invalid_request "real Run observer absent"))
         |> protocol_ok
       in
       let first_request = request child "567-run-one" "actual prepared inherited Run" in
       provider_check
       := Some
            (fun () ->
              let receipt = run_receipt child first_request.key in
              let current = state child in
              let run =
                A.Run_state.find (Option.value_exn current.run_state) receipt.run_id
                |> Option.value_exn
              in
              assert (
                P.Id.Principal.equal
                  run.principal_id
                  (Operator_authorization.principal original).id);
              assert (P.Invocation.equal_observer run.source.observer observer);
              assert (
                P.Session_ref.equal
                  run.session
                  (GD.session (Option.value_exn current.goal_descendants)));
              let operation = Option.value_exn current.active_operation in
              assert (
                List.exists run.owned_work ~f:(fun work ->
                  Int.equal work.P.Run_work.generation operation.generation
                  &&
                  match work.key with
                  | Operation id -> P.Id.Operation.equal id operation.id
                  | Retained _ -> false));
              assert (
                Option.is_some
                  (L.find_turn_handle
                     current.inference_ledger
                     ~operation_id:operation.id
                     ~generation:operation.generation));
              let included =
                confirmed parent child scope reference ~turns:1L ~attempts:1L
              in
              let occurrence =
                A.Goal_descendant_included.pending included
                |> A.Goal_descendant_pending.occurrence
              in
              (match A.Goal_descendant_occurrence.kind occurrence with
               | Attempt _ -> ()
               | Host_turn _ ->
                 failwith "provider preceded inherited Attempt confirmation");
              [%test_eq: int64]
                2L
                (GD.Branch.confirmed_prefix (branch parent scope reference));
              Eio.Promise.resolve signal_provider ();
              Eio.Promise.await provider_release);
       protect_cleanup
         ~finally:(fun () ->
           release ();
           Writer_barrier.release barrier)
         (fun () ->
            let first = Goal.ordinary daemon child first_request |> admitted in
            within env "real prepared Run provider" (fun () ->
              Eio.Promise.await provider_entered);
            let replay = Goal.ordinary daemon child first_request |> admitted in
            assert (P.Run_receipt.equal first replay);
            [%test_eq: int] 1 !provider_calls;
            let before = marks child in
            let run_count =
              List.length (A.Run_state.runs (Option.value_exn (state child).run_state))
            in
            (match
               Goal.ordinary
                 daemon
                 child
                 (request child "567-run-busy" "busy prepared Run")
             with
             | Rejected error -> [%test_eq: P.Error.code] Conflict error.code
             | Admitted _ | Uncertain _ -> failwith "busy user Run was admitted");
            [%test_eq: int64] before.turn (marks child).turn;
            [%test_eq: int64] before.attempt (marks child).attempt;
            [%test_eq: int]
              run_count
              (List.length (A.Run_state.runs (Option.value_exn (state child).run_state)));
            release ();
            within env "prepared Run terminal" (fun () ->
              Goal_native_test_support.await_quiet child)
            |> ignore;
            assert (P.Run_receipt.equal first (run_receipt child first_request.key));
            provider_check
            := Some (fun () -> failwith "cancelled prepared Run launched provider");
            let cancelled_request =
              request child "567-run-cancelled" "cancel at actual prepared Run writer"
            in
            barrier.child <- Some reference.child_session_id;
            let caller, signal_caller = Eio.Promise.create () in
            let cancelled =
              Failure "567 prepared Run caller cancelled during real writer"
            in
            let submission =
              Eio.Fiber.fork_promise ~sw (fun () ->
                Eio.Cancel.sub (fun context ->
                  Eio.Promise.resolve signal_caller context;
                  Goal.ordinary daemon child cancelled_request))
            in
            within env "prepared canonical writer" (fun () ->
              Eio.Promise.await barrier.reached);
            [%test_eq: int] 1 !provider_calls;
            Eio.Cancel.cancel (Eio.Promise.await caller) cancelled;
            (* Protected finite caller-exit may queue behind this real writer. The
            cancellation request is retained; release before joining its caller. *)
            Eio.Fiber.yield ();
            Writer_barrier.release barrier;
            (match Result.try_with (fun () -> Eio.Promise.await_exn submission) with
             | Error (Eio.Cancel.Cancelled cause) -> assert (phys_equal cause cancelled)
             | Error exn -> raise exn
             | Ok _ -> failwith "prepared writer caller did not preserve cancellation");
            let rec settled () =
              let current = state child in
              let root = branch parent scope reference in
              if
                Option.is_none current.active_operation
                && Option.is_none (GD.Branch.pending root)
                && Int64.equal (GD.Branch.confirmed_prefix root) 3L
              then current
              else (
                Eio.Fiber.yield ();
                settled ())
            in
            let final = within env "prepared delayed ACK cleanup" settled in
            let turns =
              P.Inference_query.Summary.turns (L.summary final.inference_ledger)
            in
            [%test_eq: int64] 1L turns.completed;
            [%test_eq: int64] 1L turns.failed;
            [%test_eq: int64] 0L turns.pending;
            let operation = Option.value_exn barrier.operation in
            assert (
              Option.is_some
                (L.find_turn_handle
                   final.inference_ledger
                   ~operation_id:operation
                   ~generation:final.identity.generation));
            ignore
              (confirmed parent child scope reference ~turns:2L ~attempts:1L
               : A.Goal_descendant_included.t);
            let retained = run_receipt child cancelled_request.key in
            assert (
              P.Run_receipt.equal
                retained
                (Goal.ordinary daemon child cancelled_request |> admitted));
            [%test_eq: int] 1 !provider_calls;
            [%test_eq: int64] 2L (marks child).turn;
            [%test_eq: int64]
              3L
              (GD.Branch.confirmed_prefix (branch parent scope reference))));
  print_endline
    "real Run observer/binding; replay and busy preserve counts; cancelled ACK remains \
     Included";
  [%expect
    {| real Run observer/binding; replay and busy preserve counts; cancelled ACK remains Included |}]
;;
