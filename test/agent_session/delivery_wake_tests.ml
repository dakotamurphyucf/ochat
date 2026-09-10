open Core
open Fixtures
module P = Agent_protocol
module D = P.Delivery
module E = P.Moderator_execution
module Delta = Agent_session.Session_delta
module State = Agent_session.Session_state

let%expect_test
    "notification wakes coalesce through an admitted operation without rewriting history \
     or legacy receipts"
  =
  with_actor_workspace (fun _ workspace_instance ->
    let open Delta in
    let initial, _, _, _, legacy = extension_fixture workspace_instance in
    let source : P.Invocation.observer =
      { script_id = "publisher"; source_sha256 = String.make 64 'a' }
    in
    let event =
      E.create
        { id = P.Id.Moderator_execution.create ()
        ; session_id
        ; generation = 0
        ; source
        ; operation_id = None
        ; job = None
        ; phase = Internal_event
        ; event = `Null
        ; checkpoint_sha256 = String.make 64 'b'
        ; created_at = timestamp
        }
      |> protocol_ok
    in
    let finished =
      E.complete
        event
        ~checkpoint_sha256:(String.make 64 'c')
        ~requests:{ request_turn = false; request_compaction = false; end_session = None }
      |> protocol_ok
    in
    let intents =
      List.init 2 ~f:(fun _ ->
        D.create
          { legacy.context with
            id = P.Id.Delivery.create ()
          ; invocation_id = None
          ; work = None
          ; ownership = Some { source; creator = Moderator_event event.context.id }
          }
        |> protocol_ok)
    in
    let publications =
      List.mapi intents ~f:(fun sequence intent ->
        let id =
          History_entry.Id.create ~namespace:"wake-data" ~sequence
          |> Result.ok_or_failwith
        in
        let entry = Agent_session.Notification_history.create ~id intent |> protocol_ok in
        ( D.commit ~track_wake:true intent ~history_id:id ~now:timestamp |> protocol_ok
        , entry ))
    in
    let apply state delta =
      Agent_session.Session_transition.apply ~now:timestamp state ~delta ~payloads:[]
    in
    let delta =
      Delta.Batch
        ([ Moderator_execution_changed event; Moderator_execution_changed finished ]
         @ List.map intents ~f:(fun value -> Delta.Delivery_changed value)
         @ List.map publications ~f:(fun (value, entry) ->
           Delta.Delivery_committed (value, entry)))
    in
    let committed = (apply initial delta |> protocol_ok).state in
    let history = committed.conversation.canonical_history in
    let pending = List.map publications ~f:fst in
    List.iter pending ~f:(fun value ->
      assert (
        Option.equal D.equal_wake_disposition value.wake_disposition (Some Pending_wake));
      assert (D.equal value (D.of_json (D.to_json value) |> protocol_ok)));
    let operation : P.Operation.t =
      { id = P.Id.Operation.create ()
      ; generation = 0
      ; kind = Turn Moderator_request
      ; state = Starting
      ; started_at = timestamp
      ; updated_at = timestamp
      }
    in
    let accepted =
      List.map pending ~f:(fun value ->
        D.accept_wake value ~operation_id:operation.id |> protocol_ok)
    in
    let changes = List.map accepted ~f:(fun value -> Delta.Delivery_wake_changed value) in
    assert (Result.is_error (apply committed (Batch changes)));
    let active operation =
      [ Delta.Active_operation_changed (Some operation)
      ; Lifecycle_changed
          { desired = Running; observed = Running_turn operation.P.Operation.id }
      ]
    in
    List.iter
      [ { operation with id = P.Id.Operation.create () }
      ; { operation with generation = 1 }
      ; { operation with kind = Compaction }
      ; { operation with state = Cancelling }
      ]
      ~f:(fun invalid ->
        assert (Result.is_error (apply committed (Batch (active invalid @ changes)))));
    assert (
      Result.is_error
        (apply
           committed
           (Batch
              ([ Active_operation_changed (Some operation)
               ; Lifecycle_changed { desired = Stopped; observed = Stopped }
               ]
               @ changes))));
    let admission = Delta.Batch (active operation @ changes) in
    let saved = (apply committed admission |> protocol_ok).state in
    assert (List.equal P.History.equal_entry history saved.conversation.canonical_history);
    List.iter saved.deliveries ~f:(fun value ->
      assert (
        Option.equal
          D.equal_wake_disposition
          value.wake_disposition
          (Some (Accepted_wake operation.id))));
    let completed =
      (apply
         saved
         (Batch
            [ Active_operation_changed None
            ; Lifecycle_changed { desired = Running; observed = Idle }
            ])
       |> protocol_ok)
        .state
    in
    let repeated = (apply completed (Batch changes) |> protocol_ok).state in
    assert (List.equal D.equal completed.deliveries repeated.deliveries);
    let replay = Delta.t_of_sexp (Delta.sexp_of_t admission) in
    let replayed = (apply committed replay |> protocol_ok).state in
    assert (List.equal D.equal saved.deliveries replayed.deliveries);
    let restored =
      Agent_session.Session_persistence.restore_snapshot
        (Sexp.to_string_mach (State.sexp_of_t completed))
      |> store_ok
    in
    assert (List.equal D.equal restored.deliveries completed.deliveries);
    assert (
      List.equal P.History.equal_entry restored.conversation.canonical_history history);
    let first = List.hd_exn pending
    and accepted = List.hd_exn accepted in
    let entry = snd (List.hd_exn publications) in
    assert (Result.is_error (apply committed (Delivery_committed (accepted, entry))));
    assert (Result.is_error (apply initial (Delivery_wake_changed accepted)));
    assert (
      Result.is_error
        (D.validate_transition ~previous:(Some (List.hd_exn intents)) accepted));
    assert (
      Result.is_error (D.accept_wake accepted ~operation_id:(P.Id.Operation.create ())));
    assert (Result.is_error (D.discard_wake accepted ~reason:"cannot reverse acceptance"));
    let discarded =
      D.discard_wake first ~reason:"automatic turn budget exhausted" |> protocol_ok
    in
    let discarded_state =
      (apply committed (Delivery_wake_changed discarded) |> protocol_ok).state
    in
    assert (
      List.equal
        P.History.equal_entry
        history
        discarded_state.conversation.canonical_history);
    assert (Result.is_error (D.validate_transition ~previous:(Some discarded) first));
    let rewrite_version fields version =
      List.map fields ~f:(fun (name, value) ->
        if String.equal name "schema_version" then name, `Number version else name, value)
    in
    let fields =
      match D.to_json accepted with
      | `Object fields -> fields
      | _ -> assert false
    in
    let stripped =
      List.filter fields ~f:(fun (key, _) -> not (String.equal key "wake_disposition"))
    in
    assert (Result.is_error (D.of_json (`Object stripped)));
    assert (Result.is_error (D.of_json (`Object (rewrite_version fields "2"))));
    let old = D.of_json (`Object (rewrite_version stripped "2")) |> protocol_ok in
    assert (Option.is_none old.wake_disposition);
    assert (Result.is_error (D.validate_transition ~previous:(Some accepted) old));
    assert (Result.is_error (D.validate_transition ~previous:(Some old) first));
    assert (Result.is_error (D.accept_wake old ~operation_id:operation.id));
    let historical_sexp =
      match D.sexp_of_t first with
      | List fields ->
        Sexp.List
          (List.filter fields ~f:(function
             | List [ Atom "wake_disposition"; _ ] -> false
             | _ -> true))
      | _ -> assert false
    in
    assert (Option.is_none (D.t_of_sexp historical_sexp).wake_disposition);
    List.iter [ accepted; discarded; old; legacy ] ~f:(fun value ->
      assert (D.equal value (D.of_json (D.to_json value) |> protocol_ok)));
    List.iter [ P.Completion.No_wake; Next_turn ] ~f:(fun wake ->
      let quiet = D.create { (List.hd_exn intents).context with wake } |> protocol_ok in
      let quiet =
        D.commit ~track_wake:true quiet ~history_id:entry.id ~now:timestamp |> protocol_ok
      in
      assert (Option.is_none quiet.wake_disposition));
    List.iter [ D.Job_adapter; External_ingress ] ~f:(fun source ->
      let adapter =
        D.create { first.context with ownership = None; source } |> protocol_ok
      in
      let tracked =
        D.commit ~track_wake:true adapter ~history_id:entry.id ~now:timestamp
        |> protocol_ok
      in
      assert (
        Option.equal D.equal_wake_disposition tracked.wake_disposition (Some Pending_wake));
      assert (D.equal tracked (D.of_json (D.to_json tracked) |> protocol_ok));
      assert (Option.is_none tracked.context.ownership);
      let untracked =
        D.commit adapter ~history_id:entry.id ~now:timestamp |> protocol_ok
      in
      assert (Option.is_none untracked.wake_disposition);
      let recommitted =
        D.commit ~track_wake:true untracked ~history_id:entry.id ~now:timestamp
        |> protocol_ok
      in
      assert (Option.is_none recommitted.wake_disposition));
    print_endline
      "two wakes accepted by one admitted turn; history unchanged; replay and snapshot \
       preserved";
    print_endline
      "wrong/stopped/cancelling/compaction admission rejected; terminal dispositions \
       immutable";
    print_endline
      "v1/v2 history has no new wake; v3 receipt cannot be dropped or reopened; quiet \
       policies stay quiet");
  [%expect
    {|
    two wakes accepted by one admitted turn; history unchanged; replay and snapshot preserved
    wrong/stopped/cancelling/compaction admission rejected; terminal dispositions immutable
    v1/v2 history has no new wake; v3 receipt cannot be dropped or reopened; quiet policies stay quiet
    |}]
;;
