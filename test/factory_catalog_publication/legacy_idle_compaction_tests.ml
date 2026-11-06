open! Core
open Agent_server_test_support
open Retry_support
module Foundation = Legacy_idle_completion_tests
module Fixture = Foundation.Fixture
module Physical = Foundation.Physical
module Store = Agent_store

let source =
  {|<config model="fixture-model"/>
<developer>Actual standalone aggregate compaction callback.</developer>
<script language="chatml" kind="moderator">
type state = int
type event = [ `Session_start | `Session_resume | `Tick ]
let initial_state = 0
let on_event : context -> state -> event -> state task =
  fun ctx state event -> match event with
  | `Session_start -> Task.pure(state)
  | `Session_resume -> Task.pure(state)
  | `Tick ->
    Task.bind(Runtime.request_compaction(), fun ignored_compaction ->
    Task.bind(Runtime.request_turn(), fun ignored_turn ->
    Task.pure(state + 1)))
</script>|}
;;

module Journal = struct
  let transactions (physical : Physical.t) =
    let session = physical.initial.identity.session_id in
    let scan = Store.Journal.scan physical.journal |> Source_preparation_tests.store_ok in
    List.filter_map scan.entries ~f:(fun (entry : Store.Journal.entry) ->
      if not (Int.equal (Store.Frame.flags entry.frame) 0)
      then None
      else (
        let payload = Store.Frame.payload entry.frame in
        let document =
          Document_schema.Document.decode ~limits:Document_schema.Limits.default payload
          |> document_ok
        in
        let matches_session =
          match Document_schema.Document.payload document with
          | `Object fields ->
            (match List.Assoc.find fields ~equal:String.equal "session_id" with
             | Some (`String actual) ->
               String.equal actual (P.Id.Session.to_string session)
             | _ -> failwith "actual journal transaction lacks its session identity")
          | _ -> failwith "actual journal transaction lacks its named payload"
        in
        if not matches_session
        then None
        else (
          let transaction =
            Store.Transaction.decode payload |> Source_preparation_tests.store_ok
          in
          [%test_eq: P.Id.Session.t] session transaction.session_id;
          Some transaction)))
  ;;

  let checkpoint (transaction : Store.Transaction.t) =
    A.Session_delta_document.decode
      ~limits:Document_schema.Limits.default
      transaction.delta
    |> document_ok
    |> A.Session_delta_document.value
    |> Child_usage_fault_tests.Fault.leaves
    |> List.find_map ~f:(function
      | A.Session_delta.Moderator_changed checkpoint ->
        A.Moderator_checkpoint.decode checkpoint |> protocol_ok
      | _ -> None)
  ;;

  let events (transaction : Store.Transaction.t) =
    List.map transaction.durable_events ~f:(fun document ->
      let event =
        A.Durable_event_document.decode ~limits:Document_schema.Limits.default document
        |> document_ok
        |> A.Durable_event_document.value
      in
      P.Event.Durable.Payload.of_json ~kind:event.kind event.payload |> protocol_ok)
  ;;

  let started transactions =
    List.concat_map transactions ~f:(fun transaction ->
      List.filter_map (events transaction) ~f:(function
        | Operation_started operation -> Some (transaction, operation)
        | _ -> None))
  ;;

  let checkpoint_before_start physical =
    let transactions = transactions physical in
    let started, operation =
      match started transactions with
      | [ started ] -> started
      | [] | _ :: _ :: _ -> failwith "expected exactly one actual compaction worker"
    in
    [%test_eq: P.Operation.kind] Compaction operation.kind;
    let checkpoint =
      List.filter transactions ~f:(fun transaction ->
        Option.exists (checkpoint transaction) ~f:(fun snapshot ->
          match snapshot.current_state with
          | Session.Snapshot.Int 1 -> List.is_empty snapshot.queued_internal_events
          | _ -> false))
      |> function
      | [ checkpoint ] -> checkpoint
      | [] | _ :: _ :: _ -> failwith "expected one actual callback checkpoint ACK"
    in
    assert (Int64.(checkpoint.transaction_sequence < started.transaction_sequence));
    operation
  ;;
end

module Cache = struct
  type t =
    { mutable directory : string option
    ; mutable renames : int
    }

  let create () = { directory = None; renames = 0 }

  let wrap_env t env =
    Crash_fault_io.wrap
      env
      ~boundary:After_rename
      ~matches:(fun path ->
        Option.exists t.directory ~f:(fun directory ->
          String.equal directory (Filename.dirname path)
          && String.is_prefix (Filename.basename path) ~prefix:".cache.bin."
          && String.is_suffix path ~suffix:".tmp"))
      ~reached:(fun _ -> t.renames <- t.renames + 1)
  ;;
end

let await_compacted (fixture : Fixture.t) =
  Foundation.within fixture.env "actual compaction terminal" (fun () ->
    let rec loop () =
      let state = Physical.state fixture.physical in
      match state.failure, state.active_operation with
      | Some failure, _ -> protocol_ok (Error failure)
      | None, None when Int.equal state.conversation.compaction_generation 1 -> state
      | None, (None | Some _) ->
        Eio.Fiber.yield ();
        loop ()
    in
    loop ())
;;

let%expect_test "legacy callback checkpoints before its one actual compaction worker" =
  let cache = Cache.create () in
  let current = ref None in
  let provider_operation = ref None in
  let before_provider () =
    let fixture : Fixture.t = Option.value_exn !current in
    [%test_eq: int] 1 !(fixture.providers);
    [%test_eq: int] 1 (Fixture.count fixture);
    let state = Physical.state fixture.physical in
    let expected = Journal.checkpoint_before_start fixture.physical in
    let actual = Option.value_exn state.active_operation in
    [%test_eq: P.Id.Operation.t] expected.id actual.id;
    [%test_eq: P.Operation.kind] Compaction actual.kind;
    (match actual.state with
     | Starting | Running -> ()
     | Cancelling | Completed | Failed _ | Cancelled | Interrupted _ ->
       failwith "provider entered without its actual live compaction operation");
    provider_operation := Some actual.id
  in
  Fixture.with_runtime
    ~wrap_env:(Cache.wrap_env cache)
    ~before_provider
    ~source
    (fun fixture ->
       current := Some fixture;
       cache.directory
       <- Some (Store.Session_store.Handle.cache_directory fixture.physical.handle);
       let initial = Physical.state fixture.physical in
       Option.iter initial.instruction_selection ~f:(fun selection ->
         let status = A.Instruction_selection.summary selection in
         assert (not status.project_selected);
         [%test_eq: int] 0 status.skills);
       [%test_eq: int] 0 initial.conversation.compaction_generation;
       [%test_eq: int] 0 !(fixture.providers);
       Fixture.enqueue fixture (Variant ("Tick", []));
       assert (not (Owner.drain_idle_moderator fixture.owner |> protocol_ok));
       let final = await_compacted fixture in
       [%test_eq: int] 1 (Fixture.count fixture);
       [%test_eq: int] 1 !(fixture.providers);
       assert (Option.is_none final.failure && Option.is_none final.active_operation);
       [%test_eq: int] 1 (List.length final.conversation.compaction_archives);
       [%test_eq: int] 1 (List.length final.conversation.canonical_history);
       let operation = Journal.checkpoint_before_start fixture.physical in
       [%test_eq: P.Id.Operation.t option] (Some operation.id) !provider_operation;
       let events =
         Journal.transactions fixture.physical |> List.concat_map ~f:Journal.events
       in
       let completed =
         List.filter_map events ~f:(function
           | Operation_completed operation -> Some operation
           | _ -> None)
       in
       (match completed with
        | [ completed ] ->
          [%test_eq: P.Id.Operation.t] operation.id completed.id;
          [%test_eq: P.Operation.kind] Compaction completed.kind;
          (match completed.state with
           | Completed -> ()
           | Starting | Running | Cancelling | Failed _ | Cancelled | Interrupted _ ->
             failwith "compaction terminal did not complete")
        | [] | _ :: _ :: _ -> failwith "actual compaction did not complete exactly once");
       [%test_eq: int]
         1
         (List.count events ~f:(function
            | History_replaced _ -> true
            | _ -> false));
       assert (
         not
           (List.exists events ~f:(function
              | Operation_failed _ | Operation_cancelled _ -> true
              | _ -> false)));
       (* Compaction takes priority over request_turn in this aggregate branch.
          No second Turn or provider is inferred from that subordinate request. *)
       assert (not (Owner.drain_idle_moderator fixture.owner |> protocol_ok));
       [%test_eq: int] 1 (Fixture.count fixture);
       [%test_eq: int] 1 !(fixture.providers);
       ignore (Journal.checkpoint_before_start fixture.physical : P.Operation.t);
       [%test_eq: int] 0 cache.renames;
       Owner.close_and_wait fixture.owner;
       assert (not (Owner.is_loaded fixture.owner));
       [%test_eq: int] 1 cache.renames;
       Owner.close_and_wait fixture.owner;
       [%test_eq: int] 1 cache.renames);
  print_endline
    "checkpoint ACK precedes one compaction worker/provider/terminal; close once";
  [%expect
    {| checkpoint ACK precedes one compaction worker/provider/terminal; close once |}]
;;

let%expect_test "legacy checkpoint ACK survives known compaction preparation rejection" =
  let cache = Cache.create () in
  Fixture.with_runtime ~wrap_env:(Cache.wrap_env cache) ~source (fun fixture ->
    let physical = fixture.physical in
    cache.directory <- Some (Store.Session_store.Handle.cache_directory physical.handle);
    Fixture.enqueue fixture (Variant ("Tick", []));
    (* Reserve only numeric IDs through the real public writer. A positive old
       frontier plus Int.max_value fits the durable int64 range, while the later
       compaction allocator must reject its platform-int conversion. *)
    let reservation =
      Actor.reserve_history_block physical.actor ~count:Int.max_value |> protocol_ok
    in
    assert (Int64.(reservation.first_sequence > 0L));
    assert (Int64.(reservation.reserved_through > of_int Int.max_value));
    assert (Int64.(reservation.reserved_through < max_value));
    let before = Physical.state physical in
    let previous_hash = A.Session_persistence.transaction_hash physical.durable in
    let previous = Journal.transactions physical in
    (match Owner.drain_idle_moderator fixture.owner with
     | Error failure ->
       [%test_eq: P.Error.code] Invalid_state failure.code;
       [%test_eq: string]
         "history sequence exceeds platform allocation range"
         failure.message
     | Ok _ -> failwith "compaction admitted an unrepresentable history allocator");
    let current = Physical.state physical in
    let transactions = Journal.transactions physical in
    [%test_eq: int] (List.length previous + 1) (List.length transactions);
    let acknowledged = List.last_exn transactions in
    [%test_eq: int64]
      Int64.(before.counters.transaction_sequence + 1L)
      acknowledged.transaction_sequence;
    [%test_eq: int64] Int64.(before.counters.revision + 1L) acknowledged.session_revision;
    [%test_eq: string option] previous_hash acknowledged.previous_transaction_hash;
    [%test_eq: int64]
      acknowledged.transaction_sequence
      current.counters.transaction_sequence;
    [%test_eq: int64] acknowledged.session_revision current.counters.revision;
    [%test_eq: string option]
      (Some (Store.Transaction.hash acknowledged))
      (A.Session_persistence.transaction_hash physical.durable);
    [%test_eq: int64]
      current.counters.revision
      (Store.Session_store.Handle.metadata physical.handle).session.revision;
    let actual_delta =
      A.Session_delta_document.decode
        ~limits:Document_schema.Limits.default
        acknowledged.delta
      |> document_ok
      |> A.Session_delta_document.value
      |> Child_usage_fault_tests.Fault.leaves
    in
    (match actual_delta with
     | [ Moderator_changed moderator ] ->
       assert (Option.equal Jsonaf.exactly_equal moderator current.moderator)
     | _ -> failwith "known prep rejection wrote more than its checkpoint prefix");
    let state_json state =
      state_document state
      |> Source_preparation_tests.store_ok
      |> Document_schema.Document.json
    in
    assert (
      Jsonaf.exactly_equal
        (state_json current)
        (state_json
           (A.Session_persistence.Restored.state
              (A.Session_persistence.restored physical.durable))));
    [%test_eq: int] 1 (Fixture.count fixture);
    let manager = Option.value_exn fixture.runtime.moderator_manager in
    let live =
      Chat_response.Moderator_manager.identity_snapshot manager |> Result.ok_or_failwith
    in
    (match live.current_state with
     | Int 1 -> ()
     | _ -> failwith "actual callback did not complete once");
    assert (List.is_empty live.queued_internal_events);
    assert (Option.is_none current.failure && Option.is_none current.active_operation);
    assert (List.is_empty current.conversation.compaction_archives);
    [%test_eq: int] 0 current.conversation.compaction_generation;
    [%test_eq: int] 0 !(fixture.providers);
    assert (List.is_empty (Journal.started transactions));
    assert (Store.Commit_writer.is_available physical.writer);
    let frames = Physical.frames physical in
    Fixture.with_completion
      fixture
      ~capture:(fun _ _ -> ())
      (fun _ () ->
         assert (Option.is_none (Actor.claim_idle_moderator physical.actor |> protocol_ok)));
    assert (not (Owner.drain_idle_moderator fixture.owner |> protocol_ok));
    [%test_eq: string list] frames (Physical.frames physical);
    [%test_eq: int] 1 (Fixture.count fixture);
    let after =
      Chat_response.Moderator_manager.identity_snapshot manager |> Result.ok_or_failwith
    in
    assert (
      Jsonaf.exactly_equal
        (A.Runtime_builder.encode_moderator_snapshot live)
        (A.Runtime_builder.encode_moderator_snapshot after));
    [%test_eq: int] 0 cache.renames;
    Owner.close_and_wait fixture.owner;
    assert (not (Owner.is_loaded fixture.owner));
    [%test_eq: int] 1 cache.renames;
    Owner.close_and_wait fixture.owner;
    [%test_eq: int] 1 cache.renames;
    [%test_eq: string list] frames (Physical.frames physical));
  print_endline
    "checkpoint ACK retained; known unattempted compaction released; close once";
  [%expect
    {| checkpoint ACK retained; known unattempted compaction released; close once |}]
;;
