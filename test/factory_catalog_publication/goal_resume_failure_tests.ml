open! Core
open Agent_server_test_support
open Retry_support
module Goal = Goal_service_tests
module Staging = Goal_resume_tests
module Service = Agent_server.Goal_run_service
module Resume = A.Goal_resume_request
module G = P.Goal

(* Only the later root writer is faulted. The earlier catalog-only command must
   already exist in the real journal, and the selected transaction must pair the
   actual Administrative Operation with the original Run's owned work. *)
module Fault = struct
  type selection =
    { session : P.Id.Session.t
    ; request : Resume.t
    ; run_id : P.Id.Run.t
    }

  type candidate =
    { transaction : Agent_store.Transaction.t
    ; delta : A.Session_delta.t
    ; operation : P.Operation.t
    ; index : A.Run_state.t
    ; receipt : A.Goal_command_receipt.t
    }

  type t =
    { mutable selected : selection option
    ; mutable command : A.Goal_command_receipt.t option
    ; mutable candidate : candidate option
    ; mutable hits : int
    }

  let create () = { selected = None; command = None; candidate = None; hits = 0 }

  let reached t env path =
    match t.selected with
    | None -> ()
    | Some selected ->
      let scan =
        Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path)
        |> Agent_store.Journal_segment.scan_contents ~max_payload_length:(64 * 1024 * 1024)
        |> Existing.store_ok
      in
      (match List.last scan.entries with
       | Some frame when Int.equal (Agent_store.Frame.flags frame.frame) 0 ->
         let transaction =
           Agent_store.Transaction.decode (Agent_store.Frame.payload frame.frame)
           |> Existing.store_ok
         in
         if P.Id.Session.equal transaction.session_id selected.session
         then (
           let delta =
             A.Session_delta_document.decode ~limits:D.Limits.default transaction.delta
             |> document_ok
             |> A.Session_delta_document.value
           in
           let changes = Goal_admission_fault_tests.atoms delta in
           List.iter changes ~f:(function
             | A.Session_delta.Goal_catalog_changed catalog ->
               let receipt =
                 A.Goal_catalog.receipt
                   catalog
                   ~principal_id:Staging.resume_principal.id
                   ~key:(Resume.key selected.request)
                   ~request_sha256:(Resume.digest selected.request |> protocol_ok)
                 |> protocol_ok
               in
               Option.iter receipt ~f:(fun receipt -> t.command <- Some receipt)
             | _ -> ());
           let operation =
             List.find_map changes ~f:(function
               | A.Session_delta.Active_operation_changed
                   (Some ({ kind = Turn Administrative; _ } as operation)) ->
                 Some operation
               | _ -> None)
           in
           let index =
             List.find_map changes ~f:(function
               | A.Session_delta.Run_state_changed index -> Some index
               | _ -> None)
           in
           match t.command, operation, index with
           | Some receipt, Some operation, Some index ->
             let run = A.Run_state.find index selected.run_id |> Option.value_exn in
             if
               List.exists run.owned_work ~f:(fun work ->
                 P.Run_work.Key.equal work.key (Operation operation.id))
             then (
               t.selected <- None;
               t.candidate <- Some { transaction; delta; operation; index; receipt };
               t.hits <- t.hits + 1;
               raise (Core_unix.Unix_error (EIO, "543 Administrative sync", path)))
           | _ -> ())
       | Some _ | None -> ())
  ;;
end

let service daemon =
  Service.create ~factory:(Daemon.factory daemon) ~authorize:(fun actor ->
    Staging.authorize_as (Operator_authorization.principal actor))
;;

let run current run_id =
  A.Run_state.find (Option.value_exn current.A.Session_state.run_state) run_id
  |> Option.value_exn
;;

let assert_attempts before after =
  let left = Goal.accounting before in
  let right = Goal.accounting after in
  assert (
    A.Goal_accounting.Scope.equal
      (A.Goal_accounting.scope left)
      (A.Goal_accounting.scope right));
  [%test_eq: int64]
    (A.Goal_accounting.admitted_attempts left)
    (A.Goal_accounting.admitted_attempts right);
  assert (
    List.equal
      A.Goal_accounting.Binding.equal
      (A.Goal_accounting.bindings left)
      (A.Goal_accounting.bindings right));
  assert (
    List.equal
      (fun left right ->
         A.Inference_ledger.Handle.equal
           (A.Inference_ledger.Row.handle left)
           (A.Inference_ledger.Row.handle right)
         && Jsonaf.exactly_equal
              (Inference.Observation.Attempt_record.to_json
                 (A.Inference_ledger.Row.record left))
              (Inference.Observation.Attempt_record.to_json
                 (A.Inference_ledger.Row.record right)))
      (A.Inference_ledger.rows before.A.Session_state.inference_ledger)
      (A.Inference_ledger.rows after.A.Session_state.inference_ledger))
;;

let ledger_turns state =
  let payload =
    match
      D.Json.field
        (Staging.ledger_json state.A.Session_state.inference_ledger)
        ~name:"payload"
    with
    | Value value -> value
    | _ -> failwith "canonical ledger payload absent"
  in
  match D.Json.field payload ~name:"turns" with
  | Value (`Array turns) -> turns
  | _ -> failwith "canonical turn carrier absent"
;;

let turn_operation turn =
  match D.Json.field turn ~name:"operation" with
  | Value value -> P.Operation.of_json value |> protocol_ok
  | _ -> failwith "canonical turn operation absent"
;;

let selected_turn state operation_id =
  List.filter (ledger_turns state) ~f:(fun turn ->
    P.Id.Operation.equal (turn_operation turn).id operation_id)
  |> function
  | [ turn ] -> turn
  | _ -> failwith "canonical host turn is not unique"
;;

let assert_candidate held admission (candidate : Fault.candidate) =
  let previous = run held admission.P.Run_receipt.run_id in
  let current = A.Run_state.find candidate.index previous.id |> Option.value_exn in
  let work =
    P.Run_work.create
      ~key:(Operation candidate.operation.id)
      ~generation:candidate.operation.generation
    |> protocol_ok
  in
  assert (P.Id.Principal.equal previous.principal_id current.principal_id);
  assert (P.Run_source.equal previous.source current.source);
  assert (P.Run.Lifecycle.equal previous.lifecycle current.lifecycle);
  [%test_eq: int64] (Int64.succ previous.revision) current.revision;
  assert (List.equal P.Run_work.equal (previous.owned_work @ [ work ]) current.owned_work);
  assert (
    List.equal P.Run_work.Terminal.equal previous.terminal_work current.terminal_work);
  assert (
    Option.equal
      A.Run_goal_link.equal
      (A.Run_state.goal_link (Option.value_exn held.run_state) previous.id)
      (A.Run_state.goal_link candidate.index previous.id));
  assert (
    List.equal
      P.Run_receipt.equal
      (A.Run_state.receipts (Option.value_exn held.run_state))
      (A.Run_state.receipts candidate.index));
  let events =
    A.Session_persistence.durable_events ~limits:D.Limits.default candidate.transaction
    |> Existing.store_ok
  in
  [%test_eq: int]
    1
    (List.count events ~f:(fun event ->
       match
         P.Event.Durable.Payload.of_json ~kind:event.kind event.payload |> protocol_ok
       with
       | Operation_started operation ->
         assert (
           Jsonaf.exactly_equal
             (P.Operation.to_json candidate.operation)
             (P.Operation.to_json operation));
         true
       | _ -> false));
  work
;;

let reopen
      env
      sw
      root
      daemon
      policy
      entry
      counters
      held
      admission
      request
      actor
      candidate
      written_state
      work
      primary
  =
  let old_store = Daemon.store daemon in
  (match Daemon.shutdown daemon with
   | Ok () -> ()
   | Error cleanup ->
     raise_s
       [%sexp
         "shutdown could not join after acknowledged-command failure"
       , (primary : P.Error.t)
       , (cleanup : P.Error.t)]);
  assert (Agent_store.Session_store.is_closed old_store);
  let recovered =
    Daemon.start
      ~sw
      ~env:(env :> Eio_unix.Stdenv.base)
      ~config:(config root root (Filename.concat root "parent.chatmd"))
      ~tool_dir:root
      ~home:root
      ~process_start_identity:None
      ~options:
        { Daemon.default_options with
          inference_policy = policy
        ; qualify_chatml_extensions = true
        }
      ()
    |> protocol_ok
  in
  protect_cleanup
    ~finally:(fun () -> Daemon.shutdown recovered |> protocol_ok)
    (fun () ->
       assert (not (phys_equal old_store (Daemon.store recovered)));
       let loaded =
         Registry.load
           (Daemon.registry recovered)
           held.A.Session_state.identity.session_id
         |> protocol_ok
       in
       assert (not (phys_equal entry.Registry.actor loaded.actor));
       (* This joins the one genuine host recovery lifecycle. Repeating the Goal
          command below must not execute another lifecycle or acquire authority. *)
       let current = Retry_rotation_tests.await_recovered loaded held in
       let actual = run current admission.P.Run_receipt.run_id in
       let written =
         A.Run_state.find candidate.Fault.index actual.id |> Option.value_exn
       in
       (match actual.lifecycle with
        | Terminal Interrupted -> ()
        | _ ->
          raise_s
            [%sexp "recovery did not interrupt the original Run", (actual : P.Run.t)]);
       assert (List.equal P.Run_work.equal written.owned_work actual.owned_work);
       assert (P.Id.Principal.equal written.principal_id actual.principal_id);
       assert (P.Run_source.equal written.source actual.source);
       List.iter written.terminal_work ~f:(fun previous ->
         assert (List.exists actual.terminal_work ~f:(P.Run_work.Terminal.equal previous)));
       List.iter
         (A.Run_state.receipts (Option.value_exn held.run_state))
         ~f:(fun previous ->
           assert (
             List.exists
               (A.Run_state.receipts (Option.value_exn current.run_state))
               ~f:(P.Run_receipt.equal previous)));
       assert (
         List.exists actual.terminal_work ~f:(fun proof ->
           P.Run_work.equal work proof.work
           && P.Run_work.Terminal.equal_outcome Unconfirmed proof.outcome));
       assert (
         Option.equal
           A.Run_goal_link.equal
           (A.Run_state.goal_link candidate.index actual.id)
           (A.Run_state.goal_link (Option.value_exn current.run_state) actual.id));
       assert (G.State.equal (Open (Held Source_review)) (G.state (Goal.goal current)));
       assert (
         Option.equal
           P.Goal_episode.equal
           (G.episode (Goal.goal held))
           (G.episode (Goal.goal current)));
       assert_attempts written_state current;
       let written_turn = selected_turn written_state candidate.operation.id in
       let recovered_turn = selected_turn current candidate.operation.id in
       let operation = turn_operation recovered_turn in
       (match operation.state with
        | Interrupted _ -> ()
        | _ ->
          raise_s
            [%sexp "recovered host turn is not interrupted", (operation : P.Operation.t)]);
       assert (P.Operation.equal_kind candidate.operation.kind operation.kind);
       [%test_eq: int] candidate.operation.generation operation.generation;
       assert (
         Int.equal
           0
           (P.Timestamp.compare candidate.operation.started_at operation.started_at));
       assert (
         D.Json.equal
           (match D.Json.field written_turn ~name:"ordinal" with
            | Value value -> value
            | _ -> failwith "turn ordinal absent")
           (match D.Json.field recovered_turn ~name:"ordinal" with
            | Value value -> value
            | _ -> failwith "recovered turn ordinal absent"));
       [%test_eq: int64]
         (A.Goal_accounting.admitted_turns (Goal.accounting written_state))
         (A.Goal_accounting.admitted_turns (Goal.accounting current));
       [%test_eq: int] 1 !(counters.providers);
       let session =
         P.Session_ref.create
           ~server_id:(Agent_store.Session_store.server_id (Daemon.store recovered))
           ~session_id:current.identity.session_id
       in
       let receipt =
         Staging.historical loaded session ~actor request |> Option.value_exn
       in
       assert (A.Goal_command_receipt.equal candidate.receipt receipt);
       let frames = Existing.frames env loaded in
       let counts = !(counters.providers), !(counters.inference_ports) in
       (match
          Service.resume_episode
            (service recovered)
            ~entry:loaded
            ~actor
            ~session
            ~request
          |> protocol_ok
        with
        | Retained actual -> assert (A.Goal_command_receipt.equal receipt actual)
        | _ -> failwith "recovered command replay tried to resume execution");
       Existing.assert_same current loaded;
       [%test_eq: string list] frames (Existing.frames env loaded);
       [%test_eq: int * int] counts (!(counters.providers), !(counters.inference_ports)))
;;

exception Observed_resume_failure of P.Error.t

let fault_case boundary ~recover =
  let fault = Fault.create () in
  (match
     Staging.with_recoverable_episode
       ~wrap_env:(fun env ->
         Crash_fault_io.wrap
           env
           ~boundary
           ~matches:(fun path ->
             String.is_substring path ~substring:"/journal/"
             && String.is_suffix path ~suffix:".log")
           ~reached:(Fault.reached fault env))
       (fun env
         sw
         root
         daemon
         policy
         entry
         _runtime
         counters
         session
         admission
         _old_live
         held ->
          let actor = Staging.guarded Staging.resume_principal (ref true) in
          let request = Staging.exact_request held ~key:"goal-resume-root-sync" in
          let ports = !(counters.inference_ports) in
          fault.selected
          <- Some
               { session = held.identity.session_id; request; run_id = admission.run_id };
          let receipt, failure =
            match
              Service.resume_episode (service daemon) ~entry ~actor ~session ~request
              |> protocol_ok
            with
            | Resume_failed { receipt; error } -> receipt, error
            | _ -> failwith "later root failure lost its acknowledged resume command"
          in
          [%test_eq: P.Error.code] Persistence_error failure.code;
          [%test_eq: int] 1 fault.hits;
          let candidate = Option.value_exn fault.candidate in
          assert (A.Goal_command_receipt.equal candidate.receipt receipt);
          Staging.assert_receipt request held receipt;
          let current = state entry in
          Staging.assert_existing_episode held current;
          assert (G.State.equal (Open Enabled) (G.state (Goal.goal current)));
          assert (
            List.exists
              (A.Goal_catalog.receipts (Option.value_exn current.goal_catalog))
              ~f:(A.Goal_command_receipt.equal receipt));
          assert (Owner.is_loaded entry.runtime);
          [%test_eq: int] 1 !(counters.providers);
          [%test_eq: int] ports !(counters.inference_ports);
          let work = assert_candidate held admission candidate in
          (* Replay the actual encoded candidate as DATA. Host_turn_tracking
             already charged this Operation atomically before the writer. *)
          let written_state =
            A.Session_delta.apply current candidate.delta |> protocol_ok
          in
          assert_attempts held written_state;
          let written_turn = selected_turn written_state candidate.operation.id in
          assert (
            Jsonaf.exactly_equal
              (P.Operation.to_json candidate.operation)
              (P.Operation.to_json (turn_operation written_turn)));
          [%test_eq: int]
            (List.length (ledger_turns held) + 1)
            (List.length (ledger_turns written_state));
          List.iter (ledger_turns held) ~f:(fun old ->
            assert (List.exists (ledger_turns written_state) ~f:(Jsonaf.exactly_equal old)));
          [%test_eq: int64]
            (Int64.succ (A.Goal_accounting.admitted_turns (Goal.accounting held)))
            (A.Goal_accounting.admitted_turns (Goal.accounting written_state));
          (* The failed writer poisons Store's canonical head. Receipt DATA
                 above is known ACK, not permission to read through uncertainty. *)
          let unreadable =
            Actor.goal_resume_receipt
              entry.actor
              ~actor
              ~authorize:(Staging.authorize_as Staging.resume_principal)
              ~session
              ~request
            |> rejected
          in
          [%test_eq: P.Error.code] Journal_corrupt unreadable.code;
          if recover
          then
            reopen
              env
              sw
              root
              daemon
              policy
              entry
              counters
              held
              admission
              request
              actor
              candidate
              written_state
              work
              failure;
          (* Keep the real primary failure through the whole poisoned-owner
                 fixture cleanup; only this private exact exception is caught. *)
          raise (Observed_resume_failure failure))
   with
   | () -> failwith "Administrative fault was not reached"
   | exception Observed_resume_failure failure ->
     [%test_eq: P.Error.code] Persistence_error failure.code);
  [%test_eq: int] 1 fault.hits
;;

let%expect_test "Goal resume retains its command ACK across later root writer uncertainty"
  =
  fault_case Before_sync ~recover:false;
  fault_case After_sync ~recover:true;
  print_endline
    "later Administrative write failures retain the command ACK and confirmed original \
     episode";
  print_endline
    "durable unacknowledged root reopens Interrupted with unconfirmed work; retained \
     replay has no effects";
  [%expect
    {| 
    later Administrative write failures retain the command ACK and confirmed original episode
    durable unacknowledged root reopens Interrupted with unconfirmed work; retained replay has no effects
    |}]
;;
