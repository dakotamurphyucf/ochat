open! Core
open Agent_server_test_support
open Retry_support
module F = Delegated_policy_caller_support
module Warm = Delegated_warm_resource_tests
module Shared = Delegated_policy_claim_tests
module Resource_loan = Ordinary_warm_resource_tests.Resource_loan
module Physical = Constructor_model_fault_tests.Physical
module Existing_fault = Legacy_idle_completion_fault_tests
module Store = Agent_store
module E = P.Moderator_execution
module I = P.Invocation
module Claim = Actor.Delegated_policy_claim
module Cause = A.Policy_cause_data
module PC = A.Policy_canonical_usage

(* This test selects one actual registered-parent claim transaction. An arbitrary
   receipt-containing batch is not the seed whose unknown ACK we exercise. *)
let seed_parts transaction =
  match Physical.changes transaction with
  | [ Run_state_changed index
    ; Moderator_execution_changed receipt
    ; Policy_cause_changed update
    ] -> index, receipt, update
  | _ -> failwith "unknown parent claim did not retain its exact three-part seed"
;;

let checked_seed
      (before : A.Session_state.t)
      ~server_id
      ~(child_source : P.Run_source.t)
      ~(child_reference : Store.Delegation_store.Reference.t)
      ~(child_invocation : I.t)
      transaction
  =
  let index, receipt, update = seed_parts transaction in
  let expected_index, source =
    A.Moderator_source_installation.capture before |> protocol_ok
  in
  assert (A.Run_state.equal expected_index index);
  assert (
    P.Id.Session.equal transaction.Store.Transaction.session_id before.identity.session_id);
  assert (P.Id.Session.equal receipt.context.session_id before.identity.session_id);
  [%test_eq: int] before.identity.generation receipt.context.generation;
  assert (E.equal_phase receipt.context.phase Pre_tool_call);
  assert (E.equal_status receipt.status Running && Option.is_none receipt.decision);
  assert (
    Option.is_none receipt.context.operation_id && Option.is_none receipt.context.job);
  assert (I.equal_observer receipt.context.source source.observer);
  let delegation = Option.value_exn receipt.delegation in
  assert (
    P.Id.Session.equal delegation.child_session_id child_invocation.context.session_id);
  [%test_eq: int] delegation.child_generation child_invocation.context.generation;
  assert (P.Id.Invocation.equal delegation.child_invocation_id child_invocation.context.id);
  assert (P.Id.Session.equal child_reference.child_session_id delegation.child_session_id);
  assert (
    P.Id.Session.equal child_reference.key.parent_session_id before.identity.session_id);
  [%test_eq: int] before.identity.generation child_reference.key.parent_generation;
  [%test_eq: int] delegation.child_generation child_source.generation;
  assert (
    I.equal_observer
      child_source.observer
      (Option.value_exn child_invocation.observation).observer);
  let raw_update = PC.Update.to_json update |> protocol_ok in
  let reason = Jsonaf.member_exn "reason" raw_update in
  [%test_eq: string]
    "register_event"
    (Jsonaf.member_exn "kind" reason |> Jsonaf.string_exn);
  let cause = Jsonaf.member_exn "cause" reason |> Cause.of_json |> protocol_ok in
  assert (Cause.matches_event cause receipt);
  assert (P.Id.Moderator_execution.equal (Cause.parent_event cause) receipt.context.id);
  assert (
    P.Session_ref.equal
      (Cause.callee cause)
      (P.Session_ref.create ~server_id ~session_id:before.identity.session_id));
  assert (P.Run_source.equal (Cause.callee_source cause) source);
  assert (
    P.Session_ref.equal
      (Cause.caller cause)
      (P.Session_ref.create ~server_id ~session_id:delegation.child_session_id));
  assert (Cause.Caller_source.matches_installed (Cause.caller_source cause) child_source);
  assert (P.Id.Invocation.equal (Cause.child_invocation cause) child_invocation.context.id);
  [%test_eq: string] child_reference.admission_sha256 (Cause.admission_sha256 cause);
  assert (
    List.equal
      Store.Delegation_store.Reference.equal
      [ child_reference ]
      (Cause.path cause));
  let origin =
    A.Invocation_inference_provenance.of_invocation child_invocation |> protocol_ok
  in
  assert (Option.equal P.Inference_origin.equal (Some origin) (Cause.caller_origin cause));
  assert (List.is_empty (Cause.targets cause));
  assert (Option.is_none (Cause.forwarding cause));
  [%test_eq: int]
    Daemon.default_options.factory_limits.delegation_max_depth
    (Cause.max_inherited_scopes cause);
  assert (Option.is_none before.policy_canonical_usage);
  let prefix =
    A.Session_delta.apply before (Run_state_changed expected_index) |> protocol_ok
  in
  let after =
    A.Session_delta.apply prefix (Moderator_execution_changed receipt) |> protocol_ok
  in
  let expected_update =
    PC.register_event
      prefix.policy_canonical_usage
      ~before:(A.Session_state.policy_context prefix |> protocol_ok)
      ~after:(A.Session_state.policy_context after |> protocol_ok)
      ~ledger:prefix.inference_ledger
      ~cause
    |> protocol_ok
    |> PC.Update.to_json
    |> protocol_ok
  in
  (* The existing validator independently reproduces the entire candidate, not
     just its new cause: unrelated rows, bindings or coverage must not change. *)
  assert (D.Json.equal expected_update raw_update);
  A.Session_delta.apply
    before
    (Batch
       [ Run_state_changed index
       ; Moderator_execution_changed receipt
       ; Policy_cause_changed update
       ])
  |> protocol_ok
;;

let frames env handle =
  let journal =
    Store.Journal.open_existing
      ~env
      ~directory:(Store.Session_store.Handle.journal_directory handle)
      ~max_payload_length:Physical.max_payload_length
      ~max_segment_bytes:4194304L
      ~max_segment_frames:32
    |> Existing.store_ok
  in
  let scan : Store.Journal.scan = Store.Journal.scan journal |> Existing.store_ok in
  List.map scan.entries ~f:(fun (entry : Store.Journal.entry) ->
    ( Store.Frame.flags entry.frame
    , Store.Frame.payload entry.frame
    , Store.Frame.checksum_hex entry.frame ))
;;

let transactions values =
  List.filter_map values ~f:(fun (flags, payload, _) ->
    if Int.equal flags 0
    then Some (Store.Transaction.decode payload |> Existing.store_ok)
    else None)
;;

module Fault = struct
  type selected =
    { session : P.Id.Session.t
    ; child_session : P.Id.Session.t
    ; child_generation : int
    ; index : A.Run_state.t
    ; source : P.Run_source.t
    ; journal : string
    ; policy : string
    }

  type t =
    { cache : Shared.Native_gate.t
    ; mutable selected : selected option
    ; mutable transaction : Store.Transaction.t option
    ; mutable receipt : E.t option
    ; mutable payload : string option
    ; mutable checksum : string option
    ; mutable path : string option
    ; mutable injected : exn option
    ; mutable hits : int
    ; mutable policy_reads : int
    }

  let create () =
    { cache = Shared.Native_gate.create ()
    ; selected = None
    ; transaction = None
    ; receipt = None
    ; payload = None
    ; checksum = None
    ; path = None
    ; injected = None
    ; hits = 0
    ; policy_reads = 0
    }
  ;;

  let claim selected transaction =
    if not (P.Id.Session.equal transaction.Store.Transaction.session_id selected.session)
    then None
    else (
      match Physical.changes transaction with
      | [ Run_state_changed index
        ; Moderator_execution_changed receipt
        ; Policy_cause_changed _
        ]
        when A.Run_state.equal selected.index index
             && I.equal_observer selected.source.observer receipt.context.source
             && E.equal_phase receipt.context.phase Pre_tool_call
             && E.equal_status receipt.status Running
             && Option.exists receipt.delegation ~f:(fun delegation ->
               P.Id.Session.equal delegation.child_session_id selected.child_session
               && Int.equal delegation.child_generation selected.child_generation) ->
        Some receipt
      | _ -> None)
  ;;

  let wrap t env =
    let env = Shared.Native_gate.wrap t.cache env in
    let env =
      Crash_fault_io.wrap
        env
        ~boundary:Before_open_in
        ~matches:(fun path ->
          Option.exists t.selected ~f:(fun selected -> String.equal path selected.policy))
        ~reached:(fun _ -> t.policy_reads <- t.policy_reads + 1)
    in
    Crash_fault_io.wrap
      env
      ~boundary:After_sync
      ~matches:(fun path ->
        Option.exists t.selected ~f:(fun selected ->
          String.equal selected.journal (Filename.dirname path)
          && String.is_suffix path ~suffix:".log"))
      ~reached:(fun path ->
        let selected = Option.value_exn t.selected in
        let scan =
          Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path)
          |> Store.Journal_segment.scan_contents
               ~max_payload_length:Physical.max_payload_length
          |> Existing.store_ok
        in
        match List.last scan.entries with
        | Some entry when Int.equal (Store.Frame.flags entry.frame) 0 ->
          let transaction =
            Store.Transaction.decode (Store.Frame.payload entry.frame)
            |> Existing.store_ok
          in
          (match claim selected transaction with
           | None -> ()
           | Some receipt ->
             assert (Option.is_none t.transaction);
             t.transaction <- Some transaction;
             t.receipt <- Some receipt;
             t.payload <- Some (Store.Frame.payload entry.frame);
             t.checksum <- Some (Store.Frame.checksum_hex entry.frame);
             t.path <- Some path;
             t.hits <- t.hits + 1;
             let injected =
               Core_unix.Unix_error (EIO, "592 parent claim after sync", path)
             in
             t.injected <- Some injected;
             raise injected)
        | None | Some _ -> ())
  ;;

  let arm t (fixture : F.t) before =
    let index, source = A.Moderator_source_installation.capture before |> protocol_ok in
    let handle = Option.value_exn fixture.parent.store_handle in
    let child = fixture.child_initial.identity in
    t.selected
    <- Some
         { session = Store.Session_store.Handle.session_id handle
         ; child_session = child.session_id
         ; child_generation = child.generation
         ; index
         ; source
         ; journal = Store.Session_store.Handle.journal_directory handle
         ; policy =
             Filename.concat
               fixture.parent_initial.spec.workspace_instance.canonical_root.native_path
               "policy.txt"
         };
    t.cache.cache
    <- Some
         (Filename.concat (Store.Session_store.Handle.cache_directory handle) "cache.bin")
  ;;
end

let assert_same expected actual =
  assert (
    Jsonaf.exactly_equal
      (Existing_fault.state_json expected)
      (Existing_fault.state_json actual))
;;

let error = function
  | Error failure -> failure
  | Ok _ -> failwith "unknown parent claim unexpectedly settled"
;;

let close_error owner =
  match Owner.close_and_wait owner with
  | () -> failwith "unknown parent claim reported a successful strong close"
  | exception Owner.Cleanup_failed failure -> failure
;;

let run_child root =
  let fault = Fault.create () in
  F.with_fixture ~test_root:root ~wrap_env:(Fault.wrap fault) (fun t ->
    Eio.Switch.run (fun sw ->
      let handle = Option.value_exn t.parent.store_handle in
      let anchor = Physical.snapshot_anchor t.env handle in
      let loan = Resource_loan.create ~sw t.parent in
      let retained =
        F.within t "real parent resource loan before unknown claim" (fun () ->
          Resource_loan.await loan)
      in
      assert (phys_equal retained.worker t.parent_runtime.worker);
      let before = Warm.settled t.parent in
      assert (List.is_empty (F.policy_receipts before));
      assert (List.is_empty before.invocations);
      let ports = !(t.counters.inference_ports) in
      Fault.arm fault t before;
      assert (Option.is_none t.provider.call);
      let _, operation =
        F.submit
          t.child
          t.child_writer
          ~actor:(Some t.b)
          "592 actual unknown parent claim"
      in
      let child =
        F.within t "join actual child wrapper after parent claim writer error" (fun () ->
          let rec joined () =
            let current = Warm.settled t.child in
            match current.active_operation with
            | None -> current
            | Some _ ->
              Eio.Fiber.yield ();
              joined ()
          in
          joined ())
      in
      [%test_eq: int] 1 fault.hits;
      let selected = Option.value_exn fault.transaction in
      let receipt = Option.value_exn fault.receipt in
      let delegation = Option.value_exn receipt.delegation in
      let callback =
        List.filter child.moderator_executions ~f:(fun actual ->
          E.equal_phase actual.E.context.phase Turn_end
          && Option.exists actual.context.operation_id ~f:(P.Id.Operation.equal operation))
        |> Shared.one "actual child callback whose parent wrapper exited"
      in
      (match callback.status with
       | Failed _ -> ()
       | Running | Completed _ | Interrupted _ ->
         raise_s [%sexp "child did not retain failed parent policy", (callback : E.t)]);
      let rejected =
        List.filter child.invocations ~f:(fun actual ->
          P.Id.Invocation.equal actual.I.context.id delegation.child_invocation_id)
        |> Shared.one "exact failed child preparation invocation"
      in
      [%test_eq: int] 1 (List.length child.invocations);
      assert (P.Id.Session.equal child.identity.session_id rejected.context.session_id);
      [%test_eq: int] child.identity.generation rejected.context.generation;
      [%test_eq: string] "read_file" rejected.context.tool_name;
      assert (I.equal_origin Moderator rejected.context.origin);
      assert (
        Option.equal
          P.Id.Moderator_execution.equal
          (Some callback.context.id)
          rejected.parent_event);
      let source =
        A.Runtime_builder.moderator_snapshot_observer child.moderator
        |> protocol_ok
        |> Option.value_exn
      in
      assert (I.equal_observer source callback.context.source);
      assert (I.equal_observer source (Option.value_exn rejected.observation).observer);
      assert (
        I.equal_preparation
          Pre_tool_failed
          (Option.value_exn rejected.routing).preparation);
      (* Host preparation failure retains factual negative DATA. The execute
         closure returns this rejection before native dispatch; the handler did
         not run, and no positive parent claim/decision was installed. *)
      assert (
        I.equal_status
          (Resolved
             (Fail
                { code = "invocation.pre_tool_failed"
                ; message = "Host tool preparation failed."
                ; retryable = false
                ; details = `Null
                }))
          rejected.status);
      let server_id = Store.Session_store.server_id (Daemon.store t.daemon) in
      let _, child_source =
        A.Moderator_source_installation.capture child |> protocol_ok
      in
      let _, initial_child_source =
        A.Moderator_source_installation.capture t.child_initial |> protocol_ok
      in
      assert (P.Run_source.equal initial_child_source child_source);
      let child_reference = Option.value_exn child.spec.delegation in
      let record =
        Store.Delegation_store.resolve
          (Store.Session_store.delegations (Daemon.store t.daemon))
          child_reference
        |> Existing.store_ok
      in
      let inherited = Option.value_exn record.admission.goal_origin in
      assert (List.is_empty (Store.Delegation_goal_origin.scopes inherited));
      assert (Option.is_none child.goal_catalog && Option.is_none child.goal_accounts);
      assert (Option.is_none before.goal_catalog && Option.is_none before.goal_accounts);
      checked_seed
        before
        ~server_id
        ~child_source
        ~child_reference
        ~child_invocation:rejected
        selected
      |> ignore;
      assert (Claim.pending t.parent.actor);
      assert_same before (F.state t.parent);
      [%test_eq: int] 0 fault.policy_reads;
      let expected =
        Store.Store_error.Io
          { operation = "append journal segment"
          ; path = Option.value_exn fault.path
          ; message = Exn.to_string (Option.value_exn fault.injected)
          }
        |> Existing_fault.persistence_error
      in
      let first = Claim.drain t.parent.actor |> error in
      Existing_fault.same_error expected first;
      let physical = frames t.env handle in
      let check () =
        assert_same before (F.state t.parent);
        assert (Claim.pending t.parent.actor);
        [%test_eq: (int * string * string) list] physical (frames t.env handle);
        [%test_eq: int] 1 fault.hits;
        [%test_eq: int] 0 fault.policy_reads;
        [%test_eq: int] 0 fault.cache.cache_saves;
        [%test_eq: int] ports !(t.counters.inference_ports)
      in
      check ();
      Existing_fault.same_error first (Owner.unload_and_wait t.parent.runtime |> error);
      check ();
      Existing_fault.same_error first (Claim.drain t.parent.actor |> error);
      Resource_loan.release loan;
      F.within t "actual last unrelated borrower leaves unknown claim" (fun () ->
        Resource_loan.join loan);
      check ();
      Existing_fault.same_error first (close_error t.parent.runtime);
      check ();
      Existing_fault.same_error first (close_error t.parent.runtime);
      check ();
      [%test_eq: int] 1 t.provider.child_calls;
      [%test_eq: int] 0 t.provider.ambient_calls;
      let report =
        `Object
          [ "before", Existing_fault.state_json before
          ; "claim", E.to_json receipt
          ; "server_id", `String (P.Id.Server.to_string server_id)
          ; "child_source", P.Run_source.to_json child_source
          ; "child_reference", Store.Delegation_store.reference_to_jsonaf child_reference
          ; "child_invocation", I.to_json rejected
          ; "initial_snapshot", anchor
          ; "transaction_payload", `String (Store.Transaction.encode selected)
          ; "transaction_sha256", `String (Store.Transaction.hash selected)
          ; "frame_payload", `String (Option.value_exn fault.payload)
          ; "frame_checksum", `String (Option.value_exn fault.checksum)
          ; ( "frames"
            , `String
                (Sexp.to_string_mach [%sexp (physical : (int * string * string) list)]) )
          ; "journal_path", `String (Option.value_exn fault.path)
          ; "injected", `String (Exn.to_string (Option.value_exn fault.injected))
          ; "cleanup", P.Error.to_json first
          ]
      in
      Eio.Flow.copy_string (Jsonaf.to_string report ^ "\n") (Eio.Stdenv.stdout t.env);
      (* Actual wrapper and unrelated loan joined. Unknown writer/holder/physical
         resource did NOT close; process exit is not graceful owner retirement. *)
      Core_unix.exit_immediately 73))
;;

let recover env sw root report =
  let env = (env :> Eio_unix.Stdenv.base) in
  let member name = Jsonaf.member_exn name report in
  let selected =
    member "transaction_payload"
    |> Jsonaf.string_exn
    |> Store.Transaction.decode
    |> Existing.store_ok
  in
  let receipt = member "claim" |> E.of_json |> protocol_ok in
  let before = Callback_retirement_fault_tests.decode_state (member "before") in
  let original_frames =
    member "frames"
    |> Jsonaf.string_exn
    |> Sexp.of_string
    |> [%of_sexp: (int * string * string) list]
  in
  let expected_error =
    Store.Store_error.Io
      { operation = "append journal segment"
      ; path = member "journal_path" |> Jsonaf.string_exn
      ; message = member "injected" |> Jsonaf.string_exn
      }
    |> Existing_fault.persistence_error
  in
  Existing_fault.same_error
    expected_error
    (member "cleanup" |> P.Error.of_json |> protocol_ok);
  assert (E.equal_status receipt.status Running && Option.is_none receipt.decision);
  assert (P.Id.Session.equal before.identity.session_id receipt.context.session_id);
  [%test_eq: int] before.identity.generation receipt.context.generation;
  assert (Option.is_none receipt.context.operation_id);
  assert (Option.is_none receipt.context.job);
  let event_sequence = Int64.succ before.counters.event_sequence in
  [%test_eq: int64 option] (Some event_sequence) selected.first_event_sequence;
  [%test_eq: int64 option] (Some event_sequence) selected.last_event_sequence;
  let source =
    A.Runtime_builder.moderator_snapshot_observer before.moderator
    |> protocol_ok
    |> Option.value_exn
  in
  assert (P.Invocation.equal_observer source receipt.context.source);
  assert (
    E.equal_phase receipt.context.phase Pre_tool_call && Option.is_some receipt.delegation);
  let _, actual, _ = seed_parts selected in
  assert (E.equal receipt actual);
  let server_id =
    member "server_id" |> Jsonaf.string_exn |> P.Id.Server.of_string |> protocol_ok
  in
  let seeded =
    checked_seed
      before
      ~server_id
      ~child_source:(member "child_source" |> P.Run_source.of_json |> protocol_ok)
      ~child_reference:
        (member "child_reference"
         |> Store.Delegation_store.reference_of_jsonaf
         |> Existing.store_ok)
      ~child_invocation:(member "child_invocation" |> I.of_json |> protocol_ok)
      selected
  in
  [%test_eq: int64]
    (Int64.succ before.counters.transaction_sequence)
    selected.transaction_sequence;
  [%test_eq: int64] (Int64.succ before.counters.revision) selected.session_revision;
  [%test_eq: string]
    (member "transaction_sha256" |> Jsonaf.string_exn)
    (Store.Transaction.hash selected);
  assert (
    not
      (List.exists before.moderator_executions ~f:(fun previous ->
         P.Id.Moderator_execution.equal previous.E.context.id receipt.context.id)));
  let expected =
    { seeded with
      identity =
        { seeded.identity with
          updated_at =
            Time_ns.of_int_ns_since_epoch (Int64.to_int_exn selected.accepted_at_ns)
            |> P.Timestamp.of_time_ns
        }
    ; counters =
        { before.counters with
          transaction_sequence = selected.transaction_sequence
        ; revision = selected.session_revision
        ; event_sequence
        }
    }
  in
  assert (
    Jsonaf.exactly_equal
      (A.Session_state.moderator_projection before)
      (A.Session_state.moderator_projection expected));
  let statuses = A.Session_state.extension_status expected in
  assert (
    not
      (List.equal
         P.Extension_status.equal
         (A.Session_state.extension_status before)
         statuses));
  let expected_event =
    P.Event.Durable.of_payload
      ~session_id:expected.identity.session_id
      ~sequence:event_sequence
      ~revision:expected.counters.revision
      ~timestamp:expected.identity.updated_at
      (Session_updated (A.Session_state.summary expected))
    |> fun event -> P.Event.Durable.with_extension_status event statuses
  in
  let actual_event =
    Existing_fault.events selected |> Shared.one "canonical claim status projection event"
  in
  assert (
    Jsonaf.exactly_equal
      (P.Event.Durable.to_json expected_event)
      (P.Event.Durable.to_json actual_event));
  let applied =
    A.Session_persistence.apply_transaction
      ~limits:D.Limits.default
      (A.Session_persistence.Restored.authored before)
      selected
    |> Existing.store_ok
    |> A.Session_persistence.Restored.state
  in
  assert_same expected applied;
  A.Session_state.validate expected |> protocol_ok;
  let store =
    Store.Session_store.open_existing
      ~env
      ~sw
      ~root:(Filename.concat root "data")
      ~process_start_identity:None
      ~lock_nonce:"592-claim-recovery"
    |> Existing.store_ok
  in
  protect_cleanup
    ~finally:(fun () -> Store.Session_store.close store |> Existing.store_ok)
    (fun () ->
       assert (P.Id.Server.equal server_id (Store.Session_store.server_id store));
       let handle =
         Store.Session_store.open_session
           store
           ~sw
           ~actor_lock_nonce:"592-recovered-parent"
           selected.session_id
         |> Existing.store_ok
       in
       protect_cleanup
         ~finally:(fun () ->
           Store.Session_store.close_session store handle |> Existing.store_ok)
         (fun () ->
            assert (
              Jsonaf.exactly_equal
                (member "initial_snapshot")
                (Physical.snapshot_anchor env handle));
            [%test_eq: (int * string * string) list] original_frames (frames env handle);
            let exact =
              List.filter original_frames ~f:(fun (flags, payload, _) ->
                Int.equal flags 0
                && String.equal payload (member "frame_payload" |> Jsonaf.string_exn))
              |> Shared.one "exact unknown parent claim physical frame"
            in
            let _, payload, checksum = exact in
            [%test_eq: string] (member "frame_checksum" |> Jsonaf.string_exn) checksum;
            let actual = Store.Transaction.decode payload |> Existing.store_ok in
            [%test_eq: string]
              (Store.Transaction.encode selected)
              (Store.Transaction.encode actual);
            let all = transactions original_frames in
            let previous =
              List.find_exn all ~f:(fun (actual : Store.Transaction.t) ->
                Int64.equal
                  actual.transaction_sequence
                  before.counters.transaction_sequence)
            in
            [%test_eq: string option]
              (Some (Store.Transaction.hash previous))
              selected.previous_transaction_hash;
            assert (
              List.for_all all ~f:(fun (actual : Store.Transaction.t) ->
                Int64.(actual.transaction_sequence <= selected.transaction_sequence)));
            let snapshot =
              Physical.current_snapshot
                env
                ~directory:(Store.Session_store.Handle.snapshot_directory handle)
            in
            let read =
              Physical.recovery_reader
                env
                handle
                ~session_id:selected.session_id
                ~initial_snapshot:snapshot.snapshot
            in
            let recovered = read () in
            assert (not recovered.repaired_crash_tail);
            [%test_eq: int64]
              selected.transaction_sequence
              recovered.latest_transaction_sequence;
            [%test_eq: int64] selected.session_revision recovered.latest_session_revision;
            [%test_eq: int64] event_sequence recovered.latest_event_sequence;
            [%test_eq: string option]
              (Some (Store.Transaction.hash selected))
              recovered.latest_transaction_hash;
            let after = A.Session_persistence.Restored.state recovered.state in
            assert_same expected after;
            assert (E.equal receipt (Shared.find_receipt after receipt.context.id));
            assert (List.is_empty after.invocations);
            assert (Option.is_none after.active_operation);
            assert_same after (A.Session_persistence.Restored.state (read ()).state);
            [%test_eq: (int * string * string) list] original_frames (frames env handle);
            assert (
              Jsonaf.exactly_equal
                (member "initial_snapshot")
                (Physical.snapshot_anchor env handle))))
;;

let%expect_test "unknown parent claim retains exact holder until physical recovery" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    Eio.Switch.run (fun sw ->
      let root = temporary_root env in
      protect_cleanup
        ~finally:(fun () -> Eio.Path.rmtree Eio.Path.(Eio.Stdenv.fs env / root))
        (fun () ->
           Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 120. (fun () ->
             let report =
               Callback_retirement_fault_tests.child_report
                 ~mode:"delegated-parent-claim-unknown"
                 env
                 root
             in
             recover env sw root report))));
  [%expect {| |}]
;;
