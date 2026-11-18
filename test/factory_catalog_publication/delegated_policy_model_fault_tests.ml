open! Core
open Agent_server_test_support
open Retry_support
module Model = Delegated_policy_model_tests
module Birth = Descendant_goal_birth_tests
module Direct = Descendant_turn_adapter_tests
module Goal = Goal_service_tests
module G = P.Goal
module L = A.Inference_ledger
module PC = A.Policy_canonical_usage
module Cause = A.Policy_cause_data
module Book = A.Goal_policy_contributions
module Store = Agent_store
module Physical = Constructor_model_fault_tests.Physical
module Existing_fault = Legacy_idle_completion_fault_tests
module Frames = Delegated_policy_claim_fault_tests
module Failure = Delegated_policy_caller_support.Failure

let same_state = Frames.assert_same
let within = Model.within
let report_filename = "597-canonical-unknown-report.json"

module Snapshot_anchor = struct
  let load env ~directory ~session =
    let installed = Physical.current_snapshot env ~directory in
    assert (P.Id.Session.equal session installed.snapshot.session_id);
    assert (Int64.(installed.snapshot.transaction_sequence > 0L));
    assert (Option.is_some installed.snapshot.transaction_hash);
    installed
  ;;

  let capture env ~directory ~session =
    let installed = load env ~directory ~session in
    let restored =
      A.Session_persistence.restore_snapshot ~limits:D.Limits.default installed.snapshot
      |> Existing.store_ok
    in
    A.Session_persistence.validate restored |> Existing.store_ok;
    let current = A.Session_persistence.Restored.state restored in
    assert (P.Id.Session.equal session current.identity.session_id);
    [%test_eq: int64]
      installed.snapshot.transaction_sequence
      current.counters.transaction_sequence;
    [%test_eq: int64] installed.snapshot.event_sequence current.counters.event_sequence;
    let metadata =
      Store.Snapshot.Stored.metadata (Store.Snapshot.stored installed.snapshot)
    in
    [%test_eq: int64] metadata.session_revision current.counters.revision;
    [%test_eq: int64] metadata.generation (Int64.of_int current.identity.generation);
    installed
  ;;

  let json (installed : Store.Snapshot.installed) =
    let stored = Store.Snapshot.stored installed.snapshot in
    let metadata = Store.Snapshot.Stored.metadata stored in
    `Object
      [ ( "digest"
        , `String
            (Store.Snapshot.Stored.record stored |> Store.Document_record.stored_digest) )
      ; "filename", `String installed.filename
      ; "transaction_sequence", `String (Int64.to_string metadata.transaction_sequence)
      ; "transaction_hash", `String (Option.value_exn metadata.transaction_hash)
      ; "revision", `String (Int64.to_string metadata.session_revision)
      ; "event_sequence", `String (Int64.to_string metadata.event_sequence)
      ; "generation", `String (Int64.to_string metadata.generation)
      ]
  ;;

  let check_before
        (installed : Store.Snapshot.installed)
        ~(before : A.Session_state.t)
        ~(expected : A.Session_state.t)
        ~(transaction : Store.Transaction.t)
        ~physical
    =
    let snapshot = installed.snapshot in
    let metadata = Store.Snapshot.Stored.metadata (Store.Snapshot.stored snapshot) in
    assert (P.Id.Session.equal before.identity.session_id snapshot.session_id);
    assert (Int64.(snapshot.transaction_sequence <= before.counters.transaction_sequence));
    assert (Int64.(metadata.session_revision <= before.counters.revision));
    assert (Int64.(snapshot.event_sequence <= before.counters.event_sequence));
    let transactions = Frames.transactions physical in
    let counters =
      Store.Recovery.validate_retained
        ~session_id:before.identity.session_id
        ~snapshots:[ installed ]
        ~transactions
      |> Existing.store_ok
    in
    [%test_eq: int64] expected.counters.transaction_sequence counters.transaction_sequence;
    [%test_eq: int64] expected.counters.revision counters.session_revision;
    [%test_eq: int64] expected.counters.event_sequence counters.event_sequence;
    [%test_eq: int] expected.identity.generation counters.generation;
    [%test_eq: string option]
      (Some (Store.Transaction.hash transaction))
      counters.transaction_hash;
    let previous =
      List.filter transactions ~f:(fun (tx : Store.Transaction.t) ->
        Int64.equal tx.transaction_sequence before.counters.transaction_sequence)
      |> Model.only "actual before transaction in canonical chain"
    in
    [%test_eq: string option]
      (Some (Store.Transaction.hash previous))
      transaction.previous_transaction_hash;
    [%test_eq: int64] before.counters.revision previous.session_revision;
    [%test_eq: int] before.identity.generation previous.generation
  ;;
end

module Fault = struct
  type selected =
    { transaction : Store.Transaction.t
    ; snapshot : Store.Snapshot.installed
    ; ledger : L.t
    ; policy : PC.t
    ; handle : L.Handle.t
    ; payload : string
    ; checksum : string
    ; path : string
    ; injected : exn
    }

  type t =
    { mutable armed : (P.Id.Session.t * string * string) option
    ; mutable selected : selected option
    ; entered : unit Eio.Promise.t
    ; resolver : unit Eio.Promise.u
    ; mutable hits : int
    }

  let create () =
    let entered, resolver = Eio.Promise.create () in
    { armed = None; selected = None; entered; resolver; hits = 0 }
  ;;

  let canonical transaction =
    Physical.changes transaction
    |> List.find_map ~f:(function
      | A.Session_delta.Policy_inference_ledger_changed (ledger, _, update)
        when Option.is_some (PC.Update.selection update) ->
        let policy = PC.Update.candidate update in
        let binding =
          PC.bindings policy
          |> List.max_elt ~compare:(fun left right ->
            Int64.compare (PC.Binding.ordinal left) (PC.Binding.ordinal right))
          |> Option.value_exn
        in
        let row =
          L.find ledger ~ordinal:(PC.Binding.ordinal binding) |> Option.value_exn
        in
        Some (ledger, policy, L.Row.handle row)
      | _ -> None)
  ;;

  let wrap t env =
    Crash_fault_io.wrap
      env
      ~boundary:After_sync
      ~matches:(fun path ->
        match t.armed with
        | None -> false
        | Some (_, journal, _) ->
          String.is_prefix path ~prefix:journal && String.is_suffix path ~suffix:".log")
      ~reached:(fun path ->
        match t.armed with
        | None -> ()
        | Some (session, _, snapshot_directory) ->
          let scan =
            Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path)
            |> Store.Journal_segment.scan_contents
                 ~max_payload_length:Physical.max_payload_length
            |> Existing.store_ok
          in
          (match List.last scan.entries with
           | Some entry when Int.equal (Store.Frame.flags entry.frame) 0 ->
             let payload = Store.Frame.payload entry.frame in
             let transaction = Store.Transaction.decode payload |> Existing.store_ok in
             if P.Id.Session.equal session transaction.session_id
             then
               Option.iter (canonical transaction) ~f:(fun (ledger, policy, handle) ->
                 let snapshot =
                   Snapshot_anchor.capture env ~directory:snapshot_directory ~session
                 in
                 t.armed <- None;
                 t.hits <- t.hits + 1;
                 let injected =
                   Core_unix.Unix_error (EIO, "597 canonical Model after sync", path)
                 in
                 t.selected
                 <- Some
                      { transaction
                      ; snapshot
                      ; ledger
                      ; policy
                      ; handle
                      ; payload
                      ; checksum = Store.Frame.checksum_hex entry.frame
                      ; path
                      ; injected
                      };
                 Eio.Promise.resolve t.resolver ();
                 raise injected)
           | Some _ | None -> ()))
  ;;
end

let rejected = function
  | Error failure -> failure
  | Ok _ -> failwith "unknown canonical writer permitted a later writer or cleanup"
;;

(* This is the same public Goal-born child and real local Goal activation as the
   positive/cap trajectory. The sole cut is the parent canonical H transaction,
   after the Running Model Job claim and before any policy provider call. *)
let run_child root =
  let fault = Fault.create () in
  let failure = Failure.create () in
  let child_calls = ref 0 in
  let model_calls = ref 0 in
  let handoff, release = Eio.Promise.create () in
  let release_once () =
    if not (Eio.Promise.is_resolved handoff) then Eio.Promise.resolve release ()
  in
  let provider inputs =
    Failure.guard failure (fun () ->
      let text = `Array (List.map inputs ~f:Res.Item.jsonaf_of_t) |> Jsonaf.to_string in
      if
        String.is_substring text ~substring:Model.first_marker
        || String.is_substring text ~substring:Model.second_marker
      then (
        Int.incr model_calls;
        failwith "unknown canonical Model reached its provider")
      else if String.is_substring text ~substring:Model.child_marker
      then (
        Int.incr child_calls;
        Eio.Promise.await handoff;
        Direct.function_response
          "read_file"
          (`Object [ "root", `String "data"; "file", `String "597-effect.txt" ]))
      else completed_response "597 actual parent completed native child birth")
  in
  Birth.with_native_goal_child
    ~test_root:root
    ~timeout:90.
    ~wrap_env:(Fault.wrap fault)
    ~parent_prompt_text:Model.parent_prompt
    ~sources:
      [ Model.recipe, "<developer>597 actual captured policy model recipe.</developer>"
      ; "597-effect.txt", "597 real inherited effect"
      ]
    ~inherited_tools:[ "read_file", Descendant_goal_resume_tests.parent_reader ]
    ~child_sources:[ "child.chatmd", Model.child_prompt ]
    ~post_creation_stream:provider
    (fun env _sw daemon parent child _scope _reference counters ->
       Exn.protect ~finally:release_once ~f:(fun () ->
         Failure.restore failure (fun () ->
           within env "install actual child source" (fun () ->
             Owner.ensure_loaded_authenticated child.runtime ~actor:original
             |> protocol_ok);
           within env "join actual child lifecycle" (fun () ->
             Direct.start_child daemon child);
           let session, created = Goal.create_goal daemon child in
           let before_child = state child in
           assert (G.equal (Goal.goal created) (Goal.goal before_child));
           let request =
             A.Run_admission_request.Headless.create
               ~session_id:before_child.identity.session_id
               ~generation:before_child.identity.generation
               ~expected_revision:before_child.counters.revision
               ~mode:Workflow
               ~input:
                 (User_submission
                    { kind = Plain_text; text = Model.child_marker; attachments = [] })
               ~key:(P.Idempotency_key.of_string "597-unknown-child-goal" |> protocol_ok)
             |> protocol_ok
             |> fun request ->
             A.Goal_activation_request.create
               ~goal_id:Goal.goal_id
               ~expected_goal_revision:(G.revision (Goal.goal before_child))
               ~expected_catalog_revision:
                 (A.Goal_catalog.revision (Option.value_exn before_child.goal_catalog))
               ~request
             |> protocol_ok
           in
           let store_handle = Option.value_exn parent.store_handle in
           let writer, _ =
             Actor.attach parent.actor ~mode:Read_write ~subscribe:false |> protocol_ok
           in
           let providers_before = !(counters.providers) in
           fault.armed
           <- Some
                ( (state parent).identity.session_id
                , Store.Session_store.Handle.journal_directory store_handle
                , Store.Session_store.Handle.snapshot_directory store_handle );
           Agent_server.Goal_run_service.create
             ~factory:(Daemon.factory daemon)
             ~authorize:(fun _ -> authorize)
           |> fun service ->
           Agent_server.Goal_run_service.start_episode
             service
             ~entry:child
             ~actor:original
             ~session
             ~request
           |> protocol_ok
           |> Goal.activated
           |> ignore;
           release_once ();
           within env "actual canonical H writer cut" (fun () ->
             Eio.Promise.await fault.entered);
           let selected = Option.value_exn fault.selected in
           let before = state parent in
           [%test_eq: int] 1 fault.hits;
           [%test_eq: int] 1 !child_calls;
           [%test_eq: int] 0 !model_calls;
           [%test_eq: int] (providers_before + 1) !(counters.providers);
           [%test_eq: int64]
             (Int64.succ before.counters.transaction_sequence)
             selected.transaction.transaction_sequence;
           [%test_eq: int64]
             (Int64.succ before.counters.revision)
             selected.transaction.session_revision;
           assert (
             Option.is_none
               (L.find
                  before.inference_ledger
                  ~ordinal:(L.Handle.ordinal selected.handle)));
           let job = Model.job_named before Model.first_marker in
           [%test_eq: P.Job.kind] Model_call job.kind;
           (match job.status with
            | Running -> ()
            | Queued
            | Waiting_permission _
            | Waiting_completion _
            | Succeeded
            | Failed _
            | Cancelled
            | Interrupted _ ->
              failwith "canonical cut did not follow the actual Running Model Job claim");
           assert (Option.is_none job.result && Option.is_none job.completed_at);
           let binding =
             PC.bindings selected.policy
             |> List.filter ~f:(fun binding ->
               Int64.equal (PC.Binding.ordinal binding) (L.Handle.ordinal selected.handle))
             |> Model.only "selected canonical binding"
           in
           let cause = Model.cause_for selected.policy binding in
           assert (
             P.Session_ref.equal
               (A.Policy_cause_data.caller cause)
               (G.session (Goal.goal before_child)));
           let expected =
             A.Session_persistence.apply_transaction
               ~limits:D.Limits.default
               (A.Session_persistence.Restored.authored before)
               selected.transaction
             |> Existing.store_ok
             |> A.Session_persistence.Restored.state
           in
           assert (
             List.equal
               (fun left right ->
                  Sexp.equal (P.Job.sexp_of_t left) (P.Job.sexp_of_t right))
               before.jobs
               expected.jobs);
           assert (
             List.equal
               P.Moderator_execution.equal
               before.moderator_executions
               expected.moderator_executions);
           assert (
             List.equal
               P.History.equal_entry
               before.conversation.canonical_history
               expected.conversation.canonical_history);
           let projections =
             PC.projections
               selected.policy
               ~ledger:selected.ledger
               ~session_revision:expected.counters.revision
             |> protocol_ok
           in
           let heads = List.filter_map projections ~f:PC.Projection.head in
           assert (not (List.is_empty heads));
           List.iter heads ~f:(fun head ->
             match PC.Head.work head with
             | Model_job_attempt { id; attempt } ->
               assert (P.Id.Job.equal id job.id);
               [%test_eq: int] job.attempt attempt
             | Event _ | Invocation _ | Async_job_attempt _ ->
               failwith "selected canonical frame was not the actual Model Job attempt");
           let primary =
             Store.Store_error.Io
               { operation = "append journal segment"
               ; path = selected.path
               ; message = Exn.to_string selected.injected
               }
             |> Existing_fault.persistence_error
           in
           let physical = Frames.frames env store_handle in
           Snapshot_anchor.check_before
             selected.snapshot
             ~before
             ~expected
             ~transaction:selected.transaction
             ~physical;
           let fault_snapshot = Snapshot_anchor.json selected.snapshot in
           let unchanged () =
             same_state before (state parent);
             [%test_eq: (int * string * string) list]
               physical
               (Frames.frames env store_handle);
             assert (
               Jsonaf.exactly_equal
                 fault_snapshot
                 (Snapshot_anchor.load
                    env
                    ~directory:
                      (Store.Session_store.Handle.snapshot_directory store_handle)
                    ~session:before.identity.session_id
                  |> Snapshot_anchor.json));
             [%test_eq: int] 1 fault.hits;
             [%test_eq: int] 0 !model_calls;
             [%test_eq: int] (providers_before + 1) !(counters.providers);
             Failure.raise_if_present failure
           in
           let metadata_probe () =
             let patch =
               P.Session_metadata.Patch.create
                 ~name:(Set "597 canonical unknown must fence this metadata")
                 ~set_labels:[]
                 ~remove_labels:[]
               |> protocol_ok
             in
             Actor.update_metadata
               parent.actor
               ~attachment_id:writer.id
               ~expected_metadata_revision:before.identity.metadata_revision
               ~patch
               ()
             |> rejected
             |> Existing_fault.same_error primary;
             unchanged ()
           in
           within env "exact original failure fences unrelated metadata" metadata_probe;
           within env "retained unknown cleanup" (fun () ->
             Actor.drain_descendant_cleanup parent.actor
             |> rejected
             |> Existing_fault.same_error primary);
           unchanged ();
           within env "same kernel remains fenced after cleanup" metadata_probe;
           let report =
             `Object
               [ "before", Existing_fault.state_json before
               ; "remote_at_fault", Existing_fault.state_json (state child)
               ; "expected", Existing_fault.state_json expected
               ; "fault_snapshot", fault_snapshot
               ; ( "transaction_payload"
                 , `String (Store.Transaction.encode selected.transaction) )
               ; ( "transaction_sha256"
                 , `String (Store.Transaction.hash selected.transaction) )
               ; "frame_payload", `String selected.payload
               ; "frame_checksum", `String selected.checksum
               ; ( "frames"
                 , `String
                     (Sexp.to_string_mach
                        [%sexp (physical : (int * string * string) list)]) )
               ; "primary", P.Error.to_json primary
               ; "journal_path", `String selected.path
               ; "injected", `String (Exn.to_string selected.injected)
               ]
           in
           let bytes = Jsonaf.to_string report in
           Eio.Path.save
             ~create:(`Exclusive 0o600)
             Eio.Path.(Eio.Stdenv.fs env / root / report_filename)
             bytes;
           let descriptor =
             `Object
               [ "bytes", `String (Int.to_string (String.length bytes))
               ; "sha256", `String Digestif.SHA256.(to_hex (digest_string bytes))
               ]
           in
           Eio.Flow.copy_string
             (Jsonaf.to_string descriptor ^ "\n")
             (Eio.Stdenv.stdout env);
           (* The unknown writer and exact Plan remain retained. This process
              exit is not a successful graceful close or permission to replay. *)
           Core_unix.exit_immediately 73)))
;;

(* Recovered projections and pending values are DATA only. This oracle never
   constructs an Actor ACK, starts a runtime or writes an uncommitted snapshot. *)
let recovered_pending current stream =
  let payload =
    Option.value_exn current.A.Session_state.goal_policy_contributions
    |> Book.to_document
    |> protocol_ok
    |> D.Document.payload
  in
  Jsonaf.member_exn "books" payload
  |> Jsonaf.list_exn
  |> List.concat_map ~f:(fun book -> Jsonaf.member_exn "slots" book |> Jsonaf.list_exn)
  |> List.filter ~f:(fun slot ->
    let actual = Jsonaf.member_exn "stream" slot |> PC.Stream.of_json |> protocol_ok in
    PC.Stream.equal stream actual)
  |> Model.only "actual recovered remote stream"
  |> Jsonaf.member_exn "pending"
  |> Book.Pending.of_json
  |> protocol_ok
;;

let recover_remote_facts env sw store report source selected =
  let accounting = Option.value_exn source.A.Session_state.policy_canonical_usage in
  let binding =
    PC.bindings accounting
    |> List.filter ~f:(fun binding ->
      Int64.equal (PC.Binding.ordinal binding) (L.Handle.ordinal selected))
    |> Model.only "actual recovered canonical binding"
  in
  let cause = Model.cause_for accounting binding in
  let reported =
    Jsonaf.member_exn "remote_at_fault" report
    |> Callback_retirement_fault_tests.decode_state
  in
  let remote_session = G.session (Goal.goal reported) in
  assert (P.Session_ref.equal remote_session (Cause.caller cause));
  [%test_eq: P.Id.Server.t]
    (Store.Session_store.server_id store)
    (P.Session_ref.server_id remote_session);
  let projection =
    PC.projections
      accounting
      ~ledger:source.inference_ledger
      ~session_revision:source.counters.revision
    |> protocol_ok
    |> List.filter ~f:(fun projection ->
      P.Session_ref.equal
        remote_session
        (Cause.Goal_target.owner (PC.Stream.target (PC.Projection.stream projection))))
    |> Model.only "actual recovered remote Goal projection"
  in
  let stream = PC.Projection.stream projection in
  assert (not (P.Session_ref.equal remote_session (PC.Stream.callee stream)));
  let reported_pending = recovered_pending reported stream in
  let handle =
    Store.Session_store.open_session
      store
      ~sw
      ~actor_lock_nonce:"597-canonical-recovered-remote"
      (P.Session_ref.session_id remote_session)
    |> Existing.store_ok
  in
  protect_cleanup
    ~finally:(fun () ->
      Store.Session_store.close_session store handle |> Existing.store_ok)
    (fun () ->
       let snapshot_directory = Store.Session_store.Handle.snapshot_directory handle in
       let snapshot =
         Snapshot_anchor.load
           env
           ~directory:snapshot_directory
           ~session:(P.Session_ref.session_id remote_session)
       in
       let read =
         Physical.recovery_reader
           env
           handle
           ~session_id:(P.Session_ref.session_id remote_session)
           ~initial_snapshot:snapshot.snapshot
       in
       (* A genuine child crash-tail repair, if needed, belongs to this first
         physical recovery. The subsequent DATA-only oracle must change no disk. *)
       let recovered = read () in
       let restored = recovered.state in
       let remote = A.Session_persistence.Restored.state restored in
       let pending = recovered_pending remote stream in
       assert (Book.Pending.equal reported_pending pending);
       assert (Cause.Key.equal (Cause.key cause) (Book.Pending.cause pending));
       let head = PC.Projection.head projection |> Option.value_exn in
       assert (
         A.Goal_descendant_occurrence.equal
           (Book.Pending.occurrence pending)
           (A.Goal_descendants.Canonical.Fact.occurrence (PC.Head.canonical head)));
       assert (Cause.Allocation_work.equal (Book.Pending.work pending) (PC.Head.work head));
       [%test_eq: int64]
         (Book.Pending.next_prefix pending)
         (PC.Projection.admitted_attempts projection);
       let physical = Frames.frames env handle in
       let snapshot =
         Snapshot_anchor.load
           env
           ~directory:snapshot_directory
           ~session:(P.Session_ref.session_id remote_session)
         |> Snapshot_anchor.json
       in
       let admit document =
         let previous = A.Session_state_document.value document in
         let update, disposition =
           Book.import (Option.value_exn previous.goal_policy_contributions) ~projection
           |> protocol_ok
         in
         let delta = A.Session_delta.Goal_policy_contributions_changed update in
         let transition =
           A.Session_transition.apply
             ~now:previous.identity.updated_at
             previous
             ~delta
             ~payloads:[]
           |> protocol_ok
         in
         let admitted =
           A.Session_document_transition.admit_encoded
             document
             ~delta
             ~next:transition.state
             ~limits:D.Limits.default
           |> document_ok
           |> A.Session_state_document.Admitted.state_document
         in
         A.Session_state.validate (A.Session_state_document.value admitted) |> protocol_ok;
         admitted, disposition
       in
       let first, disposition =
         admit (A.Session_persistence.Restored.state_document restored)
       in
       (match disposition with
        | Added | Replaced -> ()
        | Duplicate | Stale ->
          failwith "recovered next-prefix import did not consume pending");
       let imported = A.Session_state_document.value first in
       assert (List.is_empty (Book.Subtotal.pending (Model.subtotal imported)));
       [%test_eq: int64] (Int64.succ (Model.committed remote)) (Model.committed imported);
       let second, disposition = admit first in
       assert (Book.Disposition.equal Duplicate disposition);
       let repeated = A.Session_state_document.value second in
       let book_json current =
         Option.value_exn current.A.Session_state.goal_policy_contributions
         |> Book.to_document
         |> protocol_ok
         |> D.Document.json
       in
       assert (Jsonaf.exactly_equal (book_json imported) (book_json repeated));
       [%test_eq: int64] (Model.committed imported) (Model.committed repeated);
       assert (List.is_empty (Book.Subtotal.pending (Model.subtotal repeated)));
       same_state remote (A.Session_persistence.Restored.state (read ()).state);
       [%test_eq: (int * string * string) list] physical (Frames.frames env handle);
       assert (
         Jsonaf.exactly_equal
           snapshot
           (Snapshot_anchor.load
              env
              ~directory:snapshot_directory
              ~session:(P.Session_ref.session_id remote_session)
            |> Snapshot_anchor.json)))
;;

let recover env sw root report =
  let env = (env :> Eio_unix.Stdenv.base) in
  let member name = Jsonaf.member_exn name report in
  let before = Callback_retirement_fault_tests.decode_state (member "before") in
  let expected = Callback_retirement_fault_tests.decode_state (member "expected") in
  let selected =
    member "transaction_payload"
    |> Jsonaf.string_exn
    |> Store.Transaction.decode
    |> Existing.store_ok
  in
  let physical =
    member "frames"
    |> Jsonaf.string_exn
    |> Sexp.of_string
    |> [%of_sexp: (int * string * string) list]
  in
  let primary =
    Store.Store_error.Io
      { operation = "append journal segment"
      ; path = member "journal_path" |> Jsonaf.string_exn
      ; message = member "injected" |> Jsonaf.string_exn
      }
    |> Existing_fault.persistence_error
  in
  Existing_fault.same_error primary (member "primary" |> P.Error.of_json |> protocol_ok);
  let applied =
    A.Session_persistence.apply_transaction
      ~limits:D.Limits.default
      (A.Session_persistence.Restored.authored before)
      selected
    |> Existing.store_ok
    |> A.Session_persistence.Restored.state
  in
  same_state expected applied;
  [%test_eq: int] 1 (List.length (Model.new_rows before expected));
  [%test_eq: int64] (Int64.succ (Model.attempts before)) (Model.attempts expected);
  let store =
    Store.Session_store.open_existing
      ~env
      ~sw
      ~root:(Filename.concat root "data")
      ~process_start_identity:None
      ~lock_nonce:"597-canonical-recovery"
    |> Existing.store_ok
  in
  protect_cleanup
    ~finally:(fun () -> Store.Session_store.close store |> Existing.store_ok)
    (fun () ->
       let handle =
         Store.Session_store.open_session
           store
           ~sw
           ~actor_lock_nonce:"597-canonical-recovered-parent"
           selected.session_id
         |> Existing.store_ok
       in
       protect_cleanup
         ~finally:(fun () ->
           Store.Session_store.close_session store handle |> Existing.store_ok)
         (fun () ->
            let unchanged () =
              [%test_eq: (int * string * string) list] physical (Frames.frames env handle);
              assert (
                Jsonaf.exactly_equal
                  (member "fault_snapshot")
                  (Snapshot_anchor.load
                     env
                     ~directory:(Store.Session_store.Handle.snapshot_directory handle)
                     ~session:selected.session_id
                   |> Snapshot_anchor.json))
            in
            unchanged ();
            let _, payload, checksum =
              List.filter physical ~f:(fun (flags, payload, _) ->
                Int.equal flags 0
                && String.equal payload (member "frame_payload" |> Jsonaf.string_exn))
              |> Model.only "one exact physical canonical H frame"
            in
            [%test_eq: string] (member "frame_checksum" |> Jsonaf.string_exn) checksum;
            [%test_eq: string]
              (Store.Transaction.encode selected)
              (Store.Transaction.decode payload
               |> Existing.store_ok
               |> Store.Transaction.encode);
            [%test_eq: string]
              (member "transaction_sha256" |> Jsonaf.string_exn)
              (Store.Transaction.hash selected);
            let transactions = Frames.transactions physical in
            let previous =
              List.find_exn transactions ~f:(fun (tx : Store.Transaction.t) ->
                Int64.equal tx.transaction_sequence before.counters.transaction_sequence)
            in
            [%test_eq: string option]
              (Some (Store.Transaction.hash previous))
              selected.previous_transaction_hash;
            assert (
              List.for_all transactions ~f:(fun (tx : Store.Transaction.t) ->
                Int64.(tx.transaction_sequence <= selected.transaction_sequence)));
            let snapshot_directory =
              Store.Session_store.Handle.snapshot_directory handle
            in
            let snapshot =
              Snapshot_anchor.load
                env
                ~directory:snapshot_directory
                ~session:selected.session_id
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
            [%test_eq: int64]
              expected.counters.event_sequence
              recovered.latest_event_sequence;
            [%test_eq: string option]
              (Some (Store.Transaction.hash selected))
              recovered.latest_transaction_hash;
            same_state expected (A.Session_persistence.Restored.state recovered.state);
            let source = A.Session_persistence.Restored.state recovered.state in
            [%test_eq: int64]
              (Int64.succ (Model.committed before))
              (Model.committed source);
            let handle =
              Model.new_rows before source
              |> Model.only "same recovered canonical H"
              |> L.Row.handle
            in
            recover_remote_facts env sw store report source handle;
            let reread = A.Session_persistence.Restored.state (read ()).state in
            same_state expected reread;
            [%test_eq: int64] (Model.committed source) (Model.committed reread);
            unchanged ()))
;;

let%expect_test "canonical policy Model unknown fences unrelated writers before provider" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    Eio.Switch.run (fun sw ->
      let root = temporary_root env in
      protect_cleanup
        ~finally:(fun () -> Eio.Path.rmtree Eio.Path.(Eio.Stdenv.fs env / root))
        (fun () ->
           Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 120. (fun () ->
             let descriptor, diagnostics =
               Callback_retirement_fault_tests.completed_child_output
                 ~mode:"delegated-policy-canonical-unknown"
                 env
                 root
             in
             let descriptor = Jsonaf.of_string descriptor in
             let bytes =
               Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / root / report_filename)
             in
             [%test_eq: int]
               (Jsonaf.member_exn "bytes" descriptor |> Jsonaf.string_exn |> Int.of_string)
               (String.length bytes);
             [%test_eq: string]
               (Jsonaf.member_exn "sha256" descriptor |> Jsonaf.string_exn)
               Digestif.SHA256.(to_hex (digest_string bytes));
             let report = Jsonaf.of_string bytes in
             let primary =
               Store.Store_error.Io
                 { operation = "append journal segment"
                 ; path = Jsonaf.member_exn "journal_path" report |> Jsonaf.string_exn
                 ; message = Jsonaf.member_exn "injected" report |> Jsonaf.string_exn
                 }
               |> Existing_fault.persistence_error
             in
             Existing_fault.same_error
               primary
               (Jsonaf.member_exn "primary" report |> P.Error.of_json |> protocol_ok);
             let expected_diagnostics =
               "queued moderator cleanup failed: " ^ primary.message
               |> String.split ~on:'\n'
               |> List.map ~f:(fun line -> "+" ^ line ^ "\n")
               |> String.concat
             in
             (* The callback wrapper can emit this one secondary cleanup trace
                after its Actor reply. Hard exit does not join that wrapper, so
                the trace may still be pending. Extra or different bytes fail. *)
             if not (String.is_empty diagnostics)
             then [%test_eq: string] expected_diagnostics diagnostics;
             recover env sw root report))));
  [%expect {| |}]
;;
