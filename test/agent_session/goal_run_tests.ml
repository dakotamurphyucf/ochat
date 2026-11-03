open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module G = P.Goal
module C = A.Goal_catalog
module D = Document_schema

let set = Goal_catalog_tests.set
let member = Goal_catalog_tests.member
let reason = G.Reason.of_string "Exact completion evidence" |> protocol_ok
let action : P.Run_action.t = Finish { terminal = Completed None; relinquish = [] }
let goal_id = P.Id.Goal.of_string "goal_run_boundary" |> protocol_ok

let digest json =
  Jsonaf.to_string json |> Digestif.SHA256.digest_string |> Digestif.SHA256.to_hex
;;

(* Pure transition/model fixture only. The actual existing source/scope comes
   from the normal actor fixture; Enabled metadata here is an explicit imported
   projection, not a controller command or permission to execute. Full authored
   Goal.finish/worker admission qualification remains the combined475/476 gate. *)
let with_model ?after_close f =
  Run_admission_tests.with_startup_actor
    ?after_close
    (fun actor _ _ request _ _ scope session _ ->
       let initial = A.Session_actor.state actor |> protocol_ok in
       let index, source = A.Goal_catalog_source.capture initial |> protocol_ok in
       let create =
         A.Goal_command.create
           ~session_id
           ~generation:initial.identity.generation
           ~source:(Some source)
           ~expected_catalog_revision:0L
           ~expected_goal_revision:None
           ~key:(P.Idempotency_key.of_string "goal-run-model" |> protocol_ok)
           ~mutation:
             (Create
                { id = goal_id
                ; objective =
                    G.Objective.of_string "Qualify goal settlement" |> protocol_ok
                ; budget = G.Budget.unlimited
                })
         |> protocol_ok
       in
       let held =
         A.Goal_command_transition.prepare
           C.empty
           ~request:create
           ~principal_id
           ~session
           ~generation:initial.identity.generation
           ~current_source:(Some source)
           ~now:timestamp
         |> Goal_catalog_tests.committed
         |> fst
       in
       let enabled = Goal_catalog_tests.enable_fixture held goal_id in
       let basis = { initial with run_state = Some index; goal_catalog = Some enabled } in
       let request = request "goal-run-model-admission" in
       let admission =
         A.Run_admission.prepare
           basis
           ~scope
           ~request
           ~session
           ~run_id:(P.Id.Run.of_string "run_goal_boundary" |> protocol_ok)
           ~operation:None
           ~request_sha256:(digest (P.Run_start.to_json request))
           ~now:timestamp
         |> protocol_ok
       in
       let intent =
         A.Goal_episode_admission.intent
           ~goal_id
           ~expected_goal_revision:
             (C.find enabled goal_id |> Option.value_exn |> G.revision)
           ~expected_catalog_revision:(C.revision enabled)
         |> protocol_ok
       in
       let associated =
         A.Goal_episode_admission.prepare basis ~admission ~intent |> protocol_ok
       in
       let delta =
         A.Session_delta.Batch
           [ Run_state_changed (A.Goal_episode_admission.run_state associated)
           ; Goal_catalog_changed (A.Goal_episode_admission.catalog associated)
           ]
       in
       let committed =
         A.Session_transition.apply ~now:timestamp basis ~delta ~payloads:[]
         |> protocol_ok
       in
       f ~basis ~held ~admission ~intent ~delta committed.state)
;;

let current_goal state =
  C.find (Option.value_exn state.A.Session_state.goal_catalog) goal_id |> Option.value_exn
;;

let rebuild (previous : P.Run.t) ~lifecycle =
  P.Run.create
    ~id:previous.id
    ~session:previous.session
    ~principal_id:previous.principal_id
    ~source:previous.source
    ~mode:previous.mode
    ~lifecycle
    ~revision:(Int64.succ previous.revision)
    ~owned_work:previous.owned_work
    ~relinquished_work:previous.relinquished_work
    ~terminal_work:previous.terminal_work
    ~created_at:previous.created_at
    ~updated_at:timestamp
  |> protocol_ok
;;

let receipt state (run : P.Run.t) kind json key =
  P.Run_receipt.create
    ~run_id:run.id
    ~principal_id:run.principal_id
    ~source:run.source
    ~key:(P.Idempotency_key.of_string key |> protocol_ok)
    ~request_sha256:(digest json)
    ~kind
    ~run_revision:run.revision
    ~session_revision:(Int64.succ state.A.Session_state.counters.revision)
    ~committed_at:timestamp
  |> protocol_ok
;;

let request_achievement state =
  let index = Option.value_exn state.A.Session_state.run_state in
  let admitted = List.hd_exn (A.Run_state.runs index) in
  let active = rebuild admitted ~lifecycle:Active in
  let receipt =
    receipt state active Action (P.Run_action.to_json action) "model-action"
  in
  let execution_id =
    P.Id.Moderator_execution.of_string "mex_goal_boundary" |> protocol_ok
  in
  let intent = A.Run_intent.create ~receipt ~execution_id ~action |> protocol_ok in
  let index =
    A.Run_state.commit index ~run:active ~receipt ~intent:(Some intent) |> protocol_ok
  in
  let goal = current_goal state in
  let request =
    P.Goal_achievement.Request.create
      ~episode:(G.episode goal |> Option.value_exn)
      ~run:active
      ~receipt
      ~action
      ~evidence:reason
    |> protocol_ok
  in
  let incoming = G.request_achievement goal request |> protocol_ok in
  let catalog = Option.value_exn state.goal_catalog in
  let document = C.document catalog goal_id |> Option.value_exn in
  let document = A.Goal_document.update document incoming |> document_ok in
  let catalog = C.advance_existing catalog ~goal:document |> protocol_ok in
  A.Session_transition.apply
    ~now:timestamp
    state
    ~delta:(Batch [ Run_state_changed index; Goal_catalog_changed catalog ])
    ~payloads:[]
  |> protocol_ok
  |> fun (transition : A.Session_transition.t) -> transition.state
;;

let terminal_delta state terminal =
  let index = Option.value_exn state.A.Session_state.run_state in
  let old = List.hd_exn (A.Run_state.runs index) in
  let run = rebuild old ~lifecycle:(Terminal terminal) in
  let receipt =
    receipt state run Terminal (P.Run.Terminal.to_json terminal) "model-terminal"
  in
  let index = A.Run_state.commit index ~run ~receipt ~intent:None |> protocol_ok in
  let intents =
    List.map (A.Run_state.intents index) ~f:(fun intent ->
      match intent.A.Run_intent.disposition with
      | Pending -> A.Run_intent.consume intent ~operation_id:None |> protocol_ok
      | Consumed _ | Retired -> intent)
  in
  let index = A.Run_state.advance index ~run ~intents |> protocol_ok in
  A.Session_delta.Run_state_changed index
;;

let%expect_test
    "pure association requires Enabled fresh admission and reciprocal immutable custody"
  =
  with_model (fun ~basis ~held ~admission ~intent ~delta state ->
    assert (
      Result.is_error
        (A.Goal_episode_admission.prepare
           { basis with goal_catalog = Some held }
           ~admission
           ~intent));
    let linked = Option.value_exn state.run_state in
    let unlinked = A.Run_admission.index admission in
    assert (Result.is_error (A.Run_state.validate_transition ~previous:unlinked linked));
    let foreign =
      A.Run_goal_link.create
        ~goal_id:(P.Id.Goal.of_string "goal_foreign" |> protocol_ok)
        ~episode:(G.episode (current_goal state) |> Option.value_exn)
        ~intent_sha256:(A.Goal_episode_admission.digest intent)
      |> protocol_ok
    in
    assert (Result.is_error (A.Run_state.bind_goal linked foreign));
    let deeply_nested =
      List.fold (List.init 512 ~f:Fn.id) ~init:`Null ~f:(fun json _ -> `Array [ json ])
    in
    assert (Result.is_error (A.Run_goal_links.of_field (Some deeply_nested)));
    assert (
      Result.is_error
        (A.Run_goal_link.of_jsonaf
           (set (A.Run_goal_link.to_jsonaf foreign) "future" deeply_nested)));
    List.iter
      [ delta; restore_delta delta ]
      ~f:(fun delta ->
        let unbound =
          match delta with
          | A.Session_delta.Batch (run :: _) -> A.Session_delta.Batch [ run ]
          | _ -> assert false
        in
        assert (
          Result.is_error
            (A.Session_transition.apply ~now:timestamp basis ~delta:unbound ~payloads:[])));
    let encoded = state_document state |> D.Document.json in
    let payload = member encoded "payload" in
    let index = member payload "run_state" in
    let links =
      match member index "goal_links" with
      | `Array [ link ] -> `Array [ set link "future_link" (`String "retained") ]
      | _ -> assert false
    in
    let incoming =
      set encoded "payload" (set payload "run_state" (set index "goal_links" links))
      |> D.Document.inspect ~limits:document_limits
      |> document_ok
    in
    let restored =
      A.Session_state_document.decode ~limits:document_limits incoming |> document_ok
    in
    let rewritten =
      A.Session_state_document.encode restored ~limits:document_limits |> document_ok
    in
    assert (D.Json.equal (D.Document.json rewritten) (D.Document.json incoming)));
  print_endline
    "fresh Enabled association only; decoded orphan/rebind reject; link unknowns retained";
  [%expect
    {| fresh Enabled association only; decoded orphan/rebind reject; link unknowns retained |}]
;;

let%expect_test
    "independent terminal oracle preserves pending intent and exact outcome truth"
  =
  with_model (fun ~basis:_ ~held:_ ~admission:_ ~intent:_ ~delta:_ state ->
    let pending = request_achievement state in
    assert (Option.is_some (G.achievement_request (current_goal pending)));
    assert (
      match G.state (current_goal pending) with
      | Open _ -> true
      | Closed _ -> false);
    List.iter
      [ P.Run.Terminal.Completed None; Failed None; Cancelled; Limited; Interrupted ]
      ~f:(fun outcome ->
        let delta = terminal_delta pending outcome in
        let joined = A.Goal_run_settlement.prepare pending ~delta |> protocol_ok in
        let transition =
          A.Session_transition.apply ~now:timestamp pending ~delta:joined ~payloads:[]
          |> protocol_ok
        in
        let goal = current_goal transition.state in
        let expected_success =
          match outcome with
          | Completed _ -> true
          | _ -> false
        in
        let observed_success =
          match G.state goal with
          | Closed (Achieved _) -> true
          | _ -> false
        in
        [%test_eq: bool] observed_success expected_success;
        assert (Option.is_some (G.request_settlement goal));
        let again =
          A.Goal_run_settlement.prepare transition.state ~delta:(Batch []) |> protocol_ok
        in
        let repeated =
          A.Session_transition.apply
            ~now:timestamp
            transition.state
            ~delta:again
            ~payloads:[]
          |> protocol_ok
        in
        [%test_eq: int64]
          (C.revision (Option.value_exn repeated.state.goal_catalog))
          (C.revision (Option.value_exn transition.state.goal_catalog));
        assert (G.equal (current_goal repeated.state) goal));
    let reset =
      A.Administration.reset
        pending
        { keep_history = false
        ; keep_tasks = false
        ; keep_grants = false
        ; keep_labels = false
        ; workspace_instance = None
        }
      |> protocol_ok
    in
    let reset_goal = current_goal reset in
    [%test_eq: int64]
      (G.revision reset_goal)
      (Int64.succ (G.revision (current_goal pending)));
    assert (Option.is_some (G.request_settlement reset_goal));
    let catalog = Option.value_exn pending.goal_catalog in
    let paused = G.hold (current_goal pending) Operator_pause |> protocol_ok in
    let document =
      C.document catalog goal_id
      |> Option.value_exn
      |> fun document -> A.Goal_document.update document paused |> document_ok
    in
    let catalog = C.advance_existing catalog ~goal:document |> protocol_ok in
    let other = P.Id.Goal.of_string "goal_unrelated_enabled" |> protocol_ok in
    let command =
      Goal_catalog_tests.request
        catalog
        ~current_source:(G.source paused)
        "unrelated-enabled"
        (Create
           { id = other
           ; objective = G.Objective.of_string "Other objective" |> protocol_ok
           ; budget = G.Budget.unlimited
           })
    in
    let catalog =
      A.Goal_command_transition.prepare
        catalog
        ~request:command
        ~principal_id
        ~session:(G.session paused)
        ~generation:(G.source paused).generation
        ~current_source:(Some (G.source paused))
        ~now:timestamp
      |> Goal_catalog_tests.committed
      |> fst
      |> fun catalog -> Goal_catalog_tests.enable_fixture catalog other
    in
    let raw = C.to_jsonaf catalog |> protocol_ok in
    assert (
      Result.is_error
        (C.of_jsonaf
           (set raw "revision" (`String (Int64.to_string (Int64.pred Int64.max_value))))));
    let edge =
      C.of_jsonaf (set raw "revision" (`String (Int64.to_string Int64.(max_value - 2L))))
      |> protocol_ok
    in
    let held = C.hold_for_review edge |> protocol_ok |> Option.value_exn in
    let terminal =
      match terminal_delta pending Interrupted with
      | A.Session_delta.Run_state_changed index -> index
      | _ -> assert false
    in
    let settled =
      A.Goal_run_resolution.plan_catalog held ~runs:(Some terminal) ~hold_for_review:false
      |> protocol_ok
      |> Option.value_exn
    in
    [%test_eq: int64] (C.revision settled) Int64.max_value);
  print_endline
    "pending stays Open; exact Completed alone achieves; adverse/reset settle once";
  [%expect
    {| pending stays Open; exact Completed alone achieves; adverse/reset settle once |}]
;;

(* Imported historical evidence is an admitted data fixture only. The real actor
   below owns recovery and its persistence transaction, not episode execution. *)
let%expect_test
    "actual recovery save failure and lost ACK preserve atomic goal settlement on reopen"
  =
  let module Store = Agent_store in
  let module Persistence = A.Session_persistence in
  List.iter [ false; true ] ~f:(fun checkpoint ->
    with_model
      (fun ~basis:_ ~held:_ ~admission:_ ~intent:_ ~delta:_ state ->
         let state = request_achievement state in
         { state with
           counters =
             { state.counters with transaction_sequence = 0L; event_sequence = 0L }
         })
      ~after_close:(fun env workspace_instance initial ->
        A.Session_state.validate initial |> protocol_ok;
        let directory =
          Filename.concat workspace_instance.canonical_root.native_path "goal-run-journal"
        in
        let snapshots =
          Filename.concat
            workspace_instance.canonical_root.native_path
            "goal-run-snapshots"
        in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / snapshots);
        let expected =
          Eio.Switch.run (fun sw ->
            let journal =
              Store.Journal.create
                ~env
                ~directory
                ~max_payload_length:1048576
                ~max_segment_bytes:4194304L
                ~max_segment_frames:16
              |> store_ok
            in
            let writer =
              Store.Commit_writer.create
                ~sw
                ~journal
                ~session_id
                ~next_transaction_sequence:1L
                ~previous_transaction_hash:None
                ~queue_capacity:8
              |> store_ok
            in
            let durable =
              Run_persistence_tests.create_persistence
                writer
                (Persistence.Restored.authored initial)
                ~previous_transaction_hash:None
            in
            let reject = ref true in
            let persistence : A.Session_actor.persistence =
              { archive_reference
              ; commit =
                  (fun ~command_audit ~previous transition ->
                    if !reject
                    then
                      Error
                        (P.Error.create
                           Persistence_error
                           ~retryable:true
                           ~message:"failure before recovery write"
                           ())
                    else (
                      let%bind.Result () =
                        Persistence.commit durable ~command_audit ~previous transition
                      in
                      Error
                        (P.Error.create
                           Persistence_error
                           ~retryable:true
                           ~message:"lost acknowledgement after durable recovery"
                           ())))
              }
            in
            let actor = Run_persistence_tests.create_actor env sw initial persistence in
            Exn.protect
              ~finally:(fun () ->
                A.Session_actor.shutdown actor;
                Store.Commit_writer.close writer)
              ~f:(fun () ->
                let read () = A.Session_actor.state actor |> protocol_ok in
                assert (Result.is_error (A.Session_actor.reconcile_run_recovery actor));
                assert (G.equal (current_goal (read ())) (current_goal initial));
                assert (Option.is_none (G.request_settlement (current_goal (read ()))));
                [%test_eq: int64]
                  (Persistence.Restored.state (Persistence.restored durable)).counters
                    .transaction_sequence
                  0L;
                reject := false;
                assert (Result.is_error (A.Session_actor.reconcile_run_recovery actor));
                assert (G.equal (current_goal (read ())) (current_goal initial));
                let accepted =
                  Persistence.Restored.state (Persistence.restored durable)
                in
                assert (Option.is_some (G.request_settlement (current_goal accepted)));
                if checkpoint
                then
                  Persistence.install_snapshot_at
                    durable
                    ~env
                    ~directory:snapshots
                    ~max_payload_length:1048576
                    ~transaction_hash:(Persistence.transaction_hash durable)
                    accepted
                  |> store_ok
                  |> ignore;
                accepted))
        in
        Eio.Switch.run (fun sw ->
          let journal =
            Store.Journal.open_existing
              ~env
              ~directory
              ~max_payload_length:1048576
              ~max_segment_bytes:4194304L
              ~max_segment_frames:16
            |> store_ok
          in
          let recovered =
            Store.Recovery.load
              ~env
              ~journal
              ~snapshot_directory:snapshots
              ~max_snapshot_payload_length:1048576
              ~session_id
              ~initial:(Persistence.Restored.authored initial)
              ~restore_snapshot:(Persistence.restore_snapshot ~limits:document_limits)
              ~apply:(Persistence.apply_transaction ~limits:document_limits)
              ~validate_transaction:
                (Persistence.validate_transaction ~limits:document_limits)
              ~validate:Persistence.validate
            |> store_ok
          in
          let restored = Persistence.Restored.state recovered.state in
          assert (G.equal (current_goal restored) (current_goal expected));
          let index = Option.value_exn restored.run_state in
          let run = List.hd_exn (A.Run_state.runs index) in
          assert (P.Run.Lifecycle.equal run.lifecycle (Terminal Interrupted));
          assert (Option.is_none restored.active_operation);
          let writer =
            Store.Commit_writer.create
              ~sw
              ~journal
              ~session_id
              ~next_transaction_sequence:
                (Int64.succ recovered.latest_transaction_sequence)
              ~previous_transaction_hash:recovered.latest_transaction_hash
              ~queue_capacity:8
            |> store_ok
          in
          let durable =
            Run_persistence_tests.create_persistence
              writer
              recovered.state
              ~previous_transaction_hash:recovered.latest_transaction_hash
          in
          let actor =
            Run_persistence_tests.create_actor
              env
              sw
              restored
              (Persistence.actor_persistence durable)
          in
          Exn.protect
            ~finally:(fun () ->
              A.Session_actor.shutdown actor;
              Store.Commit_writer.close writer)
            ~f:(fun () ->
              A.Session_actor.reconcile_run_recovery actor |> protocol_ok;
              let repeated = A.Session_actor.state actor |> protocol_ok in
              [%test_eq: int64] repeated.counters.revision restored.counters.revision;
              assert (G.equal (current_goal repeated) (current_goal restored))))));
  print_endline
    "failed save changes neither; journal/snapshot restore exact adverse settlement; \
     recovery no-op";
  [%expect
    {| failed save changes neither; journal/snapshot restore exact adverse settlement; recovery no-op |}]
;;
