open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module G = P.Goal
module D = Document_schema
module C = A.Goal_catalog
module Accounts = A.Goal_accounts
module Descendants = A.Goal_descendants
module Child = A.Goal_accounting_document
module SD = A.Session_state_document
module Persistence = A.Session_persistence
module Store = Agent_store
module Existing = Goal_accounting_persistence_tests
module Catalog = Goal_catalog_tests

let goal_id = Catalog.goal_id "ordered_bootstrap"
let unrelated_id = Catalog.goal_id "ordered_unrelated"
let sentinel = `Object [ "must_survive", `String "ordered-account-bootstrap" ]
let member = Catalog.member
let set = Catalog.set

let original =
  P.Principal.create
    ~id:principal_id
    ~authentication_kind:"local"
    ~scopes:(P.Scope.Set.of_list [ Send_messages ])
    ~attributes:[]
  |> protocol_ok
  |> Operator_authorization.nonexpiring_static
;;

(* These are real command/ordered persistence DATA APIs. No Run or execution
   permission is inferred from this lower storage qualification. *)
let project (state : A.Session_state.t) ~session ~key mutation =
  let catalog = Option.value state.goal_catalog ~default:C.empty in
  let _, source = A.Goal_catalog_source.capture state |> protocol_ok in
  let request =
    A.Goal_command.create
      ~session_id:state.identity.session_id
      ~generation:state.identity.generation
      ~source:
        (if A.Goal_command.Mutation.needs_source mutation then Some source else None)
      ~expected_catalog_revision:(C.revision catalog)
      ~expected_goal_revision:
        (Option.map
           (C.find catalog (A.Goal_command.Mutation.goal_id mutation))
           ~f:G.revision)
      ~key:(P.Idempotency_key.of_string key |> protocol_ok)
      ~mutation
    |> protocol_ok
  in
  let prepared =
    A.Goal_command_admission.prepare
      state
      ~actor:original
      ~authorize:(fun _ -> Ok ())
      ~session
      ~descendant_profile:goal_descendant_profile
      ~request
      ~now:timestamp
    |> protocol_ok
  in
  let delta = Option.value_exn prepared.delta in
  A.Session_delta.apply state delta |> protocol_ok, delta
;;

let create state ~session id key =
  project
    state
    ~session
    ~key
    (Create { id; objective = Catalog.objective key; budget = G.Budget.unlimited })
;;

let child_json state id =
  Accounts.find (Option.value_exn state.A.Session_state.goal_accounts) id
  |> Option.value_exn
  |> Child.to_document
  |> protocol_ok
  |> D.Document.json
;;

let descendants_json state =
  Option.value_exn state.A.Session_state.goal_descendants
  |> Descendants.to_document
  |> protocol_ok
  |> D.Document.json
;;

let ledger_json state =
  state.A.Session_state.inference_ledger
  |> A.Inference_ledger.to_document
  |> Existing.ledger
  |> D.Document.json
;;

let carried state =
  let document =
    SD.authored state |> fun t -> SD.encode t ~limits:document_limits |> document_ok
  in
  let raw = D.Document.json document in
  set raw "future_bootstrap_host" sentinel
  |> D.Document.inspect ~limits:document_limits
  |> document_ok
  |> SD.decode ~limits:document_limits
  |> document_ok
;;

let account_extensions state =
  let accounts = Option.value_exn state.A.Session_state.goal_accounts in
  let raw = Accounts.to_jsonaf accounts |> protocol_ok in
  let raw = set raw "future_bootstrap_accounts" sentinel in
  let descendants = descendants_json state in
  let payload = member descendants "payload" in
  let roots =
    match member payload "roots" with
    | `Array [ root ] -> `Array [ set root "future_bootstrap_root" sentinel ]
    | _ -> failwith "expected the actual unrelated root"
  in
  let payload =
    set
      payload
      "profile"
      (set (member payload "profile") "future_bootstrap_profile" sentinel)
    |> fun payload ->
    set payload "roots" roots
    |> fun payload -> set payload "future_bootstrap_descendants" sentinel
  in
  let descendants =
    set descendants "payload" payload
    |> fun raw ->
    set raw "future_bootstrap_descendant_envelope" sentinel
    |> D.Document.inspect ~limits:document_limits
    |> document_ok
    |> Descendants.of_document
    |> protocol_ok
  in
  { state with
    goal_accounts = Some (Accounts.of_jsonaf raw |> protocol_ok)
  ; goal_descendants = Some descendants
  }
;;

let assert_rejected previous delta message =
  (match A.Session_transition.apply ~now:timestamp previous ~delta ~payloads:[] with
   | Ok _ -> failwith "invalid Goal-account bootstrap accepted"
   | Error error ->
     assert (P.Error.equal_code Invalid_request error.code);
     [%test_eq: string] message error.message);
  let previous = carried previous in
  let encoded =
    A.Session_delta_document.create
      delta
      ~limits:document_limits
      ~state_document:SD.authored
    |> document_ok
  in
  assert (
    Result.is_error
      (A.Session_delta_document.apply encoded ~limits:document_limits previous))
;;

let with_writer ?(session_id = session_id) sw journal ~next ~previous_hash f =
  let writer =
    Store.Commit_writer.create
      ~sw
      ~journal
      ~session_id
      ~next_transaction_sequence:next
      ~previous_transaction_hash:previous_hash
      ~queue_capacity:8
    |> store_ok
  in
  Exn.protect
    ~finally:(fun () -> Store.Commit_writer.close writer)
    ~f:(fun () -> f writer)
;;

let journal env directory ~existing =
  (if existing then Store.Journal.open_existing else Store.Journal.create)
    ~env
    ~directory
    ~max_payload_length:1048576
    ~max_segment_bytes:4194304L
    ~max_segment_frames:16
  |> store_ok
;;

let assert_final previous expected document =
  let current = SD.value document in
  let goal = C.find (Option.value_exn current.goal_catalog) goal_id |> Option.value_exn in
  assert (
    G.State.equal
      (Closed (Abandoned (Catalog.reason "closed in the same transaction")))
      (G.state goal));
  [%test_eq: int64] 2L (G.revision goal);
  [%test_eq: string] "edited before closure" (G.Objective.to_string (G.objective goal));
  [%test_eq: int] 4 (List.length (C.receipts (Option.value_exn current.goal_catalog)));
  assert (Option.is_none (G.episode goal));
  assert (
    D.Json.equal
      (C.to_jsonaf (Option.value_exn expected.A.Session_state.goal_catalog) |> protocol_ok)
      (C.to_jsonaf (Option.value_exn current.goal_catalog) |> protocol_ok));
  assert (
    D.Json.equal
      (A.Run_state.to_jsonaf (Option.value_exn expected.run_state))
      (A.Run_state.to_jsonaf (Option.value_exn current.run_state)));
  assert (D.Json.equal (ledger_json previous) (ledger_json current));
  assert (D.Json.equal (child_json expected goal_id) (child_json current goal_id));
  assert (
    D.Json.equal (child_json previous unrelated_id) (child_json current unrelated_id));
  let descendants = Option.value_exn current.goal_descendants in
  let previous_descendants = Option.value_exn previous.goal_descendants in
  assert (Option.is_none (Descendants.origin descendants));
  assert (List.is_empty (Descendants.local_included descendants));
  assert (
    Descendants.Profile.equal
      (Descendants.profile previous_descendants)
      (Descendants.profile descendants));
  assert (
    P.Session_ref.equal
      (Descendants.session previous_descendants)
      (Descendants.session descendants));
  [%test_eq: P.Id.Goal.t list]
    (List.sort [ goal_id; unrelated_id ] ~compare:P.Id.Goal.compare)
    (Descendants.roots descendants);
  List.iter (Descendants.roots descendants) ~f:(fun goal_id ->
    assert (List.is_empty (Descendants.branches descendants ~goal_id)));
  assert (D.Json.equal (descendants_json expected) (descendants_json current));
  let raw_descendants = descendants_json current in
  assert (
    D.Json.equal (member raw_descendants "future_bootstrap_descendant_envelope") sentinel);
  let payload = member raw_descendants "payload" in
  assert (D.Json.equal (member payload "future_bootstrap_descendants") sentinel);
  assert (
    D.Json.equal (member (member payload "profile") "future_bootstrap_profile") sentinel);
  let unrelated_root =
    match member payload "roots" with
    | `Array roots ->
      List.find_exn roots ~f:(fun root ->
        D.Json.equal (member root "goal_id") (P.Id.Goal.to_json unrelated_id))
    | _ -> failwith "expected descendant roots"
  in
  assert (D.Json.equal (member unrelated_root "future_bootstrap_root") sentinel);
  let accounting =
    Accounts.find (Option.value_exn current.goal_accounts) goal_id
    |> Option.value_exn
    |> fun child ->
    Child.resolve child ~goal ~ledger:current.inference_ledger |> protocol_ok
  in
  [%test_eq: int64 * int64]
    (0L, 0L)
    ( A.Goal_accounting.admitted_turns accounting
    , A.Goal_accounting.admitted_attempts accounting );
  let document = SD.encode document ~limits:document_limits |> document_ok in
  assert (
    D.Json.equal (member (D.Document.json document) "future_bootstrap_host") sentinel);
  assert (
    D.Json.equal
      (member
         (member (D.Document.payload document) "goal_accounts")
         "future_bootstrap_accounts")
      sentinel)
;;

let assert_history_transaction_origin (initial : A.Session_state.t) ~session =
  let target = History_edit_tests.occurrence 0 "retire this exact occurrence" in
  let initial : A.Session_state.t =
    { initial with
      conversation =
        { initial.conversation with
          canonical_history = [ target ]
        ; initial_prompt_entry_count = 0
        ; next_history_sequence = 8L
        ; reserved_history_through = 8L
        }
    }
  in
  A.Session_state.validate initial |> protocol_ok;
  let advanced, _, _ =
    A.Inference_ledger.admit_turn
      initial.inference_ledger
      P.Operation.
        { id = operation_id
        ; generation = initial.identity.generation
        ; kind = Turn User_submit
        ; state = Running
        ; started_at = timestamp
        ; updated_at = timestamp
        }
    |> Existing.ledger
  in
  let usage = A.Session_delta.Inference_ledger_changed advanced in
  let prefix = A.Session_delta.apply initial usage |> protocol_ok in
  let archive =
    A.Compaction_archive.reference_for
      (carried prefix)
      ~limits:document_limits
      ~kind:Delete
      operation_id
    |> protocol_ok
  in
  let retirement = A.Session_delta.History_deleted (target.id, archive) in
  let retired = A.Session_delta.apply prefix retirement |> protocol_ok in
  let created, creation = create retired ~session goal_id "after-history-create" in
  let delta = A.Session_delta.Batch [ usage; retirement; creation ] in
  (* The archive is genuine, but it cannot turn this transaction's admitted
     ledger prefix into a new accounting origin. Native and replay must agree. *)
  assert_rejected
    initial
    delta
    "new partition differs from actual before-ledger bootstrap";
  assert (
    Result.is_error
      (A.Session_document_transition.admit_encoded
         (carried initial)
         ~delta
         ~next:created
         ~limits:document_limits));
  (* The same history retirement committed as a real first transaction is a
     valid before-ledger for a subsequent public Create command. *)
  let first_delta = A.Session_delta.Batch [ usage; retirement ] in
  let first =
    A.Session_transition.apply ~now:timestamp initial ~delta:first_delta ~payloads:[]
    |> protocol_ok
  in
  let first_document =
    A.Session_document_transition.admit_encoded
      (carried initial)
      ~delta:first_delta
      ~next:first.state
      ~limits:document_limits
    |> document_ok
    |> SD.Admitted.state_document
  in
  let projected, creation = create first.state ~session goal_id "next-history-create" in
  let second =
    A.Session_transition.apply ~now:timestamp first.state ~delta:creation ~payloads:[]
    |> protocol_ok
  in
  let admitted =
    A.Session_document_transition.admit_encoded
      first_document
      ~delta:creation
      ~next:second.state
      ~limits:document_limits
    |> document_ok
    |> SD.Admitted.state_document
  in
  let current = SD.value admitted in
  assert (List.is_empty current.conversation.canonical_history);
  [%test_eq: int]
    (List.length initial.conversation.compaction_archives + 1)
    (List.length current.conversation.compaction_archives);
  [%test_eq: int64] Int64.(initial.counters.revision + 2L) current.counters.revision;
  assert (D.Json.equal (ledger_json prefix) (ledger_json current));
  assert (D.Json.equal (child_json initial unrelated_id) (child_json current unrelated_id));
  assert (D.Json.equal (child_json projected goal_id) (child_json current goal_id));
  let encoded = SD.encode admitted ~limits:document_limits |> document_ok in
  assert (D.Json.equal (member (D.Document.json encoded) "future_bootstrap_host") sentinel)
;;

let%expect_test
    "ordered Goal bootstrap retains its real creation edge through closure and journal \
     replay"
  =
  Run_admission_tests.with_startup_actor
    (fun actor _ _ _ _ _ _ session _ ->
       let state = A.Session_actor.state actor |> protocol_ok in
       let ledger, _, _ =
         A.Inference_ledger.admit_turn
           state.inference_ledger
           P.Operation.
             { id = P.Id.Operation.of_string "op_ordered_prior" |> protocol_ok
             ; generation = state.identity.generation
             ; kind = Turn User_submit
             ; state = Running
             ; started_at = timestamp
             ; updated_at = timestamp
             }
         |> Existing.ledger
       in
       let state =
         A.Session_delta.apply state (Inference_ledger_changed ledger) |> protocol_ok
       in
       [%test_eq: int64]
         1L
         (P.Inference_query.Summary.turns (A.Inference_ledger.summary ledger)).pending;
       let projected, delta = create state ~session unrelated_id "unrelated-create" in
       ignore (projected : A.Session_state.t);
       let state =
         A.Session_transition.apply ~now:timestamp state ~delta ~payloads:[]
         |> protocol_ok
         |> fun t -> t.A.Session_transition.state
       in
       ( { (account_extensions state) with
           counters =
             { state.counters with transaction_sequence = 0L; event_sequence = 0L }
         }
       , session ))
    ~after_close:(fun env workspace (initial, session) ->
      Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
        let accepted = carried initial in
        (* Historical absence is a valid decoded value, but public with_value
           cannot make it the new original of a carrier that already owns an
           account. Check begin_ itself, before any new ordered edge exists. *)
        List.iter [ None; Some Accounts.empty ] ~f:(fun goal_accounts ->
          let forged = { initial with goal_accounts } in
          assert (Result.is_error (A.Session_state.validate forged));
          let forged = SD.with_value accepted forged in
          assert (Result.is_error (SD.Transaction.begin_ forged ~limits:document_limits)));
        assert_history_transaction_origin initial ~session;
        let held, creation = create initial ~session goal_id "ordered-create" in
        let edited, edit =
          project
            held
            ~session
            ~key:"ordered-edit"
            (Revise_objective (goal_id, Catalog.objective "edited before closure"))
        in
        let closed, abandon =
          project
            edited
            ~session
            ~key:"ordered-abandon"
            (Abandon (goal_id, Catalog.reason "closed in the same transaction"))
        in
        let held_accounts = Option.value_exn held.goal_accounts in
        let historical =
          { closed with
            goal_accounts = initial.goal_accounts
          ; goal_descendants = initial.goal_descendants
          ; goal_policy_contributions = initial.goal_policy_contributions
          }
        in
        List.iter
          [ A.Session_delta.Goal_accounts_changed held_accounts
          ; Created { historical with goal_accounts = Some held_accounts }
          ]
          ~f:(fun delta ->
            assert_rejected
              historical
              delta
              "only a held goal before its first episode may bootstrap accounting");
        let advanced, _, _ =
          A.Inference_ledger.admit_turn
            initial.inference_ledger
            P.Operation.
              { id = operation_id
              ; generation = initial.identity.generation
              ; kind = Turn User_submit
              ; state = Running
              ; started_at = timestamp
              ; updated_at = timestamp
              }
          |> Existing.ledger
        in
        let wrong =
          Accounts.bootstrap
            (Option.value_exn initial.goal_accounts)
            ~goal:(C.find (Option.value_exn held.goal_catalog) goal_id |> Option.value_exn)
            ~ledger:advanced
          |> protocol_ok
        in
        let rec substitute (delta : A.Session_delta.t) : A.Session_delta.t =
          match delta with
          | A.Session_delta.Batch changes -> Batch (List.map changes ~f:substitute)
          | Goal_accounts_changed _ -> Goal_accounts_changed wrong
          | leaf -> leaf
        in
        assert_rejected
          initial
          (substitute creation)
          "new partition differs from actual before-ledger bootstrap";
        let delta = A.Session_delta.Batch [ creation; edit; abandon ] in
        let transition =
          A.Session_transition.apply ~now:timestamp initial ~delta ~payloads:[]
          |> protocol_ok
        in
        [%test_eq: int64]
          (Int64.succ initial.counters.revision)
          transition.state.counters.revision;
        let previous = carried initial in
        let admitted =
          A.Session_document_transition.admit_encoded
            previous
            ~delta
            ~next:transition.state
            ~limits:document_limits
          |> document_ok
        in
        assert_final initial closed (SD.Admitted.state_document admitted);
        let encoded =
          A.Session_delta_document.create
            delta
            ~limits:document_limits
            ~state_document:SD.authored
          |> document_ok
          |> A.Session_delta_document.document
          |> A.Session_delta_document.decode ~limits:document_limits
          |> document_ok
        in
        let replayed =
          A.Session_delta_document.apply encoded ~limits:document_limits previous
          |> document_ok
        in
        assert_final initial closed replayed;
        let directory =
          Filename.concat workspace.canonical_root.native_path "ordered-bootstrap-journal"
        in
        let snapshots =
          Filename.concat
            workspace.canonical_root.native_path
            "ordered-bootstrap-snapshots"
        in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / snapshots);
        let restored = Persistence.Restored.of_state_document previous in
        Eio.Switch.run (fun sw ->
          let journal = journal env directory ~existing:false in
          with_writer sw journal ~next:1L ~previous_hash:None (fun writer ->
            let persistence =
              Persistence.create
                ~archive:(fun _ _ -> failwith "unexpected archive")
                ~pending_archive:None
                ~before_commit:None
                ~command_accepted:(fun _ _ -> ())
                ~writer
                ~durability:Flush
                ~limits:document_limits
                ~archive_limits:document_limits
                ~retention_preflight:None
                ~restored
                ~previous_transaction_hash:None
            in
            Persistence.commit
              persistence
              ~command_audit:None
              ~previous:initial
              transition
            |> protocol_ok;
            assert_final
              initial
              closed
              (Persistence.Restored.state_document (Persistence.restored persistence))));
        Eio.Switch.run (fun sw ->
          let journal = journal env directory ~existing:true in
          let recovered =
            Store.Recovery.load
              ~env
              ~journal
              ~snapshot_directory:snapshots
              ~max_snapshot_payload_length:1048576
              ~session_id
              ~initial:restored
              ~restore_snapshot:(Persistence.restore_snapshot ~limits:document_limits)
              ~apply:(Persistence.apply_transaction ~limits:document_limits)
              ~validate_transaction:
                (Persistence.validate_transaction ~limits:document_limits)
              ~validate:Persistence.validate
            |> store_ok
          in
          [%test_eq: int64] 1L recovered.latest_transaction_sequence;
          with_writer
            sw
            journal
            ~next:2L
            ~previous_hash:recovered.latest_transaction_hash
            (fun writer ->
               assert_final
                 initial
                 closed
                 (Persistence.Restored.state_document recovered.state);
               let persistence =
                 Persistence.create
                   ~archive:(fun _ _ -> failwith "unexpected archive")
                   ~pending_archive:None
                   ~before_commit:None
                   ~command_accepted:(fun _ _ -> ())
                   ~writer
                   ~durability:Flush
                   ~limits:document_limits
                   ~archive_limits:document_limits
                   ~retention_preflight:None
                   ~restored:recovered.state
                   ~previous_transaction_hash:recovered.latest_transaction_hash
               in
               let previous = Persistence.Restored.state recovered.state in
               ignore
                 (Persistence.install_snapshot_at
                    persistence
                    ~env
                    ~directory:snapshots
                    ~max_payload_length:1048576
                    ~transaction_hash:(Persistence.transaction_hash persistence)
                    previous
                  |> store_ok
                  : Store.Snapshot.installed);
               let values =
                 P.Session_metadata.Values.create
                   ~display_name:(Some "after descendant recovery")
                   ~labels:[]
                 |> protocol_ok
               in
               let transition =
                 A.Session_transition.apply
                   ~now:timestamp
                   previous
                   ~delta:
                     (Metadata_changed
                        (values, Int64.succ previous.identity.metadata_revision))
                   ~payloads:[]
                 |> protocol_ok
               in
               Persistence.commit persistence ~command_audit:None ~previous transition
               |> protocol_ok;
               assert_final
                 initial
                 closed
                 (Persistence.Restored.state_document (Persistence.restored persistence))));
        Eio.Switch.run (fun sw ->
          let journal = journal env directory ~existing:true in
          let recovered =
            Store.Recovery.load
              ~env
              ~journal
              ~snapshot_directory:snapshots
              ~max_snapshot_payload_length:1048576
              ~session_id
              ~initial:restored
              ~restore_snapshot:(Persistence.restore_snapshot ~limits:document_limits)
              ~apply:(Persistence.apply_transaction ~limits:document_limits)
              ~validate_transaction:
                (Persistence.validate_transaction ~limits:document_limits)
              ~validate:Persistence.validate
            |> store_ok
          in
          [%test_eq: int64] 2L recovered.latest_transaction_sequence;
          assert_final
            initial
            closed
            (Persistence.Restored.state_document recovered.state);
          with_writer
            sw
            journal
            ~next:3L
            ~previous_hash:recovered.latest_transaction_hash
            (fun _ -> ()))));
  print_endline
    "ordered creation preserved; historical/reset and wrong-ledger rejected; one journal \
     transaction reopens exactly; snapshot and next write retain descendant roots";
  [%expect
    {| ordered creation preserved; historical/reset and wrong-ledger rejected; one journal transaction reopens exactly; snapshot and next write retain descendant roots |}]
;;

let%test_unit "descendant roots require their actual ordered Create and retained carrier" =
  Run_admission_tests.with_startup_actor (fun actor _ _ _ _ _ _ session _ ->
    let initial = A.Session_actor.state actor |> protocol_ok in
    let held, creation = create initial ~session goal_id "descendant-boundary-create" in
    let descendants = Option.value_exn held.goal_descendants in
    (* Native27 already supports positive descendant/Headless data. Its upgrade
       must insert only an absent causal component, not rerun older feature guards.
       Actual envelope key/version assertions keep the old-label oracle honest. *)
    let native =
      { held with
        schema_version = 27
      ; policy_canonical_usage = None
      ; goal_policy_contributions = None
      }
    in
    [%test_eq: int] 27 native.schema_version;
    let restored = A.Session_state.upgrade_schema native |> protocol_ok in
    [%test_eq: int] 29 restored.schema_version;
    assert (Option.is_none restored.policy_canonical_usage);
    assert (D.Json.equal (descendants_json held) (descendants_json restored));
    let universal =
      SD.authored { held with goal_policy_contributions = None }
      |> fun t -> SD.encode t ~limits:document_limits |> document_ok |> D.Document.json
    in
    assert (D.Json.equal (`Number "14") (member universal "schema_version"));
    let old = set universal "schema_version" (`Number "12") in
    let old = D.Document.inspect old ~limits:document_limits |> document_ok in
    [%test_eq: int] 12 (D.Document.version old);
    let restored = SD.decode old ~limits:document_limits |> document_ok |> SD.value in
    assert (Option.is_none restored.policy_canonical_usage);
    assert (D.Json.equal (descendants_json held) (descendants_json restored));
    let old = D.Document.json old in
    let positive =
      set old "payload" (set (member old "payload") "policy_canonical_usage" (`Object []))
    in
    assert (
      Result.is_error
        (D.Document.inspect positive ~limits:document_limits
         |> document_ok
         |> SD.decode ~limits:document_limits));
    assert (Option.is_none (Descendants.origin descendants));
    assert (
      Descendants.Profile.equal goal_descendant_profile (Descendants.profile descendants));
    [%test_eq: P.Id.Goal.t list] [ goal_id ] (Descendants.roots descendants);
    let genuine =
      A.Session_transition.apply ~now:timestamp initial ~delta:creation ~payloads:[]
      |> protocol_ok
    in
    let genuine =
      A.Session_document_transition.admit_encoded
        (carried initial)
        ~delta:creation
        ~next:genuine.state
        ~limits:document_limits
      |> document_ok
      |> SD.Admitted.state_document
    in
    assert (D.Json.equal (descendants_json held) (descendants_json (SD.value genuine)));
    let old_held =
      { held with
        goal_descendants = initial.goal_descendants
      ; child_contributions = initial.child_contributions
      }
    in
    A.Session_state.validate old_held |> protocol_ok;
    let rec descendant_leaf (delta : A.Session_delta.t) =
      match delta with
      | Goal_descendants_changed _ -> Some delta
      | Batch changes -> List.find_map changes ~f:descendant_leaf
      | _ -> None
    in
    let update = descendant_leaf creation |> Option.value_exn in
    let rejected previous delta =
      assert (
        Result.is_error
          (A.Session_transition.apply ~now:timestamp previous ~delta ~payloads:[]));
      let encoded =
        A.Session_delta_document.create
          delta
          ~limits:document_limits
          ~state_document:SD.authored
        |> document_ok
      in
      assert (
        Result.is_error
          (A.Session_delta_document.apply
             encoded
             ~limits:document_limits
             (carried previous)))
    in
    (* Historical omission may bootstrap exactly one new Unrelated child, through
       the same finite native/document replay edge. It cannot grant an ordinary
       replacement or Children leaf permission to invent that component. *)
    let historical = { initial with goal_descendants = None } in
    let reference =
      Store.Delegation_store.reference_of_jsonaf
        (`Object
            [ ( "key"
              , `Object
                  [ ( "parent_session_id"
                    , P.Id.Session.to_json historical.identity.session_id )
                  ; ( "parent_generation"
                    , `Number (Int.to_string historical.identity.generation) )
                  ; "principal_id", P.Id.Principal.to_json principal_id
                  ; "idempotency_key", `String "first-unrelated-bootstrap"
                  ] )
            ; "child_session_id", `String "ses_first_unrelated_bootstrap"
            ; "revision_id", `String "prv_first_unrelated_bootstrap"
            ; "request_sha256", `String (String.make 64 'a')
            ; "admission_sha256", `String (String.make 64 'b')
            ])
      |> Result.map_error ~f:Store.Store_error.to_protocol_error
      |> protocol_ok
    in
    let first_child =
      Descendants.register_first_unrelated_child
        ~host_profile:goal_descendant_profile
        ~session
        ~reference
        ~children:historical.child_contributions
        ~ledger:historical.inference_ledger
      |> protocol_ok
    in
    let delta = A.Session_delta.Goal_descendants_changed first_child in
    let transition =
      A.Session_transition.apply ~now:timestamp historical ~delta ~payloads:[]
      |> protocol_ok
    in
    let admitted =
      A.Session_document_transition.admit_encoded
        (carried historical)
        ~delta
        ~next:transition.state
        ~limits:document_limits
      |> document_ok
      |> SD.Admitted.state_document
    in
    let encoded =
      A.Session_delta_document.create
        delta
        ~limits:document_limits
        ~state_document:SD.authored
      |> document_ok
    in
    let replayed =
      A.Session_delta_document.apply encoded ~limits:document_limits (carried historical)
      |> document_ok
    in
    assert (
      D.Json.equal
        (descendants_json (SD.value admitted))
        (descendants_json (SD.value replayed)));
    assert (D.Json.equal (ledger_json historical) (ledger_json (SD.value replayed)));
    assert (
      Option.is_none
        (Descendants.origin (Option.value_exn (SD.value replayed).goal_descendants)));
    rejected historical (Created transition.state);
    rejected
      historical
      (Child_contributions_changed (Option.value_exn transition.state.child_contributions));
    let forged = SD.with_value (carried historical) transition.state in
    assert (Result.is_error (SD.encode forged ~limits:document_limits));
    assert (
      Result.is_error
        (SD.adopt
           (carried historical)
           ~limits:document_limits
           (SD.authored transition.state)));
    (* Equal held Goal/account DATA are not proof of the actual creation edge.
       Created replacement and duplicate catalog/account leaves cannot supply it. *)
    rejected old_held update;
    rejected old_held (Created held);
    rejected initial (Created held);
    rejected
      old_held
      (Batch
         [ Goal_catalog_changed (Option.value_exn held.goal_catalog)
         ; Goal_accounts_changed (Option.value_exn held.goal_accounts)
         ; update
         ]);
    let old_document = carried old_held in
    let held_document = carried held in
    List.iter
      [ old_document, held; held_document, old_held ]
      ~f:(fun (previous, next) ->
        let forged = SD.with_value previous next in
        assert (Result.is_error (SD.encode forged ~limits:document_limits));
        assert (Result.is_error (SD.Transaction.begin_ forged ~limits:document_limits));
        assert (
          Result.is_error (SD.adopt previous ~limits:document_limits (SD.authored next))));
    let marker = P.Session_storage_semantics.goal_descendants in
    let strip_marker document =
      assert (List.mem (D.Document.required_semantics document) marker ~equal:String.equal);
      set
        (D.Document.json document)
        "required_semantics"
        (`Array
            (List.filter_map (D.Document.required_semantics document) ~f:(fun name ->
               if String.equal name marker then None else Some (`String name))))
      |> D.Document.inspect ~limits:document_limits
      |> document_ok
    in
    let encoded = SD.encode held_document ~limits:document_limits |> document_ok in
    assert (Result.is_error (SD.decode ~limits:document_limits (strip_marker encoded)));
    let encoded_delta =
      A.Session_delta_document.create
        creation
        ~limits:document_limits
        ~state_document:SD.authored
      |> document_ok
      |> A.Session_delta_document.document
    in
    assert (
      Result.is_error
        (A.Session_delta_document.decode
           ~limits:document_limits
           (strip_marker encoded_delta)));
    (* Adjacent old-version conversion preserves both historical wire forms on
       an unrelated ordered write; neither form invents an accounting origin. *)
    let raw =
      SD.authored initial
      |> fun document ->
      SD.encode document ~limits:document_limits |> document_ok |> D.Document.json
    in
    let remove json name =
      match json with
      | `Object fields -> `Object (List.Assoc.remove fields name ~equal:String.equal)
      | _ -> failwith "expected object"
    in
    List.iter [ false; true ] ~f:(fun explicit_null ->
      let payload = member raw "payload" in
      let payload =
        if explicit_null
        then set payload "goal_descendants" `Null
        else remove payload "goal_descendants"
      in
      let old =
        set raw "payload" payload
        |> fun raw ->
        set raw "schema_version" (`Number "11")
        |> D.Document.inspect ~limits:document_limits
        |> document_ok
        |> fun document ->
        [%test_eq: int] (D.Document.version document) 11;
        SD.decode ~limits:document_limits document |> document_ok
      in
      assert (Option.is_none (SD.value old).goal_descendants);
      let delta = A.Session_delta.Halt_changed (Some "unrelated historical write") in
      let transition =
        A.Session_transition.apply ~now:timestamp (SD.value old) ~delta ~payloads:[]
        |> protocol_ok
      in
      let admitted =
        A.Session_document_transition.admit_encoded
          old
          ~delta
          ~next:transition.state
          ~limits:document_limits
        |> document_ok
        |> SD.Admitted.document
      in
      match D.Json.field (D.Document.payload admitted) ~name:"goal_descendants" with
      | Null -> assert explicit_null
      | Absent -> assert (not explicit_null)
      | Value _ -> failwith "historical write invented descendant accounting"))
;;

let%test_unit "ordinary absent policy accounting adds no causal birth-window limit" =
  with_actor_workspace (fun _env workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let executions =
      List.init 1025 ~f:(fun index ->
        let context : P.Moderator_execution.context =
          { id =
              P.Id.Moderator_execution.of_string ("mex_absent_" ^ Int.to_string index)
              |> protocol_ok
          ; session_id = initial.identity.session_id
          ; generation = initial.identity.generation
          ; source = { script_id = "ordinary-absent"; source_sha256 = String.make 64 'a' }
          ; operation_id = None
          ; job = None
          ; phase = Internal_event
          ; event = `Null
          ; checkpoint_sha256 = String.make 64 'b'
          ; created_at = timestamp
          }
        in
        P.Moderator_execution.create context |> protocol_ok)
    in
    let delta =
      A.Session_delta.Batch
        (List.concat_map executions ~f:(fun receipt ->
           let completed =
             P.Moderator_execution.complete
               receipt
               ~checkpoint_sha256:receipt.context.checkpoint_sha256
               ~requests:
                 { request_turn = false; request_compaction = false; end_session = None }
             |> protocol_ok
           in
           (* Preserve the existing one-running-callback invariant while exercising
              more births than the optional causal cursor's window. *)
           [ A.Session_delta.Moderator_execution_changed receipt
           ; Moderator_execution_changed completed
           ]))
    in
    assert (
      not
        (A.Session_delta.requires_policy_tracking
           delta
           ~previous:initial.policy_canonical_usage));
    let transition =
      A.Session_transition.apply ~now:timestamp initial ~delta ~payloads:[] |> protocol_ok
    in
    [%test_eq: int] 1025 (List.length transition.state.moderator_executions);
    assert (Option.is_none transition.state.policy_canonical_usage);
    let admitted =
      A.Session_document_transition.admit_encoded
        (SD.authored initial)
        ~delta
        ~next:transition.state
        ~limits:document_limits
      |> document_ok
      |> SD.Admitted.state_document
    in
    let final = SD.value admitted in
    [%test_eq: int] 1025 (List.length final.moderator_executions);
    assert (Option.is_none final.policy_canonical_usage);
    assert (
      List.equal
        P.Moderator_execution.equal
        transition.state.moderator_executions
        final.moderator_executions))
;;

let%test_unit "ordered policy seed uses current native and universal reader fences" =
  Run_admission_tests.with_startup_actor (fun actor _ _ _ _ _ _ session _ ->
    let module PC = A.Policy_canonical_usage in
    let module Cause = A.Policy_cause_data in
    let initial = A.Session_actor.state actor |> protocol_ok in
    let index, source = A.Goal_catalog_source.capture initial |> protocol_ok in
    let before = A.Session_delta.apply initial (Run_state_changed index) |> protocol_ok in
    let child_id = P.Id.Session.of_string "ses_policy_codec_child" |> protocol_ok in
    let caller =
      P.Session_ref.create
        ~session_id:child_id
        ~server_id:(P.Session_ref.server_id session)
    in
    let caller_source =
      P.Run_source.create
        ~observer:{ script_id = "policy-codec-child"; source_sha256 = String.make 64 'c' }
        ~generation:0
        ~installation_epoch:1L
      |> protocol_ok
    in
    (* Validated lower Reference DATA is not runtime ancestry or permission. The
       empty governance vector is deliberate: this case qualifies storage, while
       pure subset trajectories independently prove actual Goal attribution. *)
    let reference =
      Store.Delegation_store.reference_of_jsonaf
        (`Object
            [ ( "key"
              , `Object
                  [ "parent_session_id", P.Id.Session.to_json initial.identity.session_id
                  ; ( "parent_generation"
                    , `Number (Int.to_string initial.identity.generation) )
                  ; "principal_id", P.Id.Principal.to_json principal_id
                  ; "idempotency_key", `String "policy-codec"
                  ] )
            ; "child_session_id", P.Id.Session.to_json child_id
            ; "revision_id", `String "prv_policy_codec"
            ; "request_sha256", `String (String.make 64 'a')
            ; "admission_sha256", `String (String.make 64 'b')
            ])
      |> store_ok
    in
    let event =
      P.Moderator_execution.create_delegated
        ~delegation:
          { child_session_id = child_id
          ; child_generation = 0
          ; child_invocation_id =
              P.Id.Invocation.of_string "inv_policy_codec" |> protocol_ok
          ; admission_sha256 = String.make 64 'b'
          }
        { id = P.Id.Moderator_execution.of_string "mex_policy_codec" |> protocol_ok
        ; session_id = initial.identity.session_id
        ; generation = initial.identity.generation
        ; source = source.observer
        ; operation_id = None
        ; job = None
        ; phase = Pre_tool_call
        ; event = `Null
        ; checkpoint_sha256 = String.make 64 'a'
        ; created_at = timestamp
        }
      |> protocol_ok
    in
    let cause =
      Cause.create
        ~parent:event
        ~callee:session
        ~callee_source:source
        ~caller
        ~caller_source:(Cause.Caller_source.installed caller_source |> protocol_ok)
        ~caller_origin:(Some Unbound)
        ~path:[ reference ]
        ~targets:[]
        ~max_inherited_scopes:4
      |> protocol_ok
    in
    let after =
      A.Session_delta.apply before (Moderator_execution_changed event) |> protocol_ok
    in
    let update =
      PC.register_event
        None
        ~before:(A.Session_state.policy_context before |> protocol_ok)
        ~after:(A.Session_state.policy_context after |> protocol_ok)
        ~ledger:before.inference_ledger
        ~cause
      |> protocol_ok
    in
    (* Only the optional component changes: an unrelated source/queue/account
       mismatch cannot hide a missing final absence guard. *)
    let absent =
      SD.Transaction.begin_absent_policy (SD.authored after) ~limits:document_limits
      |> document_ok
    in
    let positive =
      { after with policy_canonical_usage = Some (PC.Update.candidate update) }
    in
    assert (
      Result.is_error
        (SD.Transaction.finish_encoded
           (SD.Transaction.with_value absent positive)
           ~next:positive
           ~limits:document_limits));
    let delta =
      A.Session_delta.Batch
        [ Run_state_changed index
        ; Moderator_execution_changed event
        ; Policy_cause_changed update
        ]
    in
    assert (
      A.Session_delta.requires_policy_tracking
        delta
        ~previous:initial.policy_canonical_usage);
    let transition =
      A.Session_transition.apply ~now:timestamp initial ~delta ~payloads:[] |> protocol_ok
    in
    let admitted =
      A.Session_document_transition.admit_encoded
        (carried initial)
        ~delta
        ~next:transition.state
        ~limits:document_limits
      |> document_ok
      |> SD.Admitted.document
    in
    [%test_eq: int] 14 (D.Document.version admitted);
    assert (
      List.mem
        (D.Document.required_semantics admitted)
        P.Session_storage_semantics.policy_canonical_usage
        ~equal:String.equal);
    let restored =
      SD.decode admitted ~limits:document_limits |> document_ok |> SD.value
    in
    let component state =
      Option.value_exn state.A.Session_state.policy_canonical_usage
      |> PC.to_document
      |> protocol_ok
      |> D.Document.json
    in
    assert (D.Json.equal (component transition.state) (component restored));
    (* One actual H traverses the new paired canonical leaf in both native and
       carried-document owners. The seed remains pure DATA, not admission to IO. *)
    let module L = A.Inference_ledger in
    let module Canonical = A.Canonical_usage_update in
    let module Occurrence = A.Goal_descendant_occurrence in
    let after, handle, tracking =
      Inference_ledger_state_tests.admit restored restored.inference_ledger
    in
    assert (L.equal_tracking tracking Tracked);
    let revision = Int64.succ restored.counters.revision in
    let occurrence =
      Occurrence.Frame.create
        ~session
        ~generation:restored.identity.generation
        ~candidate_session_revision:revision
      |> protocol_ok
      |> fun frame ->
      Occurrence.attempt
        frame
        ~ordinal:(L.Handle.ordinal handle)
        ~operation_id:(L.Handle.operation_id handle)
        ~invocation_id:(L.Handle.invocation_id handle)
      |> protocol_ok
    in
    let policy_admission =
      PC.Admission.create
        ~cause:(Cause.key cause)
        ~work:(Event event.context.id)
        ~live_prefix:(A.Session_state.policy_context restored |> protocol_ok)
        ~descendants_after:restored.goal_descendants
        ~canonical:(Attempt (handle, tracking))
        ~occurrence
      |> protocol_ok
    in
    let canonical =
      Canonical.prepare
        ~policy_usage:(Option.value_exn restored.policy_canonical_usage)
        ~policy_admission
        restored.inference_ledger
        ~after
        ~accounts:restored.goal_accounts
        ~catalog:restored.goal_catalog
        ~delegated_usage:restored.delegated_usage
        ~admission:None
      |> protocol_ok
    in
    assert (not (Canonical.accounts_changed canonical));
    let paired_delta =
      A.Session_delta.Policy_inference_ledger_changed
        ( Canonical.ledger canonical
        , Canonical.delegated_usage canonical
        , Option.value_exn (Canonical.policy_update canonical) )
    in
    let paired_transition =
      A.Session_transition.apply ~now:timestamp restored ~delta:paired_delta ~payloads:[]
      |> protocol_ok
    in
    [%test_eq: int64] revision paired_transition.state.counters.revision;
    let paired_document =
      A.Session_document_transition.admit_encoded
        (SD.decode admitted ~limits:document_limits |> document_ok)
        ~delta:paired_delta
        ~next:paired_transition.state
        ~limits:document_limits
      |> document_ok
      |> SD.Admitted.document
    in
    let paired_restored =
      SD.decode paired_document ~limits:document_limits |> document_ok |> SD.value
    in
    [%test_eq: int64] revision paired_restored.counters.revision;
    assert (D.Json.equal (component paired_transition.state) (component paired_restored));
    let ledger_document state =
      L.to_document state.A.Session_state.inference_ledger
      |> Inference_ledger_state_tests.ledger_ok
      |> D.Document.json
    in
    assert (
      D.Json.equal
        (ledger_document paired_transition.state)
        (ledger_document paired_restored));
    List.iter [ paired_transition.state; paired_restored ] ~f:(fun state ->
      let row =
        L.find state.inference_ledger ~ordinal:(L.Handle.ordinal handle)
        |> Option.value_exn
      in
      assert (L.Handle.equal handle (L.Row.handle row));
      let binding =
        match PC.bindings (Option.value_exn state.policy_canonical_usage) with
        | [ binding ] -> binding
        | [] | _ :: _ :: _ ->
          failwith "paired H did not retain its one exact causal binding"
      in
      [%test_eq: int64] (L.Handle.ordinal handle) (PC.Binding.ordinal binding);
      assert (Cause.Key.equal (Cause.key cause) (PC.Binding.cause binding)));
    (* The paired leaf composes both feature fences. This is a codec check of
       the actual candidate above; partition DATA never grants execution. *)
    let encoded_pair usage =
      A.Session_delta_document.create
        (A.Session_delta.Policy_inference_ledger_changed
           ( Canonical.ledger canonical
           , usage
           , Option.value_exn (Canonical.policy_update canonical) ))
        ~limits:document_limits
        ~state_document:SD.authored
      |> document_ok
      |> A.Session_delta_document.document
    in
    let partition_marker = P.Session_storage_semantics.delegated_usage in
    let policy_marker = P.Session_storage_semantics.policy_canonical_usage in
    let without_partition = encoded_pair None in
    assert (
      List.mem
        (D.Document.required_semantics without_partition)
        policy_marker
        ~equal:String.equal);
    assert (
      not
        (List.mem
           (D.Document.required_semantics without_partition)
           partition_marker
           ~equal:String.equal));
    let partition =
      A.Delegated_usage.create
        ~reference:Delegated_usage_persistence_tests.reference
        ~ledger:initial.inference_ledger
      |> protocol_ok
      |> fun partition ->
      A.Delegated_usage.reconcile
        partition
        ~before:initial.inference_ledger
        ~after:(Canonical.ledger canonical)
      |> protocol_ok
    in
    A.Delegated_usage.validate partition ~ledger:(Canonical.ledger canonical)
    |> protocol_ok;
    let with_partition = encoded_pair (Some partition) in
    let semantics = D.Document.required_semantics with_partition in
    assert (List.mem semantics policy_marker ~equal:String.equal);
    assert (List.mem semantics partition_marker ~equal:String.equal);
    A.Session_delta_document.decode with_partition ~limits:document_limits
    |> document_ok
    |> ignore;
    let missing_partition_marker =
      set
        (D.Document.json with_partition)
        "required_semantics"
        (`Array
            (List.filter_map semantics ~f:(fun name ->
               if String.equal name partition_marker then None else Some (`String name))))
      |> D.Document.inspect ~limits:document_limits
      |> document_ok
    in
    assert (
      List.mem
        (D.Document.required_semantics missing_partition_marker)
        policy_marker
        ~equal:String.equal);
    assert (
      Result.is_error
        (A.Session_delta_document.decode missing_partition_marker ~limits:document_limits));
    (* Generation changes require ended physical attempts. Commit interruption
       through the same paired canonical edge before testing reset conservation. *)
    let paired_restored, paired_document =
      let after =
        L.set_state
          paired_restored.inference_ledger
          handle
          (Interrupted { reason = Host_interrupted; delivery = Definitely_not_submitted })
        |> Inference_ledger_state_tests.ledger_ok
      in
      let update =
        Canonical.prepare
          ~policy_usage:(Option.value_exn paired_restored.policy_canonical_usage)
          paired_restored.inference_ledger
          ~after
          ~accounts:paired_restored.goal_accounts
          ~catalog:paired_restored.goal_catalog
          ~delegated_usage:paired_restored.delegated_usage
          ~admission:None
        |> protocol_ok
      in
      let delta =
        A.Session_delta.inference_ledger_update
          ?policy_update:(Canonical.policy_update update)
          after
          ~delegated_usage:(Canonical.delegated_usage update)
      in
      let transition =
        A.Session_transition.apply ~now:timestamp paired_restored ~delta ~payloads:[]
        |> protocol_ok
      in
      let document =
        A.Session_document_transition.admit_encoded
          (SD.decode paired_document ~limits:document_limits |> document_ok)
          ~delta
          ~next:transition.state
          ~limits:document_limits
        |> document_ok
        |> SD.Admitted.document
      in
      let decoded =
        SD.decode document ~limits:document_limits |> document_ok |> SD.value
      in
      assert (D.Json.equal (component transition.state) (component decoded));
      assert (D.Json.equal (ledger_document transition.state) (ledger_document decoded));
      decoded, document
    in
    (* Reset the actual admitted H as well as its cause, preserving factual usage. *)
    let reset =
      A.Administration.reset
        paired_restored
        { keep_history = true
        ; keep_tasks = true
        ; keep_grants = true
        ; keep_labels = true
        ; workspace_instance = None
        }
      |> protocol_ok
    in
    let reset_delta = A.Session_delta.Created reset in
    let reset_transition =
      A.Session_transition.apply
        ~now:timestamp
        paired_restored
        ~delta:reset_delta
        ~payloads:[]
      |> protocol_ok
    in
    let reset_document =
      A.Session_document_transition.admit_encoded
        (SD.decode paired_document ~limits:document_limits |> document_ok)
        ~delta:reset_delta
        ~next:reset_transition.state
        ~limits:document_limits
      |> document_ok
      |> SD.Admitted.state_document
      |> SD.value
    in
    [%test_eq: int]
      (paired_restored.identity.generation + 1)
      reset_document.identity.generation;
    assert (D.Json.equal (component reset) (component reset_document));
    let generation_delta =
      A.Session_delta.Reset_generation (paired_restored.identity.generation + 1)
    in
    let generation_transition =
      A.Session_transition.apply
        ~now:timestamp
        paired_restored
        ~delta:generation_delta
        ~payloads:[]
      |> protocol_ok
    in
    let generation_document =
      A.Session_document_transition.admit_encoded
        (SD.decode paired_document ~limits:document_limits |> document_ok)
        ~delta:generation_delta
        ~next:generation_transition.state
        ~limits:document_limits
      |> document_ok
      |> SD.Admitted.state_document
      |> SD.value
    in
    assert (
      D.Json.equal (component generation_transition.state) (component generation_document));
    assert (
      Result.is_error
        (A.Session_state.upgrade_schema { restored with schema_version = 27 }));
    let delta_document =
      A.Session_delta_document.create
        delta
        ~limits:document_limits
        ~state_document:SD.authored
      |> document_ok
      |> A.Session_delta_document.document
    in
    [%test_eq: int] 10 (D.Document.version delta_document);
    assert (
      List.mem
        (D.Document.required_semantics delta_document)
        P.Session_storage_semantics.policy_canonical_usage
        ~equal:String.equal);
    let remove json name =
      match json with
      | `Object fields -> `Object (List.Assoc.remove fields name ~equal:String.equal)
      | _ -> failwith "expected object"
    in
    let inspect raw = D.Document.inspect raw ~limits:document_limits |> document_ok in
    let old_state =
      set (D.Document.json admitted) "schema_version" (`Number "12") |> inspect
    in
    [%test_eq: int] 12 (D.Document.version old_state);
    assert (Result.is_error (SD.decode old_state ~limits:document_limits));
    let old_delta =
      set (D.Document.json delta_document) "schema_version" (`Number "8") |> inspect
    in
    [%test_eq: int] 8 (D.Document.version old_delta);
    assert (
      Result.is_error (A.Session_delta_document.decode old_delta ~limits:document_limits));
    let unmarked = remove (D.Document.json admitted) "required_semantics" |> inspect in
    assert (Result.is_error (SD.decode unmarked ~limits:document_limits));
    let unmarked_delta =
      remove (D.Document.json delta_document) "required_semantics" |> inspect
    in
    assert (
      Result.is_error
        (A.Session_delta_document.decode unmarked_delta ~limits:document_limits));
    let marker = P.Session_storage_semantics.policy_canonical_usage in
    let old_reader =
      D.Domain_codec.create
        ~limits:document_limits
        ~kind:(D.Document.kind admitted)
        ~version:(D.Document.version admitted)
        ~shape:SD.shape
        ~supported_semantics:
          (List.filter P.Session_storage_semantics.supported_children ~f:(fun name ->
             not (String.equal name marker)))
        ~decode:(fun _ -> Ok ())
        ~encode:(fun () -> Ok (`Object []))
      |> document_ok
    in
    (match D.Domain_codec.decode old_reader admitted with
     | Error (D.Error.Required_semantics_unknown actual) ->
       [%test_eq: string] marker actual
     | Error error -> raise_s [%sexp (error : D.Error.t)]
     | Ok _ -> failwith "reader without causal semantics accepted positive accounting");
    let created =
      A.Session_delta_document.create
        (A.Session_delta.Created transition.state)
        ~limits:document_limits
        ~state_document:SD.authored
      |> document_ok
      |> A.Session_delta_document.document
    in
    assert (List.mem (D.Document.required_semantics created) marker ~equal:String.equal);
    let old_created =
      set (D.Document.json created) "schema_version" (`Number "8") |> inspect
    in
    [%test_eq: int] 8 (D.Document.version old_created);
    assert (
      Result.is_error
        (A.Session_delta_document.decode old_created ~limits:document_limits));
    (* Like Fixtures.snapshot_of_state, this codec-only wrapper uses a fixture
       anchor for progressed State. It is not a physical journal/ACK proof. *)
    let snapshot =
      Store.Snapshot.create
        ~limits:document_limits
        ~session_id:restored.identity.session_id
        ~transaction_sequence:restored.counters.transaction_sequence
        ~transaction_hash:
          (if Int64.equal restored.counters.transaction_sequence 0L
           then None
           else Some (String.make 64 'a'))
        ~event_sequence:restored.counters.event_sequence
        ~created_at:timestamp
        ~prompt_artifact:(P.Id.Prompt_revision.to_string restored.spec.prompt_revision_id)
        ~workspace_identity:restored.spec.workspace_instance.conflict_domain
        ~payload:admitted
      |> store_ok
    in
    let snapshot =
      Store.Snapshot.restore (Store.Snapshot.stored snapshot) ~limits:document_limits
      |> store_ok
    in
    assert (D.Json.equal (D.Document.json snapshot.payload) (D.Document.json admitted));
    let transaction =
      Store.Transaction.create
        ~limits:document_limits
        ~session_id:restored.identity.session_id
        ~generation:restored.identity.generation
        ~transaction_sequence:1L
        ~previous_transaction_hash:None
        ~session_revision:1L
        ~first_event_sequence:None
        ~last_event_sequence:None
        ~accepted_at_ns:0L
        ~command_audit:None
        ~delta:delta_document
        ~durable_events:[]
      |> store_ok
    in
    let decoded =
      Store.Transaction.decode (Store.Transaction.encode transaction) |> store_ok
    in
    [%test_eq: string]
      (Store.Transaction.hash transaction)
      (Store.Transaction.hash decoded);
    assert (D.Json.equal (D.Document.json decoded.delta) (D.Document.json delta_document)))
;;
