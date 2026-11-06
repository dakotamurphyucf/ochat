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
        set raw "version" (`Number "11")
        |> D.Document.inspect ~limits:document_limits
        |> document_ok
        |> SD.decode ~limits:document_limits
        |> document_ok
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
