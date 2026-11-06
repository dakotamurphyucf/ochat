open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module D = Document_schema
module GD = A.Goal_descendants
module C = A.Child_contributions
module S = A.Child_usage_snapshot
module L = A.Inference_ledger
module O = Agent_store.Delegation_goal_origin
module R = Agent_store.Delegation_store
module Pending = A.Goal_descendant_pending
module Included = A.Goal_descendant_included
module Occurrence = A.Goal_descendant_occurrence
module SD = A.Session_state_document
module Persistence = A.Session_persistence
module Store = Agent_store
module Bootstrap = Goal_accounting_bootstrap_tests
module Depth = Descendant_depth_state_tests

let member = Goal_catalog_tests.member
let set = Goal_catalog_tests.set
let ledger = Goal_accounting_persistence_tests.ledger
let sentinel = `Object [ "retained", `Number "1.00" ]
let gd state = Option.value_exn state.A.Session_state.goal_descendants
let children state = Option.value_exn state.A.Session_state.child_contributions
let gd_json state = GD.to_document (gd state) |> protocol_ok |> D.Document.json
let children_json state = C.to_jsonaf (children state) |> protocol_ok

(* Imported initial domain DATA is not a physical delegation grant. All writes
   below use the actual State transition, document, journal and recovery owners. *)
let decorated state =
  let raw = gd_json state in
  let raw = set raw "future_physical_descendants" sentinel in
  let payload = member raw "payload" in
  let decorate_array raw f =
    match raw with
    | `Array values -> `Array (List.map values ~f)
    | _ -> failwith "expected array"
  in
  let payload =
    set
      payload
      "heads"
      (decorate_array (member payload "heads") (fun head ->
         set head "future_physical_head" sentinel))
    |> fun payload ->
    set
      payload
      "roots"
      (decorate_array (member payload "roots") (fun root ->
         set
           root
           "branches"
           (decorate_array (member root "branches") (fun branch ->
              set branch "future_physical_branch" sentinel))))
  in
  let accounting =
    set raw "payload" payload
    |> D.Document.inspect ~limits:document_limits
    |> document_ok
    |> GD.of_document
    |> protocol_ok
  in
  let raw = children_json state in
  let payload = member raw "payload" in
  let payload =
    set
      payload
      "children"
      (decorate_array (member payload "children") (fun slot ->
         set slot "future_physical_slot" sentinel))
  in
  let children =
    set raw "payload" payload
    |> fun raw ->
    set raw "future_physical_children" sentinel |> C.of_jsonaf |> protocol_ok
  in
  Bootstrap.carried
    { state with goal_descendants = Some accounting; child_contributions = Some children }
;;

let transition state delta =
  A.Session_transition.apply ~now:timestamp state ~delta ~payloads:[] |> protocol_ok
;;

let reserve state scope occurrence =
  match
    GD.reserve
      (gd state)
      ~children:(children state)
      ~scope
      ~occurrence
      ~local_count:0L
      ~ceiling:None
    |> protocol_ok
  with
  | Reserved (update, pending) ->
    transition state (Goal_descendants_changed update), pending
  | Already_pending _ | Blocked _ -> failwith "expected fresh physical reservation"
;;

let occurrence child ordinal =
  let frame =
    Occurrence.Frame.create
      ~session:(GD.session (gd child))
      ~generation:child.A.Session_state.identity.generation
      ~candidate_session_revision:(Int64.succ child.counters.revision)
    |> protocol_ok
  in
  Occurrence.attempt frame ~ordinal ~operation_id:None ~invocation_id:None |> protocol_ok
;;

let record previous pending =
  let child = SD.value previous in
  let _, _, prepared = Inference_tracking_tests.prepared () in
  let after, handle, tracking =
    L.admit
      child.A.Session_state.inference_ledger
      ~source:
        (Transcript.Source_id.of_string "physical-descendant" |> Result.ok_or_failwith)
      ~relation:Root
      ~operation_id:None
      ~invocation_id:None
      ~configuration:(Inference_runtime.Prepared.configuration prepared)
    |> ledger
  in
  let after, _ =
    L.observe after handle (Inference_tracking_tests.usage handle 0L 7L) |> ledger
  in
  let included =
    GD.record_included
      (gd child)
      ~children:(children child)
      ~before:child.inference_ledger
      ~after
      ~canonical:(Attempt (handle, tracking))
      ~occurrence:(Pending.occurrence pending)
      ~receipts:[ Included.create pending ]
    |> protocol_ok
  in
  let own =
    A.Delegated_usage.reconcile
      (Option.value_exn child.delegated_usage)
      ~before:child.inference_ledger
      ~after
    |> protocol_ok
  in
  let prepared =
    transition
      child
      (Batch
         [ Delegated_inference_ledger_changed (after, own)
         ; Goal_descendants_changed included
         ])
  in
  let expected_revision = Int64.succ child.counters.revision in
  [%test_eq: int64] expected_revision prepared.state.counters.revision;
  let admit next =
    A.Session_document_transition.admit_encoded
      previous
      ~delta:prepared.delta
      ~next
      ~limits:document_limits
  in
  (* The actual carrier and canonical delta are identical in all three calls.
     Direct ordered admission must check the published revision before issuing
     its admitted encoding, even without native or metadata-stamp wrappers. *)
  List.iter
    [ child.counters.revision; Int64.succ expected_revision ]
    ~f:(fun revision ->
      let wrong =
        { prepared.state with counters = { prepared.state.counters with revision } }
      in
      assert (Result.is_error (admit wrong)));
  let admitted = admit prepared.state |> document_ok in
  let delta =
    A.Session_delta_document.create
      prepared.delta
      ~limits:document_limits
      ~state_document:SD.authored
    |> document_ok
  in
  let replay ?transaction_metadata () =
    A.Session_delta_document.apply
      delta
      ?transaction_metadata
      ~limits:document_limits
      previous
  in
  assert (Result.is_error (replay ()));
  let metadata revision =
    A.Session_delta_document.Transaction_metadata.create
      ~updated_at:prepared.state.identity.updated_at
      ~revision
      ~transaction_sequence:prepared.state.counters.transaction_sequence
      ~last_event_sequence:(Some prepared.state.counters.event_sequence)
    |> document_ok
  in
  List.iter
    [ child.counters.revision; Int64.succ expected_revision ]
    ~f:(fun revision ->
      assert (Result.is_error (replay ~transaction_metadata:(metadata revision) ())));
  let replayed =
    replay ~transaction_metadata:(metadata expected_revision) () |> document_ok
  in
  let replayed = SD.encode replayed ~limits:document_limits |> document_ok in
  assert (
    D.Json.equal
      (D.Document.json replayed)
      (D.Document.json (SD.Admitted.document admitted)));
  prepared
;;

let snapshot child =
  let snapshot = A.Child_accounting.snapshot child |> protocol_ok |> Option.value_exn in
  [%test_eq: int64] 1L (S.Coverage.unobserved_children (S.coverage snapshot));
  List.iter
    (S.goal_scopes snapshot |> Option.value_exn)
    ~f:(fun scoped ->
      assert (S.Partition.equal (S.Goal_scope.own scoped) (S.own snapshot));
      [%test_eq: int64] 0L (S.Coverage.unobserved_children (S.Goal_scope.coverage scoped)));
  snapshot
;;

let import root snapshot =
  GD.import (gd root) ~children:(children root) ~snapshot
  |> protocol_ok
  |> fun update -> transition root (Goal_descendants_changed update)
;;

let stored persistence = Persistence.Restored.state (Persistence.restored persistence)

let commit persistence prepared =
  Persistence.commit
    persistence
    ~command_audit:None
    ~previous:(stored persistence)
    prepared
  |> protocol_ok
;;

let branch state = GD.branches (gd state) ~goal_id:Goal_run_tests.goal_id |> List.hd_exn

let assert_confirmed state reference prefix =
  let branch = branch state in
  [%test_eq: int64] prefix (GD.Branch.confirmed_prefix branch);
  assert (Option.is_none (GD.Branch.pending branch));
  let snapshot =
    C.snapshot (children state) ~reference |> protocol_ok |> Option.value_exn
  in
  [%test_eq: int64] prefix (S.Partition.marks (S.own snapshot)).attempt;
  let latest =
    S.goal_scopes snapshot
    |> Option.value_exn
    |> List.hd_exn
    |> S.Goal_scope.included
    |> Option.value_exn
  in
  [%test_eq: int64] prefix (Pending.next_prefix (Included.pending latest))
;;

let assert_same_domain expected actual =
  assert (D.Json.equal (gd_json expected) (gd_json actual));
  assert (D.Json.equal (children_json expected) (children_json actual));
  let ledger state =
    L.to_document state.A.Session_state.inference_ledger |> ledger |> D.Document.json
  in
  assert (D.Json.equal (ledger expected) (ledger actual))
;;

(* Actual persisted State projections below exercise the local policy adapter.
   Changing a budget in a temporary command candidate grants no runtime permit
   and does not add writes to the physical recovery scenario. *)
let policy_candidate state ~attempts ~tokens =
  let goal = Goal_run_tests.current_goal state in
  let budget =
    P.Goal.Budget.create
      ~admitted_turns:(Some 1L)
      ~admitted_attempts:attempts
      ~observed_tokens:tokens
      ~deadline:None
    |> protocol_ok
  in
  let candidate, _ =
    Bootstrap.project
      state
      ~session:(P.Goal.session goal)
      ~key:"local-policy-probe"
      (Change_budget (P.Goal.id goal, budget))
  in
  A.Session_state.validate candidate |> protocol_ok;
  candidate
;;

let local_admission state dimension =
  let goal = Goal_run_tests.current_goal state in
  let episode = P.Goal.episode goal |> Option.value_exn in
  let run =
    A.Run_state.runs (Option.value_exn state.A.Session_state.run_state)
    |> List.find ~f:(P.Goal_episode.matches_run episode)
    |> Option.value_exn
  in
  let attribution = A.Goal_work_attribution.of_run state run |> protocol_ok in
  let before =
    A.Goal_catalog.to_jsonaf (Option.value_exn state.goal_catalog) |> protocol_ok
  in
  let decision =
    A.Goal_charge_admission.prepare state ~attribution ~now:timestamp ~dimension
    |> protocol_ok
  in
  let after =
    A.Goal_catalog.to_jsonaf (Option.value_exn state.goal_catalog) |> protocol_ok
  in
  assert (D.Json.equal before after);
  assert (P.Goal.State.equal (P.Goal.state goal) (Open Enabled));
  decision
;;

let assert_pending_local_policy state known =
  let candidate =
    policy_candidate state ~attempts:(Some (Int64.succ known)) ~tokens:None
  in
  (match local_admission candidate Attempt with
   | Deferred { goal_id; reason = Reserved } ->
     assert (P.Id.Goal.equal goal_id Goal_run_tests.goal_id)
   | _ -> failwith "persisted descendant pending did not defer local admission");
  let unlimited = policy_candidate state ~attempts:None ~tokens:None in
  match local_admission unlimited Attempt with
  | Ready _ -> ()
  | _ -> failwith "unlimited local count was blocked by pending/unknown coverage"
;;

let assert_committed_local_policy state known =
  let candidate = policy_candidate state ~attempts:(Some known) ~tokens:None in
  (match local_admission candidate Attempt with
   | Held { goal_id; reason = Budget_gate Admitted_attempts; delta = Some _ } ->
     assert (P.Id.Goal.equal goal_id Goal_run_tests.goal_id)
   | _ -> failwith "accepted descendant count did not hold next local admission");
  (match local_admission candidate Host_turn with
   | Ready _ -> ()
   | _ -> failwith "descendant Attempt incorrectly filled local Turn ceiling");
  let tokens =
    P.Goal.Budget.Token_stop.create ~component:Input ~ceiling:10L |> protocol_ok
  in
  let candidate = policy_candidate state ~attempts:None ~tokens:(Some tokens) in
  let goal = Goal_run_tests.current_goal candidate in
  let account =
    A.Goal_accounts.find (Option.value_exn candidate.goal_accounts) (P.Goal.id goal)
    |> Option.value_exn
    |> fun account ->
    A.Goal_accounting_document.resolve account ~goal ~ledger:candidate.inference_ledger
    |> protocol_ok
  in
  [%test_eq: int64] 0L (A.Goal_accounting.admitted_attempts account);
  let activation =
    A.Goal_charge_admission.check_activation
      candidate
      ~goal
      ~accounting:account
      ~now:timestamp
    |> protocol_ok
  in
  match local_admission candidate Attempt, activation with
  | Ready _, Allow when Int64.equal known 1L -> ()
  | Held { reason = Budget_gate Observed_tokens; _ }, Hold (Budget_gate Observed_tokens)
    when Int64.equal known 2L -> ()
  | _ -> failwith "scoped child observations did not govern local fresh/activation policy"
;;

let%expect_test
    "positive scoped receipts and paired import survive failure, checkpoint, and next \
     canonical write"
  =
  List.iter [ false; true ] ~f:(fun checkpoint ->
    Goal_run_tests.with_model
      (fun ~basis ~held ~admission:_ ~intent:_ ~delta bound ->
         let held_goal =
           A.Goal_catalog.find held Goal_run_tests.goal_id |> Option.value_exn
         in
         let accounts =
           A.Goal_accounts.bootstrap
             A.Goal_accounts.empty
             ~goal:held_goal
             ~ledger:basis.inference_ledger
           |> protocol_ok
         in
         let account =
           A.Goal_accounts.find accounts Goal_run_tests.goal_id |> Option.value_exn
         in
         let children =
           C.create
             ~inventory:Complete_inventory
             ~max_children:(GD.Profile.max_branches goal_descendant_profile)
             ~max_encoded_bytes:
               (GD.Profile.child_max_encoded_bytes goal_descendant_profile)
           |> protocol_ok
         in
         let root =
           GD.register_new_root
             None
             ~host_profile:goal_descendant_profile
             ~session:(P.Goal.session held_goal)
             ~children:(Some children)
             ~goal:held_goal
             ~account
             ~before_ledger:basis.inference_ledger
           |> protocol_ok
           |> GD.Update.accounting
         in
         let initial =
           { basis with
             goal_accounts = Some accounts
           ; goal_descendants = Some root
           ; child_contributions = Some children
           }
         in
         let root = (transition initial delta).state in
         ignore (bound : A.Session_state.t);
         { root with
           counters =
             { root.counters with transaction_sequence = 0L; event_sequence = 0L }
         })
      ~after_close:(fun env workspace root ->
        Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
          let goal = Goal_run_tests.current_goal root in
          let own_reference =
            R.reference_of_jsonaf
              (`Object
                  [ ( "key"
                    , `Object
                        [ ( "parent_session_id"
                          , P.Id.Session.to_json root.identity.session_id )
                        ; ( "parent_generation"
                          , `Number (Int.to_string root.identity.generation) )
                        ; "principal_id", P.Id.Principal.to_json principal_id
                        ; "idempotency_key", `String "physical-positive"
                        ] )
                  ; "child_session_id", P.Id.Session.to_json second_session_id
                  ; "revision_id", P.Id.Prompt_revision.to_json prompt_revision_id
                  ; "request_sha256", `String (String.make 64 'a')
                  ; "admission_sha256", `String (String.make 64 'b')
                  ])
            |> Result.map_error ~f:Store.Store_error.to_protocol_error
            |> protocol_ok
          in
          let anchor =
            O.Anchor.create
              ~parent_session_id:root.identity.session_id
              ~parent_generation:root.identity.generation
              ~principal_id
              ~key:own_reference.key.idempotency_key
              ~child_session_id:own_reference.child_session_id
              ~transaction_id:
                (P.Id.Transaction.of_string "txn_physical_positive" |> protocol_ok)
            |> protocol_ok
          in
          let scope =
            O.Scope.create
              ~owner:(P.Goal.session goal)
              ~goal_id:(P.Goal.id goal)
              ~source:(P.Goal.source goal)
              ~creator_episode:(P.Goal.episode goal |> Option.value_exn)
              ~branch:anchor
            |> protocol_ok
          in
          let origin =
            O.associated
              ~max_scopes:(GD.Profile.max_scopes goal_descendant_profile)
              [ scope ]
            |> protocol_ok
          in
          let register =
            GD.register_branch
              (gd root)
              ~children:(children root)
              ~scope
              ~reference:own_reference
              ~origin
            |> protocol_ok
          in
          let root = (transition root (Goal_descendants_changed register)).state in
          let root =
            { root with
              counters =
                { root.counters with transaction_sequence = 0L; event_sequence = 0L }
            }
          in
          let root_document = decorated root in
          let child = Depth.new_child_state ~parent:root ~scope ~own_reference in
          (* An unobserved unrelated grandchild affects whole-child coverage,
             but must never taint the inherited Goal's projection. *)
          let unrelated =
            Depth.reference
              ~parent_session_id:child.identity.session_id
              ~parent_generation:child.identity.generation
              ~child_session_id:
                (P.Id.Session.of_string "ses_unrelated_grandchild" |> protocol_ok)
              ~name:"unrelated-grandchild"
          in
          let register =
            GD.register_child
              (gd child)
              ~children:(children child)
              ~reference:unrelated
              ~origin:O.unrelated
            |> protocol_ok
          in
          let child = (transition child (Goal_descendants_changed register)).state in
          (* This is the unpublished model supplied to the initial journal,
             not a transaction already appended to that journal. *)
          let child =
            { child with
              counters =
                { child.counters with transaction_sequence = 0L; event_sequence = 0L }
            }
          in
          ignore (snapshot child : S.t);
          let retained =
            A.Child_accounting.register
              root
              ~reference:own_reference
              ~max_children:1
              ~max_encoded_bytes:1
            |> protocol_ok
          in
          assert (D.Json.equal (C.to_jsonaf retained |> protocol_ok) (children_json root));
          let child_document = decorated child in
          let root_initial = Persistence.Restored.of_state_document root_document in
          let child_initial = Persistence.Restored.of_state_document child_document in
          let root_directory =
            Filename.concat workspace.canonical_root.native_path "positive-root-journal"
          in
          let child_directory =
            Filename.concat workspace.canonical_root.native_path "positive-child-journal"
          in
          let root_snapshots =
            Filename.concat workspace.canonical_root.native_path "positive-root-snapshots"
          in
          let child_snapshots =
            Filename.concat
              workspace.canonical_root.native_path
              "positive-child-snapshots"
          in
          List.iter [ root_snapshots; child_snapshots ] ~f:(fun directory ->
            Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / directory));
          let recover journal directory session_id initial =
            Store.Recovery.load
              ~env
              ~journal
              ~snapshot_directory:directory
              ~max_snapshot_payload_length:1048576
              ~session_id
              ~initial
              ~restore_snapshot:(Persistence.restore_snapshot ~limits:document_limits)
              ~apply:(Persistence.apply_transaction ~limits:document_limits)
              ~validate_transaction:
                (Persistence.validate_transaction ~limits:document_limits)
              ~validate:Persistence.validate
            |> store_ok
          in
          let make writer restored previous_hash reject =
            Persistence.create
              ~archive:(fun _ _ -> failwith "unexpected archive")
              ~pending_archive:None
              ~before_commit:
                (Some
                   (fun _ ->
                     if !reject
                     then
                       Error
                         (P.Error.create
                            Persistence_error
                            ~message:"before paired import write"
                            ~retryable:true
                            ())
                     else Ok ()))
              ~command_accepted:(fun _ _ -> ())
              ~writer
              ~durability:Flush
              ~limits:document_limits
              ~archive_limits:document_limits
              ~retention_preflight:None
              ~restored
              ~previous_transaction_hash:previous_hash
          in
          let checkpoint_at persistence directory =
            if checkpoint
            then
              ignore
                (Persistence.install_snapshot_at
                   persistence
                   ~env
                   ~directory
                   ~max_payload_length:1048576
                   ~transaction_hash:(Persistence.transaction_hash persistence)
                   (stored persistence)
                 |> store_ok
                 : Store.Snapshot.installed)
          in
          let expected_root = ref None in
          let expected_child = ref None in
          Eio.Switch.run (fun sw ->
            let journal = Bootstrap.journal env root_directory ~existing:false in
            Bootstrap.with_writer sw journal ~next:1L ~previous_hash:None (fun writer ->
              let reject = ref false in
              let root_writer = make writer root_initial None reject in
              let first_child = Persistence.Restored.state child_initial in
              let reserved, pending =
                reserve (stored root_writer) scope (occurrence first_child 1L)
              in
              commit root_writer reserved;
              assert_pending_local_policy (stored root_writer) 0L;
              Eio.Switch.run (fun sw ->
                let journal = Bootstrap.journal env child_directory ~existing:false in
                Bootstrap.with_writer
                  ~session_id:own_reference.child_session_id
                  sw
                  journal
                  ~next:1L
                  ~previous_hash:None
                  (fun writer ->
                     let child_writer = make writer child_initial None (ref false) in
                     commit
                       child_writer
                       (record
                          (Persistence.Restored.state_document
                             (Persistence.restored child_writer))
                          pending);
                     expected_child := Some (stored child_writer);
                     checkpoint_at child_writer child_snapshots));
              let child_recovered =
                recover
                  (Bootstrap.journal env child_directory ~existing:true)
                  child_snapshots
                  own_reference.child_session_id
                  child_initial
              in
              [%test_eq: int64] 1L child_recovered.latest_transaction_sequence;
              let child = Persistence.Restored.state child_recovered.state in
              assert_same_domain (Option.value_exn !expected_child) child;
              let before = stored root_writer in
              let prepared = import before (snapshot child) in
              let before_hash = Persistence.transaction_hash root_writer in
              let journal_payloads () =
                Store.Journal.scan journal
                |> store_ok
                |> fun (scan : Store.Journal.scan) ->
                List.map scan.entries ~f:(fun (entry : Store.Journal.entry) ->
                  Store.Frame.payload entry.frame)
              in
              let before_payloads = journal_payloads () in
              reject := true;
              (match
                 Persistence.commit
                   root_writer
                   ~command_audit:None
                   ~previous:before
                   prepared
               with
               | Error error ->
                 [%test_eq: string] error.P.Error.message "before paired import write"
               | Ok () -> failwith "paired import passed the injected persistence failure");
              assert (List.equal String.equal before_payloads (journal_payloads ()));
              assert_same_domain before (stored root_writer);
              assert_pending_local_policy (stored root_writer) 0L;
              assert (
                Option.equal
                  String.equal
                  before_hash
                  (Persistence.transaction_hash root_writer));
              assert (
                Pending.equal
                  pending
                  (GD.Branch.pending (branch (stored root_writer)) |> Option.value_exn));
              assert (
                Option.is_none
                  (C.snapshot (children (stored root_writer)) ~reference:own_reference
                   |> protocol_ok));
              reject := false;
              commit root_writer prepared;
              assert_confirmed (stored root_writer) own_reference 1L;
              assert_committed_local_policy (stored root_writer) 1L;
              expected_root := Some (stored root_writer);
              checkpoint_at root_writer root_snapshots));
          Eio.Switch.run (fun sw ->
            let journal = Bootstrap.journal env root_directory ~existing:true in
            let recovered = recover journal root_snapshots session_id root_initial in
            [%test_eq: int64] 2L recovered.latest_transaction_sequence;
            assert_same_domain
              (Option.value_exn !expected_root)
              (Persistence.Restored.state recovered.state);
            Bootstrap.with_writer
              sw
              journal
              ~next:3L
              ~previous_hash:recovered.latest_transaction_hash
              (fun writer ->
                 assert_committed_local_policy
                   (Persistence.Restored.state recovered.state)
                   1L;
                 let root_writer =
                   make
                     writer
                     recovered.state
                     recovered.latest_transaction_hash
                     (ref false)
                 in
                 let child_recovered =
                   recover
                     (Bootstrap.journal env child_directory ~existing:true)
                     child_snapshots
                     own_reference.child_session_id
                     child_initial
                 in
                 let child = Persistence.Restored.state child_recovered.state in
                 let reserved, pending =
                   reserve (stored root_writer) scope (occurrence child 2L)
                 in
                 commit root_writer reserved;
                 assert_pending_local_policy (stored root_writer) 1L;
                 Eio.Switch.run (fun sw ->
                   let journal = Bootstrap.journal env child_directory ~existing:true in
                   Bootstrap.with_writer
                     ~session_id:own_reference.child_session_id
                     sw
                     journal
                     ~next:2L
                     ~previous_hash:child_recovered.latest_transaction_hash
                     (fun writer ->
                        let child_writer =
                          make
                            writer
                            child_recovered.state
                            child_recovered.latest_transaction_hash
                            (ref false)
                        in
                        commit
                          child_writer
                          (record
                             (Persistence.Restored.state_document
                                (Persistence.restored child_writer))
                             pending);
                        expected_child := Some (stored child_writer)));
                 let child = Option.value_exn !expected_child in
                 commit root_writer (import (stored root_writer) (snapshot child));
                 assert_confirmed (stored root_writer) own_reference 2L;
                 expected_root := Some (stored root_writer)));
          let root =
            recover
              (Bootstrap.journal env root_directory ~existing:true)
              root_snapshots
              session_id
              root_initial
          in
          let child =
            recover
              (Bootstrap.journal env child_directory ~existing:true)
              child_snapshots
              own_reference.child_session_id
              child_initial
          in
          [%test_eq: int64] 4L root.latest_transaction_sequence;
          [%test_eq: int64] 2L child.latest_transaction_sequence;
          let root = Persistence.Restored.state root.state in
          let child = Persistence.Restored.state child.state in
          assert_same_domain (Option.value_exn !expected_root) root;
          assert_same_domain (Option.value_exn !expected_child) child;
          assert_confirmed root own_reference 2L;
          assert_committed_local_policy root 2L;
          List.iter [ root; child ] ~f:(fun state ->
            assert (
              D.Json.equal (member (gd_json state) "future_physical_descendants") sentinel);
            assert (
              D.Json.equal
                (member (children_json state) "future_physical_children")
                sentinel));
          let branch = member (gd_json root) "payload" |> fun raw -> member raw "roots" in
          (match branch with
           | `Array [ root ] ->
             (match member root "branches" with
              | `Array [ branch ] ->
                assert (D.Json.equal (member branch "future_physical_branch") sentinel)
              | _ -> failwith "expected physical branch")
           | _ -> failwith "expected physical root");
          let heads = member (member (gd_json child) "payload") "heads" in
          (match heads with
           | `Array [ head ] ->
             assert (D.Json.equal (member head "future_physical_head") sentinel)
           | _ -> failwith "expected inherited head");
          match member (member (children_json root) "payload") "children" with
          | `Array [ slot ] ->
            assert (D.Json.equal (member slot "future_physical_slot") sentinel)
          | _ -> failwith "expected paired child slot")));
  print_endline
    "journal and checkpoint reopen retain positive heads and atomic confirmation; failed \
     import preserves pending+empty slot; reserved second canonical write reopens \
     exactly with unknowns";
  [%expect
    {| journal and checkpoint reopen retain positive heads and atomic confirmation; failed import preserves pending+empty slot; reserved second canonical write reopens exactly with unknowns |}]
;;
