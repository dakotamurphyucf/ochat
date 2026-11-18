open! Core
open Agent_server_test_support
open Retry_support
module Tree = Descendant_ancestry_support
module Goal = Goal_service_tests
module Direct = Descendant_turn_adapter_tests
module Root_barrier = Descendant_history_adapter_tests.Root_barrier
module GD = A.Goal_descendants
module D = Agent_store.Delegation_store
module O = Agent_store.Delegation_goal_origin
module S = A.Child_usage_snapshot
module L = A.Inference_ledger
module G = P.Goal
module Included = A.Goal_descendant_included
module Pending = A.Goal_descendant_pending
module Occurrence = A.Goal_descendant_occurrence

let branch current (member : Tree.member) =
  GD.branches
    (Option.value_exn current.A.Session_state.goal_descendants)
    ~goal_id:Goal.goal_id
  |> List.find_exn ~f:(fun branch ->
    O.Scope.equal member.scope (GD.Branch.scope branch)
    && D.Reference.equal member.reference (GD.Branch.reference branch))
;;

let included branch =
  match GD.Branch.last branch with
  | Some (Included receipt) -> receipt
  | None | Some (No_writer_closed _) -> failwith "actual root lacks Included receipt"
;;

let assert_marks partition ~turns ~attempts =
  let marks = S.Partition.marks partition in
  [%test_eq: int64] turns marks.turn;
  [%test_eq: int64] attempts marks.attempt;
  [%test_eq: int64] 0L marks.untracked_turns;
  [%test_eq: int64] 0L marks.untracked_attempts
;;

let assert_snapshot snapshot (member : Tree.member) receipt =
  assert (D.Reference.equal member.reference (S.reference snapshot));
  Tree.assert_coverage (S.coverage snapshot);
  S.validate_origin
    snapshot
    ~origin:(Option.value_exn member.record.admission.goal_origin)
  |> protocol_ok;
  let scoped =
    match S.goal_scopes snapshot |> Option.value_exn with
    | [ scoped ] -> scoped
    | [] | _ :: _ :: _ -> failwith "snapshot lost its one genuine inherited scope"
  in
  assert (O.Scope.equal member.scope (S.Goal_scope.scope scoped));
  assert (S.Partition.equal (S.own snapshot) (S.Goal_scope.own scoped));
  assert (
    S.Partition.equal
      (S.Descendants.partition (S.descendants snapshot))
      (S.Descendants.partition (S.Goal_scope.descendants scoped)));
  Tree.assert_coverage (S.Goal_scope.coverage scoped);
  assert (Option.exists (S.Goal_scope.included scoped) ~f:(Included.equal receipt));
  scoped
;;

module Probe = struct
  type t =
    { tree : Tree.t
    ; root_account : A.Goal_accounting.t
    ; root_marks : A.Canonical_usage_fold.Marks.t
    ; intermediate_own : S.Partition.t
    ; intermediate_branch : GD.Branch.t
    ; sibling_branch : GD.Branch.t
    ; sibling_snapshot : S.t
    ; turn_ceiling : int64
    ; attempt_ceiling : int64
    ; providers_before : int
    ; entered : unit Eio.Promise.t
    ; enter : unit Eio.Promise.u
    ; release : unit Eio.Promise.t
    ; resume : unit Eio.Promise.u
    ; mutable submitted : (Included.t * L.Handle.t) option
    }

  let create tree =
    let root = state tree.Tree.root in
    let root_account = Goal.accounting root in
    let root_marks = Tree.root_marks root in
    let turn_ceiling =
      Int64.succ Int64.(A.Goal_accounting.admitted_turns root_account + root_marks.turn)
    in
    let attempt_ceiling =
      Int64.succ
        Int64.(A.Goal_accounting.admitted_attempts root_account + root_marks.attempt)
    in
    let budget =
      G.Budget.create
        ~admitted_turns:(Some turn_ceiling)
        ~admitted_attempts:(Some attempt_ceiling)
        ~observed_tokens:None
        ~deadline:None
      |> protocol_ok
    in
    Goal_admitted_guard_tests.command
      tree.daemon
      tree.root
      ~key:"554-tree-final-slot"
      (Change_budget (Goal.goal_id, budget))
    |> protocol_ok
    |> ignore;
    let root = state tree.root in
    assert (A.Goal_accounting.equal root_account (Goal.accounting root));
    let entered, enter = Eio.Promise.create () in
    let release, resume = Eio.Promise.create () in
    { tree
    ; root_account
    ; root_marks
    ; intermediate_own = S.own (Tree.snapshot root tree.intermediate.reference)
    ; intermediate_branch = branch root tree.intermediate
    ; sibling_branch = branch root tree.sibling
    ; sibling_snapshot = Tree.snapshot root tree.sibling.reference
    ; turn_ceiling
    ; attempt_ceiling
    ; providers_before = !(tree.counters.providers)
    ; entered
    ; enter
    ; release
    ; resume
    ; submitted = None
    }
  ;;

  let release t =
    if Option.is_none (Eio.Promise.peek t.release) then Eio.Promise.resolve t.resume ()
  ;;

  let availability current (dimension : A.Goal_admission_policy.dimension) ceiling =
    let account = Goal.accounting current in
    GD.available
      (Option.value_exn current.A.Session_state.goal_descendants)
      ~children:(Option.value_exn current.child_contributions)
      ~goal_id:Goal.goal_id
      ~local_count:
        (match dimension with
         | Host_turn -> A.Goal_accounting.admitted_turns account
         | Attempt -> A.Goal_accounting.admitted_attempts account)
      ~ceiling:(Some ceiling)
      ~dimension
    |> protocol_ok
  ;;

  let check_imports t receipt =
    let tree = t.tree in
    let root = state tree.root in
    let intermediate = state tree.intermediate.entry in
    let child = state tree.grandchild.entry in
    let root_branch = branch root tree.intermediate in
    assert (Option.is_none (GD.Branch.pending root_branch));
    [%test_eq: int64]
      Int64.(GD.Branch.confirmed_prefix t.intermediate_branch + 2L)
      (GD.Branch.confirmed_prefix root_branch);
    [%test_eq: int64]
      Int64.(GD.Branch.last_nonce t.intermediate_branch + 2L)
      (GD.Branch.last_nonce root_branch);
    assert (Included.equal receipt (included root_branch));
    assert (GD.Branch.equal t.sibling_branch (branch root tree.sibling));
    assert (
      Jsonaf.exactly_equal
        (S.to_json t.sibling_snapshot)
        (S.to_json (Tree.snapshot root tree.sibling.reference)));
    List.iter [ child; intermediate ] ~f:(fun current ->
      assert (
        List.equal
          Included.equal
          [ receipt ]
          (GD.local_included (Option.value_exn current.A.Session_state.goal_descendants))));
    let leaf = Tree.snapshot intermediate tree.grandchild.reference in
    let via_intermediate = Tree.snapshot root tree.intermediate.reference in
    assert_snapshot leaf tree.grandchild receipt |> ignore;
    assert_snapshot via_intermediate tree.intermediate receipt |> ignore;
    assert_marks (S.own leaf) ~turns:1L ~attempts:1L;
    assert_marks (S.Descendants.partition (S.descendants leaf)) ~turns:0L ~attempts:0L;
    assert (S.Partition.equal t.intermediate_own (S.own via_intermediate));
    assert_marks
      (S.Descendants.partition (S.descendants via_intermediate))
      ~turns:1L
      ~attempts:1L;
    let occurrence = Included.pending receipt |> Pending.occurrence in
    assert (
      Int64.(
        S.revision leaf
        >= Occurrence.Frame.candidate_session_revision (Occurrence.frame occurrence)));
    let aggregate = Tree.root_marks root in
    [%test_eq: int64] Int64.(t.root_marks.turn + 1L) aggregate.turn;
    [%test_eq: int64] Int64.(t.root_marks.attempt + 1L) aggregate.attempt;
    [%test_eq: int64] 0L aggregate.untracked_turns;
    [%test_eq: int64] 0L aggregate.untracked_attempts;
    assert (A.Goal_accounting.equal t.root_account (Goal.accounting root));
    let budget = G.budget (Goal.goal root) in
    assert (
      Option.equal Int64.equal (Some t.turn_ceiling) (G.Budget.admitted_turns budget));
    assert (
      Option.equal
        Int64.equal
        (Some t.attempt_ceiling)
        (G.Budget.admitted_attempts budget));
    List.iter
      [ A.Goal_admission_policy.Host_turn, t.turn_ceiling; Attempt, t.attempt_ceiling ]
      ~f:(fun (dimension, ceiling) ->
        assert (GD.Availability.equal Exhausted (availability root dimension ceiling));
        assert (
          GD.Availability.equal
            Available
            (availability root dimension (Int64.succ ceiling))))
  ;;

  let before_provider t inputs =
    assert (Option.is_none t.submitted);
    let input =
      `Array (List.map inputs ~f:Openai.Responses.Item.jsonaf_of_t) |> Jsonaf.to_string
    in
    assert (String.is_substring input ~substring:Tree.grandchild_message);
    let tree = t.tree in
    [%test_eq: int] (t.providers_before + 1) !(tree.counters.providers);
    let root = state tree.root in
    let child = state tree.grandchild.entry in
    Tree.assert_no_local_goal child;
    let receipt = included (branch root tree.intermediate) in
    let pending = Included.pending receipt in
    assert (O.Scope.equal tree.intermediate.scope (Pending.scope pending));
    assert (
      D.Reference.equal tree.intermediate.reference (Pending.branch_reference pending));
    let occurrence = Pending.occurrence pending in
    let frame = Occurrence.frame occurrence in
    assert (
      P.Id.Session.equal
        child.identity.session_id
        (P.Session_ref.session_id (Occurrence.Frame.session frame)));
    [%test_eq: int] child.identity.generation (Occurrence.Frame.generation frame);
    let ordinal, operation_id, invocation_id =
      match Occurrence.kind occurrence with
      | Attempt { ordinal; operation_id; invocation_id } ->
        ordinal, operation_id, invocation_id
      | Host_turn _ -> failwith "grandchild provider preceded actual Attempt confirmation"
    in
    let operation = Option.value_exn child.active_operation in
    assert (Option.equal P.Id.Operation.equal (Some operation.id) operation_id);
    assert (Option.is_none invocation_id);
    assert (
      Option.is_some
        (L.find_turn_handle
           child.inference_ledger
           ~operation_id:operation.id
           ~generation:operation.generation));
    let row = L.find child.inference_ledger ~ordinal |> Option.value_exn in
    let handle = L.Row.handle row in
    assert (P.Id.Session.equal child.identity.session_id (L.Handle.session_id handle));
    [%test_eq: int] child.identity.generation (L.Handle.generation handle);
    assert (Option.equal P.Id.Operation.equal operation_id (L.Handle.operation_id handle));
    assert (Option.is_none (L.Handle.invocation_id handle));
    (match Inference.Observation.Attempt_record.state (L.Row.record row) with
     | Running -> ()
     | Prepared | Terminal _ | Interrupted _ -> failwith "provider lacks real Running ACK");
    check_imports t receipt;
    t.submitted <- Some (receipt, handle);
    Eio.Promise.resolve t.enter ();
    Eio.Promise.await t.release
  ;;

  let after_terminal t =
    let receipt, handle = Option.value_exn t.submitted in
    let child = state t.tree.grandchild.entry in
    assert (Option.is_none child.active_operation);
    let row =
      L.find child.inference_ledger ~ordinal:(L.Handle.ordinal handle) |> Option.value_exn
    in
    assert (L.Handle.equal handle (L.Row.handle row));
    (match Inference.Observation.Attempt_record.state (L.Row.record row) with
     | Terminal terminal ->
       assert (
         Inference.Event.Terminal.equal_outcome
           (Inference.Event.Terminal.outcome terminal)
           Completed)
     | Prepared | Running | Interrupted _ ->
       failwith "grandchild actual Attempt did not complete");
    let actual = A.Child_accounting.snapshot child |> protocol_ok |> Option.value_exn in
    assert_marks (S.own actual) ~turns:1L ~attempts:1L;
    check_imports t receipt;
    [%test_eq: int] (t.providers_before + 1) !(t.tree.counters.providers)
  ;;
end

let%expect_test
    "real grandchild imports through intermediate while a sibling competes for the last \
     root slot"
  =
  let root_barrier = Root_barrier.create () in
  let writer_barrier = Direct.Writer_barrier.create () in
  let probe = ref None in
  Tree.with_tree
    ~wrap_env:(fun env ->
      Root_barrier.wrap root_barrier (Direct.Writer_barrier.wrap writer_barrier env))
    ~before_provider:(fun tree inputs ->
      let actual = Option.value_exn !probe in
      assert (phys_equal tree actual.Probe.tree);
      Probe.before_provider actual inputs)
    (fun tree ->
       let within phase f = Tree.within tree.env phase f in
       Tree.with_writer tree.grandchild.entry (fun grandchild_writer ->
         Tree.with_writer tree.sibling.entry (fun sibling_writer ->
           let grandchild_message =
             Tree.message tree.grandchild.entry Tree.grandchild_message
           in
           let sibling_message =
             Tree.message tree.sibling.entry "554 denied sibling must not execute"
           in
           let current_probe = Probe.create tree in
           probe := Some current_probe;
           let grandchild_before = state tree.grandchild.entry in
           let sibling_before = state tree.sibling.entry in
           let root_before = state tree.root in
           let child_session =
             P.Session_ref.create
               ~server_id:(Agent_store.Session_store.server_id (Daemon.store tree.daemon))
               ~session_id:grandchild_before.identity.session_id
           in
           root_barrier.target
           <- Some
                ( root_before.identity.session_id
                , tree.intermediate.scope
                , Root_barrier.Kind.Reserve child_session );
           writer_barrier.child <- Some grandchild_before.identity.session_id;
           let pending =
             Eio.Fiber.fork_promise ~sw:tree.sw (fun () ->
               Actor.submit_message_authenticated
                 tree.grandchild.entry.actor
                 ~actor:original
                 ~attachment_id:grandchild_writer.id
                 grandchild_message)
           in
           protect_cleanup
             ~finally:(fun () ->
               Root_barrier.release root_barrier;
               Direct.Writer_barrier.release writer_barrier;
               Probe.release current_probe;
               within "join actual grandchild submit" (fun () ->
                 Eio.Promise.await_exn pending)
               |> ignore)
             (fun () ->
                within "actual root reserves grandchild occurrence" (fun () ->
                  Eio.Promise.await root_barrier.entered);
                (* Root persistence is suspended. Only the free grandchild Actor is read. *)
                Existing.assert_same grandchild_before tree.grandchild.entry;
                [%test_eq: int] current_probe.providers_before !(tree.counters.providers);
                let occurrence = Option.value_exn root_barrier.occurrence in
                let operation_id =
                  match Occurrence.kind occurrence with
                  | Host_turn { operation_id } -> operation_id
                  | Attempt _ -> failwith "root barrier did not capture the real Turn"
                in
                Root_barrier.release root_barrier;
                within "grandchild canonical writer entered before ACK" (fun () ->
                  Eio.Promise.await writer_barrier.reached);
                assert (
                  Option.equal
                    P.Id.Operation.equal
                    (Some operation_id)
                    writer_barrier.operation);
                (* Grandchild persistence is suspended; root and the sibling are free.
                 RI's pending occurrence occupies the one ROOT-wide slot, while RS
                 has no pending occurrence. This cannot be same-branch Busy. *)
                let reserved = state tree.root in
                let intermediate_branch = branch reserved tree.intermediate in
                let pending_receipt =
                  GD.Branch.pending intermediate_branch |> Option.value_exn
                in
                assert (Occurrence.equal occurrence (Pending.occurrence pending_receipt));
                assert (
                  D.Reference.equal
                    tree.intermediate.reference
                    (Pending.branch_reference pending_receipt));
                assert (
                  O.Scope.equal tree.intermediate.scope (Pending.scope pending_receipt));
                [%test_eq: int64]
                  Int64.(GD.Branch.last_nonce current_probe.intermediate_branch + 1L)
                  (GD.Branch.last_nonce intermediate_branch);
                [%test_eq: int64]
                  (GD.Branch.last_nonce intermediate_branch)
                  (Pending.nonce pending_receipt);
                [%test_eq: int64]
                  Int64.(
                    GD.Branch.confirmed_prefix current_probe.intermediate_branch + 1L)
                  (Pending.next_prefix pending_receipt);
                assert (
                  GD.Branch.equal
                    current_probe.sibling_branch
                    (branch reserved tree.sibling));
                [%test_eq: int64]
                  (GD.Branch.confirmed_prefix current_probe.intermediate_branch)
                  (GD.Branch.confirmed_prefix intermediate_branch);
                assert (
                  GD.Availability.equal
                    Reserved
                    (Probe.availability reserved Host_turn current_probe.turn_ceiling));
                [%test_eq: int64]
                  current_probe.root_marks.turn
                  (Tree.root_marks reserved).turn;
                [%test_eq: int64]
                  current_probe.root_marks.attempt
                  (Tree.root_marks reserved).attempt;
                (match
                   Actor.submit_message_authenticated
                     tree.sibling.entry.actor
                     ~actor:original
                     ~attachment_id:sibling_writer.id
                     sibling_message
                 with
                 | Error error ->
                   [%test_eq: P.Error.code] Conflict error.code;
                   [%test_eq: string]
                     "descendant root capacity is reserved by an unconfirmed occurrence"
                     error.message
                 | Ok _ ->
                   failwith "different root branch consumed the occupied final slot");
                Existing.assert_same sibling_before tree.sibling.entry;
                Existing.assert_same reserved tree.root;
                [%test_eq: int] current_probe.providers_before !(tree.counters.providers);
                Direct.Writer_barrier.release writer_barrier;
                within "provider follows both real multilevel confirmations" (fun () ->
                  let rec await_provider () =
                    Tree.raise_failure tree;
                    match Eio.Promise.peek current_probe.entered with
                    | Some () -> ()
                    | None ->
                      Eio.Fiber.yield ();
                      await_provider ()
                  in
                  await_provider ());
                Tree.raise_failure tree;
                let submitted =
                  within "actual grandchild submit ACK" (fun () ->
                    Eio.Promise.await_exn pending)
                  |> protocol_ok
                in
                assert (
                  Option.equal
                    P.Id.Operation.equal
                    (Some operation_id)
                    submitted.operation_id);
                Probe.release current_probe;
                within "grandchild terminal operation" (fun () ->
                  Goal_native_test_support.await_quiet tree.grandchild.entry)
                |> ignore;
                Tree.raise_failure tree;
                Probe.after_terminal current_probe;
                Existing.assert_same sibling_before tree.sibling.entry))));
  print_endline
    "R→I→G confirms Turn and Attempt once; R→S cannot consume the same root slot";
  [%expect
    {| R→I→G confirms Turn and Attempt once; R→S cannot consume the same root slot |}]
;;
