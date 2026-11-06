open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module G = P.Goal
module Policy = Goal_root_policy_test_support.Goal_root_policy
module GD = A.Goal_descendants
module Catalog = A.Goal_catalog

(* Exact copied private helper, actual validated State projections only. These
   cells are not authenticated routes, reservations, ACKs or execution grants. *)
let with_root f =
  Goal_run_tests.with_model (fun ~basis ~held ~admission:_ ~intent:_ ~delta _ ->
    let goal = Catalog.find held Goal_run_tests.goal_id |> Option.value_exn in
    let accounts =
      A.Goal_accounts.bootstrap A.Goal_accounts.empty ~goal ~ledger:basis.inference_ledger
      |> protocol_ok
    in
    let children =
      A.Child_contributions.create
        ~inventory:Complete_inventory
        ~max_children:(GD.Profile.max_branches goal_descendant_profile)
        ~max_encoded_bytes:(GD.Profile.child_max_encoded_bytes goal_descendant_profile)
      |> protocol_ok
    in
    let root =
      GD.register_new_root
        None
        ~host_profile:goal_descendant_profile
        ~session:(G.session goal)
        ~children:(Some children)
        ~goal
        ~account:(A.Goal_accounts.find accounts (G.id goal) |> Option.value_exn)
        ~before_ledger:basis.inference_ledger
      |> protocol_ok
      |> GD.Update.accounting
    in
    let pre_episode =
      { basis with
        goal_accounts = Some accounts
      ; goal_descendants = Some root
      ; child_contributions = Some children
      }
    in
    A.Session_state.validate pre_episode |> protocol_ok;
    let bound =
      A.Session_transition.apply ~now:timestamp pre_episode ~delta ~payloads:[]
      |> protocol_ok
      |> fun transition -> transition.A.Session_transition.state
    in
    f ~pre_episode ~bound)
;;

let update state f =
  let catalog = Option.value_exn state.A.Session_state.goal_catalog in
  let goal = Goal_run_tests.current_goal state |> f |> protocol_ok in
  let document =
    Catalog.document catalog (G.id goal)
    |> Option.value_exn
    |> fun previous -> A.Goal_document.update previous goal |> document_ok
  in
  let catalog = Catalog.advance_existing catalog ~goal:document |> protocol_ok in
  let state = { state with goal_catalog = Some catalog } in
  A.Session_state.validate state |> protocol_ok;
  state
;;

let cell state = Policy.create state ~goal_id:Goal_run_tests.goal_id |> protocol_ok
let fresh policy state = Policy.fresh policy ~state ~now:timestamp
let admitted policy state = Policy.admitted policy ~state ~now:timestamp

let assert_live policy state =
  assert (A.Goal_admission_policy.equal_decision (fresh policy state |> protocol_ok) Allow);
  admitted policy state |> protocol_ok
;;

let assert_retired policy state =
  assert (Result.is_error (fresh policy state));
  assert (Result.is_error (admitted policy state))
;;

let%expect_test
    "stable root cell survives episode binding but never revives after explicit \
     retirement"
  =
  with_root (fun ~pre_episode ~bound ->
    let policy = cell pre_episode in
    assert (Option.is_none (G.episode (Goal_run_tests.current_goal pre_episode)));
    assert (Option.is_some (G.episode (Goal_run_tests.current_goal bound)));
    Policy.retain_candidate
      policy
      ~previous:pre_episode
      ~candidate:bound
      ~admission_hold:None;
    assert_live policy bound;
    let progressed =
      update bound (fun goal ->
        G.record_progress goal (Goal_catalog_tests.progress "still this root"))
    in
    Policy.retain_candidate
      policy
      ~previous:bound
      ~candidate:progressed
      ~admission_hold:None;
    assert_live policy progressed;
    let hold = G.Hold.Budget_gate Admitted_attempts in
    let held = update progressed (fun goal -> G.hold goal hold) in
    Policy.retain_candidate
      policy
      ~previous:progressed
      ~candidate:held
      ~admission_hold:(Some Goal_run_tests.goal_id);
    assert (
      A.Goal_admission_policy.equal_decision
        (fresh policy held |> protocol_ok)
        (Hold hold));
    admitted policy held |> protocol_ok;
    (* Factual equal-Hold writes preserve the grandfathered admitted cell. *)
    Policy.retain_candidate policy ~previous:held ~candidate:held ~admission_hold:None;
    admitted policy held |> protocol_ok;
    (* The explicit control seam retires BEFORE persistence. The simulated
       failed write keeps exactly [held], so a state-only recheck would revive. *)
    Policy.retire policy;
    assert_retired policy held;
    let enabled = update held G.resume in
    Policy.retain_candidate policy ~previous:held ~candidate:enabled ~admission_hold:None;
    assert_retired policy enabled;
    let later = cell enabled in
    assert_live later enabled;
    assert_retired policy enabled);
  print_endline
    "episode/progress retained; automatic Hold admitted; explicit failed same-Hold stays \
     retired";
  [%expect
    {| episode/progress retained; automatic Hold admitted; explicit failed same-Hold stays retired |}]
;;

let%expect_test
    "policy changes retire before failed writes and final admitted check samples current \
     deadline"
  =
  with_root (fun ~pre_episode:_ ~bound ->
    let deadline = P.Timestamp.add_ms timestamp 1 |> protocol_ok in
    let budget =
      G.Budget.create
        ~admitted_turns:None
        ~admitted_attempts:None
        ~observed_tokens:None
        ~deadline:(Some deadline)
      |> protocol_ok
    in
    let changed = update bound (fun goal -> G.change_budget goal budget) in
    let old = cell bound in
    Policy.retain_candidate old ~previous:bound ~candidate:changed ~admission_hold:None;
    (* Actual durable State still [bound] after a failed writer. *)
    assert_retired old bound;
    let current = cell changed in
    assert_live current changed;
    assert (Result.is_error (Policy.admitted current ~state:changed ~now:deadline));
    assert (
      A.Goal_admission_policy.equal_decision
        (Policy.fresh current ~state:changed ~now:deadline |> protocol_ok)
        (Hold Deadline_reached));
    List.iter
      [ { changed with goal_accounts = None }; { changed with goal_descendants = None } ]
      ~f:(fun absent ->
        let policy = cell changed in
        Policy.retain_candidate
          policy
          ~previous:changed
          ~candidate:absent
          ~admission_hold:None;
        assert_retired policy changed));
  print_endline
    "failed policy change cannot revive; current deadline and absent accounting/root deny";
  [%expect
    {| failed policy change cannot revive; current deadline and absent accounting/root deny |}]
;;
