open! Core
open Fixture
module A = Agent_session
module GD = A.Goal_descendants
module C = A.Child_contributions
module O = Agent_store.Delegation_goal_origin
module DS = Agent_store.Delegation_store
module Record = Agent_store.Delegation_record
module Route = Descendant_root_route_test_support.Goal_descendant_root_route
module Component = Descendant_component_tests
module Existing = Descendant_contribution_tests

(* Pure association DATA: public domain record construction is not a private
   Store observation, checked ancestry, runtime route or permission grant. *)
let target =
  Inference.Request.Target.create
    ~adapter:"fixture.responses"
    ~profile:"selected"
    ~profile_revision:None
    ~account:None
    ~endpoint:"fixture://responses"
    ~model:"fixture-model"
    ~settings:[]
    ~limits:Agent_store.Delegation_document.limits
  |> Result.map_error ~f:(fun error ->
    Sexp.to_string_hum (Inference.Request.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let validated raw =
  Agent_store.Delegation_document.of_record raw
  |> Component.domain
  |> Agent_store.Delegation_document.to_record
;;

let record reference origin =
  let admission : Record.Admission.t =
    { child_session_id = reference.DS.Reference.child_session_id
    ; revision_id = reference.revision_id
    ; transaction_id = P.Id.Transaction.of_string "txn_scoped" |> protocol
    ; manifest_sha256 = String.make 64 'c'
    ; parent_revision_id = P.Id.Prompt_revision.of_string "prv_route_parent" |> protocol
    ; parent_stop_epoch = Some 0L
    ; authority_sha256 = String.make 64 'd'
    ; authored_tool = None
    ; goal_origin = origin
    ; captured_recurrence = None
    ; capability_pins = []
    ; lifetime = Owned
    ; created_at = timestamp
    ; inference_target = Some target
    }
  in
  let raw : Record.t =
    { key = reference.key
    ; request_sha256 = reference.request_sha256
    ; admission
    ; stage = Linked
    ; revocation = None
    ; artifact_collection = None
    ; preservation = None
    }
  in
  validated raw
;;

let%expect_test
    "all local roots require exact frozen first-child association without creator \
     lifetime"
  =
  let ledger = make_ledger () in
  let first, children, _, _, _ =
    Component.initialized ledger (Component.make_children ())
  in
  let second, second_episode = goal ~suffix:"two" () in
  let second_held =
    G.create
      ~id:(G.id second)
      ~session
      ~creator:principal
      ~source:(source ())
      ~objective:(G.objective second)
      ~budget:G.Budget.unlimited
    |> protocol
  in
  let account = A.Goal_accounting_document.create second_held ~ledger |> protocol in
  let accounting =
    GD.register_new_root
      (Some first)
      ~host_profile:(Component.profile ())
      ~session
      ~children:(Some children)
      ~goal:second_held
      ~account
      ~before_ledger:ledger
    |> protocol
    |> GD.Update.accounting
  in
  let locator = Existing.reference "route_child" in
  let one = Existing.scope locator in
  let two =
    O.Scope.create
      ~owner:session
      ~goal_id:(G.id second)
      ~source:(source ())
      ~creator_episode:second_episode
      ~branch:(O.Scope.branch one)
    |> protocol
  in
  let scopes = [ one; two ] in
  let origin = O.associated ~max_scopes:2 scopes |> protocol in
  let private_record = record locator (Some origin) in
  let reference = DS.reference private_record in
  let update =
    GD.register_branch accounting ~children ~scope:one ~reference ~origin |> protocol
  in
  let accounting = GD.Update.accounting update in
  let children = Component.updated_children children update in
  let select ?(scopes = scopes) ?(records = [ private_record ]) ?(children = children) () =
    Route.select ~session ~scopes ~records ~accounting ~children
  in
  let routes = select () |> protocol in
  assert (List.equal O.Scope.equal (List.map routes ~f:Route.scope) scopes);
  assert (
    List.for_all routes ~f:(fun route ->
      DS.Reference.equal (Route.reference route) reference));
  List.iter routes ~f:(fun route ->
    Route.validate route ~accounting ~children |> protocol);
  (* Completion/run liveness/current Goal episode is intentionally absent from
     this immutable association relation; current policy belongs to real owner. *)
  let rejects result = assert (Result.is_error result) in
  rejects (select ~scopes:[ two; one ] ());
  rejects (select ~scopes:[ one; one ] ());
  rejects (select ~records:[] ());
  rejects (select ~records:[ private_record; private_record ] ());
  List.iter
    [ record locator None
    ; record locator (Some O.unrelated)
    ; { private_record with
        admission =
          { private_record.admission with
            transaction_id = P.Id.Transaction.of_string "txn_other_route" |> protocol
          }
      }
    ; { private_record with
        admission =
          { private_record.admission with
            revision_id = P.Id.Prompt_revision.of_string "prv_other_route" |> protocol
          }
      }
    ]
    ~f:(fun changed -> rejects (select ~records:[ validated changed ] ()));
  rejects (select ~children:(Component.make_children ()) ());
  let third, episode = goal ~suffix:"three" () in
  let absent =
    O.Scope.create
      ~owner:session
      ~goal_id:(G.id third)
      ~source:(source ())
      ~creator_episode:episode
      ~branch:(O.Scope.branch one)
    |> protocol
  in
  (* A matching first root must not silently hide an absent second root. *)
  rejects (select ~scopes:[ one; absent ] ());
  let foreign =
    P.Session_ref.create
      ~server_id:(P.Id.Server.of_string "srv_elsewhere" |> protocol)
      ~session_id:sid
  in
  rejects
    (Route.select
       ~session:foreign
       ~scopes
       ~records:[ private_record ]
       ~accounting
       ~children);
  print_endline
    "complete local vector; exact anchor/transaction/reference/origin/frontier; no \
     runtime grant";
  [%expect
    {| complete local vector; exact anchor/transaction/reference/origin/frontier; no runtime grant |}]
;;
