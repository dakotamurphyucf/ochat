open! Core
open Fixtures
module F = Creator_custody_support
module S = Agent_session
module A = S.Session_actor
module P = Agent_protocol
module M = Chat_response.Moderator_manager
module Q = S.Moderator_queue

let%expect_test "compiled owned timers retain B through same and later callback links" =
  F.with_fixture (fun _env t ->
    F.invoke t "same";
    F.invoke t "later";
    F.invoke t "spare";
    let first = F.schedule t "same" in
    let later = F.schedule t "later" in
    let same_creator = F.assert_creator t first in
    let later_creator = F.assert_creator t later in
    assert (not (P.Id.Invocation.equal same_creator later_creator));
    assert (Option.is_some (Option.value_exn first.ownership).subscription);
    assert (Option.is_none (Option.value_exn later.ownership).subscription);
    F.deliver_timer t "same";
    F.successful_drain t;
    F.assert_native_b t 1;
    let failed = F.link_later t ~reject:true in
    [%test_eq: int] 1 !(t.rejected_links);
    assert (
      Option.is_none (Option.value_exn (F.schedule t "later").ownership).subscription);
    let accepted = F.link_later t ~reject:false in
    assert (not (P.Id.Moderator_execution.equal failed accepted));
    let linked = F.schedule t "later" in
    let ownership = Option.value_exn linked.ownership in
    (match ownership.creator with
     | Invocation id -> assert (P.Id.Invocation.equal id later_creator)
     | Moderator_event _ -> failwith "link callback replaced actual creator");
    assert (Option.is_some ownership.subscription);
    assert (
      P.Schedule_provenance.equal
        (Option.value_exn later.inference_provenance)
        (Option.value_exn linked.inference_provenance));
    F.deliver_timer t "later";
    F.successful_drain t;
    F.assert_native_b t 2;
    F.assert_backend t;
    Atomic.set t.b_live false;
    assert (not (Operator_authorization.is_current t.b));
    assert (Operator_authorization.is_current t.a);
    F.deliver_timer t "spare";
    F.denied_drain t;
    [%test_eq: int] 2 !(t.effects);
    F.assert_backend t);
  print_endline
    "compiled creators outlive callback; same/later link keeps B; rejected ACK \
     conserves; A cannot renew expired B";
  [%expect
    {| compiled creators outlive callback; same/later link keeps B; rejected ACK conserves; A cannot renew expired B |}]
;;

let%expect_test
    "compiled ingress original repeats without renewal and restored DATA stays \
     unavailable"
  =
  with_actor_workspace (fun env workspace_instance ->
    let saved =
      F.with_fixture ~environment:(env, workspace_instance) (fun _env t ->
        F.invoke t "ingress";
        let original = F.registration t in
        let provenance = Option.value_exn original.inference_provenance in
        assert (
          Option.equal
            P.Inference_origin.equal
            (Some Unbound)
            (S.External_ingress.Provenance.origin provenance));
        assert (
          List.is_empty
            (S.Run_state.runs
               (Option.value (F.state t).run_state ~default:S.Run_state.empty)));
        let parent =
          List.find_exn (F.state t).invocations ~f:(fun invocation ->
            P.Id.Invocation.equal
              invocation.context.id
              (F.subscription t "ingress").context.invocation_id)
        in
        (match parent.status with
         | Published (Pending (Subscription id, _)) ->
           assert (P.Id.Subscription.equal id original.context.subscription_id)
         | Admitted | Dispatching | Resolved _
         | Published (Complete _ | Fail _ | Cancelled _ | Pending (Job _, _)) ->
           failwith "compiled ingress lost actual Pending creator");
        let first = F.accept_ingress t "creator-one" in
        F.successful_drain t;
        F.assert_native_b t 1;
        let before_duplicate = F.state t in
        let manager_before = F.snapshot t in
        (match F.prepare_ingress t "creator-one" |> protocol_ok with
         | Duplicate receipt -> assert (S.External_ingress.equal_receipt first receipt)
         | Enqueue _ -> failwith "duplicate ingress created a fresh enqueue");
        assert_same_session_snapshot before_duplicate (F.state t);
        assert_same_session_snapshot before_duplicate (S.Memory_backend.state t.backend);
        [%test_eq: string]
          (S.Moderator_checkpoint.digest manager_before)
          (S.Moderator_checkpoint.digest (F.snapshot t));
        F.assert_native_b t 1;
        let second = F.accept_ingress t "creator-two" in
        assert (not (P.Id.Ingress_event.equal first.id second.id));
        F.successful_drain t;
        F.assert_native_b t 2;
        let third = F.accept_ingress t "creator-missing" in
        assert (not (P.Id.Ingress_event.equal second.id third.id));
        (* Save while registration/subscription and original B are still current.
         Later revocation DATA cannot mask missing runtime custody on restoration. *)
        let state = F.state t in
        assert (Option.is_none (F.registration t).revoked);
        assert (Option.is_none (F.subscription t "ingress").result);
        let document =
          S.Session_state_document.authored state
          |> S.Session_state_document.encode ~limits:document_limits
          |> document_ok
        in
        let restored =
          S.Session_state_document.decode ~limits:document_limits document
          |> document_ok
          |> S.Session_state_document.value
        in
        assert_same_session_snapshot state restored;
        Atomic.set t.b_live false;
        assert (not (Operator_authorization.is_current t.b));
        F.denied_drain t;
        [%test_eq: int] 2 !(t.effects);
        F.revoke_ingress t;
        assert (Option.is_some (F.registration t).revoked);
        let revoked = F.state t in
        (match F.prepare_ingress t "after-revoke" with
         | Error failure -> [%test_eq: P.Error.code] Permission_denied failure.code
         | Ok _ -> failwith "revoked registration admitted another frame");
        assert_same_session_snapshot revoked (F.state t);
        F.assert_backend t;
        [%test_eq: int] 2 !(t.effects);
        restored)
    in
    F.with_fixture ~environment:(env, workspace_instance) ~restored:saved (fun _env t ->
      (* This is decoded DATA under a new physical Actor/Manager, not a transfer
         of original loans or a claim that a queue ordinal survived live rewrite. *)
      assert (Option.is_none (F.registration t).revoked);
      assert (Option.is_none (F.subscription t "ingress").result);
      [%test_eq: int] 3 (List.length (F.registration t).receipts);
      [%test_eq: int] 1 (List.length (Q.entries (F.queue t)));
      assert (Operator_authorization.is_current t.b);
      assert (Operator_authorization.is_current t.a);
      F.denied_drain t;
      [%test_eq: int] 0 !(t.effects);
      assert (!(t.missing) > 0);
      F.assert_backend t));
  print_endline
    "actual ingress receipt repeats B; duplicate writes/effects nothing; expiry/revoke \
     and restored absence never renew";
  [%expect
    {| actual ingress receipt repeats B; duplicate writes/effects nothing; expiry/revoke and restored absence never renew |}]
;;

let%expect_test
    "fresh admitted None survives creator and delivery ACK cuts, unlike restored absence"
  =
  with_actor_workspace (fun env workspace_instance ->
    let saved =
      F.with_fixture
        ~environment:(env, workspace_instance)
        ~native_original:No_principal
        (fun _env t ->
           let rejected =
             F.invoke_with_original ~reject_creation:true t ~original:No_principal "same"
           in
           assert (List.is_empty (F.state t).schedules);
           assert (List.is_empty (F.state t).subscriptions);
           let accepted = F.invoke_with_original t ~original:No_principal "same" in
           assert (not (P.Id.Invocation.equal rejected.context.id accepted.context.id));
           let timer = F.schedule t "same" in
           let creator = F.assert_creator t timer in
           assert (P.Id.Invocation.equal creator accepted.context.id);
           (match accepted.status with
            | Published (Pending (Subscription id, _)) ->
              assert (P.Id.Subscription.equal id (F.subscription t "same").context.id)
            | Admitted | Dispatching | Resolved _
            | Published (Complete _ | Fail _ | Cancelled _ | Pending (Job _, _)) ->
              failwith "successful Fresh None creator lost actual Pending subscription");
           let before_delivery = F.state t in
           let document =
             S.Session_state_document.authored before_delivery
             |> S.Session_state_document.encode ~limits:document_limits
             |> document_ok
           in
           let restored =
             S.Session_state_document.decode ~limits:document_limits document
             |> document_ok
             |> S.Session_state_document.value
           in
           assert_same_session_snapshot before_delivery restored;
           F.deliver_timer ~reject_ack:true t "same";
           F.successful_drain t;
           F.assert_native_none t 1;
           F.observe_original t Admitted_none;
           restored)
    in
    F.with_fixture
      ~environment:(env, workspace_instance)
      ~restored:saved
      ~native_original:No_principal
      (fun _env t ->
         (* Same native host policy permits ordinary legacy None, but cannot
           reconstruct the original admission needed by observation continuation. *)
         F.deliver_timer t "same";
         F.successful_drain t;
         F.assert_native_none t 1;
         F.observe_original t Unavailable));
  print_endline
    "rejected Fresh/delivery ACKs conserve; distinct real None creator retains \
     observation admission; same-policy restored DATA does not";
  [%expect
    {| rejected Fresh/delivery ACKs conserve; distinct real None creator retains observation admission; same-policy restored DATA does not |}]
;;
