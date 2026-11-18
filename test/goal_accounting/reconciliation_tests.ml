open! Core
open Fixture
module Catalog = Agent_session.Goal_catalog
module Accounts = Agent_session.Goal_accounts
module Child = Agent_session.Goal_accounting_document
module Reconcile = Agent_session.Goal_ledger_reconciliation
module GD = Agent_session.Goal_document

let document result =
  Result.map_error result ~f:(fun e -> Sexp.to_string_hum (D.Error.sexp_of_t e))
  |> Result.ok_or_failwith
;;

let catalog held =
  let receipt =
    Agent_session.Goal_command_receipt.create
      ~principal_id:principal
      ~key:(P.Idempotency_key.of_string "model-catalog" |> protocol)
      ~request_sha256:(String.make 64 'a')
      ~goal_id:(G.id held)
      ~goal_revision:0L
      ~catalog_revision:1L
      ~source:(Some (G.source held))
      ~accepted_at:timestamp
    |> protocol
  in
  Catalog.commit Catalog.empty ~goal:(GD.create held |> document) ~receipt |> protocol
;;

let replace catalog goal =
  let child = Catalog.document catalog (G.id goal) |> Option.value_exn in
  Catalog.advance_existing catalog ~goal:(GD.update child goal |> document) |> protocol
;;

let%expect_test
    "all ledger publishers reconcile closed partitions; protected rows skip retirement"
  =
  (* Explicit pure history model, with no execution permit or runtime claim. *)
  let before = make_ledger ~attempts:1 () in
  let goal, episode = goal () in
  let held = Storage_tests.unbound goal in
  let catalog = catalog held |> fun catalog -> replace catalog goal in
  let accounts =
    Accounts.bootstrap Accounts.empty ~goal:held ~ledger:before |> protocol
  in
  let after, handle, tracking = admit before in
  let admitted =
    Reconcile.prepare
      ~accounts:(Some accounts)
      ~catalog:(Some catalog)
      ~before
      ~after
      ~admission:(Some (G.id goal, A.Admission.Attempt { episode; handle; tracking }))
    |> protocol
  in
  assert (Reconcile.changed admitted);
  let accounts = Reconcile.accounts admitted |> Option.value_exn in
  let observed = put after handle 0L (actual_views 7L) in
  let observation =
    Reconcile.prepare
      ~accounts:(Some accounts)
      ~catalog:(Some catalog)
      ~before:after
      ~after:observed
      ~admission:None
    |> protocol
  in
  assert (not (Reconcile.changed observation));
  let closed =
    G.abandon goal (G.Reason.of_string "Model stopped" |> protocol) |> protocol
  in
  let catalog = replace catalog closed in
  let ended = L.set_state observed handle interrupted |> ledger in
  let raw = Accounts.to_jsonaf accounts |> protocol in
  let open Storage_tests in
  let payload = member raw "payload" in
  let raw =
    set
      raw
      "payload"
      (set
         payload
         "accounts"
         (first (member payload "accounts") ~f:(fun wrapper ->
            let child = member wrapper "document" in
            let contents = member child "payload" in
            set
              wrapper
              "document"
              (set
                 child
                 "payload"
                 (set
                    contents
                    "bindings"
                    (first (member contents "bindings") ~f:(fun row ->
                       set row "future_binding" `True)))))))
  in
  let protected = Accounts.of_jsonaf raw |> protocol in
  let denied = Reconcile.protected_ordinals (Some protected) ~ledger:ended |> protocol in
  assert (Set.mem denied (L.Handle.ordinal handle));
  let protected_after, _, tracking =
    L.admit
      ~protected_ordinals:denied
      ended
      ~source:(Transcript.Source_id.of_string "dispatch" |> Result.ok_or_failwith)
      ~relation:Root
      ~operation_id:None
      ~invocation_id:None
      ~configuration
    |> ledger
  in
  assert (Option.is_some (L.find protected_after ~ordinal:(L.Handle.ordinal handle)));
  (match tracking with
   | L.Untracked Protected_future_data -> ()
   | _ -> assert false);
  let next, _, _ = admit ended in
  assert (Option.is_none (L.find next ~ordinal:(L.Handle.ordinal handle)));
  let folded =
    Reconcile.prepare
      ~accounts:(Some accounts)
      ~catalog:(Some catalog)
      ~before:ended
      ~after:next
      ~admission:None
    |> protocol
  in
  assert (Reconcile.changed folded);
  let child =
    Reconcile.accounts folded
    |> Option.value_exn
    |> fun accounts -> Accounts.find accounts (G.id goal) |> Option.value_exn
  in
  let settled = Child.resolve child ~goal:closed ~ledger:next |> protocol in
  [%test_eq: int64] (A.settled_tracked_attempts settled) 1L;
  (match
     U.Metric.actual (U.component (A.usage settled ~ledger:next |> account) Input)
   with
   | Tokens tokens -> [%test_eq: int64] tokens 7L
   | Overflow -> assert false);
  print_endline
    "observations stay canonical; closed totals fold once; protected data stays retained";
  [%expect
    {| observations stay canonical; closed totals fold once; protected data stays retained |}]
;;
