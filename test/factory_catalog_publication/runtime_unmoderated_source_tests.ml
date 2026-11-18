open! Core
open Agent_server_test_support
open Retry_support
module F = Runtime_unmoderated_source_support
module B = A.Runtime_builder
module N = A.Native_tool_invocation
module Resource_loan = Ordinary_warm_resource_tests.Resource_loan
module Warm = Delegated_warm_resource_tests
module Store = Agent_store

let stale t request =
  match Actor.check_delegated_policy_caller t.F.entry.actor request with
  | Error { code = Conflict; message; retryable = false; _ }
    when String.equal message "delegated policy Caller lost its exact source binding" ->
    ()
  | Error failure ->
    raise_s [%sexp "unexpected stale unmoderated Caller", (failure : P.Error.t)]
  | Ok () -> failwith "old Caller renewed across an acknowledged source replacement"
;;

let config_request t ~key =
  let current = F.state t in
  let patch =
    P.Session_configuration.Patch.create ~model:"597-next-model" ~settings:[] ()
    |> protocol_ok
  in
  ({ session_id = current.identity.session_id
   ; attachment_id = t.F.writer
   ; expected_generation = current.identity.generation
   ; expected_revision = current.spec.configuration_revision
   ; patch
   ; idempotency_key = P.Idempotency_key.of_string key |> protocol_ok
   }
   : P.Session_configuration.Update_request.t)
;;

let%expect_test
    "actual unmoderated root retains warm facts and rotates Caller only at configuration \
     ACK"
  =
  F.with_fixture (fun t ->
    Eio.Switch.run (fun sw ->
      let initial = F.state t in
      let escaped = ref None in
      let first =
        F.run t ~id:"597-unmoderated-first" (fun borrowed ->
          let caller = F.caller t borrowed in
          let request =
            F.request
              t
              borrowed
              ~candidate:
                (P.Id.Invocation.of_string "inv_597_unmoderated_first" |> protocol_ok)
              caller
          in
          F.registered t request;
          escaped := Some request)
      in
      [%test_eq: int] 1 t.host.reads;
      assert (List.is_empty first.jobs);
      assert (List.is_empty first.moderator_executions);
      assert (
        P.Id.Prompt_revision.equal
          initial.spec.prompt_revision_id
          first.spec.prompt_revision_id);
      let previous = Warm.loaded t.entry in
      assert (phys_equal previous.worker t.first.worker);
      let loan = Resource_loan.create ~sw t.entry in
      protect_cleanup
        ~finally:(fun () ->
          Resource_loan.release loan;
          F.within t "join actual unmoderated Product loan" (fun () ->
            Resource_loan.join loan))
        (fun () ->
           let retained =
             F.within t "retain initial unmoderated physical Product" (fun () ->
               Resource_loan.await loan)
           in
           assert (phys_equal retained.worker previous.worker);
           let ports = !(t.counters.inference_ports) in
           let providers = !(t.counters.providers) in
           let before = F.state t in
           F.within t "unload idle initial unmoderated binding" (fun () ->
             Owner.unload_and_wait t.entry.runtime |> protocol_ok);
           assert (not (Owner.is_loaded t.entry.runtime));
           [%test_eq: int] 0 t.cache.cache_renames;
           F.within t "ordinary warm install from same unmoderated Product" (fun () ->
             Owner.ensure_loaded_authenticated t.entry.runtime ~actor:original
             |> protocol_ok);
           let current = Warm.loaded t.entry in
           let after = F.state t in
           F.assert_unmoderated current after;
           assert (not (phys_equal previous.worker current.worker));
           assert (
             not
               (phys_equal
                  (Option.value_exn previous.unmoderated_source)
                  (Option.value_exn current.unmoderated_source)));
           assert (
             phys_equal
               (Option.value_exn previous.native_runtime)
               (Option.value_exn current.native_runtime));
           assert (phys_equal previous.parse_user_content current.parse_user_content);
           assert (
             Option.equal
               Store.Prompt_source_receipt.equal
               (A.Prompt_source_lease.receipt previous.source_lease)
               (A.Prompt_source_lease.receipt current.source_lease));
           (match A.Prompt_source_lease.availability current.source_lease with
            | Live -> ()
            | Missing_receipt | Unavailable _ | Revoked | Closed ->
              failwith "retained unmoderated source lost its actual live owner");
           assert (
             A.Session_state.Spec.Resource_identity.equal
               (A.Session_state.Spec.Resource_identity.capture before.spec)
               (A.Session_state.Spec.Resource_identity.capture after.spec));
           assert (
             phys_equal
               (Inference_client.Execution.context previous.inference_execution)
               (Inference_client.Execution.context current.inference_execution));
           [%test_eq: int]
             previous.reserved_history_through
             current.reserved_history_through;
           [%test_eq: int64]
             before.conversation.reserved_history_through
             after.conversation.reserved_history_through;
           [%test_eq: int64]
             before.conversation.next_history_sequence
             after.conversation.next_history_sequence;
           assert (
             List.equal
               P.History.equal_entry
               before.conversation.canonical_history
               after.conversation.canonical_history);
           [%test_eq: int] ports !(t.counters.inference_ports);
           [%test_eq: int] providers !(t.counters.providers);
           assert (Jsonaf.exactly_equal (F.ledger_json before) (F.ledger_json after));
           assert (List.is_empty after.moderator_executions);
           assert (
             Result.is_error
               (Actor.check_delegated_policy_caller
                  t.entry.actor
                  (Option.value_exn !escaped)));
           let old_request = ref None in
           let fresh_request = ref None in
           let completed =
             F.run t ~id:"597-unmoderated-second" (fun borrowed ->
               let caller = F.caller t borrowed in
               let candidate =
                 P.Id.Invocation.of_string "inv_597_unmoderated_configuration"
                 |> protocol_ok
               in
               let request = F.request t borrowed ~candidate caller in
               F.registered t request;
               old_request := Some request;
               let live = F.state t in
               let native_providers = !(t.counters.providers) in
               let patch =
                 P.Session_metadata.Patch.create
                   ~name:(Set "597 actual unmoderated metadata")
                   ~set_labels:[ "source", "unmoderated" ]
                   ~remove_labels:[]
                 |> protocol_ok
               in
               Actor.update_metadata
                 t.entry.actor
                 ~attachment_id:t.writer
                 ~expected_metadata_revision:live.identity.metadata_revision
                 ~patch
                 ()
               |> protocol_ok
               |> ignore;
               let metadata = F.state t in
               [%test_eq: int64]
                 Int64.(live.identity.metadata_revision + 1L)
                 metadata.identity.metadata_revision;
               F.registered t request;
               let handle = Option.value_exn t.entry.store_handle in
               let frames = Delegated_policy_claim_fault_tests.frames t.env handle in
               let failed =
                 protect_cleanup
                   ~finally:(fun () -> t.fault.phase <- Quiet)
                   (fun () ->
                      t.fault.phase <- Reject_settlement;
                      Actor.update_configuration
                        t.entry.actor
                        (config_request t ~key:"597-unmoderated-rejected"))
               in
               let marker = Option.value_exn t.fault.marker in
               let expected =
                 Store.Store_error.of_exn
                   ~operation:"read projection recovery marker"
                   ~path:marker
                   (Core_unix.Unix_error (EIO, "566 prewriter projection read", marker))
                 |> fun error ->
                 P.Error.create
                   Persistence_error
                   ~message:(Sexp.to_string_hum (Store.Store_error.sexp_of_t error))
                   ~retryable:false
                   ()
               in
               (match failed with
                | Error actual ->
                  assert (
                    Jsonaf.exactly_equal
                      (P.Error.to_json expected)
                      (P.Error.to_json actual))
                | Ok _ -> failwith "actual prewriter projection refusal succeeded");
               [%test_eq: int] 1 t.fault.settlement_rejections;
               assert (
                 Jsonaf.exactly_equal
                   (Legacy_idle_completion_fault_tests.state_json metadata)
                   (Legacy_idle_completion_fault_tests.state_json (F.state t)));
               [%test_eq: (int * string * string) list]
                 frames
                 (Delegated_policy_claim_fault_tests.frames t.env handle);
               F.registered t request;
               let view =
                 Actor.update_configuration
                   t.entry.actor
                   (config_request t ~key:"597-unmoderated-accepted")
                 |> protocol_ok
               in
               [%test_eq: int64]
                 Int64.(metadata.spec.configuration_revision + 1L)
                 view.revision;
               let acknowledged = F.state t in
               [%test_eq: int64] view.revision acknowledged.spec.configuration_revision;
               let target =
                 match Inference.Selection.view acknowledged.spec.inference_target with
                 | Captured target -> target
                 | Unresolved -> failwith "configuration lost captured target"
               in
               [%test_eq: string] "597-next-model" (Inference.Request.Target.model target);
               assert (phys_equal current.worker (Warm.loaded t.entry).worker);
               F.assert_unmoderated current acknowledged;
               stale t request;
               let fresh = F.caller t borrowed in
               let fresh = { request with caller = Some fresh } in
               F.registered t fresh;
               fresh_request := Some fresh;
               stale t request;
               [%test_eq: int] native_providers !(t.counters.providers);
               [%test_eq: int] ports !(t.counters.inference_ports);
               [%test_eq: int] 0 t.cache.cache_renames)
           in
           [%test_eq: int] 2 t.host.reads;
           assert (
             Result.is_error
               (Actor.check_delegated_policy_caller
                  t.entry.actor
                  (Option.value_exn !old_request)));
           assert (
             Result.is_error
               (Actor.check_delegated_policy_caller
                  t.entry.actor
                  (Option.value_exn !fresh_request)));
           assert (List.is_empty completed.moderator_executions);
           assert (List.is_empty completed.jobs);
           Resource_loan.release loan;
           F.within t "join retained unmoderated borrower before close" (fun () ->
             Resource_loan.join loan);
           [%test_eq: int] 0 t.cache.cache_renames;
           F.within t "actual unmoderated strong close" (fun () ->
             Owner.close_and_wait t.entry.runtime);
           [%test_eq: int] 1 t.cache.cache_renames;
           Owner.close_and_wait t.entry.runtime;
           [%test_eq: int] 1 t.cache.cache_renames)));
  [%expect {| |}]
;;
