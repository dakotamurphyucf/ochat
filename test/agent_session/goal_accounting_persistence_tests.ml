open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module G = P.Goal
module L = A.Inference_ledger
module Accounts = A.Goal_accounts
module Child = A.Goal_accounting_document
module Accounting = A.Goal_accounting
module SD = A.Session_state_document
module Persistence = A.Session_persistence
module Store = Agent_store
module D = Document_schema

let member = Goal_catalog_tests.member
let set = Goal_catalog_tests.set

let ledger result =
  Result.map_error result ~f:(fun e -> Sexp.to_string_hum (L.Error.sexp_of_t e))
  |> Result.ok_or_failwith
;;

let account result =
  Result.map_error result ~f:(fun e -> Sexp.to_string_hum (Accounting.Error.sexp_of_t e))
  |> Result.ok_or_failwith
;;

let accounts state = Option.value_exn state.A.Session_state.goal_accounts
let goal state = Goal_run_tests.current_goal state

let resolve state =
  Accounts.find (accounts state) (G.id (goal state))
  |> Option.value_exn
  |> fun child ->
  Child.resolve child ~goal:(goal state) ~ledger:state.inference_ledger |> protocol_ok
;;

let transition before delta =
  A.Session_transition.apply ~now:timestamp before ~delta ~payloads:[] |> protocol_ok
;;

let record_with_plan state =
  let _, _, prepared = Inference_tracking_tests.prepared () in
  let after, handle, tracking =
    L.admit
      state.A.Session_state.inference_ledger
      ~source:(Transcript.Source_id.of_string "stored-accounting" |> Result.ok_or_failwith)
      ~relation:Root
      ~operation_id:None
      ~invocation_id:None
      ~configuration:(Inference_runtime.Prepared.configuration prepared)
    |> ledger
  in
  let after, _ =
    L.observe after handle (Inference_tracking_tests.usage handle 0L 7L) |> ledger
  in
  let plan =
    Accounting.plan
      (resolve state)
      ~goal:(goal state)
      ~before:state.inference_ledger
      ~after
      ~admission:
        (Some
           (Accounting.Admission.Attempt
              { episode = G.episode (goal state) |> Option.value_exn; handle; tracking }))
    |> account
  in
  let candidate = Accounts.apply (accounts state) plan |> protocol_ok in
  ( transition
      state
      (Batch [ Inference_ledger_changed after; Goal_accounts_changed candidate ])
  , handle
  , plan )
;;

let record state =
  let proposed, handle, _ = record_with_plan state in
  proposed, handle
;;

let account_json state = Accounts.to_jsonaf (accounts state) |> protocol_ok

(* These are imported model association facts and real canonical ledger data,
   never execution permission. The physical writer below qualifies storage only;
   authored controller/provider admission remains a separate integration gate. *)
let%expect_test
    "accounting journal and snapshot restore exact custody after failed save and lost ACK"
  =
  List.iter [ false; true ] ~f:(fun checkpoint ->
    let association = ref None in
    Goal_run_tests.with_model
      (fun ~basis ~held ~admission:_ ~intent:_ ~delta _ ->
         let initial =
           { basis with
             goal_catalog = Some held
           ; counters =
               { basis.counters with transaction_sequence = 0L; event_sequence = 0L }
           }
         in
         let held_goal =
           A.Goal_catalog.find held Goal_run_tests.goal_id |> Option.value_exn
         in
         let stored =
           Accounts.bootstrap
             Accounts.empty
             ~goal:held_goal
             ~ledger:initial.inference_ledger
           |> protocol_ok
         in
         let raw = Accounts.to_jsonaf stored |> protocol_ok in
         let raw = set raw "future_envelope" (`String "retained") in
         let raw =
           set
             raw
             "payload"
             (set
                (member raw "payload")
                "future_accounts"
                (`Object [ "protected", `True ]))
         in
         let stored = Accounts.of_jsonaf raw |> protocol_ok in
         association
         := Some
              (A.Session_delta.Batch
                 [ Goal_accounts_changed stored
                 ; Goal_catalog_changed (Option.value_exn basis.goal_catalog)
                 ; delta
                 ]);
         initial)
      ~after_close:(fun env workspace initial ->
        let directory =
          Filename.concat workspace.canonical_root.native_path "accounting-journal"
        in
        let snapshots =
          Filename.concat workspace.canonical_root.native_path "accounting-snapshots"
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
            Exn.protect
              ~finally:(fun () -> Store.Commit_writer.close writer)
              ~f:(fun () ->
                let reject = ref false in
                let durable =
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
                                  ~message:"before accounting write"
                                  ~retryable:true
                                  ())
                           else Ok ()))
                    ~command_accepted:(fun _ _ -> ())
                    ~writer
                    ~durability:Flush
                    ~limits:document_limits
                    ~archive_limits:document_limits
                    ~retention_preflight:None
                    ~restored:(Persistence.Restored.authored initial)
                    ~previous_transaction_hash:None
                in
                let started = transition initial (Option.value_exn !association) in
                Persistence.commit durable ~command_audit:None ~previous:initial started
                |> protocol_ok;
                let before = Persistence.Restored.state (Persistence.restored durable) in
                let proposed, handle, plan = record_with_plan before in
                reject := true;
                assert (
                  Result.is_error
                    (Persistence.commit
                       durable
                       ~command_audit:None
                       ~previous:before
                       proposed));
                assert (
                  D.Json.equal
                    (account_json before)
                    (account_json
                       (Persistence.Restored.state (Persistence.restored durable))));
                [%test_eq: int]
                  (List.length
                     (L.rows
                        (Persistence.Restored.state (Persistence.restored durable))
                          .inference_ledger))
                  0;
                reject := false;
                let lost_ack () =
                  let%bind.Result () =
                    Persistence.commit
                      durable
                      ~command_audit:None
                      ~previous:before
                      proposed
                  in
                  Error
                    (P.Error.create
                       Persistence_error
                       ~message:"lost accounting acknowledgement"
                       ~retryable:true
                       ())
                in
                assert (Result.is_error (lost_ack ()));
                let accepted =
                  Persistence.Restored.state (Persistence.restored durable)
                in
                [%test_eq: int64] (Accounting.admitted_attempts (resolve accepted)) 1L;
                (* The sole current actor rejects stale accounting plans before
                   calling its physical writer. Replaying a stale transaction
                   directly is outside that writer's sequence contract. *)
                (match Accounts.apply (accounts accepted) plan with
                 | Error error ->
                   [%test_eq: string]
                     error.P.Error.message
                     "accounting plan does not match stored frontier"
                 | Ok _ -> failwith "stale accounting plan accepted");
                (* Closure changes no account identity. A retained authoritative usage
               replacement still updates the canonical live contribution. *)
                let closed_goal =
                  G.abandon
                    (goal accepted)
                    (G.Reason.of_string "Storage fixture complete" |> protocol_ok)
                  |> protocol_ok
                in
                let catalog = Option.value_exn accepted.goal_catalog in
                let document =
                  A.Goal_catalog.document catalog (G.id closed_goal) |> Option.value_exn
                in
                let document =
                  A.Goal_document.update document closed_goal |> document_ok
                in
                let catalog =
                  A.Goal_catalog.advance_existing catalog ~goal:document |> protocol_ok
                in
                let ledger, _ =
                  L.observe
                    accepted.inference_ledger
                    handle
                    (Inference_tracking_tests.usage handle 1L 11L)
                  |> ledger
                in
                let corrected =
                  transition
                    accepted
                    (Batch
                       [ Goal_catalog_changed catalog; Inference_ledger_changed ledger ])
                in
                Persistence.commit
                  durable
                  ~command_audit:None
                  ~previous:accepted
                  corrected
                |> protocol_ok;
                let expected =
                  Persistence.Restored.state (Persistence.restored durable)
                in
                if checkpoint
                then
                  ignore
                    (Persistence.install_snapshot_at
                       durable
                       ~env
                       ~directory:snapshots
                       ~max_payload_length:1048576
                       ~transaction_hash:(Persistence.transaction_hash durable)
                       expected
                     |> store_ok
                     : Store.Snapshot.installed);
                expected))
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
          Exn.protect
            ~finally:(fun () -> Store.Commit_writer.close writer)
            ~f:(fun () ->
              let actual = Persistence.Restored.state recovered.state in
              assert (D.Json.equal (account_json actual) (account_json expected));
              assert (Accounting.equal (resolve actual) (resolve expected));
              let usage =
                Accounting.usage (resolve actual) ~ledger:actual.inference_ledger
                |> account
              in
              let input = A.Goal_usage.component usage Input in
              assert (
                match A.Goal_usage.Metric.actual input with
                | Tokens value -> Int64.equal value 11L
                | Overflow -> false);
              [%test_eq: int64] (Accounting.admitted_attempts (resolve actual)) 1L;
              assert (
                match G.state (goal actual) with
                | Closed (Abandoned _) -> true
                | Open _ | Closed (Achieved _) -> false);
              let raw = account_json actual in
              assert (D.Json.equal (member raw "future_envelope") (`String "retained"));
              assert (
                D.Json.equal
                  (member (member raw "payload") "future_accounts")
                  (`Object [ "protected", `True ]))))));
  print_endline
    "failed save unchanged; lost ACK retains one charge; journal and snapshot agree";
  [%expect
    {| failed save unchanged; lost ACK retains one charge; journal and snapshot agree |}]
;;
