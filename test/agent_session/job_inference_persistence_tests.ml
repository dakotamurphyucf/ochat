open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module D = Document_schema
module SD = A.Session_state_document
module Persistence = A.Session_persistence
module Store = Agent_store
module Model = Job_inference_provenance_tests

let member = Goal_catalog_tests.member
let set = Goal_catalog_tests.set

let jobs_json restored =
  Persistence.Restored.state_document restored
  |> SD.encode ~limits:document_limits
  |> document_ok
  |> D.Document.json
  |> fun json -> member (member json "payload") "jobs"
;;

let imported state =
  let state, bound = Model.associated state ~attempt:0 in
  let historical =
    { (Model.job ()) with id = P.Id.Job.of_string "job_historical_origin" |> protocol_ok }
  in
  let unbound =
    { (Model.job ~inference_origin:Unbound ()) with
      id = P.Id.Job.of_string "job_unbound_origin" |> protocol_ok
    }
  in
  let state =
    { state with
      jobs = [ bound; historical; unbound ]
    ; counters = { state.counters with transaction_sequence = 0L; event_sequence = 0L }
    }
  in
  let document = SD.authored state |> SD.encode ~limits:document_limits |> document_ok in
  let raw = D.Document.json document in
  let raw_jobs =
    List.map state.jobs ~f:(fun job ->
      let raw = P.Job.to_json job in
      let raw = set raw "future_job" (`String (P.Id.Job.to_string job.id)) in
      match job.inference_origin with
      | None -> raw
      | Some _ ->
        set
          raw
          "inference_origin"
          (set
             (member raw "inference_origin")
             "future_origin"
             (`Object [ "exact", `True ])))
  in
  let raw = set raw "payload" (set (member raw "payload") "jobs" (`Array raw_jobs)) in
  let document = D.Document.inspect ~limits:document_limits raw |> document_ok in
  SD.decode ~limits:document_limits document
  |> document_ok
  |> Persistence.Restored.of_state_document
;;

(* Imported storage facts, not authentic inference admission. Actual disk owners
   below qualify failure, journal replay and checkpoint custody only. *)
let%expect_test "origin custody survives failed save and journal or checkpoint reopen" =
  List.iter [ false; true ] ~f:(fun checkpoint ->
    Run_admission_tests.with_startup_actor
      (fun actor _ _ request start _ _ _ _ ->
         start (request "origin-storage") |> protocol_ok |> ignore;
         A.Session_actor.state actor |> protocol_ok |> imported)
      ~after_close:(fun env workspace initial ->
        let directory =
          Filename.concat workspace.canonical_root.native_path "origin-journal"
        in
        let snapshots =
          Filename.concat workspace.canonical_root.native_path "origin-snapshots"
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
                let reject = ref true in
                let durable =
                  Persistence.create
                    ~archive:(fun _ _ -> failwith "unexpected origin archive")
                    ~pending_archive:None
                    ~before_commit:
                      (Some
                         (fun _ ->
                           if !reject
                           then
                             Error
                               (P.Error.create
                                  Persistence_error
                                  ~message:"before origin write"
                                  ~retryable:true
                                  ())
                           else Ok ()))
                    ~command_accepted:(fun _ _ -> ())
                    ~writer
                    ~durability:Flush
                    ~limits:document_limits
                    ~archive_limits:document_limits
                    ~retention_preflight:None
                    ~restored:initial
                    ~previous_transaction_hash:None
                in
                let before = Persistence.Restored.state initial in
                let job = List.hd_exn before.jobs in
                let deltas =
                  A.Run_job_completion.cancel_queued before ~job ~now:timestamp
                  |> protocol_ok
                in
                let cancelled =
                  { job with
                    status = Cancelled
                  ; completed_at = Some timestamp
                  ; result =
                      Some (P.Completion.to_json (Cancelled "cancelled before claim"))
                  }
                in
                let proposed =
                  Model.transition before (Batch (Job_changed cancelled :: deltas))
                in
                assert (
                  Result.is_error
                    (Persistence.commit
                       durable
                       ~command_audit:None
                       ~previous:before
                       proposed));
                assert (
                  D.Json.equal
                    (jobs_json initial)
                    (jobs_json (Persistence.restored durable)));
                [%test_eq: int64]
                  (Persistence.Restored.state (Persistence.restored durable)).counters
                    .transaction_sequence
                  0L;
                reject := false;
                Persistence.commit durable ~command_audit:None ~previous:before proposed
                |> protocol_ok;
                let accepted = Persistence.restored durable in
                let accepted_state = Persistence.Restored.state accepted in
                (match jobs_json initial, jobs_json accepted with
                 | `Array before_jobs, `Array after_jobs ->
                   List.iter2_exn before_jobs after_jobs ~f:(fun before after ->
                     assert (
                       D.Json.equal
                         (member before "future_job")
                         (member after "future_job"));
                     match
                       List.Assoc.find
                         (match before with
                          | `Object fields -> fields
                          | _ -> [])
                         ~equal:String.equal
                         "inference_origin"
                     with
                     | None -> ()
                     | Some origin ->
                       assert (
                         D.Json.equal
                           (member origin "future_origin")
                           (member (member after "inference_origin") "future_origin")))
                 | _ -> failwith "expected Job arrays");
                [%test_eq: int64] accepted_state.counters.transaction_sequence 1L;
                (* Replay decoding cannot silently strip or replace the immutable
                 known child, even before the physical writer is considered. *)
                let forged =
                  A.Session_delta.Job_changed
                    { cancelled with inference_origin = Some Unbound }
                in
                let delta =
                  A.Session_delta_document.create
                    forged
                    ~limits:document_limits
                    ~state_document:SD.authored
                  |> document_ok
                  |> A.Session_delta_document.document
                  |> A.Session_delta_document.decode ~limits:document_limits
                  |> document_ok
                in
                assert (
                  Result.is_error
                    (A.Session_delta_document.apply
                       delta
                       ~limits:document_limits
                       (Persistence.Restored.state_document accepted)));
                if checkpoint
                then
                  ignore
                    (Persistence.install_snapshot_at
                       durable
                       ~env
                       ~directory:snapshots
                       ~max_payload_length:1048576
                       ~transaction_hash:(Persistence.transaction_hash durable)
                       accepted_state
                     |> store_ok
                     : Store.Snapshot.installed);
                accepted))
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
              ~initial
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
              assert (D.Json.equal (jobs_json expected) (jobs_json recovered.state));
              let actual = Persistence.Restored.state recovered.state in
              let origins =
                List.map actual.jobs ~f:(fun (job : P.Job.t) -> job.inference_origin)
              in
              (match origins with
               | [ Some (Run _); None; Some Unbound ] -> ()
               | _ -> failwith "origin presence changed across disk recovery");
              let bound = List.hd_exn actual.jobs in
              [%test_eq: int] bound.attempt 0;
              assert (
                match bound.status with
                | Cancelled -> true
                | _ -> false);
              let index = Option.value_exn actual.run_state in
              let run = List.hd_exn (A.Run_state.runs index) in
              assert (
                List.exists run.terminal_work ~f:(fun proof ->
                  match proof.P.Run_work.Terminal.work.key, proof.outcome with
                  | Retained (Job { id; attempt = 1 }), Cancelled ->
                    P.Id.Job.equal id bound.id
                  | _ -> false))))));
  print_endline
    "pre-write rejection unchanged; exact current retry; immutable decoded delta; both \
     disk paths retain unavailable/unbound/Run and unknowns";
  [%expect
    {| pre-write rejection unchanged; exact current retry; immutable decoded delta; both disk paths retain unavailable/unbound/Run and unknowns |}]
;;
