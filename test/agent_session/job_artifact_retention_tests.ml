open Core
open Fixtures
open Job_fixtures
module P = Agent_protocol
module Blob = Agent_store.Blob_store
module Intent = Agent_store.Job_result_intent
module Results = Agent_store.Job_result_store

let%expect_test
    "maintenance protects private preparations, expires forged labels and preserves \
     corrupt intent evidence"
  =
  let maximum =
    P.Timestamp.of_time_ns (Time_ns.of_int63_ns_since_epoch Int63.max_value)
  in
  List.iter [ timestamp; maximum ] ~f:(fun created_at ->
    let boundary = P.Timestamp.equal created_at maximum in
    let now = ref timestamp in
    let reject_save = ref false in
    let storage = ref None in
    with_actor
      ~now:(fun () -> !now)
      ~reject_save:(fun _ -> !reject_save)
      ~make_job_results:(fun env sw initial ->
        let value = Job_artifact_fixtures.create env sw initial in
        storage := Some value;
        Some value.publisher)
      (fun env sw actor _ backend ->
         let storage = Option.value_exn !storage in
         let session = storage.session in
         let directory = Agent_store.Session_store.Handle.directory session in
         let data_root = Agent_store.Session_store.data_root storage.sessions in
         let principal = P.Id.Principal.create () in
         let job =
           if boundary
           then
             add_claimed_job
               actor
               ~retry_policy:(Safe_retry { max_attempts = 2; backoff_ms = 1 })
           else add_claimed_job actor
         in
         let completion =
           if boundary
           then
             P.Completion.Failed
               { code = "fixture.busy"
               ; message = "Original result at the timestamp limit"
               ; retryable = true
               ; details = `Object [ "original", `String (String.make 512 'x') ]
               }
           else P.Completion.Succeeded (`String (String.make 512 'x'))
         in
         let finish () =
           A.complete_background_job
             actor
             ~job_id:job.id
             ~generation:job.generation
             ~attempt:job.attempt
             completion
         in
         let reference =
           if boundary
           then (
             (* The actual Actor forces a new artifact allocation at maximum.
                Only its persistence callback fails, after preparation. *)
             now := created_at;
             let before = A.state actor |> protocol_ok in
             let durable_before = Agent_session.Memory_backend.state backend in
             reject_save := true;
             (match finish () with
              | Error error ->
                assert (P.Error.equal_code error.code Internal_error);
                [%test_eq: string] "injected job transition failure" error.message
              | Ok _ -> failwith "injected prewriter failure committed");
             assert (phys_equal before (A.state actor |> protocol_ok));
             assert (
               phys_equal durable_before (Agent_session.Memory_backend.state backend));
             assert (
               P.Completion.equal
                 completion
                 (Results.Publisher.pending_completion storage.publisher ~job
                  |> Option.value_exn));
             reject_save := false;
             Intent.list ~env ~session ~max_count:8
             |> store_ok
             |> List.hd_exn
             |> Intent.reference)
           else
             Results.prepare
               storage.blobs
               ~env
               ~sw
               ~session
               ~job
               ~creating_principal:principal
               ~now:created_at
               ~max_bytes:4096
               completion
             |> store_ok
             |> Results.reference
         in
         let intent = Intent.list ~env ~session ~max_count:8 |> store_ok |> List.hd_exn in
         assert (
           Jsonaf.exactly_equal
             (P.Job_artifact.to_json reference)
             (P.Job_artifact.to_json (Intent.reference intent)));
         let metadata = Intent.metadata intent in
         assert (P.Timestamp.equal metadata.created_at created_at);
         let expiry = Option.value_exn metadata.expires_at in
         if boundary
         then assert (P.Timestamp.equal expiry maximum)
         else
           [%test_eq: int64] 86_400_000_000_000L (P.Timestamp.diff_ns expiry created_at);
         let id = P.Id.Blob.to_string reference.blob.id in
         let path directory suffix =
           Eio.Path.(Eio.Stdenv.fs env / Filename.concat directory (id ^ suffix))
         in
         let final = Filename.concat directory "blobs" in
         Eio.Path.rename (path final ".blob") (path storage.temporary_directory ".blob");
         Eio.Path.unlink (path final ".sexp");
         Eio.Path.save
           ~create:(`Exclusive 0o600)
           (path storage.temporary_directory ".sexp")
           (Agent_store.Blob_stage_documents.temporary_bytes (Intent.stage intent));
         let forged =
           Blob.begin_upload
             storage.blobs
             ~sw
             ~id:(P.Id.Blob.create ())
             ~creating_principal:principal
             ~target_session:(Some job.session_id)
             ~kind:File
             ~media_type:P.Job_artifact.media_type
             ~display_name:None
             ~allowed_use:metadata.allowed_use
             ~created_at
             ~expires_at:metadata.expires_at
           |> store_ok
         in
         Blob.write_string forged "caller label without private intent" |> store_ok;
         let forged = Blob.finish forged ~expected_digest:None |> store_ok in
         let future =
           if boundary
           then maximum
           else P.Timestamp.add_ms created_at 259_200_000 |> protocol_ok
         in
         let idempotency_store =
           Agent_store.Idempotency_store.open_or_create
             ~env
             ~path:(Filename.concat directory "expiry-idempotency")
           |> store_ok
         in
         let maintenance () =
           Agent_server.Maintenance.run_once
             ~collection_policy:Load_retained
             ~registry:None
             ~env
             ~idempotency_store
             ~blob_store:storage.blobs
             ~session_store:storage.sessions
             ~protected_response_sessions:[ job.session_id ]
             ~response_retention:(Time_ns.Span.of_day 1.)
             ~now:future
         in
         let stats = maintenance () |> store_ok in
         [%test_eq: int] 1 stats.expired_temporary_blobs;
         assert (
           Result.is_error
             (Blob.open_temporary storage.blobs (Blob.Handle.metadata forged).blob.id));
         [%test_eq: string]
           (P.Completion.to_json completion |> Jsonaf.to_string)
           (Eio.Path.load (path storage.temporary_directory ".blob"));
         assert (Intent.protects_temporary ~env ~data_root metadata |> store_ok);
         let intent_file =
           path (Filename.concat directory "result-preparations") ".frame"
         in
         let original = Eio.Path.load intent_file in
         Eio.Path.save ~create:(`Or_truncate 0o600) intent_file "corrupt private intent";
         assert (Result.is_error (maintenance ()));
         [%test_eq: string] "corrupt private intent" (Eio.Path.load intent_file);
         assert (Eio.Path.is_file (path storage.temporary_directory ".blob"));
         Eio.Path.save ~create:(`Or_truncate 0o600) intent_file original;
         let intent_directory = Filename.concat directory "result-preparations" in
         let saved_directory = Filename.concat directory "saved-preparations" in
         let intent_path = Eio.Path.(Eio.Stdenv.fs env / intent_directory) in
         let saved_path = Eio.Path.(Eio.Stdenv.fs env / saved_directory) in
         Eio.Path.rename intent_path saved_path;
         Eio.Path.symlink ~link_to:saved_directory intent_path;
         assert (Result.is_error (maintenance ()));
         assert (Eio.Path.is_file (path storage.temporary_directory ".blob"));
         Eio.Path.unlink intent_path;
         Eio.Path.rename saved_path intent_path;
         let alias =
           Eio.Path.(
             Eio.Stdenv.fs env
             / Filename.concat
                 storage.temporary_directory
                 (P.Id.Blob.to_string (P.Id.Blob.create ()) ^ ".sexp"))
         in
         Eio.Path.save
           ~create:(`Exclusive 0o600)
           alias
           (Agent_store.Blob_stage_documents.temporary_bytes (Intent.stage intent));
         assert (Result.is_error (maintenance ()));
         assert (Eio.Path.is_file alias);
         assert (Eio.Path.is_file (path storage.temporary_directory ".blob"));
         Eio.Path.unlink alias;
         let stats = maintenance () |> store_ok in
         [%test_eq: int] 0 stats.expired_temporary_blobs;
         let restored =
           Results.Publisher.restore
             storage.publisher
             ~jobs:[ job ]
             ~generation:job.generation
             ~max_count:8
             ~max_total_bytes:4096
           |> protocol_ok
         in
         [%test_eq: int] 1 (List.length restored);
         now := future;
         let terminal = finish () |> protocol_ok in
         [%test_eq: int] job.attempt terminal.attempt;
         assert (Option.is_none terminal.next_run_at);
         assert (Option.equal P.Timestamp.equal terminal.completed_at (Some future));
         (match boundary, terminal.status with
          | false, Succeeded -> ()
          | true, Failed failure ->
            [%test_eq: string] "Original result at the timestamp limit" failure.message;
            assert failure.retryable
          | _, _ -> failwith "artifact completion did not settle its original outcome");
         (match P.Job.terminal_result terminal |> protocol_ok with
          | Some (Artifact { reference = published; _ }) ->
            assert (
              Jsonaf.exactly_equal
                (P.Job_artifact.to_json reference)
                (P.Job_artifact.to_json published))
          | Some (Inline _) | None -> failwith "the selected result artifact changed");
         assert (
           P.Completion.equal
             completion
             (P.Job.terminal_completion
                ~load_artifact:(Results.Publisher.load storage.publisher)
                terminal
              |> protocol_ok
              |> Option.value_exn));
         let committed = A.state actor |> protocol_ok in
         (match finish () with
          | Error error -> assert (P.Error.equal_code error.code Already_resolved)
          | Ok _ -> failwith "terminal completion replayed");
         assert (phys_equal committed (A.state actor |> protocol_ok));
         assert (
           P.Completion.equal
             completion
             (Results.Publisher.load storage.publisher reference |> protocol_ok));
         assert (List.is_empty (Intent.list ~env ~session ~max_count:8 |> store_ok));
         if not boundary
         then (
           print_endline
             "expired caller label removed; exact private preparation preserved through \
              repeated maintenance";
           print_endline
             "corrupt intent prevented deletion; repaired preparation recovered under \
              its original reference";
           print_endline
             "linked intent directories and mismatched metadata filenames refused \
              without deleting the result")));
  print_endline
    "one-day and maximum expiry preserve ownership; actual Actor publishes the original \
     artifact once";
  [%expect
    {|
    expired caller label removed; exact private preparation preserved through repeated maintenance
    corrupt intent prevented deletion; repaired preparation recovered under its original reference
    linked intent directories and mismatched metadata filenames refused without deleting the result
    one-day and maximum expiry preserve ownership; actual Actor publishes the original artifact once
    |}]
;;
