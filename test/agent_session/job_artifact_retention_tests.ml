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
  let storage = ref None in
  with_actor
    ~make_job_results:(fun env sw initial ->
      let value = Job_artifact_fixtures.create env sw initial in
      storage := Some value;
      Some value.publisher)
    (fun env sw actor _ _ ->
       let storage = Option.value_exn !storage in
       let session = storage.session in
       let directory = Agent_store.Session_store.Handle.directory session in
       let data_root = Agent_store.Session_store.data_root storage.sessions in
       let principal = P.Id.Principal.create () in
       let job = add_claimed_job actor in
       let completion = P.Completion.Succeeded (`String (String.make 512 'x')) in
       let prepared =
         Results.prepare
           storage.blobs
           ~env
           ~sw
           ~session
           ~job
           ~creating_principal:principal
           ~now:timestamp
           ~max_bytes:4096
           completion
         |> store_ok
       in
       let reference = Results.reference prepared in
       let intent = Intent.list ~env ~session ~max_count:8 |> store_ok |> List.hd_exn in
       let metadata = Intent.metadata intent in
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
         (Blob.Metadata.sexp_of_t metadata |> Sexp.to_string_mach);
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
           ~created_at:timestamp
           ~expires_at:metadata.expires_at
         |> store_ok
       in
       Blob.write_string forged "caller label without private intent" |> store_ok;
       let forged = Blob.finish forged ~expected_digest:None |> store_ok in
       let future =
         P.Timestamp.to_time_ns timestamp
         |> fun at -> Time_ns.add at (Time_ns.Span.of_day 3.) |> P.Timestamp.of_time_ns
       in
       let idempotency_store =
         Agent_store.Idempotency_store.open_or_create
           ~env
           ~path:(Filename.concat directory "expiry-idempotency")
         |> store_ok
       in
       let maintenance () =
         Agent_server.Maintenance.run_once
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
         (Blob.Metadata.sexp_of_t metadata |> Sexp.to_string_mach);
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
       let _ =
         Results.Publisher.publish
           storage.publisher
           ~jobs:[ job ]
           ~job
           ~now:future
           completion
           ~persist:(fun stored -> Ok stored)
         |> protocol_ok
       in
       assert (
         P.Completion.equal
           completion
           (Results.Publisher.load storage.publisher reference |> protocol_ok));
       assert (List.is_empty (Intent.list ~env ~session ~max_count:8 |> store_ok));
       print_endline
         "expired caller label removed; exact private preparation preserved through \
          repeated maintenance";
       print_endline
         "corrupt intent prevented deletion; repaired preparation recovered under its \
          original reference";
       print_endline
         "linked intent directories and mismatched metadata filenames refused without \
          deleting the result");
  [%expect
    {|
    expired caller label removed; exact private preparation preserved through repeated maintenance
    corrupt intent prevented deletion; repaired preparation recovered under its original reference
    linked intent directories and mismatched metadata filenames refused without deleting the result
    |}]
;;
