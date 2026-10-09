open Core
open Agent_store_test_fixtures
open Job_store_fixtures
module P = Agent_protocol
module Store = Agent_store.Job_result_store
module Intent = Agent_store.Job_result_intent
module Blob = Agent_store.Blob_store

let publisher env sw blobs session =
  Store.Publisher.create
    ~env
    ~blobs
    ~sw
    ~session
    ~principal
    ~inline_bytes:64
    ~max_bytes:4096
  |> protocol_ok
;;

let prepare env sw blobs session job completion =
  Store.prepare
    blobs
    ~env
    ~sw
    ~session
    ~job
    ~creating_principal:principal
    ~now:timestamp
    ~max_bytes:4096
    completion
  |> store_ok
;;

let restore publisher jobs =
  Store.Publisher.restore
    publisher
    ~jobs
    ~generation:0
    ~max_count:16
    ~max_total_bytes:16384
;;

let%expect_test "fresh publishers recover complete stages but never replay partial work" =
  List.iter
    [ "durable"; "temporary"; "complete-part"; "short-part"; "missing" ]
    ~f:(fun stage ->
      with_store (fun env sw _ _ session _ ->
        let directory = Agent_store.Session_store.Handle.directory session in
        let temporary = Filename.concat directory "recovery-temporary" in
        let blobs =
          Blob.create
            ~env
            ~temporary_directory:temporary
            ~durable_directory:(Filename.concat directory "recovery-durable")
            ~max_upload_bytes:100000L
          |> store_ok
        in
        let job = job session in
        let completion = P.Completion.Succeeded (`String (String.make 512 'x')) in
        let prepared = prepare env sw blobs session job completion in
        let reference = Store.reference prepared in
        let id = P.Id.Blob.to_string reference.blob.id in
        let path directory suffix =
          Eio.Path.(Eio.Stdenv.fs env / Filename.concat directory (id ^ suffix))
        in
        let final = Filename.concat directory "blobs" in
        (match stage with
         | "durable" -> ()
         | stage ->
           Eio.Path.unlink (path final ".sexp");
           (match stage with
            | "temporary" -> Eio.Path.rename (path final ".blob") (path temporary ".blob")
            | "complete-part" ->
              Eio.Path.rename (path final ".blob") (path temporary ".part")
            | "short-part" ->
              let content = Eio.Path.load (path final ".blob") in
              Eio.Path.unlink (path final ".blob");
              Eio.Path.save
                ~create:(`Exclusive 0o600)
                (path temporary ".part")
                (String.prefix content 20)
            | "missing" -> Eio.Path.unlink (path final ".blob")
            | _ -> assert false));
        let fresh = publisher env sw blobs session in
        let recovered = restore fresh [ job ] |> protocol_ok in
        match recovered with
        | [] ->
          assert (String.equal stage "short-part" || String.equal stage "missing");
          assert (Option.is_none (Store.Publisher.pending_completion fresh ~job));
          [%test_eq: int]
            1
            (Intent.list ~env ~session ~max_count:8 |> store_ok |> List.length);
          print_endline (stage ^ ": retained for interruption, no completion fabricated")
        | [ (restored_job, actual, at) ] ->
          assert (P.Job.equal_kind restored_job.kind Async_tool);
          assert (P.Completion.equal actual completion);
          assert (P.Timestamp.equal at timestamp);
          let stored =
            Store.Publisher.publish
              fresh
              ~jobs:[ job ]
              ~job
              ~now:timestamp
              actual
              ~persist:(fun stored -> Ok stored)
            |> protocol_ok
          in
          (match stored with
           | Artifact { reference = actual; _ } ->
             assert (P.Id.Blob.equal reference.blob.id actual.blob.id)
           | _ -> failwith "recovery changed artifact storage");
          assert (
            P.Completion.equal
              completion
              (Store.Publisher.load fresh reference |> protocol_ok));
          assert (List.is_empty (Intent.list ~env ~session ~max_count:8 |> store_ok));
          print_endline (stage ^ ": original reference published and verified")
        | _ -> failwith "duplicate recovered attempt"));
  [%expect
    {|
    durable: original reference published and verified
    temporary: original reference published and verified
    complete-part: original reference published and verified
    short-part: retained for interruption, no completion fabricated
    missing: retained for interruption, no completion fabricated
    |}]
;;

let%expect_test
    "recovery budgets and conflicting attempts fail before caching; cancellation stays \
     terminal"
  =
  with_store (fun env sw blobs _ session _ ->
    let first = job session
    and second = job session in
    let completion text = P.Completion.Succeeded (`String (String.make 512 text)) in
    let first_prepared = prepare env sw blobs session first (completion 'a') in
    let second_prepared = prepare env sw blobs session second (completion 'b') in
    let fresh = publisher env sw blobs session in
    let jobs = [ first; second ] in
    let unchanged () =
      List.iter jobs ~f:(fun job ->
        assert (Option.is_none (Store.Publisher.pending_completion fresh ~job)));
      List.iter [ first_prepared; second_prepared ] ~f:(fun prepared ->
        assert (Result.is_ok (Store.Publisher.load fresh (Store.reference prepared))))
    in
    assert (
      Result.is_error
        (Store.Publisher.restore
           fresh
           ~jobs
           ~generation:0
           ~max_count:1
           ~max_total_bytes:16384));
    unchanged ();
    assert (
      Result.is_error
        (Store.Publisher.restore
           fresh
           ~jobs
           ~generation:0
           ~max_count:16
           ~max_total_bytes:600));
    unchanged ();
    let _conflict = prepare env sw blobs session first (completion 'c') in
    assert (Result.is_error (restore fresh jobs));
    unchanged ();
    let cancelled =
      { first with status = P.Job.Cancelled; completed_at = Some timestamp }
    in
    let newer = { second with attempt = second.attempt + 1 } in
    assert (List.is_empty (restore fresh [ cancelled; newer ] |> protocol_ok));
    unchanged ();
    print_endline
      "count, aggregate bytes and conflicting values rejected without cached selections \
       or data loss";
    print_endline "cancelled and superseded attempts ignored; original artifacts retained");
  [%expect
    {|
    count, aggregate bytes and conflicting values rejected without cached selections or data loss
    cancelled and superseded attempts ignored; original artifacts retained
    |}]
;;

let%expect_test
    "matching historical intents can recover an available body; corrupt or linked bodies \
     publish nothing"
  =
  with_store (fun env sw blobs _ session _ ->
    let job = job session in
    let completion = P.Completion.Succeeded (`String (String.make 512 'z')) in
    let abandoned = prepare env sw blobs session job completion in
    let complete = prepare env sw blobs session job completion in
    let directory = Agent_store.Session_store.Handle.directory session in
    let path prepared suffix =
      Eio.Path.(
        Eio.Stdenv.fs env
        / Filename.concat
            (Filename.concat directory "blobs")
            (P.Id.Blob.to_string (Store.reference prepared).blob.id ^ suffix))
    in
    Eio.Path.unlink (path abandoned ".blob");
    Eio.Path.unlink (path abandoned ".sexp");
    let content = Eio.Path.load (path complete ".blob") in
    let fresh = publisher env sw blobs session in
    let fails_without_cache () =
      assert (Result.is_error (restore fresh [ job ]));
      assert (Option.is_none (Store.Publisher.pending_completion fresh ~job));
      [%test_eq: int] 2 (Intent.list ~env ~session ~max_count:8 |> store_ok |> List.length)
    in
    Eio.Path.save ~create:(`Or_truncate 0o600) (path complete ".blob") "corrupt";
    fails_without_cache ();
    Eio.Path.unlink (path complete ".blob");
    let target_path = Filename.concat directory "foreign-completion" in
    let target = Eio.Path.(Eio.Stdenv.fs env / target_path) in
    Eio.Path.save ~create:(`Exclusive 0o600) target content;
    Eio.Path.symlink ~link_to:target_path (path complete ".blob");
    fails_without_cache ();
    [%test_eq: string] content (Eio.Path.load target);
    Eio.Path.unlink (path complete ".blob");
    Eio.Path.save ~create:(`Exclusive 0o600) (path complete ".blob") content;
    let restored = restore fresh [ job ] |> protocol_ok in
    [%test_eq: int] 1 (List.length restored);
    [%test_eq: int] 1 (restore fresh [ job ] |> protocol_ok |> List.length);
    let stored =
      Store.Publisher.publish
        fresh
        ~jobs:[ job ]
        ~job
        ~now:timestamp
        completion
        ~persist:(fun value -> Ok value)
      |> protocol_ok
    in
    (match stored with
     | Artifact { reference; _ } ->
       assert (P.Id.Blob.equal reference.blob.id (Store.reference complete).blob.id)
     | _ -> failwith "expected available historical artifact");
    let retained = Intent.list ~env ~session ~max_count:8 |> store_ok |> List.hd_exn in
    assert (
      P.Id.Blob.equal
        (Intent.reference retained).blob.id
        (Store.reference abandoned).blob.id);
    print_endline
      "corrupt and linked bodies rejected before caching; foreign target preserved";
    print_endline
      "matching incomplete intent did not hide the complete result; orphan retained for \
       collection");
  [%expect
    {|
    corrupt and linked bodies rejected before caching; foreign target preserved
    matching incomplete intent did not hide the complete result; orphan retained for collection
    |}]
;;

let%expect_test
    "fresh publisher preserves exact original completion and named publication evidence"
  =
  with_store (fun env sw blobs _ session _ ->
    let job = job session in
    let content =
      "{\"type\":\"succeeded\",\"value\":{\"retained\":true,\"lexical\":1e+00}}"
    in
    let completion = P.Completion.of_json (Jsonaf.of_string content) |> protocol_ok in
    let id = P.Id.Blob.create () in
    let blob =
      P.Blob.Metadata.create
        ~id
        ~kind:File
        ~media_type:P.Job_artifact.media_type
        ~byte_length:(Int64.of_int (String.length content))
        ~digest:Digestif.SHA256.(digest_string content |> to_hex)
        ()
      |> protocol_ok
    in
    let reference =
      P.Job_artifact.create
        ~session_id:job.session_id
        ~job_id:job.id
        ~generation:job.generation
        ~attempt:job.attempt
        ~blob
      |> protocol_ok
    in
    let metadata : Blob.Metadata.t =
      { blob
      ; creating_principal = principal
      ; target_session = Some job.session_id
      ; allowed_use = P.Job_artifact.allowed_use reference
      ; created_at = timestamp
      ; expires_at = None
      ; durable = false
      }
    in
    let original = Intent.create ~env ~session ~reference ~metadata |> store_ok in
    let doc =
      Agent_store.Blob_metadata_document.create metadata
      |> Result.map_error ~f:(fun error -> Agent_store.Store_error.Document error)
      |> store_ok
      |> Agent_store.Blob_metadata_document.to_document
      |> Result.map_error ~f:(fun error -> Agent_store.Store_error.Document error)
      |> store_ok
    in
    let json =
      match Document_schema.Document.json doc with
      | `Object fields ->
        `Object
          (List.Assoc.add
             fields
             ~equal:String.equal
             "future_integrity"
             (`Object [ "keep", `Null ]))
      | _ -> assert false
    in
    let metadata_doc =
      Document_schema.Document.inspect
        ~limits:Agent_store.Blob_metadata_document.limits
        json
      |> Result.map_error ~f:(fun error -> Agent_store.Store_error.Document error)
      |> store_ok
      |> Agent_store.Blob_metadata_document.of_document
      |> Result.map_error ~f:(fun error -> Agent_store.Store_error.Document error)
      |> store_ok
    in
    let stage =
      Agent_store.Blob_stage_documents.create metadata_doc
      |> Result.map_error ~f:(fun error -> Agent_store.Store_error.Document error)
      |> store_ok
    in
    let named =
      Agent_store.Job_result_intent_document.create ~reference ~stage
      |> Result.map_error ~f:(fun error -> Agent_store.Store_error.Document error)
      |> store_ok
      |> Agent_store.Job_result_intent_document.to_document
      |> Result.map_error ~f:(fun error -> Agent_store.Store_error.Document error)
      |> store_ok
    in
    let named =
      match Document_schema.Document.json named with
      | `Object fields ->
        Document_schema.Document.inspect
          ~limits:Agent_store.Job_result_intent_document.limits
          (`Object
              (List.Assoc.add
                 fields
                 ~equal:String.equal
                 "future_intent"
                 (`String "preserved")))
        |> Result.map_error ~f:(fun error -> Agent_store.Store_error.Document error)
        |> store_ok
      | _ -> assert false
    in
    let bytes =
      Agent_store.Document_record.encode
        named
        ~limits:Agent_store.Job_result_intent_document.limits
        ~flags:0
      |> Result.map_error ~f:Agent_store.Document_fields.record_error
      |> store_ok
    in
    let intent_path =
      Eio.Path.(
        Eio.Stdenv.fs env
        / Agent_store.Session_store.Handle.directory session
        / "result-preparations"
        / (P.Id.Blob.to_string (Intent.reference original).blob.id ^ ".frame"))
    in
    Eio.Path.save ~create:(`Or_truncate 0o600) intent_path bytes;
    Blob.ensure_staged_content blobs ~sw session ~stage content |> store_ok |> ignore;
    let fresh = publisher env sw blobs session in
    let recovered = restore fresh [ job ] |> protocol_ok in
    assert (List.length recovered = 1);
    let before = Eio.Path.load intent_path in
    let failed =
      Store.Publisher.publish
        fresh
        ~jobs:[ job ]
        ~job
        ~now:timestamp
        completion
        ~persist:(fun _ ->
          Error (P.Error.invalid_request "injected acknowledgement failure"))
    in
    assert (Result.is_error failed);
    assert (String.equal before (Eio.Path.load intent_path));
    let final_path =
      Eio.Path.(
        Eio.Stdenv.fs env
        / Agent_store.Session_store.Handle.directory session
        / "blobs"
        / (P.Id.Blob.to_string id ^ ".blob"))
    in
    assert (String.equal content (Eio.Path.load final_path));
    let metadata_path =
      Eio.Path.(
        Eio.Stdenv.fs env
        / Agent_store.Session_store.Handle.directory session
        / "blobs"
        / (P.Id.Blob.to_string id ^ ".sexp"))
    in
    assert (
      String.equal
        (Agent_store.Blob_stage_documents.durable_bytes stage)
        (Eio.Path.load metadata_path));
    Store.Publisher.publish
      fresh
      ~jobs:[ job ]
      ~job
      ~now:timestamp
      completion
      ~persist:(fun value -> Ok value)
    |> protocol_ok
    |> ignore;
    assert (List.is_empty (Intent.list ~env ~session ~max_count:8 |> store_ok));
    print_endline
      "restart retries original content/metadata/frame evidence; acknowledgement retires \
       intent last");
  [%expect
    {|restart retries original content/metadata/frame evidence; acknowledgement retires intent last|}]
;;

let%expect_test "noncanonical completion evidence is retained after semantic rejection" =
  with_store (fun env sw blobs _ session _ ->
    let job = job session in
    let content = " \n{\"type\":\"succeeded\",\"value\":{\"retained\":true}}\n " in
    let id = P.Id.Blob.create () in
    let blob =
      P.Blob.Metadata.create
        ~id
        ~kind:File
        ~media_type:P.Job_artifact.media_type
        ~byte_length:(Int64.of_int (String.length content))
        ~digest:Digestif.SHA256.(digest_string content |> to_hex)
        ()
      |> protocol_ok
    in
    let reference =
      P.Job_artifact.create
        ~session_id:job.session_id
        ~job_id:job.id
        ~generation:job.generation
        ~attempt:job.attempt
        ~blob
      |> protocol_ok
    in
    let metadata : Blob.Metadata.t =
      { blob
      ; creating_principal = principal
      ; target_session = Some job.session_id
      ; allowed_use = P.Job_artifact.allowed_use reference
      ; created_at = timestamp
      ; expires_at = None
      ; durable = false
      }
    in
    let intent = Intent.create ~env ~session ~reference ~metadata |> store_ok in
    Blob.ensure_staged_content blobs ~sw session ~stage:(Intent.stage intent) content
    |> store_ok
    |> ignore;
    let intent_path =
      Eio.Path.(
        Eio.Stdenv.fs env
        / Agent_store.Session_store.Handle.directory session
        / "result-preparations"
        / (P.Id.Blob.to_string id ^ ".frame"))
    in
    let original_frame = Eio.Path.load intent_path in
    assert (Result.is_error (restore (publisher env sw blobs session) [ job ]));
    [%test_eq: string] original_frame (Eio.Path.load intent_path);
    let reopened = Blob.open_session blobs session id |> store_ok in
    [%test_eq: string]
      content
      (Blob.load_verified blobs ~sw reopened ~max_bytes:4096 |> store_ok);
    print_endline "original digest verified; unsupported completion bytes retained");
  [%expect {|original digest verified; unsupported completion bytes retained|}]
;;
