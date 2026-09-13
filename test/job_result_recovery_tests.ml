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
