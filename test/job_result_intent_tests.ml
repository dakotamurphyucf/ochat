open Core
open Agent_store_test_fixtures
open Job_store_fixtures
module Intent = Agent_store.Job_result_intent
module Store = Agent_store.Job_result_store
module Blob = Agent_store.Blob_store
module P = Agent_protocol

let prepare env sw blobs session completion =
  Store.prepare
    blobs
    ~env
    ~sw
    ~session
    ~job:(job session)
    ~creating_principal:principal
    ~now:timestamp
    ~max_bytes:4096
    completion
;;

let%expect_test
    "intent scans reject corrupt, forged and linked records without deleting artifact \
     data"
  =
  with_store (fun env sw blobs _ first second ->
    let completion = P.Completion.Succeeded (`String "retained data") in
    let prepared = prepare env sw blobs first completion |> store_ok in
    let reference = Store.reference prepared in
    let intent =
      Intent.list ~env ~session:first ~max_count:8 |> store_ok |> List.hd_exn
    in
    assert (Result.is_error (Intent.list ~env ~session:first ~max_count:0));
    assert (Result.is_error (Intent.remove ~env ~session:second intent));
    let directory =
      Filename.concat
        (Agent_store.Session_store.Handle.directory first)
        "result-preparations"
    in
    let path =
      Filename.concat directory (P.Id.Blob.to_string reference.blob.id ^ ".frame")
    in
    let file = Eio.Path.(Eio.Stdenv.fs env / path) in
    let original = Eio.Path.load file in
    let changed = Bytes.of_string original in
    Bytes.set
      changed
      (Bytes.length changed - 1)
      (Char.of_int_exn ((Char.to_int original.[String.length original - 1] + 1) mod 256));
    List.iter
      [ Bytes.to_string changed
      ; String.prefix original (String.length original - 1)
      ; original ^ "trailer"
      ; String.make 40000 'x'
      ]
      ~f:(fun bytes ->
        Eio.Path.save ~create:(`Or_truncate 0o600) file bytes;
        assert (Result.is_error (Intent.list ~env ~session:first ~max_count:8));
        assert (Result.is_error (Intent.remove ~env ~session:first intent));
        assert (
          P.Completion.equal
            completion
            (Store.load blobs ~sw ~session:first ~max_bytes:4096 reference |> store_ok)));
    Eio.Path.save ~create:(`Or_truncate 0o600) file original;
    let forged = Eio.Path.(Eio.Stdenv.fs env / directory / "blb_forged.frame") in
    Eio.Path.save ~create:(`Exclusive 0o600) forged original;
    assert (Result.is_error (Intent.list ~env ~session:first ~max_count:8));
    Eio.Path.unlink forged;
    let backup_path =
      Filename.concat
        (Agent_store.Session_store.Handle.directory first)
        "intent-fixture.backup"
    in
    let backup = Eio.Path.(Eio.Stdenv.fs env / backup_path) in
    Eio.Path.rename file backup;
    Eio.Path.symlink ~link_to:backup_path file;
    assert (Result.is_error (Intent.list ~env ~session:first ~max_count:8));
    assert (Result.is_error (Intent.remove ~env ~session:first intent));
    [%test_eq: string] original (Eio.Path.load backup);
    Eio.Path.unlink file;
    Eio.Path.rename backup file;
    Store.discard prepared |> store_ok;
    assert (List.is_empty (Intent.list ~env ~session:first ~max_count:8 |> store_ok));
    print_endline
      "corrupt/truncated/oversized/trailing records, forged names, foreign sessions and \
       symlinks rejected";
    print_endline
      "artifact and link target stayed intact; verified uncommitted discard removed its \
       marker");
  [%expect
    {|
    corrupt/truncated/oversized/trailing records, forged names, foreign sessions and symlinks rejected
    artifact and link target stayed intact; verified uncommitted discard removed its marker
    |}]
;;

let%expect_test
    "a failed blob metadata save leaves a prior durable intent for unpaired data"
  =
  let armed = ref None in
  let failed_path = ref None in
  with_store
    ~wrap_env:(fun env ->
      fault_env ~on_failure:(fun path -> failed_path := Some path) env armed)
    (fun env sw blobs _ session _ ->
       armed := Some false;
       let completion = P.Completion.Succeeded (`String "stage survived") in
       assert (Result.is_error (prepare env sw blobs session completion));
       let intent = Intent.list ~env ~session ~max_count:8 |> store_ok |> List.hd_exn in
       let reference = Intent.reference intent in
       let data_path =
         (Option.value_exn !failed_path |> String.chop_suffix_exn ~suffix:".sexp")
         ^ ".blob"
       in
       [%test_eq: string]
         (P.Completion.to_json completion |> Jsonaf.to_string)
         (Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / data_path));
       assert (Result.is_error (Blob.open_temporary blobs reference.blob.id));
       assert (Result.is_error (Blob.open_session blobs session reference.blob.id));
       assert (P.Id.Blob.equal reference.blob.id (Intent.metadata intent).blob.id);
       print_endline
         "checksummed intent survived before an unpaired temporary data file; no adopted \
          result claimed");
  [%expect
    {| checksummed intent survived before an unpaired temporary data file; no adopted result claimed |}]
;;

let%expect_test
    "intent cleanup failure does not turn acknowledged publication into failure"
  =
  let armed = ref false in
  let before_unlink path =
    match !armed && String.is_suffix path ~suffix:".frame" with
    | false -> ()
    | true ->
      armed := false;
      failwith "injected intent unlink failure"
  in
  with_store
    ~wrap_env:(fun env -> fault_env ~before_unlink env (ref None))
    (fun env sw blobs _ session _ ->
       let completion = P.Completion.Succeeded (`String "committed") in
       let prepared = prepare env sw blobs session completion |> store_ok in
       let reference = Store.reference prepared in
       Store.commit prepared ~persist:(fun reference ->
         let path =
           Filename.concat
             (Agent_store.Session_store.Handle.directory session)
             "saved-reference.json"
         in
         let open Result.Let_syntax in
         let%map () =
           Agent_store.Durable_file.replace
             ~env
             ~durability:Flush_file_and_directory
             ~path
             (P.Job_artifact.to_json reference |> Jsonaf.to_string)
           |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
         in
         armed := true)
       |> protocol_ok;
       assert (not !armed);
       assert (Result.is_error (Store.discard prepared));
       let intent = Intent.list ~env ~session ~max_count:8 |> store_ok |> List.hd_exn in
       assert (
         P.Completion.equal
           completion
           (Store.load blobs ~sw ~session ~max_bytes:4096 reference |> store_ok));
       Intent.remove ~env ~session intent |> store_ok;
       assert (
         P.Completion.equal
           completion
           (Store.load blobs ~sw ~session ~max_bytes:4096 reference |> store_ok));
       print_endline
         "publication succeeded once; failed marker cleanup retained data and could \
          retry independently");
  [%expect
    {| publication succeeded once; failed marker cleanup retained data and could retry independently |}]
;;
