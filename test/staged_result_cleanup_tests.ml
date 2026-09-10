open Core
open Agent_store_test_fixtures
open Job_store_fixtures
open Blob_retention_fixtures

let reader env session ?(max_entries = 1024) ?(max_bytes = 262144) () =
  Reader.create ~env ~root:(Handle.directory session) ~max_entries ~max_bytes |> store_ok
;;

(* This fixture owns all roots and has no executing worker. Production integration
   must also supply actor/history/cache/replay roots and exclude active owners. *)
let collect env blobs session ~roots =
  Blob.with_retention blobs ~f:(fun scope ->
    let open Result.Let_syntax in
    let reader = reader env session () in
    let%bind intents = Intent.list_with_reader ~reader ~session ~max_count:16 in
    let%bind graph =
      Retention.scan ~scope ~session ~reader ~intents ~max_file_bytes:16384
    in
    let%bind retained = Retention.references graph ~roots in
    List.fold_result intents ~init:0 ~f:(fun count intent ->
      match
        List.mem retained (Intent.reference intent).blob.id ~equal:P.Id.Blob.equal
      with
      | true -> Ok count
      | false ->
        let%map () = Intent.discard_unreferenced ~env ~scope ~reader ~session intent in
        count + 1))
  |> Result.map ~f:(fun value -> Option.value_exn value)
;;

let stage_path env session id suffix =
  Eio.Path.(
    Eio.Stdenv.fs env
    / Handle.directory session
    / "blobs"
    / (P.Id.Blob.to_string id ^ suffix))
;;

let intent_path env session id suffix =
  Eio.Path.(
    Eio.Stdenv.fs env
    / Handle.directory session
    / "result-preparations"
    / (P.Id.Blob.to_string id ^ suffix))
;;

let%expect_test
    "bounded intents and staged cleanup preserve roots and remove owned atomic residue"
  =
  let reads = ref 0 in
  with_store
    ~wrap_env:(fun env ->
      fault_env env (ref None) ~before_open_in:(fun path ->
        if String.is_suffix path ~suffix:".frame" then incr reads))
    (fun env sw blobs _ session _ ->
       let id = P.Id.Blob.create () in
       stage env sw blobs session id (`String "unpublished") |> ignore;
       let original = Eio.Path.load (intent_path env session id ".frame") in
       let temporary_names = List.init 32 ~f:(fun n -> sprintf ".frame.tmp-123-%d" n) in
       List.iter temporary_names ~f:(fun suffix ->
         Eio.Path.save
           ~create:(`Exclusive 0o600)
           (intent_path env session id suffix)
           (String.prefix original 16));
       reads := 0;
       assert (
         Result.is_error
           (Intent.list_with_reader
              ~reader:(reader env session ~max_entries:24 ())
              ~session
              ~max_count:16));
       [%test_eq: int] 0 !reads;
       let bounded =
         Intent.list_with_reader ~reader:(reader env session ()) ~session ~max_count:1
         |> store_ok
       in
       [%test_eq: int] 1 (List.length bounded);
       let metadata = Eio.Path.load (stage_path env session id ".sexp") in
       let metadata_temporary = stage_path env session id ".sexp.tmp-123-0" in
       Eio.Path.save
         ~create:(`Exclusive 0o600)
         metadata_temporary
         (String.prefix metadata 16);
       [%test_eq: int] 0 (collect env blobs session ~roots:[ id ] |> store_ok);
       assert (Eio.Path.is_file (stage_path env session id ".blob"));
       [%test_eq: int] 1 (collect env blobs session ~roots:[] |> store_ok);
       assert (not (Eio.Path.is_file (stage_path env session id ".blob")));
       assert (not (Eio.Path.is_file metadata_temporary));
       List.iter temporary_names ~f:(fun suffix ->
         assert (not (Eio.Path.is_file (intent_path env session id suffix))));
       assert (List.is_empty (Intent.list ~env ~session ~max_count:1 |> store_ok));
       [%test_eq: int] 0 (collect env blobs session ~roots:[] |> store_ok);
       print_endline
         "entry exhaustion stopped before frame reads; atomic temporaries were not \
          intents";
       print_endline
         "retained root prevented deletion; unreferenced data, metadata and intent \
          residue removed";
       print_endline "fresh enumeration made repeated collection a no-op");
  [%expect
    {|
    entry exhaustion stopped before frame reads; atomic temporaries were not intents
    retained root prevented deletion; unreferenced data, metadata and intent residue removed
    fresh enumeration made repeated collection a no-op
    |}]
;;

let%expect_test
    "staged cleanup retains ownership through data, metadata and sync failures"
  =
  List.iter [ "data"; "metadata"; "blob-sync"; "intent"; "intent-sync" ] ~f:(fun phase ->
    let armed = ref false in
    let fail selected =
      match !armed && selected with
      | false -> ()
      | true ->
        armed := false;
        failwith "injected staged cleanup failure"
    in
    with_store
      ~wrap_env:(fun env ->
        fault_env
          env
          (ref None)
          ~before_unlink:(fun path ->
            fail
              (match phase with
               | "data" -> String.is_suffix path ~suffix:".blob"
               | "metadata" -> String.is_suffix path ~suffix:".sexp"
               | "intent" -> String.is_suffix path ~suffix:".frame"
               | _ -> false))
          ~before_open_in:(fun path ->
            fail
              (match phase with
               | "blob-sync" -> String.is_suffix path ~suffix:"/blobs"
               | "intent-sync" -> String.is_suffix path ~suffix:"/result-preparations"
               | _ -> false)))
      (fun env sw blobs _ session _ ->
         let id = P.Id.Blob.create () in
         stage env sw blobs session id (`String "cleanup failure fixture") |> ignore;
         armed := true;
         assert (Result.is_error (collect env blobs session ~roots:[]));
         assert (not !armed);
         let remaining = Intent.list ~env ~session ~max_count:1 |> store_ok in
         match phase with
         | "intent-sync" ->
           assert (List.is_empty remaining);
           assert (not (Eio.Path.is_file (stage_path env session id ".blob")));
           [%test_eq: int] 0 (collect env blobs session ~roots:[] |> store_ok);
           print_endline
             "intent-sync: files already durable; fresh enumeration confirmed completion"
         | _ ->
           [%test_eq: int] 1 (List.length remaining);
           [%test_eq: int] 1 (collect env blobs session ~roots:[] |> store_ok);
           assert (List.is_empty (Intent.list ~env ~session ~max_count:1 |> store_ok));
           print_endline (phase ^ ": private ownership survived; retry completed cleanup")));
  [%expect
    {|
    data: private ownership survived; retry completed cleanup
    metadata: private ownership survived; retry completed cleanup
    blob-sync: private ownership survived; retry completed cleanup
    intent: private ownership survived; retry completed cleanup
    intent-sync: files already durable; fresh enumeration confirmed completion
    |}]
;;

let%expect_test
    "conflicting staged files and missing durable intent refuse deletion before mutation"
  =
  with_store (fun env sw blobs _ session _ ->
    let id = P.Id.Blob.create () in
    let intent = stage env sw blobs session id (`String "must survive") in
    let data = stage_path env session id ".blob" in
    let metadata = stage_path env session id ".sexp" in
    let original_data = Eio.Path.load data
    and original_metadata = Eio.Path.load metadata in
    let attempt () =
      Blob.with_retention blobs ~f:(fun scope ->
        Intent.discard_unreferenced
          ~env
          ~scope
          ~reader:(reader env session ())
          ~session
          intent)
    in
    let assert_intact () =
      [%test_eq: string] original_data (Eio.Path.load data);
      assert (Eio.Path.is_file (intent_path env session id ".frame"))
    in
    Eio.Path.save ~create:(`Or_truncate 0o600) metadata "conflicting";
    assert (Result.is_error (attempt ()));
    assert_intact ();
    Eio.Path.save ~create:(`Or_truncate 0o600) metadata original_metadata;
    let temporary_root =
      Blob.with_retention blobs ~f:(fun scope -> Blob.retention_directories scope session)
      |> store_ok
      |> Option.value_exn
      |> snd
    in
    let partial =
      Eio.Path.(Eio.Stdenv.fs env / temporary_root / (P.Id.Blob.to_string id ^ ".part"))
    in
    Eio.Path.save
      ~create:(`Exclusive 0o600)
      partial
      (String.make (String.length original_data) 'x');
    assert (Result.is_error (attempt ()));
    assert_intact ();
    Eio.Path.unlink partial;
    let temporary = intent_path env session id ".frame.tmp-123-9" in
    Eio.Path.save ~create:(`Exclusive 0o600) temporary "conflicting";
    assert (Result.is_error (attempt ()));
    assert_intact ();
    Eio.Path.unlink temporary;
    Eio.Path.symlink ~link_to:(Eio.Path.native_exn data) temporary;
    assert (Result.is_error (attempt ()));
    assert_intact ();
    Eio.Path.unlink temporary;
    let unknown =
      Eio.Path.(
        Eio.Stdenv.fs env / Handle.directory session / "result-preparations" / "unknown")
    in
    Eio.Path.save ~create:(`Exclusive 0o600) unknown "unrecognized";
    assert (Result.is_error (Intent.list ~env ~session ~max_count:1));
    Eio.Path.unlink unknown;
    Intent.remove ~env ~session intent |> store_ok;
    assert (Result.is_error (attempt ()));
    [%test_eq: string] original_data (Eio.Path.load data);
    Intent.save ~env ~session intent |> store_ok;
    [%test_eq: int] 1 (collect env blobs session ~roots:[] |> store_ok);
    print_endline
      "conflicting metadata, conflicting/linked intent temporary and unknown entries \
       refused";
    print_endline
      "an in-memory intent without its durable record could not authorize staged deletion");
  [%expect
    {|
    conflicting metadata, conflicting/linked intent temporary and unknown entries refused
    an in-memory intent without its durable record could not authorize staged deletion
    |}]
;;

let%expect_test
    "unpaired, partial and missing publication stages clean up without fabricated results"
  =
  List.iter [ "temporary-data"; "short-partial"; "missing" ] ~f:(fun layout ->
    with_store (fun env sw blobs _ session _ ->
      let id = P.Id.Blob.create () in
      stage env sw blobs session id (`String "already executed") |> ignore;
      let temporary_root =
        Blob.with_retention blobs ~f:(fun scope ->
          Blob.retention_directories scope session)
        |> store_ok
        |> Option.value_exn
        |> snd
      in
      let staged suffix =
        Eio.Path.(Eio.Stdenv.fs env / temporary_root / (P.Id.Blob.to_string id ^ suffix))
      in
      let data = stage_path env session id ".blob" in
      let original = Eio.Path.load data in
      Eio.Path.unlink (stage_path env session id ".sexp");
      (match layout with
       | "temporary-data" -> Eio.Path.rename data (staged ".blob")
       | "short-partial" ->
         Eio.Path.unlink data;
         Eio.Path.save
           ~create:(`Exclusive 0o600)
           (staged ".part")
           (String.prefix original 8)
       | "missing" -> Eio.Path.unlink data
       | _ -> assert false);
      [%test_eq: int] 1 (collect env blobs session ~roots:[] |> store_ok);
      assert (List.is_empty (Intent.list ~env ~session ~max_count:1 |> store_ok));
      assert (not (Eio.Path.is_file (staged ".blob")));
      assert (not (Eio.Path.is_file (staged ".part")));
      print_endline (layout ^ ": stage and private ownership removed")));
  [%expect
    {|
    temporary-data: stage and private ownership removed
    short-partial: stage and private ownership removed
    missing: stage and private ownership removed
    |}]
;;
