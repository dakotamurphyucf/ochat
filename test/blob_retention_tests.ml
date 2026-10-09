open Core
open Agent_store_test_fixtures
open Job_store_fixtures
open Blob_retention_fixtures

let assert_references expected graph roots =
  assert (
    List.equal
      P.Id.Blob.equal
      (List.sort expected ~compare:P.Id.Blob.compare)
      (Retention.references graph ~roots |> store_ok))
;;

let%expect_test
    "exports root transitive artifact dependencies but unrooted cycles do not retain \
     themselves"
  =
  with_store (fun env sw blobs _ session _ ->
    let a = P.Id.Blob.create ()
    and b = P.Id.Blob.create ()
    and c = P.Id.Blob.create ()
    and d = P.Id.Blob.create () in
    let value id = `String (P.Id.Blob.to_string id) in
    let first = stage env sw blobs session a (value b) in
    stage env sw blobs session b (value c) |> ignore;
    stage env sw blobs session c (value b) |> ignore;
    stage env sw blobs session d (value d) |> ignore;
    let encoded = P.Id.Blob.to_string a in
    let contents =
      sprintf
        "{\"reference\":\"\\u%04x%s\"}"
        (Char.to_int encoded.[0])
        (String.drop_prefix encoded 1)
    in
    let exported =
      upload
        blobs
        sw
        session
        (P.Id.Blob.create ())
        ~media_type:"application/json"
        ~allowed_use:(P.Job_artifact.allowed_use (Intent.reference first))
        contents
    in
    let graph = scan env blobs session |> store_ok in
    assert_references [ a; b; c ] graph [];
    assert_references [ a; b; c; d ] graph [ d ];
    let exported = Blob.adopt blobs session exported |> store_ok in
    assert_references [ a; b; c ] (scan env blobs session |> store_ok) [];
    Blob.discard_unreferenced blobs session exported |> store_ok;
    let graph = scan env blobs session |> store_ok in
    assert_references [] graph [];
    assert_references [ a; b; c ] graph [ a ];
    assert_references [] graph [ P.Id.Blob.create () ];
    print_endline
      "escaped temporary and adopted export retained the full dependency chain";
    print_endline "caller-supplied job label remained an ordinary root";
    print_endline
      "unrooted mutual/self cycles disappeared; external root restored the chain");
  [%expect
    {|
    escaped temporary and adopted export retained the full dependency chain
    caller-supplied job label remained an ordinary root
    unrooted mutual/self cycles disappeared; external root restored the chain
    |}]
;;

let%expect_test
    "only complete validated staged results contribute readable dependency edges"
  =
  with_store (fun env sw blobs _ session _ ->
    let parent = P.Id.Blob.create ()
    and child = P.Id.Blob.create () in
    stage env sw blobs session child `Null |> ignore;
    stage env sw blobs session parent (`String (P.Id.Blob.to_string child)) |> ignore;
    let final suffix =
      Eio.Path.(
        Eio.Stdenv.fs env
        / Handle.directory session
        / "blobs"
        / (P.Id.Blob.to_string parent ^ suffix))
    in
    let temporary =
      Blob.with_retention blobs ~f:(fun scope -> Blob.retention_directories scope session)
      |> store_ok
      |> Option.value_exn
      |> snd
    in
    let partial =
      Eio.Path.(Eio.Stdenv.fs env / temporary / (P.Id.Blob.to_string parent ^ ".part"))
    in
    let contents = Eio.Path.load (final ".blob") in
    Eio.Path.unlink (final ".sexp");
    Eio.Path.rename (final ".blob") partial;
    assert_references [ parent; child ] (scan env blobs session |> store_ok) [ parent ];
    Eio.Path.save ~create:(`Or_truncate 0o600) partial (String.prefix contents 8);
    let graph = scan env blobs session |> store_ok in
    assert_references [] graph [];
    assert (Result.is_error (Retention.references graph ~roots:[ parent ]));
    Eio.Path.save
      ~create:(`Or_truncate 0o600)
      partial
      (String.make (String.length contents) 'x');
    assert (Result.is_error (scan env blobs session));
    Eio.Path.unlink partial;
    let graph = scan env blobs session |> store_ok in
    assert_references [] graph [];
    assert (Result.is_error (Retention.references graph ~roots:[ parent ]));
    Eio.Path.save ~create:(`Exclusive 0o600) partial contents;
    List.iter [ ".blob"; ".sexp" ] ~f:(fun suffix ->
      Eio.Path.unlink
        Eio.Path.(
          Eio.Stdenv.fs env
          / Handle.directory session
          / "blobs"
          / (P.Id.Blob.to_string child ^ suffix)));
    let graph = scan env blobs session |> store_ok in
    assert_references [] graph [];
    assert (Result.is_error (Retention.references graph ~roots:[ parent ]));
    print_endline
      "complete partial retained its child; unrooted incomplete/missing stages stayed \
       unreferenced";
    print_endline
      "direct and transitive roots with missing completion content refused proof";
    print_endline "full-length corrupt stage refused proof");
  [%expect
    {|
    complete partial retained its child; unrooted incomplete/missing stages stayed unreferenced
    direct and transitive roots with missing completion content refused proof
    full-length corrupt stage refused proof
    |}]
;;

let%expect_test
    "blob roots refuse identity, digest, JSON, missing-file and shared-budget uncertainty"
  =
  with_store (fun env sw blobs _ session _ ->
    let candidate = P.Id.Blob.create () in
    stage env sw blobs session candidate `Null |> ignore;
    let exported =
      upload
        blobs
        sw
        session
        (P.Id.Blob.create ())
        ~media_type:"application/json"
        ~allowed_use:"export"
        (Jsonaf.to_string (`String (P.Id.Blob.to_string candidate)))
    in
    let baseline () =
      assert_references [ candidate ] (scan env blobs session |> store_ok) []
    in
    baseline ();
    let directory = Eio.Path.(Eio.Stdenv.fs env / Handle.directory session / "blobs") in
    let session_bytes =
      let intent_directory =
        Eio.Path.(Eio.Stdenv.fs env / Handle.directory session / "result-preparations")
      in
      List.sum
        (module Int)
        [ directory; intent_directory ]
        ~f:(fun directory ->
          Eio.Path.read_dir directory
          |> List.sum
               (module Int)
               ~f:(fun name -> String.length (Eio.Path.load Eio.Path.(directory / name))))
    in
    assert (Result.is_error (scan ~max_bytes:session_bytes env blobs session));
    assert (Result.is_error (scan ~max_entries:1 env blobs session));
    let data = Eio.Path.(directory / (P.Id.Blob.to_string candidate ^ ".blob")) in
    let meta = Eio.Path.(directory / (P.Id.Blob.to_string candidate ^ ".sexp")) in
    let original = Eio.Path.load data
    and metadata = Eio.Path.load meta in
    Eio.Path.save
      ~create:(`Or_truncate 0o600)
      data
      (String.make (String.length original) 'x');
    assert (Result.is_error (scan env blobs session));
    Eio.Path.save ~create:(`Or_truncate 0o600) data original;
    let document =
      Agent_store.Blob_metadata_document.of_bytes metadata
      |> Result.map_error ~f:(fun error -> Agent_store.Store_error.Document error)
      |> store_ok
    in
    let value = Agent_store.Blob_metadata_document.value document in
    let wrong =
      Agent_store.Blob_metadata_document.with_value
        document
        { value with allowed_use = "different-owner" }
      |> Result.map_error ~f:(fun error -> Agent_store.Store_error.Document error)
      |> store_ok
    in
    Eio.Path.save
      ~create:(`Or_truncate 0o600)
      meta
      (Agent_store.Blob_metadata_document.to_bytes wrong
       |> Result.map_error ~f:(fun error -> Agent_store.Store_error.Document error)
       |> store_ok);
    assert (Result.is_error (scan env blobs session));
    Eio.Path.unlink meta;
    Eio.Path.symlink ~link_to:(Eio.Path.native_exn data) meta;
    assert (Result.is_error (scan env blobs session));
    Eio.Path.unlink meta;
    Eio.Path.save ~create:(`Exclusive 0o600) meta metadata;
    baseline ();
    let malformed =
      upload
        blobs
        sw
        session
        (P.Id.Blob.create ())
        ~media_type:"application/json"
        ~allowed_use:"export"
        "{incomplete"
      |> Blob.adopt blobs session
      |> store_ok
    in
    assert (Result.is_error (scan env blobs session));
    Blob.discard_unreferenced blobs session malformed |> store_ok;
    let exported = Blob.adopt blobs session exported |> store_ok in
    let export_data =
      Eio.Path.(
        directory / (P.Id.Blob.to_string (Blob.Handle.metadata exported).blob.id ^ ".blob"))
    in
    let export_bytes = Eio.Path.load export_data in
    Eio.Path.unlink export_data;
    assert (Result.is_error (scan env blobs session));
    Eio.Path.save ~create:(`Exclusive 0o600) export_data export_bytes;
    baseline ();
    print_endline "one aggregate budget covered session and temporary consumers";
    print_endline
      "digest, ownership, symlink, malformed JSON and missing consumer all refused";
    print_endline
      "restored roots returned the original reference set without scanner writes");
  [%expect
    {|
    one aggregate budget covered session and temporary consumers
    digest, ownership, symlink, malformed JSON and missing consumer all refused
    restored roots returned the original reference set without scanner writes
    |}]
;;

let%expect_test
    "unknown escaped metadata references survive adoption and retain private stages"
  =
  with_store (fun env sw blobs _ session _ ->
    let candidate = P.Id.Blob.create () in
    stage env sw blobs session candidate `Null |> ignore;
    let export_id = P.Id.Blob.create () in
    upload
      blobs
      sw
      session
      export_id
      ~media_type:"application/octet-stream"
      ~allowed_use:"export"
      "payload"
    |> ignore;
    let temporary_root =
      Blob.with_retention blobs ~f:(fun scope ->
        Blob.retention_directories scope session |> Result.map ~f:snd)
      |> store_ok
      |> Option.value_exn
    in
    let path =
      Eio.Path.(
        Eio.Stdenv.fs env / temporary_root / (P.Id.Blob.to_string export_id ^ ".sexp"))
    in
    let original = Eio.Path.load path in
    let named =
      Document_schema.Document.decode
        ~limits:Agent_store.Blob_metadata_document.limits
        original
      |> Result.map_error ~f:(fun error -> Agent_store.Store_error.Document error)
      |> store_ok
    in
    let json =
      match Document_schema.Document.json named with
      | `Object fields ->
        `Object
          (List.Assoc.add
             fields
             ~equal:String.equal
             "future_owner"
             (`Object [ "opaque", `String (P.Id.Blob.to_string candidate) ]))
      | _ -> assert false
    in
    let escaped =
      Jsonaf.to_string json
      |> fun bytes ->
      String.substr_replace_all
        bytes
        ~pattern:(P.Id.Blob.to_string candidate)
        ~with_:("\\u0062" ^ String.drop_prefix (P.Id.Blob.to_string candidate) 1)
    in
    Eio.Path.save ~create:(`Or_truncate 0o600) path escaped;
    assert_references [ candidate ] (scan env blobs session |> store_ok) [];
    let reopened = Blob.open_temporary blobs export_id |> store_ok in
    Blob.adopt blobs session reopened |> store_ok |> ignore;
    assert_references [ candidate ] (scan env blobs session |> store_ok) [];
    print_endline "escaped unknown metadata roots candidate before and after adoption");
  [%expect {|escaped unknown metadata roots candidate before and after adoption|}]
;;

let%expect_test "adoption faults preserve checked ownership and original blob integrity" =
  List.iter [ "data"; "metadata"; "data-cancellation"; "cancellation" ] ~f:(fun phase ->
    let armed = ref None in
    let cancel = ref false in
    with_store
      ~wrap_env:(fun env ->
        fault_env
          ~matches_rename:(fun path ->
            String.is_substring path ~substring:"/blobs/"
            && String.is_suffix
                 path
                 ~suffix:(if String.equal phase "data" then ".blob" else ".sexp"))
          ~before_open_out:(fun path ->
            if
              !cancel
              && String.equal phase "data-cancellation"
              && String.is_substring path ~substring:"/blobs/"
            then (
              cancel := false;
              raise Eio.Time.Timeout))
          ~before_unlink:(fun path ->
            if
              !cancel
              && String.is_substring path ~substring:"/temporary/"
              && String.is_suffix path ~suffix:".sexp"
            then (
              cancel := false;
              raise Eio.Time.Timeout))
          env
          armed)
      (fun _env sw blobs reopen session _ ->
         let contents = "original bytes with whitespace\n" in
         let id = P.Id.Blob.create () in
         let handle =
           upload
             blobs
             sw
             session
             id
             ~media_type:"text/plain"
             ~allowed_use:"adoption-fault"
             contents
         in
         if String.is_suffix phase ~suffix:"cancellation"
         then cancel := true
         else armed := Some true;
         let cancelled =
           try
             assert (Result.is_error (Blob.adopt blobs session handle));
             false
           with
           | Eio.Time.Timeout -> true
         in
         assert (Bool.equal cancelled (String.is_suffix phase ~suffix:"cancellation"));
         (match Blob.Handle.metadata_checked handle with
          | Error _ ->
            assert (List.mem [ "data"; "data-cancellation" ] phase ~equal:String.equal);
            assert (Result.is_error (Blob.load blobs handle));
            assert (Result.is_error (Blob.adopt blobs session handle));
            assert (Result.is_error (Blob.discard_unreferenced blobs session handle))
          | Ok metadata ->
            assert (Bool.equal metadata.durable (String.equal phase "cancellation"));
            assert (
              String.equal
                (Blob.load_verified blobs ~sw handle ~max_bytes:1024 |> store_ok)
                contents);
            let reopened = reopen () in
            let current =
              (if metadata.durable
               then Blob.open_session reopened session id
               else Blob.open_temporary reopened id)
              |> store_ok
            in
            assert (
              String.equal
                (Blob.load_verified reopened ~sw current ~max_bytes:1024 |> store_ok)
                contents));
         assert (Option.is_some (Blob.with_retention blobs ~f:(fun _ -> Ok ()) |> store_ok));
         print_endline (phase ^ ": checked ownership and unpoisoned owner")));
  [%expect
    {|
    data: checked ownership and unpoisoned owner
    metadata: checked ownership and unpoisoned owner
    data-cancellation: checked ownership and unpoisoned owner
    cancellation: checked ownership and unpoisoned owner
    |}]
;;

let%expect_test "adoption retains the primary error when protected refresh times out" =
  let armed = ref None in
  let refresh_timeout = ref false in
  with_store
    ~wrap_env:(fun env ->
      fault_env
        ~matches_rename:(fun path ->
          String.is_substring path ~substring:"/blobs/"
          && String.is_suffix path ~suffix:".sexp")
        ~on_failure:(fun _ -> refresh_timeout := true)
        ~before_open_in:(fun path ->
          if !refresh_timeout && String.is_suffix path ~suffix:".sexp"
          then (
            refresh_timeout := false;
            raise Eio.Time.Timeout))
        env
        armed)
    (fun _env sw blobs reopen session _ ->
       let id = P.Id.Blob.create () in
       let handle =
         upload blobs sw session id ~media_type:"text/plain" ~allowed_use:"fault" "bytes"
       in
       armed := Some true;
       (match Blob.adopt blobs session handle with
        | Error (Agent_store.Store_error.Io _) -> ()
        | Ok _ | Error _ -> assert false);
       assert (Result.is_error (Blob.Handle.metadata_checked handle));
       assert (Result.is_error (Blob.load blobs handle));
       let reopened = reopen () in
       let current = Blob.open_temporary reopened id |> store_ok in
       assert (
         String.equal
           (Blob.load_verified reopened ~sw current ~max_bytes:32 |> store_ok)
           "bytes");
       assert (Option.is_some (Blob.with_retention blobs ~f:(fun _ -> Ok ()) |> store_ok));
       print_endline
         "primary IO retained, live authority unavailable, exact old bytes reopen");
  [%expect {|primary IO retained, live authority unavailable, exact old bytes reopen|}]
;;

let%expect_test "adoption acknowledges only after both directory owners are synced" =
  let armed = ref None in
  let published = ref false in
  let temporary_sync_failure = ref false in
  let parent_synced = ref false in
  with_store
    ~wrap_env:(fun env ->
      fault_env
        ~before_unlink:(fun path ->
          if !published && String.is_suffix path ~suffix:".sexp"
          then temporary_sync_failure := true)
        ~before_open_in:(fun path ->
          if String.is_suffix path ~suffix:"ses_job_artifact_first/."
          then parent_synced := true;
          if !temporary_sync_failure && String.is_suffix path ~suffix:"/temporary/."
          then (
            temporary_sync_failure := false;
            raise (Core_unix.Unix_error (EIO, "directory sync fault", path))))
        env
        armed)
    (fun _env sw blobs reopen session _ ->
       let id = P.Id.Blob.create () in
       let handle =
         upload blobs sw session id ~media_type:"text/plain" ~allowed_use:"fault" "bytes"
       in
       parent_synced := false;
       published := true;
       assert (Result.is_error (Blob.adopt blobs session handle));
       assert !parent_synced;
       assert (Blob.Handle.metadata_checked handle |> store_ok).durable;
       let reopened = reopen () in
       let current = Blob.open_session reopened session id |> store_ok in
       assert (
         String.equal
           (Blob.load_verified reopened ~sw current ~max_bytes:32 |> store_ok)
           "bytes");
       assert (Option.is_some (Blob.with_retention blobs ~f:(fun _ -> Ok ()) |> store_ok));
       print_endline
         "source sync failure reported; installed authority verified and reopens");
  [%expect {|source sync failure reported; installed authority verified and reopens|}]
;;

let%expect_test "adoption preserves a primary exception across refresh cancellation" =
  let exception Primary_adoption_failure in
  let armed = ref None in
  let fail_unlink = ref false in
  let fail_refresh = ref false in
  with_store
    ~wrap_env:(fun env ->
      fault_env
        ~before_unlink:(fun path ->
          if
            !fail_unlink
            && String.is_substring path ~substring:"/temporary/"
            && String.is_suffix path ~suffix:".sexp"
          then (
            fail_unlink := false;
            fail_refresh := true;
            raise Primary_adoption_failure))
        ~before_open_in:(fun path ->
          if !fail_refresh && String.is_suffix path ~suffix:".sexp"
          then (
            fail_refresh := false;
            raise Eio.Time.Timeout))
        env
        armed)
    (fun _env sw blobs reopen session _ ->
       let id = P.Id.Blob.create () in
       let handle =
         upload blobs sw session id ~media_type:"text/plain" ~allowed_use:"fault" "bytes"
       in
       fail_unlink := true;
       (try
          ignore (Blob.adopt blobs session handle);
          assert false
        with
        | Primary_adoption_failure -> ());
       assert (Result.is_error (Blob.Handle.metadata_checked handle));
       let reopened = reopen () in
       let current = Blob.open_session reopened session id |> store_ok in
       assert (
         String.equal
           (Blob.load_verified reopened ~sw current ~max_bytes:32 |> store_ok)
           "bytes");
       assert (Option.is_some (Blob.with_retention blobs ~f:(fun _ -> Ok ()) |> store_ok));
       print_endline
         "original exception propagated, live authority unavailable, installed bytes \
          reopen");
  [%expect
    {|original exception propagated, live authority unavailable, installed bytes reopen|}]
;;

let%expect_test "verified content structural limits precede unknown reference traversal" =
  with_store (fun env sw blobs _ session _ ->
    let child = P.Id.Blob.create () in
    stage env sw blobs session child `Null |> ignore;
    let child_name = P.Id.Blob.to_string child in
    let reference =
      sprintf
        "\"\\u%04x%s\""
        (Char.to_int child_name.[0])
        (String.drop_prefix child_name 1)
    in
    let nested depth = String.make depth '[' ^ reference ^ String.make depth ']' in
    let too_deep =
      upload
        blobs
        sw
        session
        (P.Id.Blob.create ())
        ~media_type:"application/json"
        ~allowed_use:"unknown-root"
        (nested 257)
    in
    assert (
      String.equal
        (Blob.load_verified blobs ~sw too_deep ~max_bytes:1024 |> store_ok)
        (nested 257));
    (match scan env blobs session with
     | Error (Agent_store.Store_error.Document (Document_schema.Error.Limit_exceeded _))
       -> ()
     | Ok _ | Error _ -> assert false);
    let child_handle = Blob.open_session blobs session child |> store_ok in
    Blob.load_verified blobs ~sw child_handle ~max_bytes:1024 |> store_ok |> ignore);
  with_store (fun env sw blobs _ session _ ->
    let child = P.Id.Blob.create () in
    stage env sw blobs session child `Null |> ignore;
    let child_name = P.Id.Blob.to_string child in
    let reference =
      sprintf
        "\"\\u%04x%s\""
        (Char.to_int child_name.[0])
        (String.drop_prefix child_name 1)
    in
    let nested depth = String.make depth '[' ^ reference ^ String.make depth ']' in
    upload
      blobs
      sw
      session
      (P.Id.Blob.create ())
      ~media_type:"application/json"
      ~allowed_use:"unknown-root"
      (nested 8)
    |> ignore;
    assert_references [ child ] (scan env blobs session |> store_ok) [];
    print_endline
      "valid raw digest, over-depth proof rejected without deletion; bounded unknown \
       roots retained");
  [%expect
    {|valid raw digest, over-depth proof rejected without deletion; bounded unknown roots retained|}]
;;

let%expect_test "expiry validates even nonexpired evidence before deleting candidates" =
  with_store (fun env sw blobs _ session _ ->
    let good = P.Id.Blob.of_string "blb_a_expired_candidate" |> protocol_ok in
    let expired =
      Blob.begin_upload
        blobs
        ~sw
        ~id:good
        ~creating_principal:principal
        ~target_session:(Some (Handle.session_id session))
        ~kind:File
        ~media_type:"text/plain"
        ~display_name:None
        ~allowed_use:"expiry"
        ~created_at:timestamp
        ~expires_at:(Some timestamp)
      |> store_ok
    in
    Blob.write_string expired "expired bytes" |> store_ok;
    Blob.finish expired ~expected_digest:None |> store_ok |> ignore;
    let nonexpired =
      upload
        blobs
        sw
        session
        (P.Id.Blob.create ())
        ~media_type:"text/plain"
        ~allowed_use:"not-expired"
        "retained bytes"
    in
    let temporary =
      Blob.with_retention blobs ~f:(fun scope ->
        Blob.retention_directories scope session |> Result.map ~f:snd)
      |> store_ok
      |> Option.value_exn
    in
    let good_path =
      Eio.Path.(
        Eio.Stdenv.fs env
        / temporary
        / (P.Id.Blob.to_string (Blob.Handle.metadata nonexpired).blob.id ^ ".blob"))
    in
    let metadata : Blob.Metadata.t = Blob.Handle.metadata nonexpired in
    let document =
      Agent_store.Blob_metadata_document.create metadata
      |> Result.map_error ~f:(fun error -> Agent_store.Store_error.Document error)
      |> store_ok
    in
    let stage =
      Agent_store.Blob_stage_documents.create document
      |> Result.map_error ~f:(fun error -> Agent_store.Store_error.Document error)
      |> store_ok
    in
    let alias = Eio.Path.(Eio.Stdenv.fs env / temporary / "blb_z_wrong_owner.sexp") in
    Eio.Path.save
      ~create:(`Exclusive 0o600)
      alias
      (Agent_store.Blob_stage_documents.temporary_bytes stage);
    assert (Result.is_error (Blob.cleanup_expired blobs ~now:timestamp));
    [%test_eq: string] "retained bytes" (Eio.Path.load good_path);
    let expired_path =
      Eio.Path.(Eio.Stdenv.fs env / temporary / (P.Id.Blob.to_string good ^ ".blob"))
    in
    [%test_eq: string] "expired bytes" (Eio.Path.load expired_path);
    Eio.Path.unlink alias;
    [%test_eq: int] 1 (Blob.cleanup_expired blobs ~now:timestamp |> store_ok);
    assert (not (Eio.Path.is_file expired_path));
    [%test_eq: string] "retained bytes" (Eio.Path.load good_path);
    print_endline
      "nonexpired wrong owner blocks all deletion; admitted expiry removes one");
  [%expect {|nonexpired wrong owner blocks all deletion; admitted expiry removes one|}]
;;
