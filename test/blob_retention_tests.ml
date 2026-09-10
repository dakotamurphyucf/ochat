open Core
open Agent_store_test_fixtures
open Job_store_fixtures
module Intent = Agent_store.Job_result_intent
module Retention = Agent_store.Blob_retention
module Reader = Agent_store.Retention_reader
module Handle = Agent_store.Session_store.Handle

let upload blobs sw session id ~media_type ~allowed_use contents =
  let upload =
    Blob.begin_upload
      blobs
      ~sw
      ~id
      ~creating_principal:principal
      ~target_session:(Some (Handle.session_id session))
      ~kind:File
      ~media_type
      ~display_name:None
      ~allowed_use
      ~created_at:timestamp
      ~expires_at:None
    |> store_ok
  in
  Blob.write_string upload contents |> store_ok;
  Blob.finish upload ~expected_digest:None |> store_ok
;;

let stage env sw blobs session id value =
  let contents = P.Completion.to_json (Succeeded value) |> Jsonaf.to_string in
  let job = job session in
  let blob =
    P.Blob.Metadata.create
      ~id
      ~kind:File
      ~media_type:P.Job_artifact.media_type
      ~byte_length:(Int64.of_int (String.length contents))
      ~digest:Digestif.SHA256.(digest_string contents |> to_hex)
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
  upload
    blobs
    sw
    session
    id
    ~media_type:blob.media_type
    ~allowed_use:metadata.allowed_use
    contents
  |> Blob.adopt blobs session
  |> store_ok
  |> ignore;
  intent
;;

let scan ?(max_bytes = 65536) ?(max_entries = 256) env blobs session =
  Blob.with_retention blobs ~f:(fun scope ->
    let open Result.Let_syntax in
    let%bind intents = Intent.list ~env ~session ~max_count:16 in
    let%bind reader =
      Reader.create ~env ~root:(Handle.directory session) ~max_entries ~max_bytes
    in
    Retention.scan ~scope ~session ~reader ~intents ~max_file_bytes:16384)
  |> Result.map ~f:(fun result -> Option.value_exn result)
;;

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
      Eio.Path.read_dir directory
      |> List.sum
           (module Int)
           ~f:(fun name -> String.length (Eio.Path.load Eio.Path.(directory / name)))
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
    let value = Sexp.of_string metadata |> Blob.Metadata.t_of_sexp in
    let wrong = { value with allowed_use = "different-owner" } in
    Eio.Path.save
      ~create:(`Or_truncate 0o600)
      meta
      (Blob.Metadata.sexp_of_t wrong |> Sexp.to_string_mach);
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
