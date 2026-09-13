open Core
open Agent_store_test_fixtures
open Job_store_fixtures
open Blob_retention_fixtures
module Results = Agent_store.Job_result_store
module Publisher = Results.Publisher

let limits : Publisher.collection_limits =
  { max_intents = 16; max_entries = 1024; max_bytes = 262144; max_file_bytes = 16384 }
;;

let publisher env sw blobs session =
  Publisher.create ~env ~blobs ~sw ~session ~principal ~inline_bytes:0 ~max_bytes:4096
  |> protocol_ok
;;

let collect ?(jobs = []) ?(generation = 0) ?(limits = limits) ?(roots = []) publisher =
  Publisher.collect
    publisher
    ~jobs
    ~generation
    ~limits
    ~with_roots:(fun ~reader:_ ~candidates:_ ~f -> f roots)
;;

let show result = print_s [%sexp (result |> store_ok : Publisher.collection_stats option)]

let%expect_test
    "collection protects unpublished in-memory dependencies until attempt retirement"
  =
  let armed = ref None in
  with_store
    ~wrap_env:(fun env ->
      fault_env env armed ~matches_rename:(fun path ->
        String.is_suffix path ~suffix:".frame"))
    (fun env sw blobs _ session _ ->
       let retained_id = P.Id.Blob.create () in
       stage env sw blobs session retained_id (`String "dependency") |> ignore;
       let publisher = publisher env sw blobs session in
       let running = job session in
       let completion =
         P.Completion.Succeeded (`String (P.Id.Blob.to_string retained_id))
       in
       armed := Some false;
       assert (
         Result.is_error
           (Publisher.publish
              publisher
              ~jobs:[ running ]
              ~job:running
              ~now:timestamp
              completion
              ~persist:(fun _ -> failwith "intent write failed first")));
       [%test_eq: int]
         1
         (List.length (Intent.list ~env ~session ~max_count:16 |> store_ok));
       show (collect publisher ~jobs:[ running ]);
       assert (
         Result.is_error
           (collect publisher ~jobs:[ running ] ~limits:{ limits with max_bytes = 1 }));
       assert (Result.is_ok (Blob.open_session blobs session retained_id));
       assert (
         Result.is_error
           (Result.try_with (fun () ->
              Publisher.collect
                publisher
                ~jobs:[ running ]
                ~generation:0
                ~limits
                ~with_roots:(fun ~reader:_ ~candidates:_ ~f:_ ->
                  failwith "injected host callback exception"))));
       assert (
         Result.is_error
           (Publisher.collect
              publisher
              ~jobs:[]
              ~generation:1
              ~limits
              ~with_roots:(fun ~reader:_ ~candidates:_ ~f:_ ->
                Error (Agent_store.Store_error.Corrupt "unreadable history"))));
       assert (Result.is_ok (Blob.open_session blobs session retained_id));
       show (collect publisher ~generation:1);
       assert (Result.is_error (Blob.open_session blobs session retained_id));
       print_endline
         "pending completion rooted its dependency before its own intent existed";
       print_endline
         "budget/root errors retained files; retired attempt released the dependency");
  [%expect
    {|
    (((discarded 0) (retired 0) (retained 1)))
    (((discarded 1) (retired 0) (retained 0)))
    pending completion rooted its dependency before its own intent existed
    budget/root errors retained files; retired attempt released the dependency
    |}]
;;

let%expect_test
    "collection reconciles lost publication acknowledgement and preserves final data"
  =
  with_store (fun env sw blobs _ session _ ->
    let publisher = publisher env sw blobs session in
    let running = job session in
    let completion = P.Completion.Succeeded (`String "durably completed") in
    let saved = ref None in
    assert (
      Result.is_error
        (Publisher.publish
           publisher
           ~jobs:[ running ]
           ~job:running
           ~now:timestamp
           completion
           ~persist:(fun stored ->
             saved := Some stored;
             Error (P.Error.invalid_request "lost acknowledgement"))));
    let stored = Option.value_exn !saved in
    let completed =
      { running with
        status = Succeeded
      ; completed_at = Some timestamp
      ; result = Some (P.Stored_completion.to_json stored)
      }
    in
    let reference =
      match stored with
      | Artifact { reference; _ } -> reference
      | Inline _ -> failwith "expected artifact"
    in
    let metadata_path =
      Eio.Path.(
        Eio.Stdenv.fs env
        / Handle.directory session
        / "blobs"
        / (P.Id.Blob.to_string reference.blob.id ^ ".sexp"))
    in
    let metadata = Eio.Path.load metadata_path in
    Eio.Path.unlink metadata_path;
    show (collect publisher ~jobs:[ completed ]);
    [%test_eq: int] 1 (List.length (Intent.list ~env ~session ~max_count:16 |> store_ok));
    Eio.Path.save ~create:(`Exclusive 0o600) metadata_path metadata;
    show (collect publisher ~jobs:[ completed ]);
    assert (List.is_empty (Intent.list ~env ~session ~max_count:16 |> store_ok));
    assert (
      P.Completion.equal completion (Publisher.load publisher reference |> protocol_ok));
    show (collect publisher ~jobs:[ completed ]);
    print_endline
      "exact terminal publication retired the marker; verified result remains readable");
  [%expect
    {|
    (((discarded 0) (retired 0) (retained 1)))
    (((discarded 0) (retired 1) (retained 0)))
    (((discarded 0) (retired 0) (retained 0)))
    exact terminal publication retired the marker; verified result remains readable
    |}]
;;

let%expect_test
    "collection defers uploads, preserves rooted cycles and deletes only proven orphans"
  =
  with_store (fun env sw blobs _ session _ ->
    let first = P.Id.Blob.create ()
    and second = P.Id.Blob.create () in
    stage env sw blobs session first (`String (P.Id.Blob.to_string second)) |> ignore;
    stage env sw blobs session second (`String (P.Id.Blob.to_string first)) |> ignore;
    let publisher = publisher env sw blobs session in
    let upload =
      Blob.begin_upload
        blobs
        ~sw
        ~id:(P.Id.Blob.create ())
        ~creating_principal:principal
        ~target_session:None
        ~kind:File
        ~media_type:"text/plain"
        ~display_name:None
        ~allowed_use:"upload"
        ~created_at:timestamp
        ~expires_at:None
      |> store_ok
    in
    show (collect publisher);
    Blob.abort upload;
    show (collect publisher ~roots:[ first ]);
    let reserved =
      Blob.with_retention blobs ~f:Blob.retention_reserved_directory
      |> store_ok
      |> Option.value_exn
    in
    let unknown = Eio.Path.(Eio.Stdenv.fs env / reserved / "unrecognized-consumer") in
    Eio.Path.save ~create:(`Exclusive 0o600) unknown "opaque data";
    assert (Result.is_error (collect publisher));
    [%test_eq: int] 2 (List.length (Intent.list ~env ~session ~max_count:16 |> store_ok));
    Eio.Path.unlink unknown;
    show (collect publisher);
    assert (List.is_empty (Intent.list ~env ~session ~max_count:16 |> store_ok));
    print_endline
      "live upload deferred; external root preserved the cycle; unrooted cycle collected");
  [%expect
    {|
    ()
    (((discarded 0) (retired 0) (retained 2)))
    (((discarded 2) (retired 0) (retained 0)))
    live upload deferred; external root preserved the cycle; unrooted cycle collected
    |}]
;;
