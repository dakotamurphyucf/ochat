open Core
open Agent_store_test_fixtures
open Job_store_fixtures

let begin_blob blobs sw session =
  Blob.begin_upload
    blobs
    ~sw
    ~id:(P.Id.Blob.create ())
    ~creating_principal:principal
    ~target_session:(Some (Agent_store.Session_store.Handle.session_id session))
    ~kind:File
    ~media_type:"text/plain"
    ~display_name:None
    ~allowed_use:"fixture"
    ~created_at:timestamp
    ~expires_at:None
  |> store_ok
;;

let available blobs =
  Blob.with_retention blobs ~f:(fun _ -> Ok ()) |> store_ok |> Option.is_some
;;

let%expect_test
    "shared policies defer cleanup through upload lifetime and release on all exits"
  =
  with_store (fun _ _ blobs _ session _ ->
    let narrow = Blob.with_max_upload_bytes blobs ~max_upload_bytes:3L |> store_ok in
    Eio.Switch.run (fun sw ->
      let upload = begin_blob narrow sw session in
      assert (not (available blobs));
      assert (Result.is_error (Blob.write_string upload "too large"));
      assert (available blobs);
      let upload = begin_blob blobs sw session in
      Blob.write_string upload "larger result" |> store_ok;
      assert (not (available narrow));
      let handle = Blob.finish upload ~expected_digest:None |> store_ok in
      assert (available narrow);
      [%test_eq: string] "larger result" (Blob.load narrow handle |> store_ok);
      let upload = begin_blob blobs sw session in
      Blob.write_string upload "bad digest" |> store_ok;
      assert (Result.is_error (Blob.finish upload ~expected_digest:(Some "wrong")));
      assert (available narrow);
      let upload = begin_blob blobs sw session in
      Blob.abort upload;
      Blob.abort upload;
      assert (available narrow));
    let ended_switch =
      Eio.Switch.run (fun sw ->
        let upload = begin_blob narrow sw session in
        Blob.write_string upload "abc" |> store_ok;
        assert (not (available blobs));
        sw)
    in
    assert (available blobs);
    assert (
      Result.is_error
        (Blob.begin_upload
           blobs
           ~sw:ended_switch
           ~id:(P.Id.Blob.create ())
           ~creating_principal:principal
           ~target_session:None
           ~kind:File
           ~media_type:"text/plain"
           ~display_name:None
           ~allowed_use:"fixture"
           ~created_at:timestamp
           ~expires_at:None));
    assert (available narrow);
    print_endline "size policies stayed independent; both facades observed active uploads";
    print_endline
      "finish, limit/digest failure, repeated abort and switch exit released deferral";
    print_endline "closed-switch admission failed without poisoning storage");
  [%expect
    {|
    size policies stayed independent; both facades observed active uploads
    finish, limit/digest failure, repeated abort and switch exit released deferral
    closed-switch admission failed without poisoning storage
    |}]
;;

let%expect_test
    "retention excludes a new upload through another facade and revokes escaped scopes"
  =
  with_store (fun env sw blobs _ session _ ->
    let alias = Blob.with_max_upload_bytes blobs ~max_upload_bytes:256L |> store_ok in
    let upload = begin_blob blobs sw session in
    Blob.write_string upload "unpublished fixture" |> store_ok;
    let handle = Blob.finish upload ~expected_digest:None |> store_ok in
    let handle = Blob.adopt blobs session handle |> store_ok in
    let escaped = ref None in
    (match
       Blob.with_retention blobs ~f:(fun token ->
         escaped := Some token;
         raise Exit)
     with
     | exception Exit -> ()
     | _ -> failwith "expected callback exception");
    assert (available alias);
    assert (
      Result.is_error
        (Blob.discard_retained_unreferenced (Option.value_exn !escaped) session handle));
    [%test_eq: string] "unpublished fixture" (Blob.load alias handle |> store_ok);
    let entered, enter = Eio.Promise.create () in
    let attempted, attempt = Eio.Promise.create () in
    let completed, complete = Eio.Promise.create () in
    Eio.Fiber.fork ~sw (fun () ->
      Eio.Promise.await entered;
      Eio.Promise.resolve attempt ();
      let upload = begin_blob alias sw session in
      Blob.abort upload;
      Eio.Promise.resolve complete ());
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
      Blob.with_retention blobs ~f:(fun token ->
        Eio.Promise.resolve enter ();
        Eio.Promise.await attempted;
        Eio.Fiber.yield ();
        assert (Option.is_none (Eio.Promise.peek completed));
        Blob.discard_retained_unreferenced token session handle)
      |> store_ok
      |> Option.value_exn);
    Eio.Promise.await completed;
    assert (
      Result.is_error
        (Blob.open_session alias session (Blob.Handle.metadata handle).blob.id));
    assert (available blobs);
    print_endline "callback exception released coordination and revoked its token";
    print_endline "alias upload waited through scoped discard and then completed");
  [%expect
    {|
    callback exception released coordination and revoked its token
    alias upload waited through scoped discard and then completed
    |}]
;;

let%expect_test
    "ambiguous upload acknowledgement and cancelled readers leave coordination usable"
  =
  let armed = ref None in
  with_store
    ~wrap_env:(fun env -> fault_env env armed)
    (fun env sw blobs _ session _ ->
       let alias = Blob.with_max_upload_bytes blobs ~max_upload_bytes:256L |> store_ok in
       let upload = begin_blob blobs sw session in
       Blob.write_string upload "saved" |> store_ok;
       armed := Some true;
       assert (Result.is_error (Blob.finish upload ~expected_digest:None));
       assert (available alias);
       let upload = begin_blob alias sw session in
       Blob.write_string upload "read me" |> store_ok;
       let handle = Blob.finish upload ~expected_digest:None |> store_ok in
       let reading, read = Eio.Promise.create () in
       let cancelled =
         Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
           Eio.Fiber.first
             (fun () ->
                Blob.iter_chunks blobs ~sw handle ~chunk_size:2 ~f:(fun _ ->
                  Eio.Promise.resolve read ();
                  Eio.Fiber.await_cancel ()))
             (fun () ->
                Eio.Promise.await reading;
                assert (not (available alias));
                let concurrent_upload = begin_blob alias sw session in
                Blob.write_string concurrent_upload "concurrent" |> store_ok;
                Blob.finish concurrent_upload ~expected_digest:None |> store_ok |> ignore;
                assert (not (available alias));
                Error (Agent_store.Store_error.Corrupt "cancel reader")))
       in
       assert (Result.is_error cancelled);
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
         assert (available alias));
       [%test_eq: string] "read me" (Blob.load alias handle |> store_ok);
       print_endline
         "backpressured reader deferred retention while an independent upload completed";
       print_endline
         "lost acknowledgement released the upload without losing coordination";
       print_endline
         "blocked read remained cancellable; later reads and retention succeeded");
  [%expect
    {|
    backpressured reader deferred retention while an independent upload completed
    lost acknowledgement released the upload without losing coordination
    blocked read remained cancellable; later reads and retention succeeded
    |}]
;;
