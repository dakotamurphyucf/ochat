open! Core
module Storage = Private_storage

let ok = function
  | Ok value -> value
  | Error error ->
    raise_s [%sexp "unexpected synthetic storage error", (error : Storage.Error.t)]
;;

let name value = Storage.Name.create value |> ok

let with_directory f =
  Eio_main.run (fun env ->
    let path =
      Eio_unix.run_in_systhread (fun () ->
        Core_unix.mkdtemp "/tmp/ochat-secret-synthetic-XXXXXX")
    in
    let anchor = Eio.Path.(Eio.Stdenv.fs env / path) in
    Fun.protect
      ~finally:(fun () -> Eio.Path.rmtree anchor)
      (fun () ->
         Eio.Switch.run (fun sw ->
           let directory =
             Storage.Directory.open_or_create ~sw ~anchor ~components:[ name "private" ]
             |> ok
           in
           f env sw anchor directory)))
;;

let print_result = function
  | Ok _ -> print_endline "ok"
  | Error error -> print_s [%sexp (error : Storage.Error.t)]
;;

let%expect_test "immutable publication and durability uncertainty" =
  with_directory (fun _ _ _ directory ->
    let bytes = Bytes.of_string "synthetic" in
    print_result
      (Storage.Directory.For_testing.create_with_fault
         directory
         (name "before")
         bytes
         ~fault:Before_publication);
    print_result (Storage.Directory.read_bounded directory (name "before") ~max_bytes:32);
    print_result
      (Storage.Directory.For_testing.create_with_fault
         directory
         (name "after")
         bytes
         ~fault:Before_directory_sync);
    let stored =
      Storage.Directory.read_bounded directory (name "after") ~max_bytes:32 |> ok
    in
    print_s [%sexp (Bytes.equal bytes stored : bool)];
    print_result
      (Storage.Directory.create_immutable
         directory
         (name "after")
         (Bytes.of_string "different"));
    print_s
      [%sexp
        (Bytes.equal
           bytes
           (Storage.Directory.read_bounded directory (name "after") ~max_bytes:32 |> ok)
         : bool)];
    print_result (Storage.Directory.read_bounded directory (name "after") ~max_bytes:2));
  [%expect
    {|
    ((operation Create) (code Unavailable) (publication (Not_published)))
    ((operation Read) (code Missing) (publication ()))
    ((operation Create) (code Unavailable)
     (publication (Published_durability_unknown)))
    true
    ((operation Create) (code Exists) (publication (Not_published)))
    true
    ((operation Read) (code Too_large) (publication ()))
    |}]
;;

let%expect_test "cancelled owner removes only its newly created revision" =
  with_directory (fun _ _ _ directory ->
    let revision = name "cancelled" in
    (try
       Eio.Switch.run (fun sw ->
         ignore
           (Storage.Directory.For_testing.create_with_completion_hook
              directory
              revision
              (Bytes.of_string "synthetic")
              ~after_native:(fun () -> Eio.Switch.fail sw Exit)
            : (unit, Storage.Error.t) result))
     with
     | Exit | Eio.Cancel.Cancelled Exit -> print_endline "owner cancelled");
    print_result (Storage.Directory.read_bounded directory revision ~max_bytes:32);
    Storage.Directory.create_immutable
      directory
      (name "existing")
      (Bytes.of_string "retained")
    |> ok;
    (try
       Eio.Switch.run (fun sw ->
         ignore
           (Storage.Directory.For_testing.create_with_completion_hook
              directory
              (name "existing")
              (Bytes.of_string "synthetic")
              ~after_native:(fun () -> Eio.Switch.fail sw Exit)
            : (unit, Storage.Error.t) result))
     with
     | Exit | Eio.Cancel.Cancelled Exit -> print_endline "owner cancelled");
    print_result
      (Storage.Directory.read_bounded directory (name "existing") ~max_bytes:32));
  [%expect
    {|
    owner cancelled
    ((operation Read) (code Missing) (publication ()))
    owner cancelled
    ok
    |}]
;;

let%expect_test "descriptor admission rejects unsafe entries and changed permissions" =
  with_directory (fun _ _ anchor directory ->
    let native = Eio.Path.native_exn Eio.Path.(anchor / "private") in
    Eio_unix.run_in_systhread (fun () ->
      Core_unix.mkfifo (Filename.concat native "fifo") ~perm:0o600);
    print_result (Storage.Directory.read_bounded directory (name "fifo") ~max_bytes:32);
    Storage.Directory.create_immutable
      directory
      (name "file")
      (Bytes.of_string "synthetic")
    |> ok;
    Eio_unix.run_in_systhread (fun () ->
      Core_unix.link
        ~target:(Filename.concat native "file")
        ~link_name:(Filename.concat native "alias")
        ());
    print_result (Storage.Directory.read_bounded directory (name "file") ~max_bytes:32);
    Eio_unix.run_in_systhread (fun () ->
      Core_unix.unlink (Filename.concat native "alias"));
    Eio_unix.run_in_systhread (fun () ->
      Core_unix.chmod (Filename.concat native "file") ~perm:0o644);
    print_result (Storage.Directory.delete directory (name "file"));
    Eio_unix.run_in_systhread (fun () ->
      Core_unix.symlink ~target:"file" ~link_name:(Filename.concat native "symlink"));
    print_result (Storage.Directory.read_bounded directory (name "symlink") ~max_bytes:32));
  [%expect
    {|
    ((operation Read) (code Corrupt) (publication ()))
    ((operation Read) (code Corrupt) (publication ()))
    ((operation Delete) (code Corrupt) (publication (Not_published)))
    ((operation Read) (code Corrupt) (publication ()))
    |}]
;;

let%expect_test "independent lock descriptors and process death release" =
  with_directory (fun env sw anchor directory ->
    let lock = name "coordination" in
    let first = Storage.Lock.acquire directory lock ~sw ~mode:Exclusive |> ok in
    print_result (Storage.Lock.acquire directory lock ~sw ~mode:Exclusive);
    let inode () =
      Eio_unix.run_in_systhread (fun () ->
        (Core_unix.stat
           (Filename.concat
              (Eio.Path.native_exn Eio.Path.(anchor / "private"))
              "coordination"))
          .st_ino)
    in
    let original_inode = inode () in
    Storage.Lock.release first;
    let output, sink = Eio.Process.pipe ~sw (Eio.Stdenv.process_mgr env) in
    let input, writer = Eio.Process.pipe ~sw (Eio.Stdenv.process_mgr env) in
    let child =
      Eio.Process.spawn
        ~sw
        (Eio.Stdenv.process_mgr env)
        ~stdin:input
        ~stdout:sink
        [ "./lock_probe.exe"; Eio.Path.native_exn anchor ]
    in
    Eio.Flow.close input;
    Eio.Flow.close sink;
    let ready =
      Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
        Eio.Buf_read.line (Eio.Buf_read.of_flow ~max_size:32 output))
    in
    assert (String.equal ready "ready");
    print_result (Storage.Lock.acquire directory lock ~sw ~mode:Shared);
    Eio.Process.signal child 9;
    ignore
      (Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
         Eio.Process.await child)
       : Eio.Process.exit_status);
    ignore writer;
    let lease = Storage.Lock.acquire directory lock ~sw ~mode:Exclusive |> ok in
    assert (Int.equal original_inode (inode ()));
    print_endline "acquired after process death";
    Storage.Lock.release lease);
  [%expect
    {|
    ((operation Open_lock) (code Busy) (publication (Not_published)))
    ((operation Open_lock) (code Busy) (publication (Not_published)))
    acquired after process death
    |}]
;;

let%expect_test "backend namespaces, borrowed lifetime, bounds and redaction" =
  with_directory (fun _ sw _ directory ->
    let open Provider_secret_store in
    let ok = function
      | Ok value -> value
      | Error error ->
        raise_s [%sexp "unexpected synthetic backend error", (error : Error.t)]
    in
    let left =
      open_private_files ~sw ~directory ~namespace:(Namespace.create "a-b" |> ok) |> ok
    in
    let right =
      open_private_files ~sw ~directory ~namespace:(Namespace.create "a" |> ok) |> ok
    in
    let secret =
      Secret.of_bytes (Bytes.of_string "synthetic-secret-never-render") |> ok
    in
    create left ~revision:(Revision.create "c" |> ok) secret |> ok;
    create right ~revision:(Revision.create "b-c" |> ok) secret |> ok;
    close left;
    let revision = Revision.create "b-c" |> ok in
    print_s [%sexp (Secret.length (read right ~revision |> ok) : int)];
    (match create right ~revision secret with
     | Ok () -> assert false
     | Error error -> print_s [%sexp (error : Error.t)]);
    (match Secret.of_bytes (Bytes.create (Secret.maximum_bytes + 1)) with
     | Ok _ -> assert false
     | Error error -> print_s [%sexp (Error.code error : Error.code)]);
    Storage.Directory.close directory;
    match read right ~revision with
    | Ok _ -> assert false
    | Error error -> print_s [%sexp (Error.code error : Error.code)]);
  [%expect
    {|
    29
    ((operation Create) (code Exists) (publication (Not_published)))
    Too_large
    Closed
    |}]
;;

let%expect_test
    "shared leases span processes and failed switch admission releases exclusive lock"
  =
  with_directory (fun env sw anchor directory ->
    let lock = name "coordination" in
    let output, sink = Eio.Process.pipe ~sw (Eio.Stdenv.process_mgr env) in
    let input, _writer = Eio.Process.pipe ~sw (Eio.Stdenv.process_mgr env) in
    let child =
      Eio.Process.spawn
        ~sw
        (Eio.Stdenv.process_mgr env)
        ~stdin:input
        ~stdout:sink
        [ "./lock_probe.exe"; Eio.Path.native_exn anchor; "shared" ]
    in
    Eio.Flow.close input;
    Eio.Flow.close sink;
    assert (
      String.equal
        (Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
           Eio.Buf_read.line (Eio.Buf_read.of_flow ~max_size:32 output)))
        "ready");
    let shared = Storage.Lock.acquire directory lock ~sw ~mode:Shared |> ok in
    print_endline "shared concurrently";
    print_result (Storage.Lock.acquire directory lock ~sw ~mode:Exclusive);
    Eio.Process.signal child 9;
    ignore
      (Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
         Eio.Process.await child)
       : Eio.Process.exit_status);
    print_result (Storage.Lock.acquire directory lock ~sw ~mode:Exclusive);
    Storage.Lock.release shared;
    let failed = ref None in
    (try
       Eio.Switch.run (fun sw ->
         failed := Some sw;
         Eio.Switch.fail sw Exit)
     with
     | Exit -> ());
    (try
       ignore
         (Storage.Lock.acquire
            directory
            lock
            ~sw:(Option.value_exn !failed)
            ~mode:Exclusive
          : (Storage.Lock.t, Storage.Error.t) result)
     with
     | Invalid_argument message when String.equal message "Switch finished!" ->
       print_endline "failed switch rejected");
    let exclusive = Storage.Lock.acquire directory lock ~sw ~mode:Exclusive |> ok in
    print_endline "failed adoption released kernel lock";
    Storage.Directory.close directory;
    print_result
      (Storage.Directory.create_immutable
         directory
         (name "closed")
         (Bytes.of_string "synthetic"));
    Storage.Lock.release exclusive);
  [%expect
    {|
    shared concurrently
    ((operation Open_lock) (code Busy) (publication (Not_published)))
    ((operation Open_lock) (code Busy) (publication (Not_published)))
    failed switch rejected
    failed adoption released kernel lock
    ((operation Create) (code Closed) (publication (Not_published)))
    |}]
;;

let%expect_test "trusted opened anchor survives pathname replacement" =
  with_directory (fun env sw anchor _ ->
    let moved = Eio.Path.(Eio.Stdenv.fs env / (Eio.Path.native_exn anchor ^ "-moved")) in
    Eio.Path.with_open_dir anchor (fun opened ->
      Eio.Path.rename anchor moved;
      Fun.protect
        ~finally:(fun () -> Eio.Path.rmtree moved)
        (fun () ->
           Eio.Path.mkdir ~perm:0o700 anchor;
           let directory =
             Storage.Directory.open_or_create
               ~sw
               ~anchor:opened
               ~components:[ name "identity" ]
             |> ok
           in
           Storage.Directory.create_immutable
             directory
             (name "proof")
             (Bytes.of_string "synthetic")
           |> ok;
           assert (Eio.Path.is_file Eio.Path.(moved / "identity/proof"));
           assert (not (Eio.Path.is_directory Eio.Path.(anchor / "identity")));
           print_endline "retained anchor identity")));
  [%expect {| retained anchor identity |}]
;;

let%expect_test "anchor errors are redacted and closed switch directory adoption rejects" =
  with_directory (fun _ sw anchor _ ->
    print_result
      (Storage.Directory.open_or_create
         ~sw
         ~anchor:Eio.Path.(anchor / "missing")
         ~components:[ name "private" ]);
    let closed = ref None in
    Eio.Switch.run (fun sw -> closed := Some sw);
    (try
       ignore
         (Storage.Directory.open_or_create
            ~sw:(Option.value_exn !closed)
            ~anchor
            ~components:[ name "closed-owner" ]
          : (Storage.Directory.t, Storage.Error.t) result)
     with
     | Invalid_argument message when String.equal message "Switch finished!" ->
       print_endline "closed switch rejected");
    let directory =
      Storage.Directory.open_or_create ~sw ~anchor ~components:[ name "closed-owner" ]
      |> ok
    in
    Storage.Directory.create_immutable
      directory
      (name "proof")
      (Bytes.of_string "synthetic")
    |> ok;
    print_endline "later owner usable");
  [%expect
    {|
    ((operation Open_directory) (code Missing) (publication (Not_published)))
    closed switch rejected
    later owner usable
    |}]
;;

let%expect_test "empty ACL admission and platform-specific Darwin extended ACL rejection" =
  Eio_main.run (fun env ->
    let platform =
      Eio_unix.run_in_systhread (fun () ->
        Core_unix.uname () |> Core_unix.Utsname.sysname)
    in
    let path =
      Eio_unix.run_in_systhread (fun () ->
        Core_unix.mkdtemp "/tmp/ochat-secret-acl-synthetic-XXXXXX")
    in
    let anchor = Eio.Path.(Eio.Stdenv.fs env / path) in
    Fun.protect
      ~finally:(fun () -> Eio.Path.rmtree anchor)
      (fun () ->
         Eio.Switch.run (fun sw ->
           let directory =
             Storage.Directory.open_or_create ~sw ~anchor ~components:[ name "private" ]
             |> ok
           in
           let item = name "synthetic" in
           Storage.Directory.create_immutable directory item (Bytes.of_string "synthetic")
           |> ok;
           ignore
             (Storage.Directory.read_bounded directory item ~max_bytes:32 |> ok : bytes);
           if String.equal platform "Darwin"
           then (
             let filename = Filename.concat path "private/synthetic" in
             let chmod arguments =
               Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
                 Eio.Process.run (Eio.Stdenv.process_mgr env) ("/bin/chmod" :: arguments))
             in
             let corrupt = function
               | Ok _ -> failwith "extended ACL unexpectedly admitted"
               | Error error ->
                 assert (Storage.Error.equal_code (Storage.Error.code error) Corrupt)
             in
             chmod [ "+a"; "everyone allow read"; filename ];
             corrupt (Storage.Directory.read_bounded directory item ~max_bytes:32);
             chmod [ "-N"; filename ];
             ignore
               (Storage.Directory.read_bounded directory item ~max_bytes:32 |> ok : bytes);
             let private_path = Filename.concat path "private" in
             chmod [ "+a"; "everyone allow list,search"; private_path ];
             corrupt
               (Storage.Directory.open_or_create
                  ~sw
                  ~anchor
                  ~components:[ name "private" ]);
             chmod [ "-N"; private_path ];
             ignore
               (Storage.Directory.open_or_create
                  ~sw
                  ~anchor
                  ~components:[ name "private" ]
                |> ok
                : Storage.Directory.t))));
    print_endline "empty ACL admission passed; extended ACL checks are Darwin-specific");
  [%expect {| empty ACL admission passed; extended ACL checks are Darwin-specific |}]
;;

let%expect_test
    "durable absence confirmation rejects existing entries and retains sync uncertainty"
  =
  with_directory (fun _ _ anchor directory ->
    Storage.Directory.confirm_absent directory (name "absent") |> ok;
    (match
       Storage.Directory.For_testing.confirm_absent_with_sync_failure
         directory
         (name "absent")
     with
     | Error error ->
       assert (Storage.Error.equal_code (Storage.Error.code error) Unavailable);
       assert (Option.is_none (Storage.Error.publication error))
     | Ok () -> failwith "sync failure became durable absence");
    Storage.Directory.create_immutable
      directory
      (name "present")
      (Bytes.of_string "synthetic-retained")
    |> ok;
    let reject_existing filename =
      match Storage.Directory.confirm_absent directory (name filename) with
      | Error error ->
        assert (Storage.Error.equal_code (Storage.Error.code error) Exists);
        assert (Option.is_none (Storage.Error.publication error))
      | Ok () -> failwith "existing entry reported absent"
    in
    reject_existing "present";
    Eio_unix.run_in_systhread (fun () ->
      Core_unix.symlink
        ~target:"absent"
        ~link_name:
          (Filename.concat (Eio.Path.native_exn Eio.Path.(anchor / "private")) "symlink"));
    reject_existing "symlink";
    assert (
      Bytes.equal
        (Storage.Directory.read_bounded directory (name "present") ~max_bytes:32 |> ok)
        (Bytes.of_string "synthetic-retained"));
    print_endline
      "missing+sync confirmed; failed sync uncertain; file and dangling symlink untouched");
  [%expect
    {| missing+sync confirmed; failed sync uncertain; file and dangling symlink untouched |}]
;;

let%expect_test
    "backend absence confirmation uses its own namespace and borrowed directory lifetime"
  =
  with_directory (fun _ sw _ directory ->
    let module B = Provider_secret_store in
    let ok = function
      | Ok v -> v
      | Error e -> raise_s (B.Error.sexp_of_t e)
    in
    let backend =
      B.open_private_files ~sw ~directory ~namespace:(B.Namespace.create "absence" |> ok)
      |> ok
    in
    let revision = B.Revision.create "owned_revision" |> ok in
    B.confirm_absent backend ~revision |> ok;
    B.create
      backend
      ~revision
      (B.Secret.of_bytes (Bytes.of_string "synthetic-owned-material") |> ok)
    |> ok;
    (match B.confirm_absent backend ~revision with
     | Error error ->
       assert (B.Error.equal_code (B.Error.code error) Exists);
       assert (Option.is_none (B.Error.publication error))
     | Ok () -> failwith "present backend revision reported absent");
    B.delete backend ~revision |> ok;
    B.confirm_absent backend ~revision |> ok;
    Storage.Directory.close directory;
    (match B.confirm_absent backend ~revision with
     | Error error ->
       assert (B.Error.equal_code (B.Error.code error) Closed);
       assert (Option.is_none (B.Error.publication error))
     | Ok () -> failwith "closed borrowed directory admitted confirmation");
    print_endline
      "backend confirms its exact revision; existing and closed resources fail without \
       publication claim");
  [%expect
    {| backend confirms its exact revision; existing and closed resources fail without publication claim |}]
;;
