open! Core
module P = Agent_protocol
module B = P.Authoring_bundle.Bundle
module Store = Agent_store.Authoring_upload_store

let () = Mirage_crypto_rng_unix.use_default ()

let protocol_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : P.Error.t)]
;;

let store_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_store.Store_error.t)]
;;

let upload_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Store.Error.t)]
;;

let check label condition =
  if not condition then raise_s [%sexp "upload invariant failed", (label : string)]
;;

let server_id = P.Id.Server.of_string "srv_authoring_upload_test" |> protocol_ok
let principal = P.Id.Principal.of_string "pri_authoring_upload_test" |> protocol_ok
let foreign = P.Id.Principal.of_string "pri_authoring_upload_foreign" |> protocol_ok
let start = P.Timestamp.of_string "2026-10-13T00:00:00Z" |> protocol_ok
let later ms = P.Timestamp.add_ms start ms |> protocol_ok
let digest bytes = Chatml_source.Digest.of_contents bytes ~checkpoint:Fn.id

let root =
  Chatml_source.Id.create ~package_namespace:"pkg" ~relative_path:"chatml.project.json"
  |> Or_error.ok_exn
;;

let script =
  Chatml_source.Id.create ~package_namespace:"pkg" ~relative_path:"src/main.chatml"
  |> Or_error.ok_exn
;;

let member id bytes =
  let source =
    P.Authoring.Source.create
      ~id
      ~digest:(digest bytes)
      ~byte_length:(String.length bytes)
      ~editor_revision:None
    |> protocol_ok
  in
  B.Member.create ~source |> protocol_ok
;;

let root_bytes = "root\000\255"
let script_bytes = "hello\r\nworld"

let manifest ?(script_bytes = script_bytes) () =
  let selection =
    P.Authoring.Context.Selection.create ~target:(Entry "main") ~environment:None
    |> protocol_ok
  in
  let route =
    B.Manifest.Route.create
      ~declaration:root
      ~origin:Chatml_project.Lock.Catalog.Local
      ~dependencies:[]
    |> protocol_ok
  in
  let generated_lock =
    Chatml_source.Id.create ~package_namespace:"pkg" ~relative_path:"generated.lock"
    |> Or_error.ok_exn
  in
  let project =
    B.Manifest.Project.create
      ~root_source:root
      ~selection
      ~entry:Declared
      ~source_members:[ root; script ]
      ~routes:[ route ]
      ~mode:(Compose { selected_previous_lock = None; generated_lock })
    |> protocol_ok
  in
  B.Manifest.create
    ~basis:(Project project)
    ~members:[ member root root_bytes; member script script_bytes ]
  |> protocol_ok
;;

let begin_key = P.Idempotency_key.of_string "begin-upload-test" |> protocol_ok
let commit_key = P.Idempotency_key.of_string "commit-upload-test" |> protocol_ok
let other_commit_key = P.Idempotency_key.of_string "other-commit" |> protocol_ok

let begin_request ?(manifest = manifest ()) () =
  B.Begin.create_request ~idempotency_key:begin_key ~manifest ()
;;

let limits
      ?(max_members = 4)
      ?(max_member_bytes = 128)
      ?(max_chunk_bytes = 64)
      ?(max_bundle_bytes = 256)
      ?(max_reserved_bytes = 512)
      ?(max_reserved_per_principal = 256)
      ?(max_installed_bytes_per_bundle = 128)
      ?(max_installed_bytes = 256)
      ?(max_installed_per_principal = 256)
      ?(max_recovery_source_bytes = 1024)
      ?(max_work_steps = 10000)
      ?(max_journal_records = 32)
      ()
  =
  Store.Limits.create
    ~max_active_uploads:4
    ~max_active_per_principal:2
    ~max_members
    ~max_manifest_bytes:65536
    ~max_member_bytes
    ~max_bundle_bytes
    ~max_chunk_bytes
    ~max_reserved_bytes
    ~max_reserved_per_principal
    ~max_published_records:4
    ~max_installed_sources:32
    ~max_installed_bytes_per_bundle
    ~max_installed_bytes
    ~max_installed_per_principal
    ~max_publication_metadata_bytes:16384
    ~max_intent_bytes:8192
    ~max_marker_bytes:2048
    ~max_work_steps
    ~max_journal_records
    ~max_journal_bytes:16384
    ~max_idempotency_records:8
    ~max_tombstones:8
    ~max_tombstone_bytes:600000
    ~max_recovery_entries:1000
    ~max_recovery_metadata_bytes:1048576
    ~max_recovery_source_bytes
    ~staging_ttl:(Time_ns.Span.of_sec 60.)
    ~receipt_ttl:(Time_ns.Span.of_sec 120.)
    ~expired_tombstone_ttl:(Time_ns.Span.of_sec 180.)
  |> Or_error.ok_exn
;;

let with_root name f =
  Eio_main.run (fun env ->
    let temporary =
      Filename.concat
        (Sys.getenv "TMPDIR" |> Option.value ~default:"/tmp")
        (name ^ "." ^ P.Id.Transaction.(create () |> to_string))
    in
    let path = Eio.Path.(Eio.Stdenv.fs env / temporary) in
    Eio.Path.mkdir ~perm:0o700 path;
    Exn.protect
      ~f:(fun () ->
        Eio.Switch.run (fun sw ->
          let root =
            Agent_store.Data_root.create ~env ~path:(Filename.concat temporary "data")
            |> store_ok
          in
          Agent_store.Durable_file.replace
            ~env
            ~durability:Flush_file_and_directory
            ~path:(Agent_store.Data_root.server_id_path root)
            (P.Id.Server.to_string server_id ^ "\n")
          |> store_ok;
          let lock =
            Agent_store.Lock.acquire
              ~env
              ~sw
              ~path:(Agent_store.Data_root.daemon_lock_path root)
              ~server_id
              ~process_start_identity:(Some "upload-test")
              ~nonce:"upload-test"
            |> store_ok
          in
          f env sw root lock))
      ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true path))
;;

let open_store env sw root lock =
  Store.open_or_create
    ~env
    ~sw
    ~data_root:root
    ~daemon_lock:lock
    ~host_id:server_id
    ~limits:(limits ())
    ~checkpoint:Fn.id
  |> upload_ok
;;

let%expect_test
    "session owner joins its one authoring upload child before releasing the daemon lock"
  =
  with_root "authoring-session-child" (fun env sw root _ ->
    let session =
      Agent_store.Session_store.create
        ~env
        ~sw
        ~root:
          (Filename.concat (Filename.dirname (Agent_store.Data_root.path root)) "session")
        ~server_id
        ~process_start_identity:(Some "authoring-session-child")
        ~lock_nonce:"authoring-session-child"
      |> store_ok
    in
    let first = ref None in
    let interrupted = ref false in
    (match
       Agent_store.Session_store.open_authoring_upload_store
         session
         ~sw
         ~limits:(limits ())
         ~checkpoint:(fun () ->
           interrupted := true;
           raise Exit)
     with
     | exception Exit -> ()
     | Ok _ | Error _ -> failwith "checkpoint interruption lost its exception");
    assert !interrupted;
    Eio.Switch.run (fun child_sw ->
      let child =
        Agent_store.Session_store.open_authoring_upload_store
          session
          ~sw:child_sw
          ~limits:(limits ())
          ~checkpoint:Fn.id
        |> upload_ok
      in
      first := Some child;
      (match
         Agent_store.Session_store.open_authoring_upload_store
           session
           ~sw:child_sw
           ~limits:(limits ())
           ~checkpoint:Fn.id
       with
       | Error Busy -> ()
       | Error _ | Ok _ -> failwith "duplicate upload child opened");
      match Agent_store.Session_store.close session with
      | Error (Locked _) -> ()
      | Error _ | Ok _ -> failwith "session lock released before child join");
    assert (Store.fully_closed (Option.value_exn !first));
    let replacement =
      Agent_store.Session_store.open_authoring_upload_store
        session
        ~sw
        ~limits:(limits ())
        ~checkpoint:Fn.id
      |> upload_ok
    in
    Store.close_and_wait replacement |> upload_ok;
    assert (Store.fully_closed replacement);
    Agent_store.Session_store.close session |> store_ok;
    print_endline
      "interrupted open freed root; duplicate refused; both child close paths joined; \
       lock released");
  [%expect
    {| interrupted open freed root; duplicate refused; both child close paths joined; lock released |}]
;;

let upload_directory env root upload_id =
  Eio.Path.(
    Eio.Stdenv.fs env
    / Agent_store.Data_root.path root
    / "authoring-uploads"
    / P.Id.Authoring_upload.to_string upload_id)
;;

let phase_file env root upload_id =
  Eio.Path.(upload_directory env root upload_id / "phase.json")
;;

let send_chunk store upload_id source byte_offset bytes =
  let request =
    B.Chunk.create_request ~upload_id ~source ~byte_offset ~bytes |> protocol_ok
  in
  Store.chunk store ~principal ~request ~now:(fun () -> start) ~checkpoint:Fn.id
;;

let%expect_test
    "durable upload survives reconnect with exact idempotency and source bytes"
  =
  with_root "ochat-authoring-upload-lifecycle" (fun env sw root_handle lock ->
    let store = open_store env sw root_handle lock in
    let request = begin_request () in
    let response =
      Store.begin_upload
        store
        ~principal
        ~request
        ~now:(fun () -> start)
        ~checkpoint:Fn.id
      |> upload_ok
    in
    let upload_id = B.Begin.upload_id response in
    let replay =
      Store.begin_upload
        store
        ~principal
        ~request
        ~now:(fun () -> start)
        ~checkpoint:Fn.id
      |> upload_ok
    in
    check
      "same begin key returns same upload"
      (P.Id.Authoring_upload.equal upload_id (B.Begin.upload_id replay));
    let different = begin_request ~manifest:(manifest ~script_bytes:"changed" ()) () in
    check
      "same key different manifest conflicts"
      (match
         Store.begin_upload
           store
           ~principal
           ~request:different
           ~now:(fun () -> start)
           ~checkpoint:Fn.id
       with
       | Error Store.Error.Idempotency_conflict -> true
       | _ -> false);
    check
      "foreign principal cannot inspect upload"
      (match
         Store.status
           store
           ~principal:foreign
           ~upload_id
           ~now:(fun () -> start)
           ~checkpoint:Fn.id
       with
       | Error Store.Error.Missing_or_foreign -> true
       | _ -> false);
    let first = send_chunk store upload_id root 0 root_bytes |> upload_ok in
    check
      "first offset"
      (Int.equal (B.Chunk.accepted_until_byte first) (String.length root_bytes));
    let replay = send_chunk store upload_id root 0 root_bytes |> upload_ok in
    check
      "exact chunk repeat"
      (Int.equal (B.Chunk.accepted_until_byte replay) (String.length root_bytes));
    check
      "changed retry conflicts"
      (match send_chunk store upload_id root 0 "xxxxxx" with
       | Error Store.Error.Idempotency_conflict -> true
       | _ -> false);
    let commit = B.Commit.create_request ~upload_id ~idempotency_key:commit_key in
    check
      "incomplete commit consumes no key"
      (match
         Store.claim_commit
           store
           ~principal
           ~request:commit
           ~now:(fun () -> start)
           ~checkpoint:Fn.id
       with
       | Error Store.Error.Incomplete -> true
       | _ -> false);
    ignore (send_chunk store upload_id script 0 script_bytes |> upload_ok);
    let sealed =
      match
        Store.claim_commit
          store
          ~principal
          ~request:commit
          ~now:(fun () -> start)
          ~checkpoint:Fn.id
        |> upload_ok
      with
      | Store.Claim_result.Ready sealed -> sealed
      | Published _ | Rejected _ -> failwith "unexpected claim state"
    in
    let sources =
      Store.snapshot store ~principal sealed ~now:(fun () -> start) ~checkpoint:Fn.id
      |> upload_ok
      |> Store.Snapshot.sources
    in
    check
      "snapshot exact order and bytes"
      (List.equal
         String.equal
         (List.map sources ~f:Chatml_source.contents)
         [ root_bytes; script_bytes ]);
    Store.close_and_wait store |> upload_ok;
    let reopened = open_store env sw root_handle lock in
    check
      "old sealed handle invalid after reopen"
      (match
         Store.snapshot
           reopened
           ~principal
           sealed
           ~now:(fun () -> start)
           ~checkpoint:Fn.id
       with
       | Error Store.Error.Closed -> true
       | _ -> false);
    let replay =
      Store.begin_upload
        reopened
        ~principal
        ~request
        ~now:(fun () -> start)
        ~checkpoint:Fn.id
      |> upload_ok
    in
    check
      "recovered begin identity"
      (P.Id.Authoring_upload.equal upload_id (B.Begin.upload_id replay));
    let sealed =
      match
        Store.claim_commit
          reopened
          ~principal
          ~request:commit
          ~now:(fun () -> start)
          ~checkpoint:Fn.id
        |> upload_ok
      with
      | Store.Claim_result.Ready sealed -> sealed
      | Published _ | Rejected _ -> failwith "unexpected recovered claim state"
    in
    check
      "different commit key conflicts"
      (match
         Store.claim_commit
           reopened
           ~principal
           ~request:(B.Commit.create_request ~upload_id ~idempotency_key:other_commit_key)
           ~now:(fun () -> start)
           ~checkpoint:Fn.id
       with
       | Error Store.Error.Idempotency_conflict -> true
       | _ -> false);
    ignore
      (Store.snapshot reopened ~principal sealed ~now:(fun () -> start) ~checkpoint:Fn.id
       |> upload_ok);
    Store.close_and_wait reopened |> upload_ok;
    printf "one recovered upload; two exact source revisions\n");
  [%expect {| one recovered upload; two exact source revisions |}]
;;

exception Interrupted

let%expect_test "recovery distinguishes interrupted begin, append, and prune" =
  with_root "ochat-authoring-upload-recovery" (fun env sw root_handle lock ->
    let store = open_store env sw root_handle lock in
    let request = begin_request () in
    let upload_id =
      Store.begin_upload
        store
        ~principal
        ~request
        ~now:(fun () -> start)
        ~checkpoint:Fn.id
      |> upload_ok
      |> B.Begin.upload_id
    in
    Store.close_and_wait store |> upload_ok;
    Eio.Path.unlink (phase_file env root_handle upload_id);
    let recovered = open_store env sw root_handle lock in
    let same =
      Store.begin_upload
        recovered
        ~principal
        ~request
        ~now:(fun () -> start)
        ~checkpoint:Fn.id
      |> upload_ok
      |> B.Begin.upload_id
    in
    check
      "unacknowledged begin reuses identity"
      (P.Id.Authoring_upload.equal upload_id same);
    ignore
      (send_chunk recovered upload_id root 0 (String.prefix root_bytes 2) |> upload_ok);
    Store.close_and_wait recovered |> upload_ok;
    let data =
      Eio.Path.(upload_directory env root_handle upload_id / "member-0000.data")
    in
    Eio.Path.with_open_out ~append:true ~create:`Never data (fun flow ->
      Eio.Flow.copy_string "unacknowledged" flow;
      Eio.File.sync flow);
    let recovered = open_store env sw root_handle lock in
    ignore
      (send_chunk recovered upload_id root 2 (String.drop_prefix root_bytes 2)
       |> upload_ok);
    check "unacknowledged suffix truncated" (String.equal (Eio.Path.load data) root_bytes);
    Store.close_and_wait recovered |> upload_ok;
    Eio.Path.unlink (phase_file env root_handle upload_id);
    check
      "progressed stage without phase fails closed"
      (match
         Store.open_or_create
           ~env
           ~sw
           ~data_root:root_handle
           ~daemon_lock:lock
           ~host_id:server_id
           ~limits:(limits ())
           ~checkpoint:Fn.id
       with
       | Error (Store.Error.Storage (Agent_store.Store_error.Corrupt _)) -> true
       | _ -> false));
  with_root "ochat-authoring-upload-prune" (fun env sw root_handle lock ->
    let store = open_store env sw root_handle lock in
    let upload_id =
      Store.begin_upload
        store
        ~principal
        ~request:(begin_request ())
        ~now:(fun () -> start)
        ~checkpoint:Fn.id
      |> upload_ok
      |> B.Begin.upload_id
    in
    let phase = phase_file env root_handle upload_id in
    let interrupted () =
      if String.is_substring (Eio.Path.load phase) ~substring:"\"expired\""
      then raise Interrupted
    in
    check
      "interruption preserves original exception"
      (match
         Store.prune
           store
           ~now:(fun () -> later 61000)
           ~protected_uploads:[]
           ~checkpoint:interrupted
       with
       | exception Interrupted -> true
       | _ -> false);
    check
      "same store recovers after interrupted cleanup"
      (match
         Store.status
           store
           ~principal
           ~upload_id
           ~now:(fun () -> later 61000)
           ~checkpoint:Fn.id
       with
       | Ok Store.Status.Expired -> true
       | _ -> false);
    Store.close_and_wait store |> upload_ok;
    let directory = upload_directory env root_handle upload_id in
    let pruning =
      Eio.Path.(
        Eio.Stdenv.fs env
        / Agent_store.Data_root.path root_handle
        / "authoring-uploads"
        / (".prune-" ^ P.Id.Authoring_upload.to_string upload_id))
    in
    Eio.Path.rename directory pruning;
    let recovered = open_store env sw root_handle lock in
    check
      "interrupted prune marker consumed"
      (match Eio.Path.kind ~follow:false pruning with
       | `Not_found -> true
       | _ -> false);
    check
      "pruned upload cannot resurrect"
      (match
         Store.status
           recovered
           ~principal
           ~upload_id
           ~now:(fun () -> start)
           ~checkpoint:Fn.id
       with
       | Error Store.Error.Missing_or_foreign -> true
       | _ -> false);
    Store.close_and_wait recovered |> upload_ok);
  with_root "ochat-authoring-upload-draft-version" (fun env sw root_handle lock ->
    let store = open_store env sw root_handle lock in
    let upload_id =
      Store.begin_upload
        store
        ~principal
        ~request:(begin_request ())
        ~now:(fun () -> start)
        ~checkpoint:Fn.id
      |> upload_ok
      |> B.Begin.upload_id
    in
    Store.close_and_wait store |> upload_ok;
    let phase = phase_file env root_handle upload_id in
    let current = Eio.Path.load phase in
    check
      "current private stage schema is v2"
      (String.is_substring current ~substring:"\"schema_version\":2");
    let obsolete =
      String.substr_replace_first
        current
        ~pattern:"\"schema_version\":2"
        ~with_:"\"schema_version\":1"
    in
    Eio.Path.save ~create:`Never phase obsolete;
    check
      "unreleased old draft stage is rejected without replay"
      (match
         Store.open_or_create
           ~env
           ~sw
           ~data_root:root_handle
           ~daemon_lock:lock
           ~host_id:server_id
           ~limits:(limits ())
           ~checkpoint:Fn.id
       with
       | Error (Store.Error.Storage (Agent_store.Store_error.Corrupt _)) -> true
       | _ -> false));
  printf "partial begin, suffix and prune recover; old draft fails closed\n";
  [%expect {| partial begin, suffix and prune recover; old draft fails closed |}]
;;

let%expect_test "protected sealed claim retains source through expiry maintenance" =
  with_root "ochat-authoring-upload-protected" (fun env sw root_handle lock ->
    let store = open_store env sw root_handle lock in
    let upload_id =
      Store.begin_upload
        store
        ~principal
        ~request:(begin_request ())
        ~now:(fun () -> start)
        ~checkpoint:Fn.id
      |> upload_ok
      |> B.Begin.upload_id
    in
    ignore (send_chunk store upload_id root 0 root_bytes |> upload_ok);
    ignore (send_chunk store upload_id script 0 script_bytes |> upload_ok);
    ignore
      (Store.claim_commit
         store
         ~principal
         ~request:(B.Commit.create_request ~upload_id ~idempotency_key:commit_key)
         ~now:(fun () -> start)
         ~checkpoint:Fn.id
       |> upload_ok);
    ignore
      (Store.prune
         store
         ~now:(fun () -> later 121000)
         ~protected_uploads:[ upload_id ]
         ~checkpoint:Fn.id
       |> upload_ok);
    let member =
      Eio.Path.(upload_directory env root_handle upload_id / "member-0000.data")
    in
    check "protected bytes remain" (String.equal (Eio.Path.load member) root_bytes);
    ignore
      (Store.prune
         store
         ~now:(fun () -> later 121000)
         ~protected_uploads:[]
         ~checkpoint:Fn.id
       |> upload_ok);
    check
      "unprotected bytes cleaned"
      (match Eio.Path.kind ~follow:false member with
       | `Not_found -> true
       | _ -> false);
    Store.close_and_wait store |> upload_ok);
  printf "protected seal retained until owner releases it\n";
  [%expect {| protected seal retained until owner releases it |}]
;;

let%expect_test "recovery reapplies configured member policy and live lock" =
  with_root "ochat-authoring-upload-reconfigured" (fun env sw root_handle lock ->
    let store = open_store env sw root_handle lock in
    let upload_id =
      Store.begin_upload
        store
        ~principal
        ~request:(begin_request ())
        ~now:(fun () -> start)
        ~checkpoint:Fn.id
      |> upload_ok
      |> B.Begin.upload_id
    in
    Store.close_and_wait store |> upload_ok;
    check
      "lower member bound rejects existing stage"
      (match
         Store.open_or_create
           ~env
           ~sw
           ~data_root:root_handle
           ~daemon_lock:lock
           ~host_id:server_id
           ~limits:(limits ~max_member_bytes:8 ~max_chunk_bytes:8 ())
           ~checkpoint:Fn.id
       with
       | Error (Store.Error.Storage (Agent_store.Store_error.Corrupt _)) -> true
       | _ -> false);
    check
      "startup work budget applies to recovery"
      (match
         Store.open_or_create
           ~env
           ~sw
           ~data_root:root_handle
           ~daemon_lock:lock
           ~host_id:server_id
           ~limits:(limits ~max_work_steps:1 ())
           ~checkpoint:Fn.id
       with
       | Error Store.Error.Limit_exceeded -> true
       | _ -> false);
    let reopened = open_store env sw root_handle lock in
    check
      "failed open released local registry"
      (match
         Store.status
           reopened
           ~principal
           ~upload_id
           ~now:(fun () -> start)
           ~checkpoint:Fn.id
       with
       | Ok (Store.Status.Open _) -> true
       | _ -> false);
    Agent_store.Lock.release ~env lock |> store_ok;
    check
      "released daemon lock closes store admission"
      (match
         Store.status
           reopened
           ~principal
           ~upload_id
           ~now:(fun () -> start)
           ~checkpoint:Fn.id
       with
       | Error Store.Error.Closed -> true
       | _ -> false);
    Store.close_and_wait reopened |> upload_ok);
  printf "limits and daemon ownership rechecked\n";
  [%expect {| limits and daemon ownership rechecked |}]
;;

let%expect_test "begin reserves all future member progress records" =
  with_root "ochat-authoring-upload-record-reservation" (fun env sw root_handle lock ->
    let store =
      Store.open_or_create
        ~env
        ~sw
        ~data_root:root_handle
        ~daemon_lock:lock
        ~host_id:server_id
        ~limits:(limits ~max_journal_records:2 ())
        ~checkpoint:Fn.id
      |> upload_ok
    in
    check
      "two-member upload cannot fit two total records"
      (match
         Store.begin_upload
           store
           ~principal
           ~request:(begin_request ())
           ~now:(fun () -> start)
           ~checkpoint:Fn.id
       with
       | Error Store.Error.Limit_exceeded -> true
       | _ -> false);
    Store.close_and_wait store |> upload_ok);
  printf "future progress quota checked before begin\n";
  [%expect {| future progress quota checked before begin |}]
;;
