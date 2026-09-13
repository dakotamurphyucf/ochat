open Core
open Agent_store_test_fixtures

let%expect_test "data root creates the complete restrictive layout" =
  with_temp_directory "ochat-agent-store-root" (fun env temporary ->
    let path = Filename.concat temporary "data" in
    let root = Agent_store.Data_root.create ~env ~path |> store_ok in
    let directories =
      [ Agent_store.Data_root.path root
      ; Agent_store.Data_root.indexes_path root
      ; Agent_store.Data_root.prompt_artifacts_path root
      ; Agent_store.Data_root.temporary_blobs_path root
      ; Agent_store.Data_root.durable_blobs_path root
      ; Agent_store.Data_root.audit_path root
      ; Agent_store.Data_root.sessions_path root
      ; Agent_store.Data_root.migrations_path root
      ; Agent_store.Data_root.lost_and_found_path root
      ]
    in
    let root_mode =
      let stat : Eio.File.Stat.t =
        Eio.Path.stat ~follow:true Eio.Path.(Eio.Stdenv.fs env / path)
      in
      sprintf "%03o" (stat.perm land 0o777)
    in
    print_s
      [%sexp
        { all_directories_exist =
            (List.for_all directories ~f:(directory_exists env) : bool)
        ; root_mode : string
        }]);
  [%expect {| ((all_directories_exist true) (root_mode 700)) |}]
;;

let%expect_test "data root rejects ambient unsafe paths" =
  Eio_main.run (fun env ->
    let classify path =
      match Agent_store.Data_root.create ~env ~path with
      | Ok _ -> "accepted"
      | Error _ -> "rejected"
    in
    print_s [%sexp (List.map [ "/"; "relative" ] ~f:classify : string list)]);
  [%expect {| (rejected rejected) |}]
;;

let%expect_test "durable replacement exposes complete old or new contents" =
  with_temp_directory "ochat-agent-durable-file" (fun env temporary ->
    let path = Filename.concat temporary "state" in
    Agent_store.Durable_file.replace ~env ~durability:Flush_file_and_directory ~path "old"
    |> store_ok;
    let old = Agent_store.Durable_file.load ~env ~path |> store_ok in
    Agent_store.Durable_file.replace ~env ~durability:Flush_file_and_directory ~path "new"
    |> store_ok;
    let current = Agent_store.Durable_file.load ~env ~path |> store_ok in
    print_s [%sexp { old : string; current : string }]);
  [%expect {| ((old old) (current new)) |}]
;;

let%expect_test "durable replacement reports failures without a partial target" =
  with_temp_directory "ochat-agent-durable-failure" (fun env temporary ->
    let parent = Filename.concat temporary "missing" in
    let path = Filename.concat parent "state" in
    let failed =
      Agent_store.Durable_file.replace ~env ~durability:Flush_file ~path "value"
      |> Result.is_error
    in
    let target_exists = path_exists env path in
    print_s [%sexp { failed : bool; target_exists : bool }]);
  [%expect {| ((failed true) (target_exists false)) |}]
;;

let crash_recovery_sync_file (Eio.Resource.T (file, handler)) ~expose_descriptor ~lookups =
  let module Original = (val Eio.Resource.get handler Eio.File.Pi.Read) in
  let bindings = Eio.Resource.bindings (Eio.File.Pi.ro (module Original)) in
  let bindings =
    if expose_descriptor
    then (
      let descriptor =
        Eio.Resource.get_opt handler Eio_unix.Resource.T |> Option.value_exn
      in
      Eio.Resource.H
        ( Eio_unix.Resource.T
        , fun file ->
            Int.incr lookups;
            descriptor file )
      :: bindings)
    else bindings
  in
  Eio.Resource.T (file, Eio.Resource.handler bindings)
;;

let crash_recovery_sync_directory
      (Eio.Resource.T (directory, handler))
      ~expose_descriptor
      ~opens
      ~lookups
  =
  let module Original = (val Eio.Resource.get handler Eio.Fs.Pi.Dir) in
  let module Directory = struct
    include Original

    let open_in directory ~sw path =
      Int.incr opens;
      Original.open_in directory ~sw path
      |> crash_recovery_sync_file ~expose_descriptor ~lookups
    ;;

    let open_dir _directory ~sw:_ _path =
      failwith "directory sync must use a read-only file descriptor"
    ;;
  end
  in
  Eio.Resource.T
    (directory, Eio.Resource.handler [ H (Eio.Fs.Pi.Dir, (module Directory)) ])
;;

let crash_recovery_sync_env env ~expose_descriptor ~opens ~lookups =
  let directory, path = Eio.Stdenv.fs env in
  let fs =
    crash_recovery_sync_directory directory ~expose_descriptor ~opens ~lookups, path
  in
  object
    method fs = fs
    method cwd = env#cwd
    method stdin = env#stdin
    method stdout = env#stdout
    method stderr = env#stderr
    method net = env#net
    method domain_mgr = env#domain_mgr
    method process_mgr = env#process_mgr
    method clock = env#clock
    method mono_clock = env#mono_clock
    method secure_random = env#secure_random
    method debug = env#debug
    method backend_id = env#backend_id
  end
;;

let%expect_test "directory sync uses a read-only native descriptor and fails closed" =
  with_temp_directory "ochat-agent-directory-sync" (fun env path ->
    List.iter [ true; false ] ~f:(fun expose_descriptor ->
      let opens = ref 0 in
      let lookups = ref 0 in
      let wrapped = crash_recovery_sync_env env ~expose_descriptor ~opens ~lookups in
      let result = Agent_store.Durable_file.sync_directory ~env:wrapped ~path in
      (match expose_descriptor, result with
       | true, Ok () when !opens = 1 && !lookups = 1 -> ()
       | false, Error (Agent_store.Store_error.Io { operation; message; _ })
         when !opens = 1
              && !lookups = 0
              && String.equal operation "sync directory"
              && String.is_substring message ~substring:"native directory FD" -> ()
       | _ -> failwith "directory sync did not enforce native descriptor durability");
      Eio.Flow.copy_string
        (if expose_descriptor
         then "native directory synced\n"
         else "unsupported provider rejected\n")
        (Eio.Stdenv.stdout env)));
  [%expect
    {|
    native directory synced
    unsupported provider rejected
    |}]
;;

let%expect_test "journal frames round-trip and expose a stable checksum" =
  let encoded =
    Agent_store.Frame.encode ~max_payload_length:1024 ~flags:3 "payload" |> frame_ok
  in
  match Agent_store.Frame.decode ~max_payload_length:1024 ~contents:encoded ~offset:0 with
  | Error error ->
    raise_s [%sexp "unexpected frame error", (error : Agent_store.Frame.error)]
  | Ok (Incomplete_tail _) -> failwith "complete frame decoded as a crash tail"
  | Ok (Complete { frame; next_offset }) ->
    print_s
      [%sexp
        { flags = (Agent_store.Frame.flags frame : int)
        ; payload = (Agent_store.Frame.payload frame : string)
        ; checksum_length = (String.length (Agent_store.Frame.checksum_hex frame) : int)
        ; next_offset : int
        }];
    [%expect {| ((flags 3) (payload payload) (checksum_length 64) (next_offset 59)) |}]
;;

let%expect_test "journal frames distinguish crash tails from corruption" =
  let encoded =
    Agent_store.Frame.encode ~max_payload_length:1024 ~flags:0 "payload" |> frame_ok
  in
  let truncated = String.drop_suffix encoded 1 in
  let corrupted = Bytes.of_string encoded in
  Bytes.set corrupted 20 'X';
  let classify contents =
    match Agent_store.Frame.decode ~max_payload_length:1024 ~contents ~offset:0 with
    | Ok (Incomplete_tail _) -> "tail"
    | Ok (Complete _) -> "complete"
    | Error Checksum_mismatch -> "checksum"
    | Error _ -> "other"
  in
  print_s
    [%sexp
      { truncated = (classify truncated : string)
      ; corrupted = (classify (Bytes.to_string corrupted) : string)
      }];
  [%expect {| ((truncated tail) (corrupted checksum)) |}]
;;

let%expect_test "locks expose ownership, reject contention, and release cleanly" =
  with_temp_directory "ochat-agent-lock" (fun env temporary ->
    Eio.Switch.run (fun switch ->
      let path = Filename.concat temporary "actor.lock" in
      let acquire nonce =
        Agent_store.Lock.acquire
          ~env
          ~sw:switch
          ~path
          ~server_id
          ~process_start_identity:(Some "test-process")
          ~nonce
      in
      let first = acquire "first" |> store_ok in
      let owner =
        Agent_store.Lock.read_owner ~env ~path |> store_ok |> Option.value_exn
      in
      let contended = acquire "second" |> Result.is_error in
      Agent_store.Lock.release ~env first |> store_ok;
      let second = acquire "second" |> store_ok in
      Agent_store.Lock.release ~env second |> store_ok;
      print_s
        [%sexp
          { owner_nonce = (owner.nonce : string)
          ; process_identity = (owner.process_start_identity : string option)
          ; contended : bool
          }]));
  [%expect
    {|
    ((owner_nonce first) (process_identity (test-process)) (contended true))
    |}]
;;

let%expect_test "journal segments recover only an incomplete final frame" =
  with_temp_directory "ochat-agent-segment" (fun env directory ->
    let segment =
      Agent_store.Journal_segment.create_exclusive
        ~env
        ~directory
        ~id:Agent_store.Journal_segment.Id.first
      |> store_ok
    in
    let first =
      Agent_store.Frame.encode ~max_payload_length:1024 ~flags:0 "one" |> frame_ok
    in
    let second =
      Agent_store.Frame.encode ~max_payload_length:1024 ~flags:0 "two" |> frame_ok
    in
    Agent_store.Journal_segment.append ~env ~durability:Flush segment ~frame:first
    |> store_ok
    |> ignore;
    let partial = String.drop_suffix second 7 in
    Agent_store.Journal_segment.append ~env ~durability:Flush segment ~frame:partial
    |> store_ok
    |> ignore;
    let scan =
      Agent_store.Journal_segment.scan ~env ~max_payload_length:1024 segment |> store_ok
    in
    Agent_store.Journal_segment.truncate_crash_tail ~env segment scan |> store_ok;
    let repaired =
      Agent_store.Journal_segment.scan ~env ~max_payload_length:1024 segment |> store_ok
    in
    print_s
      [%sexp
        { before_entries = (List.length scan.entries : int)
        ; before_tail = (scan.crash_tail : bool)
        ; after_entries = (List.length repaired.entries : int)
        ; after_tail = (repaired.crash_tail : bool)
        }]);
  [%expect
    {|
    ((before_entries 1) (before_tail true) (after_entries 1) (after_tail false))
    |}]
;;

let%expect_test "journal segment rejects corruption in a complete frame" =
  with_temp_directory "ochat-agent-segment-corrupt" (fun env directory ->
    let segment =
      Agent_store.Journal_segment.create_exclusive
        ~env
        ~directory
        ~id:Agent_store.Journal_segment.Id.first
      |> store_ok
    in
    let frame =
      Agent_store.Frame.encode ~max_payload_length:1024 ~flags:0 "one" |> frame_ok
    in
    let corrupted = Bytes.of_string frame in
    Bytes.set corrupted 20 'X';
    Agent_store.Journal_segment.append
      ~env
      ~durability:Flush
      segment
      ~frame:(Bytes.to_string corrupted)
    |> store_ok
    |> ignore;
    let corrupt =
      Agent_store.Journal_segment.scan ~env ~max_payload_length:1024 segment
      |> Result.is_error
    in
    print_s [%sexp { corrupt : bool }]);
  [%expect {| ((corrupt true)) |}]
;;

let%expect_test "journal rotates segments and preserves ordered replay" =
  with_temp_directory "ochat-agent-journal" (fun env temporary ->
    let directory = Filename.concat temporary "journal" in
    let journal =
      Agent_store.Journal.create
        ~env
        ~directory
        ~max_payload_length:1024
        ~max_segment_bytes:4096L
        ~max_segment_frames:2
      |> store_ok
    in
    List.iter [ "one"; "two"; "three" ] ~f:(fun payload ->
      Agent_store.Journal.append ~durability:Flush journal ~flags:0 ~payload
      |> store_ok
      |> ignore);
    let scan = Agent_store.Journal.scan journal |> store_ok in
    let payloads =
      List.map scan.entries ~f:(fun entry -> Agent_store.Frame.payload entry.frame)
    in
    print_s
      [%sexp
        { segment_count =
            (List.map scan.entries ~f:(fun entry -> entry.segment_id)
             |> List.dedup_and_sort ~compare:Agent_store.Journal_segment.Id.compare
             |> List.length
             : int)
        ; payloads : string list
        ; crash_tail = (Option.is_some scan.crash_tail : bool)
        }]);
  [%expect
    {|
    ((segment_count 2) (payloads (one two "segment sealed" three))
     (crash_tail false))
    |}]
;;

let snapshot transaction_sequence payload =
  Agent_store.Snapshot.
    { schema_version = 1
    ; transaction_sequence
    ; transaction_hash = Some (sprintf "hash-%Ld" transaction_sequence)
    ; event_sequence = transaction_sequence
    ; created_at = timestamp
    ; prompt_artifact = "prompt-revision"
    ; workspace_identity = "workspace-instance"
    ; payload
    }
;;

let%expect_test "snapshots install atomically and load the current checkpoint" =
  with_temp_directory "ochat-agent-snapshot" (fun env temporary ->
    let directory = Filename.concat temporary "snapshot" in
    Agent_store.Snapshot.install
      ~env
      ~directory
      ~max_payload_length:4096
      (snapshot 1L "first")
    |> store_ok
    |> ignore;
    Agent_store.Snapshot.install
      ~env
      ~directory
      ~max_payload_length:4096
      (snapshot 2L "second")
    |> store_ok
    |> ignore;
    let loaded =
      Agent_store.Snapshot.load_current ~env ~directory ~max_payload_length:4096
      |> store_ok
      |> Option.value_exn
    in
    print_s
      [%sexp
        { filename = (loaded.filename : string)
        ; sequence = (loaded.snapshot.transaction_sequence : int64)
        ; payload = (loaded.snapshot.payload : string)
        }]);
  [%expect
    {|
    ((filename snapshot-0000000000000002.bin) (sequence 2) (payload second))
    |}]
;;

let%expect_test "an incomplete current snapshot falls back to the previous checkpoint" =
  with_temp_directory "ochat-agent-snapshot-fallback" (fun env temporary ->
    let directory = Filename.concat temporary "snapshot" in
    Agent_store.Snapshot.install
      ~env
      ~directory
      ~max_payload_length:4096
      (snapshot 1L "first")
    |> store_ok
    |> ignore;
    let current =
      Agent_store.Snapshot.install
        ~env
        ~directory
        ~max_payload_length:4096
        (snapshot 2L "second")
      |> store_ok
    in
    let current_path = Filename.concat directory current.filename in
    Eio.Path.save
      ~create:(`Or_truncate 0o600)
      Eio.Path.(Eio.Stdenv.fs env / current_path)
      "short";
    let loaded =
      Agent_store.Snapshot.load_current ~env ~directory ~max_payload_length:4096
      |> store_ok
      |> Option.value_exn
    in
    print_s
      [%sexp
        { sequence = (loaded.snapshot.transaction_sequence : int64)
        ; payload = (loaded.snapshot.payload : string)
        }]);
  [%expect {| ((sequence 1) (payload first)) |}]
;;

let%expect_test "snapshot pruning retains bounded current and fallback checkpoints" =
  with_temp_directory "ochat-agent-snapshot-prune" (fun env temporary ->
    let directory = Filename.concat temporary "snapshot" in
    List.iter [ 1L; 2L; 3L ] ~f:(fun sequence ->
      Agent_store.Snapshot.install
        ~env
        ~directory
        ~max_payload_length:4096
        (snapshot sequence (Int64.to_string sequence))
      |> store_ok
      |> ignore);
    let removed = Agent_store.Snapshot.prune_older ~env ~directory ~keep:2 |> store_ok in
    let floor =
      Agent_store.Snapshot.retention_floor ~env ~directory ~max_payload_length:4096
      |> store_ok
    in
    assert (Int64.equal floor 2L);
    let files =
      Eio.Path.read_dir Eio.Path.(Eio.Stdenv.fs env / directory)
      |> List.filter ~f:(String.is_suffix ~suffix:".bin")
      |> List.sort ~compare:String.compare
    in
    let newest = List.last_exn files |> Filename.concat directory in
    Eio.Path.save
      ~create:(`Or_truncate 0o600)
      Eio.Path.(Eio.Stdenv.fs env / newest)
      "short";
    let fallback =
      Agent_store.Snapshot.load_current ~env ~directory ~max_payload_length:4096
      |> store_ok
      |> Option.value_exn
    in
    print_s
      [%sexp
        { removed : int
        ; files : string list
        ; fallback_sequence = (fallback.snapshot.transaction_sequence : int64)
        }]);
  [%expect
    {|
    ((removed 1)
     (files (snapshot-0000000000000002.bin snapshot-0000000000000003.bin))
     (fallback_sequence 2))
    |}]
;;

let crash_recovery_index_path root =
  Filename.concat (Filename.concat root "indexes") "sessions.snapshot"
;;

let crash_recovery_path env filename = Eio.Path.(Eio.Stdenv.fs env / filename)

let crash_recovery_save env filename contents =
  Eio.Path.save ~create:(`Or_truncate 0o600) (crash_recovery_path env filename) contents
;;

let crash_recovery_open ~sw env root =
  Agent_store.Session_store.open_existing
    ~env
    ~sw
    ~root
    ~process_start_identity:(Some "crash-recovery-test")
    ~lock_nonce:"crash-recovery-open"
;;

let crash_recovery_create ~sw env root =
  Agent_store.Session_store.create
    ~env
    ~sw
    ~root
    ~server_id
    ~process_start_identity:(Some "crash-recovery-test")
    ~lock_nonce:"crash-recovery-create"
  |> store_ok
;;

let crash_recovery_add store ~sw id revision =
  let metadata = metadata revision in
  let metadata =
    { metadata with session = { metadata.session with id }; data_schema_version = 2 }
  in
  let handle =
    Agent_store.Session_store.create_session
      store
      ~sw
      ~transaction_id:(Agent_protocol.Id.Transaction.create ())
      ~actor_lock_nonce:"crash-recovery-actor"
      metadata
    |> store_ok
  in
  Agent_store.Session_store.close_session store handle |> store_ok
;;

let crash_recovery_seed ~sw env root =
  let store = crash_recovery_create ~sw env root in
  crash_recovery_add store ~sw session_id 7L;
  Agent_store.Session_store.close store |> store_ok
;;

let crash_recovery_assert condition message =
  if not condition then raise_s [%sexp "crash recovery regression", (message : string)]
;;

let crash_recovery_report message =
  Eio_main.run (fun env -> Eio.Flow.copy_string (message ^ "\n") (Eio.Stdenv.stdout env))
;;

let crash_recovery_namespace ~sw env root =
  let store = crash_recovery_create ~sw env root in
  let ids =
    [ "ses_archived_recovery"
    ; "ses_deleted_recovery"
    ; "ses_tombstone_recovery"
    ; "ses_staged_recovery"
    ]
    |> List.map ~f:(fun value -> Agent_protocol.Id.Session.of_string value |> protocol_ok)
  in
  List.iter (session_id :: ids) ~f:(fun id -> crash_recovery_add store ~sw id 7L);
  let archived = List.nth_exn ids 0 in
  Agent_store.Session_store.archive_session store archived |> store_ok;
  Agent_store.Session_store.remove_session store (List.nth_exn ids 1) |> store_ok;
  let data_root = Agent_store.Session_store.data_root store in
  let move id destination =
    Eio.Path.rename
      (crash_recovery_path env (Agent_store.Data_root.session_path data_root id))
      (crash_recovery_path env destination)
  in
  Agent_store.Session_store.close store |> store_ok;
  let tombstone =
    Filename.concat
      (Agent_store.Data_root.lost_and_found_path data_root)
      "deleted-ses_tombstone_recovery"
  in
  let staged =
    Filename.concat (Agent_store.Data_root.sessions_path data_root) ".creating-recovery"
  in
  move (List.nth_exn ids 2) tombstone;
  move (List.nth_exn ids 3) staged;
  archived, tombstone, staged
;;

let%expect_test
    "missing session index rebuild preserves installed archived and hidden namespaces"
  =
  with_temp_directory "ochat-crash-recovery-index" (fun env root ->
    Eio.Switch.run (fun sw ->
      let archived, tombstone, staged = crash_recovery_namespace ~sw env root in
      let partial =
        Filename.concat (Filename.concat root "sessions") ".creating-incomplete"
      in
      Eio.Path.mkdir ~perm:0o700 (crash_recovery_path env partial);
      crash_recovery_save env (Filename.concat partial "metadata.sexp") "partial";
      Eio.Path.unlink (crash_recovery_path env (crash_recovery_index_path root));
      let store = crash_recovery_open ~sw env root |> store_ok in
      let entries = Agent_store.Session_store.list_sessions store in
      let find id =
        List.find_exn entries ~f:(fun entry ->
          Agent_protocol.Id.Session.compare entry.session.id id = 0)
      in
      crash_recovery_assert
        (List.length entries = 2)
        "rebuild included a staged/deleted session";
      crash_recovery_assert
        (Agent_store.Session_store.index_was_rebuilt store)
        "missing index did not report rebuilding";
      crash_recovery_assert
        (Sexp.equal
           (Agent_protocol.Session.sexp_of_t (find session_id).session)
           (Agent_protocol.Session.sexp_of_t (session_summary 7L)))
        "rebuild changed the active session summary";
      crash_recovery_assert
        (find archived).archived
        "rebuild resurrected an archived session";
      Agent_store.Session_store.close store |> store_ok;
      let reopened = crash_recovery_open ~sw env root |> store_ok in
      crash_recovery_assert
        (Agent_store.Session_store.index_was_rebuilt reopened)
        "interrupted eager recovery lost its durable requirement";
      crash_recovery_assert
        (List.length (Agent_store.Session_store.list_sessions reopened) = 2)
        "rebuilt index was not persisted";
      Agent_store.Session_store.complete_index_recovery reopened |> store_ok;
      Agent_store.Session_store.close reopened |> store_ok;
      let completed = crash_recovery_open ~sw env root |> store_ok in
      crash_recovery_assert
        (not (Agent_store.Session_store.index_was_rebuilt completed))
        "completed eager recovery did not clear its durable marker";
      Agent_store.Session_store.close completed |> store_ok;
      crash_recovery_assert
        (List.for_all [ tombstone; staged; partial ] ~f:(directory_exists env))
        "rebuild removed an unfinished layout"));
  crash_recovery_report
    "rebuilt exact active summary; archived preserved; deleted/staged excluded; index \
     persisted";
  [%expect
    {| rebuilt exact active summary; archived preserved; deleted/staged excluded; index persisted |}]
;;

let crash_recovery_invalid env root case =
  let directory =
    Filename.concat
      (Filename.concat root "sessions")
      (Agent_protocol.Id.Session.to_string session_id)
  in
  let metadata_path = Filename.concat directory "metadata.sexp" in
  match case with
  | "identity" ->
    let changed = metadata 7L in
    let other = Agent_protocol.Id.Session.of_string "ses_wrong_recovery" |> protocol_ok in
    let changed = { changed with session = { changed.session with id = other } } in
    crash_recovery_save
      env
      metadata_path
      (Sexp.to_string_mach (Agent_store.Session_store.Metadata.sexp_of_t changed))
  | "layout" ->
    Eio.Path.rmdir (crash_recovery_path env (Filename.concat directory "journal"))
  | "metadata" -> crash_recovery_save env metadata_path "(broken"
  | "symlink" ->
    let target = Filename.concat root "outside-session" in
    Eio.Path.rename (crash_recovery_path env directory) (crash_recovery_path env target);
    Eio.Path.symlink ~link_to:target (crash_recovery_path env directory)
  | _ -> failwith "unknown crash recovery invalid fixture"
;;

let crash_recovery_check_invalid case =
  with_temp_directory "ochat-crash-recovery-invalid" (fun env root ->
    Eio.Switch.run (fun sw ->
      crash_recovery_seed ~sw env root;
      crash_recovery_invalid env root case;
      let index_path = crash_recovery_index_path root in
      Eio.Path.unlink (crash_recovery_path env index_path);
      for _ = 1 to 2 do
        (match crash_recovery_open ~sw env root with
         | Error (Corrupt _) -> ()
         | Ok store ->
           Agent_store.Session_store.close store |> store_ok;
           failwith "invalid rebuild was accepted"
         | Error error ->
           raise_s [%sexp "wrong rebuild error", (error : Agent_store.Store_error.t)]);
        crash_recovery_assert
          (not (path_exists env index_path))
          "failed rebuild published an empty index"
      done))
;;

let%expect_test
    "missing index rejects invalid active layouts without publishing an empty index"
  =
  List.iter
    [ "identity"; "layout"; "metadata"; "symlink" ]
    ~f:crash_recovery_check_invalid;
  crash_recovery_report
    "identity/layout/metadata/symlink rejected; index absent; ownership released";
  [%expect
    {| identity/layout/metadata/symlink rejected; index absent; ownership released |}]
;;

let%expect_test "existing corrupt session index stays corrupt instead of invoking rebuild"
  =
  with_temp_directory "ochat-crash-recovery-corrupt" (fun env root ->
    Eio.Switch.run (fun sw ->
      crash_recovery_seed ~sw env root;
      let index_path = crash_recovery_index_path root in
      crash_recovery_save env index_path "(complete-corrupt-index";
      (match crash_recovery_open ~sw env root with
       | Error (Corrupt _) -> ()
       | Ok store ->
         Agent_store.Session_store.close store |> store_ok;
         failwith "corrupt index accepted"
       | Error error ->
         raise_s [%sexp "wrong index error", (error : Agent_store.Store_error.t)]);
      crash_recovery_assert
        (String.equal
           (Eio.Path.load (crash_recovery_path env index_path))
           "(complete-corrupt-index")
        "corrupt index was overwritten"));
  crash_recovery_report "complete corrupt index rejected and preserved";
  [%expect {| complete corrupt index rejected and preserved |}]
;;

let%expect_test "valid session index is authoritative and backfills old archive flags" =
  with_temp_directory "ochat-crash-recovery-archive" (fun env root ->
    Eio.Switch.run (fun sw ->
      crash_recovery_seed ~sw env root;
      let store = crash_recovery_open ~sw env root |> store_ok in
      let index = Agent_store.Session_store.session_index store in
      let entry = Agent_store.Session_index.find index session_id |> Option.value_exn in
      Agent_store.Session_index.upsert index { entry with archived = true } |> store_ok;
      Agent_store.Session_store.close store |> store_ok;
      let reopened = crash_recovery_open ~sw env root |> store_ok in
      crash_recovery_assert
        (not (Agent_store.Session_store.index_was_rebuilt reopened))
        "valid index triggered rebuilding";
      Agent_store.Session_store.close reopened |> store_ok;
      Eio.Path.unlink (crash_recovery_path env (crash_recovery_index_path root));
      let rebuilt = crash_recovery_open ~sw env root |> store_ok in
      let entry = List.hd_exn (Agent_store.Session_store.list_sessions rebuilt) in
      crash_recovery_assert
        entry.archived
        "legacy archive flag was not backfilled durably";
      Agent_store.Session_store.close rebuilt |> store_ok));
  crash_recovery_report
    "valid index retained; legacy archive flag survived later index loss";
  [%expect {| valid index retained; legacy archive flag survived later index loss |}]
;;

let%expect_test "session store owns, persists, indexes, and reopens sessions" =
  with_temp_directory "ochat-agent-session-store" (fun env temporary ->
    let root = Filename.concat temporary "data" in
    Eio.Switch.run (fun switch ->
      let store =
        Agent_store.Session_store.create
          ~env
          ~sw:switch
          ~root
          ~server_id
          ~process_start_identity:(Some "test-process")
          ~lock_nonce:"daemon-first"
        |> store_ok
      in
      let handle =
        Agent_store.Session_store.create_session
          store
          ~sw:switch
          ~transaction_id
          ~actor_lock_nonce:"actor-first"
          (metadata 0L)
        |> store_ok
      in
      let indexed_before_close =
        Agent_store.Session_store.list_sessions store |> List.length
      in
      Agent_store.Session_store.close_session store handle |> store_ok;
      Agent_store.Session_store.close store |> store_ok;
      let reopened =
        Agent_store.Session_store.open_existing
          ~env
          ~sw:switch
          ~root
          ~process_start_identity:(Some "test-process")
          ~lock_nonce:"daemon-second"
        |> store_ok
      in
      let reopened_handle =
        Agent_store.Session_store.open_session
          reopened
          ~sw:switch
          ~actor_lock_nonce:"actor-second"
          session_id
        |> store_ok
      in
      let reopened_metadata = Agent_store.Session_store.Handle.metadata reopened_handle in
      let all_layout_paths_exist =
        [ Agent_store.Session_store.Handle.snapshot_directory reopened_handle
        ; Agent_store.Session_store.Handle.journal_directory reopened_handle
        ; Agent_store.Session_store.Handle.cache_directory reopened_handle
        ; Agent_store.Session_store.Handle.workspace_directory reopened_handle
        ; Agent_store.Session_store.Handle.responses_directory reopened_handle
        ; Agent_store.Session_store.Handle.audit_directory reopened_handle
        ; Agent_store.Session_store.Handle.exports_directory reopened_handle
        ; Agent_store.Session_store.Handle.archive_directory reopened_handle
        ; Agent_store.Session_store.Handle.idempotency_directory reopened_handle
        ]
        |> List.for_all ~f:(directory_exists env)
      in
      Agent_store.Session_store.close_session reopened reopened_handle |> store_ok;
      Agent_store.Session_store.close reopened |> store_ok;
      print_s
        [%sexp
          { indexed_before_close : int
          ; reopened_revision = (reopened_metadata.session.revision : int64)
          ; all_layout_paths_exist : bool
          }]));
  [%expect
    {|
    ((indexed_before_close 1) (reopened_revision 0)
     (all_layout_paths_exist true))
    |}]
;;

let%expect_test "session response retention uses Eio and never follows symlinks" =
  with_temp_directory "ochat-agent-response-retention" (fun env temporary ->
    let root = Filename.concat temporary "data" in
    Eio.Switch.run (fun switch ->
      let store =
        Agent_store.Session_store.create
          ~env
          ~sw:switch
          ~root
          ~server_id
          ~process_start_identity:(Some "test-process")
          ~lock_nonce:"response-retention"
        |> store_ok
      in
      let handle =
        Agent_store.Session_store.create_session
          store
          ~sw:switch
          ~transaction_id
          ~actor_lock_nonce:"response-retention-actor"
          (metadata 0L)
        |> store_ok
      in
      let responses = Agent_store.Session_store.Handle.responses_directory handle in
      let nested = Filename.concat responses "nested" in
      Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / nested);
      let artifact = Filename.concat nested "response.json" in
      Eio.Path.save
        ~create:(`Exclusive 0o600)
        Eio.Path.(Eio.Stdenv.fs env / artifact)
        "{}";
      let old_cutoff =
        Agent_protocol.Timestamp.of_string "2000-01-01T00:00:00Z" |> protocol_ok
      in
      let retained =
        Agent_store.Session_store.prune_response_artifacts
          store
          ~protected:[]
          ~older_than:old_cutoff
        |> store_ok
      in
      let future_cutoff =
        Agent_protocol.Timestamp.of_string "2100-01-01T00:00:00Z" |> protocol_ok
      in
      let protected =
        Agent_store.Session_store.prune_response_artifacts
          store
          ~protected:[ session_id ]
          ~older_than:future_cutoff
        |> store_ok
      in
      let protected_artifact_exists = path_exists env artifact in
      let removed =
        Agent_store.Session_store.prune_response_artifacts
          store
          ~protected:[]
          ~older_than:future_cutoff
        |> store_ok
      in
      let artifact_exists = path_exists env artifact in
      Agent_store.Session_store.close_session store handle |> store_ok;
      Agent_store.Session_store.close store |> store_ok;
      print_s
        [%sexp
          { retained : int
          ; protected : int
          ; protected_artifact_exists : bool
          ; removed : int
          ; artifact_exists : bool
          }]));
  [%expect
    {|
    ((retained 0) (protected 0) (protected_artifact_exists true) (removed 1)
     (artifact_exists false))
    |}]
;;

let principal_id =
  Agent_protocol.Id.Principal.of_string "pri_agent_store_test" |> protocol_ok
;;

let blob_id = Agent_protocol.Id.Blob.of_string "blb_agent_store_test" |> protocol_ok

let oversized_blob_id =
  Agent_protocol.Id.Blob.of_string "blb_agent_store_oversized" |> protocol_ok
;;

let%expect_test "blob uploads stream through Eio, verify digests, and enforce limits" =
  with_temp_directory "ochat-agent-blob-store" (fun env temporary ->
    let temporary_directory = Filename.concat temporary "temporary" in
    let durable_directory = Filename.concat temporary "durable" in
    let store =
      Agent_store.Blob_store.create
        ~env
        ~temporary_directory
        ~durable_directory
        ~max_upload_bytes:5L
      |> store_ok
    in
    Eio.Switch.run (fun switch ->
      let upload =
        Agent_store.Blob_store.begin_upload
          store
          ~sw:switch
          ~id:blob_id
          ~creating_principal:principal_id
          ~target_session:None
          ~kind:File
          ~media_type:"text/plain"
          ~display_name:(Some "hello.txt")
          ~allowed_use:"session.message"
          ~created_at:timestamp
          ~expires_at:None
        |> store_ok
      in
      Agent_store.Blob_store.write_string upload "hello" |> store_ok;
      let expected_digest = Digestif.SHA256.(digest_string "hello" |> to_hex) in
      let handle =
        Agent_store.Blob_store.finish upload ~expected_digest:(Some expected_digest)
        |> store_ok
      in
      let loaded = Agent_store.Blob_store.load store handle |> store_ok in
      let oversized =
        Agent_store.Blob_store.begin_upload
          store
          ~sw:switch
          ~id:oversized_blob_id
          ~creating_principal:principal_id
          ~target_session:None
          ~kind:Binary
          ~media_type:"application/octet-stream"
          ~display_name:None
          ~allowed_use:"session.message"
          ~created_at:timestamp
          ~expires_at:None
        |> store_ok
      in
      let limit_enforced =
        Agent_store.Blob_store.write_string oversized "123456" |> Result.is_error
      in
      let metadata = Agent_store.Blob_store.Handle.metadata handle in
      print_s
        [%sexp
          { loaded : string
          ; byte_length = (metadata.blob.byte_length : int64)
          ; digest_matches = (String.equal metadata.blob.digest expected_digest : bool)
          ; limit_enforced : bool
          }]));
  [%expect
    {| ((loaded hello) (byte_length 5) (digest_matches true) (limit_enforced true)) |}]
;;

let transaction ~sequence ~previous_hash ~revision ~delta =
  Agent_store.Transaction.create
    ~session_id
    ~generation:0
    ~transaction_sequence:sequence
    ~previous_transaction_hash:previous_hash
    ~session_revision:revision
    ~first_event_sequence:(Some revision)
    ~last_event_sequence:(Some revision)
    ~accepted_at_ns:revision
    ~command_audit:(Some ("command-" ^ delta))
    ~delta
    ~durable_events:[ "event-" ^ delta ]
  |> store_ok
;;

let%expect_test "commit writer and recovery enforce hash chains and repair crash tails" =
  with_temp_directory "ochat-agent-recovery" (fun env temporary ->
    let journal_directory = Filename.concat temporary "journal" in
    let snapshot_directory = Filename.concat temporary "snapshot" in
    let journal =
      Agent_store.Journal.create
        ~env
        ~directory:journal_directory
        ~max_payload_length:16384
        ~max_segment_bytes:1048576L
        ~max_segment_frames:100
      |> store_ok
    in
    Eio.Switch.run (fun switch ->
      let writer =
        Agent_store.Commit_writer.create
          ~sw:switch
          ~journal
          ~session_id
          ~next_transaction_sequence:1L
          ~previous_transaction_hash:None
          ~queue_capacity:8
        |> store_ok
      in
      let first =
        transaction ~sequence:1L ~previous_hash:None ~revision:1L ~delta:"one"
      in
      let first_commit =
        Agent_store.Commit_writer.commit writer ~durability:Flush first |> store_ok
      in
      let second =
        transaction
          ~sequence:2L
          ~previous_hash:(Some first_commit.transaction_hash)
          ~revision:2L
          ~delta:"two"
      in
      Agent_store.Commit_writer.commit writer ~durability:Flush second
      |> store_ok
      |> ignore;
      Agent_store.Commit_writer.close writer;
      Agent_store.Snapshot.install
        ~env
        ~directory:snapshot_directory
        ~max_payload_length:16384
        Agent_store.Snapshot.
          { schema_version = 1
          ; transaction_sequence = 1L
          ; transaction_hash = Some first_commit.transaction_hash
          ; event_sequence = 1L
          ; created_at = timestamp
          ; prompt_artifact = "prompt-artifact"
          ; workspace_identity = "workspace-instance"
          ; payload = "one"
          }
      |> store_ok
      |> ignore;
      let segment =
        Agent_store.Journal_segment.open_existing
          ~env
          ~directory:journal_directory
          ~id:(Agent_store.Journal.current_segment journal)
        |> store_ok
      in
      let partial =
        Agent_store.Frame.encode ~max_payload_length:16384 ~flags:0 "partial"
        |> frame_ok
        |> Fn.flip String.drop_suffix 5
      in
      Agent_store.Journal_segment.append ~env ~durability:Buffered segment ~frame:partial
      |> store_ok
      |> ignore;
      let recovered =
        Agent_store.Recovery.load
          ~env
          ~journal
          ~snapshot_directory
          ~max_snapshot_payload_length:16384
          ~session_id
          ~initial:""
          ~restore_snapshot:(fun payload -> Ok payload)
          ~apply:(fun state transaction -> Ok (state ^ "+" ^ transaction.delta))
          ~validate:(fun state ->
            if String.equal state "one+two"
            then Ok ()
            else Error (Agent_store.Store_error.Corrupt "unexpected recovered state"))
        |> store_ok
      in
      print_s
        [%sexp
          { state = (recovered.state : string)
          ; transaction_sequence = (recovered.latest_transaction_sequence : int64)
          ; event_sequence = (recovered.latest_event_sequence : int64)
          ; repaired_crash_tail = (recovered.repaired_crash_tail : bool)
          }]));
  [%expect
    {|
    ((state one+two) (transaction_sequence 2) (event_sequence 2)
     (repaired_crash_tail true))
    |}]
;;

let%expect_test "snapshot-anchored journal pruning remains restart recoverable" =
  with_temp_directory "ochat-agent-journal-prune" (fun env temporary ->
    let journal_directory = Filename.concat temporary "journal" in
    let snapshot_directory = Filename.concat temporary "snapshot" in
    let journal =
      Agent_store.Journal.create
        ~env
        ~directory:journal_directory
        ~max_payload_length:16384
        ~max_segment_bytes:1048576L
        ~max_segment_frames:1
      |> store_ok
    in
    Eio.Switch.run (fun switch ->
      let writer =
        Agent_store.Commit_writer.create
          ~sw:switch
          ~journal
          ~session_id
          ~next_transaction_sequence:1L
          ~previous_transaction_hash:None
          ~queue_capacity:8
        |> store_ok
      in
      let first =
        transaction ~sequence:1L ~previous_hash:None ~revision:1L ~delta:"one"
      in
      let first =
        Agent_store.Commit_writer.commit writer ~durability:Flush first |> store_ok
      in
      let second =
        transaction
          ~sequence:2L
          ~previous_hash:(Some first.transaction_hash)
          ~revision:2L
          ~delta:"two"
      in
      let second =
        Agent_store.Commit_writer.commit writer ~durability:Flush second |> store_ok
      in
      let third =
        transaction
          ~sequence:3L
          ~previous_hash:(Some second.transaction_hash)
          ~revision:3L
          ~delta:"three"
      in
      Agent_store.Commit_writer.commit writer ~durability:Flush third
      |> store_ok
      |> ignore;
      Agent_store.Commit_writer.close writer;
      Agent_store.Snapshot.install
        ~env
        ~directory:snapshot_directory
        ~max_payload_length:16384
        Agent_store.Snapshot.
          { schema_version = 1
          ; transaction_sequence = 2L
          ; transaction_hash = Some second.transaction_hash
          ; event_sequence = 2L
          ; created_at = timestamp
          ; prompt_artifact = "prompt-artifact"
          ; workspace_identity = "workspace-instance"
          ; payload = "one+two"
          }
      |> store_ok
      |> ignore;
      let removed =
        Agent_store.Journal.prune_before_transaction journal ~transaction_sequence:2L
        |> store_ok
      in
      let recovered =
        Agent_store.Recovery.load
          ~env
          ~journal
          ~snapshot_directory
          ~max_snapshot_payload_length:16384
          ~session_id
          ~initial:""
          ~restore_snapshot:(fun payload -> Ok payload)
          ~apply:(fun state transaction -> Ok (state ^ "+" ^ transaction.delta))
          ~validate:(fun state ->
            if String.equal state "one+two+three"
            then Ok ()
            else Error (Agent_store.Store_error.Corrupt "unexpected pruned state"))
        |> store_ok
      in
      let retained_segments =
        Eio.Path.read_dir Eio.Path.(Eio.Stdenv.fs env / journal_directory)
        |> List.count ~f:(String.is_suffix ~suffix:".log")
      in
      print_s
        [%sexp
          { removed : int
          ; retained_segments : int
          ; state = (recovered.state : string)
          ; transaction_sequence = (recovered.latest_transaction_sequence : int64)
          ; retained_transactions = (List.length recovered.transactions : int)
          }]));
  [%expect
    {|
    ((removed 1) (retained_segments 3) (state one+two+three)
     (transaction_sequence 3) (retained_transactions 2))
    |}]
;;

let prompt_revision_id =
  Agent_protocol.Id.Prompt_revision.of_string "prv_agent_store_test" |> protocol_ok
;;

let%expect_test "checkpoint sealing bounds journals and keeps incomplete-current fallback"
  =
  with_temp_directory "ochat-checkpoint-sealing" (fun env temporary ->
    let journal_directory = Filename.concat temporary "journal" in
    let snapshot_directory = Filename.concat temporary "snapshot" in
    let journal =
      Agent_store.Journal.create
        ~env
        ~directory:journal_directory
        ~max_payload_length:16384
        ~max_segment_bytes:67108864L
        ~max_segment_frames:10000
      |> store_ok
    in
    Eio.Switch.run (fun sw ->
      let writer =
        Agent_store.Commit_writer.create
          ~sw
          ~journal
          ~session_id
          ~next_transaction_sequence:1L
          ~previous_transaction_hash:None
          ~queue_capacity:8
        |> store_ok
      in
      let hash = ref None in
      for index = 1 to 8 do
        let sequence = Int64.of_int index in
        let tx =
          transaction
            ~sequence
            ~previous_hash:!hash
            ~revision:sequence
            ~delta:(Int.to_string index)
        in
        let committed =
          Agent_store.Commit_writer.commit writer ~durability:Flush tx |> store_ok
        in
        hash := Some committed.transaction_hash;
        let checkpoint =
          { (snapshot sequence (Int.to_string index)) with
            transaction_hash = !hash
          ; event_sequence = sequence
          }
        in
        ignore
          (Agent_store.Snapshot.install
             ~env
             ~directory:snapshot_directory
             ~max_payload_length:16384
             checkpoint
           |> store_ok
           : Agent_store.Snapshot.installed);
        ignore
          (Agent_store.Snapshot.prune_older ~env ~directory:snapshot_directory ~keep:2
           |> store_ok
           : int);
        let floor =
          Agent_store.Snapshot.retention_floor
            ~env
            ~directory:snapshot_directory
            ~max_payload_length:16384
          |> store_ok
        in
        ignore
          (Agent_store.Journal.prune_before_transaction
             journal
             ~transaction_sequence:floor
           |> store_ok
           : int);
        Agent_store.Journal.seal_checkpoint journal |> store_ok;
        let segment = Agent_store.Journal.current_segment journal in
        Agent_store.Journal.seal_checkpoint journal |> store_ok;
        assert (
          Agent_store.Journal_segment.Id.equal
            segment
            (Agent_store.Journal.current_segment journal))
      done;
      Agent_store.Commit_writer.close writer);
    let current =
      Eio.Path.(Eio.Stdenv.fs env / snapshot_directory / "snapshot-0000000000000008.bin")
    in
    Eio.Path.save ~create:(`Or_truncate 0o600) current "short";
    let recovered =
      Agent_store.Recovery.load
        ~env
        ~journal
        ~snapshot_directory
        ~max_snapshot_payload_length:16384
        ~session_id
        ~initial:0
        ~restore_snapshot:(fun payload -> Ok (Int.of_string payload))
        ~apply:(fun _ tx -> Ok (Int.of_string tx.Agent_store.Transaction.delta))
        ~validate:(fun value ->
          assert (value = 8);
          Ok ())
      |> store_ok
    in
    let segments =
      Eio.Path.read_dir Eio.Path.(Eio.Stdenv.fs env / journal_directory)
      |> List.count ~f:(String.is_suffix ~suffix:".log")
    in
    print_s
      [%sexp
        { segments : int
        ; state = (recovered.state : int)
        ; fallback =
            ((Option.value_exn recovered.snapshot).snapshot.transaction_sequence : int64)
        ; transactions = (List.length recovered.transactions : int)
        }]);
  [%expect {| ((segments 3) (state 8) (fallback 7) (transactions 2)) |}]
;;

let%expect_test "prompt revision artifacts are immutable and content verified" =
  with_temp_directory "ochat-agent-prompt-artifact" (fun env temporary ->
    let root = Filename.concat temporary "artifacts" in
    let store = Agent_store.Prompt_artifact_store.create ~env ~root |> store_ok in
    let source =
      Agent_store.Prompt_artifact_store.Source.create
        ~relative_path:"library/tools.chatmd"
        ~contents:"tool source"
      |> store_ok
    in
    let artifact =
      Agent_store.Prompt_artifact_store.Artifact.create
        ~revision_id:prompt_revision_id
        ~root_chatmd:"root prompt"
        ~sources:[ source ]
        ~parser_schema_version:1
        ~runtime_schema_version:1
        ~created_at:timestamp
        ()
      |> store_ok
    in
    Agent_store.Prompt_artifact_store.install store ~transaction_id artifact |> store_ok;
    let loaded =
      Agent_store.Prompt_artifact_store.load store prompt_revision_id |> store_ok
    in
    let source_path =
      Filename.concat
        root
        (Filename.concat
           (Agent_protocol.Id.Prompt_revision.to_string prompt_revision_id)
           "sources/library/tools.chatmd")
    in
    Eio.Path.unlink Eio.Path.(Eio.Stdenv.fs env / source_path);
    Eio.Path.save
      ~create:(`Exclusive 0o600)
      Eio.Path.(Eio.Stdenv.fs env / source_path)
      "tampered";
    let tamper_detected =
      Agent_store.Prompt_artifact_store.load store prompt_revision_id |> Result.is_error
    in
    print_s
      [%sexp
        { root_chatmd = (loaded.root_chatmd : string)
        ; source_count = (List.length loaded.sources : int)
        ; tamper_detected : bool
        }]);
  [%expect
    {|
    ((root_chatmd "root prompt") (source_count 1) (tamper_detected true))
    |}]
;;

let%test_unit "materialized prompt trees reject changed missing extra and linked files" =
  with_temp_directory "ochat-tree-integrity" (fun env temporary ->
    let module Store = Agent_store.Prompt_artifact_store in
    let store =
      Store.create ~env ~root:(Filename.concat temporary "artifacts") |> store_ok
    in
    let artifact =
      Store.Artifact.create
        ~revision_id:prompt_revision_id
        ~root_chatmd:"root"
        ~sources:
          [ Store.Source.create ~relative_path:"sub/import.chatmd" ~contents:"original"
            |> store_ok
          ]
        ~parser_schema_version:1
        ~runtime_schema_version:1
        ~created_at:(Agent_protocol.Timestamp.now ())
        ()
      |> store_ok
    in
    Store.install store ~transaction_id artifact |> store_ok;
    let tree = Store.materialized_tree store prompt_revision_id in
    let file = Eio.Path.(tree / "sub/import.chatmd") in
    assert ((Eio.Path.stat ~follow:false file).perm land 0o777 = 0o400);
    let check () = assert (Result.is_error (Store.load store prompt_revision_id)) in
    Eio.Path.unlink file;
    check ();
    Eio.Path.save ~create:(`Exclusive 0o600) file "changed";
    check ();
    Eio.Path.unlink file;
    Eio.Path.save ~create:(`Exclusive 0o400) file "original";
    ignore (Store.load store prompt_revision_id |> store_ok : Store.Artifact.t);
    let extra = Eio.Path.(tree / "extra") in
    Eio.Path.save ~create:(`Exclusive 0o600) extra "unexpected";
    check ();
    Eio.Path.unlink extra;
    Eio.Path.unlink file;
    Eio.Path.symlink ~link_to:"../root.chatmd" file;
    check ();
    Eio.Path.unlink file;
    Eio.Path.rmdir Eio.Path.(tree / "sub");
    Eio.Path.symlink ~link_to:temporary Eio.Path.(tree / "sub");
    check ())
;;

let%expect_test "prompt artifact pruning preserves referenced revisions" =
  with_temp_directory "ochat-agent-prompt-artifact-prune" (fun env temporary ->
    let root = Filename.concat temporary "artifacts" in
    let store = Agent_store.Prompt_artifact_store.create ~env ~root |> store_ok in
    let retained_revision =
      Agent_protocol.Id.Prompt_revision.of_string "prv_retained" |> protocol_ok
    in
    let removed_revision =
      Agent_protocol.Id.Prompt_revision.of_string "prv_removed" |> protocol_ok
    in
    let install revision_id transaction_id root_chatmd =
      let artifact =
        Agent_store.Prompt_artifact_store.Artifact.create
          ~revision_id
          ~root_chatmd
          ~sources:[]
          ~parser_schema_version:1
          ~runtime_schema_version:1
          ~created_at:timestamp
          ()
        |> store_ok
      in
      Agent_store.Prompt_artifact_store.install store ~transaction_id artifact |> store_ok
    in
    install
      retained_revision
      (Agent_protocol.Id.Transaction.of_string "txn_retain" |> protocol_ok)
      "retained";
    install
      removed_revision
      (Agent_protocol.Id.Transaction.of_string "txn_remove" |> protocol_ok)
      "removed";
    let staging = Filename.concat root ".staging-incomplete" in
    Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / staging);
    let removed =
      Agent_store.Prompt_artifact_store.prune_unreferenced
        store
        ~protected:[ retained_revision ]
      |> store_ok
    in
    print_s
      [%sexp
        { removed : int
        ; retained =
            (Agent_store.Prompt_artifact_store.exists store retained_revision : bool)
        ; unreferenced =
            (Agent_store.Prompt_artifact_store.exists store removed_revision : bool)
        ; staging = (directory_exists env staging : bool)
        }]);
  [%expect
    {|
    ((removed 1) (retained true) (unreferenced false) (staging true))
    |}]
;;

let idempotency_key =
  Agent_protocol.Idempotency_key.of_string "agent-store:test:1" |> protocol_ok
;;

let%expect_test "idempotency records replay matching requests and reject conflicts" =
  with_temp_directory "ochat-agent-idempotency" (fun env temporary ->
    let path = Filename.concat temporary "idempotency.sexp" in
    let store = Agent_store.Idempotency_store.open_or_create ~env ~path |> store_ok in
    let key =
      Agent_store.Idempotency_store.Key.
        { principal_id
        ; session_id = Some session_id
        ; method_name = "session.start"
        ; idempotency_key
        }
    in
    let record =
      Agent_store.Idempotency_store.
        { key
        ; request_digest = "request-a"
        ; accepted_transaction_sequence = Some 4L
        ; outcome = Success (`Object [ "started", `True ])
        ; created_at = timestamp
        ; expires_at = None
        ; retention = Standard
        }
    in
    Agent_store.Idempotency_store.record store record |> store_ok |> ignore;
    let replay =
      match
        Agent_store.Idempotency_store.lookup store ~key ~request_digest:"request-a"
      with
      | Replay _ -> true
      | Missing | Conflict _ -> false
    in
    let conflict =
      match
        Agent_store.Idempotency_store.lookup store ~key ~request_digest:"request-b"
      with
      | Conflict _ -> true
      | Missing | Replay _ -> false
    in
    let reopened = Agent_store.Idempotency_store.open_or_create ~env ~path |> store_ok in
    let survived_reopen =
      match
        Agent_store.Idempotency_store.lookup reopened ~key ~request_digest:"request-a"
      with
      | Replay _ -> true
      | Missing | Conflict _ -> false
    in
    print_s [%sexp { replay : bool; conflict : bool; survived_reopen : bool }]);
  [%expect {| ((replay true) (conflict true) (survived_reopen true)) |}]
;;

let large_idempotency_outcome =
  `Object
    [ ( "items"
      , `Array
          (List.init 256 ~f:(fun index ->
             `Object
               [ "index", `Number (Int.to_string index)
               ; "text", `String (String.make 256 'x' ^ "\n\"\\\tλ")
               ; "nested", `Array [ `Null; `True; `False; `Object [] ]
               ])) )
    ]
;;

let idempotency_large_record method_name =
  Agent_store.Idempotency_store.
    { key = { principal_id; session_id = Some session_id; method_name; idempotency_key }
    ; request_digest = "large-request"
    ; accepted_transaction_sequence = Some 42L
    ; outcome = Success large_idempotency_outcome
    ; created_at = timestamp
    ; expires_at = None
    ; retention = Protected
    }
;;

let is_large_idempotency_replay store record =
  match
    Agent_store.Idempotency_store.lookup
      store
      ~key:record.Agent_store.Idempotency_store.key
      ~request_digest:record.request_digest
  with
  | Replay replay -> Poly.equal replay record
  | Missing | Conflict _ -> false
;;

let%expect_test
    "encoded idempotency outcomes preserve large JSON across writes and reopen"
  =
  with_temp_directory "ochat-agent-idempotency-large" (fun env temporary ->
    let path = Filename.concat temporary "idempotency.sexp" in
    let store = Agent_store.Idempotency_store.open_or_create ~env ~path |> store_ok in
    let record = idempotency_large_record "large-original" in
    ignore
      (Agent_store.Idempotency_store.record store record |> store_ok
       : Agent_store.Idempotency_store.record);
    List.iter [ "large-next-1"; "large-next-2"; "large-next-3" ] ~f:(fun name ->
      ignore
        (Agent_store.Idempotency_store.record store (idempotency_large_record name)
         |> store_ok
         : Agent_store.Idempotency_store.record));
    let preserved_after_writes = is_large_idempotency_replay store record in
    let reopened = Agent_store.Idempotency_store.open_or_create ~env ~path |> store_ok in
    let preserved_after_reopen = is_large_idempotency_replay reopened record in
    print_s [%sexp { preserved_after_writes : bool; preserved_after_reopen : bool }]);
  [%expect {| ((preserved_after_writes true) (preserved_after_reopen true)) |}]
;;

let%expect_test "pending idempotency receipts survive restart and complete once" =
  with_temp_directory "ochat-agent-idempotency-pending" (fun env temporary ->
    let path = Filename.concat temporary "idempotency.sexp" in
    let key =
      Agent_store.Idempotency_store.Key.
        { principal_id
        ; session_id = Some session_id
        ; method_name = "session.send_message"
        ; idempotency_key
        }
    in
    let store = Agent_store.Idempotency_store.open_or_create ~env ~path |> store_ok in
    Agent_store.Idempotency_store.record
      store
      { key
      ; request_digest = "request-pending"
      ; accepted_transaction_sequence = None
      ; outcome = Pending
      ; created_at = timestamp
      ; expires_at = None
      ; retention = Protected
      }
    |> store_ok
    |> ignore;
    let reopened = Agent_store.Idempotency_store.open_or_create ~env ~path |> store_ok in
    let pending =
      match
        Agent_store.Idempotency_store.lookup
          reopened
          ~key
          ~request_digest:"request-pending"
      with
      | Replay { outcome = Pending; _ } -> true
      | Missing | Conflict _ | Replay _ -> false
    in
    Agent_store.Idempotency_store.mark_accepted
      reopened
      ~key
      ~request_digest:"request-pending"
      ~transaction_sequence:9L
    |> store_ok
    |> ignore;
    let completed =
      Agent_store.Idempotency_store.complete
        reopened
        ~key
        ~request_digest:"request-pending"
        ~accepted_transaction_sequence:None
        ~outcome:(Success (`Object [ "accepted", `True ]))
      |> store_ok
    in
    let repeated =
      Agent_store.Idempotency_store.complete
        reopened
        ~key
        ~request_digest:"request-pending"
        ~accepted_transaction_sequence:(Some 10L)
        ~outcome:(Failure (Agent_protocol.Error.invalid_request "late"))
      |> store_ok
    in
    print_s
      [%sexp
        { pending : bool
        ; completed_sequence = (completed.accepted_transaction_sequence : int64 option)
        ; repeated_sequence = (repeated.accepted_transaction_sequence : int64 option)
        ; repeated_kept_success =
            ((match repeated.outcome with
              | Success _ -> true
              | Pending | Failure _ -> false)
             : bool)
        }]);
  [%expect
    {|
    ((pending true) (completed_sequence (9)) (repeated_sequence (9))
     (repeated_kept_success true)) |}]
;;

let%expect_test "journal command audits reconcile accepted receipts safely" =
  with_temp_directory "ochat-agent-idempotency-reconcile" (fun env temporary ->
    let path = Filename.concat temporary "idempotency.sexp" in
    let store = Agent_store.Idempotency_store.open_or_create ~env ~path |> store_ok in
    let key method_name =
      Agent_store.Idempotency_store.Key.
        { principal_id; session_id = Some session_id; method_name; idempotency_key }
    in
    let protected_key = key "session.reset" in
    Agent_store.Idempotency_store.record
      store
      { key = protected_key
      ; request_digest = "protected-request"
      ; accepted_transaction_sequence = None
      ; outcome = Pending
      ; created_at = timestamp
      ; expires_at = None
      ; retention = Protected
      }
    |> store_ok
    |> ignore;
    let changed =
      Agent_store.Idempotency_store.reconcile_accepted
        store
        [ ( { key = protected_key
            ; request_digest = "protected-request"
            ; protected_record = true
            }
          , 12L )
        ; ( { key = key "session.start"
            ; request_digest = "expired-standard-request"
            ; protected_record = false
            }
          , 13L )
        ]
      |> store_ok
    in
    let accepted_sequence =
      match
        Agent_store.Idempotency_store.lookup
          store
          ~key:protected_key
          ~request_digest:"protected-request"
      with
      | Replay record -> record.accepted_transaction_sequence
      | Missing | Conflict _ -> None
    in
    let missing_protected_failed =
      Agent_store.Idempotency_store.reconcile_accepted
        store
        [ ( { key = key "session.stop"
            ; request_digest = "missing-protected-request"
            ; protected_record = true
            }
          , 14L )
        ]
      |> Result.is_error
    in
    print_s
      [%sexp
        { changed : int
        ; accepted_sequence : int64 option
        ; missing_protected_failed : bool
        }]);
  [%expect
    {|
    ((changed 1) (accepted_sequence (12)) (missing_protected_failed true))
    |}]
;;

let%expect_test "audit records survive reopen and cursors reject tampering" =
  with_temp_directory "ochat-agent-audit" (fun env temporary ->
    let directory = Filename.concat temporary "audit" in
    let open_store () =
      Agent_store.Audit_store.open_or_create ~env ~directory ~max_payload_length:16_384
      |> store_ok
    in
    let store = open_store () in
    let append name level =
      Agent_store.Audit_store.append
        store
        ~timestamp
        ~level
        ~name
        ~session_id:(Some session_id)
        ~principal_id:(Some principal_id)
        ~payload:(`Object [ "safe", `True ])
        ~redacted:true
      |> store_ok
      |> ignore
    in
    append "command.first" Info;
    append "command.second" Warning;
    append "other.third" Error;
    let page limit cursor name_prefix =
      let page = Agent_protocol.Page.Request.create ~limit ?cursor () |> protocol_ok in
      Agent_store.Audit_store.read
        store
        Agent_protocol.Audit.Read_request.
          { page
          ; session_id = Some session_id
          ; principal_id = None
          ; minimum_level = None
          ; name_prefix
          }
      |> store_ok
    in
    let first = page 1 None (Some "command.") in
    let cursor = Option.value_exn first.next_cursor in
    let reopened = open_store () in
    let second =
      Agent_store.Audit_store.read
        reopened
        { page = Agent_protocol.Page.Request.create ~limit:2 ~cursor () |> protocol_ok
        ; session_id = Some session_id
        ; principal_id = None
        ; minimum_level = None
        ; name_prefix = Some "command."
        }
      |> store_ok
    in
    let tampered =
      Agent_protocol.Page.Cursor.to_string cursor ^ "x"
      |> Agent_protocol.Page.Cursor.of_string
      |> protocol_ok
    in
    let rejected =
      Agent_store.Audit_store.read
        reopened
        { page =
            Agent_protocol.Page.Request.create ~limit:1 ~cursor:tampered () |> protocol_ok
        ; session_id = None
        ; principal_id = None
        ; minimum_level = None
        ; name_prefix = None
        }
      |> Result.is_error
    in
    print_s
      [%sexp
        { first = (List.map first.items ~f:(fun item -> item.name) : string list)
        ; second = (List.map second.items ~f:(fun item -> item.name) : string list)
        ; tampered_rejected = (rejected : bool)
        }]);
  [%expect
    {|
    ((first (command.first)) (second (command.second)) (tampered_rejected true))
    |}]
;;

let%expect_test "migration framework validates V1 without mutating the store" =
  with_temp_directory "ochat-agent-migration" (fun env temporary ->
    let root = Filename.concat temporary "data" in
    Eio.Switch.run (fun switch ->
      let store =
        Agent_store.Session_store.create
          ~env
          ~sw:switch
          ~root
          ~server_id
          ~process_start_identity:(Some "test-process")
          ~lock_nonce:"migration-create"
        |> store_ok
      in
      Agent_store.Session_store.close store |> store_ok;
      let plan =
        Agent_store.Migration.run
          ~env
          ~sw:switch
          ~root
          ~server_id
          ~process_start_identity:(Some "test-process")
          ~lock_nonce:"migration-validate"
          ~mode:Validate_only
        |> store_ok
      in
      print_s
        [%sexp
          { source_version = (plan.source_version : int)
          ; target_version = (plan.target_version : int)
          ; session_count = (plan.session_count : int)
          ; status = (plan.status : Agent_store.Migration.status)
          }]));
  [%expect
    {|
    ((source_version 1) (target_version 1) (session_count 0) (status Current))
    |}]
;;

let%expect_test "malformed migration schemas return typed errors and release the lock" =
  with_temp_directory "ochat-agent-corrupt-migration" (fun env root ->
    Eio.Switch.run (fun sw ->
      let store =
        Agent_store.Session_store.create
          ~env
          ~sw
          ~root
          ~server_id
          ~process_start_identity:None
          ~lock_nonce:"create-corrupt-test"
        |> store_ok
      in
      Agent_store.Session_store.close store |> store_ok;
      let path = Eio.Path.(Eio.Stdenv.fs env / root / "schema.sexp") in
      List.iter [ "("; ""; "not-a-record"; "((version invalid))" ] ~f:(fun malformed ->
        Eio.Path.save ~create:(`Or_truncate 0o600) path malformed;
        List.iter
          Agent_store.Migration.[ Validate_only; Dry_run; Apply ]
          ~f:(fun mode ->
            let result =
              Agent_store.Migration.run
                ~env
                ~sw
                ~root
                ~server_id
                ~process_start_identity:None
                ~lock_nonce:"inspect-corrupt-test"
                ~mode
            in
            (match result with
             | Error (Corrupt _) -> ()
             | _ -> failwith "expected typed corruption");
            assert (String.equal (Eio.Path.load path) malformed)));
      print_endline "12 typed corruption results; schema unchanged; lock reacquired"));
  [%expect {| 12 typed corruption results; schema unchanged; lock reacquired |}]
;;

let%expect_test "migration dry-run reports an older schema without applying it" =
  with_temp_directory "ochat-agent-migration-plan" (fun env temporary ->
    let root = Filename.concat temporary "data" in
    Eio.Switch.run (fun switch ->
      let store =
        Agent_store.Session_store.create
          ~env
          ~sw:switch
          ~root
          ~server_id
          ~process_start_identity:(Some "test-process")
          ~lock_nonce:"migration-create"
        |> store_ok
      in
      Agent_store.Session_store.close store |> store_ok;
      let schema_path = Filename.concat root "schema.sexp" in
      let schema = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / schema_path) in
      Eio.Path.save
        ~create:(`Or_truncate 0o600)
        Eio.Path.(Eio.Stdenv.fs env / schema_path)
        (String.substr_replace_first schema ~pattern:"(version 1)" ~with_:"(version 0)");
      let plan =
        Agent_store.Migration.run
          ~env
          ~sw:switch
          ~root
          ~server_id
          ~process_start_identity:(Some "test-process")
          ~lock_nonce:"migration-dry-run"
          ~mode:Dry_run
        |> store_ok
      in
      let apply_rejected =
        Agent_store.Migration.run
          ~env
          ~sw:switch
          ~root
          ~server_id
          ~process_start_identity:(Some "test-process")
          ~lock_nonce:"migration-apply"
          ~mode:Apply
        |> Result.is_error
      in
      print_s
        [%sexp
          { source_version = (plan.source_version : int)
          ; status = (plan.status : Agent_store.Migration.status)
          ; apply_rejected : bool
          }]));
  [%expect
    {|
    ((source_version 0) (status Migration_required) (apply_rejected true))
    |}]
;;
