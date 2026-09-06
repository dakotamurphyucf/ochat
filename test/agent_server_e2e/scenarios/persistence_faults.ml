open Core
module F = Crash_recovery_fixture
module Fault = Support.Crash_fault_io

let directory env environment name =
  let roots = Support.Temporary_environment.roots environment in
  let path = Filename.concat roots.temporary name in
  Eio.Path.mkdir ~perm:0o700 (F.path env path);
  path
;;

let fail_io path = raise (Core_unix.Unix_error (EIO, "injected persistence fault", path))
let fail_full path = raise (Core_unix.Unix_error (ENOSPC, "injected write fault", path))

let assert_io_failure = function
  | Error (Agent_store.Store_error.Io _) -> ()
  | Error error ->
    raise_s [%sexp "wrong persistence failure", (error : Agent_store.Store_error.t)]
  | Ok _ -> F.fail "injected persistence error was acknowledged as success"
;;

let replace env path value =
  Agent_store.Durable_file.replace ~env ~durability:Flush_file_and_directory ~path value
;;

let replacement_case env environment (name, boundary, after_rename) =
  let dir = directory env environment ("io-replace-" ^ name) in
  let target = Filename.concat dir "CURRENT" in
  replace env target "old" |> F.store_ok;
  let hit = ref false in
  let wrapped =
    Fault.wrap
      env
      ~boundary
      ~matches:(fun path ->
        String.is_prefix path ~prefix:(target ^ ".tmp-") || String.equal path dir)
      ~reached:(fun path ->
        hit := true;
        if String.equal name "disk-full" then fail_full path else fail_io path)
  in
  replace wrapped target "new" |> assert_io_failure;
  F.require !hit "fault boundary was not reached";
  F.require
    (String.equal (F.read env target) (if after_rename then "new" else "old"))
    "failed replacement exposed an invalid target";
  F.require
    (List.equal String.equal (Eio.Path.read_dir (F.path env dir)) [ "CURRENT" ])
    "failed replacement left staging files";
  replace env target "retry" |> F.store_ok;
  F.require (String.equal (F.read env target) "retry") "replacement could not recover"
;;

let replacements env environment =
  List.iter
    [ "disk-full", Fault.After_bytes 2, false
    ; "file-sync", Before_sync, false
    ; "rename", Before_rename, false
    ; "directory-sync", Before_directory_sync, true
    ]
    ~f:(replacement_case env environment)
;;

let snapshot sequence payload =
  Agent_store.Snapshot.
    { schema_version = 1
    ; transaction_sequence = sequence
    ; transaction_hash = None
    ; event_sequence = sequence
    ; created_at = Agent_protocol.Timestamp.now ()
    ; prompt_artifact = "fixture-prompt"
    ; workspace_identity = "fixture-workspace"
    ; payload
    }
;;

let install env dir value =
  Agent_store.Snapshot.install ~env ~directory:dir ~max_payload_length:4096 value
;;

let load_snapshot env dir =
  Agent_store.Snapshot.load_current ~env ~directory:dir ~max_payload_length:4096
  |> F.store_ok
  |> Option.value_exn
;;

let snapshot_case env environment (name, boundary, after_rename) =
  let dir = directory env environment ("io-snapshot-" ^ name) in
  ignore
    (install env dir (snapshot 1L "old") |> F.store_ok : Agent_store.Snapshot.installed);
  let target = Filename.concat dir "CURRENT" in
  let hit = ref false in
  let wrapped =
    Fault.wrap
      env
      ~boundary
      ~matches:(fun path ->
        String.is_prefix path ~prefix:(target ^ ".tmp-") || String.equal path dir)
      ~reached:(fun path ->
        hit := true;
        fail_io path)
  in
  install wrapped dir (snapshot 2L "new") |> assert_io_failure;
  F.require !hit "snapshot activation fault did not run";
  let recovered = (load_snapshot env dir).snapshot in
  F.require
    (Int64.equal recovered.transaction_sequence (if after_rename then 2L else 1L)
     && String.equal recovered.payload (if after_rename then "new" else "old"))
    "snapshot activation did not recover the exact committed checkpoint";
  ignore
    (install env dir (snapshot 3L "continued") |> F.store_ok
     : Agent_store.Snapshot.installed);
  F.require
    (String.equal (load_snapshot env dir).snapshot.payload "continued")
    "snapshot activation could not continue after recovery"
;;

let snapshots env environment =
  List.iter
    [ "partial-current", Fault.After_bytes 3, false
    ; "current-sync", Before_sync, false
    ; "before-activation", Before_rename, false
    ; "after-activation", After_rename, true
    ; "directory-sync", Before_directory_sync, true
    ]
    ~f:(snapshot_case env environment)
;;

let open_journal env dir =
  Agent_store.Journal.open_existing
    ~env
    ~directory:dir
    ~max_payload_length:4096
    ~max_segment_bytes:1_048_576L
    ~max_segment_frames:100
  |> F.store_ok
;;

let append journal payload =
  Agent_store.Journal.append journal ~durability:Flush ~flags:0 ~payload
;;

let journal_payloads journal =
  let scan = Agent_store.Journal.scan journal |> F.store_ok in
  List.filter_map scan.entries ~f:(fun entry ->
    if Agent_store.Frame.flags entry.frame = 0
    then Some (Agent_store.Frame.payload entry.frame)
    else None)
;;

let rotation_case env environment (name, boundary) =
  let dir = directory env environment ("io-rotation-" ^ name) in
  let journal =
    Agent_store.Journal.create
      ~env
      ~directory:dir
      ~max_payload_length:4096
      ~max_segment_bytes:1_048_576L
      ~max_segment_frames:100
    |> F.store_ok
  in
  ignore (append journal "acknowledged" |> F.store_ok : Agent_store.Journal.append_result);
  let target = Filename.concat dir "CURRENT" in
  let hit = ref false in
  let wrapped =
    Fault.wrap
      env
      ~boundary
      ~matches:(String.is_prefix ~prefix:(target ^ ".tmp-"))
      ~reached:(fun path ->
        hit := true;
        fail_io path)
  in
  Agent_store.Journal.rotate (open_journal wrapped dir) ~terminal_payload:"sealed"
  |> assert_io_failure;
  F.require !hit "rotation fault did not run";
  let recovered = open_journal env dir in
  F.require
    (List.equal String.equal (journal_payloads recovered) [ "acknowledged" ])
    "rotation lost or duplicated acknowledged frames";
  ignore (append recovered "continued" |> F.store_ok : Agent_store.Journal.append_result);
  F.require
    (List.equal
       String.equal
       (journal_payloads (open_journal env dir))
       [ "acknowledged"; "continued" ])
    "rotation recovery could not append"
;;

let rotations env environment =
  List.iter
    [ "before-current", Fault.Before_rename; "after-current", After_rename ]
    ~f:(rotation_case env environment)
;;

let transaction session_id sequence previous =
  Agent_store.Transaction.create
    ~session_id
    ~generation:0
    ~transaction_sequence:sequence
    ~previous_transaction_hash:previous
    ~session_revision:sequence
    ~first_event_sequence:None
    ~last_event_sequence:None
    ~accepted_at_ns:sequence
    ~command_audit:None
    ~delta:(Int64.to_string sequence)
    ~durable_events:[]
  |> F.store_ok
;;

let writer_failure env environment (name, boundary, complete_frame) =
  let dir = directory env environment ("io-writer-" ^ name) in
  let journal =
    Agent_store.Journal.create
      ~env
      ~directory:dir
      ~max_payload_length:4096
      ~max_segment_bytes:1_048_576L
      ~max_segment_frames:100
    |> F.store_ok
  in
  let session_id = Agent_protocol.Id.Session.create () in
  let first = transaction session_id 1L None in
  ignore
    (append journal (Agent_store.Transaction.encode first) |> F.store_ok
     : Agent_store.Journal.append_result);
  let target = Filename.concat dir "0000000000000001.log" in
  let hits = ref 0 in
  let wrapped =
    Fault.wrap env ~boundary ~matches:(String.equal target) ~reached:(fun path ->
      Int.incr hits;
      fail_full path)
  in
  Eio.Switch.run (fun sw ->
    let writer =
      Agent_store.Commit_writer.create
        ~sw
        ~journal:(open_journal wrapped dir)
        ~session_id
        ~next_transaction_sequence:2L
        ~previous_transaction_hash:(Some (Agent_store.Transaction.hash first))
        ~queue_capacity:4
      |> F.store_ok
    in
    let second = transaction session_id 2L (Some (Agent_store.Transaction.hash first)) in
    Agent_store.Commit_writer.commit writer ~durability:Flush second |> assert_io_failure;
    Agent_store.Commit_writer.commit writer ~durability:Flush second |> assert_io_failure;
    F.require (!hits = 1) "failed commit writer attempted another physical write";
    Agent_store.Commit_writer.close writer);
  let recovered =
    Agent_store.Recovery.load
      ~env
      ~journal:(open_journal env dir)
      ~snapshot_directory:(Filename.concat dir "snapshots")
      ~max_snapshot_payload_length:4096
      ~session_id
      ~initial:[]
      ~restore_snapshot:(fun _ -> assert false)
      ~apply:(fun history transaction ->
        Ok (history @ [ transaction.Agent_store.Transaction.delta ]))
      ~validate:(fun _ -> Ok ())
    |> F.store_ok
  in
  F.require
    (List.equal
       String.equal
       recovered.state
       (if complete_frame then [ "1"; "2" ] else [ "1" ]))
    "commit failure recovery lost acknowledged state or invented partial state"
;;

let writers env environment =
  List.iter
    [ "partial-write", Fault.After_bytes 3, false; "file-sync", Before_sync, true ]
    ~f:(writer_failure env environment)
;;
