open Core
open Agent_store_test_fixtures
module R = Agent_store.Retention_reader
module Snapshot = Agent_store.Snapshot
module Segment = Agent_store.Journal_segment
module Frame = Agent_store.Frame
module P = Agent_protocol

let%expect_test "bounded roots preserve framed references and expose incomplete journals" =
  with_temp_directory "retention-reader" (fun env root ->
    let snapshot_directory = Filename.concat root "snapshots" in
    let journal_directory = Filename.concat root "journal" in
    Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / journal_directory);
    let snapshot_id = P.Id.Blob.create ()
    and journal_id = P.Id.Blob.create () in
    let snapshot : Snapshot.t =
      { schema_version = 1
      ; transaction_sequence = 0L
      ; transaction_hash = None
      ; event_sequence = 0L
      ; created_at = timestamp
      ; prompt_artifact = "fixture"
      ; workspace_identity = "fixture"
      ; payload = P.Id.Blob.to_string snapshot_id
      }
    in
    let installed =
      Snapshot.install
        ~env
        ~directory:snapshot_directory
        ~max_payload_length:4096
        snapshot
      |> store_ok
    in
    let segment =
      Segment.create_exclusive ~env ~directory:journal_directory ~id:Segment.Id.first
      |> store_ok
    in
    let frame =
      Frame.encode ~max_payload_length:4096 ~flags:0 (P.Id.Blob.to_string journal_id)
      |> frame_ok
    in
    Segment.append ~env ~durability:Flush segment ~frame |> store_ok |> ignore;
    let reader =
      R.create ~env ~root ~max_entries:32 ~max_bytes:Int.max_value |> store_ok
    in
    [%test_eq: string list]
      [ "journal"; "snapshots" ]
      (R.list reader ~directory:"." |> store_ok);
    let snapshot_bytes =
      R.read
        reader
        ~path:(Filename.concat "snapshots" installed.filename)
        ~max_bytes:Int.max_value
      |> store_ok
    in
    let actual =
      Snapshot.decode_file ~max_payload_length:4096 snapshot_bytes |> store_ok
    in
    [%test_eq: string] snapshot.payload actual.payload;
    let journal_bytes =
      R.read
        reader
        ~path:(Filename.concat "journal" (Segment.Id.filename Segment.Id.first))
        ~max_bytes:4096
      |> store_ok
    in
    let scan = Segment.scan_contents ~max_payload_length:4096 journal_bytes |> store_ok in
    assert (not scan.crash_tail);
    [%test_eq: int] 1 (List.length scan.entries);
    let references =
      Agent_store.Blob_reference_scan.create [ snapshot_id; journal_id ] |> protocol_ok
    in
    List.iter [ snapshot_bytes; journal_bytes ] ~f:(fun bytes ->
      Agent_store.Blob_reference_scan.begin_root references;
      Agent_store.Blob_reference_scan.feed references bytes);
    assert (Agent_store.Blob_reference_scan.referenced references snapshot_id);
    assert (Agent_store.Blob_reference_scan.referenced references journal_id);
    let limited =
      R.create
        ~env
        ~root
        ~max_entries:32
        ~max_bytes:(String.length snapshot_bytes + String.length journal_bytes - 1)
      |> store_ok
    in
    R.read limited ~path:(Filename.concat "snapshots" installed.filename) ~max_bytes:4096
    |> store_ok
    |> ignore;
    assert (
      Result.is_error
        (R.read
           limited
           ~path:(Filename.concat "journal" (Segment.Id.filename Segment.Id.first))
           ~max_bytes:4096));
    let truncated =
      Segment.scan_contents ~max_payload_length:4096 (String.drop_suffix journal_bytes 3)
      |> store_ok
    in
    assert truncated.crash_tail;
    assert (List.is_empty truncated.entries);
    let changed = Bytes.of_string snapshot_bytes in
    Bytes.set
      changed
      (Bytes.length changed - 1)
      (Char.of_int_exn
         (Char.to_int (Bytes.get changed (Bytes.length changed - 1)) lxor 1));
    assert (
      Result.is_error
        (Snapshot.decode_file ~max_payload_length:4096 (Bytes.to_string changed)));
    print_endline
      "framed snapshot and journal references preserved; aggregate byte budget enforced";
    print_endline "incomplete journal and corrupt snapshot cannot become absence evidence");
  [%expect
    {|
    framed snapshot and journal references preserved; aggregate byte budget enforced
    incomplete journal and corrupt snapshot cannot become absence evidence
    |}]
;;

let%expect_test "retention reads reject growth, linked paths and enumeration excess" =
  with_temp_directory "retention-reader-fault" (fun env root ->
    let file name = Eio.Path.(Eio.Stdenv.fs env / Filename.concat root name) in
    Eio.Path.save ~create:(`Exclusive 0o600) (file "growing") "x";
    Eio.Path.save ~create:(`Exclusive 0o600) (file "foreign") "keep";
    Eio.Path.symlink ~link_to:(Filename.concat root "foreign") (file "linked");
    Eio.Path.symlink ~link_to:root (file "linked-directory");
    let armed = ref true in
    let wrapped =
      Job_store_fixtures.fault_env env (ref None) ~before_open_in:(fun path ->
        match !armed && String.is_suffix path ~suffix:"/growing" with
        | false -> ()
        | true ->
          armed := false;
          Eio.Path.save
            ~create:(`Or_truncate 0o600)
            (file "growing")
            (String.make 1024 'x'))
    in
    let reader = R.create ~env:wrapped ~root ~max_entries:32 ~max_bytes:8 |> store_ok in
    assert (Result.is_error (R.read reader ~path:"growing" ~max_bytes:8));
    assert (not !armed);
    List.iter [ "linked"; "linked-directory/foreign"; "../foreign" ] ~f:(fun path ->
      assert (Result.is_error (R.read reader ~path ~max_bytes:8)));
    let linked_root =
      R.create
        ~env
        ~root:(Filename.concat root "linked-directory/")
        ~max_entries:32
        ~max_bytes:8
      |> store_ok
    in
    assert (Result.is_error (R.list linked_root ~directory:"."));
    let limited = R.create ~env ~root ~max_entries:2 ~max_bytes:8 |> store_ok in
    assert (Result.is_error (R.list limited ~directory:"."));
    [%test_eq: string] "keep" (Eio.Path.load (file "foreign"));
    [%test_eq: int] 1024 (String.length (Eio.Path.load (file "growing")));
    print_endline
      "growth after stat and linked/traversing paths refused; input files preserved";
    print_endline "directory enumeration stopped at its entry budget");
  [%expect
    {|
    growth after stat and linked/traversing paths refused; input files preserved
    directory enumeration stopped at its entry budget
    |}]
;;
