open! Core
open Admission_tests
module Store = Agent_store
module Snap = Store.Snapshot
module Persistence = S.Session_persistence

let evidence =
  let transaction = actual_transaction (Receipt_tests.created_state ()) in
  let restored =
    Session_seed_receipt.of_created transaction ~limits ~checkpoint:Fn.ignore
    |> store_ok
    |> Session_seed_receipt.restored
  in
  restored, Store.Transaction.hash transaction
;;

let requested () =
  let restored, hash = evidence in
  let state = Persistence.Restored.state restored in
  let payload =
    Persistence.Restored.state_document restored
    |> fun document -> S.Session_state_document.encode document ~limits |> doc_ok
  in
  Snap.create
    ~limits
    ~session_id:state.identity.session_id
    ~transaction_sequence:1L
    ~transaction_hash:(Some hash)
    ~event_sequence:1L
    ~created_at:state.identity.updated_at
    ~prompt_artifact:(P.Id.Prompt_revision.to_string state.spec.prompt_revision_id)
    ~workspace_identity:state.spec.workspace_instance.conflict_domain
    ~payload
  |> store_ok
;;

let%expect_test
    "first checkpoint completes exact prefixes and preserves complete outer ownership"
  =
  Journal_tests.with_directory (fun env root ->
    let original = Eio.Path.(root / "original") in
    let snapshot = requested () in
    let installed =
      Snap.install
        ~env
        ~directory:(Eio.Path.native_exn original)
        ~max_payload_length:100_000
        snapshot
      |> store_ok
    in
    let bytes = Eio.Path.load Eio.Path.(original / installed.filename) in
    List.iteri
      [ None
      ; Some 0
      ; Some (Store.Frame.header_length - 1)
      ; Some (String.length bytes - 1)
      ; Some (String.length bytes)
      ]
      ~f:(fun index prefix ->
        let directory = Eio.Path.(root / ("phase-" ^ Int.to_string index)) in
        Eio.Path.mkdir ~perm:0o700 directory;
        Option.iter prefix ~f:(fun count ->
          Eio.Path.save
            ~create:(`Exclusive 0o600)
            Eio.Path.(directory / installed.filename)
            (String.prefix bytes count));
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(directory / "CURRENT.tmp-123-4")
          "abandoned owner output is not authority";
        let actual =
          Snap.install_first_checkpoint
            snapshot
            ~env
            ~reader:(Journal_tests.reader env directory ())
            ~max_payload_length:100_000
            ~checkpoint:Fn.ignore
          |> store_ok
        in
        assert (String.equal actual.filename installed.filename);
        assert (
          String.equal (Eio.Path.load Eio.Path.(directory / installed.filename)) bytes);
        assert (
          String.equal
            (Eio.Path.load Eio.Path.(directory / "CURRENT"))
            (installed.filename ^ "\n"));
        assert (
          String.equal
            (Eio.Path.load Eio.Path.(directory / "CURRENT.tmp-123-4"))
            "abandoned owner output is not authority"));
    let extended_json =
      match
        D.Document.json
          (Store.Document_record.document (Snap.Stored.record (Snap.stored snapshot)))
      with
      | `Object fields -> `Object (fields @ [ "future-snapshot", `Number "1.200e+03" ])
      | _ -> assert false
    in
    let extended = D.Document.inspect ~limits extended_json |> doc_ok in
    let extended =
      Store.Document_record.encode extended ~limits ~flags:0
      |> Result.map_error ~f:Store.Document_fields.record_error
      |> store_ok
    in
    let extended_snapshot =
      Snap.decode_file ~max_payload_length:100_000 extended |> store_ok
    in
    let directory = Eio.Path.(root / "extended") in
    let installed =
      Snap.install
        ~env
        ~directory:(Eio.Path.native_exn directory)
        ~max_payload_length:100_000
        extended_snapshot
      |> store_ok
    in
    let restored, hash = evidence in
    let _, actual =
      Persistence.install_first_restored_snapshot_at
        restored
        ~env
        ~reader:(Journal_tests.reader env directory ())
        ~max_payload_length:100_000
        ~transaction_hash:(Some hash)
        ~checkpoint:Fn.ignore
        (Persistence.Restored.state restored)
      |> store_ok
    in
    assert (String.equal (Eio.Path.load Eio.Path.(directory / actual.filename)) extended);
    assert (String.equal installed.filename actual.filename);
    print_endline
      "missing/empty/header/body/full phases converge; abandoned CURRENT temp and outer \
       numeric spelling survive");
  [%expect
    {| missing/empty/header/body/full phases converge; abandoned CURRENT temp and outer numeric spelling survive |}]
;;

let%expect_test "first checkpoint rejects divergent prefix or inventory without mutation" =
  Journal_tests.with_directory (fun env root ->
    let original = Eio.Path.(root / "original") in
    let snapshot = requested () in
    let installed =
      Snap.install
        ~env
        ~directory:(Eio.Path.native_exn original)
        ~max_payload_length:100_000
        snapshot
      |> store_ok
    in
    let bytes = Eio.Path.load Eio.Path.(original / installed.filename) in
    List.iteri [ "prefix"; "CURRENT"; "extra" ] ~f:(fun index case ->
      let directory = Eio.Path.(root / Int.to_string index) in
      Eio.Path.mkdir ~perm:0o700 directory;
      let prefix =
        if String.equal case "prefix" then "wrong" else String.prefix bytes 30
      in
      Eio.Path.save
        ~create:(`Exclusive 0o600)
        Eio.Path.(directory / installed.filename)
        prefix;
      if String.equal case "CURRENT"
      then
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(directory / "CURRENT")
          "other.bin\n";
      if String.equal case "extra"
      then
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(directory / "other.bin")
          "untouched";
      assert (
        Result.is_error
          (Snap.install_first_checkpoint
             snapshot
             ~env
             ~reader:(Journal_tests.reader env directory ())
             ~max_payload_length:100_000
             ~checkpoint:Fn.ignore));
      assert (
        String.equal (Eio.Path.load Eio.Path.(directory / installed.filename)) prefix)));
  [%expect {| |}]
;;
