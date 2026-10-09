open Core
open Agent_store_test_fixtures
module A = Agent_store.Prompt_artifact_store
module C = Agent_store.Prompt_manifest_document
module Pub = C.Publication
module P = Agent_protocol

let prompt_revision_id =
  P.Id.Prompt_revision.of_string "prv_prompt_publication" |> protocol_ok
;;

let digest text = Digestif.SHA256.(digest_string text |> to_hex)

let make () =
  A.Artifact.create
    ~revision_id:prompt_revision_id
    ~root_chatmd:"root prompt"
    ~sources:
      [ A.Source.create ~relative_path:"library/tools.chatmd" ~contents:"tool source"
        |> store_ok
      ]
    ~parser_schema_version:1
    ~runtime_schema_version:1
    ~created_at:timestamp
    ()
  |> store_ok
;;

let read_file env path = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path)

let write_file env path bytes =
  Eio.Path.unlink Eio.Path.(Eio.Stdenv.fs env / path);
  Eio.Path.save ~create:(`Exclusive 0o400) Eio.Path.(Eio.Stdenv.fs env / path) bytes
;;

let reader env root ~bytes =
  Agent_store.Retention_reader.create ~env ~root ~max_entries:4096 ~max_bytes:bytes
  |> store_ok
;;

let%expect_test
    "loaded manifest original byte evidence survives exact reinstall and retention"
  =
  with_temp_directory "ochat-prompt-publication" (fun env root ->
    let first_root = Filename.concat root "first" in
    let first = A.create ~env ~root:first_root |> store_ok in
    let original = make () in
    A.install first ~transaction_id original |> store_ok;
    let directory =
      Filename.concat first_root (P.Id.Prompt_revision.to_string original.revision_id)
    in
    let manifest_path = Filename.concat directory "manifest.sexp" in
    let json = Jsonaf.of_string (read_file env manifest_path) in
    let append json field =
      match json with
      | `Object fields -> `Object (fields @ [ field ])
      | _ -> assert false
    in
    let edit json name f =
      match json with
      | `Object fields ->
        `Object
          (List.map fields ~f:(fun (key, value) ->
             key, if String.equal key name then f value else value))
      | _ -> assert false
    in
    let json =
      append json ("future_envelope", `Null)
      |> fun json ->
      edit json "payload" (fun payload ->
        append payload ("unknown_manifest", `Number "1e+00"))
    in
    let raw = " \n" ^ Jsonaf.to_string json ^ "\n " in
    let expected = digest raw in
    write_file env manifest_path raw;
    write_file env (Filename.concat directory "manifest.sha256") (expected ^ "\n");
    let loaded = A.load first original.revision_id |> store_ok in
    assert (A.Artifact.same_content original loaded);
    assert (not (String.equal original.manifest_sha256 loaded.manifest_sha256));
    assert (String.equal expected loaded.manifest_sha256);
    assert (String.equal raw (Pub.bytes loaded.manifest_publication));
    A.verify_retained
      first
      ~reader:(reader env first_root ~bytes:65536)
      ~revision_id:loaded.revision_id
      ~manifest_sha256:expected
    |> store_ok;
    assert (
      Result.is_error
        (A.verify_retained
           first
           ~reader:(reader env first_root ~bytes:64)
           ~revision_id:loaded.revision_id
           ~manifest_sha256:expected));
    assert (String.equal raw (read_file env manifest_path));
    let second_root = Filename.concat root "second" in
    let second = A.create ~env ~root:second_root |> store_ok in
    A.install second ~transaction_id:(P.Id.Transaction.create ()) loaded |> store_ok;
    let reopened = A.load second loaded.revision_id |> store_ok in
    assert (String.equal expected reopened.manifest_sha256);
    assert (String.equal raw (Pub.bytes reopened.manifest_publication));
    assert (String.equal reopened.root_chatmd "root prompt");
    assert (String.equal (List.hd_exn reopened.sources).contents "tool source");
    print_endline
      "load/reinstall retain exact admitted digest and bytes; bounded retention shares \
       evidence");
  [%expect
    {|load/reinstall retain exact admitted digest and bytes; bounded retention shares evidence|}]
;;

let%expect_test
    "prompt install uncertain rename and root sync retain original publication"
  =
  List.iter [ "rename"; "sync"; "cancellation" ] ~f:(fun phase ->
    with_temp_directory "ochat-prompt-publication-fault" (fun env root ->
      let artifact_root = Filename.concat root "artifacts" in
      let armed = ref None in
      let sync_fault = ref false in
      let env =
        Job_store_fixtures.fault_env
          ~matches_rename:(fun path ->
            String.is_suffix
              path
              ~suffix:(P.Id.Prompt_revision.to_string prompt_revision_id))
          ~before_open_in:(fun path ->
            if !sync_fault && String.equal path (Filename.concat artifact_root ".")
            then (
              sync_fault := false;
              if String.equal phase "cancellation"
              then raise Eio.Time.Timeout
              else raise (Core_unix.Unix_error (EIO, "directory sync fault", path))))
          env
          armed
      in
      let store = A.create ~env ~root:artifact_root |> store_ok in
      let artifact = make () in
      if String.equal phase "rename" then armed := Some true else sync_fault := true;
      let cancelled =
        try
          assert (Result.is_error (A.install store ~transaction_id artifact));
          false
        with
        | Eio.Time.Timeout -> true
      in
      assert (Bool.equal cancelled (String.equal phase "cancellation"));
      (* Prove the selected operation actually reached the injected failure. *)
      assert (Option.is_none !armed);
      assert (not !sync_fault);
      let loaded = A.load store artifact.revision_id |> store_ok in
      assert (String.equal loaded.manifest_sha256 artifact.manifest_sha256);
      assert (
        String.equal
          (Pub.bytes loaded.manifest_publication)
          (Pub.bytes artifact.manifest_publication));
      A.verify_retained
        store
        ~reader:(reader env artifact_root ~bytes:65536)
        ~revision_id:loaded.revision_id
        ~manifest_sha256:artifact.manifest_sha256
      |> store_ok;
      assert (
        not
          (Eio.Path.is_directory
             Eio.Path.(
               Eio.Stdenv.fs env
               / artifact_root
               / (".install-" ^ P.Id.Transaction.to_string transaction_id))));
      print_endline (phase ^ ": primary failure and installed raw evidence retained")));
  [%expect
    {|
    rename: primary failure and installed raw evidence retained
    sync: primary failure and installed raw evidence retained
    cancellation: primary failure and installed raw evidence retained
    |}]
;;

let%expect_test
    "failed prompt installation preserves primary exception and foreign staging"
  =
  let exception Primary_install_failure in
  with_temp_directory "ochat-prompt-private-cleanup" (fun env root ->
    let artifact_root = Filename.concat root "artifacts" in
    let fail = ref false in
    let cleanup_fault = ref false in
    let armed = ref None in
    let env =
      Job_store_fixtures.fault_env
        ~before_open_out:(fun path ->
          if !fail && String.is_suffix path ~suffix:"manifest.sexp"
          then (
            fail := false;
            cleanup_fault := true;
            raise Primary_install_failure))
        ~before_unlink:(fun _ ->
          if !cleanup_fault
          then (
            cleanup_fault := false;
            raise Eio.Time.Timeout))
        env
        armed
    in
    let store = A.create ~env ~root:artifact_root |> store_ok in
    let artifact = make () in
    fail := true;
    (try
       ignore (A.install store ~transaction_id artifact);
       assert false
     with
     | Primary_install_failure -> ());
    assert (Result.is_error (A.load store artifact.revision_id));
    assert (not !fail);
    assert (not !cleanup_fault);
    let foreign_transaction = P.Id.Transaction.create () in
    let foreign =
      Eio.Path.(
        Eio.Stdenv.fs env
        / artifact_root
        / (".install-" ^ P.Id.Transaction.to_string foreign_transaction))
    in
    Eio.Path.mkdir ~perm:0o700 foreign;
    Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(foreign / "unowned") "preserved";
    assert (Result.is_error (A.install store ~transaction_id:foreign_transaction artifact));
    assert (String.equal (Eio.Path.load Eio.Path.(foreign / "unowned")) "preserved");
    print_endline
      "primary failure survives cleanup Timeout; preexisting transaction directory \
       untouched");
  [%expect
    {|primary failure survives cleanup Timeout; preexisting transaction directory untouched|}]
;;
