open Core
open Fixtures
module P = Agent_protocol
module A = Agent_store.Prompt_artifact_store
module B = Agent_session.Prompt_revision_builder

let%expect_test "catalog rebuild returns original installed manifest publication" =
  with_temp_directory (fun env temporary ->
    let root_file = Filename.concat temporary "root.chatmd" in
    Eio.Path.save
      ~create:(`Exclusive 0o600)
      Eio.Path.(Eio.Stdenv.fs env / root_file)
      "<user>hello</user>";
    let definition =
      Agent_session.Prompt_definition.create
        ~id:prompt_id
        ~config_name:"manifest-custody"
        ~root_file
        ~allowed_workspaces:[ workspace_id ]
        ~permission_profile:"interactive"
        ~runtime_policy:None
        ~enabled:true
        ~description:None
      |> store_ok
    in
    let root = Filename.concat temporary "artifacts" in
    let artifacts = A.create ~env ~root |> store_ok in
    let get result =
      Result.map_error result ~f:(fun errors ->
        Sexp.to_string_hum ([%sexp_of: B.Diagnostic.t list] errors))
      |> Result.ok_or_failwith
    in
    let build created_at =
      B.build
        ~env
        ~artifact_store:artifacts
        ~transaction_id:(P.Id.Transaction.create ())
        ~created_at
        definition
      |> get
    in
    let revision = build timestamp in
    let artifact = Agent_session.Prompt_revision.artifact revision in
    let directory =
      Filename.concat root (P.Id.Prompt_revision.to_string artifact.revision_id)
    in
    let path = Eio.Path.(Eio.Stdenv.fs env / directory / "manifest.sexp") in
    let json =
      match Jsonaf.of_string (Eio.Path.load path) with
      | `Object fields -> `Object (fields @ [ "future_catalog", `Null ])
      | _ -> assert false
    in
    let raw = " \n" ^ Jsonaf.to_string json ^ "\n " in
    let digest = Digestif.SHA256.(digest_string raw |> to_hex) in
    Eio.Path.unlink path;
    Eio.Path.save ~create:(`Exclusive 0o400) path raw;
    let checksum = Eio.Path.(Eio.Stdenv.fs env / directory / "manifest.sha256") in
    Eio.Path.unlink checksum;
    Eio.Path.save ~create:(`Exclusive 0o400) checksum (digest ^ "\n");
    let rebuilt = build (P.Timestamp.add_ms timestamp 1000 |> protocol_ok) in
    let retained = Agent_session.Prompt_revision.artifact rebuilt in
    assert (P.Id.Prompt_revision.equal artifact.revision_id retained.revision_id);
    assert (P.Timestamp.equal timestamp retained.created_at);
    assert (String.equal retained.manifest_sha256 digest);
    assert (
      String.equal
        raw
        (Agent_store.Prompt_manifest_document.Publication.bytes
           retained.manifest_publication));
    assert (String.equal (Eio.Path.load path) raw);
    print_endline
      "rebuild checks captured source metadata and returns installed exact \
       digest/bytes/timestamp");
  [%expect
    {|rebuild checks captured source metadata and returns installed exact digest/bytes/timestamp|}]
;;
