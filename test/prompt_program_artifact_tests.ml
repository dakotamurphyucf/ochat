open Core
open Agent_store_test_fixtures
module A = Agent_store.Prompt_artifact_store
module B = Agent_store.Prompt_program_bundle
module D = Document_schema
module P = Agent_protocol

let document_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : D.Error.t)]
;;

let metadata =
  D.Document.create
    ~limits:D.Limits.default
    ~kind:"fixture.programs"
    ~version:1
    ~payload:(`Object [ "description", `String "captured program"; "future", `Null ])
  |> document_ok
;;

let payload = B.Payload.create "let value = 42" |> store_ok
let bundle = B.create ~metadata ~payloads:[ payload ] |> store_ok
let revision_id = P.Id.Prompt_revision.of_string "prv_program_custody" |> protocol_ok

let artifact ?(root_chatmd = "prompt") program_bundle =
  A.Artifact.create
    ~revision_id
    ~root_chatmd
    ~sources:[]
    ~program_bundle
    ~parser_schema_version:1
    ~runtime_schema_version:1
    ~created_at:timestamp
    ()
;;

let digest text = Digestif.SHA256.(digest_string text |> to_hex)

let reader env root =
  Agent_store.Retention_reader.create ~env ~root ~max_entries:4096 ~max_bytes:65536
  |> store_ok
;;

let%expect_test "program sources remain private across exact artifact reload and copy" =
  with_temp_directory "ochat-program-artifact" (fun env root ->
    let store = A.create ~env ~root:(Filename.concat root "first") |> store_ok in
    let original = artifact bundle |> store_ok in
    A.install store ~transaction_id original |> store_ok;
    let loaded = A.load store revision_id |> store_ok in
    let restored = Option.value_exn loaded.program_bundle in
    assert (B.Manifest.equal (B.manifest bundle) (B.manifest restored));
    assert (
      String.equal
        (Agent_store.Prompt_manifest_document.Publication.bytes
           original.manifest_publication)
        (Agent_store.Prompt_manifest_document.Publication.bytes
           loaded.manifest_publication));
    assert (
      List.equal
        String.equal
        (List.map (B.payloads restored) ~f:B.Payload.contents)
        [ "let value = 42" ]);
    let tree = A.materialized_tree store revision_id in
    assert (List.equal String.equal (Eio.Path.read_dir tree) [ "root.chatmd" ]);
    A.verify_retained
      store
      ~reader:(reader env (Filename.concat root "first"))
      ~revision_id
      ~manifest_sha256:loaded.manifest_sha256
    |> store_ok;
    let second = A.create ~env ~root:(Filename.concat root "second") |> store_ok in
    A.install second ~transaction_id loaded |> store_ok;
    let copied = A.load second revision_id |> store_ok in
    assert (A.Artifact.same_content original copied);
    print_endline "authored tree: root.chatmd";
    print_endline "private program payload: retained and verified";
    print_endline "copied publication: exact");
  [%expect
    {| 
    authored tree: root.chatmd
    private program payload: retained and verified
    copied publication: exact |}]
;;

let%test_unit "one source count and byte allowance includes private program payloads" =
  let exact = B.Payload.create (String.make (B.max_payload_bytes - 1) 'x') |> store_ok in
  let big = B.create ~metadata ~payloads:[ exact ] |> store_ok in
  assert (Result.is_ok (artifact ~root_chatmd:"x" big));
  assert (Result.is_error (artifact ~root_chatmd:"xx" big));
  let make count =
    List.init count ~f:(fun i -> B.Payload.create (Int.to_string i) |> store_ok)
    |> fun payloads -> B.create ~metadata ~payloads |> store_ok
  in
  assert (Result.is_ok (artifact (make 255)));
  assert (Result.is_error (artifact (make 256)));
  assert (Result.is_error (B.create ~metadata ~payloads:[ payload; payload ]));
  assert (Result.is_error (B.restore (B.manifest bundle) []));
  let extra = B.Payload.create "another" |> store_ok in
  assert (Result.is_error (B.restore (B.manifest bundle) [ payload; extra ]))
;;

let%test_unit "private storage rejects absent, altered, extra and linked payloads" =
  List.iter [ `Missing; `Altered; `Extra; `Link ] ~f:(fun change ->
    with_temp_directory "ochat-program-tamper" (fun env root ->
      let store = A.create ~env ~root:(Filename.concat root "store") |> store_ok in
      let original = artifact bundle |> store_ok in
      A.install store ~transaction_id original |> store_ok;
      let directory =
        Filename.concat
          (Filename.concat root "store")
          (P.Id.Prompt_revision.to_string revision_id)
      in
      let path =
        Eio.Path.(
          Eio.Stdenv.fs env
          / Filename.concat
              (Filename.concat directory "programs")
              (B.Payload.sha256 payload))
      in
      (match change with
       | `Missing -> Eio.Path.unlink path
       | `Altered ->
         Eio.Path.unlink path;
         Eio.Path.save ~create:(`Exclusive 0o400) path "changed"
       | `Extra ->
         Eio.Path.save
           ~create:(`Exclusive 0o400)
           Eio.Path.(
             Eio.Stdenv.fs env
             / Filename.concat (Filename.concat directory "programs") (digest "extra"))
           "extra"
       | `Link ->
         Eio.Path.unlink path;
         Eio.Path.symlink ~link_to:"../root.chatmd" path);
      let loaded = A.load store revision_id in
      assert (Result.is_error loaded);
      (match change, loaded with
       | `Extra, Error (Agent_store.Store_error.Corrupt message) ->
         (* An excess entry must stop the bounded directory stream, before an
            oversized name inventory can be collected and compared. *)
         assert (String.equal message "retention scan exceeded its entry budget")
       | `Extra, _ -> failwith "excess private inventory was not bounded during scanning"
       | (`Missing | `Altered | `Link), _ -> ());
      assert (
        Result.is_error
          (A.verify_retained
             store
             ~reader:(reader env (Filename.concat root "store"))
             ~revision_id
             ~manifest_sha256:original.manifest_sha256))))
;;

let%test_unit "program-bearing manifests cannot be silently read without their semantics" =
  let original = artifact bundle |> store_ok in
  let document =
    Agent_store.Prompt_manifest_document.Publication.document
      original.manifest_publication
    |> Agent_store.Prompt_manifest_document.to_document
    |> document_ok
  in
  assert (
    List.mem
      (D.Document.required_semantics document)
      "prompt.program-bundle.v1"
      ~equal:String.equal);
  let unsupported =
    D.Domain_codec.create
      ~limits:D.Limits.default
      ~kind:"store.prompt_manifest"
      ~version:1
      ~shape:(D.Shape.object_ [] |> document_ok)
      ~supported_semantics:[]
      ~decode:(fun _ -> Ok ())
      ~encode:(fun () -> Ok (`Object []))
    |> document_ok
  in
  (match D.Domain_codec.decode unsupported document with
   | Error (D.Error.Required_semantics_unknown "prompt.program-bundle.v1") -> ()
   | _ -> failwith "unsupported program semantics were admitted");
  let unmarked =
    match D.Document.json document with
    | `Object fields ->
      `Object
        (List.filter fields ~f:(fun (name, _) ->
           not (String.equal name "required_semantics")))
    | _ -> assert false
  in
  let unmarked = D.Document.inspect ~limits:D.Limits.default unmarked |> document_ok in
  assert (Result.is_error (Agent_store.Prompt_manifest_document.of_document unmarked))
;;

let%test_unit "revision identity copies preserve unknown metadata and original evidence" =
  with_temp_directory "ochat-program-copy-metadata" (fun env root ->
    let store = A.create ~env ~root:(Filename.concat root "store") |> store_ok in
    let original = artifact bundle |> store_ok in
    A.install store ~transaction_id original |> store_ok;
    let directory =
      Filename.concat
        (Filename.concat root "store")
        (P.Id.Prompt_revision.to_string revision_id)
    in
    let path name = Eio.Path.(Eio.Stdenv.fs env / Filename.concat directory name) in
    let publication =
      Agent_store.Prompt_manifest_document.Publication.bytes original.manifest_publication
    in
    let json = Jsonaf.of_string publication in
    let json =
      match json with
      | `Object fields -> `Object (fields @ [ "unknown_revision_note", `String "keep me" ])
      | _ -> assert false
    in
    let bytes = " \n" ^ Jsonaf.to_string json ^ "\n" in
    List.iter
      [ "manifest.sexp", bytes; "manifest.sha256", digest bytes ^ "\n" ]
      ~f:(fun (name, contents) ->
        Eio.Path.unlink (path name);
        Eio.Path.save ~create:(`Exclusive 0o400) (path name) contents);
    let loaded = A.load store revision_id |> store_ok in
    let unchanged =
      A.Artifact.with_identity loaded ~revision_id ~created_at:timestamp |> store_ok
    in
    assert (
      String.equal
        bytes
        (Agent_store.Prompt_manifest_document.Publication.bytes
           unchanged.manifest_publication));
    let next = P.Id.Prompt_revision.of_string "prv_program_copy" |> protocol_ok in
    let copied =
      A.Artifact.with_identity loaded ~revision_id:next ~created_at:timestamp |> store_ok
    in
    let encoded =
      Agent_store.Prompt_manifest_document.Publication.bytes copied.manifest_publication
    in
    let doc = D.Document.decode ~limits:D.Limits.default encoded |> document_ok in
    (match D.Json.field (D.Document.json doc) ~name:"unknown_revision_note" with
     | Value (`String "keep me") -> ()
     | _ -> failwith "unknown metadata lost during revision copy");
    assert (
      String.equal
        bytes
        (Agent_store.Prompt_manifest_document.Publication.bytes
           loaded.manifest_publication));
    assert (
      B.Manifest.equal
        (B.manifest bundle)
        (B.manifest (Option.value_exn copied.program_bundle)));
    A.install store ~transaction_id copied |> store_ok;
    let restored = A.load store next |> store_ok in
    assert (String.equal copied.manifest_sha256 restored.manifest_sha256))
;;
