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
    let original =
      A.Artifact.create
        ~revision_id
        ~root_chatmd:"prompt"
        ~sources:[]
        ~program_bundle:bundle
        ~content_bundle:bundle
        ~parser_schema_version:1
        ~runtime_schema_version:1
        ~created_at:timestamp
        ()
      |> store_ok
    in
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
      | `Object fields ->
        `Object
          (List.map fields ~f:(fun (name, value) ->
             if not (String.equal name "payload")
             then name, value
             else (
               let value =
                 match value with
                 | `Object payload ->
                   `Object
                     (List.map payload ~f:(fun (name, value) ->
                        if not (String.equal name "content_bundle")
                        then name, value
                        else (
                          let value =
                            match value with
                            | `Object inventory ->
                              `Object
                                (List.map inventory ~f:(fun (name, value) ->
                                   if not (String.equal name "metadata")
                                   then name, value
                                   else (
                                     let value =
                                       match value with
                                       | `Object metadata ->
                                         `Object
                                           (metadata
                                            @ [ ( "unknown_content_note"
                                                , `String "keep content too" )
                                              ])
                                       | _ -> assert false
                                     in
                                     name, value)))
                            | _ -> assert false
                          in
                          name, value)))
                 | _ -> assert false
               in
               name, value))
           @ [ "unknown_revision_note", `String "keep me" ])
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
    let content_document =
      Option.value_exn copied.content_bundle |> B.manifest |> B.Manifest.metadata
    in
    (match
       D.Json.field (D.Document.json content_document) ~name:"unknown_content_note"
     with
     | Value (`String "keep content too") -> ()
     | _ -> failwith "unknown content metadata lost during revision copy");
    A.install store ~transaction_id copied |> store_ok;
    let restored = A.load store next |> store_ok in
    assert (String.equal copied.manifest_sha256 restored.manifest_sha256))
;;

let joint_artifact ?(root_chatmd = "prompt") ~programs ~content () =
  A.Artifact.create
    ~revision_id
    ~root_chatmd
    ~sources:[]
    ?program_bundle:programs
    ?content_bundle:content
    ~parser_schema_version:1
    ~runtime_schema_version:1
    ~created_at:timestamp
    ()
;;

let%expect_test "content and programs share publication but keep distinct private pools" =
  with_temp_directory "ochat-joint-custody" (fun env root ->
    let store = A.create ~env ~root:(Filename.concat root "store") |> store_ok in
    (* Exact equal bytes must still occupy both physical pools. *)
    let original =
      joint_artifact ~programs:(Some bundle) ~content:(Some bundle) () |> store_ok
    in
    A.install store ~transaction_id original |> store_ok;
    let loaded = A.load store revision_id |> store_ok in
    List.iter [ loaded.program_bundle; loaded.content_bundle ] ~f:(fun actual ->
      assert (B.Manifest.equal (B.manifest bundle) (B.manifest (Option.value_exn actual))));
    let root_path =
      Eio.Path.(
        Eio.Stdenv.fs env
        / Filename.concat root "store"
        / P.Id.Prompt_revision.to_string revision_id)
    in
    List.iter [ "programs"; "content" ] ~f:(fun pool ->
      assert (
        String.equal
          (Eio.Path.load Eio.Path.(root_path / pool / B.Payload.sha256 payload))
          (B.Payload.contents payload)));
    assert (
      List.equal
        String.equal
        (Eio.Path.read_dir (A.materialized_tree store revision_id))
        [ "root.chatmd" ]);
    A.verify_retained
      store
      ~reader:(reader env (Filename.concat root "store"))
      ~revision_id
      ~manifest_sha256:loaded.manifest_sha256
    |> store_ok;
    let next = P.Id.Prompt_revision.of_string "prv_joint_copy" |> protocol_ok in
    let copied =
      A.Artifact.with_identity loaded ~revision_id:next ~created_at:timestamp |> store_ok
    in
    A.install store ~transaction_id copied |> store_ok;
    let restored = A.load store next |> store_ok in
    assert (
      B.Manifest.equal
        (B.manifest bundle)
        (B.manifest (Option.value_exn restored.content_bundle)));
    print_s
      [%sexp
        { authored_tree = ([ "root.chatmd" ] : string list)
        ; private_pools = ([ "programs"; "content" ] : string list)
        ; copied_exact = true
        }]);
  [%expect
    {|
    ((authored_tree (root.chatmd)) (private_pools (programs content))
     (copied_exact true))
    |}]
;;

let%test_unit "joint physical budget admits exact count and bytes before publication" =
  let make count =
    B.create
      ~metadata
      ~payloads:
        (List.init count ~f:(fun i -> B.Payload.create (Int.to_string i) |> store_ok))
    |> store_ok
  in
  let first = make 128 in
  assert (
    Result.is_ok (joint_artifact ~programs:(Some first) ~content:(Some (make 127)) ()));
  assert (Result.is_error (joint_artifact ~programs:(Some first) ~content:(Some first) ()));
  let half =
    B.Payload.create (String.make ((B.max_payload_bytes - 1) / 2) 'x') |> store_ok
  in
  let remaining =
    B.Payload.create
      (String.make (B.max_payload_bytes - 1 - B.Payload.byte_length half) 'y')
    |> store_ok
  in
  let pack value = B.create ~metadata ~payloads:[ value ] |> store_ok in
  let programs = Some (pack half)
  and content = Some (pack remaining) in
  assert (Result.is_ok (joint_artifact ~root_chatmd:"x" ~programs ~content ()));
  assert (Result.is_error (joint_artifact ~root_chatmd:"xx" ~programs ~content ()))
;;

let%test_unit "content tampering and absent semantics reject independently of programs" =
  List.iter [ `Missing; `Altered; `Extra; `Link ] ~f:(fun change ->
    with_temp_directory "ochat-content-tamper" (fun env root ->
      let store = A.create ~env ~root:(Filename.concat root "store") |> store_ok in
      let original =
        joint_artifact ~programs:(Some bundle) ~content:(Some bundle) () |> store_ok
      in
      A.install store ~transaction_id original |> store_ok;
      let directory =
        Eio.Path.(
          Eio.Stdenv.fs env
          / Filename.concat root "store"
          / P.Id.Prompt_revision.to_string revision_id
          / "content")
      in
      let path = Eio.Path.(directory / B.Payload.sha256 payload) in
      (match change with
       | `Missing -> Eio.Path.unlink path
       | `Altered ->
         Eio.Path.unlink path;
         Eio.Path.save ~create:(`Exclusive 0o400) path "altered"
       | `Extra ->
         Eio.Path.save
           ~create:(`Exclusive 0o400)
           Eio.Path.(directory / digest "extra")
           "extra"
       | `Link ->
         Eio.Path.unlink path;
         Eio.Path.symlink ~link_to:"../root.chatmd" path);
      assert (Result.is_error (A.load store revision_id));
      assert (
        Result.is_error
          (A.verify_retained
             store
             ~reader:(reader env (Filename.concat root "store"))
             ~revision_id
             ~manifest_sha256:original.manifest_sha256))));
  let original = joint_artifact ~programs:None ~content:(Some bundle) () |> store_ok in
  let doc =
    Agent_store.Prompt_manifest_document.to_document
      (Agent_store.Prompt_manifest_document.Publication.document
         original.manifest_publication)
    |> document_ok
  in
  assert (
    List.mem
      (D.Document.required_semantics doc)
      "prompt.content-bundle.v1"
      ~equal:String.equal);
  let json =
    match D.Document.json doc with
    | `Object fields ->
      `Object
        (List.filter fields ~f:(fun (name, _) ->
           not (String.equal name "required_semantics")))
    | _ -> assert false
  in
  let unmarked = D.Document.inspect ~limits:D.Limits.default json |> document_ok in
  assert (Result.is_error (Agent_store.Prompt_manifest_document.of_document unmarked))
;;

let%test_unit
    "shared full inventory fingerprint is exact bounded and callback-transparent"
  =
  let manifest = B.manifest bundle in
  let json = B.Manifest.to_json manifest in
  let bytes = Jsonaf.to_string json in
  let limits max_bytes =
    D.Limits.create ~max_bytes ~max_nodes:10000 ~max_fields:10000 ~max_depth:128
    |> document_ok
  in
  let actual =
    B.Manifest.fingerprint
      manifest
      ~limits:(limits (String.length bytes))
      ~checkpoint:ignore
    |> document_ok
  in
  assert (String.equal actual (digest bytes));
  assert (
    Result.is_error
      (B.Manifest.fingerprint
         manifest
         ~limits:(limits (String.length bytes - 1))
         ~checkpoint:ignore));
  let marker = Failure "borrowed checkpoint" in
  match
    B.Manifest.fingerprint manifest ~limits:D.Limits.default ~checkpoint:(fun () ->
      raise marker)
  with
  | exception exn -> assert (phys_equal marker exn)
  | _ -> failwith "checkpoint was swallowed"
;;

let%test_unit "payload load shares one byte ledger across program and content pools" =
  with_temp_directory "ochat-joint-read-limit" (fun env root ->
    let store = A.create ~env ~root:(Filename.concat root "store") |> store_ok in
    let original =
      joint_artifact ~programs:(Some bundle) ~content:(Some bundle) () |> store_ok
    in
    A.install store ~transaction_id original |> store_ok;
    let directory =
      Eio.Path.(
        Eio.Stdenv.fs env
        / Filename.concat root "store"
        / P.Id.Prompt_revision.to_string revision_id)
    in
    let change pool char =
      let large =
        B.Payload.create (String.make (B.max_payload_bytes / 2) char) |> store_ok
      in
      Eio.Path.unlink Eio.Path.(directory / pool / B.Payload.sha256 payload);
      Eio.Path.save
        ~create:(`Exclusive 0o400)
        Eio.Path.(directory / pool / B.Payload.sha256 large)
        (B.Payload.contents large);
      B.create ~metadata ~payloads:[ large ]
      |> store_ok
      |> B.manifest
      |> B.Manifest.to_json
    in
    let programs = change "programs" 'p'
    and content = change "content" 'c' in
    let bytes =
      Agent_store.Prompt_manifest_document.Publication.bytes original.manifest_publication
      |> Jsonaf.of_string
    in
    let json =
      match bytes with
      | `Object fields ->
        `Object
          (List.map fields ~f:(fun (name, value) ->
             if not (String.equal name "payload")
             then name, value
             else (
               let value =
                 match value with
                 | `Object fields ->
                   `Object
                     (List.map fields ~f:(fun (name, value) ->
                        ( name
                        , if String.equal name "program_bundle"
                          then programs
                          else if String.equal name "content_bundle"
                          then content
                          else value )))
                 | _ -> assert false
               in
               name, value)))
      | _ -> assert false
    in
    let encoded = Jsonaf.to_string json in
    List.iter
      [ "manifest.sexp", encoded; "manifest.sha256", digest encoded ^ "\n" ]
      ~f:(fun (name, value) ->
        Eio.Path.unlink Eio.Path.(directory / name);
        Eio.Path.save ~create:(`Exclusive 0o400) Eio.Path.(directory / name) value);
    (* Each pool alone fits8MiB; together they exhaust it before the already-read
       authored root is counted. Failure must originate in the shared reader. *)
    match A.load store revision_id with
    | Error (Agent_store.Store_error.Corrupt "retention root exceeds its byte budget") ->
      ()
    | _ -> failwith "joint payload reads did not share the artifact byte ledger")
;;
