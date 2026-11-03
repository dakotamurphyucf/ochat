open! Core
module A = Agent_store.Prompt_artifact_store
module B = Agent_store.Prompt_program_bundle
module M = Agent_store.Prompt_manifest_document
module C = Agent_store.Captured_prompt_artifact
module D = Document_schema
module P = Agent_protocol

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : D.Error.t)]
;;

let store_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_store.Store_error.t)]
;;

let protocol_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : P.Error.t)]
;;

let timestamp = P.Timestamp.of_string "2026-10-11T12:00:00Z" |> protocol_ok
let digest text = Digestif.SHA256.(digest_string text |> to_hex)
let encoded text = `String (Base64.encode_exn text)
let revision = P.Id.Prompt_revision.of_string "prv_captured_custody" |> protocol_ok

let metadata kind =
  D.Document.create
    ~limits:D.Limits.default
    ~kind
    ~version:1
    ~payload:(`Object [ "future_metadata", `String "retained" ])
  |> ok
;;

let bundle kind contents =
  B.create ~metadata:(metadata kind) ~payloads:[ B.Payload.create contents |> store_ok ]
  |> store_ok
;;

let artifact ?(root = "<developer>captured</developer>") ?(sources = []) programs content =
  A.Artifact.create
    ~revision_id:revision
    ~root_chatmd:root
    ~sources
    ~program_bundle:programs
    ~content_bundle:content
    ~parser_schema_version:1
    ~runtime_schema_version:1
    ~created_at:timestamp
    ()
  |> store_ok
;;

let set json name value =
  match json with
  | `Object fields ->
    `Object
      (List.map fields ~f:(fun (field, old) ->
         field, if String.equal field name then value else old))
  | _ -> failwith "expected fixture object"
;;

let field json name =
  match D.Json.field json ~name with
  | Value value -> value
  | Absent | Null -> failwith "missing fixture field"
;;

let append json name value =
  match json with
  | `Object fields -> `Object (fields @ [ name, value ])
  | _ -> failwith "expected fixture object"
;;

let with_payload document f =
  let json = D.Document.json document in
  set json "payload" (f (field json "payload"))
  |> D.Document.inspect ~limits:C.limits
  |> ok
;;

let pools (artifact : A.Artifact.t) =
  List.map [ artifact.program_bundle; artifact.content_bundle ] ~f:(fun bundle ->
    Option.map bundle ~f:(fun bundle ->
      List.map (B.payloads bundle) ~f:B.Payload.contents))
;;

let%expect_test
    "complete private custody retains exact publication and compatible unknowns"
  =
  let programs = bundle "fixture.programs" "let main = 42" in
  let content = bundle "fixture.content" "\000\255\128captured edition" in
  let source =
    A.Source.create ~relative_path:"child.chatmd" ~contents:"<developer>child</developer>"
    |> store_ok
  in
  let original = artifact ~sources:[ source ] programs content in
  let publication = M.Publication.bytes original.manifest_publication in
  let json = Jsonaf.of_string publication in
  let json =
    set json "payload" (append (field json "payload") "future_manifest" (`String "exact"))
  in
  let exact = "\n " ^ Jsonaf.to_string json ^ "\n" in
  let publication =
    M.Publication.of_bytes exact ~revision_id:revision ~expected_sha256:(digest exact)
    |> store_ok
  in
  let original =
    A.Artifact.restore
      ~publication
      ~root_chatmd:original.root_chatmd
      ~sources:original.sources
      ~program_payloads:(Some (B.payloads programs))
      ~content_payloads:(Some (B.payloads content))
    |> store_ok
  in
  let document = C.create original |> ok |> C.to_document |> ok in
  let document =
    with_payload document (fun payload ->
      append payload "future_custody" (`Object [ "data", `String "keep" ]))
  in
  let json = append (D.Document.json document) "future_envelope" (`String "keep too") in
  let document = D.Document.inspect ~limits:C.limits json |> ok in
  let restored = C.of_document document |> ok in
  let roundtrip = C.to_document restored |> ok in
  assert (D.Json.equal (D.Document.json document) (D.Document.json roundtrip));
  let actual = C.value restored in
  assert (String.equal exact (M.Publication.bytes actual.manifest_publication));
  assert (String.equal original.manifest_sha256 actual.manifest_sha256);
  assert (
    List.equal
      [%equal: string * string]
      (List.map original.sources ~f:(fun s -> s.A.Source.relative_path, s.contents))
      (List.map actual.sources ~f:(fun s -> s.A.Source.relative_path, s.contents)));
  assert (
    List.equal (Option.equal (List.equal String.equal)) (pools original) (pools actual));
  let copied =
    A.Artifact.with_identity
      actual
      ~revision_id:(P.Id.Prompt_revision.of_string "prv_captured_copy" |> protocol_ok)
      ~created_at:timestamp
    |> store_ok
  in
  assert (
    List.equal (Option.equal (List.equal String.equal)) (pools actual) (pools copied));
  let copied_json =
    M.Publication.document copied.manifest_publication
    |> M.to_document
    |> ok
    |> D.Document.json
  in
  assert (
    D.Json.equal (field (field copied_json "payload") "future_manifest") (`String "exact"));
  print_endline "exact publication + binary private pools + future metadata retained";
  [%expect {| exact publication + binary private pools + future metadata retained |}]
;;

let%expect_test
    "bounded custody rejects altered identity, inventories and noncanonical bytes"
  =
  let document =
    artifact
      ~sources:
        [ A.Source.create ~relative_path:"child.chatmd" ~contents:"child" |> store_ok ]
      (bundle "fixture.programs" "program")
      (bundle "fixture.content" "edition")
    |> C.create
    |> ok
    |> C.to_document
    |> ok
  in
  let changes =
    [ ("root", fun p -> set p "root" (encoded "changed"))
    ; ( "digest"
      , fun p ->
          set
            p
            "publication"
            (set (field p "publication") "sha256" (`String (String.make 64 '0'))) )
    ; ( "revision"
      , fun p ->
          set
            p
            "publication"
            (set (field p "publication") "revision_id" (`String "prv_foreign")) )
    ; ( "source"
      , fun p ->
          set
            p
            "sources"
            (`Array
                [ `Object
                    [ "relative_path", `String "child.chatmd"
                    ; "bytes", encoded "changed"
                    ]
                ]) )
    ; ( "source_path"
      , fun p ->
          set
            p
            "sources"
            (`Array
                [ `Object
                    [ "relative_path", `String "foreign.chatmd"
                    ; "bytes", encoded "child"
                    ]
                ]) )
    ; ("missing", fun p -> set p "program_payloads" (`Array []))
    ; ( "surplus"
      , fun p -> set p "program_payloads" (`Array [ encoded "program"; encoded "extra" ])
      )
    ; ("presence", fun p -> set p "content_payloads" `Null)
    ; ("replacement", fun p -> set p "content_payloads" (`Array [ encoded "changed" ]))
    ; ("padding", fun p -> set p "root" (`String "Zh=="))
    ; ("alphabet", fun p -> set p "root" (`String "Zg-_"))
    ]
  in
  List.iter changes ~f:(fun (name, change) ->
    assert (Result.is_error (C.of_document (with_payload document change)));
    print_endline name);
  let rec nested count = if count = 0 then `Null else `Array [ nested (count - 1) ] in
  let deep =
    append (D.Document.json document) "future_deep" (nested 80)
    |> D.Document.inspect ~limits:D.Limits.default
    |> ok
  in
  (match C.of_document deep with
   | Error (D.Error.Limit_exceeded _) -> ()
   | Error _ | Ok _ -> failwith "carrier must bound directly constructed input");
  [%expect
    {|
    root
    digest
    revision
    source
    source_path
    missing
    surplus
    presence
    replacement
    padding
    alphabet
    |}]
;;

let%expect_test "one exact shared inventory and pre-decode capacity rejection" =
  let programs = bundle "fixture.programs" (String.make (B.max_payload_bytes - 2) 'p') in
  let content = bundle "fixture.content" "c" in
  let original = artifact ~root:"r" programs content in
  let document = C.create original |> ok |> C.to_document |> ok in
  let restored = C.of_document document |> ok |> C.value in
  assert (String.equal original.manifest_sha256 restored.manifest_sha256);
  let unparsed_manifest p =
    set p "publication" (set (field p "publication") "bytes" (encoded "not a manifest"))
  in
  let over_bytes =
    with_payload document (fun p ->
      unparsed_manifest p |> fun p -> set p "content_payloads" (`Array [ encoded "cc" ]))
  in
  let over_count =
    with_payload document (fun p ->
      unparsed_manifest p
      |> fun p ->
      set p "root" (encoded "")
      |> fun p ->
      set
        p
        "program_payloads"
        (`Array (List.init B.max_payload_count ~f:(fun _ -> encoded "")))
      |> fun p -> set p "content_payloads" `Null)
  in
  List.iter [ over_bytes; over_count ] ~f:(fun document ->
    match C.of_document document with
    | Error (D.Error.Limit_exceeded _) -> ()
    | Error _ | Ok _ -> failwith "inventory must reject before manifest decode/parsing");
  print_endline
    "8 MiB exact: accepted; bytes/count overflow: rejected before manifest parsing";
  [%expect
    {| 8 MiB exact: accepted; bytes/count overflow: rejected before manifest parsing |}]
;;
