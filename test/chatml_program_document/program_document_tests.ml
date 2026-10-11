open Core
module S = Chatml_source
module P = Chatml_prepared_program
module C = Chatml_program_document
module D = Document_schema

let checkpoint () = ()
let ok = Result.ok_or_failwith
let contract = S.Digest.of_contents "authentic target" ~checkpoint
let namespace = "\000\255opaque"
let environment_name = "\255\000environment"

let document_limits maximum =
  D.Limits.create ~max_bytes:maximum ~max_depth:64 ~max_fields:100000 ~max_nodes:200000
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> ok
;;

let limits = document_limits 1000000

let id path =
  S.Id.create ~package_namespace:namespace ~relative_path:path |> Or_error.ok_exn
;;

let source path contents =
  S.create (id path) ~contents ~expected_digest:None ~max_bytes:None ~checkpoint
  |> Or_error.ok_exn
;;

let location source start_byte end_byte =
  S.locate source ~start_byte ~end_byte ~checkpoint |> Or_error.ok_exn
;;

let graph ?(project = true) () =
  let entry = source "main.chatml" "open Alias\r\nAlias.run ()" in
  let implementation = source "src/library.chatml" "let run () = 7" in
  let interface = source "src/library.chatmli" "val run : unit -> int" in
  let declaration = source "chatml.project.json" "{\"namespace\":\"example\"}" in
  let lock = source "chatml.project.lock.json" "{}" in
  let config_location = location declaration 0 (S.byte_length declaration) in
  let definition =
    P.Definition.create
      ~implementation:(S.id implementation)
      ~interface:(Some (S.id interface))
      ~declared_at:config_location
    |> Or_error.ok_exn
  in
  let binding name =
    { P.Scope.name
    ; module_id = P.Definition.id definition
    ; declared_at = location entry 0 4
    }
  in
  let scopes =
    [ P.Scope.create
        (S.id entry)
        ~bindings:[ binding "Alias"; binding "Library" ]
        ~selected_at:(Some (location entry 11 12))
        ~checkpoint
      |> Or_error.ok_exn
    ; P.Scope.create (S.id implementation) ~bindings:[] ~selected_at:None ~checkpoint
      |> Or_error.ok_exn
    ; P.Scope.create (S.id interface) ~bindings:[] ~selected_at:None ~checkpoint
      |> Or_error.ok_exn
    ]
  in
  let environment =
    P.Environment.create
      ~name:environment_name
      ~required_contract:contract
      ~declared_at:(Some config_location)
    |> Or_error.ok_exn
  in
  let provenance =
    if project
    then P.Provenance.Project { declaration = S.id declaration; lock = Some (S.id lock) }
    else P.Provenance.Implicit
  in
  P.create
    ~max_input_bytes:None
    ~compiler_contract:contract
    ~environment
    ~provenance
    ~entry:(S.id entry)
    ~sources:[ lock; interface; entry; declaration; implementation ]
    ~definitions:[ definition ]
    ~scopes
    ~checkpoint
    ()
  |> Result.map_error ~f:(fun error -> error.P.Error.message)
  |> ok
;;

let capture ?(checkpoint = checkpoint) graph =
  match C.capture graph ~limits ~checkpoint with
  | Ok document -> document
  | Error _ -> failwith "capture failed"
;;

let restore
      ?(checkpoint = checkpoint)
      ?sources
      ?(compiler_contract = contract)
      document
      graph
  =
  C.restore
    document
    ~sources:(Option.value sources ~default:(P.sources graph))
    ~compiler_contract
    ~max_input_bytes:1000000
    ~checkpoint
;;

let restored document graph =
  match restore document graph with
  | Ok graph -> graph
  | Error _ -> failwith "restore failed"
;;

let field json name =
  match D.Json.field json ~name with
  | Value value -> value
  | Absent | Null -> failwith ("missing field " ^ name)
;;

let rec replace json path value =
  match path, json with
  | [], _ -> value
  | name :: rest, `Object fields ->
    `Object
      (List.map fields ~f:(fun (key, previous) ->
         key, if String.equal key name then replace previous rest value else previous))
  | index :: rest, `Array values ->
    let index = Int.of_string index in
    `Array
      (List.mapi values ~f:(fun current previous ->
         if current = index then replace previous rest value else previous))
  | _ -> failwith "invalid test update"
;;

let admit json =
  match D.Document.inspect ~limits json with
  | Error _ -> failwith "invalid fixture document"
  | Ok document -> C.of_document document ~limits ~checkpoint
;;

let mutate document path value =
  let json = D.Document.json (C.to_document document) in
  match admit (replace json ("payload" :: path) value) with
  | Ok document -> document
  | Error _ -> failwith "metadata fixture rejected structurally"
;;

let%test_unit
    "original opaque bytes and exact complete graph survive both provenance modes"
  =
  List.iter [ false; true ] ~f:(fun project ->
    let original = graph ~project () in
    let document = capture original in
    let payload = D.Document.payload (C.to_document document) in
    (* Independent byte oracle: expected wire strings are handwritten. *)
    assert (
      D.Json.equal
        (field (field payload "entry") "namespace_hex")
        (`String "00ff6f7061717565"));
    assert (
      D.Json.equal
        (field (field payload "environment") "name_hex")
        (`String "ff00656e7669726f6e6d656e74"));
    let restored = restored document original in
    assert (S.Digest.equal (P.fingerprint original) (P.fingerprint restored));
    assert (S.Id.equal (P.entry original) (P.entry restored));
    [%test_eq: string] (P.Environment.name (P.environment restored)) environment_name;
    let scope = P.find_scope restored (P.entry restored) |> Option.value_exn in
    let alias = P.Scope.find scope ~name:"Alias" |> Option.value_exn in
    let library = P.Scope.find scope ~name:"Library" |> Option.value_exn in
    assert (P.Module_id.equal alias.module_id library.module_id);
    let span = S.Location.span (P.Scope.selected_at scope |> Option.value_exn) in
    [%test_eq: int * int * int]
      (span.left.offset, span.left.line, span.left.column)
      (11, 1, 11);
    [%test_eq: int * int * int]
      (span.right.offset, span.right.line, span.right.column)
      (12, 2, 0))
;;

let%expect_test
    "metadata preserves byte identities and aliases without retaining source bodies"
  =
  let original = graph () in
  let document = capture original in
  let wire = D.Document.to_string (C.to_document document) in
  let restored = restored document original in
  printf "kind: %s\n" (D.Document.kind (C.to_document document));
  printf
    "source body present: %b\n"
    (String.is_substring wire ~substring:"let run () = 7");
  printf
    "opaque namespace bytes: %d\n"
    (String.length (S.Id.package_namespace (P.entry restored)));
  printf
    "exact fingerprint: %b\n"
    (S.Digest.equal (P.fingerprint original) (P.fingerprint restored));
  let scope = P.find_scope restored (P.entry restored) |> Option.value_exn in
  printf
    "entry aliases: %s\n"
    (String.concat
       ~sep:", "
       (List.map (P.Scope.bindings scope) ~f:(fun binding -> binding.name)));
  [%expect
    {| 
    kind: chatml.provided-program
    source body present: false
    opaque namespace bytes: 8
    exact fingerprint: true
    entry aliases: Alias, Library
    |}]
;;

let%test_unit "exact and below document and canonical input bounds" =
  let original = graph () in
  let document = capture original in
  let bytes = String.length (D.Document.to_string (C.to_document document)) in
  assert (Result.is_ok (C.capture original ~limits:(document_limits bytes) ~checkpoint));
  assert (
    Result.is_error (C.capture original ~limits:(document_limits (bytes - 1)) ~checkpoint));
  let input_bytes = P.Accounting.input_bytes (P.accounting original) in
  List.iter
    [ input_bytes - 1, false; input_bytes, true; input_bytes + 1, true ]
    ~f:(fun (max_input_bytes, expected) ->
      [%test_eq: bool]
        (Result.is_ok
           (C.restore
              document
              ~sources:(P.sources original)
              ~compiler_contract:contract
              ~max_input_bytes
              ~checkpoint))
        expected);
  assert (
    Result.is_error
      (C.restore
         document
         ~sources:[]
         ~compiler_contract:contract
         ~max_input_bytes:0
         ~checkpoint))
;;

let%test_unit "source inventory rejects missing duplicate surplus and tampered revisions" =
  let original = graph () in
  let document = capture original in
  let sources = P.sources original in
  let first = List.hd_exn sources in
  let altered =
    S.create
      (S.id first)
      ~contents:(S.contents first ^ "!")
      ~expected_digest:None
      ~max_bytes:None
      ~checkpoint
    |> Or_error.ok_exn
  in
  List.iter
    [ List.tl_exn sources
    ; first :: sources
    ; source "extra.chatml" "extra" :: sources
    ; altered :: List.tl_exn sources
    ]
    ~f:(fun sources -> assert (Result.is_error (restore document original ~sources)));
  let incorrect_contract = S.Digest.of_contents "wrong target" ~checkpoint in
  assert (
    Result.is_error (restore document original ~compiler_contract:incorrect_contract))
;;

let%test_unit "metadata tampering never restores a different graph" =
  let original = graph () in
  let document = capture original in
  let bad_digest = `String (String.make 64 '0') in
  let bad_id =
    `Object
      [ "namespace_hex", `String "00ff6f7061717565"; "path", `String "missing.chatml" ]
  in
  List.iter
    [ [ "fingerprint" ], bad_digest
    ; [ "compiler_contract" ], bad_digest
    ; [ "environment"; "required_contract" ], bad_digest
    ; [ "sources"; "0"; "digest" ], bad_digest
    ; [ "sources"; "0"; "byte_length" ], `Number "999"
    ; [ "definitions"; "0"; "declared_at"; "digest" ], bad_digest
    ; [ "definitions"; "0"; "declared_at"; "end_byte" ], `Number "999"
    ; [ "definitions"; "0"; "implementation" ], bad_id
    ; [ "definitions"; "0"; "interface" ], bad_id
    ; [ "scopes"; "0"; "bindings" ], `Array []
    ; [ "environment"; "name_hex" ], `String "61"
    ]
    ~f:(fun (path, value) ->
      assert (Result.is_error (restore (mutate document path value) original)))
;;

let%test_unit "unknown envelope and nested fields retained and coordinates never trusted" =
  let original = graph () in
  let document = capture original in
  let original_json = D.Document.json (C.to_document document) in
  let with_unknown =
    match original_json with
    | `Object fields -> `Object (("future_envelope", `String "retained") :: fields)
    | _ -> assert false
  in
  let payload = field with_unknown "payload" in
  let payload =
    match payload with
    | `Object fields ->
      `Object (("future_payload", `Object [ "v", `Number "9" ]) :: fields)
    | _ -> assert false
  in
  let json = replace with_unknown [ "payload" ] payload in
  let admitted =
    match admit json with
    | Ok value -> value
    | Error _ -> failwith "admission"
  in
  assert (D.Json.equal (D.Document.json (C.to_document admitted)) json);
  let reconstructed = restored admitted original in
  assert (S.Digest.equal (P.fingerprint reconstructed) (P.fingerprint original));
  let location = field (field payload "environment") "declared_at" in
  let location =
    match location with
    | `Object fields -> `Object (("start_line", `Number "99999") :: fields)
    | _ -> assert false
  in
  let forged_json = replace json [ "payload"; "environment"; "declared_at" ] location in
  let forged =
    match admit forged_json with
    | Ok value -> value
    | Error _ -> failwith "admission"
  in
  let graph = restored forged original in
  let location = P.Environment.declared_at (P.environment graph) |> Option.value_exn in
  [%test_eq: int] (S.Location.span location).left.line 1;
  assert (D.Json.equal (D.Document.json (C.to_document forged)) forged_json)
;;

let%test_unit "required semantics versions opaque hex and absent/null policy fail closed" =
  let document = capture (graph ()) in
  let json = D.Document.json (C.to_document document) in
  let with_semantics =
    match json with
    | `Object fields ->
      `Object (("required_semantics", `Array [ `String "future" ]) :: fields)
    | _ -> assert false
  in
  assert (Result.is_error (admit with_semantics));
  assert (Result.is_error (admit (replace json [ "schema_version" ] (`Number "2"))));
  assert (Result.is_error (admit (replace json [ "kind" ] (`String "other"))));
  List.iter [ "Ff"; "0"; "gg"; "" ] ~f:(fun value ->
    assert (
      Result.is_error
        (admit (replace json [ "payload"; "entry"; "namespace_hex" ] (`String value)))));
  assert (
    Result.is_error
      (admit (replace json [ "payload"; "environment"; "declared_at" ] `Null)))
;;

let interrupt_every operation =
  let count = ref 0 in
  operation (fun () -> Int.incr count);
  let exception Sentinel of int in
  for stop = 1 to !count do
    let visited = ref 0 in
    let interrupted =
      try
        operation (fun () ->
          Int.incr visited;
          if !visited = stop then raise (Sentinel stop));
        false
      with
      | Sentinel index -> Int.equal index stop
    in
    assert interrupted
  done
;;

let%test_unit "each observed checkpoint preserves exception and original reusable capture"
  =
  let graph = graph () in
  let document = capture graph in
  interrupt_every (fun checkpoint ->
    ignore (C.capture graph ~limits ~checkpoint : (C.t, C.Error.t) Result.t));
  interrupt_every (fun checkpoint ->
    ignore
      (C.of_document (C.to_document document) ~limits ~checkpoint
       : (C.t, C.Error.t) Result.t));
  interrupt_every (fun checkpoint ->
    ignore (restore document graph ~checkpoint : (P.t, C.Error.t) Result.t));
  ignore (restored document graph : P.t)
;;

let%test_unit "independent handwritten provided document restores exact known payload" =
  let source_id =
    S.Id.create ~package_namespace:"\000\255" ~relative_path:"entry.chatml"
    |> Or_error.ok_exn
  in
  let body =
    S.create source_id ~contents:"hello" ~expected_digest:None ~max_bytes:None ~checkpoint
    |> Or_error.ok_exn
  in
  let environment =
    P.Environment.create ~name:"manual" ~required_contract:contract ~declared_at:None
    |> Or_error.ok_exn
  in
  let scope =
    P.Scope.create source_id ~bindings:[] ~selected_at:None ~checkpoint |> Or_error.ok_exn
  in
  let expected =
    P.create
      ~max_input_bytes:None
      ~compiler_contract:contract
      ~environment
      ~provenance:P.Provenance.Implicit
      ~entry:source_id
      ~sources:[ body ]
      ~definitions:[]
      ~scopes:[ scope ]
      ~checkpoint
      ()
    |> Result.map_error ~f:(fun error -> error.P.Error.message)
    |> ok
  in
  let logical_id =
    `Object [ "namespace_hex", `String "00ff"; "path", `String "entry.chatml" ]
  in
  (* This fixture is not generated by capture or the metadata encoder. Its known
     SHA-256 vector and identity bytes independently constrain the wire mapping. *)
  let payload =
    `Object
      [ "fingerprint", `String (S.Digest.to_hex (P.fingerprint expected))
      ; "compiler_contract", `String (S.Digest.to_hex contract)
      ; "entry", logical_id
      ; ( "environment"
        , `Object
            [ "name_hex", `String "6d616e75616c"
            ; "required_contract", `String (S.Digest.to_hex contract)
            ] )
      ; "provenance", `Object [ "kind", `String "implicit" ]
      ; ( "sources"
        , `Array
            [ `Object
                [ "id", logical_id
                ; ( "digest"
                  , `String
                      "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824" )
                ; "byte_length", `Number "5"
                ]
            ] )
      ; "definitions", `Array []
      ; "scopes", `Array [ `Object [ "source", logical_id; "bindings", `Array [] ] ]
      ]
  in
  let document =
    match
      admit
        (`Object
            [ "format", `String D.Document.format
            ; "schema_version", `Number "1"
            ; "kind", `String "chatml.provided-program"
            ; "payload", payload
            ])
    with
    | Ok document -> document
    | Error _ -> failwith "handwritten document admission"
  in
  let restored = restored document expected in
  assert (S.Id.equal (P.entry restored) source_id);
  [%test_eq: string] (S.contents (List.hd_exn (P.sources restored))) "hello";
  assert (S.Digest.equal (P.fingerprint restored) (P.fingerprint expected))
;;

let%test_unit "opaque expansion is rejected before an input-sized duplicate allocation" =
  let namespace = String.make 1000000 '\255' in
  let source_id =
    S.Id.create ~package_namespace:namespace ~relative_path:"a.chatml" |> Or_error.ok_exn
  in
  let body =
    S.create source_id ~contents:"" ~expected_digest:None ~max_bytes:None ~checkpoint
    |> Or_error.ok_exn
  in
  let scope =
    P.Scope.create source_id ~bindings:[] ~selected_at:None ~checkpoint |> Or_error.ok_exn
  in
  let graph =
    P.create
      ~max_input_bytes:None
      ~compiler_contract:contract
      ~environment:(P.Environment.implicit ~required_contract:contract)
      ~provenance:P.Provenance.Implicit
      ~entry:source_id
      ~sources:[ body ]
      ~definitions:[]
      ~scopes:[ scope ]
      ~checkpoint
      ()
    |> Result.map_error ~f:(fun error -> error.P.Error.message)
    |> ok
  in
  let calls = ref 0 in
  let before = Gc.minor_words () in
  let result =
    C.capture graph ~limits:(document_limits 512) ~checkpoint:(fun () -> Int.incr calls)
  in
  let allocated = Gc.minor_words () - before in
  assert (Result.is_error result);
  (* A 1MB label's hex expansion alone would allocate >=250000 words. The
     independently chosen loose bound excludes that copy/projection without
     depending on an exact internal allocation count or checkpoint sequence. *)
  assert (allocated < 10000);
  assert (!calls < 100)
;;

let%test_unit
    "hand-counted single-source document admits exact node field and depth bounds"
  =
  let graph =
    P.single_file ~compiler_contract:contract ~source:"x" ~checkpoint ()
    |> Result.map_error ~f:(fun error -> error.P.Error.message)
    |> ok
  in
  (* Wire inventory independently counted: envelope4 fields; payload8; entry2;
     environment2; implicit provenance1; source record3 plus ID2; scope2 plus ID2.
     There are29 values. Longest path: envelope/payload/sources/record/id/string. *)
  let bounded ~fields ~nodes ~depth =
    D.Limits.create ~max_bytes:10000 ~max_depth:depth ~max_fields:fields ~max_nodes:nodes
    |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
    |> ok
  in
  List.iter
    [ 26, 29, 6, true; 25, 29, 6, false; 26, 28, 6, false; 26, 29, 5, false ]
    ~f:(fun (fields, nodes, depth, expected) ->
      [%test_eq: bool]
        (Result.is_ok
           (C.capture graph ~limits:(bounded ~fields ~nodes ~depth) ~checkpoint))
        expected)
;;

let%test_unit
    "public inventory maps exact opaque logical IDs to independently admitted payloads"
  =
  let graph = graph () in
  let document = capture graph in
  let descriptor_sources =
    List.map (C.sources document) ~f:(fun descriptor ->
      let source_id = C.Source.id descriptor in
      let original = P.find_source graph source_id |> Option.value_exn in
      assert (S.Digest.equal (C.Source.digest descriptor) (S.digest original));
      [%test_eq: int]
        (C.Source.byte_length descriptor)
        (String.length (S.contents original));
      S.create
        source_id
        ~contents:(S.contents original)
        ~expected_digest:(Some (C.Source.digest descriptor))
        ~max_bytes:(Some (Int.max 1 (C.Source.byte_length descriptor)))
        ~checkpoint
      |> Or_error.ok_exn)
  in
  let restored =
    match restore document graph ~sources:descriptor_sources with
    | Ok graph -> graph
    | Error _ -> failwith "inventory restore"
  in
  assert (S.Digest.equal (P.fingerprint graph) (P.fingerprint restored))
;;

let%test_unit
    "bounded inventories distinguish duplicate and surplus logical IDs before reference \
     admission"
  =
  let graph = graph () in
  let document = capture graph in
  let sources = P.sources graph in
  let first = List.hd_exn sources in
  let duplicate = first :: first :: List.drop sources 2 in
  (match restore document graph ~sources:duplicate with
   | Error (C.Error.Duplicate_source source_id) ->
     assert (S.Id.equal source_id (S.id first))
   | Ok _ | Error _ -> failwith "expected duplicate source rejection");
  let surplus = source "aaa.chatml" "unexpected" :: List.tl_exn sources in
  (match restore document graph ~sources:surplus with
   | Error (C.Error.Unexpected_source source_id) ->
     [%test_eq: string] (S.Id.relative_path source_id) "aaa.chatml"
   | Ok _ | Error _ -> failwith "expected surplus source rejection");
  let payload = D.Document.payload (C.to_document document) in
  let inventory = field payload "sources" in
  let duplicated_inventory =
    match inventory with
    | `Array (first :: rest) -> `Array (first :: first :: rest)
    | _ -> assert false
  in
  let document = mutate document [ "sources" ] duplicated_inventory in
  match restore document graph with
  | Error (C.Error.Duplicate_source _) -> ()
  | Ok _ | Error _ -> failwith "expected duplicate metadata rejection"
;;

let%test_unit "supplied foreign identity bytes are bounded before comparisons or maps" =
  let graph =
    P.single_file ~compiler_contract:contract ~source:"" ~checkpoint ()
    |> Result.map_error ~f:(fun error -> error.P.Error.message)
    |> ok
  in
  let document = capture graph in
  let foreign_id =
    S.Id.create ~package_namespace:(String.make 1000000 'x') ~relative_path:"main.chatml"
    |> Or_error.ok_exn
  in
  let foreign =
    S.create foreign_id ~contents:"" ~expected_digest:None ~max_bytes:None ~checkpoint
    |> Or_error.ok_exn
  in
  let calls = ref 0 in
  let result =
    C.restore
      document
      ~sources:[ foreign ]
      ~compiler_contract:contract
      ~max_input_bytes:10000
      ~checkpoint:(fun () -> Int.incr calls)
  in
  (match result with
   | Error C.Error.Limit_exceeded -> ()
   | Ok _ | Error _ -> failwith "expected before-comparison identity bound");
  assert (!calls < 100)
;;

let%test_unit
    "tiny document limit rejects large inventories before duplicating metadata lists"
  =
  let entry = source "entry.chatml" "" in
  let inventory =
    entry
    :: List.init 2000 ~f:(fun index -> source (sprintf "inventory/%04d.json" index) "")
  in
  let scope =
    P.Scope.create (S.id entry) ~bindings:[] ~selected_at:None ~checkpoint
    |> Or_error.ok_exn
  in
  let graph =
    P.create
      ~max_input_bytes:None
      ~compiler_contract:contract
      ~environment:(P.Environment.implicit ~required_contract:contract)
      ~provenance:P.Provenance.Implicit
      ~entry:(S.id entry)
      ~sources:inventory
      ~definitions:[]
      ~scopes:[ scope ]
      ~checkpoint
      ()
    |> Result.map_error ~f:(fun error -> error.P.Error.message)
    |> ok
  in
  let calls = ref 0 in
  let before = Gc.minor_words () in
  let result =
    C.capture graph ~limits:(document_limits 512) ~checkpoint:(fun () -> Int.incr calls)
  in
  let allocated = Gc.minor_words () - before in
  assert (Result.is_error result);
  (* Cloning each source record and list cell exceeds this loose independent
     bound; admission should stop before walking this already retained inventory. *)
  assert (allocated < 10000);
  assert (!calls < 100)
;;
