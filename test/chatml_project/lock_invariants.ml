open Core
module P = Chatml_project
module L = P.Lock
module C = Chatml_source
module D = Document_schema
module S = Support

let checkpoint () = ()

let lock_exn = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (P.Error.details error : D.Error.t)]
;;

let decode source = L.decode source ~limits:D.Limits.default ~checkpoint |> lock_exn
let encode receipt = L.to_string receipt ~limits:D.Limits.default ~checkpoint |> lock_exn

(* A tiny independent wire-tree edit model. It never calls a project encoder or
   domain constructor; positive transforms alter ordering, negative ones alter
   exactly the authored field or domain key under examination. *)
module Wire = struct
  type step =
    | Field of string
    | Index of int

  let rec change tree path ~f =
    match path, tree with
    | [], _ -> f tree
    | Field name :: rest, `Object fields ->
      assert (List.count fields ~f:(fun (key, _) -> String.equal key name) = 1);
      `Object
        (List.map fields ~f:(fun (key, value) ->
           key, if String.equal key name then change value rest ~f else value))
    | Index index :: rest, `Array values ->
      assert (index >= 0 && index < List.length values);
      `Array
        (List.mapi values ~f:(fun i value ->
           if Int.equal i index then change value rest ~f else value))
    | _ -> failwith "Test wire edit path does not exist"
  ;;

  let rec reverse_collections tree ~selected =
    match tree with
    | `Object fields ->
      `Object
        (List.map fields ~f:(fun (key, value) ->
           let value = reverse_collections value ~selected in
           let value =
             match value with
             | `Array values when List.mem selected key ~equal:String.equal ->
               `Array (List.rev values)
             | `Null | `True | `False | `Number _ | `String _ | `Array _ | `Object _ ->
               value
           in
           key, value))
    | `Array values ->
      `Array (List.map values ~f:(fun tree -> reverse_collections tree ~selected))
    | `Null | `True | `False | `Number _ | `String _ -> tree
  ;;

  let repeat = function
    | `Array (first :: rest) -> `Array (first :: first :: rest)
    | _ -> failwith "Test duplicate basis must be nonempty"
  ;;

  let without_field name = function
    | `Object fields ->
      `Object (List.filter fields ~f:(fun (key, _) -> not (String.equal key name)))
    | _ -> failwith "Test field-removal basis must be an object"
  ;;
end

let root_sources = Wire.[ Field "catalogs"; Index 0; Field "sources" ]
let root_modules = Wire.[ Field "catalogs"; Index 0; Field "modules" ]
let root_scopes = Wire.[ Field "catalogs"; Index 0; Field "scopes" ]
let root_dependencies = Wire.[ Field "catalogs"; Index 0; Field "dependencies" ]
let root_bindings = root_scopes @ Wire.[ Index 0; Field "bindings" ]

let%test_unit "finite collection reorderings agree with independent canonical wire" =
  let tree = Jsonaf.of_string Lock_fixtures.canonical in
  let kinds =
    [ "catalogs"
    ; "sources"
    ; "modules"
    ; "scopes"
    ; "bindings"
    ; "dependencies"
    ; "required_tools"
    ]
  in
  List.iter
    ([] :: kinds :: List.map kinds ~f:(fun kind -> [ kind ]))
    ~f:(fun selected ->
      let wire = Wire.reverse_collections tree ~selected |> Jsonaf.to_string in
      let receipt = S.capture ~path:"selection.lock.json" wire |> decode in
      [%test_eq: string] (encode receipt) Lock_fixtures.canonical);
  List.iter
    [ (root_sources @ Wire.[ Index 0; Field "sha256" ]), `String (String.make 64 '2')
    ; (root_sources @ Wire.[ Index 0; Field "byte_length" ]), `Number "1"
    ; Wire.[ Field "environment" ], `String "another-environment"
    ; ( (root_bindings @ Wire.[ Index 0; Field "module"; Field "path" ])
      , `String "another.chatml" )
    ]
    ~f:(fun (path, replacement) ->
      let wire = Wire.change tree path ~f:(fun _ -> replacement) |> Jsonaf.to_string in
      let receipt = S.capture ~path:"selection.lock.json" wire |> decode in
      assert (not (String.equal (encode receipt) Lock_fixtures.canonical)))
;;

let assert_field_rejected source = function
  | Ok _ -> failwith "Malformed lock metadata was admitted"
  | Error error ->
    S.assert_source source ~actual:(P.Error.source error);
    assert (not (List.is_empty (P.Error.field_path error)));
    (match P.Error.details error with
     | D.Error.Invalid_field _ | Duplicate_key _ -> ()
     | details -> raise_s [%sexp "Expected lock field/key error", (details : D.Error.t)])
;;

let%test_unit "canonicalization rejects duplicate domain keys instead of dropping them" =
  let tree = Jsonaf.of_string Lock_fixtures.canonical in
  List.iter
    [ Wire.[ Field "catalogs" ]
    ; root_sources
    ; root_modules
    ; root_scopes
    ; root_bindings
    ; root_dependencies
    ; Wire.[ Field "required_tools" ]
    ]
    ~f:(fun path ->
      let wire = Wire.change tree path ~f:Wire.repeat |> Jsonaf.to_string in
      let source = S.capture ~path:"selection.lock.json" wire in
      L.decode source ~limits:D.Limits.default ~checkpoint |> assert_field_rejected source)
;;

let%test_unit
    "receipt owners and lock self exclusion reject without claiming source admission"
  =
  let tree = Jsonaf.of_string Lock_fixtures.canonical in
  let owner_paths =
    [ Wire.[ Field "catalogs"; Index 0; Field "declaration"; Field "namespace" ]
    ; (root_sources @ Wire.[ Index 0; Field "id"; Field "namespace" ])
    ; (root_modules @ Wire.[ Index 0; Field "implementation"; Field "namespace" ])
    ; (root_modules @ Wire.[ Index 0; Field "interface"; Field "namespace" ])
    ; (root_scopes @ Wire.[ Index 0; Field "source"; Field "namespace" ])
    ]
  in
  List.iter owner_paths ~f:(fun path ->
    let wire =
      Wire.change tree path ~f:(fun _ -> `String "foreign") |> Jsonaf.to_string
    in
    let source = S.capture ~path:"selection.lock.json" wire in
    L.decode source ~limits:D.Limits.default ~checkpoint |> assert_field_rejected source);
  let source = S.capture ~namespace:"app" ~path:"a.chatml" Lock_fixtures.canonical in
  L.decode source ~limits:D.Limits.default ~checkpoint |> assert_field_rejected source;
  let source = S.capture ~namespace:"app" ~path:"a.chatml" Lock_fixtures.minimal in
  L.decode source ~limits:D.Limits.default ~checkpoint |> assert_field_rejected source;
  let wire =
    Wire.change
      tree
      (root_modules @ Wire.[ Index 0; Field "interface"; Field "path" ])
      ~f:(fun _ -> `String "a.chatml")
    |> Jsonaf.to_string
  in
  let source = S.capture wire in
  L.decode source ~limits:D.Limits.default ~checkpoint |> assert_field_rejected source
;;

let%test_unit "required absence/null, optional interface null and primitive errors reject"
  =
  let empty = S.capture Lock_fixtures.minimal |> decode in
  [%test_eq: int] (List.length (L.catalogs empty)) 0;
  [%test_eq: int] (List.length (L.required_tools empty)) 0;
  [%test_eq: string] (encode empty) Lock_fixtures.minimal;
  let tree = Jsonaf.of_string Lock_fixtures.canonical in
  List.iter
    [ "schema_version"
    ; "root"
    ; "entry"
    ; "environment"
    ; "compiler"
    ; "required_tools"
    ; "catalogs"
    ]
    ~f:(fun name ->
      List.iter
        [ Wire.without_field name tree
        ; Wire.change tree Wire.[ Field name ] ~f:(fun _ -> `Null)
        ]
        ~f:(fun json ->
          let source = S.capture (Jsonaf.to_string json) in
          L.decode source ~limits:D.Limits.default ~checkpoint
          |> assert_field_rejected source));
  List.iter
    [ (root_modules @ Wire.[ Index 0; Field "interface" ]), `Null
    ; (root_sources @ Wire.[ Index 0; Field "byte_length" ]), `Number "-1"
    ; (root_sources @ Wire.[ Index 0; Field "byte_length" ]), `Number "1.0"
    ; (root_sources @ Wire.[ Index 0; Field "sha256" ]), `String (String.make 64 'A')
    ; ( Wire.[ Field "entry"; Field "source"; Field "id"; Field "path" ]
      , `String "../a.chatml" )
    ; (root_dependencies @ Wire.[ Index 0; Field "name" ]), `String "self"
    ; (root_dependencies @ Wire.[ Index 0; Field "name" ]), `String "bad-name"
    ; (root_dependencies @ Wire.[ Index 0; Field "declaration" ]), `Null
    ; Wire.[ Field "entry"; Field "selection"; Field "name" ], `String ""
    ; Wire.[ Field "entry"; Field "selection"; Field "kind" ], `String "unknown"
    ; Wire.[ Field "entry"; Field "source" ], `Null
    ; ( Wire.[ Field "entry"; Field "selection" ]
      , `Object [ "kind", `String "captured"; "name", `String "main" ] )
    ; root_dependencies, `Null
    ; Wire.[ Field "schema_version" ], `Number "1e0"
    ; (root_bindings @ Wire.[ Index 0; Field "name" ]), `String "lowercase"
    ; ( Wire.[ Field "catalogs"; Index 1; Field "origin" ]
      , `Object
          [ "kind", `String "git"
          ; "repository", `String "https://example.invalid/r"
          ; "object_format", `String "sha1"
          ; "commit", `String "HEAD"
          ] )
    ]
    ~f:(fun (path, replacement) ->
      let wire = Wire.change tree path ~f:(fun _ -> replacement) |> Jsonaf.to_string in
      let source = S.capture wire in
      L.decode source ~limits:D.Limits.default ~checkpoint |> assert_field_rejected source);
  (* Actual wire spellings supplement tree edits; duplicate keys cannot survive
     conversion through a map, and number strings must not be coerced. *)
  List.iter
    [ String.substr_replace_all
        Lock_fixtures.minimal
        ~pattern:{|"schema_version":1|}
        ~with_:{|"schema_version":1,"schema_version":1|}
    ; String.substr_replace_all
        Lock_fixtures.minimal
        ~pattern:{|"schema_version":1|}
        ~with_:{|"schema_version":"1"|}
    ; String.substr_replace_all
        Lock_fixtures.minimal
        ~pattern:{|"catalogs":[]|}
        ~with_:{|"catalogs":[],"authority":"saved-live-grant"|}
    ]
    ~f:(fun wire ->
      let source = S.capture wire in
      L.decode source ~limits:D.Limits.default ~checkpoint |> assert_field_rejected source)
;;

let%expect_test
    "receipt retains expectations, aliases and entry/module role without live \
     verification"
  =
  let receipt = S.capture ~path:"selection.lock.json" Lock_fixtures.canonical |> decode in
  let catalogs = L.catalogs receipt in
  [%test_eq: int] (List.length catalogs) 2;
  let app =
    List.find_exn catalogs ~f:(fun catalog ->
      String.equal (L.Catalog.namespace catalog) "app")
  in
  let modules = L.Catalog.modules app in
  [%test_eq: int] (List.length modules) 2;
  assert (
    List.exists modules ~f:(fun selected ->
      C.Id.equal (L.Module_selection.implementation selected) (L.entry receipt)));
  List.iter catalogs ~f:(fun catalog ->
    assert (not (List.is_empty (L.Catalog.sources catalog)));
    List.iter (L.Catalog.sources catalog) ~f:(fun source ->
      [%test_eq: int] (L.Source_entry.byte_length source) 0));
  let scoped =
    List.find_exn (L.Catalog.scopes app) ~f:(fun scope ->
      String.equal (C.Id.relative_path (L.File_scope.source scope)) "a.chatml")
  in
  let bindings = L.File_scope.bindings scoped in
  [%test_eq: int] (List.length bindings) 2;
  List.iter bindings ~f:(fun binding ->
    let implementation =
      Chatml_prepared_program.Module_id.implementation binding.module_id
    in
    [%test_eq: string] (C.Id.package_namespace implementation) "shared";
    [%test_eq: string] (C.Id.relative_path implementation) "remote.chatml");
  let no_interface =
    List.find_exn modules ~f:(fun selected ->
      String.equal
        (C.Id.relative_path (L.Module_selection.implementation selected))
        "b.chatml")
  in
  assert (Option.is_none (L.Module_selection.interface no_interface));
  print_s
    [%sexp
      "expected byte lengths"
    , (List.map (L.Catalog.sources app) ~f:L.Source_entry.byte_length : int list)
    , "aliases"
    , (List.map bindings ~f:(fun binding -> binding.name) : string list)];
  [%expect {| ("expected byte lengths" (0 0 0) aliases (Alias Remote)) |}]
;;

let authored receipt source ~environment ~checkpoint =
  let declared_at =
    P.Declared_at.create source ~field_path:[ "selected-build" ] ~checkpoint
  in
  L.create
    ~declared_at
    ~root:(L.root receipt)
    ~entry:(L.entry_selection receipt)
    ~environment
    ~compiler_family:(L.compiler_family receipt)
    ~target_name:(L.target_name receipt)
    ~compiler_contract:(L.compiler_contract receipt)
    ~required_tools:(L.required_tools receipt)
    ~catalogs:(L.catalogs receipt)
    ~checkpoint
    ()
;;

let limits ~max_bytes =
  match
    D.Limits.create ~max_bytes ~max_depth:128 ~max_fields:100_000 ~max_nodes:1_000_000
  with
  | Ok limits -> limits
  | Error error -> raise_s [%sexp (error : D.Error.t)]
;;

let%test_unit "escaped canonical output is bounded and errors keep one exact source owner"
  =
  let wire_source =
    S.capture ~path:"selection.lock.json" ("\r\n" ^ Lock_fixtures.canonical ^ "\n")
  in
  let decoded = decode wire_source in
  let author_source =
    S.capture ~namespace:"author" ~path:"declaration.json" "exact author context\r\né"
  in
  let created =
    authored decoded author_source ~environment:(String.make 40 '"') ~checkpoint
    |> lock_exn
  in
  List.iter
    [ decoded, wire_source; created, author_source ]
    ~f:(fun (receipt, owner) ->
      let bytes = encode receipt in
      [%test_eq: string]
        (L.to_string receipt ~limits:(limits ~max_bytes:(String.length bytes)) ~checkpoint
         |> lock_exn)
        bytes;
      match
        L.to_string
          receipt
          ~limits:(limits ~max_bytes:(String.length bytes - 1))
          ~checkpoint
      with
      | Ok _ -> failwith "Oversized canonical lock was emitted"
      | Error error ->
        S.assert_source owner ~actual:(P.Error.source error);
        (match P.Error.details error with
         | D.Error.Limit_exceeded _ -> ()
         | details ->
           raise_s [%sexp "Expected output-limit category", (details : D.Error.t)]));
  assert (not (String.is_substring (encode created) ~substring:"exact author context"))
;;

let%test_unit "lock callbacks propagate at every observed poll and inputs remain reusable"
  =
  let source = S.capture ~path:"selection.lock.json" Lock_fixtures.canonical in
  let receipt = decode source in
  S.assert_checkpoint_propagation ~f:(fun checkpoint ->
    let actual = L.decode source ~limits:D.Limits.default ~checkpoint |> lock_exn in
    [%test_eq: string] (encode actual) Lock_fixtures.canonical);
  S.assert_checkpoint_propagation ~f:(fun checkpoint ->
    [%test_eq: string]
      (L.to_string receipt ~limits:D.Limits.default ~checkpoint |> lock_exn)
      Lock_fixtures.canonical);
  S.assert_checkpoint_propagation ~f:(fun checkpoint ->
    let actual =
      authored receipt source ~environment:(L.environment receipt) ~checkpoint |> lock_exn
    in
    [%test_eq: string] (encode actual) Lock_fixtures.canonical);
  let app = List.hd_exn (L.catalogs receipt) in
  let scope = List.hd_exn (L.Catalog.scopes app) in
  S.assert_checkpoint_propagation ~f:(fun checkpoint ->
    let actual =
      L.File_scope.create
        (L.File_scope.source scope)
        ~environment:(L.File_scope.environment scope)
        ~bindings:(L.File_scope.bindings scope)
        ~checkpoint
      |> Or_error.ok_exn
    in
    [%test_eq: int] (List.length (L.File_scope.bindings actual)) 2);
  let declared_at = P.Declared_at.create source ~field_path:[] ~checkpoint in
  S.assert_checkpoint_propagation ~f:(fun checkpoint ->
    let actual =
      L.Catalog.create
        ~declared_at
        ~namespace:(L.Catalog.namespace app)
        ~origin:(L.Catalog.origin app)
        ~declaration:(L.Catalog.declaration app)
        ~selected_environment:(L.Catalog.selected_environment app)
        ~dependencies:(L.Catalog.dependencies app)
        ~sources:(L.Catalog.sources app)
        ~modules:(L.Catalog.modules app)
        ~scopes:(L.Catalog.scopes app)
        ~checkpoint
      |> lock_exn
    in
    [%test_eq: int] (List.length (L.Catalog.sources actual)) 3);
  let finished = ref false in
  let actual =
    L.decode source ~limits:D.Limits.default ~checkpoint:(fun () ->
      assert (not !finished))
    |> lock_exn
  in
  finished := true;
  [%test_eq: string] (encode actual) Lock_fixtures.canonical
;;

let%expect_test
    "declared and captured entry selection retain explicit dependency associations"
  =
  let tree = Jsonaf.of_string Lock_fixtures.canonical in
  List.iter
    [ `Object [ "kind", `String "declared"; "name", `String "main" ]
    ; `Object [ "kind", `String "captured" ]
    ]
    ~f:(fun selection ->
      let receipt =
        Wire.change tree Wire.[ Field "entry"; Field "selection" ] ~f:(fun _ -> selection)
        |> Jsonaf.to_string
        |> S.capture
        |> decode
      in
      let roundtrip = encode receipt |> S.capture |> decode in
      let selected = L.entry_selection roundtrip in
      let label =
        match L.Entry.selection selected with
        | Declared name -> "declared:" ^ name
        | Captured -> "captured"
      in
      let dependencies =
        List.hd_exn (L.catalogs roundtrip)
        |> L.Catalog.dependencies
        |> List.map ~f:(fun d ->
          ( L.Dependency_selection.name d
          , C.Id.package_namespace (L.Dependency_selection.declaration d) ))
      in
      print_s [%sexp (label : string), (dependencies : (string * string) list)]);
  [%expect
    {|
    (declared:main ((shared shared) (shared_alias shared)))
    (captured ((shared shared) (shared_alias shared)))
  |}]
;;
