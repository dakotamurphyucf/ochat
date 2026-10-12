open Core
module P = Chatml_project
module S = Support
module D = Document_schema

let checkpoint () = ()

let assert_invalid source ~field_path = function
  | Ok _ -> failwith "Invalid handwritten declaration was admitted"
  | Error error ->
    S.assert_source source ~actual:(P.Error.source error);
    [%test_eq: string list] (P.Error.field_path error) field_path;
    (match P.Error.details error with
     | D.Error.Invalid_field { path; reason = _ } ->
       [%test_eq: string list] path field_path
     | details -> raise_s [%sexp "Expected invalid-field category", (details : D.Error.t)])
;;

let%expect_test "absent modules means own catalog, empty modules means no bindings" =
  List.iter
    [ {|{"schema_version":1,"namespace":"own","environments":{"pick":{}}}|}, true
    ; ( {|{"schema_version":1,"namespace":"none","environments":{"pick":{"modules":{}}}}|}
      , false )
    ]
    ~f:(fun (wire, own_catalog) ->
      let selected =
        S.capture wire
        |> S.decode_exn
        |> fun declaration ->
        S.select_exn declaration ~request:(Library { environment = Some "pick" })
      in
      let scope = P.Selection.environment selected in
      let bindings = P.Scope_spec.bindings scope in
      [%test_eq: bool] (Option.is_none bindings) own_catalog;
      match bindings with
      | None -> print_s [%sexp "own catalog"]
      | Some bindings ->
        [%test_eq: int] (List.length bindings) 0;
        print_s [%sexp "explicitly empty"]);
  [%expect
    {|
    "own catalog"
    "explicitly empty" |}]
;;

let%test_unit "explicit null and empty selectors do not act like omitted selectors" =
  let rejected =
    [ {|{"schema_version":1,"namespace":"p","environments":null}|}, [ "environments" ]
    ; ( {|{"schema_version":1,"namespace":"p","default_environment":null}|}
      , [ "default_environment" ] )
    ; ( {|{"schema_version":1,"namespace":"p","default_environment":""}|}
      , [ "default_environment" ] )
    ; ( {|{"schema_version":1,"namespace":"p","environments":{"pick":{"modules":null}}}|}
      , [ "environments"; "pick"; "modules" ] )
    ; ( {|{"schema_version":1,"namespace":"p","entries":{"main":{"source":"main.chatml","target":"one_off_v1","environment":null}}}|}
      , [ "entries"; "main"; "environment" ] )
    ; ( {|{"schema_version":1,"namespace":"p","file_environments":{"a.chatml":null}}|}
      , [ "file_environments"; "a.chatml" ] )
    ; ( {|{"schema_version":1,"namespace":"p","modules":{"A":{"implementation":"a.chatml","interface":null}}}|}
      , [ "modules"; "A"; "interface" ] )
    ; ( {|{"schema_version":1,"namespace":"p","dependencies":{"Shared":{"source":{"kind":"local","path":"shared"},"project":"chatml.project.json","environment":null}}}|}
      , [ "dependencies"; "Shared"; "environment" ] )
    ]
  in
  List.iter rejected ~f:(fun (wire, field_path) ->
    let source = S.capture wire in
    P.decode source ~limits:D.Limits.default ~checkpoint
    |> assert_invalid source ~field_path)
;;

let%test_unit "wire version is an integer and duplicate/unknown semantic fields reject" =
  List.iter
    [ {|{"schema_version":"1","namespace":"p"}|}
    ; {|{"schema_version":1.0,"namespace":"p"}|}
    ; {|{"schema_version":1e0,"namespace":"p"}|}
    ; {|{"schema_version":true,"namespace":"p"}|}
    ]
    ~f:(fun wire ->
      let source = S.capture wire in
      P.decode source ~limits:D.Limits.default ~checkpoint
      |> assert_invalid source ~field_path:[ "schema_version" ]);
  let source = S.capture {|{"schema_version":1,"namespace":"p","namespace":"other"}|} in
  (match P.decode source ~limits:D.Limits.default ~checkpoint with
   | Error error ->
     S.assert_source source ~actual:(P.Error.source error);
     (match P.Error.details error with
      | D.Error.Duplicate_key path -> [%test_eq: string list] path [ "namespace" ]
      | details ->
        raise_s [%sexp "Expected duplicate-key category", (details : D.Error.t)])
   | Ok _ -> failwith "Duplicate semantic key was admitted");
  let source = S.capture {|{"schema_version":1,"namespace":"p","unknown_scope":{}}|} in
  P.decode source ~limits:D.Limits.default ~checkpoint
  |> assert_invalid source ~field_path:[ "unknown_scope" ]
;;

let provenance_wire =
  {|{
  "schema_version":1,
  "namespace":"exact-é",
  "source_dirs":["src"],
  "exclude":["src/skip.chatml"],
  "modules":{"A":{"implementation":"src/a.chatml"}},
  "dependencies":{"Shared":{"source":{"kind":"local","path":"../shared"},"project":"chatml.project.json"}},
  "environments":{"pick":{"modules":{"Alias":"Shared.Remote"},"requires":{"compiler":"family"}}},
  "default_environment":"pick",
  "entries":{"main":{"source":"main.chatml","target":"one_off_v1"}},
  "capabilities":["exact_tool"]
}|}
;;

let assert_origins source declaration =
  S.assert_source source ~actual:(P.source declaration);
  [%test_eq: int] (List.length (P.source_dirs declaration)) 1;
  [%test_eq: int] (List.length (P.exclusions declaration)) 1;
  [%test_eq: int] (List.length (P.module_overrides declaration)) 1;
  [%test_eq: int] (List.length (P.dependencies declaration)) 1;
  [%test_eq: int] (List.length (P.required_tools declaration)) 1;
  List.iter (P.source_dirs declaration) ~f:(fun directory ->
    S.assert_declared_at
      source
      ~actual:(P.Source_dir.declared_at directory)
      ~field_path:[ "source_dirs"; "0" ]);
  List.iter (P.exclusions declaration) ~f:(fun (_, actual) ->
    S.assert_declared_at source ~actual ~field_path:[ "exclude"; "0" ]);
  List.iter (P.module_overrides declaration) ~f:(fun override ->
    S.assert_declared_at
      source
      ~actual:(P.Module_override.declared_at override)
      ~field_path:[ "modules"; "A" ]);
  List.iter (P.dependencies declaration) ~f:(fun dependency ->
    S.assert_declared_at
      source
      ~actual:(P.Dependency.declared_at dependency)
      ~field_path:[ "dependencies"; "Shared" ]);
  List.iter (P.required_tools declaration) ~f:(fun (_, actual) ->
    S.assert_declared_at source ~actual ~field_path:[ "capabilities"; "0" ]);
  let selected =
    S.select_exn declaration ~request:(Entry { name = "main"; environment = None })
  in
  let entry = P.Selection.entry selected |> Option.value_exn in
  S.assert_declared_at
    source
    ~actual:(P.Entry.declared_at entry)
    ~field_path:[ "entries"; "main" ];
  let environment = P.Selection.environment selected in
  [%test_eq: int] (List.length (P.Scope_spec.bindings environment |> Option.value_exn)) 1;
  S.assert_declared_at
    source
    ~actual:(P.Scope_spec.declared_at environment |> Option.value_exn)
    ~field_path:[ "environments"; "pick" ];
  List.iter
    (P.Scope_spec.bindings environment |> Option.value_exn)
    ~f:(fun binding ->
      S.assert_declared_at
        source
        ~actual:binding.declared_at
        ~field_path:[ "environments"; "pick"; "modules"; "Alias" ]);
  let requirement = P.Scope_spec.compiler_requirement environment in
  S.assert_declared_at
    source
    ~actual:(P.Compiler_requirement.declared_at requirement |> Option.value_exn)
    ~field_path:[ "environments"; "pick"; "requires" ]
;;

let%test_unit
    "all declarations retain exact captured identity, revision and whole-byte range"
  =
  let captures =
    [ S.capture ~namespace:"root" ~path:"a/project.json" provenance_wire
    ; S.capture
        ~namespace:"root"
        ~path:"a/project.json"
        ("\r\n " ^ provenance_wire ^ "\r\n")
    ; S.capture ~namespace:"root" ~path:"b/project.json" provenance_wire
    ; S.capture ~namespace:"other" ~path:"a/project.json" provenance_wire
    ]
  in
  List.iter captures ~f:(fun source -> assert_origins source (S.decode_exn source));
  let limits =
    match D.Limits.create ~max_bytes:8 ~max_depth:8 ~max_fields:8 ~max_nodes:8 with
    | Ok limits -> limits
    | Error error -> raise_s [%sexp (error : D.Error.t)]
  in
  List.iter captures ~f:(fun source ->
    match P.decode source ~limits ~checkpoint with
    | Ok _ -> failwith "Oversized declaration was admitted"
    | Error error ->
      S.assert_source source ~actual:(P.Error.source error);
      (match P.Error.details error with
       | D.Error.Limit_exceeded _ -> ()
       | details -> raise_s [%sexp "Expected byte-limit category", (details : D.Error.t)]))
;;

let%test_unit "every observed cooperative checkpoint escapes and leaves input reusable" =
  let source = S.capture provenance_wire in
  S.assert_checkpoint_propagation ~f:(fun checkpoint ->
    let declaration = P.decode source ~limits:D.Limits.default ~checkpoint in
    match declaration with
    | Error error -> raise_s [%sexp (P.Error.details error : D.Error.t)]
    | Ok declaration -> assert_origins source declaration);
  let declaration = S.decode_exn source in
  S.assert_checkpoint_propagation ~f:(fun checkpoint ->
    match
      P.select
        declaration
        ~request:(Entry { name = "main"; environment = None })
        ~checkpoint
    with
    | Error error -> raise_s [%sexp (P.Error.details error : D.Error.t)]
    | Ok selected ->
      [%test_eq: string] (P.Scope_spec.name (P.Selection.environment selected)) "pick");
  let rejected = S.capture {|{"schema_version":1,"namespace":"p","environments":null}|} in
  S.assert_checkpoint_propagation ~f:(fun checkpoint ->
    P.decode rejected ~limits:D.Limits.default ~checkpoint
    |> assert_invalid rejected ~field_path:[ "environments" ]);
  S.assert_checkpoint_propagation ~f:(fun checkpoint ->
    match
      P.select declaration ~request:(Library { environment = Some "missing" }) ~checkpoint
    with
    | Ok _ -> failwith "Unknown selector was admitted"
    | Error error -> S.assert_source source ~actual:(P.Error.source error));
  (* Returned data/accessors must not retain or reactivate the decode callback. *)
  let finished = ref false in
  let declaration =
    P.decode source ~limits:D.Limits.default ~checkpoint:(fun () ->
      assert (not !finished))
    |> function
    | Ok declaration -> declaration
    | Error error -> raise_s [%sexp (P.Error.details error : D.Error.t)]
  in
  finished := true;
  assert_origins source declaration;
  let selection_finished = ref false in
  let selected =
    P.select
      declaration
      ~request:(Library { environment = None })
      ~checkpoint:(fun () -> assert (not !selection_finished))
    |> function
    | Ok selected -> selected
    | Error error -> raise_s [%sexp (P.Error.details error : D.Error.t)]
  in
  selection_finished := true;
  [%test_eq: string] (P.Scope_spec.name (P.Selection.environment selected)) "pick";
  ignore
    (P.Selection.scope_for
       selected
       ~source:(Source.Portable_path.create "another.chatml" |> Or_error.ok_exn)
     : P.Scope_spec.t)
;;
