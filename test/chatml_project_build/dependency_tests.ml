open Core
module B = Chatml_project_build
module P = Chatml_project
module S = Chatml_source
module R = Chatml_prepared_program
module Contract = Chatml.Chatml_compiler_contract

let repository = "https://example.invalid/a.git"

let pin =
  P.Git_pin.create ~object_format:Sha1 ~commit:(String.make 40 'a') |> Or_error.ok_exn
;;

let second_repository = "https://example.invalid/b.git"

let second_pin =
  P.Git_pin.create ~object_format:Sha256 ~commit:(String.make 64 'b') |> Or_error.ok_exn
;;

let path value = Source.Portable_path.create value |> Or_error.ok_exn

let git ?package_root repository pin =
  P.Lock.Catalog.Git { repository; pin; package_root = Option.map package_root ~f:path }
;;

let contract =
  let surface =
    Chatml.Chatml_static_surface.of_runtime_surface
      Chatml.Chatml_builtin_surface.empty
      ~checkpoint:ignore
  in
  Contract.for_static_surface surface ~required_bindings:[]
;;

module Case = struct
  type source =
    | Local of string
    | Explicit_git

  type outcome =
    | Accepted of P.Lock.Catalog.origin
    | Rejected

  type t =
    { label : string
    ; owner : P.Lock.Catalog.origin
    ; declaration_path : string
    ; source : source
    ; outcome : outcome
    }
end

let cases =
  let row label owner declaration_path source outcome =
    { Case.label; owner; declaration_path; source; outcome }
  in
  [ row
      "root child"
      (git repository pin)
      "owner.json"
      (Local "./subpkg")
      (Accepted (git ~package_root:"subpkg" repository pin))
  ; row
      "config child"
      (git repository pin)
      "config/owner.json"
      (Local "./subpkg")
      (Accepted (git ~package_root:"config/subpkg" repository pin))
  ; row
      "config sibling"
      (git repository pin)
      "config/owner.json"
      (Local "../sibling")
      (Accepted (git ~package_root:"sibling" repository pin))
  ; row
      "package child"
      (git ~package_root:"packages/root" repository pin)
      "owner.json"
      (Local "./subpkg")
      (Accepted (git ~package_root:"packages/root/subpkg" repository pin))
  ; row
      "package sibling"
      (git ~package_root:"packages/root" repository pin)
      "owner.json"
      (Local "../sibling")
      (Accepted (git ~package_root:"packages/sibling" repository pin))
  ; row
      "package config sibling"
      (git ~package_root:"packages/root" repository pin)
      "config/owner.json"
      (Local "../sibling")
      (Accepted (git ~package_root:"packages/root/sibling" repository pin))
  ; row
      "repository root"
      (git ~package_root:"packages/root" repository pin)
      "owner.json"
      (Local "../..")
      (Accepted (git repository pin))
  ; row "root escape" (git repository pin) "owner.json" (Local "../sibling") Rejected
  ; row
      "package escape"
      (git ~package_root:"packages/root" repository pin)
      "owner.json"
      (Local "../../..")
      Rejected
  ; row
      "absolute host"
      (git ~package_root:"packages/root" repository pin)
      "config/owner.json"
      (Local "/host/library")
      (Accepted Local)
  ; row
      "local owner"
      P.Lock.Catalog.Local
      "config/owner.json"
      (Local "../sibling")
      (Accepted Local)
  ; row
      "explicit repository"
      (git ~package_root:"packages/root" repository pin)
      "config/owner.json"
      Explicit_git
      (Accepted (git second_repository second_pin))
  ]
;;

let compose ?(checkpoint = ignore) ?target_origin ?(target_path = "cfg/shared.json") case =
  let source =
    match case.Case.source with
    | Local directory -> `Object [ "kind", `String "local"; "path", `String directory ]
    | Explicit_git ->
      `Object
        [ "kind", `String "git"
        ; "repository", `String second_repository
        ; "object_format", `String "sha256"
        ; "commit", `String (String.make 64 'b')
        ]
  in
  let declaration =
    Support.capture
      ~namespace:"owner"
      case.declaration_path
      (sprintf
         {|{"schema_version":1,"namespace":"owner","dependencies":{"dep":{"source":%s,"project":"cfg/shared.json"}},"environments":{"default":{"modules":{"Imported":"dep.Util"}}}}|}
         (Jsonaf.to_string source))
  in
  let target_declaration =
    Support.capture
      ~namespace:"dep"
      target_path
      {|{"schema_version":1,"namespace":"dep","modules":{"Util":{"implementation":"util.chatml"}},"environments":{"default":{"modules":{"Util":"self.Util"}}}}|}
  in
  let target =
    Support.catalog
      target_declaration
      ~request:(Library { environment = None })
      ~members:[ Support.capture ~namespace:"dep" "util.chatml" "let value = 1" ]
  in
  let root =
    Support.catalog declaration ~request:(Library { environment = None }) ~members:[]
  in
  let association =
    P.Lock.Dependency_selection.create
      ~name:"dep"
      ~declaration:(S.id target_declaration)
      ~checkpoint:ignore
    |> Or_error.ok_exn
  in
  let expected_origin =
    match case.outcome with
    | Accepted origin -> origin
    | Rejected -> git repository pin
  in
  let root =
    B.Catalog_input.create
      root
      ~origin:case.owner
      ~acquisition_key:"owner-attempt"
      ~dependencies:[ association ]
  in
  let target =
    B.Catalog_input.create
      target
      ~origin:(Option.value target_origin ~default:expected_origin)
      ~acquisition_key:"target-attempt"
      ~dependencies:[]
  in
  let host = Support.capture ~namespace:"host" "inline.chatml" "let main = 0" in
  let limits = Support.limits () in
  let open Result.Let_syntax in
  let%bind gate, _ = B.Selection_gate.create Initial_prepare ~limits ~checkpoint in
  B.compose
    root
    ~catalogs:[ target ]
    ~entry:(Captured host)
    ~selection_gate:gate
    ~contract
    ~lock_id:(Support.id ~namespace:"owner" "selected.lock.json")
    ~limits
    ~checkpoint
;;

let target_receipt build =
  List.find
    (P.Lock.catalogs (B.lock build))
    ~f:(fun catalog -> String.equal (P.Lock.Catalog.namespace catalog) "dep")
  |> Option.value_exn
;;

let assert_origin (expected : P.Lock.Catalog.origin) (actual : P.Lock.Catalog.origin) =
  match expected, actual with
  | Local, Local -> ()
  | Git expected, Git actual ->
    [%test_eq: string] expected.repository actual.repository;
    assert (P.Git_pin.equal expected.pin actual.pin);
    assert (
      Option.equal Source.Portable_path.equal expected.package_root actual.package_root)
  | Local, Git _ | Git _, Local -> failwith "wrong selected origin"
;;

let%test_unit "finite independent repository-relative origin oracle through public APIs" =
  [%test_eq: int] (List.length cases) 12;
  [%test_eq: int]
    (List.count cases ~f:(fun case ->
       match case.Case.outcome with
       | Accepted _ -> true
       | Rejected -> false))
    10;
  List.iter cases ~f:(fun case ->
    match case.Case.outcome with
    | Rejected -> ignore (compose case |> Support.reject : B.Error.t)
    | Accepted origin ->
      let build = compose case |> Support.ok in
      let selected = target_receipt build in
      assert_origin origin (P.Lock.Catalog.origin selected);
      assert (
        S.Id.equal
          (P.Lock.Catalog.declaration selected)
          (Support.id ~namespace:"dep" "cfg/shared.json"));
      [%test_eq: (string * string * string) list]
        (Fixtures.scope_targets
           (B.prepared build)
           (Support.id ~namespace:"host" "inline.chatml"))
        [ "Imported", "dep", "util.chatml" ];
      let restored =
        B.restore
          (B.lock_source build)
          ~sources:(Support.without_lock build)
          ~contract
          ~limits:(Support.limits ())
          ~checkpoint:ignore
        |> Support.ok
      in
      Support.assert_same_source (B.lock_source build) (B.lock_source restored);
      assert (
        S.Digest.equal
          (R.fingerprint (B.prepared build))
          (R.fingerprint (B.prepared restored))))
;;

let%expect_test
    "manifest directory and package root are distinct from logical source identity"
  =
  List.iter [ "package sibling"; "package config sibling" ] ~f:(fun label ->
    let case =
      List.find cases ~f:(fun case -> String.equal case.Case.label label)
      |> Option.value_exn
    in
    let catalog = compose case |> Support.ok |> target_receipt in
    let prefix =
      match P.Lock.Catalog.origin catalog with
      | Git { package_root = Some root; _ } -> Source.Portable_path.to_string root
      | Local | Git { package_root = None; _ } -> failwith "expected package root"
    in
    printf
      "%s: repository prefix %s; source dep/%s\n"
      label
      prefix
      (S.Id.relative_path (P.Lock.Catalog.declaration catalog)));
  [%expect
    {|
    package sibling: repository prefix packages/sibling; source dep/cfg/shared.json
    package config sibling: repository prefix packages/root/sibling; source dep/cfg/shared.json |}]
;;

let%test_unit "wrong captured origin and repository-prefixed logical IDs reject" =
  let case =
    List.find cases ~f:(fun case -> String.equal case.Case.label "package sibling")
    |> Option.value_exn
  in
  let wrong_pin =
    P.Git_pin.create ~object_format:Sha1 ~commit:(String.make 40 'c') |> Or_error.ok_exn
  in
  List.iter
    [ P.Lock.Catalog.Local
    ; git ~package_root:"wrong" repository pin
    ; git ~package_root:"packages/sibling" second_repository pin
    ; git ~package_root:"packages/sibling" repository wrong_pin
    ]
    ~f:(fun target_origin ->
      ignore (compose ~target_origin case |> Support.reject : B.Error.t));
  ignore
    (compose ~target_path:"packages/sibling/cfg/shared.json" case |> Support.reject
     : B.Error.t)
;;

let%test_unit "repository-relative comparison checkpoints preserve identity and retry" =
  let case =
    List.find cases ~f:(fun case -> String.equal case.Case.label "package config sibling")
    |> Option.value_exn
  in
  let expected = compose case |> Support.ok in
  Support.assert_checkpoint_identity ~f:(fun checkpoint ->
    let actual = compose ~checkpoint case |> Support.ok in
    Support.assert_same_source (B.lock_source expected) (B.lock_source actual))
;;
