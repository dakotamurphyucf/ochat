open Core
module B = Chatml_project_build
module P = Chatml_project
module S = Chatml_source
module Contract = Chatml.Chatml_compiler_contract

let contract =
  let surface =
    Chatml.Chatml_static_surface.of_runtime_surface
      Chatml.Chatml_builtin_surface.empty
      ~checkpoint:ignore
  in
  Contract.for_static_surface surface ~required_bindings:[]
;;

let root
      ?(declaration_path = "owner.json")
      ?(namespace = "owner")
      ?(environment = "default")
      origin
  =
  let declaration =
    Support.capture
      ~namespace
      declaration_path
      {|{"schema_version":1,"namespace":"owner","environments":{"default":{"modules":{}},"alternate":{"modules":{}}}}|}
  in
  let catalog =
    Support.catalog
      declaration
      ~request:(Library { environment = Some environment })
      ~members:[]
  in
  B.Catalog_input.create catalog ~origin ~acquisition_key:"root-attempt" ~dependencies:[]
;;

let compose ?declaration_path ?(namespace = "owner") ?environment origin operation =
  let limits = Support.limits () in
  let open Result.Let_syntax in
  let%bind gate, _ = B.Selection_gate.create operation ~limits ~checkpoint:ignore in
  B.compose
    (root ?declaration_path ~namespace ?environment origin)
    ~catalogs:[]
    ~entry:(Captured (Support.capture ~namespace:"host" "inline.chatml" "let main = 0"))
    ~selection_gate:gate
    ~contract
    ~lock_id:(Support.id ~namespace "selected.lock.json")
    ~limits
    ~checkpoint:ignore
;;

let git ?package_root commit =
  let pin =
    P.Git_pin.create ~object_format:Sha1 ~commit:(String.make 40 commit)
    |> Or_error.ok_exn
  in
  P.Lock.Catalog.Git
    { repository = "https://example.invalid/root.git"
    ; pin
    ; package_root =
        Option.map package_root ~f:(fun value ->
          Source.Portable_path.create value |> Or_error.ok_exn)
    }
;;

let%test_unit "Git root selection is preserved even without an incoming dependency" =
  let original = compose (git 'a') Initial_prepare |> Support.ok in
  let previous_lock = B.lock_source original in
  let unchanged = compose (git 'a') (Build { previous_lock }) |> Support.ok in
  Support.assert_same_source previous_lock (B.lock_source unchanged);
  List.iter
    [ P.Lock.Catalog.Local; git 'b'; git ~package_root:"subpkg" 'a' ]
    ~f:(fun origin ->
      let error = compose origin (Build { previous_lock }) |> Support.reject in
      (match B.Error.problem error with
       | Needs_update -> ()
       | _ -> failwith "root selection must require Update");
      let source =
        B.Error.declared_at error |> Option.value_exn |> P.Declared_at.source
      in
      assert (S.Id.equal (S.id source) (Support.id ~namespace:"owner" "owner.json"));
      ignore
        (compose origin (Update { previous_lock = Some previous_lock }) |> Support.ok
         : B.t))
;;

let%test_unit "Git root source members obey exact joined repository path bound" =
  let declaration_path = "chatml.project.json" in
  [%test_eq: int] (String.length declaration_path) 19;
  let valid_prefix = String.make 1004 'a' in
  [%test_eq: int] (String.length valid_prefix + 1 + String.length declaration_path) 1024;
  let valid =
    compose ~declaration_path (git ~package_root:valid_prefix 'a') Initial_prepare
    |> Support.ok
  in
  let restored =
    B.restore
      (B.lock_source valid)
      ~sources:(Support.without_lock valid)
      ~contract
      ~limits:(Support.limits ())
      ~checkpoint:ignore
    |> Support.ok
  in
  Support.assert_same_source (B.lock_source valid) (B.lock_source restored);
  let invalid_prefix = String.make 1005 'a' in
  [%test_eq: int] (String.length invalid_prefix + 1 + String.length declaration_path) 1025;
  let error =
    compose ~declaration_path (git ~package_root:invalid_prefix 'a') Initial_prepare
    |> Support.reject
  in
  match B.Error.problem error with
  | Invalid_repository_member id ->
    assert (S.Id.equal id (Support.id ~namespace:"owner" declaration_path))
  | _ -> failwith "repository member bound classification"
;;

let%test_unit "no-prior ordinary build admits Local but never Git in the selected closure"
  =
  let local = compose Local Build_without_previous |> Support.ok in
  let restored =
    B.restore
      (B.lock_source local)
      ~sources:(Support.without_lock local)
      ~contract
      ~limits:(Support.limits ())
      ~checkpoint:ignore
    |> Support.ok
  in
  Support.assert_same_source (B.lock_source local) (B.lock_source restored);
  let gate, _ =
    B.Selection_gate.create
      Build_without_previous
      ~limits:(Support.limits ())
      ~checkpoint:ignore
    |> Support.ok
  in
  assert (B.Selection_gate.preserves_git_selections gate);
  assert (Option.is_none (B.Selection_gate.previous_lock gate));
  let nested = Preacquisition_tests.Fixture.make () in
  List.iter
    [ compose (git 'a') Build_without_previous
    ; Preacquisition_tests.Fixture.compose nested Build_without_previous
    ]
    ~f:(fun result ->
      let error = result |> Support.reject in
      match B.Error.problem error with
      | Needs_update -> ()
      | _ -> failwith "no-prior Git capture bypassed update policy")
;;
