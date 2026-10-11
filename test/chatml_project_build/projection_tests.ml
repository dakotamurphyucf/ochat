open Core
module B = Chatml_project_build
module P = Chatml_project
module C = Chatml_catalog
module S = Chatml_source

let path text = Source.Portable_path.create text |> Or_error.ok_exn
let work receipt = B.Accounting.work_steps receipt

let receipt = function
  | Ok (_, receipt) -> Ok receipt
  | Error error -> Error error
;;

let%expect_test "selected declaration edges follow captured own file environments" =
  let make ~environment ~file_environment =
    let declaration =
      Support.capture
        ~namespace:"owner"
        "owner.json"
        (sprintf
           {|{"schema_version":1,"namespace":"owner","modules":{"Own":{"implementation":"own.chatml"}},"dependencies":{"used":{"source":{"kind":"local","path":"../used"},"project":"project.json"},"unused":{"source":{"kind":"local","path":"../unused"},"project":"project.json"}},"environments":{"own":{},"empty":{"modules":{}},"imports":{"modules":{"Used":"used.Util"}}},"default_environment":"%s","file_environments":{"own.chatml":"%s"}}|}
           environment
           file_environment)
    in
    Support.catalog
      declaration
      ~request:(Library { environment = None })
      ~members:[ Support.capture ~namespace:"owner" "own.chatml" "let own = 1" ]
  in
  List.iter
    [ "own", "own"; "empty", "empty"; "own", "imports"; "imports", "empty" ]
    ~f:(fun (environment, file_environment) ->
      let catalog = make ~environment ~file_environment in
      let dependencies, _ =
        B.selected_dependencies catalog ~limits:(Support.limits ()) ~checkpoint:ignore
        |> Support.ok
      in
      print_s
        [%sexp
          (environment : string)
        , (file_environment : string)
        , (List.map dependencies ~f:P.Dependency.name : string list)]);
  [%expect
    {|
    (own own ())
    (empty empty ())
    (own imports (used))
    (imports empty (used))
    |}]
;;

let%test_unit "explicit remaining allowances aggregate pure host projections" =
  let catalog =
    Support.catalog
      (Preacquisition_tests.Fixture.owner_declaration 'a')
      ~request:(Library { environment = None })
      ~members:[ Support.capture ~namespace:"owner" "front.chatml" "let value = 0" ]
  in
  let owner = C.project catalog in
  let dependency = List.hd_exn (P.dependencies owner) in
  let polls = ref 0 in
  let gate, created =
    B.Selection_gate.create
      Initial_prepare
      ~limits:(Support.limits ())
      ~checkpoint:(fun () -> incr polls)
    |> Support.ok
  in
  assert (work created = !polls);
  Support.assert_checkpoint_identity ~f:(fun checkpoint ->
    ignore
      (B.Selection_gate.create Initial_prepare ~limits:(Support.limits ()) ~checkpoint
       |> Support.ok));
  let steps limits checkpoint =
    [ (fun () -> B.selected_dependencies catalog ~limits ~checkpoint |> receipt)
    ; (fun () ->
        B.Dependency.resolve
          dependency
          ~owner_origin:Local
          ~owner_declaration:(S.id (P.source owner))
          ~limits
          ~checkpoint
        |> receipt)
    ; (fun () ->
        B.Selection_gate.check_dependency
          gate
          ~owner
          ~owner_origin:Local
          dependency
          ~limits
          ~checkpoint)
    ; (fun () ->
        B.Selection_gate.check_root
          gate
          ~origin:Local
          ~project:(path "owner.json")
          ~limits
          ~checkpoint)
    ]
  in
  let observed =
    List.map
      (steps (Support.limits ()) ignore)
      ~f:(fun stage -> stage () |> Support.ok |> work)
  in
  let total = List.fold observed ~init:0 ~f:( + ) in
  let run maximum =
    let remaining = ref maximum in
    let acquire_calls = ref 0 in
    let result =
      List.fold_result [ 0; 1; 2; 3 ] ~init:() ~f:(fun () index ->
        let limits = Support.limits ~steps:!remaining () in
        let stage = List.nth_exn (steps limits ignore) index in
        Result.map (stage ()) ~f:(fun used -> remaining := !remaining - work used))
    in
    (match result with
     | Ok () -> incr acquire_calls
     | Error _ -> ());
    result, !remaining, !acquire_calls
  in
  let exact, remaining, acquired = run total in
  assert (Result.is_ok exact && remaining = 0 && acquired = 1);
  let below, _, acquired = run (total - 1) in
  (match B.Error.problem (Support.reject below) with
   | Limit_exceeded -> ()
   | _ -> failwith "aggregate remaining allowance classification");
  assert (acquired = 0);
  List.iter
    (steps (Support.limits ()) ignore)
    ~f:(fun stage -> ignore (stage () |> Support.ok));
  for index = 0 to 3 do
    Support.assert_checkpoint_identity ~f:(fun checkpoint ->
      ignore ((List.nth_exn (steps (Support.limits ()) checkpoint) index) () |> Support.ok))
  done
;;

let%test_unit "root code gate rejects before sentinel acquisition" =
  let original = Gate_tests.compose (Gate_tests.git 'a') Initial_prepare |> Support.ok in
  let gate, _ =
    B.Selection_gate.create
      (Build { previous_lock = B.lock_source original })
      ~limits:(Support.limits ())
      ~checkpoint:ignore
    |> Support.ok
  in
  let changed_route =
    match Gate_tests.git 'a' with
    | Git original ->
      P.Lock.Catalog.Git
        { original with repository = "https://example.invalid/other.git" }
    | Local -> failwith "Git fixture origin"
  in
  let changed_format =
    P.Lock.Catalog.Git
      { repository = "https://example.invalid/root.git"
      ; pin =
          P.Git_pin.create ~object_format:Sha256 ~commit:(String.make 64 'a')
          |> Or_error.ok_exn
      ; package_root = None
      }
  in
  List.iter
    [ P.Lock.Catalog.Local
    ; Gate_tests.git 'b'
    ; Gate_tests.git ~package_root:"sub" 'a'
    ; changed_route
    ; changed_format
    ]
    ~f:(fun origin ->
      let acquisition_calls = ref 0 in
      let checked =
        B.Selection_gate.check_root
          gate
          ~origin
          ~project:(path "owner.json")
          ~limits:(Support.limits ())
          ~checkpoint:ignore
      in
      (match checked with
       | Ok _ -> incr acquisition_calls
       | Error _ -> ());
      assert (!acquisition_calls = 0);
      match B.Error.problem (Support.reject checked) with
      | Needs_update -> ()
      | _ -> failwith "changed root intent classification");
  let changed_member =
    B.Selection_gate.check_root
      gate
      ~origin:(Gate_tests.git 'a')
      ~project:(path "other.json")
      ~limits:(Support.limits ())
      ~checkpoint:ignore
  in
  (match B.Error.problem (Support.reject changed_member) with
   | Needs_update -> ()
   | _ -> failwith "changed project member admitted");
  Support.assert_checkpoint_identity ~f:(fun checkpoint ->
    ignore
      (B.Selection_gate.check_root
         gate
         ~origin:(Gate_tests.git 'a')
         ~project:(path "owner.json")
         ~limits:(Support.limits ())
         ~checkpoint
       |> Support.ok));
  let update, _ =
    B.Selection_gate.create
      (Update { previous_lock = Some (B.lock_source original) })
      ~limits:(Support.limits ())
      ~checkpoint:ignore
    |> Support.ok
  in
  ignore
    (B.Selection_gate.check_root
       update
       ~origin:(Gate_tests.git 'b')
       ~project:(path "other.json")
       ~limits:(Support.limits ())
       ~checkpoint:ignore
     |> Support.ok)
;;

let%test_unit "resolved Git member projection preserves package-relative identities" =
  let declaration =
    Support.capture
      ~namespace:"owner"
      "config/project.json"
      {|{"schema_version":1,"namespace":"owner","dependencies":{"child":{"source":{"kind":"local","path":"../sibling"},"project":"manifest.json"}}}|}
  in
  let owner =
    P.decode declaration ~limits:Document_schema.Limits.default ~checkpoint:ignore
    |> function
    | Ok owner -> owner
    | Error _ -> failwith "valid declaration"
  in
  let dependency = List.hd_exn (P.dependencies owner) in
  let resolve checkpoint =
    B.Dependency.resolve
      dependency
      ~owner_origin:(Gate_tests.git ~package_root:"packages/root" 'a')
      ~owner_declaration:(S.id declaration)
      ~limits:(Support.limits ())
      ~checkpoint
    |> Support.ok
  in
  let selected, _ = resolve ignore in
  (match B.Dependency.origin selected with
   | Git { package_root = Some root; _ } ->
     assert (Source.Portable_path.equal root (path "packages/root/sibling"))
   | Local | Git { package_root = None; _ } -> failwith "resolved Git package prefix");
  assert (
    Source.Portable_path.equal (B.Dependency.project selected) (path "manifest.json"));
  let member checkpoint =
    B.Dependency.repository_member
      ~package_root:(Some (path "packages/root/sibling"))
      ~member:(B.Dependency.project selected)
      ~limits:(Support.limits ())
      ~checkpoint
    |> Support.ok
  in
  let repository_member, _ = member ignore in
  [%test_eq: string]
    (Source.Portable_path.to_string repository_member)
    "packages/root/sibling/manifest.json";
  Support.assert_checkpoint_identity ~f:(fun checkpoint -> ignore (resolve checkpoint));
  Support.assert_checkpoint_identity ~f:(fun checkpoint -> ignore (member checkpoint))
;;
