open Core
module B = Chatml_project_build
module P = Chatml_project
module S = Chatml_source
module R = Chatml_prepared_program

let routes build =
  List.map
    (P.Lock.catalogs (B.lock build))
    ~f:(fun catalog ->
      B.Effective.Route.create
        ~declaration:(P.Lock.Catalog.declaration catalog)
        ~origin:(P.Lock.Catalog.origin catalog)
        ~dependencies:(P.Lock.Catalog.dependencies catalog))
;;

let validate
      ?(checkpoint = ignore)
      ?(limits = Support.limits ())
      ?previous
      ?sources
      ?routes:chosen_routes
      fixture
  =
  let original = Fixtures.compose fixture |> Support.ok in
  B.Effective.compose
    ~root_project:(S.id (Fixtures.declaration fixture))
    ~selection:(Entry { name = "main"; environment = None })
    ~entry:Declared
    ~sources:(Option.value sources ~default:(Support.without_lock original))
    ~routes:(Option.value chosen_routes ~default:(routes original))
    ~previous
    ~lock_id:(Support.id ~namespace:"root" "effective.lock.json")
    ~contract:(Fixtures.contract fixture)
    ~limits
    ~checkpoint
;;

let%test_unit "captured route composition reproduces the complete selected graph" =
  let fixture = Fixtures.make () in
  let original = Fixtures.compose fixture |> Support.ok in
  let actual = validate fixture |> Support.ok in
  Fixtures.assert_model fixture actual;
  let expected = Support.without_lock original in
  let actual_sources = Support.without_lock actual in
  [%test_eq: int] (List.length actual_sources) (List.length expected);
  List.iter expected ~f:(fun source ->
    let actual =
      List.find_exn actual_sources ~f:(fun candidate ->
        S.Id.equal (S.id candidate) (S.id source))
    in
    Support.assert_same_source source actual);
  assert (
    S.Digest.equal
      (R.compiler_contract (B.prepared actual))
      (R.compiler_contract (B.prepared original)))
;;

let%test_unit "routes are exact, complete, and order independent" =
  let fixture = Fixtures.make () in
  let original = Fixtures.compose fixture |> Support.ok in
  let input_routes = routes original in
  let inputs = Support.without_lock original in
  let first = validate fixture |> Support.ok in
  let reversed =
    validate ~sources:(List.rev inputs) ~routes:(List.rev input_routes) fixture
    |> Support.ok
  in
  Support.assert_same_source (B.lock_source first) (B.lock_source reversed);
  List.iter
    [ "duplicate route", input_routes @ [ List.hd_exn input_routes ], inputs
    ; "missing route", List.tl_exn input_routes, inputs
    ; "duplicate source", input_routes, inputs @ [ List.hd_exn inputs ]
    ; ( "unrouted source"
      , input_routes
      , Support.capture ~namespace:"extra" "unused.chatml" "let unused = 1" :: inputs )
    ]
    ~f:(fun (label, routes, sources) ->
      match validate ~routes ~sources fixture with
      | Error _ -> ()
      | Ok _ -> failwithf "%s admitted" label ())
;;

let%test_unit "captured prior Build cannot authenticate a new root identity" =
  let fixture = Fixtures.make () in
  let original = Fixtures.compose fixture |> Support.ok in
  let error =
    B.Effective.compose
      ~root_project:(Support.id ~namespace:"root" "renamed.project.json")
      ~selection:(Entry { name = "main"; environment = None })
      ~entry:Declared
      ~sources:(Support.without_lock original)
      ~routes:(routes original)
      ~previous:(Some original)
      ~lock_id:(Support.id ~namespace:"root" "effective.lock.json")
      ~contract:(Fixtures.contract fixture)
      ~limits:(Support.limits ())
      ~checkpoint:ignore
    |> Support.reject
  in
  match B.Error.problem error with
  | Needs_update -> ()
  | _ -> failwith "unproven captured root rename admitted"
;;

let%test_unit "edited local capture recomposes under prior selection, without mutation" =
  let fixture = Fixtures.make () in
  let original = Fixtures.compose fixture |> Support.ok in
  let edited = Support.capture ~namespace:"root" "src/main.chatml" "let main = 27" in
  let sources =
    Support.without_lock original
    |> List.map ~f:(fun source ->
      if S.Id.equal (S.id source) (S.id (Fixtures.main fixture)) then edited else source)
  in
  let actual = validate ~previous:original ~sources fixture |> Support.ok in
  let selected =
    List.find_exn (Support.without_lock actual) ~f:(fun source ->
      S.Id.equal (S.id source) (S.id edited))
  in
  Support.assert_same_source edited selected;
  Support.assert_same_source
    (Fixtures.main fixture)
    (List.find_exn (Support.without_lock original) ~f:(fun source ->
       S.Id.equal (S.id source) (S.id edited)));
  let replay =
    B.restore
      (B.lock_source actual)
      ~sources:(Support.without_lock actual)
      ~contract:(Fixtures.contract fixture)
      ~limits:(Support.limits ())
      ~checkpoint:ignore
    |> Support.ok
  in
  assert (
    S.Digest.equal (R.fingerprint (B.prepared actual)) (R.fingerprint (B.prepared replay)))
;;

let%test_unit
    "editor wrapper derives routes within one budget and requires exact old lock"
  =
  let fixture = Fixtures.make () in
  let original = Fixtures.compose fixture |> Support.ok in
  let old_lock = B.lock_source original in
  let sources = R.sources (B.prepared original) in
  let recompose sources =
    B.Effective.recompose
      original
      ~sources
      ~lock_id:(Support.id ~namespace:"root" "effective.lock.json")
      ~contract:(Fixtures.contract fixture)
      ~limits:(Support.limits ())
      ~checkpoint:ignore
  in
  let actual = recompose sources |> Support.ok in
  Fixtures.assert_model fixture actual;
  let wrong_lock =
    Support.capture ~namespace:"root" "selected.lock.json" "changed lock revision"
  in
  List.iter
    [ List.filter sources ~f:(fun source ->
        not (S.Id.equal (S.id source) (S.id old_lock)))
    ; wrong_lock
      :: List.filter sources ~f:(fun source ->
        not (S.Id.equal (S.id source) (S.id old_lock)))
    ; old_lock :: sources
    ]
    ~f:(fun invalid ->
      match B.Error.problem (recompose invalid |> Support.reject) with
      | Lock_mismatch -> ()
      | _ -> failwith "old lock missing, changed or duplicated without rejection")
;;

let%test_unit "captured Library entry retains standalone or member ownership" =
  let contract =
    Chatml.Chatml_compiler_contract.for_target Moderator_v1 ~checkpoint:ignore
  in
  let standalone = Recomposition_tests.library contract in
  let overlap_fixture = Fixtures.make ~library:true () in
  let overlap =
    Fixtures.compose ~entry:(Captured (Fixtures.main overlap_fixture)) overlap_fixture
    |> Support.ok
  in
  List.iter
    [ standalone, contract; overlap, Fixtures.contract overlap_fixture ]
    ~f:(fun (previous, contract) ->
      let old_entry =
        List.find_exn (Support.without_lock previous) ~f:(fun source ->
          S.Id.equal (S.id source) (R.entry (B.prepared previous)))
      in
      let entry_id = S.id old_entry in
      let edited_entry =
        Support.capture
          ~namespace:(S.Id.package_namespace entry_id)
          (S.Id.relative_path entry_id)
          (S.contents old_entry ^ "\nlet revision = 1")
      in
      let effective_sources =
        R.sources (B.prepared previous)
        |> List.map ~f:(fun source ->
          if S.Id.equal (S.id source) entry_id then edited_entry else source)
      in
      let actual =
        B.Effective.recompose
          previous
          ~sources:effective_sources
          ~lock_id:
            (Support.id
               ~namespace:(S.Id.package_namespace (P.Lock.root (B.lock previous)))
               "effective.lock.json")
          ~contract
          ~limits:(Support.limits ())
          ~checkpoint:ignore
        |> Support.ok
      in
      assert (S.Id.equal (R.entry (B.prepared actual)) (R.entry (B.prepared previous)));
      let expected =
        List.filter effective_sources ~f:(fun source ->
          not (S.Id.equal (S.id source) (S.id (B.lock_source previous))))
      in
      let actual_sources = Support.without_lock actual in
      [%test_eq: int] (List.length actual_sources) (List.length expected);
      List.iter expected ~f:(fun source ->
        let selected =
          List.find_exn actual_sources ~f:(fun candidate ->
            S.Id.equal (S.id candidate) (S.id source))
        in
        Support.assert_same_source source selected);
      let restored =
        B.restore
          (B.lock_source actual)
          ~sources:actual_sources
          ~contract
          ~limits:(Support.limits ())
          ~checkpoint:ignore
        |> Support.ok
      in
      assert (
        S.Digest.equal
          (R.fingerprint (B.prepared actual))
          (R.fingerprint (B.prepared restored)));
      Support.assert_same_source
        old_entry
        (List.find_exn (Support.without_lock previous) ~f:(fun source ->
           S.Id.equal (S.id source) entry_id)))
;;

let%test_unit "captured closure traverses selected aliases and a dependency cycle" =
  let fixture = Closure_tests.Owner_model.make ~root_back:true () in
  let original = Closure_tests.Owner_model.compose fixture |> Support.ok in
  let expected_routes =
    [ "a", "Back", "root", "chatml.project.json"
    ; "a", "Shared", "a.shared", "chatml.project.json"
    ; "b", "Shared", "b.shared", "chatml.project.json"
    ; "root", "A", "a", "chatml.project.json"
    ; "root", "B", "b", "chatml.project.json"
    ]
  in
  let selected_routes build =
    List.concat_map
      (P.Lock.catalogs (B.lock build))
      ~f:(fun catalog ->
        let owner = P.Lock.Catalog.declaration catalog |> S.Id.package_namespace in
        List.map (P.Lock.Catalog.dependencies catalog) ~f:(fun selected ->
          let target = P.Lock.Dependency_selection.declaration selected in
          ( owner
          , P.Lock.Dependency_selection.name selected
          , S.Id.package_namespace target
          , S.Id.relative_path target )))
    |> List.sort ~compare:[%compare: string * string * string * string]
  in
  let from_routes =
    B.Effective.compose
      ~root_project:(P.Lock.root (B.lock original))
      ~selection:(Entry { name = "main"; environment = None })
      ~entry:Declared
      ~sources:(Support.without_lock original)
      ~routes:(routes original)
      ~previous:None
      ~lock_id:(Support.id ~namespace:"root" "effective.lock.json")
      ~contract:fixture.contract
      ~limits:(Support.limits ())
      ~checkpoint:ignore
    |> Support.ok
  in
  Closure_tests.Owner_model.assert_model ~root_back:true fixture from_routes;
  [%test_eq: (string * string * string * string) list]
    (selected_routes from_routes)
    expected_routes;
  let from_prior =
    B.Effective.recompose
      original
      ~sources:(R.sources (B.prepared original))
      ~lock_id:(Support.id ~namespace:"root" "effective-prior.lock.json")
      ~contract:fixture.contract
      ~limits:(Support.limits ())
      ~checkpoint:ignore
    |> Support.ok
  in
  Closure_tests.Owner_model.assert_model ~root_back:true fixture from_prior;
  [%test_eq: (string * string * string * string) list]
    (selected_routes from_prior)
    expected_routes
;;

let%test_unit "Git capture requires a prior admitted selection and unchanged overlap" =
  let original = Gate_tests.compose (Gate_tests.git 'a') Initial_prepare |> Support.ok in
  let entry_id = R.entry (B.prepared original) in
  let entry =
    List.find_exn (Support.without_lock original) ~f:(fun source ->
      S.Id.equal (S.id source) entry_id)
  in
  let root_id = P.Lock.root (B.lock original) in
  let sources =
    Support.without_lock original
    |> List.filter ~f:(fun source -> not (S.Id.equal (S.id source) entry_id))
  in
  let apply ?previous ?(routes = routes original) ?(sources = sources) () =
    B.Effective.compose
      ~root_project:root_id
      ~selection:(Library { environment = Some "default" })
      ~entry:(Captured entry)
      ~sources
      ~routes
      ~previous
      ~lock_id:(Support.id ~namespace:"owner" "effective.lock.json")
      ~contract:Gate_tests.contract
      ~limits:(Support.limits ())
      ~checkpoint:ignore
  in
  (match B.Error.problem (apply () |> Support.reject) with
   | Needs_update -> ()
   | _ -> failwith "unproven Git capture admitted");
  let retained = apply ~previous:original () |> Support.ok in
  assert (
    S.Digest.equal
      (R.fingerprint (B.prepared retained))
      (R.fingerprint (B.prepared original))
    |> not);
  let changed_sources =
    List.map sources ~f:(fun source ->
      if S.Id.equal (S.id source) root_id
      then Support.capture ~namespace:"owner" "owner.json" (S.contents source ^ " ")
      else source)
  in
  (match
     B.Error.problem
       (apply ~previous:original ~sources:changed_sources () |> Support.reject)
   with
   | Lock_mismatch -> ()
   | _ -> failwith "edited Git declaration admitted under unchanged pin");
  let changed_routes =
    List.map (routes original) ~f:(fun route ->
      if S.Id.equal (B.Effective.Route.declaration route) root_id
      then
        B.Effective.Route.create
          ~declaration:root_id
          ~origin:(Gate_tests.git 'b')
          ~dependencies:[]
      else route)
  in
  match
    B.Error.problem (apply ~previous:original ~routes:changed_routes () |> Support.reject)
  with
  | Needs_update -> ()
  | _ -> failwith "changed Git pin admitted through prior selection"
;;

let%test_unit "work and byte ceilings reject; checkpoint identity survives" =
  let fixture = Fixtures.make () in
  ignore
    (validate ~limits:(Support.limits ~steps:1 ()) fixture |> Support.reject : B.Error.t);
  ignore
    (validate ~limits:(Support.limits ~bytes:1 ()) fixture |> Support.reject : B.Error.t);
  let exception Interrupted of int ref in
  let token = ref 1 in
  let polls = ref 0 in
  (match
     validate fixture ~checkpoint:(fun () ->
       incr polls;
       if !polls = 15 then raise (Interrupted token))
   with
   | exception Interrupted actual -> assert (phys_equal actual token)
   | Ok _ | Error _ -> failwith "checkpoint interruption did not escape");
  assert (!polls = 15)
;;
