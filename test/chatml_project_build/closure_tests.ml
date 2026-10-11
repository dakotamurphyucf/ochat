open Core
module B = Chatml_project_build
module P = Chatml_project
module S = Chatml_source
module R = Chatml_prepared_program
module Contract = Chatml.Chatml_compiler_contract

module Package = struct
  type t =
    { declaration : S.t
    ; catalog : Chatml_catalog.t
    ; dependencies : P.Lock.Dependency_selection.t list
    }

  let selection name declaration =
    P.Lock.Dependency_selection.create ~name ~declaration ~checkpoint:ignore
    |> Or_error.ok_exn
  ;;

  let create ~namespace ~wire ~request ~members ~dependencies =
    let declaration = Support.capture ~namespace "chatml.project.json" wire in
    let catalog = Support.catalog declaration ~request ~members in
    { declaration; catalog; dependencies }
  ;;

  let input ?dependencies ?key t =
    let namespace = S.Id.package_namespace (S.id t.declaration) in
    B.Catalog_input.create
      t.catalog
      ~origin:Local
      ~acquisition_key:(Option.value key ~default:("fixture-origin-" ^ namespace))
      ~dependencies:(Option.value dependencies ~default:t.dependencies)
  ;;
end

module Owner_model = struct
  type t =
    { root : Package.t
    ; a : Package.t
    ; b : Package.t
    ; a_shared : Package.t
    ; b_shared : Package.t
    ; entry : S.t
    ; contract : Contract.t
    }

  let library = P.Selection.Library { environment = None }

  let shared namespace =
    Package.create
      ~namespace
      ~wire:
        (sprintf
           {|{"schema_version":1,"namespace":"%s","source_dirs":["src"]}|}
           namespace)
      ~request:library
      ~members:[ Support.capture ~namespace "src/util.chatml" "let value = 1" ]
      ~dependencies:[]
  ;;

  let make ?(root_back = false) ?(interface = false) () =
    let contract = Contract.for_target One_off_v1 ~checkpoint:ignore in
    let a_shared = shared "a.shared" in
    let b_shared = shared "b.shared" in
    let entry = Support.capture ~namespace:"root" "runner.chatml" "let main = 0" in
    let root =
      Package.create
        ~namespace:"root"
        ~wire:
          {|{"schema_version":1,"namespace":"root","source_dirs":["src"],"dependencies":{"A":{"source":{"kind":"local","path":"packages/a"},"project":"chatml.project.json","namespace":"a"},"B":{"source":{"kind":"local","path":"packages/b"},"project":"chatml.project.json","namespace":"b"}},"environments":{"default":{"modules":{"AHead":"A.Front","BHead":"B.Front","RootOwn":"self.Own"}}},"entries":{"main":{"source":"runner.chatml","target":"one_off_v1"}}}|}
        ~request:(Entry { name = "main"; environment = None })
        ~members:
          [ Support.capture ~namespace:"root" "src/own.chatml" "let value = 0"; entry ]
        ~dependencies:
          [ Package.selection "A" (Support.id ~namespace:"a" "chatml.project.json")
          ; Package.selection "B" (Support.id ~namespace:"b" "chatml.project.json")
          ]
    in
    let a =
      let back_dependency =
        if root_back
        then
          {|,"Back":{"source":{"kind":"local","path":"../.."},"project":"chatml.project.json","namespace":"root"}|}
        else ""
      in
      let back_binding = if root_back then {|,"RootBack":"Back.Own"|} else "" in
      let front = Support.capture ~namespace:"a" "src/front.chatml" "let value = 2" in
      let members =
        if interface
        then
          [ front; Support.capture ~namespace:"a" "src/front.chatmli" "val value : int" ]
        else [ front ]
      in
      Package.create
        ~namespace:"a"
        ~wire:
          (sprintf
             {|{"schema_version":1,"namespace":"a","source_dirs":["src"],"dependencies":{"Shared":{"source":{"kind":"local","path":"../shared"},"project":"chatml.project.json","namespace":"a.shared"}%s},"environments":{"default":{"modules":{"Own":"self.Front","Peer":"Shared.Util"%s}},"signature":{"modules":{}}},"file_environments":{"src/front.chatmli":"signature"}}|}
             back_dependency
             back_binding)
        ~request:library
        ~members
        ~dependencies:
          (Package.selection "Shared" (S.id a_shared.declaration)
           ::
           (if root_back then [ Package.selection "Back" (S.id root.declaration) ] else [])
          )
    in
    let b =
      Package.create
        ~namespace:"b"
        ~wire:
          {|{"schema_version":1,"namespace":"b","source_dirs":["src"],"dependencies":{"Shared":{"source":{"kind":"local","path":"../shared"},"project":"chatml.project.json","namespace":"b.shared"}},"environments":{"default":{"modules":{"Own":"self.Front","Peer":"Shared.Util"}}}}|}
        ~request:library
        ~members:[ Support.capture ~namespace:"b" "src/front.chatml" "let value = 3" ]
        ~dependencies:[ Package.selection "Shared" (S.id b_shared.declaration) ]
    in
    { root; a; b; a_shared; b_shared; entry; contract }
  ;;

  let catalogs t = List.map [ t.a; t.b; t.a_shared; t.b_shared ] ~f:Package.input

  let compose ?catalogs t =
    let limits = Support.limits () in
    let open Result.Let_syntax in
    let%bind selection_gate, _ =
      B.Selection_gate.create Initial_prepare ~limits ~checkpoint:ignore
    in
    B.compose
      (Package.input t.root)
      ~catalogs:
        (Option.value
           catalogs
           ~default:(List.map [ t.a; t.b; t.a_shared; t.b_shared ] ~f:Package.input))
      ~entry:Declared
      ~selection_gate
      ~contract:t.contract
      ~lock_id:(Support.id ~namespace:"root" "selected.lock.json")
      ~limits
      ~checkpoint:ignore
  ;;

  let targets build namespace path =
    Fixtures.scope_targets (B.prepared build) (Support.id ~namespace path)
  ;;

  let assert_model ?(root_back = false) ?(interface = false) t build =
    let prepared = B.prepared build in
    [%test_eq: int] (List.length (P.Lock.catalogs (B.lock build))) 5;
    [%test_eq: int] (List.length (R.definitions prepared)) 5;
    [%test_eq: int] (List.length (R.sources prepared)) (if interface then 13 else 12);
    assert (S.Id.equal (R.entry prepared) (S.id t.entry));
    assert (
      Option.is_none
        (R.find_definition prepared (R.Module_id.of_implementation (S.id t.entry))));
    [%test_eq: (string * string * string) list]
      (targets build "root" "runner.chatml")
      [ "AHead", "a", "src/front.chatml"
      ; "BHead", "b", "src/front.chatml"
      ; "RootOwn", "root", "src/own.chatml"
      ];
    [%test_eq: (string * string * string) list]
      (targets build "a" "src/front.chatml")
      ([ "Own", "a", "src/front.chatml"; "Peer", "a.shared", "src/util.chatml" ]
       @ if root_back then [ "RootBack", "root", "src/own.chatml" ] else []);
    [%test_eq: (string * string * string) list]
      (targets build "b" "src/front.chatml")
      [ "Own", "b", "src/front.chatml"; "Peer", "b.shared", "src/util.chatml" ];
    List.iter [ "a.shared"; "b.shared" ] ~f:(fun namespace ->
      [%test_eq: (string * string * string) list]
        (targets build namespace "src/util.chatml")
        [ "Util", namespace, "src/util.chatml" ]);
    let front =
      R.find_definition
        prepared
        (R.Module_id.of_implementation (Support.id ~namespace:"a" "src/front.chatml"))
      |> Option.value_exn
    in
    let expected_interface =
      if interface then Some (Support.id ~namespace:"a" "src/front.chatmli") else None
    in
    assert (Option.equal S.Id.equal (R.Definition.interface front) expected_interface);
    if interface
    then
      [%test_eq: (string * string * string) list]
        (targets build "a" "src/front.chatmli")
        []
  ;;
end

let%expect_test "same relative Shared locator resolves through its own declaration" =
  let fixture = Owner_model.make () in
  let build = Owner_model.compose fixture |> Support.ok in
  Owner_model.assert_model fixture build;
  List.iter [ fixture.a; fixture.b ] ~f:(fun owner ->
    let dependencies = P.dependencies (Chatml_catalog.project owner.Package.catalog) in
    [%test_eq: int] (List.length dependencies) 1;
    let dependency = List.hd_exn dependencies in
    [%test_eq: string] (P.Dependency.name dependency) "Shared";
    match P.Dependency.source dependency with
    | Local { directory } -> [%test_eq: string] directory "../shared"
    | Git _ -> failwith "Expected identical Local locator spelling");
  List.iter [ "a"; "b" ] ~f:(fun namespace ->
    let peer =
      List.find_exn
        (Owner_model.targets build namespace "src/front.chatml")
        ~f:(fun (name, _, _) -> String.equal name "Peer")
    in
    let _, target_namespace, path = peer in
    printf "%s.Shared: %s/%s\n" namespace target_namespace path);
  [%expect
    {|
    a.Shared: a.shared/src/util.chatml
    b.Shared: b.shared/src/util.chatml
    |}]
;;

let%test_unit "root alias reuse retains declared entry outside discovered catalog" =
  let fixture = Owner_model.make ~root_back:true () in
  let original = Owner_model.compose fixture |> Support.ok in
  Owner_model.assert_model ~root_back:true fixture original;
  List.iter
    [ List.rev (Owner_model.catalogs fixture)
    ; [ Package.input fixture.b_shared
      ; Package.input fixture.a
      ; Package.input fixture.b
      ; Package.input fixture.a_shared
      ]
    ]
    ~f:(fun catalogs ->
      let actual = Owner_model.compose ~catalogs fixture |> Support.ok in
      Owner_model.assert_model ~root_back:true fixture actual;
      Support.assert_same_source (B.lock_source original) (B.lock_source actual));
  let restored =
    B.restore
      (B.lock_source original)
      ~sources:(Support.without_lock original)
      ~contract:fixture.contract
      ~limits:(Support.limits ())
      ~checkpoint:ignore
    |> Support.ok
  in
  Owner_model.assert_model ~root_back:true fixture restored;
  assert (
    S.Digest.equal
      (R.fingerprint (B.prepared original))
      (R.fingerprint (B.prepared restored)))
;;

let%test_unit "owner association and unused namespace failures are finite and located" =
  let fixture = Owner_model.make () in
  let others =
    List.map [ fixture.b; fixture.a_shared; fixture.b_shared ] ~f:Package.input
  in
  let association_changes =
    [ []; [ Package.selection "Shared" (S.id fixture.b_shared.declaration) ] ]
  in
  [%test_eq: int] (List.length association_changes) 2;
  List.iter association_changes ~f:(fun dependencies ->
    let error =
      Owner_model.compose
        ~catalogs:(Package.input ~dependencies fixture.a :: others)
        fixture
      |> Support.reject
    in
    (match B.Error.problem error with
     | Invalid_dependency name -> [%test_eq: string] name "Shared"
     | _ -> failwith "Expected owner association rejection");
    Support.assert_same_source
      fixture.a.declaration
      (B.Error.declared_at error |> Option.value_exn |> P.Declared_at.source));
  let duplicate = Package.input ~key:"distinct-origin" fixture.a in
  let error =
    Owner_model.compose ~catalogs:(duplicate :: Owner_model.catalogs fixture) fixture
    |> Support.reject
  in
  (match B.Error.problem error with
   | Namespace_conflict namespace -> [%test_eq: string] namespace "a"
   | _ -> failwith "Expected conflicting acquisition identity rejection");
  let unused = Owner_model.shared "unused" in
  let error =
    Owner_model.compose
      ~catalogs:(Package.input unused :: Owner_model.catalogs fixture)
      fixture
    |> Support.reject
  in
  match B.Error.problem error with
  | Unexpected_catalog id -> assert (S.Id.equal id (S.id unused.declaration))
  | _ -> failwith "Expected surplus available declaration rejection"
;;

let%test_unit "selected optional companion changes association and owns its own scope" =
  let omitted = Owner_model.make () in
  let captured = Owner_model.make ~interface:true () in
  let old_build = Owner_model.compose omitted |> Support.ok in
  let new_build = Owner_model.compose captured |> Support.ok in
  Owner_model.assert_model omitted old_build;
  Owner_model.assert_model ~interface:true captured new_build;
  assert (
    not
      (S.Digest.equal
         (R.fingerprint (B.prepared old_build))
         (R.fingerprint (B.prepared new_build))));
  let restored =
    B.restore
      (B.lock_source new_build)
      ~sources:(Support.without_lock new_build)
      ~contract:captured.contract
      ~limits:(Support.limits ())
      ~checkpoint:ignore
    |> Support.ok
  in
  Owner_model.assert_model ~interface:true captured restored
;;
