open Core
module B = Chatml_project_build
module S = Chatml_source
module R = Chatml_prepared_program
module P = Chatml_project
module Contract = Chatml.Chatml_compiler_contract

let lock_id = Support.id ~namespace:"role" "delegated.lock.json"

let library ?(restricted = false) contract =
  let declaration =
    Support.capture
      ~namespace:"role"
      "chatml.project.json"
      (if restricted
       then
         {|{"schema_version":1,"namespace":"role","source_dirs":["src"],"environments":{"default":{"modules":{"Counter":"self.Counter"},"requires":{"targets":["moderator_v1"]}}}}|}
       else
         {|{"schema_version":1,"namespace":"role","source_dirs":["src"],"environments":{"default":{"modules":{"Counter":"self.Counter"}}}}|})
  in
  let counter =
    Support.capture ~namespace:"role" "src/counter.chatml" "let initial = 0"
  in
  let interface =
    Support.capture ~namespace:"role" "src/counter.chatmli" "val initial : int"
  in
  let entry =
    Support.capture
      ~namespace:"host"
      "moderator.chatml"
      "let initial_state = Counter.initial let on_event ctx state event = \
       Task.pure(state)"
  in
  let catalog =
    Support.catalog
      declaration
      ~request:(Library { environment = None })
      ~members:[ counter; interface ]
  in
  let root =
    B.Catalog_input.create
      catalog
      ~origin:Local
      ~acquisition_key:"role-attempt"
      ~dependencies:[]
  in
  let limits = Support.limits () in
  let gate, _ =
    B.Selection_gate.create Initial_prepare ~limits ~checkpoint:ignore |> Support.ok
  in
  B.compose
    root
    ~catalogs:[]
    ~entry:(Captured entry)
    ~selection_gate:gate
    ~contract
    ~lock_id:(Support.id ~namespace:"role" "moderator.lock.json")
    ~limits
    ~checkpoint:ignore
  |> Support.ok
;;

let recompose ?(limits = Support.limits ()) ?(checkpoint = ignore) build contract id =
  B.recompose build ~contract ~lock_id:id ~limits ~checkpoint
;;

let assert_retained original actual =
  let old = Support.without_lock original in
  let fresh = Support.without_lock actual in
  [%test_eq: int] (List.length old) (List.length fresh);
  List.iter2_exn old fresh ~f:Support.assert_same_source;
  let old_prepared = B.prepared original in
  let new_prepared = B.prepared actual in
  assert (S.Id.equal (R.entry old_prepared) (R.entry new_prepared));
  [%test_eq: int] (List.length (R.definitions old_prepared)) 1;
  List.iter (R.scopes old_prepared) ~f:(fun expected ->
    let actual =
      R.find_scope new_prepared (R.Scope.source expected) |> Option.value_exn
    in
    [%test_eq: string list]
      (List.map (R.Scope.bindings expected) ~f:(fun binding -> binding.name))
      (List.map (R.Scope.bindings actual) ~f:(fun binding -> binding.name));
    List.iter2_exn
      (R.Scope.bindings expected)
      (R.Scope.bindings actual)
      ~f:(fun expected actual ->
        assert (R.Module_id.equal expected.module_id actual.module_id);
        assert (
          S.Id.equal
            (S.Location.source expected.declared_at)
            (S.Location.source actual.declared_at));
        assert (
          S.Digest.equal
            (S.Location.source_digest expected.declared_at)
            (S.Location.source_digest actual.declared_at));
        let expected_span = S.Location.span expected.declared_at in
        let actual_span = S.Location.span actual.declared_at in
        [%test_eq: int] expected_span.left.offset actual_span.left.offset;
        [%test_eq: int] expected_span.right.offset actual_span.right.offset))
;;

let%expect_test "a retained library admits a new authentic role with a distinct lock" =
  let original = library (Contract.for_target Moderator_v1 ~checkpoint:ignore) in
  let old_lock = S.contents (B.lock_source original) in
  let contract = Contract.for_target Delegated_moderator_v1 ~checkpoint:ignore in
  let restored =
    B.restore
      (B.lock_source original)
      ~sources:(Support.without_lock original)
      ~contract:(Contract.for_target Moderator_v1 ~checkpoint:ignore)
      ~limits:(Support.limits ())
      ~checkpoint:ignore
    |> Support.ok
  in
  let actual = recompose restored contract lock_id |> Support.ok in
  assert_retained original actual;
  [%test_eq: string] (S.contents (B.lock_source original)) old_lock;
  assert (
    not
      (S.Digest.equal
         (R.fingerprint (B.prepared original))
         (R.fingerprint (B.prepared actual))));
  assert (
    S.Digest.equal
      (R.compiler_contract (B.prepared actual))
      (Contract.fingerprint contract ~checkpoint:ignore));
  let replay =
    B.restore
      (B.lock_source actual)
      ~sources:(Support.without_lock actual)
      ~contract
      ~limits:(Support.limits ())
      ~checkpoint:ignore
    |> Support.ok
  in
  assert (
    S.Digest.equal (R.fingerprint (B.prepared replay)) (R.fingerprint (B.prepared actual)));
  printf
    "%s -> %s\n"
    (P.Lock.target_name (B.lock original))
    (P.Lock.target_name (B.lock actual));
  printf
    "new lock: %s; same entry and all four retained sources\n"
    (S.Id.relative_path (S.id (B.lock_source actual)));
  printf "original unchanged; new exact lock restores independently\n";
  [%expect
    {|
    moderator_v1 -> delegated_moderator_v1
    new lock: delegated.lock.json; same entry and all four retained sources
    original unchanged; new exact lock restores independently
    |}]
;;

let%test_unit
    "recomposition cannot change fixed selection or collide with retained identities"
  =
  let contract = Contract.for_target Delegated_moderator_v1 ~checkpoint:ignore in
  let original = library (Contract.for_target Moderator_v1 ~checkpoint:ignore) in
  List.iter
    (R.sources (B.prepared original))
    ~f:(fun source ->
      match
        B.Error.problem (recompose original contract (S.id source) |> Support.reject)
      with
      | Invalid_lock_identity -> ()
      | _ -> failwith "retained identity collision classification");
  let declared = Fixtures.make () |> Fixtures.compose |> Support.ok in
  (match B.Error.problem (recompose declared contract lock_id |> Support.reject) with
   | Invalid_entry_selection -> ()
   | _ -> failwith "declared selection was reinterpreted");
  let restricted =
    library ~restricted:true (Contract.for_target Moderator_v1 ~checkpoint:ignore)
  in
  (match B.Error.problem (recompose restricted contract lock_id |> Support.reject) with
   | Compiler_requirement -> ()
   | _ -> failwith "authored role requirement ignored");
  List.iter
    [ Support.limits ~steps:1 (); Support.limits ~bytes:1 () ]
    ~f:(fun limits ->
      ignore (recompose ~limits original contract lock_id |> Support.reject : B.Error.t));
  assert_retained original (recompose original contract lock_id |> Support.ok)
;;

let%test_unit "every observed recomposition interruption leaves the receiver reusable" =
  let surface =
    Chatml.Chatml_static_surface.of_runtime_surface
      Chatml.Chatml_builtin_surface.empty
      ~checkpoint:ignore
  in
  let contract = Contract.for_static_surface surface ~required_bindings:[] in
  let original = library contract in
  let old_fingerprint = R.fingerprint (B.prepared original) in
  Support.assert_checkpoint_identity ~f:(fun checkpoint ->
    let actual = recompose ~checkpoint original contract lock_id |> Support.ok in
    assert_retained original actual);
  assert (S.Digest.equal old_fingerprint (R.fingerprint (B.prepared original)))
;;
